{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Zip

data ArchiveStatus read write closed
  = Uninitialised
  | Readable FilePath read
  | Writeable FilePath write
  | Closed FilePath closed

isUninitialised :: ArchiveStatus read write closed -> Bool
isUninitialised Uninitialised = True
isUninitialised _ = False

isClosed :: ArchiveStatus read write closed -> Bool
isClosed Closed{} = True
isClosed _ = False

isReadable :: ArchiveStatus read write closed -> Bool
isReadable Readable{} = True
isReadable _ = False

isWriteable :: ArchiveStatus read write closed -> Bool
isWriteable Writeable{} = True
isWriteable _ = False

data Model (v :: Type -> Type)
  = Model
  { modelArchiveStatus ::
      ArchiveStatus
        (Map (v ByteString) (v ByteString))
        (Map (v ByteString) (v ByteString))
        (Map (v ByteString) (v ByteString))
  }

initialModel :: Model v
initialModel =
  Model
    { modelArchiveStatus = Uninitialised
    }

data Env
  = Env
  { envTmpDir :: FilePath
  , envArchiveStatusRef :: IORef (ArchiveStatus Zip.Archive Zip.ArchiveBuilder ())
  }

data CNew (v :: Type -> Type)
  = CNew
      -- | Archive name
      FilePath
  deriving (Show, Generic, FunctorB, TraversableB)

cNew ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cNew =
  Command
    { commandGen = \_model -> Just $ CNew . (++ ".zip") <$> Gen.string (Range.constant 1 10) Gen.alphaNum
    , commandExecute = \(CNew archiveName) -> do
        tmpDir <- asks envTmpDir
        archiveStatusRef <- asks envArchiveStatusRef

        liftIO $ do
          archive <- Zip.new $ tmpDir </> archiveName
          writeIORef archiveStatusRef $ Writeable archiveName archive
    , commandCallbacks =
        [ Require $ \model (CNew _archiveName) -> isUninitialised $ modelArchiveStatus model
        , Update $ \model (CNew archiveName) _output -> model{modelArchiveStatus = Writeable archiveName $ Map.empty}
        , Ensure $ \_old _new (CNew _archiveName) () -> pure ()
        ]
    }

data CFinish (v :: Type -> Type)
  = CFinish
  deriving (Show, Generic, FunctorB, TraversableB)

cFinish ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cFinish =
  Command
    { commandGen = \_model -> Just $ pure CFinish
    , commandExecute = \CFinish -> do
        archiveStatusRef <- asks envArchiveStatusRef
        archiveStatus <- liftIO $ readIORef archiveStatusRef
        case archiveStatus of
          Writeable archiveName archive -> liftIO $ do
            Zip.finish_ archive
            writeIORef archiveStatusRef $ Closed archiveName ()
          _ -> error "actual archive is not closed"
    , commandCallbacks =
        [ Require $ \model CFinish -> isWriteable $ modelArchiveStatus model
        , Update $ \model CFinish _output ->
            case modelArchiveStatus model of
              Writeable archiveName archive -> model{modelArchiveStatus = Closed archiveName archive}
              _ -> error "model archive is not closed"
        , Ensure $ \_old _new CFinish () -> pure ()
        ]
    }

data COpen (v :: Type -> Type)
  = COpen
  deriving (Show, Generic, FunctorB, TraversableB)

cOpen ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cOpen =
  Command
    { commandGen = \_model -> Just $ pure COpen
    , commandExecute = \COpen -> do
        tmpDir <- asks envTmpDir
        archiveStatusRef <- asks envArchiveStatusRef
        archiveStatus <- liftIO $ readIORef archiveStatusRef
        case archiveStatus of
          Closed archiveName () -> liftIO $ do
            archive <- Zip.open $ tmpDir </> archiveName
            writeIORef archiveStatusRef $ Readable archiveName archive
          _ -> error "actual archive is not closed"
    , commandCallbacks =
        [ Require $ \model COpen -> isClosed $ modelArchiveStatus model
        , Update $ \model COpen _output ->
            case modelArchiveStatus model of
              Closed archiveName archive -> model{modelArchiveStatus = Readable archiveName archive}
              _ -> error "model archive is not closed"
        , Ensure $ \_old _new COpen () -> pure ()
        ]
    }

data CClose (v :: Type -> Type)
  = CClose
  deriving (Show, Generic, FunctorB, TraversableB)

cClose ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cClose =
  Command
    { commandGen = \_model -> Just $ pure CClose
    , commandExecute = \CClose -> do
        archiveStatusRef <- asks envArchiveStatusRef
        archiveStatus <- liftIO $ readIORef archiveStatusRef
        case archiveStatus of
          Readable archiveName archive -> liftIO $ do
            Zip.close archive
            writeIORef archiveStatusRef $ Closed archiveName ()
          _ -> error "actual archive is not readable"
    , commandCallbacks =
        [ Require $ \model CClose -> isReadable $ modelArchiveStatus model
        , Update $ \model CClose _output ->
            case modelArchiveStatus model of
              Readable archiveName archive -> model{modelArchiveStatus = Closed archiveName archive}
              _ -> error "model archive is not readable"
        , Ensure $ \_old _new CClose () -> pure ()
        ]
    }

commands ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  [Command gen m Model]
commands =
  [ cNew
  , cFinish
  , cOpen
  , cClose
  ]

prop_correctness :: Property
prop_correctness = property $ do
  actions <- forAll $ Gen.sequential (Range.constant 0 10) initialModel commands
  hoist runWithEnv $ executeSequential initialModel actions
  where
    runWithEnv :: ReaderT Env IO a -> IO a
    runWithEnv m = do
      archiveStatusRef <- newIORef Uninitialised
      withSystemTempDirectory "zippy-tests" $ \tmpDir -> do
        let env = Env{envTmpDir = tmpDir, envArchiveStatusRef = archiveStatusRef}
        runReaderT m env

main :: IO Bool
main = checkParallel $$discover
