{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import GHC.Generics (Generic)
import Hedgehog hiding (label)
import qualified Hedgehog
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
        (Map ByteString ByteString)
        (Map ByteString ByteString)
        (Map ByteString ByteString)
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

label :: String -> Callback input output state
label l = labelWith $ \_model -> const l

labelWith :: (state Concrete -> output -> String) -> Callback input output state
labelWith f = Ensure $ \old _new _command output -> Hedgehog.label (fromString $ f old output)

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
        [ label "new"
        , Require $ \model (CNew _archiveName) -> isUninitialised $ modelArchiveStatus model
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
        [ labelWith $ \model _output ->
            "finish "
              ++ case modelArchiveStatus model of
                Writeable _archiveName archive ->
                  if Map.size archive <= 5
                    then "(<= 5)"
                    else "(> 5)"
                _ -> error "model archive is not writeable"
        , Require $ \model CFinish -> isWriteable $ modelArchiveStatus model
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
        [ label "open"
        , Require $ \model COpen -> isClosed $ modelArchiveStatus model
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
        [ label "close"
        , Require $ \model CClose -> isReadable $ modelArchiveStatus model
        , Update $ \model CClose _output ->
            case modelArchiveStatus model of
              Readable archiveName archive -> model{modelArchiveStatus = Closed archiveName archive}
              _ -> error "model archive is not readable"
        , Ensure $ \_old _new CClose () -> pure ()
        ]
    }

data CAddContents (v :: Type -> Type)
  = CAddContents
      -- | (File name, File content)
      [(ByteString, ByteString)]
  deriving (Show, Generic, FunctorB, TraversableB)

cAddContents ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cAddContents =
  Command
    { commandGen = \model ->
        case modelArchiveStatus model of
          Writeable _archiveName archive ->
            Just $
              CAddContents
                <$> Gen.list
                  (Range.constant 1 5)
                  ( (,)
                      <$> Gen.bytes (Range.constant 1 $ max 1 (Map.size archive - 10))
                      <*> Gen.bytes (Range.constant 0 10000)
                  )
          _ ->
            Nothing
    , commandExecute = \(CAddContents entries) -> do
        archiveStatusRef <- asks envArchiveStatusRef
        archiveStatus <- liftIO $ readIORef archiveStatusRef
        case archiveStatus of
          Writeable _archiveName archive -> liftIO $ do
            for_ entries $ \(fileName, fileContent) -> do
              Zip.addContent fileName fileContent archive
          _ -> error "actual archive is not writeable"
    , commandCallbacks =
        [ label "add content"
        , Require $ \model (CAddContents _entries) ->
            case modelArchiveStatus model of
              Writeable _archiveName archive -> Map.size archive < maxEntries
              _ -> False
        , Update $ \model (CAddContents entries) _output ->
            case modelArchiveStatus model of
              Writeable archiveName archive ->
                model
                  { modelArchiveStatus =
                      Writeable archiveName $
                        foldl' (\acc (fileName, fileContent) -> Map.insert fileName fileContent acc) archive entries
                  }
              _ -> error "model archive is not writeable"
        , Ensure $ \_old _new (CAddContents _entries) () -> pure ()
        ]
    }
  where
    maxEntries = 50

data CReadContent (v :: Type -> Type)
  = CReadContent
      -- | File name
      ByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cReadContent ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cReadContent =
  Command
    { commandGen = \model ->
        case modelArchiveStatus model of
          Readable _archiveName archive ->
            Just $
              CReadContent
                <$> Gen.choice
                  ( [Gen.bytes (Range.constant 1 10)]
                      ++ [Gen.element (Map.keys archive) | not $ Map.null archive]
                  )
          _ ->
            Nothing
    , commandExecute = \(CReadContent fileName) -> do
        archiveStatusRef <- asks envArchiveStatusRef
        archiveStatus <- liftIO $ readIORef archiveStatusRef
        case archiveStatus of
          Readable _archiveName archive -> liftIO $ do
            Zip.readContent fileName archive
          _ -> error "actual archive is not readable"
    , commandCallbacks =
        [ labelWith $ \model output ->
            "read content "
              ++ maybe "(missing) " (const "(present) ") output
              ++ case modelArchiveStatus model of
                Readable _archiveName archive ->
                  if Map.size archive <= 5
                    then "(<= 5)"
                    else "(> 5)"
                _ -> error "model archive is not readable"
        , Require $ \model (CReadContent _fileName) -> isReadable $ modelArchiveStatus model
        , Ensure $ \old _new (CReadContent fileName) output ->
            case modelArchiveStatus old of
              Readable _archiveName archive -> do
                output === Map.lookup fileName archive
              _ -> error "actual archive is not readable"
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
  , cAddContents
  , cReadContent
  ]

prop_correctness :: Property
prop_correctness = property $ do
  actions <- forAll $ Gen.sequential (Range.constant 0 100) initialModel commands
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
