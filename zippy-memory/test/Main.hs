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
import qualified Data.ByteString as ByteString
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String (fromString)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Hedgehog hiding (label)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Zip.Memory
  ( Memory
  , memoryFromByteString
  , memoryNew
  , memoryRead
  , memorySeek
  , memorySize
  , memoryTell
  , memoryToByteString
  , memoryWrite
  )

data Model (v :: Type -> Type)
  = Model
  { modelMemory :: Maybe ModelMemory
  }

data ModelMemory
  = ModelMemory
  { modelMemoryBytes :: !ByteString
  , modelMemoryPos :: !Word64
  }

modelMemoryEmpty :: ModelMemory
modelMemoryEmpty = ModelMemory ByteString.empty 0

modelMemoryRead :: ModelMemory -> Word64 -> ByteString
modelMemoryRead _ 0 = ByteString.empty
modelMemoryRead (ModelMemory bytes pos) count
  | count >= fromIntegral (ByteString.length bytes) - pos =
      {- When `count :: Word64` exceed the size of `Int`, `fromIntegral count` goes negative,
      and `ByteString.take (fromIntegral count)` returns `ByteString.empty`.

      Avoid this by only calling `ByteString.take` when `count` is reasonable.
      -}
      ByteString.drop (fromIntegral pos) bytes
  | otherwise =
      ByteString.take (fromIntegral count) (ByteString.drop (fromIntegral pos) bytes)

modelMemoryWrite :: ModelMemory -> ByteString -> ModelMemory
modelMemoryWrite (ModelMemory bytes pos) value =
  let (prefix, suffix) = ByteString.splitAt (fromIntegral pos) bytes
  in ModelMemory
       (prefix <> value <> ByteString.drop (ByteString.length value) suffix)
       (pos + fromIntegral (ByteString.length value))

modelMemorySeek :: ModelMemory -> Word64 -> ModelMemory
modelMemorySeek (ModelMemory bytes _pos) pos =
  ModelMemory bytes (min pos (fromIntegral $ ByteString.length bytes))

modelMemorySize :: ModelMemory -> Word64
modelMemorySize (ModelMemory bytes _pos) = fromIntegral $ ByteString.length bytes

modelMemoryTell :: ModelMemory -> Word64
modelMemoryTell (ModelMemory _bytes pos) = pos

modelMemoryToByteString :: ModelMemory -> ByteString
modelMemoryToByteString (ModelMemory bytes _pos) = bytes

initialModel :: Model v
initialModel =
  Model
    { modelMemory = Nothing
    }

label :: String -> Callback input output state
label l = labelWith $ \_model -> const l

labelWith :: (state Concrete -> output -> String) -> Callback input output state
labelWith f = Ensure $ \old _new _command output -> Hedgehog.label (fromString $ f old output)

data Env
  = Env
  { envMemoryRef :: IORef (Maybe Memory)
  }

data CNew (v :: Type -> Type)
  = CNew
  deriving (Show, Generic, FunctorB, TraversableB)

cNew ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cNew =
  Command
    { commandGen = \_model -> Just $ pure CNew
    , commandExecute = \CNew -> do
        memoryRef <- asks envMemoryRef

        liftIO $ do
          memory <- memoryNew
          writeIORef memoryRef $ Just memory
    , commandCallbacks =
        [ label "new"
        , Require $ \model CNew -> isNothing $ modelMemory model
        , Update $ \model CNew _output -> model{modelMemory = Just modelMemoryEmpty}
        , Ensure $ \_old _new CNew () -> pure ()
        ]
    }

data CWrite (v :: Type -> Type)
  = CWrite
      -- | Bytes to write
      ByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cWrite ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cWrite =
  Command
    { commandGen = \_model -> Just $ CWrite <$> Gen.bytes (Range.constant 0 100)
    , commandExecute = \(CWrite value) -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          for_ mMemory $ \memory -> memoryWrite memory value
    , commandCallbacks =
        [ label "write"
        , Require $ \model (CWrite _value) -> isJust $ modelMemory model
        , Update $ \model (CWrite value) _output ->
            model{modelMemory = (\memory -> modelMemoryWrite memory value) <$> modelMemory model}
        , Ensure $ \_old _new (CWrite _value) () -> pure ()
        ]
    }

data CRead (v :: Type -> Type)
  = CRead
      -- | Number of bytes to read
      !Word64
  deriving (Show, Generic, FunctorB, TraversableB)

cRead ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cRead =
  Command
    { commandGen = \model -> do
        memory <- modelMemory model
        pure $
          CRead
            <$> Gen.choice
              [ Gen.integral (Range.constant 0 $ modelMemorySize memory)
              , Gen.integral (Range.constant 0 maxBound)
              ]
    , commandExecute = \(CRead count) -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          memoryRead (fromJust mMemory) count
    , commandCallbacks =
        [ label "read"
        , Require $ \model (CRead _count) -> isJust $ modelMemory model
        , Update $ \model (CRead count) _output ->
            model
              { modelMemory =
                  ( \memory ->
                      let pos = modelMemoryTell memory
                      in modelMemorySeek memory $ pos + count
                  )
                    <$> modelMemory model
              }
        , Ensure $ \old _new (CRead count) output -> do
            memory <- evalMaybe $ modelMemory old
            output === modelMemoryRead memory count
        ]
    }

data CTell (v :: Type -> Type)
  = CTell
  deriving (Show, Generic, FunctorB, TraversableB)

cTell ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cTell =
  Command
    { commandGen = \_model -> Just $ pure CTell
    , commandExecute = \CTell -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          memoryTell (fromJust mMemory)
    , commandCallbacks =
        [ labelWith $ \model _output ->
            "tell"
              ++ case modelMemorySize $ fromJust (modelMemory model) of
                0 -> " (size = 0)"
                _ -> " (size is non-zero)"
        , Require $ \model CTell -> isJust $ modelMemory model
        , Ensure $ \old _new CTell output -> do
            memory <- evalMaybe $ modelMemory old
            output === modelMemoryTell memory
        ]
    }

data CSeek (v :: Type -> Type)
  = CSeek !Word64
  deriving (Show, Generic, FunctorB, TraversableB)

cSeek ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cSeek =
  Command
    { commandGen = \model -> do
        memory <- modelMemory model
        pure $ CSeek <$> Gen.integral (Range.constant 0 $ modelMemorySize memory)
    , commandExecute = \(CSeek pos) -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          memorySeek (fromJust mMemory) pos
    , commandCallbacks =
        [ label "seek"
        , Require $ \model (CSeek pos) ->
            case modelMemory model of
              Nothing -> False
              Just memory -> pos <= modelMemorySize memory
        , Update $ \model (CSeek pos) _output -> model{modelMemory = (\memory -> modelMemorySeek memory pos) <$> modelMemory model}
        , Ensure $ \_old _new (CSeek _pos) _output -> pure ()
        ]
    }

data CSize (v :: Type -> Type)
  = CSize
  deriving (Show, Generic, FunctorB, TraversableB)

cSize ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cSize =
  Command
    { commandGen = \_model -> Just $ pure CSize
    , commandExecute = \CSize -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          memorySize (fromJust mMemory)
    , commandCallbacks =
        [ label "size"
        , Require $ \model CSize -> isJust $ modelMemory model
        , Ensure $ \old _new CSize output -> do
            memory <- evalMaybe $ modelMemory old
            output === modelMemorySize memory
        ]
    }

data CToByteString (v :: Type -> Type)
  = CToByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cToByteString ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cToByteString =
  Command
    { commandGen = \_model -> Just $ pure CToByteString
    , commandExecute = \CToByteString -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          mMemory <- readIORef memoryRef
          memoryToByteString (fromJust mMemory)
    , commandCallbacks =
        [ label "to bytestring"
        , Require $ \model CToByteString -> isJust $ modelMemory model
        , Ensure $ \old _new CToByteString output -> do
            memory <- evalMaybe $ modelMemory old
            output === modelMemoryToByteString memory
        ]
    }

data CFromByteString (v :: Type -> Type)
  = CFromByteString !ByteString
  deriving (Show, Generic, FunctorB, TraversableB)

cFromByteString ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  Command gen m Model
cFromByteString =
  Command
    { commandGen = \_model -> Just $ CFromByteString <$> Gen.bytes (Range.constant 0 10000)
    , commandExecute = \(CFromByteString value) -> do
        memoryRef <- asks envMemoryRef
        liftIO $ do
          memory <- memoryFromByteString value
          writeIORef memoryRef $ Just memory
    , commandCallbacks =
        [ label "from bytestring"
        , Require $ \model (CFromByteString _value) -> isNothing $ modelMemory model
        ]
    }

commands ::
  MonadGen gen =>
  (MonadReader Env m, MonadIO m) =>
  [Command gen m Model]
commands =
  [ cNew
  , cRead
  , cWrite
  , cSize
  , cTell
  , cSeek
  , cToByteString
  , cFromByteString
  ]

prop_memoryFileCorrect :: Property
prop_memoryFileCorrect = withTests 1000 . property $ do
  actions <- forAll $ Gen.sequential (Range.constant 0 100) initialModel commands
  hoist runWithEnv $ executeSequential initialModel actions
  where
    runWithEnv :: ReaderT Env IO a -> IO a
    runWithEnv m = do
      memoryRef <- newIORef Nothing
      let env = Env{envMemoryRef = memoryRef}
      runReaderT m env

main :: IO Bool
main = checkParallel $$discover
