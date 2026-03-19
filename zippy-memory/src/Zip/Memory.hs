{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Zip.Memory
  ( memoryToFile
  , Memory
  , memoryNew
  , memoryFromByteString
  , memoryRead
  , memoryWrite
  , memoryTell
  , memorySeek
  , memorySize
  , memoryToByteString
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (ByteString (..))
import Data.Foldable (for_)
import Data.Word (Word64)
import Foreign (peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import GHC.Exts
  ( Int (..)
  , Int#
  , MutVar#
  , MutableByteArray#
  , RealWorld
  , Word64#
  , getSizeofMutableByteArray#
  , gtWord64#
  , int2Word#
  , newByteArray#
  , newMutVar#
  , plusWord64#
  , readMutVar#
  , readWord64Array#
  , readWord8Array#
  , resizeMutableByteArray#
  , word2Int#
  , word64ToWord#
  , wordToWord64#
  , writeMutVar#
  , writeWord64Array#
  , writeWord8Array#
  )
import GHC.IO (IO (..), unIO)
import GHC.Stack (HasCallStack)
import GHC.Word (Word64 (..), Word8 (..))
import Zip.Archive (File (..))

memoryToFile :: Memory -> File
memoryToFile memory =
  File
    { fileRead = memoryRead memory
    , fileWrite = memoryWrite memory
    , fileSeek = memorySeek memory
    , fileTell = memoryTell memory
    , fileSize = memorySize memory
    , fileClose = pure ()
    }

data Memory
  = Memory
  { memoryBufferVar :: !(MutVar# RealWorld (MutableByteArray# RealWorld))
  -- ^ Mutable, resizable memory
  , memorySeekVar :: !(MutableByteArray# RealWorld)
  -- ^ Seek position (size: 8 bytes)
  }

intToWord64# :: Int# -> Word64#
intToWord64# x = wordToWord64# (int2Word# x)

word64ToInt# :: Word64# -> Int#
word64ToInt# x = word2Int# (word64ToWord# x)

-- | Create some seekable memory.
memoryNew ::
  -- Postcondition: seek position is 0
  IO Memory
memoryNew = IO $ \s0 ->
  let
    !(# s1, buffer #) = newByteArray# 0# s0
    !(# s2, bufferVar #) = newMutVar# buffer s1
    !(# s3, seekVar #) = newByteArray# 8# s2
    s4 = writeWord64Array# seekVar 0# (intToWord64# 0#) s3
  in
    (# s4, Memory bufferVar seekVar #)

-- | Initialise some seekable memory from a 'ByteString'.
memoryFromByteString ::
  ByteString ->
  {-| Postconditions:

  * Seek position is 0
  * Size is the length of the input 'ByteString'
  -}
  IO Memory
memoryFromByteString (BS fptr size@(I# size#)) = IO $ \s0 ->
  let
    !(# s1, buffer #) = newByteArray# size# s0
    !(# s2, bufferVar #) = newMutVar# buffer s1
    !(# s3, seekVar #) = newByteArray# 8# s2
    s4 = writeWord64Array# seekVar 0# (intToWord64# 0#) s3
    !(# s5, () #) =
      unIO
        ( withForeignPtr fptr $ \ptr ->
            for_ [0 .. size - 1] $ \index@(I# index#) -> do
              W8# byte <- peekElemOff ptr index
              IO $ \s00 ->
                let s01 = writeWord8Array# buffer index# byte s00
                in (# s01, () #)
        )
        s4
  in
    (# s5, Memory bufferVar seekVar #)

-- | Read (up to) the given number of bytes from the current seek position.
memoryRead ::
  Memory ->
  Word64 ->
  {-| Postcondition:

  * Seek position is advanced by the number of bytes read
  -}
  IO ByteString
memoryRead memory@(Memory bufferVar _seekVar) count = IO $ \s0 ->
  let
    !(# s1, buffer #) = readMutVar# bufferVar s0
    !(# s2, size# #) = getSizeofMutableByteArray# buffer s1
  in
    case size# of
      0# -> (# s2, ByteString.empty #)
      _ ->
        unIO
          ( do
              pos <- memoryTell memory
              let actualCount = min count (W64# (intToWord64# size#) - pos)
              if actualCount == 0
                then pure ByteString.empty
                else do
                  fptr <- mallocForeignPtrBytes $ fromIntegral actualCount
                  withForeignPtr fptr $ \ptr ->
                    for_ [0 .. actualCount - 1] $ \index -> do
                      byte <-
                        IO $ \s00 ->
                          let
                            !(I# index#) = fromIntegral $ pos + index
                            !(# s01, byte# #) = readWord8Array# buffer index# s00
                          in
                            (# s01, W8# byte# #)
                      pokeElemOff ptr (fromIntegral index) byte
                  memorySeek memory $ pos + actualCount
                  pure $ BS fptr (fromIntegral actualCount)
          )
          s2

-- | Write a value at the current seek position.
memoryWrite ::
  Memory ->
  -- | Value
  ByteString ->
  {-| Postconditions:

  * Seek position is advanced by the number of bytes written
  * The underlying buffer's size is at least @original seek position + value's size@
  -}
  IO ()
memoryWrite memory@(Memory bufferVar _seekVar) value = IO $ \s0 ->
  let
    !(# s1, buffer #) = readMutVar# bufferVar s0

    !(# s2, bufferSize #) = getSizeofMutableByteArray# buffer s1
    bufferSize# = intToWord64# bufferSize

    !(# s3, W64# pos# #) = unIO (memoryTell memory) s2
    !(W64# valueSize#) = fromIntegral @Int @Word64 $ ByteString.length value
  in
    case word64ToInt# valueSize# of
      0# -> (# s3, () #)
      _ ->
        let
          minRequiredSize# = pos# `plusWord64#` valueSize#

          !(# s6, buffer' #) =
            case minRequiredSize# `gtWord64#` bufferSize# of
              0# ->
                -- value fits in current buffer
                (# s2, buffer #)
              _ ->
                -- buffer needs to be grown
                let
                  !(# s4, buffer'' #) = resizeMutableByteArray# buffer (word64ToInt# minRequiredSize#) s3
                  s5 = writeMutVar# bufferVar buffer'' s4
                in
                  (# s5, buffer'' #)
        in
          unIO
            ( do
                for_ [0 .. W64# valueSize# - 1] $ \index -> do
                  IO $ \s00 ->
                    let
                      !(W8# byte#) = ByteString.index value $ fromIntegral index
                      !(I# index#) = fromIntegral $ W64# pos# + index
                      s01 = writeWord8Array# buffer' index# byte# s00
                    in
                      (# s01, () #)
                memorySeek memory $ W64# (pos# `plusWord64#` valueSize#)
            )
            s6

-- | Set the seek position.
memorySeek ::
  HasCallStack =>
  Memory ->
  {-| Seek position

  Precondition: @seek position <= size@
  -}
  Word64 ->
  IO ()
memorySeek memory@(Memory _bufferVar seekVar) pos@(W64# pos#) = do
  size <- memorySize memory
  if pos > size
    then error $ "seek destination " ++ show pos ++ " exceeds size " ++ show size
    else IO $ \s0 ->
      let !s1 = writeWord64Array# seekVar 0# pos# s0
      in (# s1, () #)

-- | Report the seek position.
memoryTell :: Memory -> IO Word64
memoryTell (Memory _bufferVar seekVar) =
  IO $ \s0 ->
    let !(# s1, pos #) = readWord64Array# seekVar 0# s0
    in (# s1, W64# pos #)

memorySize :: Memory -> IO Word64
memorySize (Memory bufferVar _seekVar) =
  IO $ \s0 ->
    let
      !(# s1, buffer #) = readMutVar# bufferVar s0
      !(# s2, size #) = getSizeofMutableByteArray# buffer s1
    in
      (# s2, W64# (intToWord64# size) #)

-- | Copy the current contents to a 'ByteString'.
memoryToByteString :: Memory -> IO ByteString
memoryToByteString memory@(Memory bufferVar _seekVar) =
  IO $ \s0 ->
    let !(# s1, buffer #) = readMutVar# bufferVar s0
    in unIO
         ( do
             size <- fromIntegral <$> memorySize memory
             fptr <- mallocForeignPtrBytes size
             withForeignPtr fptr $ \ptr -> do
               for_ [0 .. size - 1] $ \index@(I# index#) -> do
                 byte <-
                   IO $ \s00 ->
                     let !(# s01, byte# #) = readWord8Array# buffer index# s00
                     in (# s01, W8# byte# #)

                 pokeElemOff ptr index byte
             pure $ BS fptr size
         )
         s1
