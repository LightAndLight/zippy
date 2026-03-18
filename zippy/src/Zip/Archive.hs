{-# LANGUAGE RankNTypes #-}

{-| Low-level ZIP archive manipulation

Reference: <https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT>
-}
module Zip.Archive
  ( -- * Reading
    Archive (..)
  , open
  , close

    -- ** Central directory
  , CentralDirectory (..)
  , findInCentralDirectory
  , listFileNames

    -- *** Central directory header
  , CentralDirectoryHeader (..)
  , centralDirectoryHeaderLocalHeader

    -- ** Local file header
  , LocalFileHeader (..)
  , localFileHeaderCompressionMethod
  , localFileHeaderData
  , LocalFileHeaderData (..)

    -- * Writing
  , ArchiveBuilder (..)
  , Change (..)
  , new
  , finish_
  , add
  , ContentSummary (..)
  , addCompressedContent

    -- ** Entry metadata
  , FileInfo (..)

    -- * Compression
  , Compression (..)
  , DeflateMode (..)
  , deflateModeCompressionLevel
  , compress
  , Compressed (..)

    -- * File operations
  , File (..)
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (unless)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Digest.CRC32 as CRC32
import Data.Foldable (foldlM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word16, Word32, Word64)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import System.IO
  ( Handle
  , IOMode (..)
  , SeekMode (..)
  , hClose
  , hFileSize
  , hSeek
  , hSetFileSize
  , hTell
  , openFile
  )

data ArchiveBuilder
  = ArchiveBuilder
  { archiveBuilderFile :: !File
  , archiveBuilderChanges :: !(IORef (DList Change))
  }

{-| Create an (incomplete) archive for writing.

'finish_' must be called on the 'ArchiveBuilder' to produce a valid archive.
-}
new :: FilePath -> IO ArchiveBuilder
new path = do
  handle <- openFile path ReadWriteMode
  hSetFileSize handle 0
  changesRef <- newIORef DList.empty
  pure
    ArchiveBuilder
      { archiveBuilderFile = handleToFile handle
      , archiveBuilderChanges = changesRef
      }

data Change
  = Added
      -- | Local header offset
      !Word32
      !FileInfo
      -- | File name
      !ByteString

-- | Fields common to @local file header@ and @central directory header@.
data FileInfo
  = FileInfo
  { fileInfoVersionNeededToExtract :: !Word16
  , fileInfoGeneralPurposeBitFlag :: !Word16
  , fileInfoCompressionMethod :: !Word16
  , fileInfoLastModFileTime :: !Word16
  , fileInfoLastModFileDate :: !Word16
  , fileInfoCrc32 :: !Word32
  , fileInfoCompressedSize :: !Word32
  , fileInfoUncompressedSize :: !Word32
  , fileInfoFileNameLength :: !Word16
  , fileInfoExtraFieldLength :: !Word16
  }

writeFileInfo :: FileInfo -> ArchiveBuilder -> IO ()
writeFileInfo fileInfo archive = do
  writeLE16 archive $ fileInfoVersionNeededToExtract fileInfo
  writeLE16 archive $ fileInfoGeneralPurposeBitFlag fileInfo
  writeLE16 archive $ fileInfoCompressionMethod fileInfo
  writeLE16 archive $ fileInfoLastModFileTime fileInfo
  writeLE16 archive $ fileInfoLastModFileDate fileInfo
  writeLE32 archive $ fileInfoCrc32 fileInfo
  writeLE32 archive $ fileInfoCompressedSize fileInfo
  writeLE32 archive $ fileInfoUncompressedSize fileInfo
  writeLE16 archive $ fileInfoFileNameLength fileInfo
  writeLE16 archive $ fileInfoExtraFieldLength fileInfo

data Compression
  = -- | No compression
    Stored
  | Deflate !DeflateMode
  deriving (Show, Eq)

data DeflateMode
  = Normal
  | Maximum
  | Fast
  | SuperFast
  deriving (Show, Eq)

deflateModeCompressionLevel :: DeflateMode -> Zlib.CompressionLevel
deflateModeCompressionLevel mode =
  case mode of
    Normal -> Zlib.defaultCompression
    Maximum -> Zlib.CompressionLevel 9
    Fast -> Zlib.CompressionLevel 3 -- What do other implementations use?
    SuperFast -> Zlib.CompressionLevel 1

data Compressed
  = Compressed
      -- | Uncompressed size (precondition: @length < 2^32@)
      !Word64
      -- | CRC-32 of uncompressed content
      !Word32
      -- | Compression used
      !Compression
      -- | Compressed content (precondition: @length < 2^32@)
      !ByteString

compress ::
  Compression ->
  -- | Uncompressed data
  ByteString ->
  Compressed
compress compression content =
  Compressed uncompressedSize (CRC32.crc32 content) compression $
    case compression of
      Stored -> content
      Deflate mode ->
        let compressionParams =
              Zlib.defaultCompressParams
                { Zlib.compressLevel = deflateModeCompressionLevel mode
                , Zlib.compressMethod = Zlib.deflateMethod
                }
        in LazyByteString.toStrict (Zlib.compressWith compressionParams $ LazyByteString.fromStrict content)
  where
    uncompressedSize = fromIntegral (ByteString.length content) :: Word64

data ContentSummary
  = ContentSummary
      -- | Uncompressed size
      !Word64
      -- | CRC-32
      !Word32
      -- | Compression method
      !Compression
      -- | Compressed size
      !Word64

add ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  {-| Action that writes content to the archive.

  When the action is run, the file is already at the correct seek position for writing the new entry.

  Arguments:

  * Function that writes to the archive (and advances seek position)
  -}
  ((ByteString -> IO ()) -> IO ContentSummary) ->
  ArchiveBuilder ->
  IO ()
add fileName m archive = do
  localHeaderOffset <- do
    offset <- fileTell $ archiveBuilderFile archive
    if offset < 2 ^ (32 :: Int)
      then pure (fromIntegral offset :: Word32)
      else error $ "ZIP file is too large (ZIP64 not yet supported)"

  fileNameLength <- do
    let len = ByteString.length fileName
    if len < 2 ^ (16 :: Int)
      then pure (fromIntegral len :: Word16)
      else error $ "ZIP file entry's file name is too large (ZIP64 not yet supported)"

  let extraFieldLength = 0

  fileSeek (archiveBuilderFile archive) $
    fromIntegral localHeaderOffset + localFileHeaderSize fileNameLength extraFieldLength

  ContentSummary uncompressedSize crc32 compression compressedSize <-
    m $ fileWrite (archiveBuilderFile archive)
  nextOffset <- fileTell $ archiveBuilderFile archive

  uncompressedSize' <- do
    if uncompressedSize < 2 ^ (32 :: Int)
      then pure (fromIntegral uncompressedSize :: Word32)
      else error $ "ZIP file entry's uncompressed content is too large (ZIP64 not yet supported)"

  compressedSize' <- do
    if compressedSize < 2 ^ (32 :: Int)
      then pure (fromIntegral compressedSize :: Word32)
      else error $ "ZIP file entry's compressed content is too large (ZIP64 not yet supported)"

  let
    fileInfo =
      FileInfo
        { fileInfoVersionNeededToExtract = 0xFF00
        , fileInfoGeneralPurposeBitFlag =
            -- Bit 15: Reserved by PKWARE.
            (0 `shiftL` 15)
              .|.
              -- Bit 14: Reserved by PKWARE for alternate streams.
              (0 `shiftL` 14)
              .|.
              -- Bit 13: Set when encrypting the Central Directory to indicate selected data values in the Local Header are masked to hide their actual values.
              (0 `shiftL` 13)
              .|.
              -- Bit 12: Reserved by PKWARE for enhanced compression.
              (0 `shiftL` 12)
              .|.
              -- Bit 11: Language encoding flag (EFS).  If this bit is set, the filename and comment fields for this file MUST be encoded using UTF-8.
              (0 `shiftL` 11)
              .|.
              -- Bit 10: Currently unused.
              (0 `shiftL` 10)
              .|.
              -- Bit 9: Currently unused.
              (0 `shiftL` 9)
              .|.
              -- Bit 8: Currently unused.
              (0 `shiftL` 8)
              .|.
              -- Bit 7: Currently unused.
              (0 `shiftL` 7)
              .|.
              -- Bit 6: Strong encryption.
              (0 `shiftL` 6)
              .|.
              -- Bit 5: If this bit is set, this indicates that the file is compressed patched data.
              (0 `shiftL` 5)
              .|.
              -- Bit 4: Reserved for use with method 8, for enhanced deflating.
              (0 `shiftL` 4)
              .|.
              -- Bit 3: If this bit is set, the fields crc-32, compressed size and uncompressed size are set to zero in the local header.
              (0 `shiftL` 3)
              .|. ( case compression of
                      Stored -> 0
                      Deflate mode ->
                        {- (For Methods 8 and 9 - Deflating)

                            Bit 2  Bit 1
                        -}
                        case mode of
                          -- 0      0    Normal (-en) compression option was used.
                          Normal -> (0 `shiftL` 2) .|. (0 `shiftL` 1)
                          -- 0      1    Maximum (-exx/-ex) compression option was used.
                          Maximum -> (0 `shiftL` 2) .|. (1 `shiftL` 1)
                          -- 1      0    Fast (-ef) compression option was used.
                          Fast -> (1 `shiftL` 2) .|. (0 `shiftL` 1)
                          -- 1      1    Super Fast (-es) compression option was used.
                          SuperFast -> (1 `shiftL` 2) .|. (1 `shiftL` 1)
                  )
              .|.
              -- Bit 0: If set, indicates that the file is encrypted.
              0
        , fileInfoCompressionMethod =
            case compression of
              Stored -> 0
              Deflate{} -> 8
        , fileInfoLastModFileTime = 0
        , fileInfoLastModFileDate = 0
        , fileInfoCrc32 = crc32
        , fileInfoCompressedSize = compressedSize'
        , fileInfoUncompressedSize = uncompressedSize'
        , fileInfoFileNameLength = fileNameLength
        , fileInfoExtraFieldLength = 0
        }

  fileSeek (archiveBuilderFile archive) $ fromIntegral localHeaderOffset
  writeLocalFileHeader fileInfo fileName archive

  fileSeek (archiveBuilderFile archive) nextOffset

  let change = Added localHeaderOffset fileInfo fileName
  modifyIORef' (archiveBuilderChanges archive) (`DList.snoc` change)

addCompressedContent ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  -- | Compressed content
  Compressed ->
  ArchiveBuilder ->
  IO ()
addCompressedContent fileName (Compressed uncompressedSize crc32 compression compressedContent) =
  add fileName $ \write -> do
    compressedSize <- pure $! fromIntegral (ByteString.length compressedContent)
    write compressedContent
    pure $ ContentSummary uncompressedSize crc32 compression compressedSize

writeLocalFileHeader ::
  FileInfo ->
  -- | File name
  ByteString ->
  ArchiveBuilder ->
  IO ()
writeLocalFileHeader fileInfo fileName archive = do
  -- local file header signature
  writeLE32 archive 0x04034b50

  writeFileInfo fileInfo archive

  -- file name
  fileWrite (archiveBuilderFile archive) fileName

  -- extra field
  pure ()

{-| Complete the construction of a ZIP archive.

Also closes the underlying 'File'.
-}
finish_ :: ArchiveBuilder -> IO ()
finish_ archive = do
  centralDirectoryOffset <- do
    pos <- fileTell $ archiveBuilderFile archive
    if pos < 2 ^ (32 :: Int)
      then pure (fromIntegral pos :: Word32)
      else error "Central directory offset is too large (ZIP64 not yet supported)"

  changes <- fmap DList.toList . readIORef $ archiveBuilderChanges archive
  centralDirectoryCount <- do
    count <-
      foldlM
        ( \acc change -> do
            case change of
              Added localHeaderOffset fileInfo fileName -> do
                writeCentralDirectoryHeader localHeaderOffset fileInfo fileName archive
                pure (acc + 1 :: Word64)
        )
        0
        changes

    if count < 2 ^ (16 :: Int)
      then pure (fromIntegral count :: Word16)
      else error "Central directory has too many entries (ZIP64 not yet supported)"

  endOfCentralDirectoryOffset <- do
    pos <- fileTell $ archiveBuilderFile archive
    if pos < 2 ^ (32 :: Int)
      then pure (fromIntegral pos :: Word32)
      else error "End-of-central-directory offset is too large (ZIP64 not yet supported)"
  let centralDirectorySize = endOfCentralDirectoryOffset - centralDirectoryOffset

  writeEndOfCentralDirectoryRecord
    centralDirectoryCount
    centralDirectorySize
    centralDirectoryOffset
    archive
  fileClose $ archiveBuilderFile archive

writeCentralDirectoryHeader ::
  -- | Local header offset
  Word32 ->
  FileInfo ->
  -- | File name
  ByteString ->
  ArchiveBuilder ->
  IO ()
writeCentralDirectoryHeader localHeaderOffset fileInfo fileName archive = do
  -- central file header signature
  writeLE32 archive 0x02014b50

  -- version made by
  writeLE16 archive 0

  writeFileInfo fileInfo archive

  -- file comment length
  writeLE16 archive 0

  -- disk number start
  writeLE16 archive 0

  -- internal file attributes
  writeLE16 archive 0

  -- external file attributes
  writeLE32 archive 0

  -- relative offset of local header
  writeLE32 archive localHeaderOffset

  -- file name
  fileWrite (archiveBuilderFile archive) fileName

  -- extra field
  pure ()

  -- file comment
  pure ()

writeEndOfCentralDirectoryRecord ::
  -- | Number of entries in the central directory
  Word16 ->
  -- | Size of the central directory
  Word32 ->
  -- | Offset of the central directory
  Word32 ->
  ArchiveBuilder ->
  IO ()
writeEndOfCentralDirectoryRecord centralDirectoryCount centralDirectorySize centralDirectoryOffset archive = do
  -- end of central dir signature
  writeLE32 archive 0x06054b50

  -- number of this disk
  writeLE16 archive 0

  -- number of the disk with the start of the central directory
  writeLE16 archive 0

  -- total number of entries in the central directory on this disk
  writeLE16 archive centralDirectoryCount

  -- total number of entries in the central directory
  writeLE16 archive centralDirectoryCount

  -- size of the central directory
  writeLE32 archive centralDirectorySize

  -- offset of start of central directory with respect to the starting disk number
  writeLE32 archive centralDirectoryOffset

  -- .ZIP file comment length
  writeLE16 archive 0

  -- .ZIP file comment
  pure ()

data Archive
  = Archive
  { archiveFile :: !File
  , archiveCentralDirectory :: !CentralDirectory
  }

data CentralDirectory
  = CentralDirectory
  { centralDirectoryFile :: !File
  , centralDirectoryOffset :: !Word64
  , centralDirectorySize :: !Word64
  , centralDirectoryCount :: !Word64
  }

findInCentralDirectory ::
  -- | File name
  ByteString ->
  CentralDirectory ->
  IO (Maybe CentralDirectoryHeader)
findInCentralDirectory fileName centralDirectory = do
  let count = centralDirectoryCount centralDirectory
  let file = centralDirectoryFile centralDirectory
  let base = centralDirectoryOffset centralDirectory
  go count file base
  where
    go count file base = do
      signature <- do
        fileSeek file base
        readLE32 file
      unless (signature == 0x02014b50) . error $
        "missing central file header signature at offset " ++ show base

      fileNameLength <- do
        fileSeek file $
          base
            +
            -- `file name length` field
            (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4)
        readLE16 file
      extraFieldLength <- readLE16 file
      fileCommentLength <- readLE16 file

      fileName' <- do
        fileSeek file $
          base
            +
            -- `file name` field
            (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4)
        fileRead file (fromIntegral fileNameLength)

      if fileName == fileName'
        then do
          pure $ Just (CentralDirectoryHeader file base)
        else
          if count > 1
            then do
              let
                base' =
                  base
                    +
                    -- `file name` field
                    (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4)
                    + fromIntegral fileNameLength
                    + fromIntegral extraFieldLength
                    + fromIntegral fileCommentLength
              go (count - 1) file base'
            else
              pure Nothing

listFileNames ::
  CentralDirectory ->
  IO [ByteString]
listFileNames centralDirectory = do
  let count = centralDirectoryCount centralDirectory
  let file = centralDirectoryFile centralDirectory
  let base = centralDirectoryOffset centralDirectory
  go count file base
  where
    go count file base = do
      signature <- fileSeek file base *> readLE32 file
      unless (signature == 0x02014b50) . error $
        "missing central file header signature at offset " ++ show base

      fileNameLength <- do
        fileSeek file $
          base
            +
            -- `file name length` field
            (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4)
        readLE16 file
      extraFieldLength <- readLE16 file
      fileCommentLength <- readLE16 file

      fileName' <- do
        fileSeek file $
          base
            +
            -- `file name` field
            (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4)
        fileRead file (fromIntegral fileNameLength)

      (:) fileName'
        <$> if count > 1
          then do
            let
              base' =
                base
                  +
                  -- `file name` field
                  (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4)
                  + fromIntegral fileNameLength
                  + fromIntegral extraFieldLength
                  + fromIntegral fileCommentLength
            go (count - 1) file base'
          else
            pure []

data CentralDirectoryHeader
  = CentralDirectoryHeader
  { centralDirectoryHeaderFile :: !File
  , centralDirectoryHeaderOffset :: !Word64
  }

centralDirectoryHeaderLocalHeader :: CentralDirectoryHeader -> IO LocalFileHeader
centralDirectoryHeaderLocalHeader (CentralDirectoryHeader file offset) = do
  fileSeek file $
    offset
      +
      -- `relative offset of local header` field
      (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4)

  offset' <- readLE32 file

  fileSeek file $ fromIntegral offset'
  signature <- readLE32 file
  unless (signature == 0x04034b50) . error $
    "missing local file header signature at offset " ++ show offset'

  pure $ LocalFileHeader file (fromIntegral offset')

data LocalFileHeader
  = LocalFileHeader {localFileHeaderFile :: !File, localFileHeaderOffset :: !Word64}

localFileHeaderSize ::
  -- | File name length
  Word16 ->
  -- | Extra field length
  Word16 ->
  Word64
localFileHeaderSize fileNameLength extraFieldLength =
  4
    + 2
    + 2
    + 2
    + 2
    + 2
    + 4
    + 4
    + 4
    + 2
    + 2
    + fromIntegral fileNameLength
    + fromIntegral extraFieldLength

localFileHeaderCompressionMethod :: LocalFileHeader -> IO Word16
localFileHeaderCompressionMethod (LocalFileHeader file offset) = do
  fileSeek file $
    offset
      +
      -- `compression method` field
      (4 + 2 + 2)
  readLE16 file

localFileHeaderData :: LocalFileHeader -> IO LocalFileHeaderData
localFileHeaderData (LocalFileHeader file offset) = do
  fileSeek file $
    offset
      +
      -- `file name length` field
      (4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4)
  fileNameLength <- readLE16 file
  extraFieldLength <- readLE16 file

  compressedSize <- do
    fileSeek file $
      offset
        +
        -- `compressed size` field
        (4 + 2 + 2 + 2 + 2 + 2 + 4)
    readLE32 file

  pure $
    LocalFileHeaderData
      ( offset
          +
          -- data
          ( 4
              + 2
              + 2
              + 2
              + 2
              + 2
              + 4
              + 4
              + 4
              + 2
              + 2
              + fromIntegral fileNameLength
              + fromIntegral extraFieldLength
          )
      )
      (fromIntegral compressedSize)

data LocalFileHeaderData
  = LocalFileHeaderData
  { localFileHeaderDataOffset :: !Word64
  , localFileHeaderDataSize :: !Word64
  }

data File
  = File
  { fileRead :: Word64 -> IO ByteString
  {- ^ Read the given number of bytes from the current seek position.

  Seek position is advanced by the number of bytes read.
  -}
  , fileWrite :: ByteString -> IO ()
  {- ^ Write the value to the current seek position.

  Seek position is advanced by the number of bytes written.
  -}
  , fileSeek :: HasCallStack => Word64 -> IO ()
  -- ^ Set the seek position.
  , fileTell :: IO Word64
  -- ^ Get the seek position.
  , fileSize :: IO Word64
  -- ^ Get the file's size.
  , fileClose :: IO ()
  -- ^ Close the file.
  }

handleToFile :: Handle -> File
handleToFile handle =
  File
    { fileRead = ByteString.hGet handle . fromIntegral
    , fileWrite = ByteString.hPut handle
    , fileSeek = withFrozenCallStack (withCallStack . hSeek handle AbsoluteSeek . fromIntegral)
    , fileTell = fromIntegral <$> hTell handle
    , fileSize = fromIntegral <$> hFileSize handle
    , fileClose = hClose handle
    }

data WithCallStack = WithCallStack CallStack SomeException

instance Show WithCallStack where
  show (WithCallStack cs err) =
    show err
      ++ "\n"
      ++ unlines (fmap ("  " ++) (lines $ prettyCallStack cs))

instance Exception WithCallStack

withCallStack :: HasCallStack => IO a -> IO a
withCallStack ma = do
  result <- try ma
  case result of
    Left err -> throwIO $ WithCallStack callStack err
    Right a -> pure a

data ZipException
  = EndOfCentralDirectoryRecordNotFound
  deriving (Show, Eq)

instance Exception ZipException

open :: FilePath -> IO Archive
open path = do
  handle <- openFile path ReadMode
  let file = handleToFile handle

  mEndOfCentralDirectoryRecord <- findEndOfCentralDirectoryRecord file
  case mEndOfCentralDirectoryRecord of
    Nothing ->
      throwIO EndOfCentralDirectoryRecordNotFound
    Just endOfCentralDirectoryRecord -> do
      offset <- endOfCentralDirectoryRecordCentralDirectoryOffset endOfCentralDirectoryRecord
      size <- endOfCentralDirectoryRecordCentralDirectorySize endOfCentralDirectoryRecord
      count <- endOfCentralDirectoryRecordCentralDirectoryCount endOfCentralDirectoryRecord
      pure
        Archive
          { archiveFile = file
          , archiveCentralDirectory =
              CentralDirectory
                { centralDirectoryFile = file
                , centralDirectoryOffset = fromIntegral offset
                , centralDirectorySize = fromIntegral size
                , centralDirectoryCount = fromIntegral count
                }
          }
  where
    chunkSize :: Word64
    chunkSize = 512

    findEndOfCentralDirectoryRecord :: File -> IO (Maybe EndOfCentralDirectoryRecord)
    findEndOfCentralDirectoryRecord file = do
      size <- fileSize file

      let
        chunkOffset
          -- read the entire file
          | chunkSize > size = 0
          -- read a chunk from the end of the file
          | otherwise = size - chunkSize

        index
          | chunkSize > size = size - 1
          | otherwise = chunkSize - 1

      fileSeek file chunkOffset
      chunk <- fileRead file chunkSize
      step file size chunkOffset chunk index

    step ::
      File ->
      -- \| File size
      Word64 ->
      -- \| Chunk offset
      Word64 ->
      -- \| Chunk
      ByteString ->
      -- \| Index in chunk
      Word64 ->
      IO (Maybe EndOfCentralDirectoryRecord)
    step file size chunkOffset chunk index =
      if index < 4
        then
          -- signature cannot be (completely) in this chunk
          nextChunk file size chunkOffset
        else
          if ByteString.index chunk (fromIntegral index) == 0x06
            && ByteString.index chunk (fromIntegral $ index - 1) == 0x05
            && ByteString.index chunk (fromIntegral $ index - 2) == 0x4b
            && ByteString.index chunk (fromIntegral $ index - 3) == 0x50
            then do
              {- This *may* be the signature.

              Confirm by looking up the `.ZIP file comment length`, then checking that the
              `.ZIP file comment` extends to the end of the file.
              -}
              let candidateSignatureOffset = chunkOffset + index - 3
              zipFileCommentLength <- do
                fileSeek file $
                  candidateSignatureOffset
                    +
                    -- `.ZIP file comment length` field
                    (4 + 2 + 2 + 2 + 2 + 4 + 4)
                readLE16 file
              zipFileCommentOffset <- fileTell file
              if zipFileCommentOffset + fromIntegral zipFileCommentLength == size
                then
                  pure $ Just (EndOfCentralDirectoryRecord file candidateSignatureOffset)
                else
                  nextChunk file size chunkOffset
            else
              step file size chunkOffset chunk (index - 1)

    nextChunk ::
      File ->
      -- \| File size
      Word64 ->
      -- \| Chunk offset
      Word64 ->
      IO (Maybe EndOfCentralDirectoryRecord)
    nextChunk file size chunkOffset
      | chunkOffset < chunkSize =
          pure Nothing
      | otherwise = do
          {-
          We didn't examine the first 3 bytes of the chunk, because they can't
          form the 4-byte signature. So the next chunk we examine should end
          with these 3 bytes so we *can* examine them.
          -}
          let chunkOffset' = chunkOffset - chunkSize + 3
          fileSeek file chunkOffset'
          chunk' <- fileRead file chunkSize
          step file size chunkOffset' chunk' (chunkSize - 1)

close :: Archive -> IO ()
close archive = fileClose $ archiveFile archive

data EndOfCentralDirectoryRecord
  = EndOfCentralDirectoryRecord
  { endOfCentralDirectoryRecordFile :: !File
  , endOfCentralDirectoryRecordOffset :: !Word64
  }

endOfCentralDirectoryRecordCentralDirectoryCount :: EndOfCentralDirectoryRecord -> IO Word16
endOfCentralDirectoryRecordCentralDirectoryCount (EndOfCentralDirectoryRecord file offset) = do
  fileSeek file $ offset + (4 + 2 + 2 + 2)
  readLE16 file

endOfCentralDirectoryRecordCentralDirectorySize :: EndOfCentralDirectoryRecord -> IO Word32
endOfCentralDirectoryRecordCentralDirectorySize (EndOfCentralDirectoryRecord file offset) = do
  fileSeek file $ offset + (4 + 2 + 2 + 2 + 2)
  readLE32 file

endOfCentralDirectoryRecordCentralDirectoryOffset :: EndOfCentralDirectoryRecord -> IO Word32
endOfCentralDirectoryRecordCentralDirectoryOffset (EndOfCentralDirectoryRecord file offset) = do
  fileSeek file $ offset + (4 + 2 + 2 + 2 + 2 + 4)
  readLE32 file

-- Little-endian decoding

readLE16 :: File -> IO Word16
readLE16 file = do
  bytes <- fileRead file 2
  pure $!
    fromIntegral (ByteString.index bytes 0)
      .|. fromIntegral (ByteString.index bytes 1) `shiftL` 8

readLE32 :: File -> IO Word32
readLE32 file = do
  bytes <- fileRead file 4
  pure $!
    fromIntegral (ByteString.index bytes 0)
      .|. fromIntegral (ByteString.index bytes 1) `shiftL` 8
      .|. fromIntegral (ByteString.index bytes 2) `shiftL` 16
      .|. fromIntegral (ByteString.index bytes 3) `shiftL` 24

-- Little-endian encoding

writeLE16 :: ArchiveBuilder -> Word16 -> IO ()
writeLE16 archive value = do
  let file = archiveBuilderFile archive
  fileWrite file . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  fileWrite file . ByteString.singleton . fromIntegral $
    (0x0000FF00 .&. value) `shiftR` 8

writeLE32 :: ArchiveBuilder -> Word32 -> IO ()
writeLE32 archive value = do
  let file = archiveBuilderFile archive
  fileWrite file . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  fileWrite file . ByteString.singleton . fromIntegral $
    (0x0000FF00 .&. value) `shiftR` 8
  fileWrite file . ByteString.singleton . fromIntegral $
    (0x00FF0000 .&. value) `shiftR` 16
  fileWrite file . ByteString.singleton . fromIntegral $
    (0xFF000000 .&. value) `shiftR` 24
