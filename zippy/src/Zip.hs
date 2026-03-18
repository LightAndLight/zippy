module Zip
  ( -- * Reading
    Archive
  , withArchive
  , open
  , close
  , readContent
  , RepeatedEntryException (..)

    -- * Writing
  , ArchiveBuilder
  , create
  , new
  , finish_
  , addFile
  , addContent

    -- * Compression methods
  , Compression (..)
  , DeflateMode (..)
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Control.Exception (bracket, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.UTF8 as Utf8
import Zip.Archive
  ( Archive
  , ArchiveBuilder (..)
  , Compressed (..)
  , Compression (..)
  , DeflateMode (..)
  , File (..)
  , LocalFileHeaderData (..)
  , RepeatedEntryException (..)
  , addCompressedContent
  , archiveCentralDirectory
  , archiveFile
  , centralDirectoryHeaderLocalHeader
  , close
  , compress
  , fileRead
  , fileSeek
  , findInCentralDirectory
  , finish_
  , localFileHeaderCompressionMethod
  , localFileHeaderData
  , new
  , open
  )

-- | Read (and automatically 'close') a ZIP archive.
withArchive ::
  -- | Archive path
  FilePath ->
  (Archive -> IO a) ->
  IO a
withArchive path = bracket (open path) close

-- | Create (and automatically 'finish_') a ZIP archive.
create ::
  -- | Archive path
  FilePath ->
  (ArchiveBuilder -> IO a) ->
  IO a
create path f = do
  archive <- new path
  a <- f archive
  finish_ archive
  pure a

{-| Copy a file to the archive.

Uses the same compression strategy as 'addContent'.
-}
addFile ::
  {-| File name

  Preconditions:

  * @file name's length in bytes when UTF-8 encoded < 2^16@
  * @file's size < 2^32@
  -}
  FilePath ->
  ArchiveBuilder ->
  IO ()
addFile fileName archive = do
  content <- ByteString.readFile fileName
  addContent (Utf8.fromString fileName) content archive

{-| Add an entry to the archive.

Default-level @Deflate@ compression is used if the compressed content is shorter than the uncompressed data.
Otherwise, the content is stored as-is.
-}
addContent ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  -- | Uncompressed content (precondition: @length < 2^32@)
  ByteString ->
  ArchiveBuilder ->
  IO ()
addContent fileName content = do
  let compressed@(Compressed uncompressedSize crc32 _compression compressedContent) = compress (Deflate Normal) content
  if uncompressedSize > fromIntegral (ByteString.length compressedContent)
    then addCompressedContent fileName compressed
    else addCompressedContent fileName (Compressed uncompressedSize crc32 Stored content)

{-| Read an entry from the archive (if the entry can be found).

Throws a 'RepeatedEntryException' if there are multiple entries with the given file name.
-}
readContent ::
  -- | File name
  ByteString ->
  Archive ->
  IO (Maybe ByteString)
readContent fileName archive = do
  mCentralDirectoryHeader <- findInCentralDirectory fileName (archiveCentralDirectory archive)
  case mCentralDirectoryHeader of
    [] -> pure Nothing
    [centralDirectoryHeader] -> do
      localFileHeader <- centralDirectoryHeaderLocalHeader centralDirectoryHeader
      compressedContent <- do
        LocalFileHeaderData offset size <- localFileHeaderData localFileHeader
        fileSeek (archiveFile archive) offset
        fileRead (archiveFile archive) size
      compressionMethod <- localFileHeaderCompressionMethod localFileHeader
      uncompressedContent <-
        case compressionMethod of
          0 ->
            -- The file is stored (no compression)
            pure compressedContent
          8 ->
            -- The file is Deflated
            pure $!
              LazyByteString.toStrict
                (Zlib.decompressWith Zlib.defaultDecompressParams $ LazyByteString.fromStrict compressedContent)
          _ ->
            error $ "Compression method " ++ show compressionMethod ++ " not yet supported"
      pure $ Just uncompressedContent
    entries ->
      throwIO $ RepeatedEntryException fileName (fromIntegral $ length entries)
