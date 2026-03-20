module Zip
  ( -- * Reading
    Archive
  , withArchive
  , withArchiveFile
  , open
  , openFile
  , close
  , readContent
  , RepeatedEntryException (..)
  , readContents

    -- * Writing
  , ArchiveBuilder
  , create
  , createFile
  , new
  , newFile
  , finish_
  , addFile
  , addContent

    -- * Compression methods
  , Compression (..)
  , DeflateMode (..)

    -- * @File@ objects
  , File (..)
  , handleToFile
  )
where

import Control.Exception (bracket, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as Utf8
import Data.Traversable (for)
import Zip.Archive
  ( Archive
  , ArchiveBuilder (..)
  , Compressed (..)
  , Compression (..)
  , DeflateMode (..)
  , File (..)
  , RepeatedEntryException (..)
  , addCompressedContent
  , archiveCentralDirectory
  , centralDirectoryHeaderLocalHeader
  , close
  , compress
  , extract
  , findInCentralDirectory
  , finish_
  , handleToFile
  , new
  , newFile
  , open
  , openFile
  )

-- | Read (and automatically 'close') a ZIP archive from a 'File' object.
withArchiveFile :: File -> (Archive -> IO a) -> IO a
withArchiveFile file = bracket (openFile file) close

-- | Read (and automatically 'close') a ZIP archive from the file system.
withArchive ::
  -- | Archive path
  FilePath ->
  (Archive -> IO a) ->
  IO a
withArchive path = bracket (open path) close

-- | Create (and automatically 'finish_') a ZIP archive using a 'File' object.
createFile ::
  File ->
  (ArchiveBuilder -> IO a) ->
  IO a
createFile file f = do
  archive <- newFile file
  a <- f archive
  finish_ archive
  pure a

-- | Create (and automatically 'finish_') a ZIP archive on the file system.
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
  centralDirectoryHeaders <- findInCentralDirectory fileName (archiveCentralDirectory archive)
  case centralDirectoryHeaders of
    [] -> pure Nothing
    [centralDirectoryHeader] -> do
      localFileHeader <- centralDirectoryHeaderLocalHeader centralDirectoryHeader
      uncompressedContent <- extract localFileHeader
      pure $ Just uncompressedContent
    entries ->
      throwIO $ RepeatedEntryException fileName (fromIntegral $ length entries)

{-| Read all entries from the archive with the given file name.

Provided for completeness. Most ZIP archives do not have duplicate entries, so 'readContent' is
generally enough.
-}
readContents ::
  -- | File name
  ByteString ->
  Archive ->
  IO [ByteString]
readContents fileName archive = do
  centralDirectoryHeaders <- findInCentralDirectory fileName (archiveCentralDirectory archive)
  for centralDirectoryHeaders $ \centralDirectoryHeader -> do
    localFileHeader <- centralDirectoryHeaderLocalHeader centralDirectoryHeader
    uncompressedContent <- extract localFileHeader
    pure uncompressedContent
