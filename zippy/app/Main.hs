module Main where

import Control.Applicative (many, (<**>))
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.UTF8 as Utf8
import Data.Foldable (for_, traverse_)
import qualified Options.Applicative as Options
import System.Exit (exitFailure)
import qualified Zip
import qualified Zip.Archive
import Prelude hiding (read)

data Cli
  = Create
      -- | Input files
      [FilePath]
      -- | Output file
      FilePath
  | Read
      -- | File to read from archive
      String
      -- | Input archive
      FilePath
  | List
      -- | Input archive
      FilePath

cliParser :: Options.Parser Cli
cliParser =
  Options.hsubparser
    ( Options.command "create" (Options.info createParser Options.fullDesc)
        <> Options.command "read" (Options.info readParser Options.fullDesc)
        <> Options.command "list" (Options.info listParser Options.fullDesc)
    )
  where
    createParser =
      Create
        <$> many (Options.strArgument $ Options.metavar "FILE" <> Options.help "Input file(s)")
        <*> Options.strOption
          (Options.long "output" <> Options.short 'o' <> Options.metavar "FILE" <> Options.help "Output file")

    readParser =
      Read
        <$> Options.strArgument (Options.metavar "NAME" <> Options.help "File to read from archive")
        <*> Options.strOption
          ( Options.long "input"
              <> Options.short 'i'
              <> Options.metavar "FILE"
              <> Options.help "Archive to read"
          )

    listParser =
      List
        <$> Options.strOption
          ( Options.long "input"
              <> Options.short 'i'
              <> Options.metavar "FILE"
              <> Options.help "Archive to read"
          )

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
  case cli of
    Create inputs output -> create inputs output
    Read fileName input -> read fileName input
    List input -> list input

create ::
  -- | Input files
  [FilePath] ->
  -- | Output file
  FilePath ->
  IO ()
create inputs output = do
  archive <- Zip.new output
  for_ inputs $ \input -> do
    putStrLn $ "Adding " ++ input ++ "..."
    Zip.addFile input archive
  Zip.finish_ archive
  putStrLn $ "Created " ++ output

read ::
  -- | File name
  String ->
  -- | Input file
  FilePath ->
  IO ()
read fileName input = do
  archive <- Zip.open input
  mContent <- Zip.readContent (Utf8.fromString fileName) archive
  case mContent of
    Nothing -> exitFailure
    Just content -> ByteString.putStrLn content

list ::
  -- | Input file
  FilePath ->
  IO ()
list input = do
  names <-
    Zip.withArchive input $
      Zip.Archive.listFileNames . Zip.Archive.archiveCentralDirectory
  traverse_ ByteString.Char8.putStrLn names
