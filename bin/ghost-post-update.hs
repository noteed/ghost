-- ghost-post-update is a script that can be copied as a post-update hook in
-- all repositories managed by Ghost. When executed, it will copy the
-- repository content to some staging or production directory (based on the
-- name of the pushed branch). Based on a .ghost file, it will run a few other
-- actions, like running a virtual server with nginx.
{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)

import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import Data.Version (showVersion)

import System.Console.CmdArgs.Implicit
import System.Directory
  ( createDirectoryIfMissing, doesFileExist
  , getCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>), splitDirectories, splitPath)
import System.IO (hPutStrLn, withFile, IOMode(WriteMode), hFlush, stdout, stderr)
import System.Posix.Process (executeFile)
import System.Process (runProcess, waitForProcess)

versionString :: String
versionString =
  "ghost-post-update " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

-- | The Ghost user home.
homeDir :: FilePath
homeDir = "/home/thu/ghost" -- TODO "/home/ghost"

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ shell
    ]
  &= summary versionString
  &= program "ghost-post-update"

data Cmd = PostUpdate { postUpdateRef :: String }
  deriving (Data, Typeable)

shell :: Cmd
shell = PostUpdate
  { postUpdateRef = ""
    &= typ "REF"
    &= argPos 0
  } &= help "This command is used as a post-update hook. It updates some runtime directories."

processCmd :: Cmd -> IO ()
processCmd PostUpdate{..} = do
  currentDir <- getCurrentDirectory
  let domain = last $ splitDirectories currentDir
      productionDirectory = homeDir </> "run/production" </> domain
      stagingDirectory = homeDir </> "run/staging" </> domain
  putStrLn $ "Ghost receiving push (updating " ++ postUpdateRef ++ ")."
  putStrLn $ "Domain: " ++ domain
  hFlush stdout
  case postUpdateRef of
    "refs/heads/master" -> do
      _ <- createDirectoryIfMissing True productionDirectory
      _ <- runAndWaitProcess "git" (words "checkout master .")
        (Just [("GIT_WORK_TREE", productionDirectory)])
      maybeGhostFile <- readGhostFile productionDirectory
      case maybeGhostFile of
        Nothing -> do
          putStrLn $ "No .ghost file found."
          hFlush stdout
        Just GhostFile{..} -> do
          putStrLn $ "Reloading Nginx configuration (static root: " ++
            ghostFileStaticRoot ++ ")."
          hFlush stdout
          _ <- runAndWaitProcess "sudo" (words "/etc/rc.d/nginx reload") Nothing
          return ()
{-
      check .ghost file.
      generate run/production/nginx/domain.conf
-}
      putStrLn $ "Done."

    "refs/heads/staging" -> do
      _ <- runAndWaitProcess "git" (words "checkout staging .")
        (Just [("GIT_WORK_TREE", stagingDirectory)])
      putStrLn $ "Done."

    x -> do
      putStrLn $ "ERROR: received `" ++ x ++ "` instead of one of"
      putStrLn "  refs/heads/master"
      putStrLn "  refs/heads/staging"

-- TODO share between programs.
runAndWaitProcess cmd args env = do
  p <- runProcess cmd args Nothing env Nothing Nothing Nothing
  waitForProcess p

data GhostFile = GhostFile
  { ghostFileStaticRoot :: String
  }

readGhostFile :: FilePath -> IO (Maybe GhostFile)
readGhostFile dir = do
  let domain = last $ splitPath dir
      ghostFilePath = dir </> domain <.> "ghost"
  b <- doesFileExist ghostFilePath
  if b
    then do
      content <- readFile ghostFilePath
      return $ parseGhostFile content
    else return Nothing

parseGhostFile :: String -> Maybe GhostFile
parseGhostFile content = case parsedLines of
  [root] -> Just $ GhostFile root
  _ -> Nothing
  where
    parsedLines = mapMaybe parseLine $ lines content
    parseLine l = case words l of
      ["static-root:", root] -> Just root
      _ -> Nothing
