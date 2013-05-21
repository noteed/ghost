-- ghost-command can be specified in the git user authorized_keys for each
-- public key. This implements some access control when a single user account
-- (generaly git) is shared among several people (ghost-command is run with a
-- username (for which access must be checked) passed in argument).
{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)

import Control.Applicative ((<$>))
import Data.Version (showVersion)

import System.Console.CmdArgs.Implicit
import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hPutStrLn, withFile, IOMode(WriteMode))
import System.Posix.Process (executeFile)

versionString :: String
versionString =
  "ghost-shell " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ shell
    ]
  &= summary versionString
  &= program "ghost-command"

data Cmd = Shell { shellUser :: String }
  deriving (Data, Typeable)

shell :: Cmd
shell = Shell
  { shellUser = ""
    &= typ "USER"
    &= help ("username associated to the public key for which the connection"
      ++ " was accepted")
    &= explicit
    &= name "u"
    &= name "user"
  } &= help "Run commands normally accepted by git-shell with access control."

processCmd :: Cmd -> IO ()
processCmd Shell{..} =
  if null shellUser
  then mapM_ putStrLn
    [ versionString
    , ""
    , "ghost-command must be run with the --user option from the forced"
    , "command associated to a public key in the ~/.ssh/authrized_keys file."
    ]
  else do
    home <- getHomeDirectory
    originalCommand <- lookup "SSH_ORIGINAL_COMMAND" <$> getEnvironment
    -- TODO this is for debugging only.
    withFile (home </> "ghost-command.txt") WriteMode $ \h -> do
      hPutStrLn h $
        "originalCommand:\n" ++ show originalCommand
    case originalCommand of
      Nothing -> putStrLn "SSH_ORIGINAL_COMMAND is not present."
      Just "" -> putStrLn "SSH_ORIGINAL_COMMAND is empty."
      Just commandArgs ->
        case words commandArgs of
          -- TODO check command and args
          command : argument : [] -> do
            let repoPath = home </> "user" </> (init $ tail argument) -- TODO init . tail can explode.
            _ <- executeFile command True [repoPath] Nothing
            return ()
          _ : _ -> return () -- TODO
          _ -> return () -- Can't happen
