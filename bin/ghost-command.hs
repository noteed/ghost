{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)

import Control.Applicative ((<$>))
import Data.Version (showVersion)

import System.Console.CmdArgs.Implicit
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hPutStrLn, withFile, IOMode(WriteMode))

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
    originalCommand <- lookup "SSH_ORIGINAL_COMMAND" <$> getEnvironment
    withFile ("/home/ghost" </> "ghost-command.txt") WriteMode $ \h -> do
      hPutStrLn h $
        "originalCommand:\n" ++ show originalCommand
    putStrLn "Yeah"
    print originalCommand
