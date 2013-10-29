-- ghost-control is the server-side tool to add new repositories and users.
{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import System.Directory
  ( copyFile, createDirectoryIfMissing, doesFileExist, getHomeDirectory
  )
import System.FilePath ((</>), (<.>))
import System.IO (hFlush, hPutStrLn, withFile, IOMode(WriteMode), stdout)

import Ghost (refreshAuthorizedKeys, runAndWaitProcess)

versionString :: String
versionString =
  "Ghost " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."
#ifdef USE_LINODE
  ++ "\n(Ghost is built with Linode API support.)"
#endif

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdInit, cmdAddUser, cmdAddRepository
    ]
  &= summary versionString
  &= program "ghost-control"

data Cmd =
    Init
  | AddUser
    { addUserPublicKeyPath :: String
    , addUserName :: String
    }
  | AddRepository
    { addRepoName :: String
    , addRepoUserName :: String
    }
  deriving (Data, Typeable)

cmdInit :: Cmd
cmdInit = Init
  &= help "Initialize Ghost (server-side)."

cmdAddUser :: Cmd
cmdAddUser = AddUser
  { addUserPublicKeyPath = ""
    &= typ "PUBLIC KEY PATH"
    &= help "path to user's public key"
    &= explicit
    &= name "k"
    &= name "key"
  , addUserName = ""
    &= typ "USERNAME"
    &= help "user name"
    &= explicit
    &= name "u"
    &= name "user"
  } &= help "Add a user to Ghost."
    &= explicit
    &= name "add-user"

cmdAddRepository :: Cmd
cmdAddRepository = AddRepository
  { addRepoName = ""
    &= typ "REPOSITORY NAME"
    &= help "the name of repository to create"
    &= explicit
    &= name "repo"
  , addRepoUserName = ""
    &= typ "USERNAME"
    &= help "user name for which to add a new repository"
    &= explicit
    &= name "u"
    &= name "user"
  } &= help "Add a repository to Ghost."
    &= explicit
    &= name "add-repository"

processCmd :: Cmd -> IO ()
processCmd Init{..} = do
  putStrLn "Ghost: server-side initialization."
  home <- getHomeDirectory
  let authorizedKeys = home </> ".ssh" </> "authorized_keys"
      administratorKeys = home </> "administrator" </> "keys"

  _ <- createDirectoryIfMissing True $ home </> "administrator"
  _ <- createDirectoryIfMissing True $ home </> "users"
  _ <- createDirectoryIfMissing True $ home </> "gits"
  _ <- createDirectoryIfMissing True $ home </> "run/staging/nginx"
  _ <- createDirectoryIfMissing True $ home </> "run/production/nginx"
  withFile (home </> "run" </> "nginx.conf") WriteMode $ \h -> do
    hPutStrLn h $ "include " ++ home ++ "/run/staging/nginx/*.conf;"
    hPutStrLn h $ "include " ++ home ++ "/run/production/nginx/*.conf;"

  b <- doesFileExist $ authorizedKeys
  if b
    then do
      putStrLn $ "Prior authorized_keys file found, " ++
        "copying it as administrator key."
      -- TODO make sure there is a single key in it. For now, it assumes this
      -- is the key copied by setup-ghost-archlinux.sh.
      copyFile authorizedKeys administratorKeys
    else putStrLn $ "No prior authorized_keys file found."
  refreshAuthorizedKeys "/home/ghost/bin/ghost-command"

  putStrLn "Ghost server-side initialization complete."
  putStrLn "Add the following line to your main Nginx configuration"
  putStrLn "file (inside an `http {..}` block.):"
  putStrLn $ "  include " ++ home ++ "/run/nginx.conf;"
  putStrLn "Add the following line to your sudoers file:"
  putStrLn "  ghost   ALL=(ALL) NOPASSWD: /etc/rc.d/nginx"
  putStrLn "(so the post-update hook can instruct Nginx to reload its configuration)."

processCmd AddUser{..} = do
  putStrLn "Ghost: adding a new user."
  home <- getHomeDirectory
  -- TODO validate username (forbid "administrator", just in case)
  -- TODO validate key
  -- TODO actually handle more than one key
  let userKeys = home </> "users" </> addUserName </> "keys"
  _ <- createDirectoryIfMissing True $ home </> "users" </> addUserName
  b <- doesFileExist addUserPublicKeyPath
  if b
    then do
      copyFile addUserPublicKeyPath userKeys
    else putStrLn $ "File " ++ addUserPublicKeyPath ++ " not found."
  refreshAuthorizedKeys "/home/ghost/bin/ghost-command"

processCmd AddRepository{..} = do
  putStrLn "Ghost: adding a new repository."
  hFlush stdout
  home <- getHomeDirectory
  let repoDir = home </> "users" </> addRepoUserName </> addRepoName <.> "git"
  -- TODO check username
  -- TODO check repository name
  _ <- runAndWaitProcess "git"
    ["init", "--bare", repoDir]
    Nothing
  copyFile ("bin" </> "ghost-post-update")
    (repoDir </> "hooks" </> "post-update")
  return ()
