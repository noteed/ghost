module Ghost where

import System.Exit (ExitCode)
import System.Process (runProcess, waitForProcess)

runAndWaitProcess :: FilePath -> [String] -> Maybe [(String, String)]
  -> IO ExitCode
runAndWaitProcess cmd arguments env = do
  p <- runProcess cmd arguments Nothing env Nothing Nothing Nothing
  waitForProcess p
