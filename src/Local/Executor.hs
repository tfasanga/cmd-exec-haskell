module Local.Executor
  ( executeLocalShellCmdIO,
    executeLocalShellCmdErrIO,
    runLocalShellCmdIO,
  )
where

import Control.Exception (bracketOnError)
import Executor
import qualified System.Exit as SE
import System.IO (Handle, hGetContents)
import System.Process

-- Execute an external command and return its output combined with error
executeLocalShellCmdIO :: String -> IO (ExitCode, String)
executeLocalShellCmdIO cmd = do
  putStrLn cmd
  (readEnd, writeEnd) <- createPipe
  withCreateProcess_
    "executeShellCmd"
    (shell cmd)
      { delegate_ctlc = True,
        std_out = UseHandle writeEnd,
        std_err = UseHandle writeEnd
      }
    $ \(_, _, _, ph) -> do
      exitCode <- waitForProcess ph
      content <- hGetContents readEnd
      return (translateExitCode exitCode, content)

-- Execute an external command and return its output
executeLocalShellCmdErrIO :: String -> IO (ExitCode, String, String)
executeLocalShellCmdErrIO cmd = do
  putStrLn cmd
  (readEnd, writeEnd) <- createPipe
  (readErrEnd, writeErrEnd) <- createPipe
  withCreateProcess_
    "executeShellCmd"
    (shell cmd)
      { delegate_ctlc = True,
        std_out = UseHandle writeEnd,
        std_err = UseHandle writeErrEnd
      }
    $ \(_, _, _, ph) -> do
      exitCode <- waitForProcess ph
      content <- hGetContents readEnd
      contentErr <- hGetContents readErrEnd
      return (translateExitCode exitCode, content, contentErr)

-- Run an external command
runLocalShellCmdIO :: String -> IO ExitCode
runLocalShellCmdIO cmd = do
  putStrLn cmd
  withCreateProcess_
    "runShellCmd"
    (shell cmd) {delegate_ctlc = True}
    $ \(_, _, _, ph) -> do
      exitCode <- waitForProcess ph
      return $ translateExitCode exitCode

withCreateProcess_ ::
  String ->
  CreateProcess ->
  ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) ->
  IO a
withCreateProcess_ msg c = bracketOnError before after
  where
    before = createProcess_ msg c
    after = cleanupProcess

-- Private translator
translateExitCode :: SE.ExitCode -> ExitCode
translateExitCode SE.ExitSuccess = ExitSuccess
translateExitCode (SE.ExitFailure code) = ExitFailure code
