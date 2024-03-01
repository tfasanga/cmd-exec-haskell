module ExecutorRemote
  ( executeRemoteShellCmdIO,
    runRemoteShellCmdIO,
  )
where

import Executor (ExitCode)
import ExecutorLocal
import Ssh

executeRemoteShellCmdIO :: SshCommand -> IO (ExitCode, String)
executeRemoteShellCmdIO = executeLocalShellCmdIO . show

runRemoteShellCmdIO :: SshCommand -> IO ExitCode
runRemoteShellCmdIO = runLocalShellCmdIO . show
