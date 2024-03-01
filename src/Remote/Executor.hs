module Remote.Executor
  ( executeRemoteShellCmdIO,
    runRemoteShellCmdIO,
  )
where

import Executor (ExitCode)
import Local.Executor
import Ssh

executeRemoteShellCmdIO :: SshCommand -> IO (ExitCode, String)
executeRemoteShellCmdIO = executeLocalShellCmdIO . show

runRemoteShellCmdIO :: SshCommand -> IO ExitCode
runRemoteShellCmdIO = runLocalShellCmdIO . show
