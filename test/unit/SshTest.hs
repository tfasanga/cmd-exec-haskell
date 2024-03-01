module SshTest (sshUnitTests) where

import Ssh
import Test.Tasty
import Test.Tasty.HUnit

sshUnitTests :: TestTree
sshUnitTests =
  testGroup
    "SSH Unit tests"
    [ testCase "SSH default port" $
        show (Ssh sshCredentials1 "ls -l")
          @?= "ssh user@dev 'ls -l'",
      testCase "SSH non-default port" $
        show (Ssh sshCredentials2 "ls -l")
          @?= "ssh vagrant@labdev -p 2223 'ls -l'",
      testCase "SSH escape" $
        show (Ssh sshCredentials1 "echo 'abc' && ls -l")
          @?= "ssh user@dev 'echo \\'abc\\' && ls -l'"
    ]

sshCredentials1 :: SshCredentials
sshCredentials1 = SshCredentials {username = "user", hostname = "dev", port = Nothing, privateKeyFile = Nothing}

sshCredentials2 :: SshCredentials
sshCredentials2 = SshCredentials {username = "vagrant", hostname = "labdev", port = Just 2223, privateKeyFile = Nothing}
