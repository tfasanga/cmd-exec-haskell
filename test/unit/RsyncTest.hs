module RsyncTest (rsyncUnitTests) where

import Machine
import Rsync
import Ssh
import Test.Tasty
import Test.Tasty.HUnit

rsyncUnitTests :: TestTree
rsyncUnitTests =
  testGroup
    "Rsync Unit tests"
    [ testCase "rsync default options" $
        buildRsyncCmd src1 dst1 []
          @?= Just "rsync --archive --relative --delete --verbose -o /tmp/root/./src/local user@dev:/remote/dir",
      testCase "rsync custom options" $
        buildRsyncCmd src1 dst1 opts1
          @?= Just "rsync --archive --relative --delete --verbose -o --exclude build --exclude .git --exclude .gradle /tmp/root/./src/local user@dev:/remote/dir",
      testCase "rsync custom port" $
        buildRsyncCmd src1 dst2 []
          @?= Just "rsync -P 2223 --archive --relative --delete --verbose -o /tmp/root/./src/local vagrant@labdev:/remote/dir"
    ]

src1 :: RsyncSource
src1 = RsyncSrc "/tmp/root" "src/local"

dst1 :: RsyncDestination
dst1 = RsyncDst (RemoteMachine sshCredentials1) "/remote/dir"

opts1 :: [RsyncOption]
opts1 = [Exclude "build", Exclude ".git", Exclude ".gradle"]

sshCredentials1 :: SshCredentials
sshCredentials1 = SshCredentials {username = "user", hostname = "dev", port = Nothing, privateKeyFile = Nothing}

dst2 :: RsyncDestination
dst2 = RsyncDst (RemoteMachine sshCredentials2) "/remote/dir"

sshCredentials2 :: SshCredentials
sshCredentials2 = SshCredentials {username = "vagrant", hostname = "labdev", port = Just 2223, privateKeyFile = Nothing}
