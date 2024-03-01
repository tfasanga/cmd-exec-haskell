module Main(main) where

import Test.Tasty
import SshTest
import RsyncTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [sshUnitTests, rsyncUnitTests]
