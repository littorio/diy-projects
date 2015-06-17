module Main where

import Test.Framework as TF (defaultMain, Test)
import Jeometry.Basics.Tests

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [Jeometry.Basics.Tests.fullRun]
