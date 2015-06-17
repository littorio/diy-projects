module Main where

import Test.Framework as TF (defaultMain, Test)
import Jeometry.Distances.Tests

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [Jeometry.Distances.Tests.fullRun]
