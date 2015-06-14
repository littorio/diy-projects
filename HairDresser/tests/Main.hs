module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Property


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs


prop_jinny2 :: [Int] -> Property
prop_jinny2 xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_jinny :: [Int] -> Bool
prop_jinny xs         = head (qsort xs) == minimum xs

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Data.Decimal" [
--                testProperty "bookTest1: "           prop_jinny,
                testProperty "bookTest2: "           prop_jinny2
                ]
       ]