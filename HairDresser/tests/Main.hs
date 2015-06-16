module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Property

import Jeometry.Basics
import Graphics.PDF

main :: IO ()
main = defaultMain tests

prop_PointDistanceCommutativity :: PdfPoint -> PdfPoint -> Bool
prop_PointDistanceCommutativity p1 p2 = distBetween2Points p1 p2 == distBetween2Points p2 p1

prop_PointDistanceAssociativity :: Double -> Double -> Double -> Double -> Double -> Property
prop_PointDistanceAssociativity a b x1 x2 x3 = (x1 < x2) && (x2 < x3) ==> dist1 === dist2 where
            y1 = a * x1 + b
            y2 = a * x2 + b
            y3 = a * x3 + b
            dist1 = (distBetween2Points (x1 :+ y1) (x2 :+ y2)) + (distBetween2Points (x2 :+ y2) (x3 :+ y3))
            dist2 = (distBetween2Points (x1 :+ y1) (x3 :+ y3))

tests :: [TF.Test]
tests = [
        testGroup "Jeometry.Basics tests" [
                testProperty "Distance between points is commutative: "             prop_PointDistanceCommutativity,
                testProperty "Distance between points is associative: "             prop_PointDistanceAssociativity
                ]
       ]