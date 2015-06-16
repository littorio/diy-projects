module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import Jeometry.Basics

main :: IO ()
main = defaultMain tests

instance Arbitrary Point where
   arbitrary = do
     Positive x <- arbitrary
     Positive y <- arbitrary
     return (x :+ y)

prop_PointDistanceCommutativity :: Point -> Point -> Bool
prop_PointDistanceCommutativity p1 p2 = distToPoint p1 p2 == distToPoint p2 p1

prop_PointDistanceAssociativity :: Double -> Double -> Double -> Double -> Double -> Property
prop_PointDistanceAssociativity a b x1 x2 x3 = (x1 < x2) && (x2 < x3) ==> dist1 === dist2 where
            y1 = a * x1 + b
            y2 = a * x2 + b
            y3 = a * x3 + b
            dist1 = (distToPoint (x1 :+ y1) (x2 :+ y2)) + (distToPoint (x2 :+ y2) (x3 :+ y3))
            dist2 = (distToPoint (x1 :+ y1) (x3 :+ y3))

tests :: [TF.Test]
tests = [
        testGroup "Jeometry.Basics tests" [
                testProperty "Distance between points is commutative: "             prop_PointDistanceCommutativity,
                testProperty "Distance between points is associative: "             prop_PointDistanceAssociativity
                ]
       ]