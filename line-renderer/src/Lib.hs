module Lib
  ( someFunc,
  )
where

import Data.List (intercalate)
import qualified Data.List as L
import Diagrams.Prelude
import System.Random
import Prelude

someFunc :: IO ()
someFunc = do
  putStrLn "Running"
  let vs = map genVectors [1 .. 10000]
  writeFile "../data/vectors/vector.csv" (intercalate "\n" (map showVectors vs))
  print "Done"

showVectors :: (Show a1, Show a2, Show a3, Show a4) => [(a1, a2, a3, a4)] -> String
showVectors xs = intercalate "," (map showVector xs)

showVector :: (Show a1, Show a2, Show a3, Show a4) => (a1, a2, a3, a4) -> String
showVector (x, y, w, h) = show x <> "," <> show y <> "," <> show w <> "," <> show h

genVectors :: Int -> [(Double, Double, Double, Double)]
genVectors i =
  let positions = L.unfoldr (Just . uniformR (-20, 20))
      sizes = L.unfoldr (Just . uniformR (1, 6))
      nbox = L.unfoldr (Just . uniformR (10, 35))
      posGen = mkStdGen i
      sizeGen = mkStdGen $ i + 10
      boxGen = mkStdGen $ i + 1000
   in genVectorSpace (head (nbox boxGen :: [Int])) (positions posGen :: [Double]) (sizes sizeGen :: [Double])

genVectorSpace :: Int -> [Double] -> [Double] -> [(Double, Double, Double, Double)]
genVectorSpace 0 _ _ = mempty
genVectorSpace i (x : y : posrest) (h : w : sizerest) = (x, y, w, h) : genVectorSpace (i - 1) posrest sizerest
genVectorSpace i x p = genVectorSpace (i - 1) x p