module Lib
    ( someFunc
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import System.Random
import Prelude
import qualified Data.List as L
import Control.Monad

someFunc :: IO ()
someFunc = 
    forM_ [1 .. 100] (\ i -> renderSVG ("images/img" <> show i <> ".svg") (mkWidth 800) (genImages i))

genImages :: Int -> Diagram B
genImages i = 
    let
        positions = L.unfoldr (Just . uniformR (30, 60))
        sizes = L.unfoldr (Just . uniformR (1, 6))
        nbox = L.unfoldr (Just . uniformR (10, 35))
        posGen = mkStdGen i
        sizeGen = mkStdGen i
        boxGen = mkStdGen i
    in
        genVectorSpace (head (nbox boxGen :: [Int])) (positions posGen :: [Double]) (sizes sizeGen :: [Double])


genVectorSpace :: Int -> [Double] -> [Double] -> Diagram B
genVectorSpace 0 _ _ = mempty
genVectorSpace i (x:y:posrest) (h:w:sizerest) = (translateY y $ translateX x $ pad 2 $ center $ fc black $ rect w h) <> genVectorSpace (i - 1) posrest sizerest
genVectorSpace i x p = genVectorSpace (i - 1) x p