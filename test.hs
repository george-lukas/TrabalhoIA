module Main where

import Data.Array
import Data.List (nub)
import AStar

tMap :: Array (Int, Int) Char
tMap = listArray ((0, 0), (2,2)) ['x', 'x', 'x', 'x', 't', 'x', 'x', 'x', 'x']

uBound :: Ix i => Array (i,i) e -> (i,i)
uBound = snd . bounds

nextStepGen :: (Int,Int) -> [((Int,Int), Int)]
nextStepGen (a,b) = map (\x -> (x, 1)) $ nub [
  if a > 0 then (a - 1, b) else (a, b) , -- walk to the left
  if a < (fst . uBound $ tMap) - 1 then (a + 1, b) else (a, b) , -- walk to the right
  if b > 0 then (a, b - 1) else (a, b) , -- walk up
  if b < (snd . uBound $ tMap) - 1 then (a , b + 1) else (a, b)] -- walk down

main :: IO ()
main = print $ astarSearch (0,0) (\p -> tMap ! p == 't' ) nextStepGen (const 1)
