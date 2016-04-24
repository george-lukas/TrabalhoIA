{- module Hyrule describes the dataTypes of whole Kingdom and the dungeons of Hyrule -}

module Hyrule where

import Data.Array
import AStar
import Data.Maybe
import Data.List

-- Defining some dataTypes

-- Valids Terrains in Hyrule
data Terrain = Grass | Sand | Forest | Mountain | Water | WDungeon | NWDungeon

-- Valid Objtects in Hyrule Kingdom
data Object = Pendant | MasterSword | Gate Place | Empty

-- Valid nodes where Link can walk
data Tile = Tile Terrain Object

-- Dungeons receive coordinates X,Y
data Place = Dungeon1 (Int,Int) | Dungeon2 (Int, Int) | Dungeon3 (Int, Int)

-- Data position descrites the folowing position of Link
type Position = (Int, Int)

-- Here is the weight to the heuristics. A function that returns a weight
weight :: Terrain -> Int
weight Grass = 10
weight Sand = 20
weight Forest = 100
weight Mountain = 150
weight Water = 180

--- Controling Link movements

-- Utility for nextStepGen
tMap :: Array (Int, Int) Tile
tMap = listArray ((0,0), (41,41)) [Tile Grass Empty, Tile Grass Empty, Tile Water Empty]
                                   
-- Utility for nextStepGen
uBound :: Ix i => Array (i,i) e -> (i,i)
uBound = snd . bounds

nextStepGen :: (Int,Int) -> [((Int,Int), Int)]
nextStepGen (a,b) = map (\x -> (x, 1)) $ nub [
  if a > 0 then (a - 1, b) else (a, b) , -- walk to the left
  if a < (fst . uBound $ tMap) - 1 then (a + 1, b) else (a, b) , -- walk to the right
  if b > 0 then (a, b - 1) else (a, b) , -- walk up
  if b < (snd . uBound $ tMap) - 1 then (a , b + 1) else (a, b)] -- walk down
