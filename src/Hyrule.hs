-- | Module Hyrule describes the whole Kingdom of Hyrule as DataTypes and Type aliases

module Hyrule where

import Data.Array

-- Map

data Tile = Tile { terrain :: Terrain, object :: Object} deriving (Show)

data Terrain = Grass | Sand | Forest | Mountain | Water | WDungeon | NWDungeon deriving (Eq, Ord, Show)

data Object = Pendant | MasterSword | Gate Place | DummyGate | Home | Empty deriving (Eq, Show)

data Place = Dungeon Int | Overworld deriving (Eq, Show)

data Path = Path Place [Tile]

-- Model

type Weight = Int

type Position = (Int, Int)

type Area = Array Position Tile

type AreaSize = Int
