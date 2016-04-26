-- |

{-# LANGUAGE QuasiQuotes #-}

module Main where

import AStar
import Data.Array
import Data.List (nub)
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Text.Trifecta
import Control.Applicative ((<|>))
import Control.Monad (join)
import Text.RawString.QQ

-- Map

data Tile = Tile { terrain :: Terrain, object :: Object} deriving (Show)

data Terrain = Grass | Sand | Forest | Mountain | Water | WDungeon | NWDungeon deriving (Eq, Ord, Show)

data Object = Pendant | MasterSword | Gate Place | Home | Empty deriving (Eq, Show)

data Place = Dungeon Int | Overworld deriving (Eq, Show)

-- Model

type Weight = Int

type Position = (Int, Int)

type Area = Array Position Tile

type Side = Int


weight :: Terrain -> Weight
weight terr = Map.findWithDefault 10 terr $ Map.fromList [(Grass, 10),
                                                          (Sand, 20),
                                                          (Forest, 100),
                                                          (Mountain, 150),
                                                          (Water, 180),
                                                          (WDungeon, 10)]

nextStepGen :: Area ->  Position -> [(Position, Weight)]
nextStepGen area pos@(a,b) = let w = weight . terrain $ area ! pos
                             in map (\x -> (x, w)) $ nub [
  if a > 0 then (a - 1, b) else (a, b) , -- walk to the left
  if a < (fst  upperBound) then (a + 1, b) else (a, b) , -- walk to the right
  if b > 0 then (a, b - 1) else (a, b) , -- walk down
  if b < (snd  upperBound) then (a , b + 1) else (a, b)] -- walk up

goalReached :: Area -> Position -> Bool
goalReached area pos = object (area ! pos) == MasterSword

heuristic :: Area -> Position -> Weight
heuristic area pos =  weight . terrain $ (area ! pos)


-- Parser

data TerrainError = TerrainError
data ObjectError = ObjectError


parseTerrain :: Parser Terrain
parseTerrain = do
  terrain' <- letter
  case terrain' of
    'g' -> return Grass
    's' -> return Sand
    'f' -> return Forest
    'm' -> return Mountain
    'w' -> return Water
    'W' -> return WDungeon
    'N' -> return NWDungeon
    _   -> fail "Terreno desconhecido"

parseObject :: Parser Object
parseObject = do
  object' <- letter <|> digit <|> char '_'
  case object' of
    'S' -> return MasterSword
    'P' -> return Pendant
    '_' -> return Empty
    'H' -> return Home
    '1' -> return . Gate $ Dungeon 1
    '2' -> return . Gate $ Dungeon 2
    '3' -> return . Gate $ Dungeon 3
    'O' -> return . Gate $ Overworld
    _   -> fail "Objeto desconhecido"

skipSpace :: Parser ()
skipSpace = skipMany space

parseTiles :: Side -> Parser (Array Position Tile)
parseTiles side = do

  rows <- some parseLine
  if length rows == side
    then return $ listArray ((0,0), (side - 1, side - 1)) $ join rows
    else fail $ "O mapa deve possuir " ++ show side ++ " linhas"

  where
    parseLine = do
      tiles  <- parseTile
      remain <- many $ char ' ' *> parseTile
      newline
      let row = tiles : remain
      if length row == side
        then return row <?> "Row of Tiles"
        else fail $ "O mapa deve possuir " ++ show side ++ " colunas"

    parseTile = do
      terrain' <- parseTerrain
      object'  <- parseObject
      (return $ Tile terrain' object') <?> "Tile"

-- Utils

findObject :: Object -> Area -> Maybe Position
findObject obj area = fst <$> find (\(_, t) -> object t == obj) (assocs area)

startPoint, lowerBound, upperBound :: Position

startPoint = foundHome . findObject Home $ hMap
  where
    foundHome (Just idx) = idx
    foundHome _ = error "Casa do link não foi encontrada"
lowerBound = (0,0)
upperBound = (5,5)


sMap :: String
sMap = [r|g_ g_ g_ g_ g_ w_
g_ g_ g_ g_ g_ g_
g_ w_ w_ g_ w_ g_
g_ w_ w_ gS g_ w_
g_ w_ w_ w_ w_ w_
g_ g_ g_ g_ g_ g_
|]

hMap :: Area
hMap = extract $ parseString (parseTiles 6) mempty sMap
  where
    extract (Success p) = p
    extract (Failure e) = error $  "Não foi possível realizar o parser do mapa. Erro: " ++ show e

main :: IO ()
main = let quickestPath = astarSearch startPoint (goalReached hMap) (nextStepGen hMap) $ heuristic hMap
       in print $ quickestPath
