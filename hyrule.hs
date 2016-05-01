-- |

module Main where

import AStar
import Data.Array
import Data.List (nub)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Text.Trifecta
import Control.Applicative ((<|>))
import Control.Monad (join)

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
    'd' -> return WDungeon
    'n' -> return NWDungeon
    _   -> fail "Terreno desconhecido"

parseObject :: Parser Object
parseObject = do
  object' <- letter <|> digit <|> char '_'
  case object' of
    'S' -> return MasterSword
    'P' -> return Pendant
    '_' -> return Empty
    'H' -> return Home
    'D' -> return DummyGate
    '1' -> return . Gate $ Dungeon 1
    '2' -> return . Gate $ Dungeon 2
    '3' -> return . Gate $ Dungeon 3
    'O' -> return . Gate $ Overworld
    _   -> fail "Objeto desconhecido"

parseTiles :: AreaSize -> Parser (Array Position Tile)
parseTiles areaSize = do

  rows <- some parseLine
  if length rows == areaSize
    then return $ listArray ((0,0), (areaSize - 1, areaSize - 1)) $ join rows
    else fail $ "O mapa deve possuir " ++ show areaSize ++ " linhas"

  where
    parseLine = do
      tiles  <- parseTile
      remain <- many $ char ' ' *> parseTile
      newline
      let row = tiles : remain
      if length row == areaSize
        then return row <?> "Row of Tiles"
        else fail $ "O mapa deve possuir " ++ show areaSize ++ " colunas"

    parseTile = do
      terrain' <- parseTerrain
      object'  <- parseObject
      (return $ Tile terrain' object') <?> "Tile"

parseMap :: AreaSize ->  String -> Area
parseMap areaSize str = extract $ parseString (parseTiles areaSize) mempty str
  where
    extract (Success p) = p
    extract (Failure e) = error $  "Não foi possível realizar o parser do mapa. Erro: " ++ show e

-- Utils

findObject :: Object -> Area -> Maybe Position
findObject obj area = fst <$> find (\(_, t) -> object t == obj) (assocs area)

findOverworldStartPosition :: Area -> Position
findOverworldStartPosition overworld  = fromJust . findObject Home $ overworld

findDungeonStartPosition :: Area -> Position
findDungeonStartPosition dungeon = fromJust . findObject (Gate $ Overworld) $ dungeon

overworldSize = 42
dungeonSize = 28

weight :: Terrain -> Weight
weight terr = Map.findWithDefault 10 terr $ Map.fromList [(Grass, 10),
                                                          (Sand, 20),
                                                          (Forest, 100),
                                                          (Mountain, 150),
                                                          (Water, 180),
                                                          (WDungeon, 10)]


walkable :: Area -> Position -> Bool
walkable area pos = terrain (area ! pos) /= NWDungeon

nextStepGen :: Area ->  Position -> [(Position, Weight)]
nextStepGen area pos@(a,b) = let w = weight . terrain $ area ! pos
                             in map (\x -> (x, w)) $ nub [
  if a > 0 && walkable area (a - 1, b) then (a - 1, b) else (a, b) , -- walk to the left
  if a < (fst . snd . bounds $ area) && walkable area (a + 1, b) then (a + 1, b) else (a, b) , -- walk to the right
  if b > 0  && walkable area (a, b - 1) then (a, b - 1) else (a, b) , -- walk down
  if b < (snd . snd . bounds $ area) && walkable area (a, b + 1) then (a , b + 1) else (a, b)] -- walk up

reachedGoal :: Object -> Area  -> Position -> Bool
reachedGoal obj area  pos = object (area ! pos) == obj

reachedSword = reachedGoal MasterSword
reachedPendant = reachedGoal Pendant
reachedGate = reachedGoal . Gate . Dungeon


heuristic :: Area -> Position -> Weight
heuristic area pos =  weight . terrain $ (area ! pos)

-- map position from (y,x), zero-index to (x,y), one-idex
rearrangePosition :: Position -> Position
rearrangePosition (a,b) = (b + 1,a + 1)


main :: IO ()
main = do
  contentOverworld <- readFile "../maps/overworld.map"
  contentDungeon1  <- readFile "../maps/dungeon1.map"
  contentDungeon2  <- readFile "../maps/dungeon1.map"
  contentDungeon3  <- readFile "../maps/dungeon1.map"

  let overworldMap = parseMap overworldSize contentOverworld
      dungeon1Map  = parseMap dungeonSize contentDungeon1
      dungeon2Map  = parseMap dungeonSize contentDungeon2
      dungeon3Map  = parseMap dungeonSize contentDungeon3
      overworldSP = findOverworldStartPosition overworldMap
      firstDungeonSP = findDungeonStartPosition dungeon1Map
      secondDungeonSP = findDungeonStartPosition dungeon2Map
      thirdDungeonSP = findDungeonStartPosition dungeon3Map

      --firstDungeonPosition = fromJust . findObject $ (Gate (Dungeon 1)) overworldMap
      --secondDungeonPosition = fromJust . findObject $ (Gate (Dungeon 2)) overworldMap
      --thirdDungeonPosition = fromJust . findObject $ (Gate (Dungeon 3)) overworldMap

      totalCostAndPath = do
        (costFD, pathToFirstDungeon)  <- astarSearch overworldSP (reachedGate 1 overworldMap) (nextStepGen overworldMap) $ heuristic overworldMap
        (costFP, pathToFirstPendant)  <- astarSearch firstDungeonSP (reachedPendant dungeon1Map) (nextStepGen dungeon1Map) $ heuristic dungeon1Map
        (costFP', pathBackFromFirstPendant) <- return (costFP, reverse pathToFirstPendant)
        (costSD, pathToSecondDungeon) <- astarSearch (last pathToFirstDungeon) (reachedGate 2 overworldMap) (nextStepGen overworldMap) $ heuristic overworldMap
        (costSP, pathToSecondPendant) <- astarSearch secondDungeonSP (reachedPendant dungeon2Map) (nextStepGen dungeon2Map) $ heuristic dungeon2Map
        (costTD, pathToThirdDungeon)  <- astarSearch (last pathToSecondDungeon) (reachedGate 3 overworldMap) (nextStepGen overworldMap) $ heuristic overworldMap
        (costTP, pathToThirdPendant)  <- astarSearch thirdDungeonSP (reachedPendant dungeon3Map) (nextStepGen dungeon3Map) $ heuristic dungeon3Map
        (costMS, pathToMasterSword)   <- astarSearch (last pathToThirdDungeon) (reachedSword overworldMap) (nextStepGen overworldMap) $ heuristic overworldMap
        let totalCost'  = costFD + 2 * costFP + costSD + 2 * costSP + costTD + 2 * costTP + costMS
            totalPath' = map (map rearrangePosition) $ [pathToFirstDungeon] ++ [pathToFirstPendant] ++ [(reverse pathToFirstPendant)] ++ [pathToSecondDungeon] ++
              [pathToSecondPendant] ++ [(reverse pathToSecondPendant)] ++ [pathToThirdDungeon] ++  [pathToThirdPendant] ++
              [(reverse pathToThirdPendant)] ++ [pathToMasterSword]
        return (totalCost', totalPath')

  print $ totalCostAndPath
