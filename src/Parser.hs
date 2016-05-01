module Parser where

import Hyrule
import Data.Array
import Text.Trifecta
import Control.Applicative ((<|>))
import Control.Monad (join)

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
    extract (Failure e) = error $  "Nao foi possovel realizar o parser do mapa. Erro: " ++ show e
