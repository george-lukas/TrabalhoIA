-- | 

module Renderer where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Hyrule
import Control.Monad (void)
import Data.Foldable(toList)
import Data.List.Split (chunksOf, splitOn)

nextTerrain :: AreaType -> String -> String
nextTerrain Overworld "g" = "s"
nextTerrain Overworld "s" = "f"
nextTerrain Overworld "f" = "m"
nextTerrain Overworld "m" = "w"
nextTerrain Overworld "w" = "g"
nextTerrain (Dungeon _) "d" = "n"
nextTerrain (Dungeon _) "n" = "d"
nextTerrain _ _ = error "Wrong class"

tileToClass :: Tile -> String
tileToClass tile =
  case terrain tile of
    Grass -> "g"
    Sand -> "s"
    Forest -> "f"
    Mountain -> "m"
    Water -> "w"
    WDungeon -> "d"
    NWDungeon -> "n"
    --_ -> error "Terreno desconhecido"
    ++ " " ++
  case object tile of
    Pendant -> "P"
    MasterSword -> "S"
    Gate Overworld -> "O"
    Gate (Dungeon n) -> show n
    DummyGate -> "D"
    Home -> "H"
    Empty -> "_"
    --_ -> error "Objeto desconhecido"

classToTileData :: String -> (String, String)
classToTileData class' = let (t:o:_) = splitOn " " class' in (t,o)

saveMap :: AreaType ->  String -> IO ()
saveMap Overworld   m = writeFile "../maps/overworld.map" m
saveMap (Dungeon n) m | n >= 1 && n <= 3 = writeFile ("../maps/dungeon" ++ show n ++ ".map") m
                      | otherwise        = error "Mapa desconhecido"


setup :: Area ->  Window -> UI ()
setup area window = void $ do

  let aSize = if areaType area == Overworld then 42 else 28

  -- Setup windows data
  return window # set UI.title "Area Editor"
  UI.addStyleSheet window "main.css"

  -- map modeled as a table
  table <- UI.table
  tdList <- mapM ( mapM (\tile -> let cn = tileToClass tile in  UI.td #. cn # set UI.value cn ) ) (chunksOf aSize . toList . areaModel $ area)

  trList <- mapM (\tds -> UI.tr #+ tds) $ map (map element) tdList
  element table #+ map element trList

  -- 
  saveButton <- UI.button # set UI.text "Save Map"
  getBody window #+ [element table, element saveButton]

  mapM_ (mapM_ (\td -> on UI.click td $ const $ do
                   (terrain', object') <- classToTileData <$> get UI.value td
                   let cn = (nextTerrain (areaType area) terrain') ++ " " ++ object'
                   element td # set UI.value cn #. cn)) tdList

  on UI.click saveButton $ const $ do
    lst <- mapM (mapM (\c -> do
                          (t', o') <- classToTileData <$> get UI.value c
                          return $ t' ++ o')) tdList
    liftIO $ saveMap (areaType area) (concat . concat . map (++ ["\n"]) $ lst)
    return ()

boot :: Area -> IO ()
boot = startGUI defaultConfig {jsPort = Just 8088, jsStatic = Just "../wwwroot"} . setup
