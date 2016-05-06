-- | 

module Renderer where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Hyrule
import Control.Monad (void)
import System.IO (writeFile)

nextTerrain :: String -> String
nextTerrain "g" = "s"
nextTerrain "s" = "f"
nextTerrain "f" = "m"
nextTerrain "m" = "w"
nextTerrain "w" = "g"
nextTerrain _ = error "Wrong class"

saveMap :: AreaType ->  String -> IO ()
saveMap Overworld   m = writeFile "../maps/overworld.map" m
saveMap (Dungeon n) m | m >= 1 && m <= 3 = writeFile ("../maps/dungeon" ++ show n ++ ".map") m
                      | otherwise        = error "Mapa desconhecido"


setup :: Area ->  Window -> UI ()
setup area window = void $ do

  let model = areaModel area
      areaType = areaType area

  -- Setup windows data
  return window # set UI.title "Area Editor"
  UI.addStyleSheet window "main.css"

  -- map modeled as a table
  table <- UI.table
  tdList <- mapM (\x ->
                   mapM (\tile ->
                          UI.td #. "g_"
                          # set UI.value "S") [1..42]) [1..42]

  trList <- mapM (\tds -> UI.tr #+ tds) $ map (map element) tdList
  element table #+ map element trList

  -- 
  saveButton <- UI.button # set UI.text "Save Map"
  getBody window #+ [element table, element saveButton]

  mapM_ (mapM_ (\td -> on UI.click td $ const $ do
                   className <- get UI.value td
                   let newClassName = nextTerrain $ className
                   element td # set UI.value newClassName #. newClassName)) tdList

  on UI.click saveButton $ const $ do
    lst <- mapM (mapM (get UI.value)) tdList
    liftIO $ saveMap (concat . concat $ lst)
    return ()
