-- | 

module Renderer where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Hyrule
import Control.Monad (void)

nextTerrain :: String -> String
nextTerrain "grass" = "sand"
nextTerrain "sand" = "forest"
nextTerrain "forest" = "mountain"
nextTerrain "mountain" = "water"
nextTerrain "water" = "grass"
nextTerrain _ = error "Wrong class"


setup :: Window -> UI ()
setup window = void $ do
  return window # set UI.title "Area Editor"
  UI.addStyleSheet window "main.css"
  table <- UI.table
  tdList <- mapM (\x -> mapM (\y -> UI.td # set UI.id_ (show x ++ "," ++ show y) #. "grass" # set UI.value "grass") [1..42]) [1..42]
  trList <- mapM (\tds -> UI.tr #+ tds) $ map (map element) tdList
  element table #+ map element trList
  getBody window #+ [element table]
  mapM_ (mapM_ (\td -> on UI.click td $ const $ do
                   className <- get UI.value td
                   let newClassName = nextTerrain $ className
                   element td # set UI.value newClassName #. newClassName)) tdList
