module Main where

import Graphics.Gloss
import Logic
import Gui

main :: IO ()
main = do
  putStrLn $ show initWorld
  simulate FullScreen background 1000 initWorld draw tick

background = makeColorI 28 27 32 255
