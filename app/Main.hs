module Main where

import Game
import Images

main :: IO ()
main = do
   images <- loadImages
   run images
