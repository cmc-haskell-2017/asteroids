module Main where

import Asteroids

main :: IO ()
main = do
   images <- loadImages
   run images
