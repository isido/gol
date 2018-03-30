module Main where

import Gol


main :: IO ()
main = do
  putStrLn ("World before: " ++ show Gol.blinker)
  putStrLn ("World after: " ++ show (Gol.tick Gol.blinker))
