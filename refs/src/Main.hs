module Main where

import Refs

main :: IO ()
main = do
  summed <- Refs.getAtBatsSum "batting.csv"
  putStrLn $ "Total atBats was: " ++ show summed
