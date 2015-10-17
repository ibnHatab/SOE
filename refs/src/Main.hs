module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Csv

type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
  let summed = fmap (V.foldr summer 0) v
  putStrLn $ "Total atBats was: " ++ show summed
  where summer (_name, _year, _team, atBats) n = n + atBats
