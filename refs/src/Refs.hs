module Refs where

import qualified Data.ByteString.Lazy as BL
import Data.Foldable as F

import Data.Csv.Streaming

type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

forth :: (t, t1, t2, t3) -> t3
forth (_, _, _, x) = x

summer = (+) . forth

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader


getAtBatsSum filePath = do
  csvData <- BL.readFile filePath
  return $ F.foldr summer 0 (baseballStats csvData)
