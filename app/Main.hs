module Main where

import Data.Shapefile


main :: IO ()
main = do
  fh <- openData "/home/m/github/shapefile2json/bla/England.shp"
--   print =<< getMetadata fh
  print =<< readData fh
  closeData fh
  {-
   Option to query fields
   extract by bbox
   apply coordinate transformation
   select fields to include
  -}