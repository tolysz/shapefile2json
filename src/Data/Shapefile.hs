{-# LANGUAGE BangPatterns, LambdaCase, TupleSections #-}
module Data.Shapefile
    ( readData
    , openData
    , closeData
    , getMetadata
    ) where

import Database.Shapefile
import Database.Shapefile.Shp.OneToOne
import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import GHC.Word

processRecord
 :: [(DbfFieldHandle, BSL.ByteString -> f)]
   -> ShpHandle
   -> Int
   -> IO (ShpRec,[f])

processRecord dbs fh i = getShpRecord fh i >>= \case
   (a, Nothing)    -> return (a,[])
   (a, Just dbrh ) -> do
      ff <- forM dbs (\(d,f) -> ( f <$> readDbfField dbrh d) )
      return (a,ff)

openData = flip openShp True

getMetadata fh =
  map (fieldName &&& dbfFieldType . fieldDesc) <$> shpDbfFields fh

readData fh = do
--      fh  <- openShp f True
     re  <- shpHeader    fh
     dbs <- shpDbfFields fh
     rc  <- getShpRecCnt fh
--      let db = map (fieldName &&& dbfFieldType . fieldDesc) dbs
     sh <- forM [0 .. fromIntegral rc -1] (processRecord (zip dbs (repeat id)) fh)
     return (re, rc, sh)

closeData = closeShp
