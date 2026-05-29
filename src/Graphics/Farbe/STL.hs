{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Farbe.STL where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Graphics.Farbe.Vec
import Graphics.Farbe.VertexArray
import Control.Monad
import GHC.Generics
import Control.Monad.IO.Class
import Data.Maybe



data STL = STL { triangles :: [Triangle] } deriving Show

getSTL :: Get STL
getSTL = do
	replicateM 80 $ getWord8
	i <- getWord32le
	fmap STL $ replicateM (itoi i) $ do
		tri <- getTriangle
		_ :: Word16 <- get
		return $ tri

getTriangle :: Get Triangle
getTriangle = do
	[n,v1,v2,v3] <- replicateM 4 getSTLV3
	return $ Triangle n v1 v2 v3

getSTLV3 :: Get (V3 Float)
getSTLV3 = do
	(x:y:z:[]) <- replicateM 3 getFloatle
	return $ V3 x y z

data Triangle = Triangle
	{ tn  :: V3 Float
	, tv1 :: V3 Float
	, tv2 :: V3 Float
	, tv3 :: V3 Float
	}
	deriving (Eq, Ord, Read, Show, Generic)


readFileBinSTL :: MonadIO m => FilePath -> m [(V3 Float, V3 Float)]
readFileBinSTL p = do
	STL tri <- fmap (runGet getSTL) $ liftIO $ B.readFile p
	return $ concatMap (\(Triangle n a b c) -> [(n,a),(n,b),(n,c)]) tri


readFileSTL :: MonadIO m => FilePath -> m [V3 Float]
readFileSTL p = liftIO $ readSTL <$> readFile p

readSTL :: String -> [V3 Float]
readSTL = catMaybes . map f . map words . lines
	where
	f (('v':_):x:y:z:_) = Just $ V3 (read x) (read y) (read z)
	f _ = Nothing


loadSTL :: (HandVBO m, MonadIO m) => String -> m (VArray (V3 Float))
loadSTL s = readFileSTL s >>= newVArray



type Face = [[Float]]

writeFileSTL :: FilePath -> [Face] -> IO ()
writeFileSTL s ts = writeFile s $ showSTL ts

showSTL :: [Face] -> String
showSTL ts
	=  "solid solid\n"
	++ concatMap showSTLTriangle ts
	++ "endsolid solid\n"
	where
	showSTLTriangle t
		= "facet normal 0 0 0\n"
		++ "outer loop\n"
		++ unlines (map (("vertex "++) . unwords . map show) t)
		++ "endloop\n"
		++ "endfacet\n"

