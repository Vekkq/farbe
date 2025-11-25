{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Farbe.STL where

import Data.Binary
import Data.Binary.Get
import Data.Int
import Graphics.Farbe
import Graphics.Farbe.Vec
import Graphics.Farbe.Utils
import Graphics.Farbe.Tuple
import Control.Monad
import GHC.Generics
import Control.Monad.IO.Class
import Data.Maybe



data STL = STL { triangles :: [Triangle] } deriving Show

instance Binary STL where
	get = do
		replicateM 80 $ getWord8
		i <- getWord32le
		fmap STL $ replicateM (itoi i) $ do
			tri <- get
			_ :: Word16 <- get
			return $ tri
	put = undefined


instance Binary (V3 Float) where
	get = do
		(x:y:z:[]) <- replicateM 3 getFloatle
		return $ V3 x y z
	put = undefined


data Triangle = Triangle
	{ tn  :: V3 Float
	, tv1 :: V3 Float
	, tv2 :: V3 Float
	, tv3 :: V3 Float
	}
	deriving (Eq, Ord, Read, Show, Generic)

instance Binary Triangle where
	get = do
		(n,v1,v2,v3) <- get
		return $ Triangle n v1 v2 v3
	put = undefined


readFileBinSTL p = do
	STL tri <- decodeFile p
	return $ concatMap (\(Triangle n a b c) -> [(n,a),(n,b),(n,b)]) tri


readFileSTL :: MonadIO m => FilePath -> m [V3 Float]
readFileSTL p = liftIO $ readSTL <$> readFile p

readSTL :: String -> [V3 Float]
readSTL = catMaybes . map f . map words . lines
	where
	f (('v':_):x:y:z:_) = Just $ V3 (read x) (read y) (read z)
	f _ = Nothing


-- ~ loadSTL :: MonadGL m => String -> m (VArray (V3 Float))
-- ~ loadSTL s = readFileSTL s >>= newVArray



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

