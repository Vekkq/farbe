{-# OPTIONS_GHC -fno-warn-tabs #-}

module STL where

import Control.Monad.IO.Class

import Data.Maybe
import Graphics.Farbe.Vec


type Face = [[Float]]



readFileSTL :: MonadIO m => FilePath -> m [V3 Float]
readFileSTL p = liftIO $ readSTL <$> readFile p

readSTL :: String -> [V3 Float]
readSTL = catMaybes . map f . map words . lines
	where
	f (('v':_):x:y:z:_) = Just $ V3 (read x) (read y) (read z)
	f _ = Nothing




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

