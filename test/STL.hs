{-# OPTIONS_GHC -fno-warn-tabs #-}

module STL where

import Text.ParserCombinators.ReadP
import Data.List (tails)
import Control.Monad.IO.Class

import Data.Maybe
import Graphics.Farbe.Vec


import Debug.Trace


type Face = [[Float]]





--main = test

test = do
	x <- readFileSTL "cube.stl"
	mapM_ print x
	writeFileSTL "cube2.stl" x


readFileSTL :: MonadIO m => FilePath -> m [Face]
readFileSTL p = liftIO $ readSTL <$> readFile p


readSTL :: String -> [Face]
readSTL = group3 . map² read . concatMap f . map words . lines
	where
	f (('v':_):x:y:z:_) = [[x,y,z]]
	f _ = []

	map² = map . map

	group3 (a:b:c:xs) = [a,b,c] : group3 xs
	group3 _ = []



readFileSTL' :: MonadIO m => FilePath -> m [V3 Float]
readFileSTL' p = liftIO $ readSTL' <$> readFile p


readSTL' :: String -> [V3 Float]
readSTL' = catMaybes . map f . map words . lines
	where
	f (('v':_):x:y:z:_) = Just $ V3 (read x) (read y) (read z)
	f _ = Nothing

	map² = map . map

	group3 (a:b:c:xs) = [a,b,c] : group3 xs
	group3 _ = []






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

