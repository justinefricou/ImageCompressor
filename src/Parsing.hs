--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- Parsing
--

module Parsing (parsePixels) where

import DataTypes (Pixel(..), defaultPixel, Coordinates(..), Color(..))

readTuple :: String -> (Int, Int)
readTuple = read

readTruple :: String -> (Int, Int, Int)
readTruple = read

tupleToCoords :: (Int, Int) -> Coordinates
tupleToCoords (x, y) = Coordinates x y

trupleToColor :: (Int, Int, Int) -> Color
trupleToColor (r,g,b) = Color red green blue
                        where red = fromIntegral r
                              green = fromIntegral g
                              blue = fromIntegral b

getPixel :: [String] -> Pixel
getPixel list | length list /= 2 = defaultPixel
              | otherwise = Pixel coords color
              where coords = tupleToCoords $ readTuple $ list !! 0
                    color = trupleToColor $ readTruple $ list !! 1

parsePixels :: [String] -> [Pixel]
parsePixels [] = []
parsePixels (x:xs) = (getPixel $ words x) : (parsePixels xs)
