--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- Display
--

module Display (
    showClustersList,
    showCluster,
    showPixel,
    showCoordinates,
    showColor
) where

import DataTypes (Cluster(..), Pixel(..), Coordinates(..), Color(..))

showClustersList :: [Cluster] -> String
showClustersList [] = ""
showClustersList (x:xs) = (showCluster x) ++ (showClustersList xs)

showCluster :: Cluster -> String
showCluster c = "--\n" ++ (showColor $ mean c) ++ "\n-\n\
                \" ++ (showPixelsList $ pixels c)

showPixelsList :: [Pixel] -> String
showPixelsList [] = ""
showPixelsList (x:xs) = (showPixel x) ++ "\n" ++ (showPixelsList xs)

showPixel :: Pixel -> String
showPixel p = (showCoordinates $ coords p) ++ " " ++ (showColor $ color p)

showCoordinates :: Coordinates -> String
showCoordinates (Coordinates x y) = show (x, y)

showColor :: Color -> String
showColor (Color r g b) = show (round r, round g, round b)