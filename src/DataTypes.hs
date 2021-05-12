--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- DataTypes
--

module DataTypes (
    Cluster(..), defaultCluster,
    Color(..), defaultColor,
    Coordinates(..), defaultCoordinates,
    Pixel(..), defaultPixel
) where

data Cluster = Cluster {
   mean :: Color,
   pixels :: [Pixel]
} deriving (Show)

defaultCluster :: Cluster
defaultCluster = Cluster defaultColor []

data Color = Color {
    r :: Float,
    g :: Float,
    b :: Float
} deriving (Show, Read)

defaultColor :: Color
defaultColor = Color 0 0 0

data Coordinates = Coordinates {
    x :: Int,
    y :: Int
} deriving (Show, Read)

defaultCoordinates :: Coordinates
defaultCoordinates = Coordinates 0 0

data Pixel = Pixel {
    coords :: Coordinates,
    color :: Color
} deriving (Show)

defaultPixel :: Pixel
defaultPixel = Pixel defaultCoordinates defaultColor