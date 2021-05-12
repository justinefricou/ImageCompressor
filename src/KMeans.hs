--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- KMeans
--

module KMeans (getNRandomColors, getOutput) where

import System.Random

import DataTypes (Cluster(..), Color(..), defaultColor, Pixel(..))
import Display (showClustersList)

-- Returns an array of n random colors
getNRandomColors :: Int -> IO [Color]
getNRandomColors n = do
   gen <- getStdGen
   return (getNColors n (randomRs (0, 255) gen :: [Int]))

-- Returns an array of n colors from the array of integers given as param
getNColors :: Int -> [Int] -> [Color]
getNColors 0 _ = []
getNColors n list = ((Color red green blue) : (getNColors (n - 1) list))
    where red = fromIntegral $ list !! (n * 3)
          green = fromIntegral $ list !! (n * 3 + 1)
          blue = fromIntegral $ list !! (n * 3 + 2)

-- Returns a String to show the list of final clusters
getOutput :: [Color] -> [Pixel] -> Float -> String
getOutput co pix lim = showClustersList $ getClusters (initClusters co) pix lim

-- Returns the clusters, filled with the right pixels
getClusters :: [Cluster] -> [Pixel] -> Float -> [Cluster]
getClusters clust px lim | (isLimReached current prev lim) = current
                         | otherwise = getClusters current px lim
   where current = updateMeans $ fixEmptyClusters (dispatchPix emptyClust px) 0
         prev = clust
         emptyClust = initClusters $ getMeanColors prev

-- Returns an array of clusters with the given mean colors and no pixels
initClusters :: [Color] -> [Cluster]
initClusters [] = []
initClusters (x:xs) = ((Cluster x []) : initClusters xs)

-- Returns True if every cluster has shifted less than the convergence limit
isLimReached :: [Cluster] -> [Cluster] -> Float -> Bool
isLimReached (x:_) [] _ = True
isLimReached [] (x:_) _ = True
isLimReached [x1] [x2] l | (colorShift (mean x1) (mean x2)) <= l = True
                         | otherwise = False
isLimReached (x1:xs1) (x2:xs2) l | (colorShift (mean x1) (mean x2)) > l = False
                                 | otherwise = isLimReached xs1 xs2 l

-- Returns the difference (euclidean distance) between 2 colors
colorShift :: Color -> Color -> Float
colorShift color1 color2 = sqrt (re*re + gr*gr + bl*bl)
   where re = (r color1) - (r color2)
         gr = (g color1) - (g color2)
         bl = (b color1) - (b color2)

-- Fills still empty clusters (after dispatching pixels) with the closest pixel
fixEmptyClusters :: [Cluster] -> Int -> [Cluster]
fixEmptyClusters [] _ = []
fixEmptyClusters list i | (length list) <= i = list
                        | pixelArrayLen == 0 = fixEmptyClusters newList 0
                        | otherwise = fixEmptyClusters list (i + 1)
                        where pixelArrayLen = getNbPx $ list !! i
                              newList = reverse $ fixEmptyCluster list i

-- Moves a pixel from a big enough cluster to an empty cluster (index as param)
fixEmptyCluster :: [Cluster] -> Int -> [Cluster]
fixEmptyCluster [] _ = []
fixEmptyCluster l i | (idxSrcClust == -1 || idxSrcPixel == -1) = l
                    | otherwise = finalList
 where finalList = replaceClustAtIdx listDuplicatePx newSrcClust idxSrcClust
       (idxSrcClust, idxSrcPixel) = closestInClusts (mean $ l !! i) l
       listDuplicatePx = replaceClustAtIdx l filledClust i
       filledClust = Cluster (mean $ l !! i) [movedPixel]
       newSrcClust = Cluster (mean $ l !! idxSrcClust) newSrcArray
       newSrcArray = removePixInArray (pixels (l !! idxSrcClust)) idxSrcPixel
       movedPixel = (pixels (l !! idxSrcClust)) !! idxSrcPixel

-- Replaces a cluster in an array at a given index with the given cluster
replaceClustAtIdx :: [Cluster] -> Cluster -> Int -> [Cluster]
replaceClustAtIdx [] _ _ = []
replaceClustAtIdx list clust i = startArray ++ [clust] ++ endArray
      where startArray = take i list
            endArray = reverse $ take ((length list) - i - 1) (reverse list)

-- Removes the cluster at the given index from the array
removePixInArray :: [Pixel] -> Int -> [Pixel]
removePixInArray [] _ = []
removePixInArray list i = startArray ++ endArray
      where startArray = take i list
            endArray = reverse $ take ((length list) - i - 1) (reverse list)

-- Finds the pixel closest to the given color amongst the clusters and returns
-- the indexes of the cluster and the pixel in this cluster's pixel array
closestInClusts :: Color -> [Cluster] -> (Int, Int)
closestInClusts col [x] | getNbPx x < 2 = (-1, -1)
                        | otherwise = (0, (closestInArray col (pixels x)))
closestInClusts c (x:xs) | getNbPx x < 2 = (iClustTail + 1, iPxTail)
                         | iClustTail == -1 = (0, idxPxInHead)
                         | distHead <= distTail = (0, idxPxInHead)
                         | otherwise = (iClustTail + 1, iPxTail)
 where distHead = colorShift c (color $ (pixels x) !! idxPxInHead)
       distTail = colorShift c (color $ (pixels (xs !! iClustTail)) !! iPxTail)
       idxPxInHead = closestInArray c (pixels x)
       (iClustTail, iPxTail) = closestInClusts c xs

-- Returns the number of pixels contained in the cluster given as parameter
getNbPx :: Cluster -> Int
getNbPx cluster = length $ pixels cluster

-- Returns the pixel whose color is the closest to the one given as param
closestInArray :: Color -> [Pixel] -> Int
closestInArray _ [] = 0
closestInArray _ [x] = 0
closestInArray col (x:xs) | distHead <= distTail = 0
                            | otherwise = idxClosestInTail + 1
            where distHead = colorShift col (color x)
                  distTail = colorShift col (color (xs !! idxClosestInTail))
                  idxClosestInTail = closestInArray col xs

-- Updates the final clusters with their new mean
updateMeans :: [Cluster] -> [Cluster]
updateMeans [] = []
updateMeans (x:xs) = ((Cluster newMeanColor (pixels x)) : (updateMeans xs))
                     where newMeanColor = calcMeanColor $ pixels x

-- Fills the clusters with the right pixels
dispatchPix :: [Cluster] -> [Pixel] -> [Cluster]
dispatchPix [] _ = []
dispatchPix clusters [] = clusters
dispatchPix clusters (x:xs) = dispatchPix newClusters xs
    where newClusters = sortPixel clusters x (getIndexClosestClust clusters x)

-- Puts a pixel into the cluster at the given index
sortPixel :: [Cluster] -> Pixel -> Int -> [Cluster]
sortPixel [] _ _ = []
sortPixel (x:xs) px 0 = ((Cluster (mean x) ((pixels x) ++ [px])) : xs)
sortPixel (x:xs) px n = (x : (sortPixel xs px (n - 1)))

-- Returns the index of the cluster that's the closest to the given pixel
getIndexClosestClust :: [Cluster] -> Pixel -> Int
getIndexClosestClust [] _ = 0
getIndexClosestClust [x] _ = 0
getIndexClosestClust (x:xs) pix | distCurrent <= distTail = 0
                                | otherwise = idxClosestInTail + 1
        where distCurrent = colorShift (mean x) (color pix)
              distTail = colorShift (mean (xs !! idxClosestInTail)) (color pix)
              idxClosestInTail = getIndexClosestClust xs pix

-- Returns an array of the mean colors of the clusters given as parameter
getMeanColors :: [Cluster] -> [Color]
getMeanColors [] = []
getMeanColors (x:xs) = ((mean x) : (getMeanColors xs))

-- Returns the mean color of the given pixels
calcMeanColor :: [Pixel] -> Color
calcMeanColor [] = defaultColor
calcMeanColor pixels = Color (meanRed) (meanGreen) (meanBlue)
                       where meanRed = getAverage $ getIntensities 'r' pixels
                             meanGreen = getAverage $ getIntensities 'g' pixels
                             meanBlue = getAverage $ getIntensities 'b' pixels

-- Returns the average of the values contained in the given array
getAverage :: [Float] -> Float
getAverage [] = 0
getAverage values = (sum values) / (fromIntegral $ length values)

-- Returns the intensities of red, green or blue for the given pixels
getIntensities :: Char -> [Pixel] -> [Float]
getIntensities _ [] = []
getIntensities 'r' (x:xs) = ((r $ color x) : (getIntensities 'r' xs))
getIntensities 'g' (x:xs) = ((g $ color x) : (getIntensities 'g' xs))
getIntensities 'b' (x:xs) = ((b $ color x) : (getIntensities 'b' xs))
getIntensities c (x:xs) = (0 : (getIntensities c xs))
