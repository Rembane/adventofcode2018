module Day6 where

import Data.Function (on)
import Data.List (sortBy, sortOn)
import Data.Ord (comparing)

newtype Point = Point {unPoint :: (Int, Int)}

-- | Newton distance, lets see if it bites us.
distance :: (Int, Int) -> (Int, Int) -> Double
distance p1 p2 = sqrt $ (fromIntegral $ on (-) fst p1 p2)**2.0 + (fromIntegral $ on (-) snd p1 p2)**2.0

distanceToAll :: (Int, Int) -> [(Int, Int)] -> [Double]
distanceToAll p1 = map (distance p1)

-- | Find out which point surrounding a point as the rook moves that is closest to another point.
closestDir :: (Int, Int) -> (Int, Int) -> (Int, Int)
closestDir = _

main :: IO ()
main = do
  coords <- sortBy (comparing (snd . unPoint) <> comparing (fst . unPoint)) . map ((\[x,y] -> Point (x,y)) . map ((read :: String -> Int) . takeWhile (/= ',')) . words) . lines <$> readFile "input.txt"
  print coords
  print 7
