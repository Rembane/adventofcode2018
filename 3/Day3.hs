module Day3 where

import           Control.Arrow                            ( first
                                                          , second
                                                          )
import           Data.Function                            ( on )
import           Data.Maybe                               ( catMaybes )
import           Data.Ord                                 ( comparing )
import           Data.List

-- (x, y)
newtype Point = Point { unPoint :: (Int, Int) } deriving (Eq)

instance Ord Point where
  compare p1 p2 = comparing (fst . unPoint) p1 p2
               <> comparing (snd . unPoint) p1 p2

instance Show Point where
  show = show . unPoint

data Rectangle = Rectangle
  { topleft     :: Point
  , bottomright :: Point
  } deriving (Eq, Show)

instance Ord Rectangle where
  compare = comparing topleft

inRectangle :: Point -> Rectangle -> Bool
inRectangle p r = p >= topleft r && p <= bottomright r

-- If there is an overlap, we get back a new Rectangle with the overlap.
-- This function assumes that rectangle one is less than rectangle two.
overlap :: Rectangle -> Rectangle -> Maybe Rectangle
overlap r1 r2
  | inRectangle (topleft r2) r1 || inRectangle (bottomright r1) r2
  = pure $ Rectangle (Point (topX, min bottomY topY))
                     (Point (bottomX, max topY bottomY))
  | otherwise
  = Nothing
 where
  topY    = on max (snd . unPoint . topleft) r1 r2
  topX    = on max (fst . unPoint . topleft) r1 r2
  bottomY = on min (snd . unPoint . bottomright) r1 r2
  bottomX = on min (fst . unPoint . bottomright) r1 r2

area :: Rectangle -> Int
area r = w * h
 where
  h = snd (unPoint (bottomright r)) - snd (unPoint (topleft r))
  w = fst (unPoint (bottomright r)) - fst (unPoint (topleft r))

parseRectangle :: String -> Rectangle
parseRectangle s =
  let (lft, rs1) = (first read . break (== ',') . tail . dropWhile (/= '@')) s
      (tp , rs2) = (first read . break (== ':') . tail) rs1
      (wd , rs3) = (first read . break (== 'x') . drop 2) rs2
      ht         = (read . tail) rs3
  in  Rectangle (Point (lft, tp)) (Point (lft + wd, tp + ht))

main :: IO ()
main = do
  xs <- sort . map parseRectangle . lines <$> readFile "input.txt"
  print $ sum $ map area $ catMaybes $ zipWith overlap xs (tail xs)
  -- print xs
