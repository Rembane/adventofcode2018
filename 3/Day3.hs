{-# LANGUAGE TupleSections #-}
module Day3 where

import           Control.Arrow                            ( first
                                                          , second
                                                          )
import           Data.Function                            ( on )
import           Data.Ix                                  ( range )
import           Data.List                                ( concat
                                                          , concatMap
                                                          , lines
                                                          , group
                                                          , groupBy
                                                          , maximumBy
                                                          , sort
                                                          , sortOn
                                                          )
import           Data.Maybe                               ( mapMaybe )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                          ( Map )
import           Data.Ord                                 ( comparing )
import           Data.Tuple                               ( swap )

-- ... -> [((Claim id, (x, y))]
parseRectangle :: String -> [(Int, (Int, Int))]
parseRectangle s =
  let (cid, rs0) = (first read . break (== ' ') . tail) s
      (lft, rs1) =
        (first read . break (== ',') . tail . dropWhile (/= '@')) rs0
      (tp, rs2) = (first read . break (== ':') . tail) rs1
      (wd, rs3) = (first read . break (== 'x') . drop 2) rs2
      ht        = (read . tail) rs3
      rs        = (,) <$> range (lft, lft + wd - 1) <*> range (tp, tp + ht - 1)
  in  map ((,) cid) rs

lengthOne :: [a] -> Bool
lengthOne [_] = True
lengthOne _   = False

main :: IO ()
main = do
  cs <- concatMap parseRectangle . lines <$> readFile "input.txt"
  putStr "Part 1: "
  print
    $ length
    . filter (> 1)
    . M.elems
    . M.fromListWith (+)
    . map ((, 1) . snd)
    $ cs

  putStr "Part 2: "
  print
    $ let coords = (M.fromListWith (++) . map (second pure . swap)) cs
          m      = (M.fromListWith (++) . map (second pure)) cs
      in  head
          . mapMaybe
              (\(cid, cs) ->
                case all lengthOne <$> mapM (`M.lookup` coords) cs of
                  Just True -> Just cid
                  _         -> Nothing
              )
          $ M.assocs m

