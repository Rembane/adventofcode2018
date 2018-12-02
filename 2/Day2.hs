module Day2 where

import           Control.Arrow                            ( (***)
                                                          , (&&&)
                                                          )
import           Data.List                                ( group
                                                          , sort
                                                          )
solve1 :: [String] -> Int
solve1 = uncurry (*) . (f *** f) . unzip . map
  ((elem 2 &&& elem 3) . map length . group . sort)
  where f = length . filter id

solve2 :: [String] -> String
solve2 =
  map fst
    . filter (uncurry (==))
    . head
    . filter ((== 1) . length . filter (uncurry (/=)))
    . uncurry (zipWith zip)
    . (id &&& tail)
    . sort

main :: IO ()
main = do
  codes <- lines <$> readFile "input.txt"
  putStr "Part one: "
  print $ solve1 codes

  putStr "Part two: "
  print $ solve2 codes
