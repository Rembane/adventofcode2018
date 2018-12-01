module Day1 where

import           Data.List                                ( cycle
                                                          , foldl'
                                                          , scanl'
                                                          )
import           Data.Maybe                               ( fromJust
                                                          , isNothing
                                                          )
import qualified Data.Set                      as S

parse :: String -> Int
parse ('+' : xs) = read xs
parse ('-' : xs) = negate (read xs)
parse s          = error ("Wrong parse: " <> s)

data Cxt = Cxt { curr :: Int, seen :: S.Set Int, solution :: Maybe Int }

emptyCxt :: Cxt
emptyCxt = Cxt 0 S.empty Nothing

main :: IO ()
main = do
  frequencies <- map parse . lines <$> readFile "input.txt"

  putStr "Part 1: "
  print $ sum frequencies

  putStr "Part 2: "
  print $ fromJust . solution . head $ dropWhile
    (isNothing . solution)
    (scanl'
      (\acc i ->
        let i' = i + curr acc
        in  if S.member i' (seen acc)
              then acc { solution = pure i' }
              else acc { seen = S.insert i' (seen acc), curr = i' }
      )
      emptyCxt
      (cycle frequencies)
    )
