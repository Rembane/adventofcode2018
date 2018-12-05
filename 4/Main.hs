{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.Ix
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Text.Regex.Applicative

data Action
  = Begin Int -- which guard...
  | FallsAsleep
  | WakesUp
  deriving (Show)

isBegin (Begin _) = True
isBegin _         = False

data DateTime = DateTime
  { year   :: Int
  , month  :: Int
  , day    :: Int
  , hour   :: Int
  , minute :: Int
  }
  deriving (Eq, Ord, Show)

data Sleeping = Sleeping
  { guardId :: Int
  , start   :: Int
  , stop    :: Int
  }
  deriving Show

intPattern :: RE Char Int
intPattern = read <$> some (psym isDigit)

datetimePattern :: RE Char DateTime
datetimePattern =
  DateTime
    <$> (intPattern <* "-")
    <*> (intPattern <* "-")
    <*> (intPattern <* " ")
    <*> (intPattern <* ":")
    <*> intPattern

actionPattern :: RE Char Action
actionPattern =
  (Begin <$> ("Guard #" *> intPattern <* " begins shift"))
    <|> (FallsAsleep <$ "falls asleep")
    <|> (WakesUp <$ "wakes up")

linePattern :: RE Char (DateTime, Action)
linePattern = (,) <$> ("[" *> datetimePattern) <*> ("] " *> actionPattern)

crunchList :: [(DateTime, Action)] -> [Sleeping]
crunchList [] = []
crunchList ((_, Begin gid) : xs) =
  uncurry (++) . (createSleep *** crunchList) . break (isBegin . snd) $ xs
 where
  createSleep :: [(DateTime, Action)] -> [Sleeping]
  createSleep [] = []
  createSleep ((start, FallsAsleep) : (stop, WakesUp) : rs) =
    Sleeping gid (minute start) (minute stop - 1) : createSleep rs

main :: IO ()
main = do
  guards <-
    groupBy (on (==) guardId)
    .   sortOn guardId
    .   crunchList
    .   sortOn fst
    .   concat
    .   maybeToList
    .   match (some (linePattern <* many "\n"))
    <$> readFile "input.txt"

  putStr "Part one: "
  print
    $ ( uncurry (*)
      . second
          ( fst
          . maximumBy (comparing snd)
          . map (head &&& length)
          . group
          . sort
          )
      . (guardId . head &&& concatMap (range . (start &&& stop)))
      . fst
      . maximumBy (comparing snd)
      . map (id &&& getSum . foldMap (Sum . uncurry (-) . (stop &&& start)))
      )
        guards

  putStr "Part two: "
  print
    $ uncurry (*)
    . second fst
    . maximumBy (comparing (snd . snd))
    . map
        (   guardId
        .   head
        &&& ( maximumBy (comparing snd)
            . map (head &&& length)
            . group
            . sort
            . concatMap (range . (start &&& stop))
            )
        )
    $ guards
