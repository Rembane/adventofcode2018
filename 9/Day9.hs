module Main where

import           Control.Monad.State.Strict
import           Data.Foldable                            ( foldMap )
import           Data.List                                ( maximum
                                                          , replicate
                                                          )
import           Data.Monoid                              ( Endo(..) )

-- | Counter clockwise, current:clockwise.
data Circle = Circle [Int] [Int]
  deriving Show

data GameState = GameState
  { next         :: Int
  , marbleCircle :: Circle
  , players      :: Circle
  } deriving Show

emptyCircle = Circle [] [0]

popCurrent :: Circle -> (Int, Circle)
popCurrent (Circle ccw (x : cw)) = (x, Circle ccw cw)
popCurrent (Circle ccw []) = let (c : cw') = reverse ccw in (c, Circle [] cw')

shiftRight :: Circle -> Circle
shiftRight c = let (n, Circle ccw cw) = popCurrent c in Circle (n : ccw) cw

shiftLeft :: Circle -> Circle
shiftLeft (Circle []        cw) = let (c : cw') = reverse cw in Circle cw' [c]
shiftLeft (Circle (x : ccw) cw) = Circle ccw (x : cw)

placeMarble :: Int -> Circle -> Circle
placeMarble m mc =
  let (Circle ccw cw) = (shiftRight . shiftRight) mc in Circle ccw (m : cw)

-- | Number of players...
emptyGameState :: Int -> GameState
emptyGameState n = GameState 1 emptyCircle (Circle [] $ replicate n 0)

play :: State GameState ()
play = modify
  (\s -> if (== 0) (mod (next s) 23)
    then
      let (c, mc') = popCurrent
            (appEndo (foldMap Endo (replicate 7 shiftLeft)) (marbleCircle s))
      in  s
            { next = next s + 1
            , marbleCircle = mc'
            , players = let (p1, Circle ccw cw) = popCurrent (players s)
                        in  shiftRight (Circle ccw ((next s + p1 + c) : cw))
            }
    else s { next         = next s + 1
           , marbleCircle = placeMarble (next s) (marbleCircle s)
           , players      = shiftRight $ players s
           }
  )


-- Note that you need to compile the program for it to work with part 2.
-- Puzzle input: 486 players; last marble is worth 70833 points
main :: IO ()
main = do
  putStr "Part 1: "
  let s = execState (replicateM_ 70833 play) (emptyGameState 486)
  let (Circle ccw cw) = players s in print $ maximum (ccw ++ cw)

  putStr "Part 2: "
  let s = execState (replicateM_ 7083300 play) (emptyGameState 486)
  let (Circle ccw cw) = players s in print $ maximum (ccw ++ cw)
