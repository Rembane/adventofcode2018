{-# LANGUAGE TupleSections #-}
import           Control.Arrow                            ( (&&&)
                                                          , (***)
                                                          , first
                                                          , second
                                                          )
import           Data.Bits                                ( (.&.)
                                                          , (.|.)
                                                          )
import           Data.Char                                ( isDigit )
import           Data.List                                ( break
                                                          , concatMap
                                                          , foldl'
                                                          , groupBy
                                                          , isPrefixOf
                                                          , partition
                                                          , sortOn
                                                          , splitAt
                                                          , stripPrefix
                                                          , uncons
                                                          )
import qualified Data.Map.Strict               as M
import           Data.Maybe                               ( fromJust )
import           Data.Monoid                              ( All(..) )
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Data.Tuple                               ( swap )
import           Debug.Trace

data Opcode
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Eq, Ord, Show)

-- | We ignore the opcode in Instruction for now.
eval :: Opcode -> Instruction -> Machine -> Machine
eval op i m = case op of
  Addr -> binr (+)
  Addi -> bini (+)
  Mulr -> binr (*)
  Muli -> bini (*)
  Banr -> binr (.&.)
  Bani -> bini (.&.)
  Borr -> binr (.|.)
  Bori -> bini (.|.)
  Setr -> upd (readRegister (inputA i) m)
  Seti -> upd (inputA i)
  Gtir -> upd (cmp (>) (inputA i) (readRegister (inputB i) m))
  Gtri -> upd (cmp (>) (readRegister (inputA i) m) (inputB i))
  Gtrr -> upd (cmp (>) (readRegister (inputA i) m) (readRegister (inputB i) m))
  Eqir -> upd (cmp (==) (inputA i) (readRegister (inputB i) m))
  Eqri -> upd (cmp (==) (readRegister (inputA i) m) (inputB i))
  Eqrr ->
    upd (cmp (==) (readRegister (inputA i) m) (readRegister (inputB i) m))
 where
  upd :: Int -> Machine
  upd = updateRegister (output i) m

  binr :: (Int -> Int -> Int) -> Machine
  binr f = upd (f (readRegister (inputA i) m) (readRegister (inputB i) m))

  bini :: (Int -> Int -> Int) -> Machine
  bini f = upd (f (readRegister (inputA i) m) (inputB i))

  cmp :: (Int -> Int -> Bool) -> Int -> Int -> Int
  cmp f a b = if f a b then 1 else 0

-- | Make a lookup on the opcode in the instruction, then run eval with the right opcode.
eval' :: M.Map Int Opcode -> Instruction -> Machine -> Machine
eval' m i = eval (m M.! opcode i) i

data Machine = Machine
  { ra :: Int
  , rb :: Int
  , rc :: Int
  , rd :: Int
  }
  deriving (Eq, Show)

emptyMachine :: Machine
emptyMachine = Machine 0 0 0 0

readRegister :: Int -> Machine -> Int
readRegister i m = case i of
  0 -> ra m
  1 -> rb m
  2 -> rc m
  3 -> rd m
  _ -> error "That is not a register!"

updateRegister :: Int -> Machine -> Int -> Machine
updateRegister i m v = case i of
  0 -> m { ra = v }
  1 -> m { rb = v }
  2 -> m { rc = v }
  3 -> m { rd = v }
  _ -> error "That is not a register!"

parseMachine :: String -> Maybe Machine
parseMachine s =
  let [r1, r2, r3, r4] = map (read . filter isDigit) (words s)
  in  pure $ Machine r1 r2 r3 r4

data Instruction = Instruction
  { opcode :: Int
  , inputA :: Int
  , inputB :: Int
  , output :: Int
  }
  deriving (Show)

parseInstruction :: String -> Maybe Instruction
parseInstruction s = case map read $ words s of
  [r1, r2, r3, r4] -> pure $ Instruction r1 r2 r3 r4
  _                -> Nothing

data Test = Test
  { before      :: Machine
  , instruction :: Instruction
  , after       :: Machine
  }
  deriving (Show)

-- | Does this Test pass for this Opcode?
runTest :: Opcode -> Test -> Bool
runTest o t = facit
  == readRegister outputRegister (eval o (instruction t) (before t))
 where
  outputRegister = output $ instruction t
  facit          = readRegister outputRegister $ after t

-- | Find all opcodes that pass the test.
findPassingOpcodes :: Test -> [Opcode]
findPassingOpcodes t = filter
  (`runTest` t)
  [ Addr
  , Addi
  , Mulr
  , Muli
  , Banr
  , Bani
  , Borr
  , Bori
  , Setr
  , Seti
  , Gtir
  , Gtri
  , Gtrr
  , Eqir
  , Eqri
  , Eqrr
  ]

parseTest :: [String] -> Maybe Test
parseTest [b, i, a] =
  Test
    <$> (parseMachine =<< stripPrefix "Before: " b)
    <*> parseInstruction i
    <*> (parseMachine =<< stripPrefix "After: " a)
parseTest _ = Nothing

addStructure :: [String] -> ([[String]], [String])
addStructure ss =
  let (quad, rest) = first init $ splitAt 4 ss
  in  case isPrefixOf "Before: [" . fst <$> uncons quad of
        Just True -> first (quad :) $ addStructure rest
        _         -> ([], dropWhile null ss)

-- | This creates a mapping between the number of an opcode and the name of the opcode.
loopSolve :: [Test] -> M.Map Int Opcode
loopSolve = loopSolve' M.empty . map (id &&& findPassingOpcodes)
 where
  loopSolve' :: M.Map Int Opcode -> [(Test, [Opcode])] -> M.Map Int Opcode
  loopSolve' m [] = m
  loopSolve' m tos =
    uncurry loopSolve'
      . first
          ( foldr (uncurry M.insert . ((opcode . instruction) *** head)) m
          . (\fs -> if validate fs
              then fs
              else error ("Weirdness in input data: " <> show fs)
            )
          )
      . uncurry clean
      . partition ((== 1) . length . snd)
      . filter
          ( uncurry (&&)
          . ((flip M.notMember m . opcode . instruction) *** (not . null))
          )
      $ tos

  validate :: [(Test, [Opcode])] -> Bool
  validate =
    getAll
      . foldMap (All . (== 1) . Set.size . Set.fromList)
      . foldr (uncurry (M.insertWith (++)) . first (opcode . instruction))
              M.empty

  clean
    :: [(Test, [Opcode])]
    -> [(Test, [Opcode])]
    -> ([(Test, [Opcode])], [(Test, [Opcode])])
  clean os = (os, ) . map (second (filter (`Set.notMember` os')))
    where os' = Set.fromList (concatMap snd os)

main :: IO ()
main = do
  (p1, p2) <-
    first (fromJust . mapM parseTest) . addStructure . lines <$> readFile
      "input.txt"
  putStr "Part 1: "
  print $ length $ filter (>= 3) $ map (length . findPassingOpcodes) p1

  putStr "Part 2: "
  let intToOp = loopSolve p1
  print intToOp
  print $ foldl' (flip (eval' intToOp))
                 emptyMachine
                 (fromJust $ mapM parseInstruction p2)
