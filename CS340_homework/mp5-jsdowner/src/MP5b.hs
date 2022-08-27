{-# LANGUAGE FlexibleInstances #-}

module MP5b where
import MP5a
import Data.Maybe
import Data.Ord
import Data.List
import Data.Tree
import Data.Map (Map, empty, fromList, (!), findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO


{- Replace with your own game data types  -}

data Piece = A | B deriving (Eq, Show, Read)


type Board = [Maybe Piece]

emptyBoard :: Board
emptyBoard = replicate 42 Nothing

myChunksOf :: Int -> [a] -> [[a]]
myChunksOf 0 lst = [lst]
myChunksOf _ [] = []
myChunksOf n lst = [take n lst] ++ (myChunksOf n (drop n lst))

instance {-# OVERLAPPING #-} Show Board where
  show = intercalate "\n" . myChunksOf 7 . concat . map showSquare
    where showSquare Nothing = "-"
          showSquare (Just p) = show p

opponent :: Piece -> Piece
opponent A = B
opponent B = A

--play a piece at the specified column (valid positions = 1-7)
playMove :: Int -> Piece -> Board -> Board
playMove column p b | full b = b
                    | column > 7 = playMove (column `mod` 7) p b-- emptyBoard
                    | otherwise = let
                                      chooseRow :: Int -> [[Maybe Piece]] -> Int
                                      chooseRow col [] = 0
                                      chooseRow col (r:rs) | r !! (col-1) == Nothing = (length rs)
                                                           | otherwise = chooseRow col rs
                                      -- chooseRow col (r:rs) -> do
                                      --                             case r !! (col-1) of
                                      --                               Nothing -> do (length rs)
                                      --                               _ -> do putStrLn "Illegal move"
                                      --                                     chooseRow col (r:rs)
                                      rows' = reverse (myChunksOf 7 b)
                                      k = chooseRow column rows'
                                      rows = reverse rows'
                                      beforeMe = concat (take k rows)
                                      myRow = head (take 1 (drop k rows))
                                      afterMe = concat (take (6 - (k+1)) (drop (k+1) rows))
                                      myRowMod = (take (column - 1) myRow) ++ [Just p] ++ (drop column myRow)
                                  in  (beforeMe ++ myRowMod ++ afterMe)

--list of moves starting with empty board
playMoves :: [Int] -> Board
playMoves moves = play moves A emptyBoard
  where play [] _ b = b
        play (m:ms) p b = let (m':ms') = [((m_ - 1) `mod` 7) + 1 | m_ <- (m:ms)]
                              b' = playMove m' p b
                          in (play ms' (opponent p) b')





{- Some convenience functions and types -- feel free to replace/modify  -}

-- prune :: Int -> Tree a -> Tree a
-- prune 0 (Node x _) = Node x []
-- prune _ (Node x []) = Node x []
-- prune n (Node x ns) = Node x $ map (prune (n-1)) ns


element_wise_add :: [Int] -> [Int] -> [Int]
element_wise_add [] _  = []
element_wise_add  _ [] = []
element_wise_add (a:as) (b:bs) = [a+b] ++ (element_wise_add as bs)

----------------------------------------------------

-- -- illustrating with char/num coordinates -- a1, b2, c3, etc.
--rs' = concat [r1,r2,r3,r4,r5,r6]
-- getDiags :: [[(Int,Int)]] -> [[Char]] -> [[[Char]]]
-- getDiags [] _ = []
-- getDiags (p:ps) b = let rows = myChunksOf 7 b
--                         (i:is) = p
--                         abs = zip (fst <$> (i:is)) (snd <$> (i:is))
--                         first_list = [rows !! a !! b | (a,b) <- abs]
--                     in ([first_list] ++ getDiags ps b)

-- [rows1,rows2,rows3,rows4,rows5,rows6] = replicate 7 <$> ['a'..'f']
-- r1 = element_wise_concat rows1 ['1'..'7']
-- r2 = element_wise_concat rows2 ['1'..'7']
-- r3 = element_wise_concat rows3 ['1'..'7']
-- r4 = element_wise_concat rows4 ['1'..'7']
-- r5 = element_wise_concat rows5 ['1'..'7']
-- r6 = element_wise_concat rows6 ['1'..'7']

-- element_wise_concat :: [Char] -> [Char] -> [[Char]]
-- element_wise_concat [] _  = []
-- element_wise_concat  _ [] = []
-- element_wise_concat (a:as) (b:bs) = [[a,b]] ++ (element_wise_concat as bs)

-- diagonal indices


--------------------------------------------------------
getDiagIdx:: Bool -> [Int] -> [[Int]]
getDiagIdx positive nums | positive == True = (\n -> [element_wise_add [0..3] (replicate 4 k)  | k <- n]) nums
                         | otherwise = (\n -> [element_wise_add [0,-1,-2,-3] (replicate 4 k)  | k <- n]) nums

diagsFromIdx :: [[(Int,Int)]] -> [Maybe Piece] -> [[Maybe Piece]]
diagsFromIdx [] _ = []
diagsFromIdx (p:ps) b = let rows = myChunksOf 7 b
                            (i:is) = p
                            abs = zip (fst <$> (i:is)) (snd <$> (i:is))
                            first_list = [rows !! a !! b | (a,b) <- abs]
                        in ([first_list] ++ diagsFromIdx ps b)

-- diagsFromIdx' :: [[(Int,Int)]] -> [String] -> [[String]]
-- diagsFromIdx' [] _ = []
-- diagsFromIdx' (p:ps) b = let rows = myChunksOf 7 b
--                              (i:is) = p
--                              abs = zip (fst <$> (i:is)) (snd <$> (i:is))
--                              first_list = [rows !! a !! b | (a,b) <- abs]
--                          in ([first_list] ++ diagsFromIdx' ps b)


-- b' = playMoves [1,2,1,2,1,2,3,4,3,4,3,4,5,6,5,6,5,6,7,1,7,1,7,1,2,2,2,2,4,4,5,5,6,6]


-- lines'' :: [String] -> [[String]]
-- lines'' b =  let (w,h) = (7,6)
--                  rows = myChunksOf w b
--                  -------
--                  groupsOf4Rows = [(!!) rows <$> i | i <- [element_wise_add [0..3](replicate 4 k) | k <- [0..2]]]
--                  (a',b',b'') = (getDiagIdx True [0..2], getDiagIdx True [0..3], getDiagIdx False [3..6])
--                  diag_idx = [zip a b | a <- a', b <- b'] ++ [zip a b | a <- a', b <- b'']
                 
--                  wins_c = [map (!! i) g | g <- groupsOf4Rows, i <- [0..6]]
--                  wins_r = [(!!) r <$> (element_wise_add [0..3] (replicate 4 k)) | r <- rows, k <- [0..3]]
--                  wins_d = diagsFromIdx' diag_idx b
--                       --  | rs <- [rows, reverse rows]]
--              in  concat [wins_r, wins_c, wins_d]

--------------------------------------------------------

(a_,b_,b_') = (getDiagIdx True [0..2], getDiagIdx True [0..3], getDiagIdx False [3..6])
diag_idx = [zip a b | a <- a_, b <- b_] ++ [zip a b | a <- a_, b <- b_']
--[(!!) (myChunksOf $ playMoves [1..42]) <$> i | i <- [element_wise_add [0..3](replicate 4 k) | k <- [0..2]]]


 

lines' :: Board -> [[Maybe Piece]]
lines' b =  let (w,h) = (7,6)
                rows = myChunksOf w b
                -------
                groupsOf4Rows = [(!!) rows <$> i | i <- [element_wise_add [0..3](replicate 4 k) | k <- [0..2]]]
                (a',b',b'') = (getDiagIdx True [0..2], getDiagIdx True [0..3], getDiagIdx False [3..6])
                diag_idx = [zip a b | a <- a', b <- b'] ++ [zip a b | a <- a', b <- b'']
                wins_c = [map (!! i) g | g <- groupsOf4Rows, i <- [0..6]]
                wins_r = [(!!) r <$> (element_wise_add [0..3] (replicate 4 k)) | r <- rows, k <- [0..3]]
                wins_d = diagsFromIdx diag_idx b
                      --  | rs <- [rows, reverse rows]]
            in  concat [wins_r, wins_c, wins_d]
--------------------------------------------------------
-- did the specified piece win?
wins :: Piece -> Board -> Bool
wins p = any (all (== Just p)) . lines'
-- eg. B win: 

-- returnWin :: Bool -> []

--b = playMove 5 B (playMoves [1..20])

-- did someone win?
won :: Board -> Bool
won b = wins A b || wins B b

-- is the board full?
full :: Board -> Bool
full = all (/= Nothing)

-- -- whose turn is it?
turn :: Board -> Piece
turn board = let a = length $ filter (== Just A) board
                 b = length $ filter (== Just B) board
         in if a  <= b then A else B
--------------------------------------------------------

data Scored a = Scored { score :: Int, scoredVal :: a }
instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y
instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y
instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v



-- [1,2,1,2,1,2,3,4,3,4,3,4,5,6,5,6,5,6,7,1,7,1,7,1,2,2,2,2]


playInteractive :: IO ()
playInteractive = play A emptyBoard 
  where play turn board
          | wins A board = putStrLn "'A' wins!"
          | wins B board = putStrLn "'B' wins!"
          | full board = putStrLn "Drawn"
          | otherwise = do
              putStr "Enter a move: "
              --move' <- getLine
              move' <- readLn
              case board !! (move'-1) of
                (Just _) -> do putStrLn "Illegal move"
                               play turn board
                _ -> do let board' = playMove move' turn board
                        print board'
                        play (opponent turn) board'


-- Each node in the tree represents the state of the board, and its children
-- represent the different board states resulting from playing a valid move on the
-- parent's board.
gameTree :: Board -> Tree Board
gameTree b = Node b 
             $ map gameTree 
             $ map (\i -> playMove (i+1) (turn b) b) moves
    where
        rows = reverse (myChunksOf 7 b)
        moves | won b || full b = []
              | otherwise = removeDuplicates $ move rows

move :: [[Maybe Piece]] -> [Int]
move [] = []
move [[]] = []
move (r:rs) = removeDuplicates $ [c +1| c <- [0..6], r !! c == Nothing] ++ (move rs)



-- rs' = myChunksOf 7 b'
-- r6 = rs' !! 0
-- r5 = rs' !! 1
-- r4 = rs' !! 2
-- r3 = rs' !! 3
-- r2 = rs' !! 4
-- r1 = rs' !! 5

-- availMoves :: Board -> [Int]
-- availMoves b = let (r:rows) = myChunksOf 7 b
--                    [c | c <- [1..7], 


-- CLOSE TO FINISHED BOARDS:  
-- b' = playMoves [1,2,1,2,1,2,3,4,3,4,3,4,5,6,5,6,5,6,7,1,7,1,7,1,2,2,2,2,4,4,4,7,7,7]
-- b''=playMoves [1,1,3,3,5,5,7,7,1,2,3,4,5,6,7,2,2,5,6,6,4,1,1,2,5,3,1,6,6,2,3,3,2]
-- b = playMoves [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,2,3,4,5,6,7,6,1,1,2,3,7,2,1,7,4,5]
-- constrains the maximum height of a tree
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns
-- convenience function for printing a tree
-- e.g., printTree $ playMoves [1..6]
printTree :: Board -> IO ()
printTree = putStrLn . drawTree . fmap show . gameTree

scoreBoard :: Piece -> Board -> Scored Board
scoreBoard p b | wins p b = Scored 100 b
               | wins (opponent p) b = Scored (-100) b
               | otherwise = Scored 0 b

scoreBoard' :: (Piece, Board) -> Scored Board
scoreBoard' (p, b) | wins p b = Scored 100 b
                   | wins (opponent p) b = Scored (-100) b
                   | otherwise = Scored 0 b
-- convenience function for printing a scored tree
-- e.g., printScoredTree $ playMoves [1,5,9,2,8,7]

-- playMoves [1..22]
-- scoreBoard B b
-- --
-- output:
-- --
-- Score: 100

printScoredTree :: Board -> IO ()
printScoredTree = putStrLn . drawTree . fmap (show . scoreBoard A) . gameTree


-- Minimax function from lecture notes
-- minimax :: (a -> Scored a) -> Tree a -> Scored a
-- minimax scorefn (Node _ ns) = maximize ns
--   where maximize = maximumBy (comparing score) . map (eval minimize)
--         minimize = minimumBy (comparing score) . map (eval maximize)
--         eval _ (Node x []) = scorefn x
--         eval f (Node x ns) = let Scored s _ = f ns in Scored s x

-- minimax' :: (a -> Scored a) -> Tree a -> Scored a
-- minimax' scorefn (Node _ ns) = maximize ns
--   where maximize = maximumBy (comparing score) . map (eval minimize)
--         minimize = minimumBy (comparing score) . map (eval maximize)
--         eval _ (Node x []) = scorefn x
--         eval f (Node x ns) = let Scored s _ = f ns in Scored s x



playAI :: IO ()
playAI = play A emptyBoard
  where play :: Piece -> Board -> IO ()
        play _ b | wins A b = putStrLn "'A' wins!"
                 | wins B b = putStrLn "'B' wins!"
                 | full b = putStrLn "Draw"
        play A b = do 
          putStr "Enter a move: "
          move' <- readLn
          case b !! (move'-1) of
            (Just _) -> do putStrLn "Illegal move"
                           play A b
            _ -> do let b' = playMove move' A b
                    print b'
                    play B b'
        play B b = do
          let b' = scoredVal $ minimax_ab (scoreBoard B) (gameTree b)
          print b'
          play A b'



instance Bounded (Scored a) where
  minBound = Scored minBound undefined
  maxBound = Scored maxBound undefined

minimax_ab :: (a -> Scored a) -> Tree a -> Scored a
minimax_ab scorefn (Node _ ns) = maximize minBound maxBound ns
  where maximize a b [] = minBound
        maximize a b (n:ns) | a > b = a
                            | otherwise = let sn = eval a b minimize n
                                              a' = max a sn
                                          in max sn $ maximize a' b ns
        minimize a b [] = maxBound
        minimize a b (n:ns) | a > b = b
                            | otherwise = let sn = eval a b maximize n
                                              b' = min b sn
                                          in min sn $ minimize a b' ns
        eval a b f (Node x []) = scorefn x
        eval a b f (Node x ns) = let Scored s _ = f a b ns
                                 in Scored s x