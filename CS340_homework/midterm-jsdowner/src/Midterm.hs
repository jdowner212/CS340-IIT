module Midterm where
import Data.Char

student_name = "Jane Downer"      
student_id   = "A20452471"



-- ## Part A: Polymorphic functions

-- For functions `pa_1`, `pa_2`, `pa_3`, `pa_4`, and `pa_5`, we include polymorphic
-- type declarations but no working definitions. You are to provide a working,
-- well-typed definition for each function that returns without error for valid
-- inputs. 

-- Part A: Polymorphic functions 
     

pa_1 :: (a -> b -> c) -> (d -> a) -> (e -> b) -> d -> e -> c
pa_1 f a b d e = f (f1 d) (f2 e)
   where f1 = (\d -> a d)
         f2 = (\e -> b e)

pa_2 :: (a -> a -> [a]) ->  b ->  (b -> [a] -> c)    -> a    -> c
pa_2 = undefined


pa_3 :: (a -> [b]) -> (b -> c) -> a -> c -> [c]
pa_3 atoB btoC a c = [btoC (head (atoB a))]


pa_4 :: ((a -> b) -> c -> d) -> b -> c -> d
pa_4 = undefined


pa_5 :: ((a,b) -> c) -> (c -> d) -> [(a,b)] -> [d]
pa_5 abtoC ctoD [(a,b)] = [ctoD (abtoC (a, b))]


-- Part B: Recursion and List processing

{-
   Creates an infinite list of lists, where each list starts with the given 
   element and extends one successor further than the previous one.

   Examples:

   listsFrom 5
   => [[5],[5,6],[5,6,7],[5,6,7,8],[5,6,7,8,9],...]

   listsFrom 'e'
   => ["e","ef","efg","efgh","efghi","efghij","efghijk","efghijkl",...]
-}
listsFrom :: Enum a 
          => a -- the starting element for each list
          -> [[a]]
listsFrom a = [[a..inf] | inf <- [a..]]



{-
   "Riffles" together elements from the two argument lists, alternating between
   taking N from the first, N from the second, etc.

   Examples:

   riffle 3 [1..10] [20..30]
   => [1,2,3,20,21,22,4,5,6,23,24,25,7,8,9,26,27,28,10,29,30]   

   riffle 5 "abracadabra" "supercalifragilistic"
   => "abracsuperadabrcalifaragilistic"
-}
riffle :: Int  -- N
       -> [a]  -- first list to riffle together
       -> [a]  -- second list to riffle together
       -> [a]
riffle n []   lst2 = lst2
riffle n lst1 []   = lst1
riffle n lst1 lst2 = ((take' n lst1) ++ (take' n lst2)) ++ (riffle n (drop' n lst1) (drop' n lst2))


{-
   Returns successive sublists of length N of the given list.

   Examples:

   ngrams 3 [1..10]
   => [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9],[8,9,10]]   

   ngrams 5 "abracadabra"
   => ["abrac","braca","racad","acada","cadab","adabr","dabra"]   
-}
ngrams :: Int  -- N
       -> [a]  -- input list
       -> [[a]]
ngrams n [] = []
ngrams n lst | n > length' lst = []
             | otherwise = [(take' n lst)] ++ (ngrams n (drop' 1 lst))


{-
   The autokey encryption cipher is similar to the Vigenere cipher, in that it
   uses successive characters of a key to encrypt a message. The key, however,
   is based on the message itself, prefixed by a single key character given as
   input to the encryption function.

   E.g., if the message to encrypt is "HELLO" and the key character is 'T', the
   encryption key is "THELLO", and encryption proceeds in the same way as in the
   Vigenere cipher:

               message:  H E L L O
                   key:  T H E L L
                         ---------
     encrypted message:  A L P W Z

   Your function should only encrypt letters, and all letters should be
   converted to uppercase before encryption.

   Examples:

   autokeyEncrypt 'A' "COMPUTER SCIENCE!"  -> t
   => "CQABJNXV JUKMRPG!"

   autokeyEncrypt 'G' "PARIS, FRANCE"  --> t
   => "VPRZA, XWRNPG"
-}
autokeyEncrypt :: Char     -- the key character
               -> String   -- the message to encrypt (plaintext)
               -> String   -- the encrypted message (ciphertext)
autokeyEncrypt key str = ake' key' (all_upper str) []
   where
      key' = (all_upper (key : ((join . removePunc . split_p) str)))

      ake' :: String -> String -> [Char] -> String
      ake' (k:ks) []     lst = lst
      ake' (k:ks) (x:xs) lst | is_punc x = ake' (k:ks) xs (lst ++ [x])
                             | otherwise = ake' ks xs (lst ++ [k'])
         where k' = chr $ 65 + mod (ord x + ord k) 26



-----------
is_punc :: Char -> Bool
is_punc c | (ord (toUpper c) - 65 >= 0) && (ord (toUpper c) - 65 <= 25) = False
          | otherwise = True

split_p :: [Char] -> [[Char]]
split_p input =  sp [[]] input
   where 
      sp :: [[Char]] -> [Char] -> [[Char]]
      sp xs   [] = xs
      sp [[]] (y:ys)            = sp [[y]] ys
      sp xs   (y:ys) | scenario_1 = sp first_list_1 ys
                     | scenario_2 = sp first_list_2 ys
         where

            scenario_1 = is_punc ((head . reverse) ((head . reverse) xs)) == (is_punc y)
            scenario_2 = is_punc ((head . reverse) ((head . reverse) xs)) /= (is_punc y)
            first_list_1 = ((reverse . drop 1 . reverse) xs) ++ [((head . reverse) xs) ++ [y]]
            first_list_2 = ((reverse . drop 1 . reverse) xs) ++ [((head . reverse) xs)] ++ [[y]]


removePunc xs = [ x | x <- xs, not ((is_punc . head') x) ]
------------

join :: [[Char]] -> [Char]
join lsts = j [] lsts
   where 
      j :: [Char] -> [[Char]] -> [Char]
      j []  (x:xs) = j x xs
      j lst []     = lst
      j lst  words = j (lst ++ (head' words)) (drop' 1 words)


------------------------------------------------------------





{-
   Decrypts an autokey-encrypted ciphertext, given the key character.

   Examples:

   autokeyDecrypt 'T' "ALPWZ"
   => "HELLO"

   autokeyDecrypt 'G' "VPRZA, YWRNPG"
   => "PARIS, FRANCE"
-}
autokeyDecrypt :: Char -> String -> String
autokeyDecrypt key str = ad (toUpper key) (all_upper str) []
   where
      ad :: Char -> String -> [Char] -> String
      ad k []     lst = lst
      ad k (x:xs) lst | is_punc x = ad k xs (lst ++ [x])
                      | otherwise = ad k' xs (lst ++ [k'])
         where k' = chr $ 65 + mod (ord x - ord k) 26


{-
   All the remaining functions deal with the game of Tic-Tac-Toe, where a board
   is represented as a list of lists of Ints, where each Int represents a move
   (or the absence of one). 1 and 0 are player moves, while -1 indicates that
   no move has been made at that position. Positions are specified as (row,col)
   tuples. 

   E.g., consider the Tic-Tac-Toe game in progress:

        O | X | O
       –––––––––––
          | X | 
       –––––––––––
        X |   |  
   
   We can represent this game with the list:

      [[0,1,0], [-1,1,-1], [1,-1,-1]]
-}


{-
   Returns an empty Tic-Tac-Toe board (i.e., without any moves).
-}
ttt_empty :: [[Int]]
ttt_empty = take' 3 (repeat [toInt (-1),toInt (-1),toInt (-1)])
   

{-
   Places a move for a player at the specified position and returns the
   resulting board.

   Examples:

   ttt_place ttt_empty 1 (2,1) => [[-1,-1,-1],[-1,-1,-1],[-1,1,-1]]
-}
ttt_place :: [[Int]]    -- starting board
          -> Int        -- player
          -> (Int,Int)  -- position
          -> [[Int]]
ttt_place board player (row,col) | row == 0 = [modify_row (board !! r) c player] ++ (drop' (r+1) board)
                                 | row == 1 = [board !! 0] ++ [modify_row (board !! 1) c player] ++ (drop' 2 board)
                                 | row == 2 = (all_but_last board) ++ [modify_row (board !! 2) c player]
      where 
         new_row = modify_row (board !! r) c player
         r = (toInt . toInteger) row
         c = (toInt . toInteger) col
         row_1 = board !! 0
         row_2 = board !! 1
         row_3 = board !! 2



modify_row :: [Int] -> Int -> Int -> [Int]
modify_row [r1,r2,r3] r player | r == 0 = [player, r2,    r3]
                               | r == 1 = [r1,    player, r3]
                               | r == 2 = [r1,    r2,    player]



{-
   Plays a sequence of moves, starting with the specified player and alternating
   with each subsequent move. Returns the board after all the moves.
   
   Examples:
 
[-1,-1,-1]
[-1,1,-1]
[-1,-1,-1]



   ttt_play ttt_empty 1 [(1,1),(0,0),(2,0),(0,2),(0,1)]
   => [[0,1,0],[-1,1,-1],[1,-1,-1]]
-}
ttt_play :: [[Int]]      -- starting board
         -> Int          -- starting player
         -> [(Int,Int)]  -- moves
         -> [[Int]]
ttt_play board player (m:moves) | length' (m:moves) == 0 = board
                                | length' (m:moves) == 1 = ttt_place board player new_m
                                | otherwise = ttt_play (ttt_place board player new_m)  (nxt player)  moves
                                         where 
                                               nxt :: Int -> Int
                                               nxt 1 = 0
                                               nxt 0 = 1

                                               new_m = ((toInt . toInteger) (fst m), (toInt . toInteger) (snd m))

{- 
   Determines whether the specified player has won on the board -- i.e., has a
   horizontal, vertical, or diagonal sequence of moves. Note that the board may
   not be "legal" (e.g., one player may have too many moves, or two players may
   have won).

   Examples:

   ttt_win [[1,1,1],[-1,-1,-1],[-1,-1,-1]] 1 => True

   ttt_win [[1,1,1],[-1,-1,-1],[-1,-1,-1]] 0 => False   
-}        
ttt_win :: [[Int]]  -- board
        -> Int      -- player
        -> Bool
ttt_win b p | diag || diag'   || 
             (b !! 0) == [p,p,p] || 
             (b !! 1) == [p,p,p] ||
             (b !! 2) == [p,p,p]   || 
             (top_left && center_left && bottom_left) ||
             (center_top && center && center_bottom) ||
             (top_right && center_right && bottom_right) = True
            | otherwise = False
         where 
            diag = ((b !! 0) !! 0 == p) && ((b !! 1) !! 1 == p) && ((b !! 2) !! 2 == p)
            diag' = ((b !! 0) !! 2 == p) && ((b !! 1) !! 1 == p) && ((b !! 2) !! 0 == p)
            top_left      = (b !! 0) !! 0 == p
            center_left   = (b !! 1) !! 0 == p
            bottom_left   = (b !! 2) !! 0 == p
            center_top    = (b !! 0) !! 1 == p
            center        = (b !! 1) !! 1 == p
            center_bottom = (b !! 2) !! 1 == p
            top_right     = (b !! 0) !! 2 == p
            center_right  = (b !! 1) !! 2 == p
            bottom_right  = (b !! 2) !! 2 == p





{-
   Returns a list of winning moves for the specified player on the board,
   assuming that the game has not yet been won by either player. A move is only
   possible if the other player hasn't moved there already. Again, note that the
   board may not be "legal".

   Examples:

   ttt_winningMoves [[0,-1,1],[-1,0,-1],[1,-1,1]] 1
   => [(1,2),(2,1)]

-}
ttt_winningMoves :: [[Int]] -> Int -> [(Int,Int)]
ttt_winningMoves board p = twm (fix_b board) p (w:wins) []
--where
fix_b:: [[Int]] -> [[Int]]
fix_b [[]] = [[]]
fix_b b =  [[f b (0,0), f b (0,1), f b (0,2)],
            [f b (1,0), f b (1,1), f b (1,2)],
            [f b (2,0), f b (2,1), f b (2,2)]]
   where
      f board (a,b) = fix (el_at board (a,b))

twm :: [[Int]] -> Int -> [[(Int, Int)]] -> [(Int,Int)] -> [(Int, Int)]
twm board p []       lst = lst
twm board p (w:wins) lst | path == [fix (-1),p,p] = twm board p wins (lst ++ [(fst (w !! 0), snd (w !! 0))])
                         | path == [p,fix (-1),p] = twm board p wins (lst ++ [(fst (w !! 1), snd (w !! 1))])
                         | path == [p,p,fix (-1)] = twm board p wins (lst ++ [(fst (w !! 2), snd (w !! 2))])
                         | otherwise = twm board p wins lst
   where

         path =  [el_at board (w !! (fix 0)), el_at board (w !! (fix 1)), el_at board (w !! (fix 2))]


www1 = [(fix 0, fix 2),(fix 1,fix 2),(fix 2,fix 2)]
www2 = [(fix 0, fix 1),(fix 1,fix 1),(fix 2,fix 1)]
www3 = [(fix 0, fix 2),(fix 1,fix 2),(fix 2,fix 2)]

bbb = [[fix 0,    fix (-1), fix 1],
       [fix (-1), fix 0,    fix (-1)],
       [fix 1,    fix (-1), fix 1]]

(w:wins) =   [[(fix 0, fix 0),(fix 1,fix 0),(fix 2,fix 0)], -- left column
              [(fix 0, fix 1),(fix 1,fix 1),(fix 2,fix 1)], -- middle column
              [(fix 0, fix 2),(fix 1,fix 2),(fix 2,fix 2)], -- right column
              [(fix 0, fix 0),(fix 0,fix 1),(fix 0,fix 2)], -- top row
              [(fix 1, fix 0),(fix 1,fix 1),(fix 1,fix 2)], -- top row
              [(fix 2, fix 0),(fix 2,fix 1),(fix 2,fix 2)], -- top row
              [(fix 0, fix 0),(fix 1,fix 1),(fix 2,fix 2)], -- diag1
              [(fix 0, fix 2),(fix 1,fix 1),(fix 2,fix 0)]] -- diag2
  


-----------------------------------------------------------

all_but_last :: [a] -> [a]
all_but_last lst = (reverse . (drop 1) . reverse) lst


all_upper :: [Char] -> [Char]
all_upper [] = []
all_upper (x:xs) | is_punc x = x : (all_upper xs)
                 | otherwise =  (toUpper x) : (all_upper xs)

all_lower :: [Char] -> [Char]
all_lower [] = []
all_lower (x:xs) = (toLower x) : (all_lower xs)

cycleN :: Int  -- N
       -> [a]  -- input list
       -> [a] 
cycleN 0 _ = []
cycleN 1 x = x
cycleN n x = (cycleN 1 x) ++ (cycleN (n-1) x)


drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n xs@(_:xs')
   | n > 0     = drop' (n-1) xs'
   | otherwise = xs

fix = (\x -> (toInt . toInteger) x)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v                        -- anything where second argument is empty list returns the first argument
foldr' f v (x:xs) = f x (foldr' f v xs) 


getLast' :: [a] -> a
getLast' [] = error "Empty List"
getLast' lst = (head' . reverse') lst

head' :: [a] -> a
head' [] = error "Empty List"
head' (x:xs) = x

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' lst =  reverse'' lst []
  where
    reverse'' []     a = a
    reverse'' (x:xs) a = reverse'' xs (x:a)


take' :: Int -> [a] -> [a]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs

toInt :: Integer -> Int
toInt n = fromIntegral n

el_at :: [[a]] -> (Int, Int) -> a
el_at lst (x,y) = (lst !! x) !! y
