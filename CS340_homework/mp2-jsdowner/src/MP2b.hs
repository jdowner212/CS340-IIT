module MP2b where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

---------------------------------------------------
-------------------- Make World -------------------
---------------------------------------------------



-- Makes a barren world for the Game of Life. A world is a 2-tuple of:
--  1. its width (W) and height (H), as a tuple of integers
--  2. a list of H lists of W Bools, each value representing a cell's state
--     (True for living, False for dead)

makeWorld :: (Int,Int)  -- (width,height) of world
          -> ((Int,Int), [[Bool]])  -- world
makeWorld dims@(w,h) = (dims, replicate h $ replicate w False)



---------------------------------------------------
------------------ Live Neighbors -----------------
---------------------------------------------------



-- Computes the number of living neighbors of a cell.
liveNeighbors :: ((Int,Int), [[Bool]])  -- world
              -> (Int,Int)              -- cell
              -> Int                    -- num of living neighbors
liveNeighbors world (w',h') = countTrues (nbrToBool  []  cell_bools  nbrCoords)  0
    where
        dims        = fst world
        cell_bools  = snd world
        nbrCoords   = neigh dims (w',h')
        ----------------------
        -- given list of coordinate pairs, determines if cells are alive or dead
        nbrToBool :: [Bool] -> [[Bool]] -> [(Int, Int)] -> [Bool]
        nbrToBool  asBools  ogBools  []   = asBools
        nbrToBool  asBools  ogBools  nbrs = nbrToBool  (asBools ++ [aliveTF])  ogBools  (drop' 1 nbrs) 
            where
                aliveTF =  el_at ogBools x y
                x = (fst . head) nbrs
                y = (snd . head) nbrs
        ----------------------
        countTrues :: [Bool] -> Int -> Int
        countTrues  []     n  = n
        countTrues  cells  n  | head cells = countTrues (drop' 1 cells) (n + 1)
                              | otherwise  = countTrues (drop' 1 cells) n
        ----------------------
        neigh :: (Int,Int) -> (Int, Int) -> [(Int,Int)]
        neigh (w,h) (w',h') | ((w'==0)   && (h'==0))                           = [(w',h'+1),(w'+1,h'),(w'+1,h'+1)]
                            | ((w'==0)   && (h'==h-1))                         = [(w',h'-1),(w'+1,h'),(w'+1,h'-1)]
                            | ((w'==w-1) && (h'==0))                           = [(w',h'+1),(w'-1,h'),(w'-1,h'+1)]
                            | ((w'==w-1) && (h'==h-1))                         = [(w',h'-1),(w'-1,h'),(w'-1,h'-1)]
                            | ((w'==w-1) && (h'>0)   && (h'<h-1))              = [(w',h'-1),(w',h'+1),(w'-1,h'),(w'-1,h'-1),(w'-1,h'+1)]
                            | ((w'==0)   && (h'>0)   && (h'<h-1))              = [(w',h'-1),(w',h'+1),(w'+1,h'),(w'+1,h'-1),(w'+1,h'+1)]
                            | ((w'>0)    && (w'<w-1) && (h'==0))               = [(w'-1,h'),(w'+1,h'),(w',h'+1),(w'-1,h'+1),(w'+1,h'+1)]
                            | ((w'>0)    && (w'<w-1) && (h'==h-1))             = [(w',h'-1),(w'-1,h'),(w'-1,h'-1),(w'+1,h'),(w'+1,h'-1)]
                            | ((w'>0)    && (w'<w-1) && (h'>0) && (h'<h-1))    = [(w',h'-1),(w',h'+1),(w'-1,h'),(w'-1,h'-1),(w'-1,h'+1),(w'+1,h'),(w'+1,h'-1),(w'+1,h'+1)]
                            | otherwise                                        = []




------------------------------------------------
----------------- Next World -------------------
------------------------------------------------



-- Computes the next world state according to Conrad's rules:
--  1. Birth: a dead cell with exactly three living neighbors comes alive. 
--  2. Survival: a living cell with two to three neighbors remains alive. 
--  3. Death: a living cell with zero or one neighbors dies in isolation; 
--            a living cell with four or more neighbors dies of overcrowding


------
nextWorld :: ((Int,Int), [[Bool]]) -> ((Int,Int), [[Bool]])
nextWorld world = (fst world, ntB_cols (nn_grid world))
    where
        --------------
        --ntB_cols :: [[Int]] -> [[Bool]]
        ntB_cols [] = []
        ntB_cols (g:grid) = [(ntB_rows g)] ++ (ntB_cols grid)
        --------------
        --ntB_rows :: [Int] -> [Bool]
        ntB_rows [] = []
        ntB_rows (x:xs) = (next_state x) : (ntB_rows xs)
            where
                next_state x | (x == 2 || x == 3) = True
                            | otherwise          = False
        --------------
        -- get number of neighbors represented as grid
        --nn_grid :: ((Int,Int), [[Bool]]) -> [[Int]]
        nn_grid world = btN_col [] world ((fst . fst) world - 1, (snd . fst) world - 1)
        --------------
        --btN_col :: [Int] -> ((Int,Int), [[Bool]]) -> (Int, Int) -> [[Int]]
        btN_col lst world (0,y) = [(btN_rows lst world (0,y))]
        btN_col lst world (x,y) = (btN_col lst world (x-1, y)) ++ [(btN_rows lst world (x,y))]
        ------------
        --btN_rows :: [Int] -> ((Int,Int), [[Bool]]) -> (Int, Int) -> [Int]
        btN_rows lst world (x,(-1)) = lst
        btN_rows lst world (x,y) = btN_rows ((liveNeighbors world (x,y)) : lst) world (x,y-1)



---------------------------------------------------
------------------ Draw Picture -------------------
---------------------------------------------------


drawWorld :: ((Int,Int), [[Bool]])  -- world
          -> Picture
drawWorld world = 

        let w  = 500  -- screen width
            h  = 500  -- screen height
            n  = (toFloat . snd . fst) world       -- number of squares
            sq = color black (rectangleSolid 1 1)  -- unit square
        in scale (w/n) (h/n) $
            translate (-n/2 + 0.5) (-n/2 + 0.5) $
            pictures [translate i j sq | (i,j) <- (tuple_itf . live_coords) world]

            where
                -- converts list of tuple pairs to floats because gloss is picky
                --tuple_itf :: [(Int, Int)] -> [(Float, Float)]
                tuple_itf [] = []
                tuple_itf (x:xs) = ((toFloat . fst) x, (toFloat . snd) x) : (tuple_itf xs)
                ----------------------------------
                -- returns positions of live cells
                --live_coords :: ((Int,Int), [[Bool]]) -> [(Int,Int)]
                live_coords world = lc'
                                        []
                                        (snd world)              -- booleans
                                        (all_coords (fst world)) -- all possible locations within world's dimensions
                                        (fst world)              -- dimensions

                    where
                        lc' :: [(Int,Int)] -> [[Bool]]-> [(Int, Int)] -> (Int, Int) -> [(Int,Int)]
                        lc' new bools []     (w,h)  = new
                        lc' new bools (i:is) (w,h)  | el_at bools a b = lc' (new ++ [i]) bools is  (w,h)
                                                    | otherwise       = lc'  new         bools is  (w,h)
                                    where 
                                        a = fst i
                                        b = snd i

                        -- all possible location within given dimensions
                        all_coords :: (Int, Int) -> [(Int,Int)]
                        all_coords (w,h) = [(x,y) | x <- [0..w-1], y <- [0..h-1]]


---------------------------------------------------
------------------ Handle Events ------------------
---------------------------------------------------

handleEvents :: Event  -- event information
             -> ((Int,Int), [[Bool]])  -- world
             -> ((Int,Int), [[Bool]])
handleEvents (EventKey (MouseButton LeftButton) Up _ (mx,my))   world@((w,h), cells)    =   world_after_click world mx my -- mx and my get converted in world_after_click
handleEvents  _                                                 world                   =   world



world_after_click :: ((Int,Int), [[Bool]]) -> Float -> Float -> ((Int,Int), [[Bool]])
world_after_click world mx my = ((fst world), (new_bools  (snd world)
                                              (toInt (shift' mx))
                                              (toInt (shift' my))))
    where
        ------------------------
        shift' :: Float -> Float
        shift' click = (click + 250) * (n / 500)
            where n = ((toFloat . fst . fst) world)
        --------------------
        new_bools :: [[Bool]] -> Int -> Int -> [[Bool]]
        new_bools old_bools x y | x > (length' old_bools) - 1 =  old_bools
                                | otherwise                   =  (take' x old_bools) ++ new_true ++ (drop' (x + 1) old_bools)
            where
                new_true = [t_at_index sublist y]
                sublist  = head (drop x (old_bools))
                --------------------
                -- given index, replaces boolean in list with True
                t_at_index :: [Bool] -> Int -> [Bool]
                t_at_index lst y | y <= (length' lst) - 1     = (take' y lst) ++ [True] ++ (drop' (y + 1) lst)
                                 | otherwise                  = lst


--------------------------------------------------
------------ general helper functions ------------
--------------------------------------------------

toInt :: Float -> Int
toInt x = round x

toFloat :: Int -> Float
toFloat x = (fromIntegral x :: Float)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n xs@(_:xs')
   | n > 0     = drop' (n-1) xs'
   | otherwise = xs

take' :: Int -> [a] -> [a]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = (+1) (length' xs)

el_at :: [[a]] -> Int -> Int -> a
el_at lst x y = (head . drop (y-1)) ((head . drop (x-1)) lst)






-- for testing purposes

wrld = ((toInt 5,toInt 5), [[False, True,  True,  False, True],
                            [True,  True,  False, True,  True],
                            [False, False, True,  True, False],
                            [True,  True, True,  False, True],
                            [True,  True,  False, True,  True]])



