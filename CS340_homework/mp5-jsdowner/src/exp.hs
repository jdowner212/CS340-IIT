module MP5a where

import Data.Maybe
import Data.Ord
import Data.List
import Data.List
--------
import Data.List.Split (chunksOf)
--------
import Data.Tree
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO


-- cd /Users/user/Desktop/CS340/mp5-jsdowner/src  
-- stack ghci
-- :set prompt "ghci> "
-- :l exp

type Seed = Int

------------------------------------------------------
------------------------------------------------------
type GraphDims = (Int, Int)
type GraphNumPaths = Int
type AllNodes = [(Int,Int)]
type GraphLoc = (Int, Int) -- (x,y); (1,1) @ top-left
type GraphPath = [GraphLoc] -- path through the graph
type AdjMap = Map GraphLoc [GraphLoc]
type GraphNeighbors = [(GraphLoc, GraphLoc)]
type NeighborDict = [(GraphLoc, [GraphLoc])]
data Graph = Empty | Graph {
              allNodes :: AllNodes,
              graphNumPaths :: GraphNumPaths,
              graphPath :: GraphPath,
              graphAdjMap :: Map GraphLoc [GraphLoc],
              graphNeighbors :: GraphNeighbors,
              graphNeighDict :: NeighborDict
            } deriving (Eq)


-- takes info about graph + current location and returns list of neighbors to that location
adjLocs :: [GraphLoc] -> GraphLoc -> NeighborDict -> [GraphLoc]
adjLocs [] _ _ = []
adjLocs _ _ [] = []
adjLocs nodes' (x, y) dct = concat [snd items | items <- dct, fst items == (x,y)]

-- i.e., open wall
connectNodes :: GraphLoc -> GraphLoc -> Graph -> Graph
connectNodes l1 l2 g@(Graph _ _ _ cmap _ _) =
  g { graphAdjMap = insertWith (++) l2 [l1] $ insertWith (++) l1 [l2] cmap }


genGraph :: AllNodes -> GraphNumPaths -> GraphDims -> GraphNeighbors -> NeighborDict -> State StdGen Graph
genGraph allNodes n_paths dims pairs pairsDict = let g = (Graph allNodes n_paths [] empty pairs pairsDict)
                                                     start = getStart g
                                                 in  gen g start (concat [snd items | items <- pairsDict, fst items == start]) -- (start, allNodes, n_paths, pairsDict)

-- gen :: Graph -> (GraphLoc, AllNodes, GraphNumPaths, NeighborDict) -> State StdGen Graph
-- gen g (currLoc, nodes, n_paths, dict) = do
--               nLocs <- getShuffled $ adjLocs nodes currLoc dict
--               foldM (\g'@(Graph _ _ _ cmap _ _) nLoc ->
--                         if nLoc `member` cmap
--                         then return g' 
--                         else gen (connectNodes currLoc nLoc g') (nLoc, nodes ,n_paths, dict))
--                     g nLocs
-- num_p = graphNumPaths g

gen :: Graph -> GraphLoc -> [GraphLoc] -> State StdGen Graph
gen g currLoc adjLocList =  do
                              nLocs <- getShuffled adjLocList -- (concat [snd items | items <- dict, fst items == currLoc])   -- [getStart g, getGoal g] -- adjLocs (allNodes g) currLoc (graphNeighDict g)
                              foldM (\g'@(Graph _ _ _ cmap _ _) nLoc ->
                                      if nLoc `member` cmap
                                      then return g' 
                                      else gen (connectNodes currLoc nLoc g') nLoc adjLocList)
                                    g nLocs -- dict



-- currLoc' = s
-- do
-- foldM (g'@  (Graph nodes' num_p [] cmap' ppp d) nLoc ->
--         if nLoc `member` cmap'
--         then return g' 
--         else gen (connectNodes currLoc' nLoc g') (nLoc, nodes', GraphNumPaths g', d))
--       g' nLocs


-- graphnum
num_p = graphNumPaths g

s = getStart g
g = Graph nodes' num_p [] cmap' ppp d
cmap' = empty
instance Show Graph where
  show = printGraph 

printGraph :: Graph -> String
printGraph Empty = ""
printGraph g@(Graph allnodes _ path cmap neigh neighDict) =  let  
                                                                (v:vs) | cmap == empty = []
                                                                       | otherwise = keys cmap
                                                                costs | cmap == empty = []
                                                                      | otherwise =  [aStarCost ns (v:vs) | ns <- allnodes]
                                                                total_c = sum costs
                                                                d = consolidate (pToD (neigh, allnodes))
                                                                goal = getGoal g
                                                                start = getStart g
                                                                path' | cmap == empty = [start]
                                                                      | otherwise = (v:vs)

                                                              in "\n\nAll Nodes: " ++ (show allnodes) ++ "\n\nNeighbors: \n" ++ (printD d) ++ 
                                                                "\nStart: " ++ (show (getStart g)) ++ "\nGoal: " ++ (show goal) ++ 
                                                                "\nPath: " ++ (show (path' ++ [goal])) ++  "\nCost: " ++ (show total_c) ++ "\n\n"

fromJustGraph :: Maybe Graph -> Graph
fromJustGraph Nothing = Empty
fromJustGraph m = fromJust m

printD :: NeighborDict -> String
printD [] = ""
printD (d:ds) = (show d ++ "\n" ++ (printD ds))


input = "graph nodes nump path map neigh dict"

ddd = fromList [((1,2),([(3,3),(4,4)])),((3,2),([(5,3),(6,4)])),((12,2),([(3,9),(4,7)]))]
ccc = fst <$> [((1,2),([(3,3),(4,4)])),((3,2),([(5,3),(6,4)])),((12,2),([(3,9),(4,7)]))]
n_paths = 20
w = 7
h = 7
num_n = 10
n_paths' = 20
nodes' = randomPairList num_n (w,h) (w*h `mod` 11)
pairs = createPairings n_paths' nodes'
pairsDict = consolidate (pToD (pairs, nodes'))
sums = [a + b | (a,b) <- nodes']
sorted = sortOn snd (zip nodes' sums)
y = fst (head pairs)

--pToD :: [(GraphLoc, GraphLoc)] -> AllNodes -> [NeighDict]
pToD :: ([(GraphLoc, GraphLoc)], AllNodes) -> NeighborDict
pToD (_, []) = []
pToD (pairs, (n:nodes)) = let y = [snd pair | pair <- pairs, n == fst pair]
                              y' | y == [] = []
                                 | otherwise = zip [n] [y]
                              z = [fst pair | pair <- pairs, n == snd pair]
                              z' | z == [] = []
                                 | otherwise = zip [n] [z]
                          in (y' ++ z' ++ (pToD (pairs, nodes)))
                          
  
consolidate :: NeighborDict -> NeighborDict
consolidate [] = []
consolidate dict = let (k:keys) = removeDuplicates (fst <$> dict)
                       c [] _ = []
                       c _ [] = []
                       c (k':keys') dict' = let lists = snd <$> dict'
                                                k_lists = [snd items | items <- dict' , fst items == k']
                                                zipped = zip [k'] k_lists
                                            in  (zipped ++ (c keys' dict'))
                   in (c (k:keys) dict)

-- pairsFinal :: ([(GraphLoc, GraphLoc)], AllNodes) -> NeighborDict
-- pairsFinal ([], _) = []
-- pairsFinal (_, []) = []
-- pairsFinal (pairs, nodes) = let raw = pToD (pairs, nodes)
--                                 keys = removeDuplicates (fst <$> raw)
--                             in consolidate raw

display_loc :: [GraphLoc] -> String
display_loc = flip shows ""

-- original:
randomGraph :: Int -> GraphNumPaths -> GraphDims -> Seed -> IO Graph
randomGraph num_nodes n_paths dims seed = let nodes' = randomPairList num_nodes dims (seed*num_nodes `mod` 7)
                                              ps = createPairings n_paths nodes'
                                              d = consolidate (pToD (ps, nodes'))
                                          in evalState (genGraph nodes' n_paths dims ps d) <$>  newStdGen


ppp = createPairings 20 nodes'
d = consolidate (pToD (ppp, nodes'))

--------------------------------
--------------------------------

-- generalized, higher-order search
search :: (Eq a, Show a) => 
          (a -> Bool) 
          -> (a -> [a]) 
          -> ([a] -> [a] -> [a])
          -> [a] -> [a] 
          -> Maybe a
search goal adj comb unvisited visited
  | null unvisited = Just (head (reverse visited))
  | goal (head unvisited) = Just (head unvisited)
  | otherwise = let (n:ns) = unvisited
                in debug n $ 
                   search goal adj comb
                          (comb (removeDups (adj n)) ns)
                          (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))

debug :: Show a => a -> b -> b                                  -- Prints value of node currently being explored
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y

dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = search goal succ (++) [start] []

bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = sortOn cost (new ++ old)



output a b = seq (unsafePerformIO (print a))  b

-- (output "test" 23) * 25




nextPaths :: Graph -> [Graph]
nextPaths g@(Graph _ dims p@(loc:_) cmap _ _) = do
  adj <- filter (not . flip elem p) $ findWithDefault [] loc cmap
  return $ g { graphPath = adj:p }


-- solveGraph :: Graph -> Maybe Graph
-- solveGraph g@(Graph ns p _ _ _ _) =
--   let
--       goal = getGoal g
--       start = getStart g
--   in  dfs ((== goal) . head . graphPath)
--           nextPaths
--           (g { graphPath = [start]})



solveGraph' :: Ord a => (Graph -> a) -> Graph -> Maybe Graph
solveGraph' cost g@(Graph ns p _ _ _ sol_) =  
    let (w,h) = getGoal g
        start = getStart g
    in bestFirstSearch ((==(w,h)). head . graphPath) 
                       nextPaths
                       cost
                       (g { graphPath = [start]})



solveRandomGraph :: GraphNumPaths -> GraphNumPaths ->  GraphDims -> Seed -> (Graph -> Maybe Graph) -> IO Graph
solveRandomGraph num_n num_p dims seed solver= do g <- randomGraph num_n num_p dims seed
                                                  return $ fromMaybe Empty $ solver g
                                   

getGoal :: Graph -> GraphLoc
getGoal g@(Graph ns _ _ _ _ _) = let sums = [a + b | (a,b) <- ns]
                                     sorted = sortOn snd (zip ns sums)
                                 in  fst (head (reverse sorted))

getStart :: Graph -> GraphLoc
getStart g@(Graph ns _ _ _ _ _)= let sums = [a + b | (a,b) <- ns]
                                     sorted = sortOn snd (zip ns sums)
                                 in  fst (head sorted)


bfsSolveGraph' :: Graph -> Maybe Graph
bfsSolveGraph' g@(Graph  _ _ _ _ _ _) = solveGraph' cost g
  where
        cost g'@(Graph _ _ p@((x,y):_) _ _ _) = let 
                                                  (w,h) = getGoal g'
                                              in  abs (w-x) + abs (y-h)


aStarSolveGraph :: Graph -> Maybe Graph
aStarSolveGraph g@(Graph ns _ p _ _ _) = solveGraph' cost g
  where 
        cost g'@(Graph ns' _ path _ _ _) = aStarCost (getGoal g') path

aStarCost :: GraphLoc -> GraphPath -> Int
aStarCost (w,h) [] = 0
aStarCost (w,h) (v:vs) = let x = fst v
                             y = snd v
                             vis = (v:vs)
                             this_c = abs (w-x) + abs (h-y) + length vis
                          in this_c + aStarCost (w,h) vs

------------------------------------
------------------------------------
xxxx = 9

oneRand :: Int -> Seed -> Int
oneRand max s0 = let 
                     randomRange :: Int -> Seed -> (Int, Seed)
                     randomRange max seed = ((101+seed) `mod` max, 55*(seed+100) `div` 13)
                     (v1, s1) = randomRange max ((s0+100) - 91) -- (23*s0 `mod` 19)
                     (v2, _)  = randomRange max ((71+s1) `mod` 23) -- max (41*s1 `mod` 29)
                 in v2

-- returns list of locations
randomPairList :: GraphNumPaths -> GraphLoc -> Seed -> AllNodes
randomPairList 0 _ _ = []
randomPairList n (w,h) s0 = let (p1,p2) = randomPair (w,h) s0
                                s = [a + 10 | a <- [1..100], isPrime a]
                                more = [randomPair (w,h) s | s <- [1..100]]
                                more' = removeDuplicates more
                                more1 = [(b,a+3) | (a,b) <- more, (a + b) `mod` 2 == 0, a < (h-4)]
                                more1b = [(b,a-1) | (a,b) <- more, (a + b) `mod` 2 == 0, a >= (h-4)]
                                more2 = [(b+3,a) | (a,b) <- more, (a + b) `mod` 2 == 1 , b < (w-4)]
                                more2b = [(b-1,a) | (a,b) <- more, (a + b) `mod` 2 == 1 , b >= (w-4)]
                                all = (removeDuplicates (more1 ++ more2 ++ more1b ++ more2b))
                            in  take n all --([(p1,p2)] ++ randomPairList (n-1) w h s1)
  


randomPair :: GraphLoc -> Seed -> GraphLoc
randomPair (w,h) s = let v1 = oneRand w (s*711 `div` 31)
                         v2 = oneRand h (s*1011 `div` 47)
                     in  (v1, v2)


divisors x = 1:[ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]
isPrime x = divisors x == [1,x]

shuff' :: [a] -> IO [a]
shuff' [] = return []
shuff' lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuff' rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)

------------------------------------
------------------------------------

-- given a number and list of locations, 
-- returns random pairs of locations connected by paths
createPairings :: GraphNumPaths -> AllNodes -> GraphNeighbors
createPairings 0 _ = []
createPairings _ [] = []
createPairings numPaths nodes = let
                                    pFromP [] _ = [] -- pFromP :: Eq a => [a] -> [(Int,Int)] -> [(a,a)]
                                    pFromP _ [] = []
                                    pFromP nodes (i:is) = [(nodes !! (fst i), nodes !! (snd i))] ++ (pFromP nodes is)
                                    indices = [(a,b) | a <- [0 ..length nodes - 1], b <- [0 ..length nodes - 1], a < b]
                                    indices' = nRandElems numPaths indices
                                in (pFromP nodes indices')




indices = [(a,b) | a <- [0 .. (length nodes') - 1], b <- [0 .. (length nodes') - 1], a<b]
is = nRandElems 20 indices

-- pFromP nodes (i:is) = [(nodes !! (fst i), nodes !! (snd i))] ++ (pFromP nodes is)
-- takes list of paired locaitons and puts them in map list format


nRandElems :: Eq a => Int -> [a] -> [a]
nRandElems 0 _ = []
nRandElems _ [] = []
nRandElems n ls = let i = oneRand ((length ls) - 1) (13 * (length ls) `mod` 7)
                      newItem = ls !! i
                      newList = dropFromList newItem ls
                  in ([newItem] ++ (nRandElems (n-1) newList))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (l:ls) | contains l ls = removeDuplicates ls
                        | otherwise = [l] ++ (removeDuplicates ls)





dropFromList :: Eq a => a -> [a] -> [a]
dropFromList _ [] = []
dropFromList item (x:xs) | item == x = dropFromList item xs
                         | otherwise = [x] ++ dropFromList item xs

contains :: Eq a => a -> [a] -> Bool
contains elements [] = False
contains element (l:ls) | element == l = True
                        | otherwise = contains element ls

getShuffled :: [a] -> State StdGen [a]
getShuffled l = do gen <- get
                   let (g1, g2) = split gen
                       l' = shuffle' l (length l) g1
                   put g2
                   return l'




