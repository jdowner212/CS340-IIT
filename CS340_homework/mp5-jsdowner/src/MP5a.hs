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
import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO,run,assert)
--import Control.Exception(finally,bracket)


--[(6,0),(4,2),(3,1),(4,0),(1,1),(2,0),(2,2),(0,0),(0,2),(5,1)]

--allNodes <$> (aStarSolveGraph (Graph (randomPairList 7 (4,7) (w*h `mod` 11)) 9 [] empty pairs d))




-- aStarSolveGraph (Graph nodes' num_p [] cmap' pairs d)
zeroPaths = 0
emptyAdjMap = empty
emptyPath = []
nodes' = randomPairList num_n (w,h) (w*h `mod` 11)
g = Graph nodes' zeroPaths [] emptyAdjMap pairs d


--nnn' = randomPairList 5 (6,6) (6*6 `mod` 11)
--ps' = createPairings 9 nnn'
dms = ()




---ddd = fromList [((1,2),([(3,3),(4,4)])),((3,2),([(5,3),(6,4)])),((12,2),([(3,9),(4,7)]))]
--ccc = fst <$> [((1,2),([(3,3),(4,4)])),((3,2),([(5,3),(6,4)])),((12,2),([(3,9),(4,7)]))]
n_paths = 20
w = 7
h = 7
num_n = 10
n_paths' = 20
pairs = createPairings n_paths' nodes'
pairsDict = pToD (pairs, nodes')
sums = [a + b | (a,b) <- nodes']
sorted = sortOn snd (zip nodes' sums)
y = fst (head pairs)
d = pToD (pairs, nodes')


-- adjLocs [(4,3),(0,1),(2,1),(3,2),(1,2)] (0,1) [((4,3),[(2,1),(1,2),(0,1)]),((0,1),[(2,1),(1,2)]),((0,1),[(4,3)]),((2,1),[(3,2),(1,2)]),((2,1),[(0,1),(4,3)]),((3,2),[(2,1)]),((1,2),[(4,3),(0,1),(2,1)])]


-- Graph: nodes n_paths path_list cmap neigh_list neigh_dict



type Seed = Int
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
                                                     start = getStartGoal g 's'
                                                     gen :: Graph -> GraphLoc -> [GraphLoc] -> State StdGen Graph
                                                     gen g currLoc adjLocList =  do
                                                                                    nLocs <- getShuffled adjLocList
                                                                                    foldM (\g'@(Graph _ _ _ cmap _ _) nLoc ->
                                                                                            if nLoc `member` cmap
                                                                                            then return g' 
                                                                                            else gen (connectNodes currLoc nLoc g') nLoc adjLocList)
                                                                                          g nLocs 
                                                 in  gen g start (concat [snd items | items <- pairsDict, fst items == start])


instance Show Graph where
  show = printGraph 

printGraph :: Graph -> String
printGraph Empty = ""
printGraph g@(Graph allnodes _ _ _ _ neighDict) =  let
--printGraph g@(Graph allnodes _ path cmap neigh neighDict) =  let  graph
                                                                neigh = createPairings (graphNumPaths g) allnodes
                                                                cmap = graphAdjMap g
                                                                (v:vs) | cmap == empty = []
                                                                       | otherwise = keys cmap
                                                                costs | cmap == empty = []
                                                                      | otherwise =  [aStarCost ns (v:vs) | ns <- allnodes]
                                                                total_c = sum costs
                                                                d = pToD (neigh, allnodes)
                                                                goal = getStartGoal g 'g'
                                                                start = getStartGoal g 's'
                                                                path' | cmap == empty = [start]
                                                                      | otherwise = (v:vs)
                                                                printD :: NeighborDict -> String
                                                                printD [] = ""
                                                                printD (d:ds) = (show d ++ "\n" ++ (printD ds))
                                                              -- in ""
                                                              in "\n\nAll Nodes: " ++ (show allnodes) ++ "\n\nNeighbors: \n" ++ (printD d) ++ 
                                                                "\nStart: " ++ (show (getStartGoal g 's')) ++ "\nGoal: " ++ (show goal) ++ 
                                                                "\nPath: " ++ (show (path' ++ [goal])) ++  "\nCost: " ++ (show total_c) ++ "\n\n"






fromJustGraph :: Maybe Graph -> Graph
fromJustGraph Nothing = Empty
fromJustGraph m = fromJust m


--pToD :: [(GraphLoc, GraphLoc)] -> AllNodes -> [NeighDict]
pToD :: ([(GraphLoc, GraphLoc)], AllNodes) -> NeighborDict
pToD (_, []) = []
pToD (pairs, (n:nodes)) = let y = [snd pair | pair <- pairs, n == fst pair]
                              y' | y == [] = []
                                 | otherwise = zip [n] [y]
                              z = [fst pair | pair <- pairs, n == snd pair]
                              z' | z == [] = []
                                 | otherwise = zip [n] [z]
                              ---
                              raw = (y' ++ z' ++ (pToD (pairs, nodes)))
                              (k:keys) = removeDuplicates (fst <$> raw)
                              c [] _ = []
                              c _ [] = []
                              c (k':keys') dict' = let lists = snd <$> dict'
                                                      -- k_lists = [snd items | items <- dict' , fst items == k']
                                                       k_lists = concat $ snd <$> (\key_ -> [d_ | d_ <- dict', fst d_  == key_]) k'
                                                       zipped = [(k', k_lists)]
                                                   in  (zipped ++ (c keys' dict'))
                          in (c (k:keys) raw)




--original:
randomGraph :: Int -> GraphNumPaths -> GraphDims -> Seed -> IO Graph
randomGraph num_nodes n_paths dims seed = let nodes' = randomPairList num_nodes dims (seed*num_nodes `mod` 7)
                                              ps = createPairings n_paths nodes'
                                              d = pToD (ps, nodes')
                                          in  evalState (genGraph nodes' n_paths dims ps d) <$>  newStdGen


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
                in -- debug n $ 
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

-- dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
-- dfs goal succ start = search goal succ (++) [start] []

bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = sortOn cost (new ++ old)




nextPaths :: Graph -> [Graph]
nextPaths Empty = []
nextPaths g@(Graph _ dims p@(loc:_) cmap _ _) = do
  adj <- filter (not . flip elem p) $ findWithDefault [] loc cmap
  return $ g { graphPath = adj:p }


solveGraph' :: Ord a => (Graph -> a) -> Graph -> Maybe Graph
solveGraph' cost g@(Graph ns p _ _ _ sol_) =  
    let (w,h) = getStartGoal g 'g'
        start = getStartGoal g 's'
    in bestFirstSearch ((==(w,h)). head . graphPath) 
                       nextPaths
                       cost
                       (g { graphPath = [start]})


solveRandomGraph :: GraphNumPaths -> GraphNumPaths ->  GraphDims -> Seed -> (Graph -> Maybe Graph) -> IO Graph
solveRandomGraph num_n num_p dims seed solver= do g <- randomGraph num_n num_p dims seed
                                                  return $ fromMaybe Empty $ solver g
                                   

getStartGoal :: Graph -> Char -> GraphLoc
getStartGoal Empty _ = (0,0)
getStartGoal g@(Graph ns _ _ _ _ _) c = let sums = [a + b | (a,b) <- ns]
                                            sorted = sortOn snd (zip ns sums)
                                            lst | c == 'g' = reverse sorted
                                                | c == 's' = sorted
                                        in  fst (head lst)


-- bfsSolveGraph' :: Graph -> Maybe Graph
-- bfsSolveGraph' g@(Graph  _ _ _ _ _ _) = solveGraph' cost g
--   where
--         cost g'@(Graph _ _ p@((x,y):_) _ _ _) = let 
--                                                     (w,h) = getStartGoal g' 'g'
--                                                 in  abs (w-x) + abs (y-h)


aStarSolveGraph :: Graph -> Maybe Graph
aStarSolveGraph Empty = Nothing
aStarSolveGraph g@(Graph ns _ p _ _ _) = solveGraph' cost g
  where 
        cost g'@(Graph ns' _ path _ _ _) = aStarCost (getStartGoal g' 'g') path

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
                     (v1, s1) = randomRange max (((s0+2)*101) `mod` 11) -- (23*s0 `mod` 19)
                     (v2, _)  = randomRange max v1 -- ((71+ (abs s1)) `mod` 23) -- max (41*s1 `mod` 29)
                 in v2

randomRange :: Int -> Seed -> (Int, Seed)
randomRange mx seed | mx<1 = (0, ((seed*101)+17) `mod` 13)
                    | otherwise =  (((111*seed) + 17) `mod` mx, 55*seed `mod` 7)

-- returns list of locations
randomPairList :: GraphNumPaths -> GraphLoc -> Seed -> AllNodes
randomPairList 0 _ _ = []
randomPairList n (w,h) s0 = let (p1,p2) = randomPair (w,h) s0
                                s = [a + 10 | a <- [1..100], isPrime a]
                                more = [randomPair (w,h) s | s <- [1..100]]
                                more' = removeDuplicates more
                                more1 = [(b,a) | (a,b) <- more, (a + b) `mod` 2 == 0, a < (h-4)]
                                more1b = [(b,a) | (a,b) <- more, (a + b) `mod` 2 == 0, a >= (h-4)]
                                more2 = [(b,a) | (a,b) <- more, (a + b) `mod` 2 == 1 , b < (w-4)]
                                more2b = [(b,a) | (a,b) <- more, (a + b) `mod` 2 == 1 , b >= (w-4)]
                                all = (removeDuplicates (more1 ++ more2 ++ more1b ++ more2b))
                            in  (take n all)
  


randomPair :: GraphLoc -> Seed -> GraphLoc
randomPair (w,h) s = let v1 = oneRand w (s*711 `div` 31)
                         v2 = oneRand h (s*1011 `div` 47)
                     in  (v1, v2)


divisors x = 1:[ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]
isPrime x = divisors x == [1,x]


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

nRandElems :: Eq a => Int -> [a] -> [a]
nRandElems 0 _ = []
nRandElems _ [] = []
nRandElems n ls = let len = length ls
                      i = oneRand (len-1) (13*n `mod` 11)--oneRand ((length ls) - 1) (13 * (length ls) `mod` 7)
                      newItem = ls !! i
                      newList = dropFromList newItem ls
                  in ([newItem]) ++ (nRandElems (n-1) newList)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (l:ls) | contains l ls = removeDuplicates ls
                        | otherwise = [l] ++ (removeDuplicates ls)



dropFromList :: Eq a => a -> [a] -> [a]
dropFromList _ [] = []
dropFromList item (x:xs) | item == x = dropFromList item xs
                         | otherwise = [x] ++ dropFromList item xs

contains :: Eq a => a -> [a] -> Bool
contains element [] = False
contains element (l:ls) | element == l = True
                        | otherwise = contains element ls

getShuffled :: [a] -> State StdGen [a]
getShuffled l = do gen <- get
                   let (g1, g2) = split gen
                       l' = shuffle' l (length l) g1
                   put g2
                   return l'




