module MP3a where

import Data.List
import Data.List.Split

{-
  Binary tree related type definitions.
-}
data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a) -- | Leaf a
  deriving (Eq, Show)

showTree :: Show a => BinTree a -> Int -> String
showTree (EmptyTree) _ = []
showTree (Node a t1 t2) n = replicate n '*' ++ show a ++ "\n" ++ showTree t1 (n+1) ++ showTree t2 (n+1)

data Direction = L | R


{-
  Creates a `BinTree` where all nodes contain the specified value.
-}
treeRepeat :: a -> BinTree a
treeRepeat a = Node a (treeRepeat a) (treeRepeat a)

{-
  Creates a `BinTree` where the nodes are populated with the natural numbers, 
  starting at the "root" of the tree and then downwards and from left to right 
  across each level of the tree.
-}
instance Eq Direction where
  L == L = True
  R == R = True
  _ == _ = False

-- (Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 3 (Node 6 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)))

treeNats :: BinTree Integer
treeNats = Node 1 (treeNats' 2) (treeNats' 3)
  where
    treeNats' :: Num a => a -> BinTree a
    treeNats' x = Node x (treeNats' (x*2)) (treeNats' (x*2 + 1))

{-
  Takes a list of `Direction` values (`L`eft or `R`ight) and traverses the tree 
  to return the value in the target node. 
  
  Examples:
  
  treeVal [L,R] treeNats => 5
  
  treeVal [] treeNats => 1  
-}
treeVal :: [Direction] -> BinTree a -> a
treeVal []     (Node x t_left t_right)             = x
treeVal (d:ds) (Node x t_left t_right) | d == L    = treeVal ds t_left
                                       | otherwise = treeVal ds t_right


{-
  Converts a tree to a list; the root of the tree is the first list value, and 
  the values in the tree are taken downwards and across each level. 
  
  Examples:

  take 10 $ treeToList treeNats
  => [1,2,3,4,5,6,7,8,9,10]
-}

treeToList :: BinTree a -> [a]
treeToList EmptyTree = []
treeToList t = treeToList' [t]
  where
    treeToList' :: [BinTree a] -> [a]
    treeToList' [] =  []
    treeToList' ts =  map get_node ts    ++   treeToList' (concat (map immediateChildren ts))

    get_node :: BinTree a -> a
    get_node (Node n _ _ ) = n

    immediateChildren :: BinTree a -> [BinTree a]
    immediateChildren (Node _  EmptyTree    EmptyTree) = []
    immediateChildren (Node _  EmptyTree    t2       ) = [t2]
    immediateChildren (Node _  t1           EmptyTree) = [t1]
    immediateChildren (Node _  t1           t2       ) = [t1, t2]


{-
  "Flips" the `BinTree` so that we obtain the mirror image of the original tree. 
  
  For instance, flipping the tree on the left gives us the one on the right:

             1                     1
           /   \                 /   \
          2     3      =>       3     2
         / \   / \             / \   / \
        4   5 6   7           7   6 5   4
-}
treeFlip :: BinTree a -> BinTree a
treeFlip EmptyTree      = EmptyTree
treeFlip (Node n t1 t2) = Node n (treeFlip t2) (treeFlip t1) 

{-
  Returns a `BinTree` based on an infinite list where the first item of the list
  is the root, and subsequent items from the list are assigned to nodes 
  downwards and across the levels of the tree.
  
  Examples:

  take 10 $ treeToList $ treeFromList [1..]
  => [1,2,3,4,5,6,7,8,9,10]
  
  Hint: check out your `treeNats` for inspiration!
-}

treeFromList :: [a] -> BinTree a
treeFromList [] = EmptyTree
treeFromList (n:ns) = treeFromList' n (n:ns) 0
  where 
    treeFromList' :: a -> [a] -> Int -> BinTree a
    treeFromList' thisNode [] _     = EmptyTree
    treeFromList' thisNode ns i | i' +1 > length ns = Node thisNode EmptyTree EmptyTree
                                | i'' +1> length ns = Node thisNode (treeFromList' (atIndexa i' ns) ns i')  EmptyTree
                                | otherwise         = Node thisNode (treeFromList' (atIndexa i' ns) ns i') (treeFromList' (atIndexa i'' ns) ns i'')
      where
        i' = (i*2)+1
        i'' = i' + 1


{-
  Takes a function and an initial value, and returns a `BinTree` where the root 
  value is the initial value, and values in subsequent nodes are based on 
  repeated applications of the given function to the value.
  
  Examples:

  treeVal [R,R,R] $ treeIterate (2*) 1
  => 16384

  take 15 $ treeToList $ treeFlip $ treeIterate (2*) 1
  => [1,4,2,64,32,16,8,16384,8192,4096,2048,1024,512,256,128]

  Hint: checkout `iterate`.
-}
treeIterate :: (a -> a) -> a -> BinTree a
treeIterate f val = treeIterate' f val 0
  where
    treeIterate' :: (a -> a) -> a -> Int -> BinTree a
    treeIterate' f val 0 = Node val (treeIterate' f val 1) (treeIterate' f val 2)
    treeIterate' f val n = Node (applyNTimes f val n) (Node (applyNTimes f val nl) tll tlr) (Node (applyNTimes f val nr) trl trr)
      where
          nl = n*2 + 1
          nr = n*2 + 2
          tll = treeIterate' f val (nl*2 + 1)
          tlr = treeIterate' f val (nl*2 + 2)
          trl = treeIterate' f val (nr*2 + 1)
          trr = treeIterate' f val (nr*2 + 2)


{-
  BinTree instance of the Functor class.
-}
instance Functor BinTree where
  fmap f EmptyTree      = EmptyTree
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)


{-
  BinTree instance of the Applicative class.  
-}
instance Applicative BinTree where
  pure a = treeRepeat a
  EmptyTree <*> _                 = EmptyTree
  _ <*> EmptyTree                 = EmptyTree
  Node f t1 t2 <*> Node v t1' t2' = Node (f v) (t1 <*> t1') (t2 <*> t2')


-------------------------------------------

instance Functor (State s) where
  fmap f st = State $ \s -> let (s', x) = runState st s
                            in (s', f x)

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  stf <*> stx = State $ \s -> let (s', f)  = runState stf s
                                  (s'', x) = runState stx s'
                              in (s'', f x)

instance Monad (State s) where
  st >>= f = State $ \s -> let (s', x) = runState st s
                           in runState (f x) s'


data State s a = State { runState :: s -> (s, a) }

---------------------------------

atIndexa :: Int -> [a] -> a
atIndexa a (x:xs) | a >= length (x:xs) = head (reverse xs)
                  | otherwise = head (drop a (x:xs))

applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes f val 0 = val
applyNTimes f val n = applyNTimes f (f val) (n-1)