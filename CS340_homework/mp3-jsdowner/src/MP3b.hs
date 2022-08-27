module MP3b where


{-
  Playing card definitions (feel free to add your own supporting types, so long 
  as you keep `Card`).
-}

data Suit = S | H | D | C deriving (Eq,Ord,Enum,Bounded,Show,Read)
data Value = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K deriving (Eq,Ord,Enum,Bounded,Show,Read)
data Card = Card Value Suit deriving Show

{-
  A full deck of 52 playing cards.
-}
deck :: [Card]
deck = [Card v s | s <- [S .. C], v <- [A .. K]]

{-
  Hand types. Don't change these.
-}
data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight
            | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Show, Ord)


{-
  Takes a list of 5 cards and returns the strongest hand they can be
  used to make. 

  Examples (note that your `Card` values may look different):

  hand [Card Two H, Card Three D, Card A H, Card Five D, Card Four S]
  => Straight

  hand [Card Two D, Card Three C, Card Two C, Card Three D, Card Two H]
  => FullHouse
-}
hand :: [Card] -> Hand
hand cs | isRoyalFlush cs     = RoyalFlush
        | isStraightFlush cs  = StraightFlush
        | isFourOfAKind cs    = FourOfAKind
        | isFlush cs          = Flush
        | isFullHouse cs      = FullHouse
        | isStraight cs       = Straight
        | isThreeOfAKind cs   = ThreeOfAKind
        | isTwoPair cs        = TwoPair
        | isPair cs           = Pair
        | otherwise           = HighCard

{-
  Takes a list of 5-`Card` lists, and returns a list of tuples of type 
  `(Int, Hand)`, where each tuple indicates the number of times a certain 
  `Hand` occurs in the input list. The tuples should be sorted in decreasing 
  order of frequency.
  
  See the machine problem write-up on how to test this function with the 
  generators defined for you below.
-}

computeStats :: [[Card]] -> [(Int, Hand)]
computeStats [] = []
computeStats cs = computeStats' (handsInList cs []) allHands []
  where
    allHands = [RoyalFlush, StraightFlush, FourOfAKind, Flush, FullHouse, Straight, ThreeOfAKind, TwoPair, Pair, HighCard]

    computeStats' :: [Hand] -> [Hand] -> [(Int, Hand)] -> [(Int, Hand)]
    computeStats' inList [] tups = tups
    computeStats' inList (a:all) tups | countOccurrences inList a == 0  = computeStats' inList all tups
                                      | otherwise = computeStats' inList all (reverse (quicksort (tups ++ [(count, a)])))
      where
        count = countOccurrences inList a

--------------------- Enumerating functions -----------------------


allDoubles :: Eq a => [a] -> [(a,a)]
allDoubles [] = []
allDoubles (v:vs)  = allDoubles' vs v []
  where
    allDoubles' :: Eq a => [a] -> a -> [(a,a)] -> [(a,a)]
    allDoubles' []     v' tups = tups
    allDoubles' (v:vs) v' tups = tups ++ [(v',list_vs) | list_vs <- (v:vs)] ++ allDoubles' vs v []

allPairs :: Eq a => [a] -> [(a,a)]
allPairs xs = allPairs' (allDoubles xs) []
  where
    allPairs' :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
    allPairs' [] pairs = pairs
    allPairs' (x:xs) pairs | fst x == snd x = allPairs' (dropValue xs x) (pairs ++ [x])
                           | otherwise = allPairs' xs pairs

allPerfectPairs :: Eq a => [a] -> [(a,a)]
allPerfectPairs xs = allPairs (under3 xs)
  where 
    under3 :: Eq a => [a] -> [a]
    under3 [] = []
    under3 (x:xs) | countOccurrences (x:xs) x > 2 = under3 (dropValue (x:xs) x)
                  | otherwise = [x] ++ under3 xs

allTriplets :: Eq a => [a] -> [(a,a,a)]
allTriplets (v:vs) = allTriplets' (v:vs) []
  where
    allTriplets' :: Eq a => [a] -> [(a,a,a)] -> [(a,a,a)]
    allTriplets' []     triplets = triplets
    allTriplets' (v:vs) triplets | count >= 3 = allTriplets' vs' (triplets ++ [(v,v,v)])
                                 | otherwise  = allTriplets' vs  triplets
      where
        vs' = (dropValue vs v) ++ add_back
        add_back = take (count - 3) (repeat v)
        count = countOccurrences (v:vs) v


--------------- Counting functions ---------------

countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences [] x = 0
countOccurrences lst x = countOccurrences' lst x 0
  where
    countOccurrences' :: Eq a => [a] -> a -> Int -> Int
    countOccurrences' [] x count = count
    countOccurrences' (v:vs) x count | x == v    = countOccurrences' vs x (count + 1)
                                     | otherwise = countOccurrences' vs x count

countSameSuits :: [Suit] -> [Int]
countSameSuits given = countSameSuits' given [S, H, D, C] []
  where
    countSameSuits' :: [Suit] -> [Suit] -> [Int] -> [Int]
    countSameSuits' given [] counts = counts
    countSameSuits' given (c:check) counts = countSameSuits' given check (counts ++ [countOccurrences given c])

countSameValues :: [Value] -> [Int]
countSameValues given = countSameValues' given [A .. K] []
  where
    countSameValues' :: [Value] -> [Value] -> [Int] -> [Int]
    countSameValues' given [] counts = counts
    countSameValues' given (c:check) counts = countSameValues' given check (counts ++ [countOccurrences given c])

--------------------- Misc. helper functions -----------------------

is' :: Hand -> ([Card] -> Bool)
is' RoyalFlush    =  isRoyalFlush
is' StraightFlush =  isStraightFlush
is' FourOfAKind   =  isFourOfAKind
is' Flush         =  isFlush
is' FullHouse     =  isFullHouse
is' Straight      =  isStraight
is' ThreeOfAKind  =  isThreeOfAKind
is' TwoPair       =  isTwoPair
is' Pair          =  isPair
is' HighCard      =  isHighCard

has :: Eq a => [a] -> a -> Bool
has [] input = False
has (x:xs) input | input == x = True
                 | otherwise = has xs input

suits :: [Card] -> [Suit]
suits [] = []
suits [Card v s] = [s]
suits (c:cs) = suits [c] ++ suits cs

values :: [Card] -> [Value]
values [] = []
values [Card v s] = [v]
values (c:cs) = values [c] ++ values cs

------------ Detecting various card hands --------------

isRoyalFlush :: [Card] -> Bool
isRoyalFlush cs = isStraightFlush cs == True && minimum' (values cs) == A && atIndexb 1 (quicksort (values cs)) == Ten

isStraightFlush :: [Card] -> Bool
isStraightFlush cs = isStraight cs && isFlush cs

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cs = has (countSameValues (values cs)) 5 || has (countSameValues (values cs)) 4

isFlush :: [Card] -> Bool
isFlush cs = has (countSameSuits (suits cs)) 5

isFullHouse :: [Card] -> Bool
isFullHouse cs = length ps == 1 && length ts == 1  && (fst $ head ps) /= (fst' $ head ts)
  where
    ps = allPerfectPairs (values cs)
    ts = allTriplets (values cs)

isStraight :: [Card] -> Bool
isStraight cs = isStraight' cs (values cs)
  where
    isStraight' :: [Card] -> [Value] -> Bool
    isStraight' cs vs | (minimum' vs == A) && (quicksort withoutA == [Ten .. K]) = True
                      | (quicksort vs == take 5 [(minimum' vs) .. ]) = True
                      | otherwise = False
      where
        withoutA = dropValue vs A

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cs = has (countSameValues (values cs)) 5 || has (countSameValues (values cs)) 4 || has (countSameValues (values cs)) 3

isTwoPair :: [Card] -> Bool
isTwoPair cs = countPairs cs == 2 
  where
    countPairs :: [Card] -> Int
    countPairs cs = length (allPairs (values cs))

isPair :: [Card] -> Bool
isPair cs = length (allPairs (values cs)) == 1

isHighCard :: [Card] -> Bool
isHighCard cs | (isRoyalFlush cs
                || isStraightFlush cs
                || isFourOfAKind cs
                || isFullHouse cs
                || isFlush cs
                || isStraight cs
                || isThreeOfAKind cs
                || isTwoPair cs
                || isPair cs == True) = False
              | otherwise = True

handsInList :: [[Card]] -> [Hand] -> [Hand]
handsInList [] toReturn = toReturn
handsInList (c:cs) toReturn = handsInList cs (toReturn ++ [hand c])

--------------------- General helper functions -----------------------

dropValue :: Eq a => [a] -> a -> [a]
dropValue [] v' = []
dropValue (v:vs) v' | has (v:vs) v' == False = (v:vs)
                    | v' == v = dropValue vs v'
                    | otherwise = [v] ++ dropValue vs v'

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

minimum' :: Ord a => [a] -> a
minimum' lst = head (quicksort lst)

fst' :: (a,a,a) -> a
fst' (x,y,z) = x

snd' :: (a,a,a) -> a
snd' (x,y,z) = y

thr' :: (a,a,a) -> a
thr' (x,y,z) = z

atIndexb :: Int -> [a] -> a
atIndexb a (x:xs) | a >= length (x:xs) = head (reverse xs)
                  | otherwise = head (drop a (x:xs))