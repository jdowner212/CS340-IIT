{-# LANGUAGE ImplicitParams #-}

module MP3Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP3a
import MP3b

spec :: Spec
spec = do
  describe "Binary tree" $ do
    describe "treeRepeat" $ do 
      it "returns the correct root node" $ do
        let (Node x _ _) = treeRepeat 10 in x `shouldBe` 10
      it "creates a tree consisting only of provided value" $ do
        (treeFromList $ take 7 $ treeToList $ treeRepeat 'a') `shouldBe` (Node 'a' (Node 'a' (Node 'a' EmptyTree EmptyTree) (Node 'a' EmptyTree EmptyTree)) (Node 'a' (Node 'a' EmptyTree EmptyTree) (Node 'a' EmptyTree EmptyTree)))

    describe "treeNats" $ do
      it "returns tree with ordered natural numbers beginning at one and increasing breadth-wise across the tree." $ do
        (treeFromList $ take 7 $ treeToList $ treeNats) `shouldBe` (Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 3 (Node 6 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)))

    describe "treeVal" $ do 
      it "follows directions at nodes and returns target value" $ do
        treeVal [L,R] treeNats `shouldBe` 5
        treeVal [] treeNats `shouldBe` 1
    
    describe "treeToList" $ do
      it "takes a tree and returns a list of node values corresponding to the breadth-wise traversal of the tree" $ do
        (take 10 $ treeToList treeNats) `shouldBe` [1,2,3,4,5,6,7,8,9,10]
        -- treeToList EmptyTree `shouldBe` []
    
    describe "treeFlip" $ do
      it "takes a tree and returns its mirror image" $ do
        treeFlip (Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 3 (Node 6 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)))
          `shouldBe` (Node 1 (Node 3 (Node 7 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 2 (Node 5 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)))

    describe "treeFromList" $ do
      it "takes a list representing the values of a breath-width traveresal and returns a tree" $ do 
        treeFromList [1,2,3,4,5,6,7] `shouldBe` (Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 3 (Node 6 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)))

    describe "treeIterate" $ do
      it "takes a funciton and an initial value and returns a tree where each node represents the repeated application of the function to that value" $ do
        (treeFromList $ take 3 $ treeToList $ treeFlip $ treeIterate (2*) 1) `shouldBe` (treeFromList [1,4,2])

    describe "showTree" $ do 
      it "takes a tree and displays it" $ do
        showTree (treeFromList $ take 7 $ treeToList $ treeNats) 0 `shouldBe` "1\n*2\n**4\n**5\n*3\n**6\n**7\n"

    describe "atIndexa" $ do
      it "takes an index and a list and returns the value in the list at that index" $ do
        atIndexa 8 ['a','b','c','d','e','f'] `shouldBe` 'f'
        atIndexa 4 ['a','b','c','d','e','f'] `shouldBe` 'e'

    describe "applyNTimes" $ do
      it "takes a function, a value, and an integer and returns the result of applying the function to the value that many times" $ do
        applyNTimes (2*) 1 3 `shouldBe` 8
        applyNTimes (2*) 1 0 `shouldBe` 1

  describe "Poker stats" $ do
    describe "deck" $ do      
      it "is the correct length" $ do
        length deck `shouldBe` 52
    
    describe "hand" $ do
      it "takes a list of 5 cards and returns the strongest hand that can be played" $ do
        (hand [Card A H, Card K H, Card Q H, Card J H, Card Ten H]) `shouldBe` RoyalFlush
        (hand [Card Two S, Card Three S, Card Four S, Card Five S, Card Six S]) `shouldBe` StraightFlush
        (hand [Card Two H, Card Three D, Card A H, Card Five D, Card Four S]) `shouldBe` Straight
        (hand [Card A S, Card A H, Card A C, Card A D, Card Five D]) `shouldBe` FourOfAKind
        (hand [Card A S, Card Three S, Card Seven S, Card Ten S, Card J S]) `shouldBe` Flush
        (hand [Card Two D, Card Two S, Card Five S, Card Five D, Card Five H]) `shouldBe` FullHouse
        (hand [Card Four D, Card Seven S, Card Five S, Card Five D, Card Five H]) `shouldBe` ThreeOfAKind
        (hand [Card Four S, Card Four S, Card Five S, Card Five D, Card J H]) `shouldBe` TwoPair
        (hand [Card Four S, Card Six S, Card Five S, Card Seven D, Card Four H]) `shouldBe` Pair
        (hand [Card Q S, Card Six S, Card Five S, Card Seven D, Card Four H]) `shouldBe` HighCard


    describe "computeStats" $ do
      it "takes a list of 5-card lists and returns a list of tuples showing the frequency of each hand" $ do
        computeStats [[Card Three S, Card Three D, Card Three C, Card Two D, Card Two C],
                      [Card Four S, Card Four C, Card Two S, Card Two C, Card Q H],
                      [Card A S, Card Four C, Card Two D, Card Three H, Card Seven S],
                      [Card Four S, Card Four D, Card Four C, Card Five D, Card Five C],
                      [Card Five S, Card Four S, Card A S, Card Q S, Card K S]] `shouldBe` [(2,FullHouse),(1,Flush),(1,TwoPair),(1,HighCard)]

    describe "isRoyalFlush" $ do
      it "determines whether a list of five cards is a royal flush" $ do
        (isRoyalFlush [Card A H, Card K H, Card Q H, Card J H, Card Ten H]) `shouldBe` True
        (isRoyalFlush [Card A H, Card Two H, Card Three H, Card Four H, Card Five H]) `shouldBe` False
    
    describe "isStraightFlush" $ do 
      it "determines whether a list of five cards is a straight flush" $ do
        (isStraightFlush [Card A H, Card Two H, Card Three H, Card Four H, Card Five H]) `shouldBe` True
        (isStraightFlush [Card A H, Card Ten H, Card J H, Card Q H, Card K H]) `shouldBe` True
        (isStraightFlush [Card Nine H, Card Ten H, Card J H, Card Q H, Card K H]) `shouldBe` True
        (isStraightFlush [Card A H, Card Three H, Card Four H, Card Five H, Card Six H]) `shouldBe` False

    describe "isFourOfAKind" $ do
      it "determines whether a list of five cards has four of the same value" $ do
        (isFourOfAKind [Card Four S, Card Four C, Card Four H, Card Two C, Card Four D]) `shouldBe` True
        (isFourOfAKind [Card Four S, Card Four C, Card Four H, Card Two C, Card Three D]) `shouldBe` False

    describe "isFlush" $ do
      it "determines whether a list of 5 cards all have the same suit" $ do
        (isFlush [Card Five S, Card Seven S, Card Two S, Card Q S, Card A S]) `shouldBe` True
        (isFlush [Card Five S, Card Seven C, Card Two H, Card Q C, Card A D]) `shouldBe` False

    describe "isStraight" $ do 
      it "determines whether a list of five cards has five consecutive values" $ do
        (isStraight [Card A D, Card Two H, Card Three S, Card Four H, Card Five C]) `shouldBe` True
        (isStraight [Card Nine H, Card Ten S, Card J H, Card Q D, Card K C]) `shouldBe` True
        (isStraight [Card A D, Card Three H, Card Four H, Card Five H, Card Six H]) `shouldBe` False    

    describe "isThreeOfAKind" $ do
      it "determines whether a list of five cards has three of the same value" $ do
        (isThreeOfAKind [Card Four S, Card Four C, Card Four H, Card Two C, Card Seven D]) `shouldBe` True
        (isThreeOfAKind [Card Four S, Card Four C, Card Seven H, Card Two C, Card Nine D]) `shouldBe` False

    describe "isTwoPair" $ do
      it "determines whether a list of five cards has two pairs of matching values" $ do
        (isTwoPair [Card Four S, Card Four C, Card Five H, Card Five C, Card Seven D]) `shouldBe` True
        (isTwoPair [Card Four S, Card Five C, Card Nine H, Card Five C, Card Seven D]) `shouldBe` False

    describe "isPair" $ do
      it "determines whether a list of five cards has one pair of matching values" $ do
        (isPair [Card Four S, Card Four C, Card Five H, Card Six C, Card Seven D]) `shouldBe` True
        (isPair [Card Four S, Card Five C, Card Nine H, Card Two C, Card Seven D]) `shouldBe` False  
    
    describe "isHighCard" $ do
      it "determines whether no hands above 'high card' are valid" $ do
        (isHighCard [Card Four S, Card Seven D, Card K S, Card Six C, Card Two D]) `shouldBe` True
        (isHighCard [Card Four S, Card Seven D, Card K S, Card Four C, Card Two D]) `shouldBe` False

    describe "handsInList" $ do
      it "returns a list of hands present in a list of five cards" $ do
        handsInList [[Card Four S, Card Seven D, Card K S, Card Six C, Card Two D],
                     [Card Four S, Card Seven D, Card K S, Card Four C, Card Two D],
                     [Card Four S, Card Two D, Card Two S, Card Four C, Card Two D]] [] `shouldBe` [HighCard,Pair,FullHouse]
              
    describe "allDoubles" $ do
      it "returns a list of every possible pair that could be generated from values in a list" $ do
        allDoubles [1,2,3,4,5,5] `shouldBe` [(1,2),(1,3),(1,4),(1,5),(1,5),(2,3),(2,4),(2,5),(2,5),(3,4),(3,5),(3,5),(4,5),(4,5),(5,5)]

    describe "allPairs" $ do
      it "takes a list of values and returns list of two-tuples corresponding to those values that appeared more than once" $ do
        allPairs [1,2,3,4,5,5,5] `shouldBe` [(5,5)]
        allPairs [1,2,3,4,5] `shouldBe` []
    
    describe "allPerfectPairs" $ do
      it "takes a list of values and returns list of two-tuples corresponding to those values that appeared exactly twice" $ do
        allPerfectPairs [1,2,3,4,4,5,5,5] `shouldBe` [(4,4)]
        allPerfectPairs [1,2,3,4,5,5,5] `shouldBe` []

    describe "allTriplets" $ do
      it "takes a list of values and returns list of three-tuples corresponding to thoe values that appear more than twice" $ do
        allTriplets [1,2,3,4,5,5,5] `shouldBe` [(5,5,5)]
        allTriplets [1,2,3,4,5,5] `shouldBe` []
    
    describe "countOccurrences" $ do
      it "takes a list and a value and returns the number of times that value appears in the list" $ do
        countOccurrences [] 5 `shouldBe` 0
        countOccurrences [1,2,3,3,4,5,3,2,5] 5 `shouldBe` 2
    
    describe "countSameSuits" $ do
      it "takes a list of suits returns a list of the number of times each possible suit appears in that list" $ do
        countSameSuits [H,C] `shouldBe` [0,1,0,1]
        countSameSuits [] `shouldBe` [0,0,0,0]

    describe "countSameValues" $ do
      it "takes a list of values returns a list of the number of times each possible card value appears in that list" $ do
        countSameValues [Two,Seven,A,K,Seven] `shouldBe` [1,1,0,0,0,0,2,0,0,0,0,0,1]
        countSameValues [] `shouldBe` [0,0,0,0,0,0,0,0,0,0,0,0,0]

    -- describe "is'" $ do
    --   it "takes a hand and returns a partially-applied function to test whether a potential list of cards can be defined by that hand" $ do
    --     is' RoyalFlush    `shouldBe`  isRoyalFlush
    --     is' StraightFlush `shouldBe`  isStraightFlush
    --     is' FourOfAKind   `shouldBe`  isFourOfAKind
    --     is' Flush         `shouldBe`  isFlush
    --     is' FullHouse     `shouldBe`  isFullHouse
    --     is' Straight      `shouldBe`  isStraight
    --     is' ThreeOfAKind  `shouldBe`  isThreeOfAKind
    --     is' TwoPair       `shouldBe`  isTwoPair
    --     is' Pair          `shouldBe`  isPair
    --     is' HighCard      `shouldBe`  isHighCard

    describe "has" $ do
      it "takes a list and a value and returns boolean specifying whether that value is in the list" $ do
        has [1,2,3,4] 4 `shouldBe` True
        has [] 3 `shouldBe` False
        has [1,2,3,4,5] 6 `shouldBe` False

    
    describe "suits" $ do
      it "takes a list of cards and returns the suit corresponding to each card" $ do
        suits [Card A H, Card Five C, Card Six S, Card J H, Card K H] `shouldBe` [H,C,S,H,H]
        suits [] `shouldBe` []
    
    describe "values" $ do
      it "takes a list of cards and returns the value corresponding to each card" $ do
        values [Card A H, Card Five C, Card Six S, Card J H, Card K H] `shouldBe` [A,Five,Six,J,K]
        values [] `shouldBe` []

    describe "dropValue" $ do
      it "takes a list and a value and removes every instance of that value from the list" $ do
        dropValue [1,2,3,4] 4 `shouldBe` [1,2,3]
        dropValue [1,2,2,3,4] 2 `shouldBe` [1,3,4]
        dropValue [1,2,3,4] 5 `shouldBe` [1,2,3,4]
        dropValue [] 5 `shouldBe` []

    
    describe "quicksort" $ do
      it "takes a list and returns that same list sorted from least to greatest value" $ do
        quicksort [1,3,2,5,7] `shouldBe` [1,2,3,5,7]

    describe "minimum'" $ do
      it "takes a list and returns its minimum value" $ do
        minimum' [A,K,Four] `shouldBe` A
        minimum' [3,7,1,4] `shouldBe` 1
    
    describe "fst'" $ do
      it "takes a three-tuple and returns the first element" $ do
        fst' (4,3,7) `shouldBe` 4

    describe "snd'" $ do
      it "takes a three-tuple and returns the second element" $ do
        snd' (4,3,7) `shouldBe` 3

    describe "thr'" $ do
      it "takes a three-tuple and returns the third element" $ do
        thr' (4,3,7) `shouldBe` 7        

    describe "atIndexb" $ do
      it "takes an index and a list and returns the value in the list at that index" $ do
        atIndexb 4 [A,Two,Three,Seven,Nine,Ten] `shouldBe` Nine



    




