{-# LANGUAGE ImplicitParams #-}

module MP5Spec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO,run,assert)
import Control.Exception
import MP5a
import MP5b


spec :: Spec
spec = do
  describe "Search" $ do
    it "Works as expected" $ do
      pendingWith "Your tests here!"
    it "Given graph nodes and a specific location, returns a list of neighbors" $ do
      adjLocs ([(0,0),(4,0),(2,0),(0,1)]::[GraphLoc])
              ((0,0)::GraphLoc)
              ([((0,0),[(4,0),(2,0),(0,1)]),
                ((4,0),[(2,0),(0,1),(0,0)]),
                ((2,0),[(0,1),(0,0),(4,0)]),
                ((0,1),[(0,0),(4,0),(2,0)])]::NeighborDict)
              `shouldBe` ([(4,0),(2,0),(0,1)]::[GraphLoc])
      adjLocs ([]::[GraphLoc]) 
              ((0,0)::GraphLoc) 
              ([((0,0),[(4,0),(2,0),(0,1)]),
                ((4,0),[(2,0),(0,1),(0,0)]),
                ((2,0),[(0,1),(0,0),(4,0)]),
                ((0,1),[(0,0),(4,0),(2,0)])]::NeighborDict) `shouldBe` ([]::[GraphLoc])
      adjLocs ([(0,0),(4,0),(2,0),(0,1)]::[GraphLoc])
              ((0,0)::GraphLoc)
              ([]::NeighborDict) `shouldBe` ([]::[GraphLoc])
    it "Converts from Maybe Graph to Graph" $ do
      fromJustGraph Nothing `shouldBe` Empty
      fromJustGraph (aStarSolveGraph Empty) `shouldBe` Empty
    --   --randomGraph 4 6 (5,5) 23 `shouldBe`
    --   --solveRandomGraph 5 7 (6,5) 17 aStarSolveGraph `shouldBe`
    it "Prints empty graph" $ do
      printGraph Empty `shouldBe` ""
    it "Returns proper output for 0 random pairs" $ do
      createPairings 0 [(0,0),(4,0),(0,1),(0,3),(1,0)] `shouldBe` []
      createPairings 6 [] `shouldBe` []
      createPairings 6 [(0,0),(4,0),(0,1),(0,3),(1,0)]`shouldBe` [((4,0),(0,3)),((0,0),(0,3)),
                                                                  ((0,0),(1,0)),((0,1),(1,0)),
                                                                  ((0,0),(4,0)),((0,0),(0,1))]
    it "Given nodes and neighbors list, empty mapping structure" $ do
      pToD ([
        ((0,0),(4,0)),((0,0),(0,1)),
        ((0,0),(0,3)),((0,0),(1,0)),
        ((4,0),(0,1)),((4,0),(0,3))], []) `shouldBe` []
      pToD ([((0,0),(4,0)),((0,0),(0,1)),
             ((0,0),(0,3)),((0,0),(1,0)),
             ((4,0),(0,1)),((4,0),(0,3))], 
             [(0,0),(4,0),(0,1),(0,3),(1,0)]) `shouldBe`[((0,0),[(4,0),(0,1),(0,3),(1,0)]),((4,0),[(0,1),(0,3),(0,0)]),
                                                         ((0,1),[(0,0),(4,0)]),((0,3),[(0,0),(4,0)]),((1,0),[(0,0)])]
    it "Given a max value and seed, returns a random range." $ do
      randomRange 100 3 `shouldBe` (50,4)
      randomRange 0 6 `shouldBe` (0,12)
    it "Creates random list of ordered pairs within specified range." $ do
      randomPairList 10 (5,5) 12 `shouldBe`[(2,0),(0,0),(4,0),(0,1),(0,3),(0,2),(3,1),(2,4),(2,2),(0,4)]
      randomPairList 0 (20,9) 13 `shouldBe` []
    it "Returns a list of divisors for a given number." $ do
      divisors (100::Int) `shouldBe` [1,2,4,5,10,20,25,50,100]
      divisors (0::Int) `shouldBe` [1,0]
    it "When asking for 0 random elements from list, returns empty list" $ do
      nRandElems 0 ([1,2,3,4,5]::[Int]) `shouldBe` []
      nRandElems 7 ([]::[Int]) `shouldBe` []
      nRandElems 5 ([1..100]::[Int]) `shouldBe` [15,95,73,19,60]
    it "handles empty list" $ do
      removeDuplicates ([]::[Int]) `shouldBe` []
      removeDuplicates ([0,3,4,5,3,6,7,5,3,17,3]::[Int]) `shouldBe` [0,4,6,7,5,17,3]
      removeDuplicates ([0,1,2,3]::[Int]) `shouldBe` [0,1,2,3]
    it "Handles empty list" $ do
      dropFromList (7::Int) [] `shouldBe` []
      dropFromList (7::Int) [1..10] `shouldBe` [1,2,3,4,5,6,8,9,10]
    it "Handles empty list" $ do
      contains (7::Int) ([]::[Int]) `shouldBe` False
      contains (7::Int) ([7,36]::[Int]) `shouldBe` True
      contains (9::Int) ([1..15]::[Int]) `shouldBe` True
      contains (9::Int) ([10..20]::[Int]) `shouldBe` False
    it "printGraph returns correct string" $ do
      printGraph (Graph nodes' zeroPaths emptyPath emptyAdjMap pairs d) `shouldBe` "\n\nAll Nodes: [(6,0),(4,2),(3,1),(4,0),(1,1),(2,0),(2,2),(0,0),(0,2),(5,1)]\n\nNeighbors: \n\nStart: (0,0)\nGoal: (5,1)\nPath: [(0,0),(5,1)]\nCost: 0\n\n"
    it "Search finds and returns input" $ do
      search (==5) (\x -> [x+1]) (++) [1] [] `shouldBe` Just 5
      search (==5) (\x -> [6,7,8]) (++) [1] [] `shouldBe` Just 1
    it "calling allNodes returns correct list of nodes" $ do
      allNodes g `shouldBe` [(6,0),(4,2),(3,1),(4,0),(1,1),(2,0),(2,2),(0,0),(0,2),(5,1)]
    it "calling graphPath returns correct list" $ do
      graphPath g `shouldBe` []
    it "calling graphNeighbors returns correct list" $ do
      graphNeighbors g `shouldBe` ([((3,1),(2,0)),((2,2),(0,0)),((2,2),(0,2)),((3,1),(0,2)),((4,0),(0,2)),((6,0),(2,2)),((4,2),(2,0)),((3,1),(2,2)),((2,0),(5,1)),((3,1),(5,1)),((6,0),(4,2)),((4,2),(4,0)),((4,0),(0,0)),((0,0),(0,2)),((1,1),(2,0)),((4,2),(0,2)),((4,2),(2,2)),((6,0),(0,2)),((6,0),(2,0)),((2,0),(0,2))]::GraphNeighbors)
    it "calling graphNeighDict returns correct list" $ do
      graphNeighDict g `shouldBe` ([((6,0),[(2,2),(4,2),(0,2),(2,0)]),((4,2),[(2,0),(4,0),(0,2),(2,2),(6,0)]),((3,1),[(2,0),(0,2),(2,2),(5,1)]),((4,0),[(0,2),(0,0),(4,2)]),((1,1),[(2,0)]),((2,0),[(5,1),(0,2),(3,1),(4,2),(1,1),(6,0)]),((2,2),[(0,0),(0,2),(6,0),(3,1),(4,2)]),((0,0),[(0,2),(2,2),(4,0)]),((0,2),[(2,2),(3,1),(4,0),(0,0),(4,2),(6,0),(2,0)]),((5,1),[(2,0),(3,1)])]::NeighborDict)
    it "aStarSolveGraph returns solved graph" $ do 
      show <$> aStarSolveGraph (Graph nodes' zeroPaths emptyPath emptyAdjMap pairs d) `shouldBe` Just "\n\nAll Nodes: [(6,0),(4,2),(3,1),(4,0),(1,1),(2,0),(2,2),(0,0),(0,2),(5,1)]\n\nNeighbors: \n\nStart: (0,0)\nGoal: (5,1)\nPath: [(0,0),(5,1)]\nCost: 0\n\n"
    it "aStarSolveGraph returns Nothing when input is empty" $ do
      aStarSolveGraph Empty `shouldBe` Nothing
    it "aStarCost function returns cost for a given path" $ do
      aStarCost (6,6) [(6,0),(4,2),(3,1),(4,0),(1,1),(2,0),(2,2),(0,0),(0,2),(5,1)] `shouldBe` 139
    it "returns the Start or Goal node when called" $ do
      getStartGoal g 's' `shouldBe` (0,0)
      getStartGoal g 'g' `shouldBe` (5,1)
      getStartGoal Empty 's' `shouldBe` (0,0)
      getStartGoal Empty 'g' `shouldBe` (0,0)


  describe "Minimax" $ do
    it "Works as expected" $ do
      pendingWith "Your tests here!"
    it "correctly prints empty board" $ do
      show emptyBoard `shouldBe` "-------\n-------\n-------\n-------\n-------\n-------"
    it "correctly prints full board" $ do
      show (playMoves [1..42]) `shouldBe` "BABABAB\nABABABA\nBABABAB\nABABABA\nBABABAB\nABABABA"
    it "separates lists into chunks of specified lengths (part 1)" $ do
      myChunksOf 0 ([1..4]::[Int]) `shouldBe` [[1..4]]
      myChunksOf 3 ([]::[Int]) `shouldBe` []
      myChunksOf 4 ([1..16]::[Int]) `shouldBe` [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
    it "Opponent function changes from A to B or vice versa" $ do
      opponent A `shouldBe` B
      opponent B `shouldBe` A
    it "PlayMove works properly." $ do
      show <$> playMove 1 A emptyBoard `shouldBe` ["Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Just A",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing"]
      show <$> playMove 1 A (playMoves [1..42]) `shouldBe` ["Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A",
                                                            "Just B","Just A","Just B","Just A","Just B","Just A"]
      show <$> playMove 8 B emptyBoard `shouldBe` ["Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Just B",
                                                   "Nothing","Nothing","Nothing","Nothing","Nothing","Nothing"]

    it "PlayMoves works properly." $ do
      playMoves [] `shouldBe` emptyBoard
      show <$> playMoves [5..90] `shouldBe` ["Just A","Just B","Just A","Just B","Just B","Just A","Just B",
                                             "Just B","Just A","Just B","Just A","Just A","Just B","Just A",
                                             "Just A","Just B","Just A","Just B","Just B","Just A","Just B",
                                             "Just B","Just A","Just B","Just A","Just A","Just B","Just A",
                                             "Just A","Just B","Just A","Just B","Just B","Just A","Just B",
                                             "Just B","Just A","Just B","Just A","Just A","Just B","Just A"]
    it "Performs element-wise addition properly (1)" $ do
      element_wise_add [] [1..3] `shouldBe` []
      element_wise_add [1..3] [] `shouldBe` []   
      element_wise_add [1..3] [4..6] `shouldBe` [5,7,9]   
    it "Returns consecutive lists of 4 with start and end values incrementing by 1" $ do
      getDiagIdx True [0..5] `shouldBe` [[0,1,2,3],[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7,8]]
      getDiagIdx False [0..5] `shouldBe` [[0,-1,-2,-3],[1,0,-1,-2],[2,1,0,-1],[3,2,1,0],[4,3,2,1],[5,4,3,2]]
    it "Given indices, returns list of pieces along diagonals (1a)" $ do
      diagsFromIdx [] emptyBoard `shouldBe` []
      show <$> diagsFromIdx ([[(1,1),(2,2),(3,3),(4,4)]]::[[(Int,Int)]]) emptyBoard `shouldBe` ["----"]
      show <$> diagsFromIdx diag_idx (playMoves [1..42]) `shouldBe` ["BBBB","AAAA","BBBB","AAAA","AAAA","BBBB","AAAA","BBBB","BBBB","AAAA","BBBB","AAAA","AAAA","BBBB","AAAA","BBBB","BBBB","AAAA","BBBB","AAAA","AAAA","BBBB","AAAA","BBBB"]
    it "Given a board, returns a list of winning entries" $ do
      show <$> lines' (playMoves [1..42]) `shouldBe` ["BABA","ABAB","BABA","ABAB","ABAB","BABA",
                                                      "ABAB","BABA","BABA","ABAB","BABA","ABAB",
                                                      "ABAB","BABA","ABAB","BABA","BABA","ABAB",
                                                      "BABA","ABAB","ABAB","BABA","ABAB","BABA",
                                                      "BABA","ABAB","BABA","ABAB","BABA","ABAB",
                                                      "BABA","ABAB","BABA","ABAB","BABA","ABAB",
                                                      "BABA","ABAB","BABA","ABAB","BABA","ABAB",
                                                      "BABA","ABAB","BABA","BBBB","AAAA","BBBB",
                                                      "AAAA","AAAA","BBBB","AAAA","BBBB","BBBB",
                                                      "AAAA","BBBB","AAAA","AAAA","BBBB","AAAA",
                                                      "BBBB","BBBB","AAAA","BBBB","AAAA","AAAA",
                                                      "BBBB","AAAA","BBBB"]
    it "Takes a board and a player and determines whether that player has any winning moves on the board." $ do
      wins A (playMoves [1..30]) `shouldBe` True
      wins B (playMoves [1..30]) `shouldBe` True
      wins A (playMoves [1..20]) `shouldBe` False
      wins B (playMoves [1..20]) `shouldBe` False
      wins A (playMoves [1..22]) `shouldBe` False
      wins B (playMoves [1..22]) `shouldBe` True
    it "Takes a board and determines whether any winning moves exist on the board. (1)" $ do
      won emptyBoard `shouldBe` False
      won (playMoves [1..22]) `shouldBe` True
    it "Determines whether the board is full." $ do
      full emptyBoard `shouldBe` False
      full (playMoves [1..30]) `shouldBe` False
      full (playMoves [1..42]) `shouldBe` True
    it "Determines which player's turn it is." $ do
      turn emptyBoard `shouldBe` A
      turn (playMoves [1..11]) `shouldBe` B
      turn (playMoves [1..16]) `shouldBe` A
      turn (playMoves [1..42]) `shouldBe` A
    it "Takes rows of Maybe Pieces and returns available moves." $ do
      move [] `shouldBe` []
      move [[]] `shouldBe` []
      move (myChunksOf 7 emptyBoard) `shouldBe` [1,2,3,4,5,6,7]
      move (myChunksOf 7 (playMoves [1..40])) `shouldBe` [6,7]
      move (myChunksOf 7 (playMoves [1..42])) `shouldBe` []
    it "Prunes a gameTree to specified number of levels." $ do
      show (prune 0 (gameTree (playMoves [1..40]))) `shouldBe` "Node {rootLabel = BABAB--\nABABABA\nBABABAB\nABABABA\nBABABAB\nABABABA, subForest = []}"
      show (prune 1 (gameTree emptyBoard)) `shouldBe` "Node {rootLabel = -------\n-------\n-------\n-------\n-------\n-------, subForest = [Node {rootLabel = -------\n-------\n-------\n-------\n-------\n-A-----, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\n--A----, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\n---A---, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\n----A--, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\n-----A-, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\n------A, subForest = []},Node {rootLabel = -------\n-------\n-------\n-------\n-------\nA------, subForest = []}]}"
      show (prune 0 (gameTree emptyBoard)) `shouldBe`  "Node {rootLabel = -------\n-------\n-------\n-------\n-------\n-------, subForest = []}"
      show (prune 100 (gameTree (playMoves [1..40]))) `shouldBe` "Node {rootLabel = BABAB--\nABABABA\nBABABAB\nABABABA\nBABABAB\nABABABA, subForest = []}"
    it "Shows the score of a given player on  given board." $ do
      show (scoreBoard A (playMoves [1..42])) `shouldBe` "Score: 100\n\nBABABAB\nABABABA\nBABABAB\nABABABA\nBABABAB\nABABABA"
      show (scoreBoard B (playMoves [1..42])) `shouldBe` "Score: 100\n\nBABABAB\nABABABA\nBABABAB\nABABABA\nBABABAB\nABABABA"
      show (scoreBoard B (playMoves [1..22])) `shouldBe` "Score: 100\n\n-------\n-------\nB------\nABABABA\nBABABAB\nABABABA"
      show (scoreBoard A (playMoves [1..22])) `shouldBe` "Score: -100\n\n-------\n-------\nB------\nABABABA\nBABABAB\nABABABA"
      show (scoreBoard B (playMoves [1..12])) `shouldBe` "Score: 0\n\n-------\n-------\n-------\n-------\nBABAB--\nABABABA"
      show (scoreBoard A (playMoves [1..12])) `shouldBe` "Score: 0\n\n-------\n-------\n-------\n-------\nBABAB--\nABABABA"
    it "Uses minimax_ab to assign a score to a board for a player" $ do               
      ((score $ minimax_ab (scoreBoard B) (gameTree (playMoves [1..41]))) < -9000000000000000000) `shouldBe` True
      (score $ minimax_ab (scoreBoard B) (prune 1 (gameTree emptyBoard))) `shouldBe` 0
      (scoredVal $  minimax_ab (scoreBoard B) (prune 1 (gameTree emptyBoard))) == (playMoves [1]) `shouldBe` True

    



    



      


