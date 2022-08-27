{-# LANGUAGE ImplicitParams #-}

module MidtermSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import Midterm

spec :: Spec
spec = do
  describe "Student info" $ do
    it "has been updated" $ do 
      student_name `shouldNotBe` "Charlie S. Student"
      student_id `shouldNotBe` "A12345678"
  
  describe "Part A" $ do
    it "contains a valid definition for pa_1" $ do
      seq pa_1 True `shouldBe` True
    it "contains a valid definition for pa_2" $ do
      seq pa_2 True `shouldBe` True
    it "contains a valid definition for pa_3" $ do
      seq pa_3 True `shouldBe` True
    it "contains a valid definition for pa_4" $ do
      seq pa_4 True `shouldBe` True
    it "contains a valid definition for pa_5" $ do
      seq pa_5 True `shouldBe` True

  describe "listsFrom" $ do
    it "works for the provided examples" $ do
      take 5 (listsFrom 5) `shouldBe` [[5],[5,6],[5,6,7],[5,6,7,8],[5,6,7,8,9]]

  describe "riffle" $ do
    it "works for the provided examples" $ do
      riffle 5 "abracadabra" "supercalifragilistic" `shouldBe` "abracsuperadabrcalifaragilistic"

  describe "ngrams" $ do
    it "works for the provided examples" $ do
      ngrams 5 "abracadabra" `shouldBe` ["abrac","braca","racad","acada","cadab","adabr","dabra"] 
      
  describe "autokeyEncrypt" $ do
    it "works for the provided examples" $ do
      autokeyEncrypt 'G' "PARIS, FRANCE" `shouldBe` "VPRZA, XWRNPG"

  describe "autokeyDecrypt" $ do
    it "works for the provided examples" $ do
      autokeyDecrypt 'G' "VPRZA, XWRNPG" `shouldBe` "PARIS, FRANCE"
  
  describe "ttt_place" $ do
    it "works for the provided examples" $ do
      ttt_place ttt_empty 1 (2,1)  `shouldBe` [[-1,-1,-1],[-1,-1,-1],[-1,1,-1]]
      
  describe "ttt_play" $ do
    it "works for the provided examples" $ do
      ttt_play ttt_empty 1 [(1,1),(0,0),(2,0),(0,2),(0,1)] `shouldBe` [[0,1,0],[-1,1,-1],[1,-1,-1]]

  describe "ttt_win" $ do
    it "works for the provided examples" $ do
      ttt_win [[1,1,1],[-1,-1,-1],[-1,-1,-1]] 1 `shouldBe` True
      ttt_win [[1,1,1],[-1,-1,-1],[-1,-1,-1]] 0 `shouldBe` False   

  describe "ttt_winningMoves" $ do
    it "works for the provided examples" $ do
      ttt_winningMoves [[0,-1,1],[-1,0,-1],[1,-1,1]] 1 `shouldBe` [(1,2),(2,1)] 
