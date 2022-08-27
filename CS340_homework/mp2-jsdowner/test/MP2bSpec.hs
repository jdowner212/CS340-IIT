{-# LANGUAGE ImplicitParams #-}

module MP2bSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Control.Exception
import MP2b


spec :: Spec
spec = do
  describe "makeWorld" $ do
    it "creates worlds of the correct size" $ do
      property prop_worldSize
      
  describe "liveNeighbors" $ do
    it "works correctly" $ do
      pendingWith "Write your own tests here!"

  describe "nextWorld" $ do
    it "works correctly" $ do
      pendingWith "Write your own tests here!"


prop_worldSize :: Property
prop_worldSize = forAll dims $ \(w,h) -> 
                   let (d, world) = makeWorld (w,h)
                   in d == (w,h) 
                      && length world == h 
                      && all ((== w) . length) world
  where dims = arbitrary `suchThat` (\(x,y) -> x > 0 && y > 0)
