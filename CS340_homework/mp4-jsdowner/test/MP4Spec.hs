{-# LANGUAGE ImplicitParams #-}

module MP4Spec (spec) where

import Test.Hspec
import Test.HUnit
import MP4


spec :: Spec
spec = do
  describe "MP4" $ do
    it "parses valid functions correctly" $ do
      sequence_ $ zipWith shouldBe (run funcDef <$> testCases)
                                   (Just <$> testResults)
    it "fails on invalid functions" $ do
      sequence_ $ (`shouldBe` Nothing) <$> (run funcDef <$> failCases)


testCases =
  [  "int foo1() { }"              -- expected: Just (("foo1",[],[],""),"")

  ,  "int foo2(char param1) { }"   -- expected: Just (("foo2",["param1"],[],""),"")

  ,  "char foo3(char param1) { \
     \   return param1; \
     \}"

  ,  "char foo4(char param1) { \
     \   return -1; \
     \}"

  ,  "char foo5(char p1, char p2, int p3) { \
     \   return 0; \
     \}"

  ,  "char foo6(char p1, char p2, int p3) { \
     \   char local1; \
     \   return local1; \
     \}"

  ,  "char foo7(char p1, char p2, int p3) { \
     \   char l1, l2, l3; \
     \   int l4, l5, l6;  \
     \   return -1; \
     \ }"

  ,  "char foo8(int p1, int p2, int p3) { \
     \   char buf1; \
     \   int n, m; \
     \   char buf2; \
     \   n = m; \
     \   buf1 = 10; \
     \   buf2 = -20; \
     \   return 100; \
     \}"
  ]


testResults =
  [  (("foo1",[],[],""),"")
  ,  (("foo2",["param1"],[],""),"")
  ,  (("foo3",["param1"],[],"param1"),"")
  ,  (("foo4",["param1"],[],"-1"),"")
  ,  (("foo5",["p1","p2","p3"],[],"0"),"")
  ,  (("foo6",["p1","p2","p3"],["local1"],"local1"),"")
  ,  (("foo7",["p1","p2","p3"],["l1","l2","l3","l4","l5","l6"],"-1"),"")
  ,  (("foo8",["p1","p2","p3"],["buf1","n","m","buf2"],"100"),"")
  ]


failCases =
  [  "foo1() { }"

  ,  "void foo2() { }"

  ,  "int foo3(int) { }"     -- expected nothing, got Just (("foo3",[],[],""),"")

  ,  "int foo4(int i,) { }"

  ,  "int foo5(char i) { \   
     \   return i \
     \}"                     -- expected nothing, got "Just (("foo5",["i"],[],""),"")"


  ,  "int foo6(char i) { \
     \   char l \
     \   return l; \
     \}"

  ,  "int foo7(char i) { \
     \   return l; \
     \   char l; \
     \}"

  ,  "int foo8(char i) { \
     \   char l; \
     \   return l; \
     \   l = 10; \
     \}"

  , "int foo9(char i) { \
     \   l = 10; \
     \   char l; \
     \   return l; \
     \}"
  ]
