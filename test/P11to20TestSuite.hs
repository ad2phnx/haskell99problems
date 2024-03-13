module P11to20TestSuite where

import P11to20
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

p11to20TestSuite :: TestTree
p11to20TestSuite =
  testGroup
    "Pr. 11-20"
    [ encodeModTestSuite
    , decodeModTestSuite
    , encodeDirectTestSuite
    , dupliTestSuite
    , repliTestSuite
    , dropEveryTestSuite
    ]

-- 11. test suite
encodeModTestSuite :: TestTree
encodeModTestSuite = testGroup "Problem 11" [encodeModElemList]

encodeModElemList = testCase "encodeMod a char list" $ assertEqual [] [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] (encodeMod "aaaabccaadeeee")

-- 12. test suite
decodeModTestSuite :: TestTree
decodeModTestSuite = testGroup "Problem 12" [decodeModElemList]

decodeModElemList = testCase "decodeMod a char list" $ assertEqual [] "aaaabccaadeeee" (decodeMod [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'])

-- 13. test suite
encodeDirectTestSuite :: TestTree
encodeDirectTestSuite = testGroup "Problem 13" [encodeDirectElemList]

encodeDirectElemList =
  testCase "encodeDirect a char list" $
    assertEqual
      []
      [ Multiple 4 'a'
      , Single 'b'
      , Multiple 2 'c'
      , Multiple 2 'a'
      , Single 'd'
      , Multiple 4 'e'
      ]
      (encodeDirect "aaaabccaadeeee")

-- 14. test suite
dupliTestSuite :: TestTree
dupliTestSuite = testGroup "Problem 14" [dupliElemList]

dupliElemList =
  testCase "dupli a char list" $
    assertEqual
      []
      [1, 1, 2, 2, 3, 3]
      (dupli [1, 2, 3])

-- 15. test suite
repliTestSuite :: TestTree
repliTestSuite = testGroup "Problem 15" [repliElemList]

repliElemList =
  testCase "repli a char list" $
    assertEqual
      []
      "aaabbbccc"
      (repli "abc" 3)

-- 16. test suite
dropEveryTestSuite :: TestTree
dropEveryTestSuite = testGroup "Problem 16" [dropEveryElemList]

dropEveryElemList =
  testCase "dropEvery a char list" $
    assertEqual
      []
      "abdeghk"
      (dropEvery "abcdefghik" 3)
