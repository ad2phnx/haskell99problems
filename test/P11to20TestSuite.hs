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
    , splitTestSuite
    , sliceTestSuite
    , rotateTestSuite
    , removeAtTestSuite
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

-- 17. test suite
splitTestSuite :: TestTree
splitTestSuite = testGroup "Problem 17" [splitElemList]

splitElemList =
  testCase "split a char list" $
    assertEqual
      []
      ("abc", "defghik")
      (split "abcdefghik" 3)

-- 18. test suite
sliceTestSuite :: TestTree
sliceTestSuite = testGroup "Problem 18" [sliceElemList]

sliceElemList =
  testCase "slice a char list" $
    assertEqual
      []
      "cdefg"
      (slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7)

-- 19. test suite
rotateTestSuite :: TestTree
rotateTestSuite = testGroup "Problem 19" [rotateElemList, revRotateElemList]

rotateElemList =
  testCase "rotate a char list" $
    assertEqual
      []
      "defghabc"
      (rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3)

revRotateElemList =
  testCase "rotate a char list" $
    assertEqual
      []
      "ghabcdef"
      (rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2))

-- 20. test suite
removeAtTestSuite :: TestTree
removeAtTestSuite = testGroup "Problem 20" [removeAtElemList]

removeAtElemList =
  testCase "removeAt a char list" $
    assertEqual
      []
      ('b', "acd")
      (removeAt 2 ['a', 'b', 'c', 'd'])
