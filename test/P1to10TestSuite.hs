module P1to10TestSuite where

import P1to10
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

p1to10TestSuite :: TestTree
p1to10TestSuite =
  testGroup
    "Pr. 1-10"
    [ myLastTestSuite
    , myButLastTestSuite
    , elementAtTestSuite
    , myLengthTestSuite
    , myReverseTestSuite
    , isPalindromeTestSuite
    , flattenTestSuite
    , compressTestSuite
    , packTestSuite
    , encodeTestSuite
    ]

-- 1. test suite
myLastTestSuite :: TestTree
myLastTestSuite = testGroup "Problem 1" [lastElemIntList, lastElemCharList]

lastElemIntList = testCase "Last element of int list" $ assertEqual [] 4 (myLast [1, 2, 3, 4])

lastElemCharList = testCase "Last element of char list" $ assertEqual [] 'z' (myLast ['x', 'y', 'z'])

-- 2. test suite
myButLastTestSuite :: TestTree
myButLastTestSuite = testGroup "Problem 2" [butLastElemIntList, butLastElemCharList]

butLastElemIntList = testCase "Last element of int list" $ assertEqual [] 3 (myButLast [1, 2, 3, 4])

butLastElemCharList = testCase "Last element of char list" $ assertEqual [] 'y' (myButLast ['a' .. 'z'])

-- 3. test suite
elementAtTestSuite :: TestTree
elementAtTestSuite = testGroup "Problem 3" [kthElemIntList, kthElemCharList]

kthElemIntList = testCase "Kth element of int list" $ assertEqual [] 2 (elementAt [1, 2, 3] 2)

kthElemCharList = testCase "Kth element of char list" $ assertEqual [] 'e' (elementAt "haskell" 5)

-- 4. test suite
myLengthTestSuite :: TestTree
myLengthTestSuite = testGroup "Problem 4" [lenIntList, lenCharList]

lenIntList = testCase "Length of int list" $ assertEqual [] 3 (myLength [123, 456, 789])

lenCharList = testCase "Kth element of char list" $ assertEqual [] 13 (myLength "Hello, world!")

-- 5. test suite
myReverseTestSuite :: TestTree
myReverseTestSuite = testGroup "Problem 5" [revIntList, revCharList]

revIntList = testCase "Reverse int list" $ assertEqual [] [1, 2, 3, 4] (myReverse [4, 3, 2, 1])

revCharList = testCase "Reverse char list" $ assertEqual [] "!amanap ,lanac a ,nalp a ,nam A" (myReverse "A man, a plan, a canal, panama!")

-- 6. test suite
isPalindromeTestSuite :: TestTree
isPalindromeTestSuite = testGroup "Problem 6" [palindromeNotIntList, palindromeIsCharList, palindromeIsIntList]

palindromeNotIntList = testCase "Is not palindrome int list" $ assertEqual [] False (isPalindrome [1, 2, 3])

palindromeIsCharList = testCase "Is palindrome char list" $ assertEqual [] True (isPalindrome "madamimadam")

palindromeIsIntList = testCase "Is palindrome int list" $ assertEqual [] True (isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1])

-- 7. test suite
flattenTestSuite :: TestTree
flattenTestSuite = testGroup "Problem 7" [flattenElem, flattenElemList, flattenEmptyList]

flattenElem = testCase "Flatten on Elem item" $ assertEqual [] [5] (flatten (Elem 5))

flattenElemList = testCase "Flatten an Elem List" $ assertEqual [] [1, 2, 3, 4, 5] (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))

flattenEmptyList = testCase "Flat an empty List" $ assertEqual [] ([] :: [Int]) (flatten (List []))

-- 8. test suite
compressTestSuite :: TestTree
compressTestSuite = testGroup "Problem 8" [compressElemList]

compressElemList = testCase "Eompress a char list" $ assertEqual [] "abcade" (compress "aaaabccaadeeee")

-- 9. test suite
packTestSuite :: TestTree
packTestSuite = testGroup "Problem 9" [packElemList]

packElemList = testCase "Pack a char list" $ assertEqual [] ["aaaa", "b", "cc", "aa", "d", "eeee"] (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])

-- 10. test suite
encodeTestSuite :: TestTree
encodeTestSuite = testGroup "Problem 10" [encodeElemList]

encodeElemList = testCase "encode a char list" $ assertEqual [] [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
