import P1to10TestSuite
import Test.Tasty (TestTree, defaultMain, testGroup)

main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Haskell Ninety-Nine Problems Tests"
        [ p1to10TestSuite
        ]
