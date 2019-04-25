import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1 + 2) 2)

tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests