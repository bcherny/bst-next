import Test.HUnit
import BSTv1

-- Test case

--              8
--           /    \
--         3       10
--       /  \       \
--     1     6       14
--         /  \     /
--       4     7  13

n1 = Node Nil 1 Nil n3
n4 = Node Nil 4 Nil n6
n7 = Node Nil 7 Nil n6
n6 = Node n4 6 n7 n3
n3 = Node n1 3 n6 n8
n13 = Node Nil 13 Nil n14
n14 = Node n13 14 Nil n10
n10 = Node Nil 10 n14 n8
n8 = Node n3 8 n10 Nil

test1 = TestCase $ assertEqual "n8" (next n8) n10

tests = TestList [TestLabel "n8" test1]
