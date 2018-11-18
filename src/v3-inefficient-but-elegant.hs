{-# LANGUAGE GADTs, StandaloneDeriving #-}

module BST (next) where

import Data.List (elemIndex)

----------------------- BST datatype -----------------------

data BST a where
  Nil :: BST a
  Node :: (Eq a, Ord a) => BST a -> a -> BST a -> BST a
deriving instance Eq a => Eq (BST a)
deriving instance Show a => Show (BST a)

----------------------- Utilities -----------------------

inorder :: BST a -> [BST a]
inorder Nil = []
inorder n@(Node l _ r) = inorder l ++ [n] ++ inorder r

next :: Eq a => BST a -> BST a -> BST a
next t a = case elemIndex a l of
  Just n -> l!! (succ n)
  Nothing -> Nil
  where l = inorder t

----------------------- Tests -----------------------

--              8
--           /    \
--         3       10
--       /  \       \
--     1     6       14
--         /  \     /
--       4     7  13

n1 = Node Nil 1 Nil
n4 = Node Nil 4 Nil
n7 = Node Nil 7 Nil
n6 = Node n4 6 n7
n3 = Node n1 3 n6
n13 = Node Nil 13 Nil
n14 = Node n13 14 Nil
n10 = Node Nil 10 n14
n8 = Node n3 8 n10

test = do
  assert (next n8 n1) n3
  assert (next n8 n3) n4
  assert (next n8 n4) n6
  assert (next n8 n6) n7
  assert (next n8 n7) n8
  assert (next n8 n8) n10
  assert (next n8 n10) n13
  assert (next n8 n13) n14

assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected = case actual == expected of
  True -> putStrLn $ "OK " ++ show expected ++ " is " ++ show actual
  False -> putStrLn $ "FAIL " ++ show expected ++ " is " ++ show actual
