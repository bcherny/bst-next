{-# LANGUAGE GADTs, StandaloneDeriving #-}

module BST (next) where

----------------------- BST datatype -----------------------

data BST a where
  Nil :: BST a
  Node :: (Eq a, Ord a) => { left :: BST a
                           , val :: a
                           , right :: BST a
                           } -> (BST a)
deriving instance Eq a => Eq (BST a)
deriving instance Show a => Show (BST a)

----------------------- Utilities -----------------------

min :: BST a -> BST a
min Nil = Nil
min n@(Node Nil _ _) = n
min (Node l _ _) = BST.min l

-- Traverse up, until we find a parent node that has a value larger than a.
-- tree, node -> parent
bigParent :: BST a -> a -> BST a
bigParent Nil _ = Nil
bigParent t@(Node l ta r) a
  | a <= ta = t
  | a > ta = bigParent l a

-- tree, node -> parent of node
parent :: BST a -> BST a -> BST a
parent p@(Node l pa r) n@(Node _ a _)
  | n == l || n == r = p
  | a < pa = parent l n
  | otherwise = parent r n
parent _ _ = Nil

-- tree, node -> next node
next :: (Eq a) => BST a -> BST a -> BST a
next _ Nil = Nil
next _ (Node _ _ r@Node{}) = BST.min r
next t n
  | n == left p = p
  | otherwise = bigParent t $ val n
  where p = parent t n

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
