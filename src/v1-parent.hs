{-# LANGUAGE GADTs #-}

module BST (next) where

----------------------- BST datatype -----------------------

data BST a where
  Nil :: BST a
  Node :: (Eq a, Ord a) => { left :: BST a
                           , val :: a
                           , right :: BST a
                           , parent :: BST a
                           } -> (BST a)

instance Eq (BST a) where
  Nil == Nil = True
  (Node _ a _ _) == (Node _ b _ _) = a == b
  _ == _ = False

instance (Show a) => Show (BST a) where
  show Nil = ""
  show (Node l v r _) = "(" ++ show v ++ ")"

----------------------- Utilities -----------------------

min :: BST a -> BST a
min Nil = Nil
min n@(Node Nil _ _ _) = n
min (Node l _ _ _) = BST.min l

-- Traverse up, until we find a parent node that has a value larger than a.
bigParent :: a -> BST a -> BST a
bigParent _ Nil = Nil
bigParent a p@(Node _ pv _ pp)
  | pv > a = p
  | otherwise = bigParent a pp

next :: BST a -> BST a
next Nil = Nil
next (Node _ _ r@Node{} _) = BST.min r
next n@(Node _ a _ p)
  | n == left p = p
  | otherwise = bigParent a p

----------------------- Tests -----------------------

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

test = do
  assert (next n1) n3
  assert (next n3) n4
  assert (next n4) n6
  assert (next n6) n7
  assert (next n7) n8
  assert (next n8) n10
  assert (next n10) n13
  assert (next n13) n14

assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected = case actual == expected of
  True -> putStrLn $ "OK " ++ show expected ++ " is " ++ show actual
  False -> putStrLn $ "FAIL " ++ show expected ++ " is " ++ show actual
