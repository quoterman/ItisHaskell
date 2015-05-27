module Task2 where

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show
--interp. 'a' type binary search tree

height :: Tree a -> Int
height Nil = -1
height (Node _ l r) = 1 + (max (height l) (height r))


tmap :: (a -> b) -> Tree a -> Tree b
tmap fn tr = case tr of
                Nil -> Nil
                Node v l r -> Node (fn v) (tmap fn l) (tmap fn r)


intToX :: Integer -> Char
intToX i = 'X'


tree = Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil))

--main = height tree
--main = do tmap intToX tree
