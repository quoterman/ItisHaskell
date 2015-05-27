module Task1 where

data Tree = Nil | Node Int Tree Tree deriving Show
--interp. int binary search tree

height :: Tree -> Int
height Nil = -1
height (Node _ l r) = 1 + (max (height l) (height r))


sumT :: Tree -> Int
sumT Nil = 0
sumT (Node v l r) = v + (sumT l) + (sumT r)


findElem :: Tree -> Int -> Bool
findElem Nil _ = False
findElem (Node v l r) x
	| x == v = True
	| x < v = findElem l x
	| x > v = findElem r x




tree = Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil))
