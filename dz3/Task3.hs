module Task3 where

data List a b = Nil | Cons a (List b a)  deriving (Show, Eq)
-- interp.

list = Cons "Hello" (Cons 5 (Cons "World" (Cons 15 Nil)))

lab_length :: List a b -> Int
lab_length lab = case lab of
                  Nil -> 0
                  Cons _ tl -> 1 + (lab_length tl)

dmap :: (a -> c) -> (b -> d) -> List a b -> List c d
dmap fn1 fn2 lod = case lod of
            Nil -> Nil
            Cons hd tl -> Cons (fn1 hd) (dmap fn2 fn1 tl)

fun1 :: String -> Integer
fun1 i = 1

fun2 :: Integer -> Char
fun2 i = 'X'

--main = do lab_length list
main = do dmap fun1 fun2 list