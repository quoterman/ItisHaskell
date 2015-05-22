module Task1 where


mapl :: (a -> b) -> [a] -> [b]
mapl fn xs = foldl (\acc x -> acc ++ (fn x):[]) [] xs

mapr :: (a -> b) -> [a] -> [b]
mapr fn xs = foldr (\x acc -> (fn x):acc) [] xs

m = [1,2,3]
fn i = i+3

main =  do mapl fn m
--main =  do mapr fn a

