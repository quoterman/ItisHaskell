module Task2 where


--filter
filterR :: (a -> Bool) -> [a] -> [a]
filterR _ [] = []
filterR fn (x:xs) = if (fn x)
                       then x : filterR fn xs
                   else
                       filterR fn xs


filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr _ [] = []
filterFoldr fn (x:xs) = foldr (\x xs -> if fn x then x:xs else xs ) [] xs


filterFoldl :: (a -> Bool) -> [a] -> [a]
filterFoldl _ [] = []
filterFoldl fn (x:xs) = foldl (\xs x -> if fn x then xs ++ x:[] else xs) [] xs


--concat
concatR :: [[a]] -> [a]
concatR [] = []
concatR (x:xs) = x ++ concatR xs

concatFoldr :: [[a]] -> [a]
concatFoldr [] = []
concatFoldr xxs = foldr (++) [] xxs

concatFoldl :: [[a]] -> [a]
concatFoldl [] = []
concatFoldl xxs = foldl (++) [] xxs


--concatMap
concatMapR :: (a -> [b]) -> [a] -> [b]
concatMapR _ [] = []
concatMapR fn (x:xs) = fn x ++ concatMapR fn xs

concatMapFoldl :: (a -> [b]) -> [a] -> [b]
concatMapFoldl _ [] = []
concatMapFoldl fn xxs = foldl (\xs x -> xs ++ (fn x)) [] xxs

concatMapFoldr :: (a -> [b]) -> [a] -> [b]
concatMapFoldr _ [] = []
concatMapFoldr fn xxs = foldr (\x xs -> (fn x) ++ xs) [] xxs