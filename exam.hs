evenSum :: [Int] -> [Int]
evenSum [] = []
evenSum [x] = [x]
evenSum (x:xs:xss) = (x+xs) : evenSum xss

