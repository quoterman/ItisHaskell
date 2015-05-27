
evenSum :: Integral a => [a] -> a

evenSum l = accumSum 0 l
    where accumSum n l = 
            if l == []
                then n
                else let x = head l 
                         xs = tail l 
                         t = x + xs
                         n:t