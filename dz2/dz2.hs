module Dz where

fibb :: Int -> Int
fibb n
	| n == 0 = 0
	| otherwise = fibTail n 1 0
				  where fibTail n result previous = if n /= 0 
				  								    then fibTail (n - 1) ( result + previous ) result
					    						    else result

factorial :: Int -> Int
factorial n = factTail 1 n
			  where factTail result n = if n > 1
								   then factTail (result * n) (n - 1)
								   else result

akk :: Int -> Int -> Int
akk m n
   | m == 0 = n + 1
   | (m > 0) && (n == 0) = akk (m - 1) 1
   | (m > 0) && (n > 0) = akk (m - 1) (akk m (n - 1))
   | otherwise = error "Positive numbers expected"