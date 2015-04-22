module Dz where

fibbTail :: Int -> Int
fibbTail n
	| n == 0 = 0
	| otherwise = fibAux n 1 0
				  where fibAux n result previous = if n /= 0 
				  								   then fibAux (n - 1) ( result + previous ) result
					    						   else result

factorial :: Int -> Int
factorial n = helper 1 n
			  where helper acc n = if n > 1
								   then helper (acc * n) (n - 1)
								   else acc

akk :: Int -> Int -> Int
akk m n
   | m == 0 = n + 1
   | (m > 0) && (n == 0) = akk (m - 1) 1
   | (m > 0) && (n > 0) = akk (m - 1) (akk m (n - 1))
   | otherwise = error "Positive numbers expected"