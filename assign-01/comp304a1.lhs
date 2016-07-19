"COMP 304 Assignment 1"
 David Barnett (300313764)

Variations on a search
~~~~~~~~~~~~~~~~~~~~~~

A) count

Using pattern mathcing because it shows the different
cases the aligorithm will face and 

> count :: (Num a, Eq a) => a -> [a] -> a
> count _ [] = 0
> count n (x:xs)
>           | n == x = 1 + count n xs
>           | otherwise = count n xs


> main :: IO()
> main = do
>       print(count 1 [1,2,1,2,1] == 3)
