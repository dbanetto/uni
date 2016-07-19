"COMP 304 Assignment 1"
 David Barnett (300313764)

1. Variations on a search
~~~~~~~~~~~~~~~~~~~~~~

A) count

Using pattern mathcing because it shows the different
cases the aligorithm will face 

> count :: (Num a, Eq a) => a -> [a] -> a
> count _ [] = 0
> count n (x:xs)
>           | n == x = 1 + count n xs
>           | otherwise = count n xs

An alternative method would be to use if/else blocks
instead of mathcing on values

B) allPos

Using pattern mathcing because it shows the different
cases the aligorithm will face. A sub function
was used for the recursion to keep track of the index in
the array.

> allPos :: (Num a, Eq a) => a -> [a] -> [a]
> allPos n arr = (allPosI n arr 1) 
>
> -- n - is the number we are looking for
> -- arr - is the reminding array we are checking
> -- i - is the current index in the array 
> -- returns an array of in where n was found in the array
> allPosI :: (Num a, Eq a) => a -> [a] -> a -> [a]
> allPosI _ [] _ = []
> allPosI n (x:xs) i
>               | n == x = [i] ++ allPosI n xs (i+1) 
>               | otherwise = allPosI n xs (i+1) 


An alternative method would be to use if/else blocks
instead of mathcing on values or use a standard 
list function.

C) firstLastPos

This method breaks down the problem into two sub-problems of
finding the 1st and last elemnts seperatly for ease of mind.

> -- n - is the number we are looking for
> -- arr - is the reminding array we are checking
> -- returns a tuple of the first and last indices where we saw n
> firstLastPos :: (Num a, Eq a) => a -> [a] -> (a, a)
> firstLastPos _ [] = (0,0)
> firstLastPos n arr = (firstPos n arr 1, lastPos n arr 1 0)
>
> -- n - is the number we are looking for
> -- arr - is the reminding array we are checking
> -- i - is the current index in the array 
> -- returns the first index of the 1st instance of an element equalling n
> firstPos :: (Num a, Eq a) => a -> [a] -> a -> a
> firstPos _ [] _ = 0
> firstPos n (x:xs) i
>               | n == x = i
>               | otherwise = firstPos n xs (i+1)
>
> -- n - is the number we are looking for
> -- arr - is the reminding array we are checking
> -- i - is the current index in the array 
> -- h - is the highest index we saw n being equal to
> -- returns the last index of the 1st instance of an element equalling n
> lastPos :: (Num a, Eq a) => a -> [a] -> a -> a -> a
> lastPos _ [] _ h = h
> lastPos n (x:xs) i h
>               | n == x = lastPos n xs (i+1) i
>               | otherwise = lastPos n xs (i+1) h

An alternative method with be to join lastPos and firstPos together
but at the cost of doubling the number of cases to match against and greatly
increasing the complexity of the function.

Examples and testing
~~~~~~~~~~~~~~~~~~~

> main :: IO()
> main = do
>       -- Count Examples
>       print("count 1 [1,2,1,2,1] == 3")
>       print(count 1 [1,2,1,2,1])
>
>       print("count 5 [1,2,1,2,1] == 0")
>       print(count 5 [1,2,1,2,1])
>
>       print("count 5 [] == 0")
>       print(count 5 [])
>
>       -- allPos Examples
>       print("allPos 1 [1,2,1,2,1] == [1,3,5]")
>       print(allPos 1 [1,2,1,2,1])
>
>       print("allPos 3 [1,2,1] == []")
>       print(allPos 3 [1,2,1])
>
>       -- firstLastPos Examples
>       print("firstLastPos 1 [1,2,1,2,1] == (1,5)")
>       print(firstLastPos 1 [1,2,1,2,1])
>
>       print("firstLastPos 2 [1,2,1,2,1] == (2,4)")
>       print(firstLastPos 2 [1,2,1,2,1])
>
>       print("firstLastPos 5 [1,2,1,2,1] == (0,0)")
>       print(firstLastPos 5 [1,2,1,2,1])
>
