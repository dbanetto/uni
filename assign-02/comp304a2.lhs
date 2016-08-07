% David Barnett (300313264)
% COMP304 Assignment 2

\section{1. Binary Trees}

> data BinTree a = Empty | Node a (BinTree a) (BinTree a)
>              deriving (Show)

\subsection{a. hasbt}


> hasbt :: (Eq a) => a -> BinTree a -> Bool
> hasbt _ Empty = False
> hasbt a (Node x l r)
>       | a == x = True
>       | otherwise = hasbt a l || hasbt a r

\subsection{b. equalbt}

> equalbt :: (Eq a) => BinTree a -> BinTree a -> Bool
> equalbt Empty Empty = True
> equalbt (Node _ _ _) Empty = False
> equalbt Empty (Node _ _ _) = False
> equalbt (Node lx ll lr) (Node rx rl rr)
>       | lx == rx = equalbt ll rl && equalbt lr rr
>       | otherwise = False


\subsection{c. reflectbt}

> reflectbt :: BinTree a -> BinTree a
> reflectbt Empty = Empty
> reflectbt (Node x l r) = Node x (reflectbt r) (reflectbt l)

\subsection{d. fringebt}

> fringebt :: BinTree a -> [a]
> fringebt Empty = []
> fringebt (Node a Empty Empty) = [a]
> fringebt (Node _ l Empty) = fringebt l
> fringebt (Node _ Empty r) = fringebt r
> fringebt (Node _ l r) = fringebt l ++ fringebt r


\subsection{e. fullbt}

> fullbt :: BinTree a -> Bool
> fullbt Empty = True
> fullbt (Node _ Empty (Node _ _ _)) = False
> fullbt (Node _ (Node _ _ _) Empty) = False
> fullbt (Node _ l r) = fullbt l && fullbt r

\section{2. Binary tree folds}

\subsection{a. btfold}

> btfold :: (a -> b -> b -> b) -> b -> BinTree a -> b
> btfold _ u Empty = u
> btfold f u (Node a Empty Empty) = f a u u
> btfold f u (Node a l Empty) = f a (btfold f u l) u
> btfold f u (Node a Empty r) = f a u (btfold f u r)
> btfold f u (Node a l r) = f a (btfold f u l) (btfold f u r)

\subsection{b. Question 1 using btfold}

\subsubsection{hasbtf}

> hasbtf :: (Eq a) => a -> BinTree a -> Bool
> hasbtf x t = btfold (\u v w -> x == u || v || w) False t

\subsubsection{equalbtf}

This is not possible to implement using `btfold` as it requires traversing
both BinTrees in step lock. An alternative fold function to implement
this behaviour would be quite spesific with each and would almost be
just the same as using a higher order function to equate elements in
the two trees, such as:

> equalbtff :: (a -> a -> Bool) -> BinTree a -> BinTree a -> Bool
> equalbtff _ Empty Empty = True
> equalbtff _ (Node _ _ _) Empty = False
> equalbtff _ Empty (Node _ _ _) = False
> equalbtff f (Node lx ll lr) (Node rx rl rr)
>       | f lx rx = equalbtff f ll rl && equalbtff f lr rr
>       | otherwise = False

\subsubsection{reflectbtf}

> reflectbtf :: BinTree a -> BinTree a
> reflectbtf t = btfold (\u v w -> Node u w v) Empty t

\subsubsection{fringebtf}

> fringebtf :: BinTree a -> [a]
> fringebtf t = btfold (\u v w -> if length v == 0 && length w == 0 then [u] else v ++ w) [] t

Would of thought to use `v == [] && w == []` instead but this introdued
the need for the `Eq` type class so chose to use lenght of the arrays to
keep the same type contraints as the non-fold version of the function.

\subsubsection{fullbtf}

Using the ordering data type because it has 3 enumations.
With the 3 enumations we can check for difference between
the unit/ empty sub-trees, a full subtree and a not full subtree
which bubbles all the way up the fold.

> fullbtf :: BinTree a -> Bool
> fullbtf Empty = True
> fullbtf t = (btfold ffold LT t) == EQ
>           where
>               ffold _ LT LT = EQ
>               ffold _ EQ EQ = EQ
>               ffold _ _ _ = GT


\section{3. Binary Search Trees}

\subsection{empty}

> empty :: (Ord a, Eq a) => BinTree a
> empty = Empty

\subsection{insert}

> insert :: (Ord a, Eq a) => a -> BinTree a -> BinTree a
> insert x Empty = (Node x Empty Empty)
> insert x (Node a l r)
>   | x > a     = Node a l (insert x r)
>   | x == a    = Node a l r
>   | otherwise = Node a (insert x l) r

\subsection{has}

> has :: (Ord a, Eq a) => a -> BinTree a -> Bool
> has _ Empty = False
> has x (Node a l r)
>   | x > a     = has x r
>   | x == a    = True 
>   | otherwise = has x l

\subsection{delete}

> delete :: (Ord a, Eq a) => a -> BinTree a -> BinTree a
> delete _ Empty = Empty
> delete x (Node a Empty Empty)
>       | x == a = Empty
>       | otherwise = Node a Empty Empty
> delete x (Node a l Empty)
>       | x == a = l
>       | x < a = Node a (delete x l) Empty
>       | otherwise = Node a l Empty
> delete x (Node a Empty r)
>       | x == a = r
>       | x > a = Node a Empty (delete x r)
>       | otherwise = Node a Empty r
> delete x (Node a l r)
>       | x == a = Node (smallest r) l (delete (smallest r) r)
>       | x > a = Node a (delete x l) r
>       | x < a = Node a l (delete x r)
>       where 
>           smallest (Node a Empty _) = a 
>           smallest (Node a l _) = smallest l

\subsection{flatten}

Flattens the tree in descending order

> flatten :: BinTree a -> [a]
> flatten Empty = []
> flatten (Node a l r) = (flatten l) ++ [a] ++ (flatten r)

\subsection{equals}

Uses the implementation detail of flatten that it is in
descending order so even if the BST's are inserted in different
orders the flatten arrays will be the same so we can equate on them
for simplicity.

> equals :: (Eq a) => BinTree a -> BinTree a -> Bool
> equals a b = (flatten a) == (flatten b)

\section{Graph Algorithms}

> type Graph a = [(a, Int, a)]

\subsubsection{reachable}

\subsubsection{cliques}

\section{Examples}

\subsection{Tests}

> testhasbtEmpty  = hasbt 1 empty == False
> testhasbtHasTop = hasbt 1 (insert 1 empty) == True
> testhasbtHas2nd = hasbt 1 (insert 1 (insert 2 empty)) == True

equalbtEmpty   = equalbt empty empty == True

> testequalbtFilled1 = equalbt empty (insert 1 empty) == False
> testequalbtFilled2 = equalbt (insert 1 empty) empty == False

> testreflectEmpty = reflectbt empty == empty

> main = do
>  putStrLn("hasbt Test")
>  print(testhasbtEmpty)
>  print(testhasbtHasTop)
>  print(testhasbtHas2nd)
>
>  putStrLn("equalbt Test")
>  print(testequalbtFilled1)
>  print(testequalbtFilled2)
