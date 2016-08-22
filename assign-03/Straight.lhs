Compiler and interpreter for simple straight line programs.

\section{Data types}

A straight line program is just a list of assignment statements, where an
expression can contain variables, integer constants and arithmetic operators.

> import qualified Data.List as L
> data Prog = Prog [Stmt]
>             deriving (Show)

While is added as an expression and a list of statements to always have the ability to
evaluate some value to check if the while should continue and the list of statements to
be able to wrap the necessary jump statements around the internal statements.

The If statement is similar to the While statement expect in how it uses an
expression to find out which branch it should be using and the list of statements
allow them to be wrapped with the necessary jump statements to give the correct behaviour.

> data Stmt = Decl [(Var, Type)]
>           | Asgn Var Exp
>      	    | While Exp [Stmt]
>      	    | If Exp [Stmt] [Stmt]
>             deriving (Show)

> data Exp = Const Val
>          | ConstB CBool
>          | Var Char
>      	   | Bin Op Exp Exp
>      	   | ConNot Exp
>      	   | Con CondOp Exp Exp
>            deriving (Show)

> data Op = Plus | Minus | Times | Div
>           deriving (Show, Eq)

> data Type = TBool | TInt
>             deriving (Show, Eq)

> data CBool = BTrue | BFalse
>              deriving (Show, Eq)

> data CondOp = And | Or | Not
>               deriving (Show, Eq)

The store is a list of variable names and their values.  For now, a variable
now is just a single character and a value is an integer.

> type Store = [(Var, Val)]
> type Var = Char
> type Label = Int
> type Val = Int

Straight line programs are translated into code for a simple stack-oriented
virtual machine.

A VM code program is a list of commands, where a command is either a load
immdiate (which loads a constant onto the stack), load (which loads the value
of a variable onto the stack), store (which saves the value at the top of the
stack into memory, and deletes it), or an arithmetic operation (which applies
an operations to the two values at the top of the stack and replaces them by
the result).

> type Code = [Command]
>
> data Command = LoadI Val
>              | Load  Var
>              | Store Var
>              | BinOp Op
>              | BoolOp CondOp
>              | Target Label  -- jump target
>              | Jump Label    -- jump to label target
>              | ConJump Label -- jump to label target if Var == 0
>               deriving (Show, Eq)
>
> type Stack = [Int]

Run a program, by compiling and then executing the resulting code
The program is run with an initially empty store and empty stack, and the
output is just the resulting store.  This could be modified to provide an
initial store.

\subsection{run}

> run :: Prog -> Store
> run prog = snd (exec code ([], [], code))
>      	     where code = translate prog
>      	           snd (_, s, _) = s

\section{Translate}

Translate straight line program into stack machine code

> translate :: Prog -> Code
> translate (Prog stmts) = trans stmts

To ensure that every label has a unique label each statement to be translated
will be allocated 2 labels to be unique to it by incrementing the label counter `n`.
Though not all statements need to have a unique label there is no cost expect for incrementing
the `n` variable. Another way of implementing this would be to return the value of `n` form
`trans'` functions and allowing them to update the counter as needed. Though I think that way
makes for much more plumbing code to make sure the next translation gets the new label counter.

> trans :: [Stmt] -> Code
> trans [] = []
> trans stmts = code
>       where checked = check stmts
>             (code, _) = transn checked 0
>
> transn :: [Stmt] -> Int -> (Code, Int)
> transn [] n = ([], n)
> transn (stmt:stmts) n = (t ++ rest, m)
>     where (t, m)    = trans' stmt  n
>           (rest, _) = transn stmts m
>
> trans' :: Stmt -> Int -> (Code, Int)

The `Decl` statement declares the list of variables with their given type and set them with a
zero value (so Int's = 0, and Bools = False).

> trans' (Decl vars) n = (transvars vars [], n)
>       where transvars [] vs = vs
>             transvars ((v, _):vv) vs = transvars vv ([LoadI 0, Store v] ++ vs)



> trans' (Asgn var exp) n = ((transexp exp) ++ [Store var], n)
> trans' (While exp loop) n = (cmds, label)
>       where cmds = [Target start] ++ (transexp exp) ++ [ConJump end] ++ loopstmts ++ [Jump start, Target end]
>             (loopstmts, label) = transn loop (n + 2)
>             end   = n
>             start = n + 1
> trans' (If exp succ fail) n = (cmds, label )
>        where cmds =  (transexp exp) ++ [ConJump lfail] ++ truetmt ++ [Jump end, Target lfail] ++ falsestmt  ++ [Target end]
>              (truetmt, n1) = transn succ (n + 2)
>              (falsestmt, label) = transn fail n1
>              end   = n
>              lfail = n + 1
>
>
> transexp :: Exp -> Code
> transexp (Const n) = [LoadI n]
> transexp (ConstB n) = [LoadI (x n) ]
>           where x BTrue = 1
>                 x BFalse = 0
> transexp (Var v) = [Load v]
> transexp (Bin op e1 e2) = transexp e1 ++ transexp e2 ++ [BinOp op]
> transexp (ConNot exp) = (transexp exp) ++ [BoolOp Not]
> transexp (Con op e1 e2) = transexp e1 ++ transexp e2 ++ [BoolOp op]

\section{check}

This section does static checking on the static constraints of the language.
These include only assigning a variable once, only having one type for a variable
operators being constricted to their types and not being able to declare a variable twice.
This

> type TStore = [(Var, Type)]
> check :: [Stmt] -> [Stmt]
> check [] = []
> check stmts
>       | c (checks stmts []) = stmts
>       | otherwise = error "failed static check"
>       where c (True, ss) = seq ss True
>             c _ =  False
>
> checks :: [Stmt] -> TStore -> (Bool, TStore)
> checks [] ss = (length ss > 0, ss)
> checks ((Decl vars):ss) store = (res, sstore)
>          where sstore = addVar vars store
>                (res, _) = checks ss sstore
>                addVar [] ts = ts
>                addVar ((v, t):vs) ts = addVar vs (setValT v t ts)
> checks ((Asgn v e):ss) store = (has && res, store)
>       where has
>               | (getValT v store) == (checkexp e store) = True
>               | otherwise = error ("assignment of variable '" ++ [v] ++ "' does not match type")
>             (res, _) = checks ss store
> checks ((While e loop ):ss) store = (exp && t && res, sstore)
>       where exp
>               | TBool == (checkexp e store) = True
>               | otherwise = error "while loop expression not a bool"
>             (t, sstore) = checks loop store
>             (res, _) =  checks ss sstore
> checks ((If e succ fail):ss) store = ((exp && suc && fai && res), sstore)
>           where exp
>                   | TBool == (checkexp e store) = True
>                   | otherwise = error "if expression not bool"
>                 (suc, sst) = checks succ store
>                 (fai, fst) = checks fail store
>                 sstore = sst `L.union` fst
>                 (res, _) = checks ss store

The `checkexp` function asserts that the types used in the sub-expression
are correct and also checks if the variable has been declared before hand and
if it is the correct type.

> checkexp :: Exp -> TStore -> Type
> checkexp (Const v) _ = TInt
> checkexp (ConstB v) _ = TBool
> checkexp (Var v) store
>           | hasValT v store = getValT v store
>           | otherwise      = error "used variable before assignment"
> checkexp (Bin _ ex1 ex2) s = cmpType (cmpType (checkexp ex1 s) (TInt)) (checkexp ex2 s)
> checkexp (ConNot ex1) s    = cmpType (checkexp ex1 s) (TBool)
> checkexp (Con _ ex1 ex2) s = cmpType (cmpType (checkexp ex1 s) (TBool)) (checkexp ex2 s)

\subsection{VM Code Execute}

Execute a stack code program

> exec :: Code -> (Stack, Store, Code) -> (Stack, Store, Code)
> exec [] ss = ss

The Jump statement enables movement around the Code and is required in
exec because it has control over what is the next command to be executed in
the list. Another option to implement the jump would be to split the jump into
forward and back jumps to allow for the optimised case of foward jumps of just dropping from
the current position and make a backwards jump use the ineffiecent way of looking from the
start of the VM code. Run time errors are included to ensure correct execution in case
of bugs in translate.

> exec ((Jump l):_) ss@(_, _, prog) = exec next ss
>   where nx = dropWhile (\ c -> c /= (Target l)) prog
>         next
>           | nx == [] = error "Label not found"
>           | otherwise = tail nx

Conditional jump is implemented using the same mechainics as the Jump statement.
This was done for ease of use. The Conditional jump relies on there being a value on
the stack to be consumed as the compared value. The Conditional jump (when using Int's as
base) will jump if the value is not equal to 0. Run time errors are included to ensure correct execution in case
of bugs in translate.

> exec ((ConJump l) : _) ([], _, _) = error "Conditional jump requires an element on the stack"
> exec ((ConJump l) : cmds) (x:stack, store, prog) = exec next (stack, store, prog)
>   where nx = dropWhile (\ c -> c /= (Target l)) prog
>         next
>           | toBool x = cmds
>           | nx == [] = error "Label not found"
>           | otherwise = tail nx
>
>
> exec (cmd : cmds) ss = exec cmds (exec' cmd ss)
>

> exec' :: Command -> (Stack, Store, Code) -> (Stack, Store, Code)
> exec' (LoadI n) (stack, store, p) = (n:stack, store, p)
> exec' (Load v) (stack, store, p) = (x:stack, store, p)
> 	where x = getVal v store
> exec' (Store v) (x:stack, store, p) = (stack, store', p)
> 	where store' = setVal v x store
>
> exec' (BinOp op)  (x:y:stack, store, p) = (z:stack, store, p)
> 	where z = apply op x y

Since the stack is a list of Int's to complete boolean operations
the integer values would need to be converted to booleans and back
into integers after completing the calculation. An alternative would
to change the stack to have an ADT representing different types
such as Int's and Bool and hold the real values as they are in Haskell,
however I chose to keep everything as Int's because it makes the VM code layer
to be closer representation of actual compilers.

> exec' (BoolOp Not)  (x:stack, store, p) = (z:stack, store, p)
> 	where z = fromBool (not (toBool x))
> exec' (BoolOp op)  (x:y:stack, store, p) = (z:stack, store, p)
> 	where z = fromBool (apply op (toBool x) (toBool y))
> 	      apply And a b = a && b
> 	      apply Or  a b = a || b

Target is an NOOP here because it is cleaner to handle it in exec' than exec

> exec' (Target _) ss = ss

\subsection{Arithmetic operators}

Apply an arithmetic operator

> apply :: Op -> Int -> Int -> Int
> apply Plus x y = x + y
> apply Minus x y = x - y
> apply Times x y = x * y
> apply Div x y = x `div` y

\section{Store}

\subsection{getVal}

Look up a variable in the store

> getVal :: Var -> Store -> Val
> getVal v s = foldr (\(u,x) r -> if u == v then x else r) (error "Variable not found") s

\subsection{setVal}

Assign a value to a variable in the store

> setVal :: Var -> Val -> Store -> Store
> setVal v x [] = [(v,x)]
> setVal v x (o@(u,w):s)
>               | v == u = (v,x):s
> 	     	    | otherwise = (o:(setVal v x s)) -- fix setVal losing values

\subsection{hasVal}

> hasVal :: Var -> Store -> Bool
> hasVal _ [] = False
> hasVal a ((x, _):xs)
>           | a == x = True
>           | otherwise = False

\section{Type checking}

Checks if two values have the same types
and returns the type. This is useful for
asserting the type of an expression.

> cmpType :: Type -> Type -> Type
> cmpType a b
>       | a == b = a
>       | otherwise = error "mismatched types"

Utility functions to handle VM bools into haskell bool's
for ease of use.

> toBool :: Int -> Bool
> toBool n
>       | n == 0 = False
>       | otherwise = True

> fromBool :: Bool -> Int
> fromBool n
>       | False = 0
>       | otherwise = 1

\subsection{Type Store methods}

Type Store is a collection used in the static checking portion
of the translation to keep track the type of a variable during the
program's reasoning. Hard errors are used in failure cases
because the plumping to make them into `Maybe`'s is a bit too
complicated for me.

> hasValT :: Var -> TStore -> Bool
> hasValT _ [] = False
> hasValT v ((x, _):xs)
>           | v == x = True
>           | otherwise = hasValT v xs

> getValT :: Var -> TStore -> Type
> getValT _ [] = error "does not exist"
> getValT v ((x, t):xs)
>           | v == x = t
>           | otherwise = getValT v xs

> setValT :: Var -> Type -> TStore -> TStore
> setValT v t [] = [(v, t)]
> setValT v t (o@(x, _):xs)
>           | v == x = error "already declared"
>           | otherwise = (o:(setValT v t xs))

\section{Testing}

\subsection{Programs}

Some examples for testing

> s0 = Decl [('a', TInt), ('b', TBool)]
> s1 = Asgn 'b' (ConstB BFalse)
> s2 = Asgn 'a' (Const 0)
> s3 = While (Var 'b') [Asgn 'a' (Bin Plus (Var 'a') (Const 1)), While (Var 'b') [Asgn 'a' (Bin Plus (Var 'a') (Const 1))]]
> p1 = Prog [s0, s1, s2, s3]

> testIfTrue    = run (Prog [(Decl [('a', TInt), ('b', TBool)]), (Asgn 'b' (ConstB BTrue)), If (Var 'b') [(Asgn 'a' (Const 10))] [(Asgn 'a' (Const 0))]]) == [('b', 1), ('a', 10)]
> testIfFalse   = run (Prog [(Decl [('a', TInt), ('b', TBool)]), (Asgn 'b' (ConstB BFalse)), If (Var 'b') [(Asgn 'a' (Const 10))] [(Asgn 'a' (Const 0))]]) == [('b', 0), ('a', 0)]

Static checks tests

> useBeforeDecl = checks [(Asgn 'a' (Const 0))] [] == (False, [])
> assgnVar      = checks [(Decl [('a', TInt)]), (Asgn 'a' (Const 0))] []  == (True, [('a', TInt)])
> misAssgnVar   = checks [(Decl [('a', TBool)]), (Asgn 'a' (Const 0))] [] == (False, [('a', TBool)])

> main = do
>
>   putStr("Tests\n")
>   print(testIfTrue)
>   print(testIfFalse)
>   -- print(useBeforeDecl) -- cannot be run because it rightfully error
>   print(assgnVar) --
>   print(misAssgnVar) --
>
>   putStr("\n\nTest program\n")
>   print(translate p1)
>   print(run p1)
