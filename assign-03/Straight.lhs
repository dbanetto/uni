Compiler and interpreter for simple straight line programs.

\section{Data types}

A straight line program is just a list of assignment statements, where an
expression can contain variables, integer constants and arithmetic operators.

> data Prog = Prog [Stmt]
>             deriving (Show)

While is added as an expression and a list of statements to always have the ability to
evaluate some value to check if the while should continue and the list of statements to
be able to wrap the necessary jump statements around the internal statements.

The If statement is similar to the While statement expect in how it uses an
expression to find out which branch it should be using and the list of statements
allow them to be wrapped with the necessary jump statements to give the correct behaviour.

> data Stmt = Asgn Var Exp
>      	    | While Exp [Stmt]
>      	    | If Exp [Stmt] [Stmt]
>             deriving (Show)

> data Exp = Const Val
>          | Var Char
>      	   | Bin Op Exp Exp
>      	   | ConNot Exp
>      	   | Con CondOp Exp Exp
>            deriving (Show)

> data Op = Plus | Minus | Times | Div
>           deriving (Show, Eq)

> data Val = Bool Bool | Int Int
>              deriving (Show, Eq)

> data CondOp = And | Or | Not
>               deriving (Show, Eq)

The store is a list of variable names and their values.  For now, a variable
now is just a single character and a value is an integer.

> type Store = [(Var, Val)]
> type Var = Char
> type Label = Int

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
> type Stack = [Val]

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
>       where (code, _) = transn stmts 0
> 
> transn :: [Stmt] -> Int -> (Code, Int)
> transn [] n = ([], n)
> transn (stmt:stmts) n = (t ++ rest, m)
>     where (t, m)    = trans' stmt  n
>           (rest, _) = transn stmts m
>
> trans' :: Stmt -> Int -> (Code, Int)
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
> transexp (Var v) = [Load v]
> transexp (Bin op e1 e2) = transexp e1 ++ transexp e2 ++ [BinOp op]
> transexp (ConNot exp) = (transexp exp) ++ [BoolOp Not]
> transexp (Con op e1 e2) = transexp e1 ++ transexp e2 ++ [BoolOp op]

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
>           | isBool x = cmds
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
> exec' (BinOp op)  (x:y:stack, store, p) = ((Int z):stack, store, p)
> 	where z = apply op (isInt x) (isInt y)
>
> exec' (BoolOp Not)  (x:stack, store, p) = ((Bool z):stack, store, p)
> 	where z = not (isBool x)
> exec' (BoolOp op)  (x:y:stack, store, p) = ((Bool z):stack, store, p)
> 	where z = apply op (isBool x) (isBool y)
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
>               | v == u = (v,(cmpType x w)):s
> 	     	    | otherwise = o:setVal v x s -- fix setVal losing values

\section{Type checking}

Checks if two values have the same types

> cmpType :: Val -> Val -> Val
> cmpType a@(Int _) (Int _) = a
> cmpType a@(Bool _) (Bool _) = a
> cmpType _ _ = error "mismatched types"

> isInt :: Val -> Int
> isInt (Int n) = n
> isInt _ = error "Is not integer"

> isBool :: Val -> Bool
> isBool (Bool n) = n
> isBool _ = error "Is not boolean"

\section{Testing}

\subsection{Programs}

Some examples for testing

> s1 = Asgn 'b' (Const (Bool True))
> s2 = Asgn 'a' (Const (Int 0))
> s3 = While (Var 'b') [Asgn 'a' (Bin Plus (Var 'a') (Const (Int 1))), While (Var 'b') [Asgn 'a' (Bin Plus (Var 'a') (Const (Int 1)))]]
> p1 = Prog [s1, s2, s3]

> testIfTrue = run (Prog [(Asgn 'a' (Const (Bool True))), If (Var 'a') [(Asgn 'a' (Const (Int 10)))] [(Asgn 'a' (Const (Int 0)))]]) == [('a', (Int 10))]
> testIfFalse = run (Prog [(Asgn 'a' (Const (Bool False))), If (Var 'a') [(Asgn 'a' (Const (Int 10)))] [(Asgn 'a' (Const (Int 0)))]]) == [('a', (Int 0))]

> main = do
>
>   putStr("Tests\n")
>   print(testIfTrue)
>   print(testIfFalse)
>
>   putStr("Test program\n")
>   print(translate p1)
>   print(run p1)
