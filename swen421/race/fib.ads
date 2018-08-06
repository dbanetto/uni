package Fib is
   type Nat is range 1..Integer'Last;

   function fib( X : Integer) return Nat;
end Fib;
