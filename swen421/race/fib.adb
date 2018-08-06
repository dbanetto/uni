package body Fib is

function Fib ( X : Integer ) return Nat is
   a : Integer;
   b : Integer;
   c : Integer;
begin

   a := 1;
   b := 0;
   for i in 0..X loop
    c := a;
    a := a + b;
    b := c;
   end loop;

   return Nat(a);
end Fib;


end Fib;
