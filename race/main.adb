with Fib;
with Ada.Integer_Text_IO;
with credit;

procedure main is
   input : Integer;
begin
   Ada.Integer_Text_IO.Get(input);
   Ada.Integer_Text_IO.Put(Integer(Fib.fib(input)));
end main;
