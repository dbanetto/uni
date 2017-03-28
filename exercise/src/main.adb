with Exercise;
with Ada.Assertions; use Ada.Assertions;

procedure Main is
   a : Exercise.IntArray(1 .. 10) := (1 ,2 ,3 ,4 ,5 ,6 ,7 ,8, 9, 10);
   b : Exercise.IntArray(0..0) := (others => 0);
begin
   --  Insert code here.
   Assert( Exercise.InArray(4, a));

   Exercise.DeleteValue(5, a ,b);
end Main;
