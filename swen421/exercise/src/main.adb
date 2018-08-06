with Exercise;
with Ada.Assertions;
use Ada.Assertions;
use Exercise;

procedure Main is
   a : Exercise.IntArray(1 .. 10) := (1 ,2 ,3 ,4 ,5 ,6 ,5 ,8, 9, 5);
   b : Exercise.IntArray(0.. 10) := (others => 0);
   c : Exercise.OrderedIntArray(1..3) := (others => 0);
   d : Exercise.OrderedIntArray(1..3) := (others => 1);
   e : Exercise.OrderedIntArray(1..3) := (1, 3 ,5);
   f : Exercise.OrderedIntArray(1..3) := (2, 4 ,6);
begin
   --  Insert code here.
   Assert( Exercise.InArray(4, a));

   Assert( Exercise.DeleteValue(5, a) = (1 ,2 ,3 ,4 ,6 ,8, 9));

   Assert(Exercise.ContcatArray(c, d) = (0,0,0,1,1,1));

   Assert(Exercise.MergeArray(c, c) = (0,0,0,0,0,0));

   Assert(Exercise.MergeArray(e, f) = (1,2,3,4,5,6));
end Main;
