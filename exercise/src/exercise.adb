with Ada.Assertions; use Ada.Assertions;

package body Exercise with Spark_Mode is
   function InArray (value : Integer; input : IntArray) return Boolean is
   begin
      for i in input'Range loop
         if input(i) = value then
            return True;
         end if;
      end loop;
      return False;
   end InArray;

   procedure DeleteValue
     (value  :        Integer;
      input  : in     IntArray;
      output :    out IntArray) is

      index : Integer;
      count : Integer := 0;
   begin

      for i in input'Range loop
         if input(i) = value then
            if count = Integer'Last then
               exit;
            end if;
            count := count + 1;
         end if;
      end loop;

      Assert( count <= input'Length );

      output := input(input'First .. (input'Last - count));
      index := output'First;

      if output'Length = 0 then
         return;
      end if;

      for i in input'Range loop
         pragma Loop_Invariant (index in output'Range);

         if input(i) /= value then
            output(index) := input(i);
            index := Integer'Succ(index);
         end if;
      end loop;
   end DeleteValue;



   function ContcatArray ( a : IntArray ; b : IntArray) return IntArray is
      output : IntArray := a & b;
   begin
      return output;
   end ContcatArray;
end Exercise;
