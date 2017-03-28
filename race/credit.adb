package body credit with SPARK_Mode is


   function isValid(input: columns) return Boolean is
      sum : Integer := 0;
      second : Boolean := false;
   begin

      for i in input'Range loop
         if second then
            sum := sum + ((input(i) * 2) mod 10);
         else
            sum := sum + input(i);
         end if;
      end loop;

      return sum mod 10 = 0;
   end isValid;


end credit;
