with Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

package body Exercise with
     Spark_Mode is

   function InArray
     (value : Integer;
      input : IntArray) return Boolean is
     (for some i in input'Range => input (i) = value);

   function DeleteValue
     (value :    Integer;
      input : in IntArray) return IntArray
   is
   begin
      for i in input'Range loop
         if input (i) = value then
            return input (input'First .. i - 1) &
              DeleteValue (value, input (i + 1 .. input'Last));
         end if;
      end loop;

      return input;
   end DeleteValue;

   function FirstIndexOf (value : Integer; input : IntArray) return Integer is
   begin
      for i in input'Range loop
         if input(i) = value then
           return i;
           end if;
      end loop;

      return 0;
   end FirstIndexOf;


   function ContcatArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray is
     (a & b);

   function MergeArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray
   is
      aIndex : Integer  := a'First;
      bIndex : Integer  := b'First;
      c      : IntArray := IntArray (a) & IntArray (b);
      cIndex : Integer  := c'First;
   begin

      loop
         exit when aIndex > a'Last and bIndex > b'Last;

         if aIndex > a'Last then
            -- Completed merging A, just need to tack on the rest of B
            c (cIndex) := b (bIndex);
            cIndex     := cIndex + 1;
            bIndex     := bIndex + 1;
         elsif bIndex > b'Last then
            -- Completed merging B, just need to tack on the rest of A
            c (cIndex) := a (aIndex);
            cIndex     := cIndex + 1;
            aIndex     := aIndex + 1;
         else
            -- There is some left in A & B
            -- need to compare to see which is the lesser to keep ordering
            if a (aIndex) <= b (bIndex) then
               c (cIndex) := a (aIndex);
               cIndex     := cIndex + 1;
               aIndex     := aIndex + 1;
            else
               c (cIndex) := b (bIndex);
               cIndex     := cIndex + 1;
               bIndex     := bIndex + 1;
            end if;
         end if;
      end loop;

      return OrderedIntArray (c);
   end MergeArray;

end Exercise;
