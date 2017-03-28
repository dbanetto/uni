package Exercise with
     Spark_Mode is

   type IntArray is array (Integer range <>) of Integer;

   function InArray (value : Integer; input : IntArray) return Boolean with
      Post => InArray'Result =
      (for some k in input'Range => value = input (k));

   procedure DeleteValue
     (value  :        Integer;
      input  : in     IntArray;
      output :    out IntArray) with
      Pre => input'Length >= 1,
     Post => input'Length >= output'Length and
             (for all i in output'Range => output (i) /= value);

   function ContcatArray (a : IntArray; b : IntArray) return IntArray with
      Pre =>
       (a'Length >= 1 and b'Length >= 1 and
        (for all i in a'First .. a'Last - 1 => a (i) <= a (i + 1)) and
        (for all i in b'First .. b'Last - 1 => b (i) <= b (i + 1)) and
        a (a'Last) <= b (b'First)

      ),

      Post => ContcatArray'Result'Length = a'Length + b'Length and
      (for all i in
         ContcatArray'Result'First .. ContcatArray'Result'Last - 1 =>
         ContcatArray'Result (i) <= ContcatArray'Result (i + 1));

end Exercise;
