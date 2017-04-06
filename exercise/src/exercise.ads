package Exercise with
     Spark_Mode is

   type IntArray is array (Integer range <>) of Integer with
        Predicate => IntArray'Length >= 0;

   -- util function for spec
   function Contains
     (values : IntArray;
      value  : Integer) return Boolean is
     (for some v of values => v = value) with
      Post => Contains'Result = (for some v of values => value = v);

   -- Ordered (acending) array of integers
   subtype OrderedIntArray is IntArray with
        Predicate =>
        (for all i in OrderedIntArray'First .. OrderedIntArray'Last - 1 =>
           OrderedIntArray (i) <= OrderedIntArray (i + 1));

   function InArray (value : Integer; input : IntArray) return Boolean with
      Post => InArray'Result =
      (for some k in input'Range => value = input (k));

   function FirstIndexOf
     (value : Integer;
      input : IntArray) return Integer with
      Pre  => InArray (value, input),
      Post =>
      (for some i in input'Range =>
      -- ensure that the input has the value
         value = input (i) and
      -- ensure that the
         i = FirstIndexOf'Result and
      -- ensure that all values before the found index does not include the value
      -- thus it is the first instance of the value
         (for all k in 0 .. i => input (k) /= value));

   function countValue
     (value : Integer;
      input : IntArray) return Integer is
      -- use stack to store state, yippes
      -- for each element we find in the list we add 1 and then add the rest
      -- by recursively to the rest of the list
     (if
        (for some i in input'Range => i = value)
      then
        -- we make a sub array from the found element plus one to the end of the array
        1 + (countValue(value, input (FirstIndexOf (value, input) + 1 .. input'Last)))
      -- for the case of the sub array does not contain the value return 0
      else 0) with
      Post =>
      (countValue'Result =
       (if
          (for some i in input'Range => i = value)
        then
          1 + (countValue(value, input (FirstIndexOf (value, input) + 1 .. input'Last)))
        else 0));

   function DeleteValue
     (value :    Integer;
      input : in IntArray) return IntArray with
     Post =>
       -- check if result is a permutation of input
       (for all i of DeleteValue'Result =>
          countValue (i, DeleteValue'Result) = countValue (i, input)) and
     -- the length of result array plus number of occurance of value in input is equal
     -- to the size of the input array
     DeleteValue'Result'Length + countValue(value, input) = input'Length and
     -- the result does not include value
      (for all i of DeleteValue'Result => i /= value);

   function ContcatArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray with
      -- ensure that a is lees than b so a ordered con
      Pre  => (a (a'Last) <= b (b'First)),
      Post => ContcatArray'Result = a & b;

   function MergeArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray with
      -- check if result is a permutation of a and b
      Post =>
      (for all i of MergeArray'Result =>
         countValue (i, MergeArray'Result) =
         countValue (i, a) + countValue (i, b));

end Exercise;
