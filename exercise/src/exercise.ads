package Exercise with
     Spark_Mode is

   type IntArray is array (Integer range <>) of Integer;

   subtype OrderedIntArray is IntArray
     with Predicate => (for all i in OrderedIntArray'First .. OrderedIntArray'Last -1
                        => OrderedIntArray(i) <= OrderedIntArray(i + 1));

   function InArray (value : Integer; input : IntArray) return Boolean with
      Post => InArray'Result =
      (for some k in input'Range => value = input (k));

   function DeleteValue
     (value  :        Integer;
      input  : in     IntArray) return IntArray with
     Post => (for all i in DeleteValue'Result'Range => DeleteValue'Result(i) /= value);

   function ContcatArray (a : OrderedIntArray; b : OrderedIntArray) return OrderedIntArray with
      Pre =>
       (a (a'Last) <= b (b'First)),
      Post => ContcatArray'Result'Length = a'Length + b'Length;

   function MergeArray (a : OrderedIntArray; b : OrderedIntArray) return OrderedIntArray with
      Post => MergeArray'Result'Length = a'Length + b'Length;

end Exercise;
