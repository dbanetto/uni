package Exercise with
     Spark_Mode is

   type IntArray is array (Integer range <>) of Integer with
        Predicate => IntArray'Length >= 0;

   function Contains
     (values : IntArray;
      value  : Integer) return Boolean is
     (for some v of values => v = value) with
      Post => Contains'Result = (for some v of values => value = v);

   subtype OrderedIntArray is IntArray with
        Predicate =>
        (for all i in OrderedIntArray'First .. OrderedIntArray'Last - 1 =>
           OrderedIntArray (i) <= OrderedIntArray (i + 1));

   function InArray (value : Integer; input : IntArray) return Boolean with
      Post => InArray'Result = (for some ele of input => ele = value);

   function DeleteValue
     (value :    Integer;
      input : in IntArray) return IntArray with
      Post => (for all ele of DeleteValue'Result => ele /= value) and
      (for all ele of DeleteValue'Result => Contains (input, ele)) and
      (for all inEle of input =>
         (inEle = value xor Contains (DeleteValue'Result, inEle)));

   function ContcatArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray with
      -- ensure that a is lees than b so no merges are required
      Pre  => (a (a'Last) <= b (b'First)),
      Post => ContcatArray'Result = a & b;

   function MergeArray
     (a : OrderedIntArray;
      b : OrderedIntArray) return OrderedIntArray with
      Post => MergeArray'Result'Length = a'Length + b'Length and
      (for all o of MergeArray'Result =>
         Contains (a, o) or Contains (b, o)) and
      (for all x of a => Contains (MergeArray'Result, x)) and
      (for all x of b => Contains (MergeArray'Result, x));

end Exercise;
