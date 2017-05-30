with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Hashed_Maps;
with Ada.Containers.Formal_Hashed_Sets;

generic
   type Node is private;
   MaxNodes : Ada.Containers.Count_Type;

   with function Hash (Key : Node) return Ada.Containers.Hash_Type is <>;
   with function "=" (Left, Right : Node) return Boolean is <>;

package Graph with SPARK_Mode is

   package NodeSet is new Ada.Containers.Formal_Hashed_Sets(Element_Type => Node,
                                                            "=" => Graph."=",
                                                            Equivalent_Elements => Graph."=",
                                                            Hash => Graph.Hash);

   type Node_Set is new NodeSet.Set(Capacity => MaxNodes, Modulus => Hash_Type(MaxNodes));

   package EdgeMap is new Ada.Containers.Formal_Hashed_Maps(Key_Type => Node,
                                                            Element_Type => Node_Set,
                                                            Equivalent_Keys => Graph."=",
                                                            Hash => Graph.Hash);

   type Edge_Map is new EdgeMap.Map(Capacity => MaxNodes, Modulus => Hash_Type(MaxNodes));

   type Object is private;

   function Empty_Graph return Object;

private

   type Object is record
      nodes : Node_Set;
      edges : Edge_Map;
   end record;

end Graph;
