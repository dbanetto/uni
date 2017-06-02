with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;

generic
   Capacity : Ada.Containers.Count_Type;
package Graphs with Pure, SPARK_Mode is

   type Graph is tagged private;
   type Node is private;

   function Empty_Graph return Graph;

   function Is_Full(self : Graph) return Boolean;

   function New_Node(self : out Graph) return Node;

   function Has_Edge(self : Graph ; from, to : Node) return Boolean;

   procedure Add_Edge(self : out Graph ; from, to : Node ; success : out Boolean)
     with Pre => not self.Is_Full,
     Post => success and then self.Has_Edge(from, to);

private
   type Node is new Integer;

   type Edge is tagged record
      to   : Node;
      from : Node;
   end record;

   function "<" (Left, Right : Edge) return Boolean;
   function "=" (Left, Right : Edge) return Boolean;

   package EdgeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Edge,
                                                             "<" => "<",
                                                             "=" => "=");

   subtype Edge_Set is EdgeSet.Set(Capacity => Capacity);

   type Graph is tagged record
      Edges : Edge_Set;
      Factory : Node := 0;
   end record;

end Graphs;
