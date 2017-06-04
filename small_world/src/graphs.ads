with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;

--generic
--   Capacity : Ada.Containers.Count_Type;
package Graphs with  SPARK_Mode is
   Capacity : constant Count_Type := 10;

   type Graph is tagged private;
   type Node is private;


   function Size (self : Graph) return Count_Type;

   function Empty_Graph return Graph
     with Post => Empty_Graph'Result.Size = 0;

   function Is_Full(self : Graph) return Boolean is
      (self.Size = Capacity);

   procedure New_Node(self : in out Graph ; new_node : out Node);

   function Has_Edge(self : Graph ; from, to : Node) return Boolean;

   procedure Add_Edge(self : in out Graph ; from, to : Node ; success : out Boolean)
     with Pre => not self.Is_Full,
     Post => success and then (self.Has_Edge(from, to) and self'Old.Size + 1 = self.Size);


   procedure Print_Graph(self : Graph);
private

   type Node is new Count_Type;

   type Edge is tagged record
      from : Node;
      to   : Node;
   end record;

   function "<" (Left, Right : Edge) return Boolean;
   function "=" (Left, Right : Edge) return Boolean;

   package EdgeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Edge,
                                                             "<" => "<",
                                                             "=" => "=");

   subtype Edge_Set is EdgeSet.Set(Capacity => 10); -- FIXME: when generic replace with

   type Graph is tagged record
      Edges : Edge_Set;
      Factory : Node := 0;
   end record;


   function Adjacent_Nodes(self : Graph ; from : Node) return Edge_Set;

end Graphs;
