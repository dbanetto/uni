with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;
with Ada.Containers.Formal_Ordered_Maps;


generic
   Capacity : Count_Type;
package Graphs with  SPARK_Mode is
   -- Edge_Capacity : Count_Type := (Capacity * (Capacity - 1)) / 2;
   -- Capacity : Count_Type := 10;

   type Graph is tagged private;
   type Node_Label is private;
   type Distance is range 0..Count_Type'Last;

   function Size (self : Graph) return Count_Type;

   function Empty_Graph return Graph
     with Post => Empty_Graph'Result.Size = 0;

   function Is_Full(self : Graph) return Boolean is
      (self.Size = Graphs.Capacity);

   procedure New_Node(self : in out Graph ; new_node : out Node_Label);

   function Has_Edge(self : Graph ; from, to : Node_Label) return Boolean;

   procedure Add_Edge(self : in out Graph ; from, to : Node_Label ; success : out Boolean)
     with Pre => not self.Is_Full,
     Post => success and then (self.Has_Edge(from, to) and self'Old.Size + 1 = self.Size);

   -- function Adjacent_Nodes(self : Graph ; from : Node_Label) return Node_Set;

   function Distance_Between(self : Graph ; from, to : Node_Label) return Distance;

   function Has_Path(self : Graph ; from, to : Node_Label) return Boolean;
   --  with Post => Has_Path'Result and (for some next of self.Adjacent_Nodes(from) => self.Has_Path(next, to));

   procedure Print_Graph(self : Graph);

   function Diameter(self : Graph) return Distance;

   function Small(self : Graph ; coefficent : Positive) return Boolean;

private

   type Node_Label is new Count_Type;
   -- this solves issue with Node_Label failing to Prove with Spark
   subtype Node_T is Node_Label;

   type Edge is tagged record
      from : Node_Label;
      to   : Node_Label;
   end record;

   function "<" (Left, Right : Edge) return Boolean;
   function "=" (Left, Right : Edge) return Boolean;

   package EdgeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Edge);

   subtype Edge_Set is EdgeSet.Set(Capacity => Capacity);

   package NodeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Node_Label);
   subtype Node_Set is NodeSet.Set(Capacity => Capacity);

   package NodeDistance is new Ada.Containers.Formal_Ordered_Maps(Key_Type => Node_T,
                                                                  Element_Type => Distance);

   subtype Node_Distance is NodeDistance.Map(Capacity => Capacity);

   type Graph is tagged record
      Edges : Edge_Set;
      Factory : Node_Label := 0;
   end record;

   -- these two need to be public so things can be proven
   function Distance_Between_All(self : Graph ; from : Node_Label) return Node_Distance;

   function Adjacent_Nodes(self : Graph ; from : Node_Label) return Node_Set;

end Graphs;
