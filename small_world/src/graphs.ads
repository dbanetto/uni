with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;
with Ada.Containers.Formal_Ordered_Maps;

generic
   Capacity : Count_Type;
package Graphs with SPARK_Mode is

   -- Capacity : Count_Type := 10;
   Edge_Capacity : Count_Type := Capacity * Capacity;

   type Graph is tagged private;
   type Node_Label is private;
   type Node_Labels is array (1..Capacity) of Node_Label;
   type Distance is range 0..Count_Type'Last;

   function Edge_Count (self : Graph) return Count_Type;
   function Node_Count (self : Graph) return Count_Type;

   function Has_Node (self : Graph ; node: Node_Label) return Boolean;
   function Has_Edge(self : Graph ; from, to : Node_Label) return Boolean;

   function Get_Nodes(self : Graph) return Node_Labels;

   function Empty_Graph return Graph
     with Post => Empty_Graph'Result.Edge_Count = 0
     and Empty_Graph'Result.Node_count = 0;

   function Is_Full(self : Graph) return Boolean is
     (self.Node_Count = Capacity);

   procedure New_Node(self : in out Graph ; new_node : out Node_Label) with
     Pre => not self.Is_Full,
     Post => self'Old.Node_Count + 1 = self.Node_Count ;

   procedure Add_Edge(self : in out Graph ; from, to : Node_Label ; success : out Boolean)
     with Pre => not self.Is_Full,
     Post => (success and then       (self.Has_Edge(from, to) and self'Old.Edge_Count + 1 = self.Edge_Count))
     or (not success and then ((not self.Has_Edge(from, to)) and self'Old.Edge_Count     = self.Edge_Count));

   function Distance_Between(self : Graph ; from, to : Node_Label) return Distance with
     Pre => self.Has_Path(from, to),
     Post => (from = to and then Distance_Between'Result = 0)
     or (self.Has_Edge(from, to) and then Distance_Between'Result = 1)
     or (self.Has_Path(from, to) and then
             -- the next step along the path one less step to the goal
             (for some next of self.Get_Nodes =>
                -- ensure not refective (prevents infinite loops) and is a neighbour and on the path to To
                (next /= from and self.Has_Edge(from, next) and self.Has_Path(next, to))
                -- ensure that it is the shortest path by getting all distances to To and
                -- checking the result is one step more then that (this is inductive)
              and then Distance_Between'Result = self.Distance_Between(next, to) + 1));

   function Has_Path(self : Graph ; from, to : Node_Label) return Boolean
     with Post => Has_Path'Result and then
   -- base case
     (self.Has_Edge(from, to)
        -- inductive case: there exists some neighbour that has a path to `To`
      or (for some next of self.Get_Nodes =>
          -- ensure not refective (prevents infinite loops) and is a neighbour
           (next /= from and self.Has_Edge(from, next))
           and then self.Has_Path(next, to)));

   function Diameter(self : Graph) return Distance with
     Pre => self.Edge_Count > 0,
     Post => (for all from of self.Get_Nodes =>
                (for all to of self.Get_Nodes =>
                Diameter'Result >= self.Distance_Between(from, to)));

   function Small(self : Graph ; coefficent : Positive) return Boolean is
     (Positive(self.Diameter) < coefficent * Integer(self.Node_Count));

   -- debug
   procedure Print_Graph(self : Graph);

private

   type Node_Label is new Count_Type;

   type Edge is tagged record
      from : Node_Label;
      to   : Node_Label;
   end record;

   function "<" (Left, Right : Edge) return Boolean;
   function "=" (Left, Right : Edge) return Boolean;

   package EdgeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Edge);

   subtype Edge_Set is EdgeSet.Set(Capacity => Edge_Capacity);

   package NodeDistance is new Ada.Containers.Formal_Ordered_Maps(Key_Type => Node_Label,
                                                                  Element_Type => Distance);

   package NodeSet is new Ada.Containers.Formal_Ordered_Sets(Element_Type => Node_Label);
   subtype Node_Set is NodeSet.Set(Capacity => Capacity);

   subtype Node_Distance is NodeDistance.Map(Capacity => Capacity);

   type Graph is tagged record
      Edges : Edge_Set;
      Factory : Node_Label := 0;
   end record;

   -- these two need to be public so things can be proven
   function Distance_Between_All(self : Graph ; from : Node_Label) return Node_Distance;

   function Adjacent_Nodes(self : Graph ; from : Node_Label) return Node_Set with
     Pre => self.Has_Node(from),
     Post => (for all to of Adjacent_Nodes'Result => self.Has_Edge(from, to));

end Graphs;
