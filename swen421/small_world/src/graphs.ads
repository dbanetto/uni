with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;
with Ada.Containers.Formal_Ordered_Maps;

generic
   Node_Capacity : Count_Type;
package Graphs with SPARK_Mode is
   -- Note: comment out the generic section above & uncomment Node_Capacity below
   -- to convert between this being a generic package or not.

   -- Node_Capacity : constant Count_Type := 10;
   Edge_Capacity : constant Count_Type := Node_Capacity * Node_Capacity;

   type Graph is tagged private;
   type Node_Label is private; -- labels to be used in the graph
   type Node_Labels is array (1..Node_Capacity) of Node_Label;
   type Distance is range 0..Count_Type'Last; -- total distance between two nodes

   Empty_Node : constant Node_Label;

   -- functions to get the current size of the sets
   function Edge_Count (self : Graph) return Count_Type;
   function Node_Count (self : Graph) return Count_Type;

   -- functions to check if data exists in the Graph
   function Has_Node (self : Graph ; node: Node_Label) return Boolean;
   function Has_Edge(self : Graph ; from, to : Node_Label) return Boolean;

   -- returns every node in the graph, but since it is an
   -- array there are some "tombstone" elements, every one that equals
   -- Graphs.Empty_Node is considered invalid
   function Get_Nodes(self : Graph) return Node_Labels
   with Post => (for all nodes of Get_Nodes'Result => self.Has_Node(nodes));

   -- creates an empty graph to worth with
   function Empty_Graph return Graph
     with Post => Empty_Graph'Result.Edge_Count = 0
     and Empty_Graph'Result.Node_count = 0;

   -- checks if capacity is reached
   function Is_Nodes_Full(self : Graph) return Boolean is
     (self.Node_Count = Node_Capacity);

   function Is_Edges_Full(self : Graph) return Boolean is
    (self.Edge_Count = Edge_Capacity);

   -- generates a new node to be used in the graph
   procedure New_Node(self : in out Graph ; new_node : out Node_Label)
   with Pre =>
   not self.Is_Nodes_Full,
   Post =>
   -- the node count will increase
   self'Old.Node_Count + 1 = self.Node_Count and
     -- The new node is not an empty node
     new_node /= Empty_Node and
     -- that the nodes before the addition are the same with the addition of the new
     -- node, all other undefined nodes are Empty_Nodes in Get_Nodes
     (for all i in self.Get_Nodes'Range =>
        (i < self.Node_Count and then (self.Get_Nodes(i) = self'Old.Get_Nodes(i))) or
          (i = self.Node_Count and then self.Get_Nodes(i) = New_Node) or
        (i > self.Node_Count and then self.Get_Nodes(i) = Empty_Node));


   -- adds an edge pair to the graph
   procedure Add_Edge(self : in out Graph ; from, to : Node_Label)
   with
   Pre =>
   -- does not currently have the edge
   not self.Has_Edge(from, to) and
     -- edge set is not full
     not self.Is_Edges_Full,
   Post =>
   -- Will have an edge now
   self.Has_Edge(from, to) and
     -- The total of edges have increased
     self'Old.Edge_Count + 1 = self.Edge_Count;


   -- finds the shortest path between two nodes (directed `from` to `to)
   function Distance_Between(self : Graph ; from, to : Node_Label) return Distance
   with
   -- Must have a path between the two nodes to get a valid distance
   Pre => self.Has_Path(from, to),
   Post =>
   -- first case: it is a path to itself, the distance will be 0
     (from = to and then Distance_Between'Result = 0)
     -- second case: it is a path to a neighbour, distane is 1
     or (self.Has_Edge(from, to) and then Distance_Between'Result = 1)
       -- third case: (inductive) it has a path and its distance is one more
       -- than the next step along the path
     or (self.Has_Path(from, to) and then
             -- the next step along the path one less step to the goal
             (for some next of self.Get_Nodes =>
                -- ensure not refective (prevents infinite loops) and is a neighbour and on the path to To
                (next /= from and self.Has_Edge(from, next) and self.Has_Path(next, to))
                -- ensure that it is the shortest path by getting all distances to To and
                -- checking the result is one step more then that (this is inductive)
              and then Distance_Between'Result = self.Distance_Between(next, to) + 1));


   -- checks if there is a path between two nodes (directed `from` to `to)
   function Has_Path(self : Graph ; from, to : Node_Label) return Boolean
   with Post =>
   Has_Path'Result and then
   -- base case
     (self.Has_Edge(from, to)
        -- inductive case: there exists some neighbour that has a path to `To`
      or (for some next of self.Get_Nodes =>
            -- ensure not refective (to prevent infinite loops) and is a neighbour
            (next /= from and self.Has_Edge(from, next))
              -- inductive step
          and then self.Has_Path(next, to)));

   -- finds the maximum distance between two nodes
   function Diameter(self : Graph) return Distance
   with Post =>
     (for all from of self.Get_Nodes =>
        (for all to of self.Get_Nodes =>
           -- checks to see that Diameter is the greatest distance in the graph
            Diameter'Result >= self.Distance_Between(from, to))) and
     (for some from of self.Get_Nodes =>
        (for some to of self.Get_Nodes =>
           -- checks to see that Diameter is a valid distance in the graph
            Diameter'Result = self.Distance_Between(from, to)));

   -- calcuates if the graph is small or not
   function Small(self : Graph ; coefficent : Float) return Boolean is
     (Float(self.Diameter) < coefficent * Float(self.Node_Count));

   -- debug print sub-program
   procedure Print_Graph(self : Graph);

private

   type Edge is tagged record
      from : Node_Label;
      to   : Node_Label;
   end record;

   type Node_Label is new Count_Type;

   -- these types are here to avoid an error when proving
   -- it reports that it is confused node_label with an int
   -- and sub typing it removes this error with no change needed to other code
   subtype Node_T is Node_Label;
   subtype Edge_T is Edge;

   Empty_Node : constant Node_Label := 0;

   function "<" (Left, Right : Edge) return Boolean;
   function "=" (Left, Right : Edge) return Boolean;

   package EdgeSet is new Formal_Ordered_Sets(Element_Type => Edge);
   subtype Edge_Set is EdgeSet.Set(Capacity => Edge_Capacity);

   package NodeDistance is new Formal_Ordered_Maps(Key_Type => Node_T,
                                                   Element_Type => Distance);

   package NodeSet is new Formal_Ordered_Sets(Element_Type => Node_T);
   subtype Node_Set is NodeSet.Set(Capacity => Node_Capacity);

   subtype Node_Distance is NodeDistance.Map(Capacity => Node_Capacity);

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
