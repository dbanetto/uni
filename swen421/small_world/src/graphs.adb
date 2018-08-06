with Logger; use Logger;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Hashed_Sets;
with Ada.Containers.Formal_Ordered_Maps;


package body Graphs with SPARK_Mode is
   use EdgeSet;

   function "<" (Left, Right : Edge) return Boolean is
     (Left.from > Right.from or Left.to < Right.to);

   function "=" (Left, Right : Edge) return Boolean is
     ( Left.from = Right.from and Left.to = Right.to );

   function Empty_Graph return Graph is
      empty_graph : Graph;
   begin
      return empty_graph;
   end;

   function Edge_Count (self : Graph) return Count_Type is
     (EdgeSet.Length(self.Edges));
   function Node_Count (self : Graph) return Count_Type is
     (Count_Type(self.Factory));

   function Has_Node (self : Graph ; node: Node_Label) return Boolean is
     (node < self.Factory);

   function Get_Nodes(self : Graph ) return Node_Labels is
      labels : Node_Labels := (others => Empty_Node);
   begin
      for i in 1..self.Node_Count loop
         labels(i) := Node_Label(i);
      end loop;
      return labels;
   end Get_Nodes;

   procedure New_Node(self : in out Graph ; new_node : out Node_Label) is
   begin
      self.factory := self.factory + 1;
      new_node := self.factory;
   end New_Node;

   function Has_Edge(self : Graph ; from, to : Node_Label) return Boolean is
      find_edge : Edge := Edge'(from, to);
      neighbours : Node_Set;
   begin
      neighbours := self.Adjacent_Nodes(from);
      return NodeSet.Contains(neighbours, to);
   end Has_Edge;


   procedure Add_Edge(self : in out Graph ; from, to : Node_Label) is
      new_edge : Edge := Edge'(from , to);
   begin

      if self.Has_Edge(from, to) or self.Is_Edges_Full then
         return;
      end if;
      -- debug
      Logger.Log("Before");
      self.Print_Graph;
      Logger.Log("");

      EdgeSet.Insert(self.Edges, new_edge);

      -- debug
      Logger.Log("After");
      self.Print_Graph;
      Logger.Log("");

   end Add_Edge;

   function Has_Path(self : Graph ; from, to : Node_Label) return Boolean is
     distances : Node_Distance;
   begin
      distances := self.Distance_Between_All(from);

      return NodeDistance.Contains(distances, to);
   end Has_Path;

   function Adjacent_Nodes(self : Graph ; from : Node_Label) return Node_Set is
      adjacent : Node_Set;
   begin
      for element of self.Edges loop
         if element.from = from then
            NodeSet.Insert(adjacent, element.to);
         end if;

         pragma Loop_Invariant (NodeSet.Length(adjacent) <= EdgeSet.Length(self.Edges));
      end loop;

      return adjacent;
   end Adjacent_Nodes;

   function Distance_Between(self : Graph ; from, to : Node_Label ) return Distance is
      distances : Node_Distance;
   begin
      distances := self.Distance_Between_All(from);

      if not NodeDistance.Contains(distances, to) then
         return 0;
      end if;

      return NodeDistance.Element(distances, to);
   end Distance_Between;

   function Distance_Between_All(self : Graph ; from : Node_Label) return Node_Distance is
      adjacent : Node_Set;
      visited : Node_Set;
      worklist : Node_Set;
      current : Node_Label;

      distances : Node_Distance;
      current_distance : Distance;
   begin
      NodeSet.Insert(worklist, from);
      NodeDistance.Insert(distances, from, 0);

      -- use Dijkstra's algorithm to find the shortest path to all nodes
      -- from a single source node
      loop
         current := NodeSet.First_Element(worklist);
         NodeSet.Delete(worklist, current); -- pop from work queue
         NodeSet.Insert(visited, current); -- mark node as visited

         adjacent := self.Adjacent_Nodes(current);

         current_distance := NodeDistance.Element(distances, current);

         for neighbour of adjacent loop
            declare
               distance_from : Distance;
            begin

               -- either get an old value or initalise it as "infinity"
               if NodeDistance.Contains(distances, neighbour) then
                  distance_from := NodeDistance.Element(distances, neighbour);
               else
                  distance_from := Distance'Last;
               end if;

               -- update if a shorter path is found
               if current_distance + 1 < distance_from then
                  if NodeDistance.Contains(distances, neighbour) then
                     NodeDistance.Replace(distances, neighbour, current_distance + 1);
                  else
                     NodeDistance.Insert(distances, neighbour, current_distance + 1);
                  end if;
               end if;

            end;
         end loop;

         -- put the unvisited neighbours into the worklist
         NodeSet.Difference(adjacent, visited);
         NodeSet.Union(worklist, adjacent);
         exit when NodeSet.Is_Empty(worklist);
      end loop;

      return distances;
   end Distance_Between_All;

   function Diameter(self : Graph) return Distance is
      element : Distance;
      max : Distance := Distance'First;
      distances : Node_Distance;
   begin
      if self.Edge_Count = 0 then
         return 0;
      end if;

      -- find the maximum distance between two nodes
      for node in 1..self.Factory loop

         distances := self.Distance_Between_All(node);

         for index in distances loop
            element := NodeDistance.Element(distances, index);
            if element > max then
               max := element;
            end if;
         end loop;

      end Loop;

      return max;
   end Diameter;

   procedure Print_Graph(self : Graph) is
      element : Edge;
   begin
      for index in self.Edges loop
         element := EdgeSet.Element(self.Edges, index);

         Logger.Log_Int(Integer(element.from), New_Line => false);
         Logger.Log(" =>", New_Line => false);
         Logger.Log_Int(Integer(element.to));
      end loop;
      Logger.New_Line;
   end Print_Graph;
end Graphs;
