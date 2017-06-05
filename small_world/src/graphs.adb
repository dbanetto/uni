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

   function Size (self : Graph) return Count_Type is
      (EdgeSet.Length(self.Edges));


   procedure New_Node(self : in out Graph ; new_node : out Node_Label) is
   begin
      new_node := self.factory;
      self.factory := self.factory + 1;
   end New_Node;

   function Has_Edge(self : Graph ; from, to : Node_Label) return Boolean is
      find_edge : Edge := Edge'(from, to);
   begin
       return EdgeSet.Contains(self.Edges, find_edge);
   end Has_Edge;


   procedure Add_Edge(self : in out Graph ; from, to : Node_Label ; success : out Boolean) is
      new_edge : Edge := Edge'(from , to);
      cursor : EdgeSet.Cursor;
   begin

      if self.Is_Full then
         success := false;
         return;
      end if;

      Logger.Log("Before");
      self.Print_Graph;
      Logger.Log("");

      EdgeSet.Insert(self.Edges, new_edge, cursor, success);

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
      element : Edge;
   begin
      for index in self.Edges loop
         element := EdgeSet.Element(self.Edges, index);

         if element.from = from then
            NodeSet.Insert(adjacent, Node_T(element.to));
         end if;

         pragma Loop_Invariant (NodeSet.Length(adjacent) <= EdgeSet.Length(self.Edges));
      end loop;

      return adjacent;
   end Adjacent_Nodes;

   function Distance_Between(self : Graph ; from, to : Node_Label ) return Distance is
      distances : Node_Distance;
   begin
      distances := self.Distance_Between_All(from);

      if not NodeDistance.Contains(distances, Node_T(to)) then
         return 0;
      end if;

      return NodeDistance.Element(distances, Node_T(to));
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

      loop
         current := NodeSet.First_Element(worklist);
         NodeSet.Delete(worklist, current);
         NodeSet.Insert(visited, current);

         adjacent := self.Adjacent_Nodes(current);

         current_distance := NodeDistance.Element(distances, current);

         for index in adjacent loop
            declare
               neighbour : Node_Label;
               distance_from : Distance;
            begin
               neighbour := NodeSet.Element(adjacent, index);
               if NodeDistance.Contains(distances, neighbour) then
                  distance_from := NodeDistance.Element(distances, neighbour);
               else
                  distance_from := Distance'Last;
               end if;

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
      for node in 0..self.Factory loop

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

   function Small(self : Graph ; coefficent : Positive) return Boolean is
      total : Distance := 0;
      distances : Node_Distance;

      -- a node is only counted if it has an edge
      node_count : Integer := 0;
      node_counted : Boolean;
   begin
      for node in 0..self.Factory loop
         node_counted := false;
         distances := self.Distance_Between_All(node);

         for index in distances loop
            total := total + NodeDistance.Element(distances, index);
            if not node_counted then
               node_count := node_count + 1;
               node_counted := true;
            end if;
         end loop;
      end Loop;

      return (Integer(total) / node_count) < coefficent * node_count;
   end Small;

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
