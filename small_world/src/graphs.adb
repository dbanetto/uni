with Logger; use Logger;

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


   procedure New_Node(self : in out Graph ; new_node : out Node) is
   begin
      new_node := self.factory;
      self.factory := self.factory + 1;
   end New_Node;

   function Has_Edge(self : Graph ; from, to : Node) return Boolean is
      find_edge : Edge := Edge'(from, to);
   begin
       return EdgeSet.Contains(self.Edges, find_edge);
   end Has_Edge;


   procedure Add_Edge(self : in out Graph ; from, to : Node ; success : out Boolean) is
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


   function Adjacent_Nodes(self : Graph ; from : Node) return Edge_Set is
      adjacent : Edge_Set;
      element : Edge;
      cursor : EdgeSet.Cursor;
      success : Boolean;
   begin
      for index in self.Edges loop
         element := EdgeSet.Element(self.Edges, index);

         if element.from = from then
            EdgeSet.Insert(adjacent, element, cursor, success);
         end if;
      end loop;

      return adjacent;
   end Adjacent_Nodes;


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
