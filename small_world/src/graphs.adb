

package body Graphs with SPARK_Mode is
   use EdgeSet;

   function "<" (Left, Right : Edge) return Boolean is
     (Left.from < Right.from);

   function "=" (Left, Right : Edge) return Boolean is
     (Left.to = Right.to and Left.from = Right.from);

   function Empty_Graph return Graph is
      empty_graph : Graph := Graph'(Factory => 0, others => <>);
   begin
      return empty_graph;
   end;

   function New_Node(self : out Graph) return Node is
      value : Node := self.factory;
   begin
      self.factory := self.factory + 1;
      return value;
   end New_Node;

   function Is_Full(self : Graph) return Boolean is
     (EdgeSet.Length(self.Edges) = Graphs.Capacity);

   function Has_Edge(self : Graph ; from, to : Node) return Boolean is
      find_edge : Edge := Edge'(from, to);
      found_cusor : EdgeSet.Cursor;
   begin
      found_cusor := EdgeSet.Find(self.Edges, find_edge);

      return found_cusor = EdgeSet.No_Element;
   end Has_Edge;


   procedure Add_Edge(self : out Graph ; from, to : Node ; success : out Boolean) is
      new_edge : Edge := Edge'(from, to);
      cursor : EdgeSet.Cursor;
   begin

      if self.Is_Full then
         success := false;
         return;
      end if;

      EdgeSet.Insert(self.Edges, new_edge, cursor, success);
   end Add_Edge;


end Graphs;
