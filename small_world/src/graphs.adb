package body Graphs with SPARK_Mode is

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

   procedure Add_Edge(self : out Graph ; to, from : Node ; success : out Boolean) is
      new_edge : Edge := Edge'(to, from);
      cursor : EdgeSet.Cursor;
   begin

      if self.Is_Full then
         success := false;
         return;
      end if;

      EdgeSet.Insert(self.Edges, new_edge, cursor, success);
   end Add_Edge;


end Graphs;
