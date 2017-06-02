with Logger; use Logger;
with Ada.Containers; use Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Graphs;

procedure Main with SPARK_Mode is
   package My_Graph is new Graphs(Capacity=> 10);
   use My_Graph;

   me_graph : Graph := Empty_Graph;

   v1 : Node := me_graph.New_Node;
   v2 : Node := me_graph.New_Node;
   v3 : Node := me_graph.New_Node;

   success : Boolean;
begin
   me_graph.Add_Edge(v1, v2, success);
   Assert (success);

   me_graph.Add_Edge(v2, v3, success);
   Assert (success);

   me_graph.Add_Edge(v3, v1, success);
   Assert (success);

end Main;
