with Logger; use Logger;
with Ada.Containers; use Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Graphs; use Graphs;
with Logger; use Logger;

procedure Main with SPARK_Mode is
   --package My_Graph is new Graphs(Capacity=> 10);
   --use My_Graph;

   me_graph : Graph := Empty_Graph;

   v1 : Node;
   v2 : Node;
   v3 : Node;

   success : Boolean;
begin

   me_graph.New_Node(v1);
   me_graph.New_Node(v2);
   me_graph.New_Node(v3);


   me_graph.Add_Edge(v1, v2, success);
   Assert (success);

   me_graph.Add_Edge(v2, v3, success);
   Assert (success);

   me_graph.Add_Edge(v3, v1, success);
   Assert (success);

   -- Logger.Log_Boolean();
   Logger.Log_Boolean (me_graph.Has_Edge(v1, v2));
   Logger.Log_Boolean (me_graph.Has_Edge(v2, v1));
   Logger.Log_Boolean (me_graph.Has_Edge(v2, v3));
   Logger.Log_Boolean (me_graph.Has_Edge(v3, v2));
   Logger.Log_Boolean (me_graph.Has_Edge(v3, v1));
   Logger.Log_Boolean (me_graph.Has_Edge(v1, v3));


end Main;
