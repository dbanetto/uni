with Logger;         use Logger;
with Ada.Containers; use Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Graphs;
with Logger;         use Logger;

package body Test is
   type Vecs is array (1..4) of Node_Label;

   procedure Check_All_Nodes(me_graph: Graph; v : Vecs) is

   begin
      Logger.Log("Path truth table (Has_Edge, Has_Path, Distance)");
      for x in Vecs'Range loop
         for y in Vecs'Range loop
            Logger.Log_Int(x, New_Line => False);
            Logger.Log(" =>", New_Line => False);
            Logger.Log_Int(y, New_Line => False);
            Logger.Log(" is ", New_Line => False);
            Logger.Log_Boolean (me_graph.Has_Edge (v(x), v(y)), New_Line => False);
            Logger.Log(", ", New_Line => False);
            Logger.Log_Boolean (me_graph.Has_Path (v(x), v(y)), New_Line => False);
            Logger.Log(", ", New_Line => False);
            Logger.Log_Int (Integer(me_graph.Distance_Between (v(x), v(y))));
         end loop;
      end loop;
      end Check_All_Nodes;

   procedure Main with
      Spark_Mode is
      me_graph : Graph;


      v : Vecs := (others => Empty_Node);
   begin

      Logger.Log("Pre-nodes & edges checks");
      Check_All_Nodes(me_graph, v);

      Logger.Log_Int (Integer (me_graph.Diameter));
      Logger.Log_Boolean (me_graph.Small(1.0));

      Logger.Log("Adding Nodes");

      for i in Vecs'Range loop
         exit when me_graph.Is_Nodes_Full;
         me_graph.New_Node (v(i));
         pragma Loop_Invariant (for all x in 1..i => (for some n of me_graph.Get_Nodes => v(x) = n));
         pragma Assert (for all x in 1..i => (for some n of me_graph.Get_Nodes => v(x) = n));
      end loop;

      Logger.Log("Adding Edges to the graph");

      me_graph.Add_Edge (v(1), v(2));

      me_graph.Add_Edge (v(1), v(3));

      me_graph.Add_Edge (v(2), v(4));

      me_graph.Add_Edge (v(3), v(4));

      pragma Assert (me_graph.Edge_Count > 0);

      Logger.Log("Final Graph");
      me_graph.Print_Graph;

      -- Log all the connections to ensure they are correct
      Check_All_Nodes(me_graph, v);


      Logger.Log_Int (Integer (me_graph.Diameter));
      Logger.Log_Boolean (me_graph.Small(1.0));

   end Main;
end Test;
