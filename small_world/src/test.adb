with Logger;         use Logger;
with Ada.Containers; use Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Graphs;
with Logger;         use Logger;

package body Test is

   type Vecs is array (1..4) of Node_Label;

   procedure Main with
      Spark_Mode is
      me_graph : Graph;

      v :  Vecs;
      success : Boolean;
   begin

      for i in Vecs'Range loop
         me_graph.New_Node (v(i));
      end loop;

      me_graph.Add_Edge (v(1), v(2), success);
      Assert (success);

      me_graph.Add_Edge (v(1), v(4), success);
      Assert (success);

      me_graph.Add_Edge (v(2), v(3), success);
      Assert (success);

      me_graph.Add_Edge (v(3), v(4), success);
      Assert (success);

      Logger.Log("Final Graph");
      me_graph.Print_Graph;

      -- Log all the connections to ensure they are correct
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


      Logger.Log_Int (Integer (me_graph.Diameter));

      Logger.Log_Boolean (me_graph.Small(1));

   end Main;
end Test;
