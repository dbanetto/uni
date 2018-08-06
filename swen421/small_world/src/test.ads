with Graphs;   -- use Graphs;

package Test with SPARK_Mode is

  package My_Graph is new Graphs(Node_Capacity => 10);
  use My_Graph;

   procedure Main;

end Test;
