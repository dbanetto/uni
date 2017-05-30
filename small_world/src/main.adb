with Logger; use Logger;
with Ada.Containers; use Ada.Containers;
with Graph;

procedure Main is
   function Hash (key : Integer) return Hash_Type is (Ada.Containers.Hash_Type(key));

   package Int_Graph is new Graph(Node => Integer,
                                  MaxNodes => 100,
                                  Hash => Hash);

   -- graph : Int_Graph := Int_Graph.Empty_Graph;
begin
   null;
end Main;
