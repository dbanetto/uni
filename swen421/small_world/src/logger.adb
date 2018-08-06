with Ada.Text_IO; use Ada.Text_IO;

package body Logger is
   procedure Log(value : String ; New_Line : Boolean := True) is
   begin
      if New_Line then
         Put_Line(value);
      else
         Put(value);
      end if;
   end Log;

   procedure Log_Int(value : Integer; New_Line : Boolean := True) is
   begin
      if New_Line then
         Put_Line(Integer'Image(value));
      else
         Put(Integer'Image(value));
      end if;
   end Log_Int;

   procedure Log_Boolean(value : Boolean; New_Line : Boolean := True) is
   begin
      if New_Line then
         Put_Line(Boolean'Image(value));
      else
         Put(Boolean'Image(value));
      end if;
   end Log_Boolean;

   procedure New_Line is
   begin
      Put_Line("");
   end New_Line;

end Logger;
