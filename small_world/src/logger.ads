package Logger with Spark_Mode is
   -- boundary package for logging

   procedure Log(value : String ; New_Line : Boolean := True);

   procedure New_Line;

   procedure Log_Int(value : Integer ; New_Line : Boolean := True);

   procedure Log_Boolean(value : Boolean ; New_Line : Boolean := True);

end Logger;
