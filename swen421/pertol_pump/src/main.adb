with Vehicle;
with Pump;           use Pump;
with common;         use common;
with Reservoir;
with Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

procedure main with
   Spark_Mode is
   t : Vehicle.Tank :=
     Vehicle.Initialize (current => FuelUnit (0), capacity => FuelUnit (1000));
   amount : FuelUnit := FuelUnit (50);
begin
   -- initial setup
   Pump.Initialize;
   Reservoir.Initialize (FuelUnit (5000), FuelUnit (10), FuelUnit (100));

   -- use #1
   Pump.LiftNozzle (Octane_95);

   Pump.PumpFuelFull (t);
   Ada.Text_IO.Put_Line (Float'Image (Pump.GetDebt));

   Pump.Pay (Pump.GetDebt);

   Pump.ReturnNozzle;

   -- use #2
   Pump.LiftNozzle (Octane_91);
   t := Vehicle.Initialize (current => FuelUnit (0), capacity => FuelUnit (1000));

   Pump.PumpFuelFull (t);
   Ada.Text_IO.Put_Line (Float'Image (Pump.GetDebt));

   Pump.Pay (Pump.GetDebt);
   Pump.ReturnNozzle;

   -- use #3
   Pump.LiftNozzle (Diesel);
   t := Vehicle.Initialize (current => FuelUnit (0), capacity => FuelUnit (1000));

   Pump.PumpFuel (t, amount);
   Ada.Text_IO.Put_Line (Float'Image (Pump.GetDebt));

   Pump.Pay (Pump.GetDebt);

   Pump.ReturnNozzle;
end main;
