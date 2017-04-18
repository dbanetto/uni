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
   Pump.Initialize;
   Reservoir.Initialize (FuelUnit (500), FuelUnit (10), FuelUnit (100));

   -- Pump.PumpFuelFull(t);
   Pump.LiftNozzle (Octane_95);

   -- Pump.PumpFuel (t, amount);

   Pump.PumpFuelFull (t);
   -- Assert (not Vehicle.IsFull(t)); -- due to Reservoir

   Ada.Text_IO.Put (Float'Image (Pump.GetDebt));
   Assert (Pump.GetState = Waiting);

   loop
      exit when Pump.GetDebt = MoneyUnit (0.0);
      Pump.Pay (Pump.GetDebt);
   end loop;
   Assert (Pump.GetState = Waiting);
   Pump.ReturnNozzle;
end main;
