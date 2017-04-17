with Vehicle;
with Pump;
with common;
with Ada.Text_IO;
with Ada.Assertions;
use Ada.Assertions;
use Pump;
use common;

procedure main with SPARK_Mode => On is
   t : Vehicle.Tank := Vehicle.Initialize(current => FuelUnit(0),
                                          capacity => FuelUnit(100));
begin
   -- Pump.Initialize;
   -- Pump.PumpFuel(t);

   Pump.LiftNozzle(Pump.Octane_95);

   loop
      exit when Vehicle.IsFull(t);
      Pump.PumpFuel(t);
   end loop;

   Ada.Text_IO.Put(Float'Image(Pump.GetDebt));
   Assert(Pump.GetState = Pump.Waiting);

   loop
      exit when Pump.GetDebt = MoneyUnit(0.0);
      Pump.Pay(Pump.GetDebt);
   end loop;
   Assert(Pump.GetState = Pump.Waiting);
   Pump.ReturnNozzle;
end main;
