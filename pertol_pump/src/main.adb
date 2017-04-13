with Vehicle;
with Pump;
with common;
with Ada.Text_IO;
with Ada.Assertions;
use Ada.Assertions;
use Pump;
use common;

procedure main with SPARK_Mode is
   p : Pump.PumpUnit := Pump.Initialize;
   t : Vehicle.Tank := Vehicle.Initialize(current => FuelUnit(0),
                                          capacity => FuelUnit(100));
begin

   Pump.LiftNozzle(p, Pump.Octane_95);

   if not Vehicle.IsFull(t) then
      Pump.PumpFuel(p, t);
   end if;

   Ada.Text_IO.Put(Float'Image(Pump.GetDebt(p)));
   Assert(Pump.GetState(p) = Pump.Waiting);

   loop
      exit when Pump.GetDebt(p) = MoneyUnit(0.0);
      Pump.Pay(p, Pump.GetDebt(p));
   end loop;
   Assert(Pump.GetState(p) = Pump.Waiting);
   Pump.ReturnNozzle(p);
end main;
