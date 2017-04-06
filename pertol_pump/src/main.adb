with Vehicle;
with Pump;
with common;
with Ada.Text_IO;
use common;

procedure main is
   p : Pump.PumpUnit := Pump.Initialize;
   t : Vehicle.Tank := Vehicle.Initialize(current => FuelUnit(10),
                                          capacity => FuelUnit(100));
begin

   Pump.PumpFuel(p, t);

   Pump.LiftNozzle(p, Pump.Octane_95);


   Pump.PumpFuel(p, t);

   Ada.Text_IO.Put(Float'Image(Pump.GetDebt(p)));

   Pump.Pay(p, Pump.GetDebt(p));

   Pump.ReturnNozzle(p);
end main;
