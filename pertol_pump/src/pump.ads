with Vehicle;
with common;
use common;

package Pump is

type FuelType is (Octane_91, Octane_95, Diesel);

type PumpBase is private;
type PumpReady is private;
type PumpPumping is private;
type PumpWaiting is private;

function LiftNozzle(this : PumpBase) return PumpReady;

function ReturnNozzle(this : PumpReady) return PumpBase;
function StartPumpingFuel(this : PumpReady ; tank : Vehicle.Tank) return PumpPumping;

function IsVehicleFull(this : PumpPumping) return Boolean;
function IsReservoirEmpty(this : PumpPumping) return Boolean;
function ReturnNozzleWaiting(this : PumpPumping) return PumpWaiting;

private
   type PumpBase is record
   Fuel : FuelType;
end record;

type PumpReady is record
   null;
end record;

type PumpPumping is record
      tank : Vehicle.Tank;
      total_pumped : FuelUnit;
end record;

type PumpWaiting is record
      null;
end record;

end Pump;
