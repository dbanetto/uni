with Vehicle;
with common; use common;

package Pump is

   -- move to Fuel / Resivour package
   type FuelType is (Octane_91, Octane_95, Diesel);

   -- could refactor into functions: IsBase, IsWaiting
   type PumpState is (Base, Waiting);

   type PumpUnit is private;

   function Initialize return PumpUnit with
      Post => GetState (Initialize'Result) = Base and
      GetDebt (Initialize'Result) = MoneyUnit (0.00);

   procedure LiftNozzle (this : in out PumpUnit; fuel : FuelType) with
      Pre  => GetState (this) = Base,
      Post => GetState (this) = Waiting;

   procedure ReturnNozzle (this : in out PumpUnit) with
      Pre  => GetState (this) = Waiting and GetDebt (this) = MoneyUnit (0.00),
      Post => GetState (this) = Base;

   procedure PumpFuel (this : in out PumpUnit; tank : in out Vehicle.Tank) with
      Pre  => GetState (this) = Waiting and not Vehicle.IsFull (tank),
      Post => GetDebt (this) > GetDebt (this'Old);

   procedure Pay (this : in out PumpUnit; amount : MoneyUnit) with
      Pre  => GetDebt (this) > MoneyUnit (0) and GetDebt (this) >= amount,
      Post => GetDebt (this'Old) - GetDebt (this) = amount;

   function GetState (this : in PumpUnit) return PumpState;

   function GetDebt (this : in PumpUnit) return MoneyUnit;

private

   type PumpUnit is record
      state    : PumpState;
      fuel     : FuelType;
      moneyDue : MoneyUnit;
   end record;

end Pump;
