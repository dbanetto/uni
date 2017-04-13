with Vehicle;
with common; use common;

package Pump with SPARK_Mode is

   -- move to Fuel / Resivour package
   type FuelType is (Octane_91, Octane_95, Diesel);

   -- could refactor into functions: IsBase, IsWaiting
   type PumpState is (Base, Waiting);

   type PumpUnit is private with
     Type_Invariant =>
       (GetState(PumpUnit) = Base and then GetDebt(PumpUnit) = MoneyUnit(0.0)) and
       (GetState(PumpUnit) = Waiting and then GetDebt(PumpUnit) >= MoneyUnit(0.0));

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
      Pre  => GetState (this) = Waiting,
      Post => GetState (this) = Waiting and GetDebt (this) >= GetDebt (this'Old);

   procedure Pay (this : in out PumpUnit; amount : MoneyUnit) with
      Pre  => GetDebt (this) >= amount and amount > MoneyUnit (0.00),
      Post => GetDebt (this'Old) - GetDebt (this) = amount and GetState (this) = Waiting;

   function GetState (this : in PumpUnit) return PumpState with
     Post => GetState(this) = GetState(this'Old);

   function GetDebt (this : in PumpUnit) return MoneyUnit with
     Post => GetDebt(this) = GetDebt(this'Old);

private

   type PumpUnit is record
      state    : PumpState;
      fuel     : FuelType;
      moneyDue : MoneyUnit;
   end record;

end Pump;
