with Vehicle;
with common; use common;

package Pump with
     Spark_Mode,
  Abstract_State => (Unit, State),
  Initializes => (Unit, State)
is

   -- move to Fuel / Resivour package
   type FuelType is (Octane_91, Octane_95, Diesel, None);

   -- could refactor into functions: IsBase, IsWaiting
   type PumpState is (Base, Waiting);

--     procedure Initialize with
--       Global => (Output => (Unit, State)),
--        Depends => ((Unit, State) => null),
--        Post   => GetDebt = MoneyUnit (0.0) and GetState = Base;

   procedure LiftNozzle (fuel : FuelType) with
     Global => (In_Out => (State, Unit)),
      Pre    => GetState = Base,
      Post   => GetState = Waiting;

   procedure ReturnNozzle with
     Global => (In_Out => State, Input => Unit),
      Depends => (State => (State, Unit)),
      Pre    => GetState = Waiting and GetDebt = MoneyUnit (0.00),
      Post   => GetState = Base;

   procedure PumpFuel (tank : in out Vehicle.Tank) with
     Global => (In_Out => Unit, Input => State),
      Depends => ((Unit, tank) => (tank, Unit, State)),
      Pre    => GetState = Waiting,
      Post   => GetState = Waiting and
      GetDebt >= GetDebt'Old; -- get before & after snippets

   procedure Pay (amount : MoneyUnit) with
      Global => (In_Out => Unit, Proof_In => State),
      Pre    => GetDebt >= amount and amount >= MoneyUnit (0.00),
      Post   => GetDebt'Old - GetDebt = amount and GetState = Waiting;

   function GetState return PumpState with
      Global => (Input => State);

   function GetDebt return MoneyUnit with
      Global => (Input => Unit);

private

end Pump;
