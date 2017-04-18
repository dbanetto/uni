with Vehicle;
with common; use common;
with Reservoir;

package Pump with
     Spark_Mode,
     Abstract_State => (Unit, State),
     Initializes    => (Unit, State) is

   procedure Initialize with
      Global  => (In_Out => (Unit, State)),
      Depends => ((Unit, State) => (null), (null) => (Unit, State)),
      Post    => GetDebt = MoneyUnit (0.0) and GetState = Base;

   procedure LiftNozzle (fuel : FuelType) with
      Global => (In_Out => (State, Unit)),
      Pre    => GetState = Base,
      Post   => GetState = Waiting;

   procedure ReturnNozzle with
      Global  => (In_Out => State, Input => Unit),
      Depends => (State => (State, Unit)),
      Pre     => GetState = Waiting and GetDebt = MoneyUnit (0.00),
      Post    => GetState = Base and GetDebt = MoneyUnit (0.00);

   procedure PumpFuelFull (tank : in out Vehicle.Tank) with
      Global  => (In_Out => (Unit, Reservoir.Res), Input => State),
      Depends =>
      ((Unit, tank, Reservoir.Res) => (tank, Unit, State, Reservoir.Res)),
      Pre => GetState = Waiting,
-- GNAT cannot prove that GetDebt after is greater or equal to GetDebt before
      Post => GetDebt >= GetDebt'Old; -- get before & after snippets

   procedure PumpFuel
     (tank   : in out Vehicle.Tank;
      amount : FuelUnit) with
      Global  => (In_Out => (Unit, Reservoir.Res), Input => State),
      Depends =>
      ((Unit, tank, Reservoir.Res) =>
         (tank, Unit, State, amount, Reservoir.Res)),
      Pre => GetState = Waiting and amount >= FuelUnit (0),
-- GNAT cannot prove that GetDebt after is greater or equal to GetDebt before
      Post => GetDebt >= GetDebt'Old; -- get before & after snippets

   procedure Pay (amount : MoneyUnit) with
      Global => (In_Out => Unit, Proof_In => State),
      Pre    => GetState = Waiting and
      GetDebt >= amount and
      amount >= MoneyUnit (0.00),
      -- GNAT GetDebt before is equal to the sum of amount paid & current debt
      Post => GetDebt'Old = amount + GetDebt;

   function GetState return PumpState with
      Global => (Input => State);

   function GetDebt return MoneyUnit with
      Global => (Input => Unit);

private

end Pump;
