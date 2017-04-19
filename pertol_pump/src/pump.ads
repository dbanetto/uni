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
      Post    => GetDebt = MoneyUnit (0.0) and not isNozzleOut;

   procedure LiftNozzle (fuel : FuelType) with
      Global => (In_Out => (State, Unit)),
      Pre    => not isNozzleOut and
     ( -- if there is a debt a different fuel type cannot be picked
        (GetDebt > MoneyUnit (0.0) and fuel = GetCurrentFuelType) or
       -- if there is no debt, does not matter which fuel nozzle is picked up
        (GetDebt = MoneyUnit (0.0))
     ),
      Post => isNozzleOut and fuel = GetCurrentFuelType and GetDebt = GetDebt'Old;

   procedure ReturnNozzle with
      Global  => (In_Out => State, Input => Unit),
      Depends => (State => (State, Unit)),
      Pre     => isNozzleOut,
      Post    => not isNozzleOut;

   procedure PumpFuelFull (tank : in out Vehicle.Tank) with
      Global  => (In_Out => (Unit, Reservoir.Res), Input => State),
      Depends =>
      ((Unit, tank, Reservoir.Res) => (tank, Unit, State, Reservoir.Res)),
      Pre => isNozzleOut,
   -- GNAT cannot prove that GetDebt after is greater or equal to GetDebt before
      Post => GetCurrentFuelType = GetCurrentFuelType'Old and
      -- there are two cases with pumping, if the tank is not full & reservior not empty
      -- then there will be some pumping & an increase in debt
      ((not Vehicle.IsFull (tank'Old) and
       not Reservoir.isEmpty (GetCurrentFuelType)'Old and
       GetDebt > GetDebt'Old) or
      -- the second case is when either the tank is full or reservior is empty
      -- so no pumping will occur & thus no change in debt
      ((Vehicle.IsFull (tank'Old) or
        Reservoir.isEmpty (GetCurrentFuelType)'Old) and
       GetDebt = GetDebt'Old));

   procedure PumpFuel (tank : in out Vehicle.Tank; amount : FuelUnit) with
      Global  => (In_Out => (Unit, Reservoir.Res), Input => State),
      Depends =>
      ((Unit, tank, Reservoir.Res) =>
         (tank, Unit, State, amount, Reservoir.Res)),
      Pre => isNozzleOut and amount >= FuelUnit (0),
   -- GNAT cannot prove that GetDebt after is greater or equal to GetDebt before
     Post => GetCurrentFuelType = GetCurrentFuelType'Old and
      -- there are two cases with pumping, if the tank is not full & reservior not empty
      -- then there will be some pumping & an increase in debt
      ((not Vehicle.IsFull (tank'Old) and
       not Reservoir.isEmpty (GetCurrentFuelType)'Old and
           GetDebt > GetDebt'Old) or
      -- the second case is when either the tank is full or reservior is empty
      -- so no pumping will occur & thus no change in debt
      ((Vehicle.IsFull (tank'Old) or
        Reservoir.isEmpty (GetCurrentFuelType)'Old) and
       GetDebt = GetDebt'Old));

   procedure Pay (amount : MoneyUnit) with
      Global => (In_Out => Unit),
      Pre    => GetDebt >= amount and amount >= MoneyUnit (0.00),
      -- GNAT cannot prove that GetDebt before is equal to the sum of amount paid & current debt
      Post => GetDebt'Old = GetDebt + amount;

   function isNozzleOut return Boolean with
      Global => (Input => State);

   function GetDebt return MoneyUnit with
      Global => (Input => Unit);

   function GetCurrentFuelType return FuelType with
      Global => (Input => Unit);

end Pump;
