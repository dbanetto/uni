package body Pump with
     Spark_Mode,
     Refined_State =>
     (Unit  => (moneyDue, fuelkind, total_pumped),
      State => nozzleState)
is

   moneyDue    : MoneyUnit := MoneyUnit (0.0);
   total_pumped : FuelUnit  := FuelUnit (0);
   nozzleState : PumpState := Base;
   fuelkind    : FuelType  := Octane_91;

   procedure Initialize is
   begin
      moneyDue    := MoneyUnit (0.0);
      total_pumped := FuelUnit (0);
      nozzleState := Base;
      fuelkind    := Octane_91;
   end Initialize;

   ----------------
   -- LiftNozzle --
   ----------------

   procedure LiftNozzle (fuel : FuelType) is
   begin
      nozzleState := Waiting;
      fuelkind    := fuel;
   end LiftNozzle;

   ------------------
   -- ReturnNozzle --
   ------------------

   procedure ReturnNozzle is
   begin
      -- enforce pre-conditions
      if nozzleState /= Waiting and moneyDue /= MoneyUnit (0.0) then
         return;
      end if;

      nozzleState := Base;
   end ReturnNozzle;

   --------------
   -- PumpFuel --
   --------------

   procedure PumpFuelFull (tank : in out Vehicle.Tank) is
      amount : FuelUnit := FuelUnit'Last;
   begin
      PumpFuel(tank, amount);
   end PumpFuelFull;

   procedure PumpFuel (tank : in out Vehicle.Tank ; amount : FuelUnit) is
      -- shadow amount so we can edit it
      actual_amount : FuelUnit := amount;
   begin
      -- enforce pre-conditions
      if nozzleState /= Waiting
        or Vehicle.IsFull (tank)
        or Reservoir.isEmpty(fuelkind) then
         return;
      end if;

      -- ensure that you do not over drain the reservoir
      if Reservoir.GetVolume(fuelkind) < actual_amount then
         actual_amount := Reservoir.GetVolume(fuelkind);
      end if;

      Vehicle.Fill (tank, actual_amount);
      Reservoir.drain(fuelkind, actual_amount);

      total_pumped := total_pumped + actual_amount;

      -- TODO: fuel price for each fuel type
      moneyDue := moneyDue + MoneyUnit (Float (actual_amount) * 2.0);
   end PumpFuel;

   ---------
   -- Pay --
   ---------

   procedure Pay (amount : MoneyUnit) is
   begin
      -- enforcing pre-conditions
      if moneyDue < amount then
         return;
      end if;
      moneyDue := moneyDue - amount;
   end Pay;

   --------------
   -- Getters  --
   --------------

   function GetState return PumpState is (nozzleState);

   function GetDebt return MoneyUnit is (moneyDue);

end Pump;
