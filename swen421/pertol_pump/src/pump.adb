with Register;

package body Pump with
     Spark_Mode,
     Refined_State =>
     (Unit  => (moneyDue, fuelkind),
      State => (nozzleOut))
is

   moneyDue : MoneyUnit := MoneyUnit (0.0);
   nozzleOut : Boolean := False;
   fuelkind : FuelType  := Octane_91;

   procedure Initialize is
   begin
      moneyDue    := MoneyUnit (0.0);
      nozzleOut := False;
      fuelkind    := Octane_91;
   end Initialize;

   ----------------
   -- LiftNozzle --
   ----------------

   procedure LiftNozzle (fuel : FuelType) is
   begin
      if nozzleOut or (GetDebt > MoneyUnit(0.0) and fuel /= fuelkind) then
         return;
      end if;

      nozzleOut := True;
      fuelkind := fuel;
   end LiftNozzle;

   ------------------
   -- ReturnNozzle --
   ------------------

   procedure ReturnNozzle is
   begin
      -- enforce pre-conditions
      if not isNozzleOut and moneyDue /= MoneyUnit (0.0) then
         return;
      end if;

      nozzleOut := False;
   end ReturnNozzle;

   --------------
   -- PumpFuel --
   --------------

   procedure PumpFuelFull (tank : in out Vehicle.Tank) is
      amount : FuelUnit := FuelUnit'Last;
   begin
      if not isNozzleOut then
         return;
      end if;
      PumpFuel(tank, amount);
   end PumpFuelFull;

   procedure PumpFuel (tank : in out Vehicle.Tank ; amount : FuelUnit) is
      -- shadow amount so we can edit it
      actual_amount : FuelUnit := amount;
      added_debt : MoneyUnit;
   begin
      -- enforce pre-conditions
      if not isNozzleOut
        or amount <= FuelUnit (0)
        or Vehicle.IsFull (tank)
        or Reservoir.isEmpty(fuelkind) then
         return;
      end if;

      -- ensure that you do not over drain the reservoir
      if Reservoir.GetVolume(fuelkind) < actual_amount then
         actual_amount := Reservoir.GetVolume(fuelkind);
      end if;

      if actual_amount <= FuelUnit(0) then
         return;
      end if;

      Vehicle.Fill (tank, actual_amount);
      Reservoir.drain(fuelkind, actual_amount);
      added_debt := MoneyUnit (Float(actual_amount) * Float(Register.GetPriceOfFuel(fuelkind)));
      moneyDue := moneyDue + added_debt;

      pragma Assert (added_debt >= MoneyUnit(0.0));
   end PumpFuel;

   ---------
   -- Pay --
   ---------

   procedure Pay (amount : MoneyUnit) is
      old_due : MoneyUnit;
   begin
      -- enforcing pre-conditions
      if moneyDue < amount and Float(amount) < 0.0  then
         return;
      end if;

      old_due := moneyDue;
      moneyDue := moneyDue - amount;

      pragma Assert ( old_due = moneyDue + amount);
   end Pay;

   --------------
   -- Getters  --
   --------------

   function isNozzleOut return Boolean is (nozzleOut);

   function GetDebt return MoneyUnit is (moneyDue);

   function GetCurrentFuelType return FuelType is (fuelkind);

end Pump;
