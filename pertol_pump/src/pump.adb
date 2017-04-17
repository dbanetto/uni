package body Pump with
     Spark_Mode,
  Refined_State => (Unit => (moneyDue, fuelkind, totalPumped),
                   State => nozzleState) is

   moneyDue    : MoneyUnit := MoneyUnit (0.0);
   totalPumped : FuelUnit := FuelUnit (0);
   nozzleState : PumpState := Base;
   fuelkind    : FuelType := None;

--     procedure Initialize is
--     begin
--        moneyDue    := MoneyUnit (0.0);
--        totalPumped := FuelUnit (0);
--        nozzleState := Base;
--        fuelkind    := None;
--     end Initialize;

   ----------------
   -- LiftNozzle --
   ----------------

   procedure LiftNozzle (fuel : FuelType) is
   begin
      nozzleState := Waiting;
      fuelkind := fuel;
   end LiftNozzle;

   ------------------
   -- ReturnNozzle --
   ------------------

   procedure ReturnNozzle is
   begin
      if moneyDue /= MoneyUnit(0.0) then
         return;
      end if;

      nozzleState := Base;
   end ReturnNozzle;

   --------------
   -- PumpFuel --
   --------------

   procedure PumpFuel (tank : in out Vehicle.Tank) is
      amount : FuelUnit := FuelUnit'Last;
   begin
      if not (nozzleState = Waiting) or Vehicle.IsFull (tank) then
         return;
      end if;

      Vehicle.Fill (tank, amount);
      -- TODO: fuel price for each fuel type
      moneyDue := moneyDue + MoneyUnit (Float (amount) * 2.0);
   end PumpFuel;

   ---------
   -- Pay --
   ---------

   procedure Pay (amount : MoneyUnit) is
   begin
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
