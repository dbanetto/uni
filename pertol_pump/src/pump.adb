package body Pump is

   function Initialize return PumpUnit is
      unit : PumpUnit := (fuel => Diesel,
                          moneyDue => MoneyUnit(0.00),
                          state => Base
                          );
   begin
      return unit;
   end Initialize;

   ----------------
   -- LiftNozzle --
   ----------------

   procedure LiftNozzle (this : in out PumpUnit ; fuel : FuelType) is
   begin
      this.state := Waiting;
      this.fuel := fuel;
   end LiftNozzle;

   ------------------
   -- ReturnNozzle --
   ------------------

   procedure ReturnNozzle (this : in out PumpUnit) is
   begin
      this.state := Base;
   end ReturnNozzle;

   --------------
   -- PumpFuel --
   --------------

   procedure PumpFuel (this : in out PumpUnit; tank : in out Vehicle.Tank) is
      amount : FuelUnit := FuelUnit'Last;
   begin
      Vehicle.Fill(tank, amount);
      -- TODO: fuel price for each fuel type
      this.moneyDue := this.moneyDue + MoneyUnit(Float(amount) * 2.0);
   end PumpFuel;

   ---------
   -- Pay --
   ---------

   procedure Pay(this : in out PumpUnit ; amount : MoneyUnit) is
   begin
      this.moneyDue := this.moneyDue - amount;
   end Pay;


   --------------
   -- GetState --
   --------------

   function GetState (this : in PumpUnit) return PumpState is
   begin
      return this.state;
   end GetState;

   function GetDebt(this : in PumpUnit) return MoneyUnit is
   begin
      return this.moneyDue;
   end GetDebt;


end Pump;
