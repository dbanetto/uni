package body Vehicle is

   function Initialize(capacity : FuelUnit; current  : FuelUnit)  return Tank is
      t : Tank := ( capacity => capacity,
                       current => current);
   begin
      return t;
   end;

   ------------
   -- IsFull --
   ------------

   function IsFull (this : Tank) return Boolean is
   begin
      return this.current = this.capacity;
   end IsFull;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (this : in out Tank; amount : in out FuelUnit)
   is
   begin
      -- only store the amount of the fuel that can fit
      if amount + this.current > this.capacity then
         -- store how much is actually put into the tank
         amount := this.capacity - this.current;
         this.current := this.capacity;
      else
         -- fill up the tank
         this.current :=  amount + this.current;
      end if;
   end Fill;

   function GetCurrent (this : in Tank) return FuelUnit  is (this.current);
   function GetCapacity (this : in Tank) return FuelUnit is (this.capacity);

end Vehicle;
