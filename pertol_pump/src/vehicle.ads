with common; use common;

package Vehicle with
     Spark_Mode is

   type Tank is private;

   function Initialize (capacity : FuelUnit; current : FuelUnit) return Tank;

   function GetCurrent (this : in Tank) return FuelUnit with
      Global     => null,
      Convention => Ghost;

   function GetCapacity (this : in Tank) return FuelUnit with
      Global     => null,
      Convention => Ghost;

   function IsFull (this : Tank) return Boolean with
      Post => (IsFull'Result and GetCurrent (this) = GetCapacity (this)) or
      (not IsFull'Result and GetCurrent (this) > GetCapacity (this));

   procedure Fill (this : in out Tank; amount : in out FuelUnit) with
      Global => null,
      Post   => GetCurrent (this) <= GetCapacity (this) and
      GetCurrent (this) >= GetCurrent (this'Old) and
      amount <= amount'Old and
      amount >= 0 and
      amount = GetCurrent (this) - GetCurrent (this'Old);

private

   type Tank is record
      capacity : FuelUnit;
      current  : FuelUnit;
   end record with
      Predicate => Tank.capacity >= Tank.current;

end Vehicle;
