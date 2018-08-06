with common; use common;

package Vehicle with
     Spark_Mode is

   type Tank is private;

   function Initialize (capacity : FuelUnit; current : FuelUnit) return Tank with
   Pre => capacity >= current;

   function GetCurrent (this : in Tank) return FuelUnit with
      Global     => null;

   function GetCapacity (this : in Tank) return FuelUnit with
      Global     => null;

   function IsFull (this : Tank) return Boolean with
      Post => IsFull'Result = (GetCurrent (this) = GetCapacity (this));

   procedure Fill (this : in out Tank; amount : in out FuelUnit) with
     Global => null,
      Pre => not IsFull(this),
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
