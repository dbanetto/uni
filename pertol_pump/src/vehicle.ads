with common; use common;

package Vehicle with SPARK_Mode is

   type Tank is private;

   function Initialize(capacity : FuelUnit; current  : FuelUnit)  return Tank;

   function IsFull (this : Tank) return Boolean;

   procedure Fill (this : in out Tank; amount :in out FuelUnit) with
     Pre => not IsFull (this);

private

   type Tank is record
      capacity : FuelUnit;
      current  : FuelUnit;
   end record with
      Predicate => Tank.capacity >= Tank.current;

end Vehicle;
