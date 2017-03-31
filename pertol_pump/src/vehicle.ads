with common;
use common;

package Vehicle is

   type Tank is private;

private

type Tank is record
   capacity : FuelUnit;
   current : FuelUnit;
end record  with Predicate => Tank.capacity >= Tank.current;

end Vehicle;
