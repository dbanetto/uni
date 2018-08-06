with common; use common;

package Reservoir with
     Spark_Mode,
     Abstract_State => (Res) is


   function GetVolume (fuel : FuelType) return FuelUnit with
      Global => (Input => Res);

   function isEmpty (fuel : FuelType) return Boolean with
     Global => (Input => Res),
     Post => isEmpty'Result = (GetVolume(fuel) = FuelUnit(0));


   procedure drain(fuel : FuelType ; amount : FuelUnit) with
     Global => (Output => Res),
     Depends => (Res => (fuel, amount)),
     Pre =>  GetVolume(fuel) >= amount,
     Post => GetVolume(fuel)'Old = GetVolume(fuel) + amount;

   procedure Initialize
     (set_disel     : FuelUnit;
      set_octane_91 : FuelUnit;
      set_octane_95 : FuelUnit) with
      Global  => (Output => (Res)),
      Depends => ((Res) => (set_octane_95, set_octane_91, set_disel)),
      Post    => GetVolume (Diesel) = set_disel and
      GetVolume (Octane_91) = set_octane_91 and
      GetVolume (Octane_95) = set_octane_95;

end Reservoir;
