with common; use common;

package Reservoir with
     Spark_Mode,
     Abstract_State => (Res) is

   function isEmpty (fuel : FuelType) return Boolean with
      Global => (Input => Res),
      Pre    => (fuel = Diesel or fuel = Octane_91 or fuel = Octane_95);

   function GetVolume (fuel : FuelType) return FuelUnit with
      Global => (Input => Res),
      Pre    => (fuel = Diesel or fuel = Octane_91 or fuel = Octane_95);

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
