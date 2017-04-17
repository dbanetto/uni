with common; use common;

package body Reservoir with
     Refined_State =>
     (Res => (current_disel, current_octane_91, current_octane_95))
is

   current_disel     : FuelUnit;
   current_octane_91 : FuelUnit;
   current_octane_95 : FuelUnit;

   procedure Initialize (set_disel : FuelUnit ; set_octane_91 : FuelUnit ; set_octane_95 : FuelUnit) is
   begin
      current_disel := set_disel;
      current_octane_91 := set_octane_91;
      current_octane_95 := set_octane_95;
   end Initialize;

   function isEmpty
     (fuel : FuelType) return Boolean is
     (case fuel is
        when Diesel    => current_disel = FuelUnit (0),
        when Octane_91 => current_octane_91 = FuelUnit (0),
        when Octane_95 => current_octane_95 = FuelUnit (0),
         when others    => raise Constraint_Error);

      function GetVolume
     (fuel : FuelType) return FuelUnit is
     (case fuel is
        when Diesel    => current_disel,
        when Octane_91 => current_octane_91,
        when Octane_95 => current_octane_95,
        when others    => raise Constraint_Error);

end Reservoir;
