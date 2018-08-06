with common; use common;

package Register with SPARK_Mode is

   function GetPriceOfFuel( fuel : FuelType ) return MoneyUnit
   with Post => GetPriceOfFuel'Result > MoneyUnit(0.0);

end Register;
