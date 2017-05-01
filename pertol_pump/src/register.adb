package body Register with SPARK_Mode is

   function GetPriceOfFuel( fuel : FuelType ) return MoneyUnit is
     (case fuel is
         when Octane_91 => MoneyUnit(2.01),
         when Octane_95 => MoneyUnit(2.30),
      when Diesel => MoneyUnit(1.85));


end Register;
