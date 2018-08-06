package common with SPARK_Mode is
   subtype FuelUnit is Integer range 0 .. Integer'Last;
   subtype MoneyUnit is Float digits 2
     with Predicate => MoneyUnit >= 0.0;
   -- move to Fuel / Resivour package
   type FuelType is (Octane_91, Octane_95, Diesel);

   -- could refactor into functions: IsBase, IsWaiting
   type PumpState is (Base, Waiting);
 end common;
