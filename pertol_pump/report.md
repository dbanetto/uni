% Petrol Pump Units 
% SWEN421 Assignment 1 - David Barnett (300313764)

# Assumptions

 * This petrol pump exists in a cash-less society thus the only means to pay is credit card and it gives no change.
 * Since the only means to pay is credit card the customer pays at the pump.
 * It is assumed that the real implementation to get fuel prices would query an external service for prices 
 * It is assumed that the petrol has only one nozzle and the pump's physical design allows for different fuel types
   to go through a single nozzle.
 * It assumes that only one pump will be managed with this software at a time (one computer per pump, e.g. with Raspberry Pi's).
 * It assumes it is OK to pump into multiple fuel tanks
 * It assumes it is not OK to pump another fuel types while a debt for another fuel type is outstanding
 * It assumes that a fuel tank can only be fueled by the pump
 * It is assumed that the user always pumps the right fuel type into the tank & the pump has no liability if this is incorrect
 * It is assumed that fuel is only dispensed in discreet units.

# Structure

The general design of the project is using abstract globals to hide the state of the system.

There are 4 packages that make up the system as a whole. `pump`, `vehicle`, `reservoir` and `register`.

The `pump` package gives a public interface of all the actions that the user could complete physically to
the pump.
All the `pump`'s state is stored globally behind a `Abstract_State` attribute.
The abstract state of the package is split into two, the internal state of the pumping unit
(such as amount owed or which fuel to pump) and the state of the physical object.
This allows a distinction in the data flow between changing the physical state of the pump and the internal state of pump.
This package is intended to be the only package the user should interact with. However, due to my inexperience 
with Ada I could not partition sub-programs that should only be visibly internally to the system and those visible
to outside of the system.

The `reservoir` package represents the reservoir of fuel for each fuel type (Diesel, 91 Octane and 95 Octane).
It accomplishes this by hiding all the global state via `Abstract_State` attributes.
The goal of this package is to represent the state of the physical reservoirs that hold the fuel.
However, the package should only be visible to maintenance users and the pumps but, my inexperience with Ada
has prevented to be able to make this partition and ended up allowing the user have access to sub-programs such
as `drain` and `Initialize`.
The reservoirs have an `Initialize` sub-program to allow for setting the initial
state of the reservoirs from the physical state and to be a mechanism to allow for testing of the sub-system.

The `vehicle` package represents a fuel tank from a vehicle.
This package does not use globals instead uses a private record to hold the capacity and current volume of the tank.
Multiple fuel tanks are allowed as it is a valid use for the user to fill up multiple tanks in one pumping session, e.g.
filling jerry cans for the next boating trip.
This package exposes some sub-programs that should not be available to the user, such as `fill`.

The `register` package has the responsibility to ensure the prices for fuel is correct.
The register does not manage the balance sheet of the pump as all transactions happen at the pump instead
of at the register.
All the state of the register is constant in this implementation but it is assumed that this will be swapped
out for an implementation what will request the values from another system.

A design goal for the system was to not have any assertions that could fail on a valid user action.
To implement this if a precondition to a sub-program is not met the procedure will not run & fulfill the 
post-condition as the pump has already entered an error state.

Robustness of the system is ensured by checking the preconditions before
changing the state of the unit, if they fail then the sub-program will do
nothing. Given better experience with Ada I would log this as a warning to the
user.

# Justification of Correctness

## Functional & Flow Contracts

All public sub-programs have a pre- and post-condition either explicitly or implicitly (due to being expression functions).

There are two instances where `GNATProve` could not provide the postconditions of a sub-program.
These are the two pumping procedures. They both cannot be proved in a similar section of the post condition
that ensures that if fuel has been pumped then the debt owed will increase by a reasonable amount.

To ensure the correct usage of values there are two distinct types to represent an amount of fuel and an amount
of money, `FuelUnit` and `MoneyUnit` are these types.

## Data Contracts

In all packages that have abstract states all sub-programs have explicit data flow contracts.
In the pump package, where there are two abstract states, such that all sub-programs are denoted
with contracts that reflect their usage of the global state.

## Coverage report

The total code coverage with no unit tests are: 82.41%

File          | Coverage (lines)
--------------|--------------:
common.ads    |   0.00% of 1
main.adb      | 100.00% of 23
pump.adb      |  76.32% of 38
vehicle.adb   |  71.43% of 14
vehicle.ads   |   0.00% of 2
reservoir.adb |  88.00% of 25
register.adb  | 100.00% of 5
Total         |  82.41% of 108

## SPARK Report

Output of SPARK Prove all with all provers turned on.

```
----------------------------------------------------------------------------------
SPARK Analysis results  Total      Flow   Interval                Provers Unproved
----------------------------------------------------------------------------------
Data Dependencies           .         .          .                      .        .
Flow Dependencies           .         .          .                      .        .
Initialization             21        21          .                      .        .
Non-Aliasing                .         .          .                      .        .
Run-time Checks            26         .          1 16 (CVC4  94%, Z3  6%)        9
Assertions                  .         .          .                      .        .
Functional Contracts       28         .          .              22 (CVC4)        6
LSP Verification            .         .          .                      .        .
----------------------------------------------------------------------------------
Total                      75  21 (28%)     1 (1%)               38 (51%) 15 (20%)
```

> Note: empty columns have been redacted for space
