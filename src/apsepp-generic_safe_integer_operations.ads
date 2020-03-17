-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Integer_Type is range <>;

package Apsepp.Generic_Safe_Integer_Operations is

   pragma Pure (Apsepp.Generic_Safe_Integer_Operations);

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type Safe_Integer is private;

   subtype Natural_Safe_Integer is Safe_Integer
     with Dynamic_Predicate => Val (Natural_Safe_Integer) >= 0;

   function Create (Value     : Integer_Type;
                    Saturated : Boolean      := False) return Safe_Integer
     with Pre  => not Saturated or else (Value = Integer_Type'First
                                           or else
                                         Value = Integer_Type'Last),
          Post => Val (Create'Result) = Value
                    and then
                  Sat (Create'Result) = Saturated;

   function Val (X : Safe_Integer) return Integer_Type;

   function Sat (X : Safe_Integer) return Boolean;

   function "+" (X_1 : Safe_Integer;
                 X_2 : Natural_Safe_Integer) return Safe_Integer;

   subtype Natural_Base
     is Integer_Type'Base range 0 .. Integer_Type'Base'Last;

   procedure Inc (X : in out Safe_Integer; By : Natural_Base := 1);

private

   type Safe_Integer is record

      V : Integer_Type := Integer_Type'First;

      S : Boolean      := False;

   end record
     with Type_Invariant => (
                              Val (Safe_Integer) = Integer_Type'First
                                or else
                              Val (Safe_Integer) = Integer_Type'Last
                                or else
                              not Sat (Safe_Integer)
                            );

end Apsepp.Generic_Safe_Integer_Operations;
