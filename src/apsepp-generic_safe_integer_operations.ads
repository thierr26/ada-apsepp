-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Integer_Type is range <>;

package Apsepp.Generic_Safe_Integer_Operations is

   pragma Pure (Apsepp.Generic_Safe_Integer_Operations);

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type Safe_Integer is private
     with Type_Invariant => Safe_Integer_Invariant (Safe_Integer);

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

   function Safe_Integer_Invariant (X : Safe_Integer) return Boolean
     is (
          Val (X) = Integer_Type'First
            or else
          Val (X) = Integer_Type'Last
            or else
          not Sat (X)
        );

private

   type Safe_Integer is record

      V : Integer_Type := Integer_Type'First;

      S : Boolean      := False;

   end record;

end Apsepp.Generic_Safe_Integer_Operations;
