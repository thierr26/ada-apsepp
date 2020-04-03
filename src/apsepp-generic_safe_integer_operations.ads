-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Generic_Logical_Array;

generic

   -- TODOC: Can be an integer (signed or modular) type, but also an
   -- enumeration type. <2020-04-03>
   type Integer_Type is (<>);

package Apsepp.Generic_Safe_Integer_Operations is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type Safe_Integer is private;

   subtype Natural_Safe_Integer is Safe_Integer
     with Dynamic_Predicate =>
       Integer_Type'Pos (Val (Natural_Safe_Integer)) >= 0;

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
                 X_2 : Natural_Safe_Integer) return Safe_Integer
     with Post => (Sat ("+"'Result) and Val ("+"'Result) = Integer_Type'Last)
                    or else
                  Integer_Type'Pos (Val ("+"'Result))
                    =
                  Integer_Type'Pos (Val (X_1)) + Integer_Type'Pos (Val (X_2));

   subtype Natural_Base
     is Integer_Type'Base range Integer_Type'Val (0) .. Integer_Type'Base'Last;

   procedure Inc (X  : in out Safe_Integer;
                  By : Natural_Base := Natural_Base'Val (1))
     with Post => (Sat (X) and Val (X) = Integer_Type'Last)
                    or else
                  Integer_Type'Pos (Val (X)) = Integer_Type'Pos (Val (X'Old))
                                                 +
                                               Natural_Base'Pos (By);

   function Inc (X  : Safe_Integer;
                 By : Natural_Base := Natural_Base'Val (1)) return Safe_Integer
     with Post => (Sat (Inc'Result) and Val (Inc'Result) = Integer_Type'Last)
                    or else
                  (
                    Integer_Type'Pos (Val (Inc'Result))
                      =
                    Integer_Type'Pos (Val (X)) + Natural_Base'Pos (By)
                  );

   procedure Dec (X : in out Safe_Integer)
     with Post => (Sat (X) and Val (X) = Integer_Type'First)
                    or else
                  Integer_Type'Pos (Val (X)) = Integer_Type'Pos (Val (X'Old))
                                                 -
                                               1;

   function Dec (X : Safe_Integer) return Safe_Integer
     with Post => (Sat (Dec'Result) and Val (Dec'Result) = Integer_Type'First)
                    or else
                  (
                    Integer_Type'Pos (Val (Dec'Result))
                      =
                    Integer_Type'Pos (Val (X)) - 1
                  );

   function "*" (X_1, X_2 : Natural_Safe_Integer) return Natural_Safe_Integer
     with Post => (
                    Integer_Type'Pos (Val (X_1)) /= 0
                      and then
                    Integer_Type'Pos (Val (X_2)) /= 0
                  )
                    or else
                  Integer_Type'Pos (Val ("*"'Result)) = 0;

private

   package Logical_Array is new Generic_Logical_Array (Index_Type => Positive);

   use Logical_Array;

   type Safe_Integer is record

      V : Integer_Type := Integer_Type'First;

      S : Boolean      := False;

   end record
     with Type_Invariant
            => Some_True ((Val (Safe_Integer) = Integer_Type'First,
                           Val (Safe_Integer) = Integer_Type'Last,
                           not Sat (Safe_Integer)));

end Apsepp.Generic_Safe_Integer_Operations;
