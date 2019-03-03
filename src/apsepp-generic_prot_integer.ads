-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   type Integer_Type is range <>;
package Apsepp.Generic_Prot_Integer is

   -- Evaluate the pre-conditions in this package.
   pragma Assertion_Policy (Pre => Check);

   type O_P_I_Type is private

     with Type_Invariant => (
                              Val (O_P_I_Type) = Integer_Type'First
                                or else
                              Val (O_P_I_Type) = Integer_Type'Last
                                or else
                              not Sat (O_P_I_Type)
                            );

   function Create (V : Integer_Type; S : Boolean := False) return O_P_I_Type

     with Pre  => not S or else (V = Integer_Type'First
                                   or else
                                 V = Integer_Type'Last),

          Post => not sat (Create'Result);

   function Val (X : O_P_I_Type) return Integer_Type;

   function Sat (X : O_P_I_Type) return Boolean;

   function "+" (X_1, X_2 : O_P_I_Type) return O_P_I_Type

     with Pre => Val (X_2) >= 0;

   procedure Inc (X : in out O_P_I_Type; By : Integer_Type'Base := 1)

     with Pre => By >= 0;

private

   type O_P_I_Type is record
      Value     : Integer_Type := Integer_Type'First;
      Saturated : Boolean      := False;
   end record;

end Apsepp.Generic_Prot_Integer;
