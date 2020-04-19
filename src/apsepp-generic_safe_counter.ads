-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Generic_Safe_Integer_Operations;

generic

   -- TODOC: Can be an integer (signed or modular) type, but also an
   -- enumeration type. <2020-04-03>
   type Integer_Type is (<>);

package Apsepp.Generic_Safe_Counter is

   -- TODOC: Default value is such that the position of 'Val' is 0 and 'Sat' is
   -- false. <2020-04-02>
   type Safe_Counter is private;

   function Val (X : Safe_Counter) return Integer_Type;

   function Sat (X : Safe_Counter) return Boolean
     with Post => not Sat'Result
                    or else
                  X = Integer_Type'Last;

   function "=" (Left  : Safe_Counter;
                 Right : Integer_Type) return Boolean
     with Post => Val (Left) /= Right xor "="'Result;

   procedure Inc (X : in out Safe_Counter)
     with Post => X = Integer_Type'Val
                        (
                          Integer_Type'Pos (Val (X'Old))
                            +
                          (if Val (X'Old) < Integer_Type'Last then
                              1
                           else
                              0)
                        )
                    and then
                  (Sat (X) xor Val (X'Old) < Integer_Type'Last);

   -- TODOC: There is no way to unsaturate a 'Safe_Counter' object with 'Dec'.
   -- <2020-04-02>
   procedure Dec (X : in out Safe_Counter)
     with Post => X = Integer_Type'Val
                        (
                          Integer_Type'Pos (Val (X'Old))
                            -
                          (if Val (X'Old) > Integer_Type'Val (0)
                                and
                              not Sat (X) then
                              1
                           else
                              0)
                        )
                    and then
                  Sat (X) = Sat (X'Old);

   procedure Reset (X : in out Safe_Counter)
     with Post => Integer_Type'Pos (Val (X)) = 0 and then not Sat (X);

private

   package Safe_Integer_Operations
     is new Generic_Safe_Integer_Operations (Integer_Type);

   type Safe_Counter is new Safe_Integer_Operations.Safe_Integer;

end Apsepp.Generic_Safe_Counter;
