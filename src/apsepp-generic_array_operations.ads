-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Discrete_Operations;

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
   with function "<" (Left, Right : Element_Type) return Boolean;
package Apsepp.Generic_Array_Operations is

   package D is new Apsepp.Generic_Discrete_Operations (Index_Type);

   function Monotonic_Incr (A      : Array_Type;
                            Strict : Boolean    := True) return Boolean
     is (for all K in A'Range => K = A'First
                                   or else
                                 A(D.Pr (K)) < A(K)
                                   or else
                                 (
                                   not Strict
                                     and then
                                   A(D.Pr (K)) = A(K)
                                 ));

   procedure Insert_Incr (A                : in out Array_Type;
                          Max_Insert_Index :        Index_Type;
                          Elem             :        Element_Type;
                          Rev              :        Boolean      := False)

     with Pre  => Monotonic_Incr (A(A'First .. D.Pr (Max_Insert_Index)),
                                  Strict => False)
                    and then
                  Max_Insert_Index in A'Range,

          Post => Monotonic_Incr (A(A'First .. Max_Insert_Index),
                                  Strict => False);

end Apsepp.Generic_Array_Operations;
