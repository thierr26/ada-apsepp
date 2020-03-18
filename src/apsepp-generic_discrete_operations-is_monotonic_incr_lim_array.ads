-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Element_Type is limited private;

   type Array_Type is array (Discrete_Type range <>) of Element_Type;

   type Element_Func_Ret_Type is limited private;

   with function Element_Func
     (Element : Element_Type) return Element_Func_Ret_Type;

   Strict : Boolean := True;

   with function "=" (Left,
                      Right : Element_Func_Ret_Type) return Boolean is <>;

   with function "<" (Left,
                      Right : Element_Func_Ret_Type) return Boolean is <>;

function Apsepp.Generic_Discrete_Operations.Is_Monotonic_Incr_Lim_Array
  (A : Array_Type) return Boolean;
