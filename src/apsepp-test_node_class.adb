-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

package body Apsepp.Test_Node_Class is

   ----------------------------------------------------------------------------

   function Has_No_Children_W_Same_Tags
     (Obj : Test_Node_Interfa'Class) return Boolean
     is ((for all K_1 in 1 .. Obj.Child_Count =>
           (for all K_2 in 1 .. Obj.Child_Count =>
             K_2 = K_1 or else Obj.Child (K_1)'Tag
                                 /=
                               Obj.Child (K_2)'Tag)));

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
