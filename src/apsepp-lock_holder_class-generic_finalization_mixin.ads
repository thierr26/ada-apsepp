-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Parent (<>) is limited new Lock_Holder_Interfa with private;

package Apsepp.Lock_Holder_Class.Generic_Finalization_Mixin is

   type Child_W_Finalization is limited new Parent with private;

private

   -- TODOC: The 'L_H' discriminant of the 'Inner' component "points" to the
   -- instance of 'Child_W_Finalization'. <2020-02-22>
   -- REF: "Programming in Ada 2012" by John Barnes, section 18.7. <2020-02-22>
   -- REF: ARM8.6(17/3). <2020-02-22>
   -- REF: https://en.wikibooks.org/wiki/Ada_Programming/Object_Orientation#Multiple_Inheritance_via_Mix-in. <2020-02-22>
   type Child_W_Finalization is limited new Parent with record

      Inner : Lock_Holder_Controlled_Handler
        (L_H      => Child_W_Finalization'Access,
         Disabled => False);

   end record;

end Apsepp.Lock_Holder_Class.Generic_Finalization_Mixin;
