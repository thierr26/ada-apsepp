-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Barrier_Class is

   type Barrier_Interfa is limited interface;

   not overriding
   function Closed (Obj : Barrier_Interfa) return Boolean is abstract;

   not overriding
   procedure Close (Obj : in out Barrier_Interfa) is abstract;

   not overriding
   procedure Open (Obj : in out Barrier_Interfa) is abstract;

end Apsepp.Barrier_Class;
