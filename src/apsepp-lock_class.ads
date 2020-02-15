-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Lock_Class is

   type Lock_Interfa is limited interface;

   not overriding
   function Locked (Obj : Lock_Interfa) return Boolean is abstract;

   not overriding
   procedure On_Lock (Obj : Lock_Interfa) is null;

   not overriding
   procedure On_Unlock (Obj : Lock_Interfa) is null;

end Apsepp.Lock_Class;
