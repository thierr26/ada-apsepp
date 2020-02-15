-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Lock_Holder_Class is

   type Lock_Holder_Interface is limited interface;

   not overriding
   function Holds (Obj : Lock_Holder_Interface) return Boolean is abstract;

   not overriding
   function Take
     (Obj : in out Lock_Holder_Interface) return Boolean is abstract;

   not overriding
   function Release
     (Obj : in out Lock_Holder_Interface) return Boolean is abstract
     with Post'Class => not Obj.Holds;

   not overriding
   procedure On_Take (Obj : Lock_Holder_Interface) is null;

   not overriding
   procedure On_Release (Obj : Lock_Holder_Interface) is null;

end Apsepp.Lock_Holder_Class;
