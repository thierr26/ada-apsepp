-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization;

package Apsepp.Lock_Holder_Class is

   type Lock_Holder_Interfa is limited interface;

   not overriding
   function Holds (Obj : Lock_Holder_Interfa) return Boolean is abstract;

   not overriding
   function Take
     (Obj : in out Lock_Holder_Interfa) return Boolean is abstract;

   not overriding
   function Release
     (Obj : in out Lock_Holder_Interfa) return Boolean is abstract
     with Post'Class => not Obj.Holds;

   not overriding
   procedure On_Take (Obj : Lock_Holder_Interfa) is null;

   not overriding
   procedure On_Release (Obj : Lock_Holder_Interfa) is null;

   type Lock_Holder_Controlled_Handler
     (L_H      : not null access Lock_Holder_Interfa'Class;
      Disabled : Boolean)
     is limited new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Initialize (Obj : in out Lock_Holder_Controlled_Handler);

   overriding
   procedure Finalize (Obj : in out Lock_Holder_Controlled_Handler)
     with Post'Class => not Obj.L_H.Holds;

private

   type Lock_Holder_Controlled_Handler
     (L_H      : not null access Lock_Holder_Interfa'Class;
      Disabled : Boolean)
     is limited new Ada.Finalization.Limited_Controlled with null record;

end Apsepp.Lock_Holder_Class;
