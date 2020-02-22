-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization;         use Ada.Finalization;
with Apsepp.Lock_Class;        use Apsepp.Lock_Class;
with Apsepp.Lock_Holder_Class; use Apsepp.Lock_Holder_Class;

private with Apsepp.Protected_Barrier;

package Apsepp.Scope_Bound_Locking is

   type Lock is limited new Lock_Interfa with private;

   overriding
   function Locked (Obj : Lock) return Boolean;

   type Controlled_Lock_Holder
     (L : not null access Lock'Class) is limited new Limited_Controlled
                                                       and
                                                     Lock_Holder_Interfa
     with private;

   overriding
   procedure Initialize (Obj : in out Controlled_Lock_Holder)
     with Post'Class => Obj.L.Locked;

   overriding
   procedure Finalize (Obj : in out Controlled_Lock_Holder);

   overriding
   function Holds (Obj : Controlled_Lock_Holder) return Boolean
     with Post'Class => not Holds'Result or else Obj.L.Locked;

   overriding
   function Take (Obj : in out Controlled_Lock_Holder) return Boolean
     with Post'Class => Obj.L.Locked;

   overriding
   function Release (Obj : in out Controlled_Lock_Holder) return Boolean
     with Post'Class => not Obj.Holds;

private

   Lock_Protection_Barrier : aliased Protected_Barrier.Barrier;

   type Lock is limited new Lock_Interfa with record

      Locked_Flag : Boolean := False;

   end record;

   type Controlled_Lock_Holder
     (L : not null access Lock'Class) is limited new Limited_Controlled
                                                       and
                                                     Lock_Holder_Interfa
     with record

      Holds_Flag : Boolean := False;

   end record;

end Apsepp.Scope_Bound_Locking;
