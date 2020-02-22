-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Lock_Class; use Apsepp.Lock_Class;

with Apsepp.Lock_Holder_Class.Generic_Finalization_Mixin;
  use Apsepp.Lock_Holder_Class;

private with Apsepp.Protected_Barrier;

package Apsepp.Scope_Bound_Locking is

   type Lock is limited new Lock_Interfa with private;

   overriding
   function Locked (Obj : Lock) return Boolean;

   package Lock_Holder_Package is

      type Lock_Holder (L : not null access Lock'Class)
        is limited new Lock_Holder_Interfa with private;

      overriding
      function Holds (Obj : Lock_Holder) return Boolean
        with Post'Class => not Holds'Result or else Obj.L.Locked;

      overriding
      function Take (Obj : in out Lock_Holder) return Boolean
        with Post'Class => Obj.L.Locked;

      overriding
      function Release (Obj : in out Lock_Holder) return Boolean
        with Post'Class => not Obj.Holds;

   private

      type Lock_Holder (L : not null access Lock'Class)
        is limited new Lock_Holder_Interfa with record

         Holds_Flag : Boolean := False;

      end record;

   end Lock_Holder_Package;

   -- TODOC: Subtyping used as renaming. <2020-02-22>
   -- TODOC: Has discriminant 'L'. <2020-02-22>
   subtype Lock_Holder is Lock_Holder_Package.Lock_Holder;

   package Lock_Holder_Finalization_Mixin
     is new Generic_Finalization_Mixin
     (Parent => Lock_Holder_Package.Lock_Holder);

   -- TODOC: Subtyping used as renaming. <2020-02-22>
   -- TODOC: Has discriminant 'L'. <2020-02-22>
   subtype Controlled_Lock_Holder
     is Lock_Holder_Finalization_Mixin.Child_W_Finalization;

private

   Lock_Protection_Barrier : aliased Protected_Barrier.Barrier;

   type Lock is limited new Lock_Interfa with record

      Locked_Flag : Boolean := False;

   end record;

end Apsepp.Scope_Bound_Locking;
