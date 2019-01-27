-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private with Ada.Finalization;

package Apsepp.Scope_Bound_Locks is

   type SB_Lock_CB is access procedure;

   type SB_Lock (Lock_CB, Unlock_CB : SB_Lock_CB := null) is limited private;

   type SB_Lock_Access is not null access all SB_Lock;

   function Locked (Lock : SB_Lock) return Boolean;

   type SB_L_Locker (Lock : SB_Lock_Access) is tagged limited private

     with Type_Invariant'Class => Locked (Lock.all);

   not overriding
   function Has_Actually_Locked (Obj : SB_L_Locker) return Boolean;

private

   type SB_Lock (Lock_CB, Unlock_CB : SB_Lock_CB := null) is limited record
      Is_Locked : Boolean := False;
   end record;

   type SB_L_Locker (Lock : SB_Lock_Access)
     is limited new Ada.Finalization.Limited_Controlled with record
      Is_Instantiated : Boolean := False;
      Has_Locked      : Boolean := False;
   end record;

   overriding
   procedure Initialize (Obj : in out SB_L_Locker);

   overriding
   procedure Finalize (Obj : in out SB_L_Locker);

end Apsepp.Scope_Bound_Locks;
