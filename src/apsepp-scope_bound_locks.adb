-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks.Private_Handler;

package body Apsepp.Scope_Bound_Locks is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out SB_L_Locker) is

      use Apsepp.Scope_Bound_Locks.Private_Handler;

   begin

      Protected_Handler.Do_Lock (Lock          => Obj.Lock,
                                 Actually_Locked => Obj.Has_Locked);
      Obj.Is_Instantiated := True;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out SB_L_Locker) is

      use Apsepp.Scope_Bound_Locks.Private_Handler;

   begin

      Protected_Handler.Do_Unlock (Lock => Obj.Lock);

   end Finalize;

   ----------------------------------------------------------------------------

   function Locked (Lock : SB_Lock) return Boolean
     is (Lock.Is_Locked);

   ----------------------------------------------------------------------------

   not overriding
   function Instantiated (Obj : SB_L_Locker) return Boolean
     is (Obj.Is_Instantiated);

   ----------------------------------------------------------------------------

   not overriding
   function Has_Actually_Locked (Obj : SB_L_Locker) return Boolean
     is (Obj.Has_Locked);

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locks;
