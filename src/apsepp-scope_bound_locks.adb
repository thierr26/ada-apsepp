-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks.Private_Handler;

package body Apsepp.Scope_Bound_Locks is

   ----------------------------------------------------------------------------

   procedure SB_Lock_CB_procedure is

   begin

      if SBLCB_Access /= null then
         SBLCB_Access.all;
      end if;

   end SB_Lock_CB_procedure;

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out SB_L_Locker) is

      use Private_Handler;

   begin

      Protected_Handler.Do_Lock (Lock            => Obj.Lock,
                                 Actually_Locked => Obj.Has_Locked);
      Obj.Is_Instantiated := True;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out SB_L_Locker) is

      use Private_Handler;

   begin

      if SB_L_Locker'Class (Obj).Has_Actually_Locked then
         Protected_Handler.Do_Unlock (Lock => Obj.Lock);
      end if;

   end Finalize;

   ----------------------------------------------------------------------------

   function Locked (Lock : SB_Lock) return Boolean
     is (Lock.Is_Locked);

   ----------------------------------------------------------------------------

   not overriding
   function Has_Actually_Locked (Obj : SB_L_Locker) return Boolean
     is (Obj.Has_Locked);

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locks;
