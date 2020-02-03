-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Scope_Bound_Locks.Private_Handler is

   ----------------------------------------------------------------------------

   protected body Protected_Handler is

      -----------------------------------------------------

      procedure Do_Lock (Lock            :     SB_Lock_Access;
                         Actually_Locked : out Boolean) is

      begin

         Actually_Locked := not Locked (Lock.all);
         Lock.Is_Locked := True;
         if Actually_Locked and then Lock.Lock_CB /= null then
            Lock.Lock_CB.all;
         end if;

      end Do_Lock;

      -----------------------------------------------------

      procedure Do_Unlock (Lock : SB_Lock_Access) is

      begin

         if Lock.Unlock_CB /= null then
            Lock.Unlock_CB.all;
         end if;
         Lock.Is_Locked := False;

      end Do_Unlock;

      -----------------------------------------------------

   end Protected_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locks.Private_Handler;
