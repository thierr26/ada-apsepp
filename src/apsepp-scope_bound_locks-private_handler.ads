-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private package Apsepp.Scope_Bound_Locks.Private_Handler is

   ----------------------------------------------------------------------------

   protected Protected_Handler is

      procedure Do_Lock (Lock            :     SB_Lock_Access;
                         Actually_Locked : out Boolean)

        with Post => Locked (Lock.all);

      procedure Do_Unlock (Lock : SB_Lock_Access)

        with Post => not Locked (Lock.all);

   end Protected_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locks.Private_Handler;
