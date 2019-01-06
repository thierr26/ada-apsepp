-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Generic_Shared_Instance.Access_Setter is

   ----------------------------------------------------------------------------

   Locker : SB_L_Locker (Lock'Access);

   ----------------------------------------------------------------------------

   function Has_Actually_Set return Boolean
     is (Locker.Has_Actually_Locked);

   ----------------------------------------------------------------------------

begin

   if Locker.Has_Actually_Locked then
      Instance_Access := Inst_Access;
      CB;
   end if;

end Apsepp.Generic_Shared_Instance.Access_Setter;
