-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Generic_Shared_Instance.Creator is

   ----------------------------------------------------------------------------

   Locker : SB_L_Locker (Lock'Access);

   ----------------------------------------------------------------------------

   function Has_Actually_Created return Boolean
     is (Locker.Has_Actually_Locked);

   ----------------------------------------------------------------------------

begin

   if not Just_Pretend and then Locker.Has_Actually_Locked then

      declare

         Inst : aliased Instance_Type := Create;

      begin

         Instance_Access := Inst'Unchecked_Access;
         CB;

      end;

   end if;

end Apsepp.Generic_Shared_Instance.Creator;
