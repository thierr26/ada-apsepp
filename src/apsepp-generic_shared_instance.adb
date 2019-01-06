-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Generic_Shared_Instance is

   ----------------------------------------------------------------------------

   procedure Unl_CB is

   begin

      if Unlock_CB /= null then
         Unlock_CB.all;
      end if;
      Instance_Access := null;

   end Unl_CB;

   ----------------------------------------------------------------------------

   function Instance return Instance_Type_Access
     is (Instance_Access);

   ----------------------------------------------------------------------------

   function Locked return Boolean
     is (Locked (Lock));

   ----------------------------------------------------------------------------

   function Instantiated return Boolean
     is (Instance_Access /= null);

   ----------------------------------------------------------------------------

end Apsepp.Generic_Shared_Instance;
