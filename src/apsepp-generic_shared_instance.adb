-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Generic_Shared_Instance is

   ----------------------------------------------------------------------------

   procedure Unl_CB is

   begin

      if Unlock_CB /= null then
         Unlock_CB.all;
      end if;

      if Deallocation_Needed then

         declare

            procedure Free is new Ada.Unchecked_Deallocation
              (Object => Instance_Ancestor_Type'Class,
               Name   => Instance_Type_Access);

         begin

            Deallocation_Needed := False;
            Free (Instance_Access);

         end;

      else
         Instance_Access := null;
      end if;

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
