-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance.Creator;

package body Apsepp.Generic_Fixture.Creator is

   ----------------------------------------------------------------------------

   function Alloc return Shared_Instance.Instance_Type_Access
     is (Shared_Instance.Instance_Type_Access (Allocate));

   ----------------------------------------------------------------------------

   package Shared_Instance_Creator is new Shared_Instance.Creator
     (Allocate     => Alloc,
      Just_Pretend => Just_Pretend);

   ----------------------------------------------------------------------------

   function Has_Actually_Created return Boolean
     is (Shared_Instance_Creator.Has_Actually_Created);

   ----------------------------------------------------------------------------

end Apsepp.Generic_Fixture.Creator;
