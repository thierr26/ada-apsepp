-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Generic_Shared_Instance;
with Apsepp.Scope_Bound_Locks; use Apsepp.Scope_Bound_Locks;

generic
   type Fixture_Type is tagged limited private;
   type Fixture_Type_Access is not null access all Fixture_Type;
   with function Default_Allocator return Fixture_Type_Access;
   Lock_CB, Unlock_CB : SB_Lock_CB := null;
package Apsepp.Generic_Fixture is

   function Instance return Fixture_Type_Access;

private

   package Shared_Instance is new Generic_Shared_Instance
     (Instance_Ancestor_Type => Fixture_Type,
      Lock_CB                => Lock_CB,
      Unlock_CB              => Unlock_CB);

end Apsepp.Generic_Fixture;
