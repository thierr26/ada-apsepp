-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Fixture is

   -- Default_Allocator is referenced by Creator child package only.
   pragma Unreferenced (Default_Allocator);

   ----------------------------------------------------------------------------

   function Instance return Fixture_Type_Access
     is (Fixture_Type_Access (Shared_Instance.Instance));

   ----------------------------------------------------------------------------

end Apsepp.Generic_Fixture;
