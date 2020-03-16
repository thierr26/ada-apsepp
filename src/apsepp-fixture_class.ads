-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Fixture_Class is

   type Fixture_Interfa is limited interface;

   not overriding
   procedure Set_Up (Obj : Fixture_Interfa) is null;

   not overriding
   procedure Clean_Up (Obj : Fixture_Interfa) is null;

end Apsepp.Fixture_Class;
