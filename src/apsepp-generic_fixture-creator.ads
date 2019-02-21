-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   with function Allocate return Fixture_Type_Access
     is Default_Allocator;
   Just_Pretend : Boolean := False;
package Apsepp.Generic_Fixture.Creator is

   function Has_Actually_Created return Boolean;

end Apsepp.Generic_Fixture.Creator;
