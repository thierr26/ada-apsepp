-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic
   with function Allocate return Instance_Type_Access;
   with procedure CB is null;
   Just_Pretend : Boolean := False;
package Apsepp.Generic_Shared_Instance.Creator is

   function Has_Actually_Created return Boolean;

end Apsepp.Generic_Shared_Instance.Creator;
