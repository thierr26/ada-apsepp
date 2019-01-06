-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   Inst_Access : Instance_Type_Access;
   with procedure CB is null;
package Apsepp.Generic_Shared_Instance.Access_Setter is

   function Has_Actually_Set return Boolean;

end Apsepp.Generic_Shared_Instance.Access_Setter;
