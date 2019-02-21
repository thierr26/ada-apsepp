-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   Fallback_Instance : Instance_Type_Access;
package Apsepp.Generic_Shared_Instance.Fallback_Switch is

   function Instance_FS return Instance_Type_Access
     is (if Instantiated then
            Instance
         else
            Fallback_Instance);

end Apsepp.Generic_Shared_Instance.Fallback_Switch;
