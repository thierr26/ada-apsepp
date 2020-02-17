-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Private_Test_Reporter is

   ----------------------------------------------------------------------------

   function Test_Reporter return not null access Test_Reporter_Interfa'Class
     is (Test_Reporter_Shared_Instance.Instance_Access);

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Private_Test_Reporter;
