-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

procedure Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert
  (Cond    : Boolean;
   Message : String := "") is

begin

   Assert (Test_Case_Tag, Cond, Message);

end Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert;
