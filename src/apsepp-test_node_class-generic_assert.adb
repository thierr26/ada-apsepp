-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

procedure Apsepp.Test_Node_Class.Generic_Assert (Cond    : Boolean;
                                                 Message : String := "") is

begin

   Assert (Test_Node_Tag, Cond, Message);

end;
