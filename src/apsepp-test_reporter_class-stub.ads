-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Stub is

   type Test_Reporter_Stub is limited new Test_Reporter_Interfa with private;

   overriding
   procedure Provide_Node_Lineage (Obj          : in out Test_Reporter_Stub;
                                   Node_Lineage :        Tag_Array;
                                   Active       :    out Boolean) is null;

private

   type Test_Reporter_Stub
     is limited new Test_Reporter_Interfa with null record;

end Apsepp.Test_Reporter_Class.Stub;
