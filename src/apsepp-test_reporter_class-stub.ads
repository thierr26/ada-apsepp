-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Stub is

   type Test_Reporter_Stub is limited new Test_Reporter_Interfa with private;

   overriding
   function Is_Conflicting_Node_Tag (Obj      : Test_Reporter_Stub;
                                     Node_Tag : Tag) return Boolean
     is (False);

   overriding
   procedure Provide_Node_Lineage (Obj          : in out Test_Reporter_Stub;
                                   Node_Lineage :        Tag_Array) is null;

private

   type Test_Reporter_Stub
     is limited new Test_Reporter_Interfa with null record;

end Apsepp.Test_Reporter_Class.Stub;
