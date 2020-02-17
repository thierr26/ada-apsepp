-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Sink is

   type Test_Reporter_Sink is limited new Test_Reporter_Interfa with private;

   overriding
   function Is_Conflicting_Node_Tag (Obj      : Test_Reporter_Sink;
                                     Node_Tag : Tag) return Boolean;

   overriding
   procedure Provide_Node_Lineage (Obj          : in out Test_Reporter_Sink;
                                   Node_Lineage :        Tag_Array) is null;

private

   type Test_Reporter_Sink
     is limited new Test_Reporter_Interfa with null record;

end Apsepp.Test_Reporter_Class.Sink;
