-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Reporter_Class.Sink is

   ----------------------------------------------------------------------------

   overriding
   function Is_Conflicting_Node_Tag (Obj      : Test_Reporter_Sink;
                                     Node_Tag : Tag) return Boolean
     is (False);
   -- PORT: Defining this expression function directly in the package
   -- specification causes a warning to be issued (unreferenced 'Obj').
   -- <2020-02-17>

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Sink;
