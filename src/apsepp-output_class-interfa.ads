-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Output_Class.Interfa is

   type Output_Interfa is limited interface;

   not overriding
   procedure Put_Line (Obj : Output_Interfa; S : String) is null;

end Apsepp.Output_Class.Interfa;
