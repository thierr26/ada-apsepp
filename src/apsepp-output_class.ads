-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Output_Class is

   pragma Pure (Output_Class);

   type Output_Interfa is limited interface;

   not overriding
   procedure Put_Line (Obj : in out Output_Interfa; Item : String) is null;

end Apsepp.Output_Class;
