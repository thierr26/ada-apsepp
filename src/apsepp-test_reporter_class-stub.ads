-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Stub is

   type Test_Reporter_Stub is new Test_Reporter_Interfa with private;

   overriding
   function Unreported_Routine_Exception_Details
     (Obj : Test_Reporter_Stub) return Boolean
     is (False);

private

   type Test_Reporter_Stub is new Test_Reporter_Interfa with null record;

end Apsepp.Test_Reporter_Class.Stub;
