-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Stub is

   type Test_Reporter_Stub is new Test_Reporter_Interfa with private;

private

   type Test_Reporter_Stub is new Test_Reporter_Interfa with null record;

end Apsepp.Test_Reporter_Class.Stub;
