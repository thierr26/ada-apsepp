-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private with Apsepp.Output_Class.Standard;

package Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure;

private

   Output_Instance : aliased Apsepp.Output_Class.Standard.Output_Standard;

end Apsepp_Test_Harness;
