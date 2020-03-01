-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp_Test_Suite,
             Apsepp.Output_Class.Standard;

package Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure;

private

   use Apsepp_Test_Suite,
       Apsepp.Output_Class.Standard;

   type Output_Standard_Access is access Output_Standard;

   Test_Suite : aliased Apsepp_T_S;

end Apsepp_Test_Harness;
