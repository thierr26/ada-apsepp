-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Output_Class.Standard,
             Apsepp_Scope_Bound_Locking_Test_Case;

package Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure;

private

   use Apsepp.Output_Class.Standard,
       Apsepp_Scope_Bound_Locking_Test_Case;

   type Output_Standard_Access is access Output_Standard;

   Scope_Bound_Locking_Test_Case : Apsepp_Scope_Bound_Locking_T_C;

end Apsepp_Test_Harness;
