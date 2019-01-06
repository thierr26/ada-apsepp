-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks;
with Apsepp.Output.Config;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test is

      use Apsepp.Scope_Bound_Locks;
      use Apsepp.Output;

      Output_Locker : aliased SB_L_Locker (Apsepp.Output.Config.Lock'Access);

   begin

      Apsepp.Output.Config.Setup (Output_Locker'Access,
                                  Output_Instance'Access);
      Output.Put_Line ("Hello world!");

   end Apsepp_Test;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
