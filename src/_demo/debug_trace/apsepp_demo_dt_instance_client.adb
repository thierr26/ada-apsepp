-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

-- "With" the shared debug trace instance access point package.
with Apsepp.Debug_Trace;

package body Apsepp_Demo_DT_Instance_Client is

   ----------------------------------------------------------------------------

   procedure Instance_Client is

      use Apsepp.Debug_Trace; -- Makes function Apsepp.Debug_Trace.Debug_Trace
                              -- visible (access to the shared debug trace
                              -- instance).

   begin

      -- Call primitive operations of debug trace instance.

      Debug_Trace.Trace ("Simple trace line");

      Debug_Trace.Trace
        ("Trace with entity name",
         "Apsepp_Demo_DT_Instance_Client.Instance_Client");

   end Instance_Client;

   ----------------------------------------------------------------------------

end Apsepp_Demo_DT_Instance_Client;
