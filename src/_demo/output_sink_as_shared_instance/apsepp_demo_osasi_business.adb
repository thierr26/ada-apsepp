-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp_Demo_OSASI_Instance_Client;

package body Apsepp_Demo_OSASI_Business is

   ----------------------------------------------------------------------------

   procedure Run_Business is

      use Apsepp_Demo_OSASI_Instance_Client;

   begin

      Output_Sink_Client;

   end Run_Business;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Business;
