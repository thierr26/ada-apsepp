-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp_Demo_OSASI_Instance_Client;

package body Apsepp_Demo_OSASI_Business is

   ----------------------------------------------------------------------------

   procedure Run_Business is

      use Apsepp_Demo_OSASI_Instance_Client;

   begin

      Output_Sink_Client; -- Output line D01, D02, D05, D06.

   end Run_Business;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Business;
