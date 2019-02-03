-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp_Demo_OSASI_Instance_Controllers;
  use Apsepp_Demo_OSASI_Instance_Controllers;

-- See comments in package bodies Apsepp_Demo_OSASI_Instance_Controllers and
-- Apsepp_Demo_OSASI_Instance_Client.

procedure Apsepp_Demo_Output_Sink_As_Shared_Instance is

begin

   Show_Output_Sink_Instance_State; -- Output line A1.

   Output_Sink_Instance_Controller;

   Show_Output_Sink_Instance_State; -- Output line A4.

   Deeper_Output_Sink_Instance_Controller;

   Show_Output_Sink_Instance_State; -- Output line A6.

   Deeper_Output_Sink_Instance_Controller (J_P => True);

   Show_Output_Sink_Instance_State; -- Output line A8.

end Apsepp_Demo_Output_Sink_As_Shared_Instance;
