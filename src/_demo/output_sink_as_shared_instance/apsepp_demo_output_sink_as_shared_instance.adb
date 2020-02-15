-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp_Demo_OSASI_Instance_Controllers;
  use Apsepp_Demo_OSASI_Instance_Controllers;

-- See comments in package bodies 'Apsepp_Demo_OSASI_Instance_Controllers' and
-- 'Apsepp_Demo_OSASI_Instance_Client'.

procedure Apsepp_Demo_Output_Sink_As_Shared_Instance is

begin

   Show_Output_Sink_Instance_State; -- Output line A01, B01.

   Output_Sink_Instance_Controller;

   Show_Output_Sink_Instance_State; -- Output line A04, B04.

   Deeper_Output_Sink_Instance_Controller;

   Show_Output_Sink_Instance_State; -- Output line A06, B06.

   Deeper_Output_Sink_Instance_Controller;

   Show_Output_Sink_Instance_State; -- Output line A08, B08.

   Output_Sink_Instance_Controller_Custom_Instance;

   Show_Output_Sink_Instance_State; -- Output line A11, B11.

end Apsepp_Demo_Output_Sink_As_Shared_Instance;
