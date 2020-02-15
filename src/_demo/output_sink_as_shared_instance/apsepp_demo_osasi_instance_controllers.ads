-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- "Private with" the needed implementation packages of class output sink.
private with Apsepp.Output_Class.Standard,
             Apsepp.Output_Class.Quiet,
             Output_Exclam_Impl;

package Apsepp_Demo_OSASI_Instance_Controllers is

   procedure Show_Output_Sink_Instance_State;

   procedure Output_Sink_Instance_Controller;

   procedure Deeper_Output_Sink_Instance_Controller;

   procedure Output_Sink_Instance_Controller_Custom_Instance;

private

   -- Make types 'Apsepp.Output_Class.Standard.Output_Standard' and
   -- 'Apsepp.Output_Class.Quiet.Output_Quiet' visible.
   use Apsepp.Output_Class.Standard,
       Apsepp.Output_Class.Quiet;

   -- Define access types to the needed output sink concrete types.
   type Output_Standard_Access is access Output_Standard;
   type Output_Quiet_Access    is access Output_Quiet;

   -- Define an instance of 'Output_Exclam_Impl.Output_Exclam'.
   Output_Exclam_Instance : Output_Exclam_Impl.Output_Exclam;

end Apsepp_Demo_OSASI_Instance_Controllers;
