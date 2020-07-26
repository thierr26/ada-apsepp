-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Command_Line,
     Apsepp_Demo_DT_Run;

procedure Apsepp_Demo_Debug_Trace is

   File_Name : constant String := (if Ada.Command_Line.Argument_Count > 0 then
                                      Ada.Command_Line.Argument (1)
                                   else
                                      "");

begin

   Apsepp_Demo_DT_Run.Run (File_Name);

end Apsepp_Demo_Debug_Trace;
