-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Debug_Trace_Class.Stub; use Apsepp.Debug_Trace_Class.Stub;

package Apsepp.Debug_Trace_Class.Standard is

   type Debug_Trace_Standard is limited new Debug_Trace_Stub with private;

   overriding
   procedure Trace (Obj         : Debug_Trace_Standard;
                    Message     : String;
                    Entity_Name : String             := "");

private

   type Debug_Trace_Standard is limited new Debug_Trace_Stub with null record;

end Apsepp.Debug_Trace_Class.Standard;
