-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace_Class.Output,
     Apsepp.Output_Class.Standard;

package Apsepp_Demo_DT_Run is

   procedure Run;

private

   use Apsepp.Debug_Trace_Class.Output,
       Apsepp.Output_Class.Standard;

   -- Declare the shared instances at library level to avoid an accessibility
   -- check failure at run-time.
   Debug_Trace_Instance : aliased Debug_Trace_Output;
   Output_Instance      : aliased Output_Standard;

end Apsepp_Demo_DT_Run;
