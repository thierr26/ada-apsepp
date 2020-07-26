-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace,
     Apsepp.Debug_Trace_Class.File,
     Apsepp.Debug_Trace_Class.Output,
     Apsepp.Output_Class.Standard;

package Apsepp_Demo_DT_Run is

   procedure Run (File_Name : String);

private

   use Apsepp.Debug_Trace_Class.File,
       Apsepp.Debug_Trace_Class.Output,
       Apsepp.Output_Class.Standard;

   -- Declare the shared instances at library level to avoid an accessibility
   -- check failure at run-time.
   Debug_Trace_Instance_F : aliased Debug_Trace_File;
   Debug_Trace_Instance_O : aliased Debug_Trace_Output;
   Output_Instance        : aliased Output_Standard;

   -- Extend the lock holder type, to make it possible to take specific
   -- cleanup actions on release.
   type Holder_W_Debug_Trace_File_Cleanup
     is new Apsepp.Debug_Trace.Debug_Trace_Shared_Instance.Holder
     with null record;

   overriding
   procedure On_Release (Obj : Holder_W_Debug_Trace_File_Cleanup);

end Apsepp_Demo_DT_Run;
