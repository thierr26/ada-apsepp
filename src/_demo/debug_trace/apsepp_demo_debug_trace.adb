-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Debug_Trace;
with Apsepp.Debug_Trace_Class.Output.Create;
with Apsepp.Output;
with Apsepp.Output_Class.Standard.Create;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp_Demo_DT_Instance_Client; use Apsepp_Demo_DT_Instance_Client;
with Apsepp_Demo_DT_Scope_Debug_User; use Apsepp_Demo_DT_Scope_Debug_User;

procedure Apsepp_Demo_Debug_Trace is

      -----------------------------------------------------

      -- Allocation of the debug trace shared instance and of the output sink
      -- shared instance. See demo program
      -- Apsepp_Demo_Output_Sink_As_Shared_Instance and package body
      -- Apsepp_Demo_OSASI_Instance_Controllers for details about shared
      -- instance creation.

      use Apsepp.Debug_Trace_Class.Output;

      -- Allocator function (allocates an instance of type
      -- Apsepp.Debug_Trace_Class.Output.Debug_Trace_Output).
      function Allocate_Debug_Trace_Output
        return Apsepp.Debug_Trace.Debug_Trace_Access
        is (new Debug_Trace_Output'(Apsepp.Debug_Trace_Class.Output.Create));

      package Debug_Trace_Output_Creator
        is new Apsepp.Debug_Trace.Shared_Instance.Creator
        (Allocate => Allocate_Debug_Trace_Output);

      use Apsepp.Output_Class.Standard;

      -- Allocator function (allocates an instance of type
      -- Apsepp.Output_Class.Standard.Output_Standard).
      function Allocate_Output_Standard return Apsepp.Output.Output_Access
        is (new Output_Standard'(Apsepp.Output_Class.Standard.Create));

      package Output_Standard_Creator
        is new Apsepp.Output.Shared_Instance.Creator
        (Allocate => Allocate_Output_Standard);

      -----------------------------------------------------

      pragma Unreferenced (Debug_Trace_Output_Creator,
                           Output_Standard_Creator);

      -----------------------------------------------------

begin

   -- Call debug trace user subprograms.

   Instance_Client;

   Scope_Debug_User_1;

   Scope_Debug_User_2;

end Apsepp_Demo_Debug_Trace;
