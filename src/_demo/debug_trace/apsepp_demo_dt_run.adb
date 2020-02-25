-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace,
     Apsepp.Output,
     Apsepp.Generic_Shared_Instance.Finalized_S_R,
     Apsepp_Demo_DT_Instance_Client,
     Apsepp_Demo_DT_Finalized_Tracer_User;

package body Apsepp_Demo_DT_Run is

   ----------------------------------------------------------------------------

   procedure Run is

      -- Set up the debug trace and output sink shared instances. See demo
      -- program 'Apsepp_Demo_Output_Sink_As_Shared_Instance' and package body
      -- 'Apsepp_Demo_OSASI_Instance_Controllers' for details about setting up
      -- shared instances.

      use Apsepp.Debug_Trace,
          Apsepp.Output;

      Debug_Trace_Lock_Holder : Debug_Trace_Shared_Instance.Holder;

      package Debug_Trace_S_R is new Debug_Trace_Shared_Instance.Finalized_S_R
        (Instance_Access      => Debug_Trace_Instance'Access,
         Lock_Holder_Type     => Debug_Trace_Shared_Instance.Holder,
         Lock_Holder_Instance => Debug_Trace_Lock_Holder);

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R
        (Instance_Access      => Output_Instance'Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Debug_Trace_S_R, Output_S_R);

   begin

      -- Call debug trace user subprograms.

      Apsepp_Demo_DT_Instance_Client.Instance_Client;

      Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_1;

      Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_2;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Demo_DT_Run;
