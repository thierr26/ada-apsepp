-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Directories,
     Ada.Assertions,
     Apsepp.Output,
     Apsepp.Generic_Shared_Instance.Finalized_S_R,
     Apsepp_Demo_DT_Instance_Client,
     Apsepp_Demo_DT_Finalized_Tracer_User;

package body Apsepp_Demo_DT_Run is

   ----------------------------------------------------------------------------

   overriding
   procedure On_Release (Obj : Holder_W_Debug_Trace_File_Cleanup) is

      pragma Unreferenced (Obj);

   begin

      Debug_Trace_Instance_F.Clean_Up;

   end On_Release;

   ----------------------------------------------------------------------------

   procedure Work is

   begin

      -- Call debug trace user subprograms.

      Apsepp_Demo_DT_Instance_Client.Instance_Client;

      Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_1;

      Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_2;

   end Work;

   ----------------------------------------------------------------------------

   procedure Run_And_Output_To_Standard is

      -- Set up the debug trace and output sink shared instances. See demo
      -- program 'Apsepp_Demo_Output_Sink_As_Shared_Instance' and package body
      -- 'Apsepp_Demo_OSASI_Instance_Controllers' for details about setting up
      -- shared instances.

      use Apsepp.Debug_Trace,
          Apsepp.Output;

      Debug_Trace_Lock_Holder : Debug_Trace_Shared_Instance.Holder;

      package Debug_Trace_S_R is new Debug_Trace_Shared_Instance.Finalized_S_R
        (Instance_Access      => Debug_Trace_Instance_O'Access,
         Lock_Holder_Type     => Debug_Trace_Shared_Instance.Holder,
         Lock_Holder_Instance => Debug_Trace_Lock_Holder);

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R
        (Instance_Access      => Output_Instance'Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Debug_Trace_S_R, Output_S_R);

   begin

      if Debug_Trace_Lock_Holder.Holds then
         -- We're holding the lock.

         -- Set the precision of the displayed time (number of digits in
         -- fractional part of seconds).
         Debug_Trace_Instance_O.Set_Up (Time_Fraction_Digits => 6);
      end if;

      Work;

   end Run_And_Output_To_Standard;

   ----------------------------------------------------------------------------

   procedure Run_And_Output_To_File (File_Name : String) is

      -- Set up the debug trace and output sink shared instances. See demo
      -- program 'Apsepp_Demo_Output_Sink_As_Shared_Instance' and package body
      -- 'Apsepp_Demo_OSASI_Instance_Controllers' for details about setting up
      -- shared instances.

      use Apsepp.Debug_Trace,
          Apsepp.Output;

      Debug_Trace_Lock_Holder : Holder_W_Debug_Trace_File_Cleanup;

      package Debug_Trace_S_R is new Debug_Trace_Shared_Instance.Finalized_S_R
        (Instance_Access      => Debug_Trace_Instance_F'Access,
         Lock_Holder_Type     => Holder_W_Debug_Trace_File_Cleanup,
         Lock_Holder_Instance => Debug_Trace_Lock_Holder);

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R
        (Instance_Access      => Output_Instance'Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Debug_Trace_S_R, Output_S_R);

   begin

      if Debug_Trace_Lock_Holder.Holds then
         -- We're holding the lock.

         -- Set the precision of the displayed time (number of digits in
         -- fractional part of seconds).
         Debug_Trace_Instance_F.Set_Up (File_Name            => File_Name,
                                        Time_Fraction_Digits => 6);
      end if;

      Work;

   end Run_And_Output_To_File;

   ----------------------------------------------------------------------------

   procedure Run (File_Name : String) is

   begin

      if File_Name'Length = 0 then
         -- No file name given by user.

         -- Output to standard output.
         Run_And_Output_To_Standard;

      else
         -- User has given a file name.

         -- Don't overwrite an existing file.
         Ada.Assertions.Assert (not Ada.Directories.Exists (File_Name),
                                File_Name & " already exists. Aborting.");

         -- Output to file.
         Run_And_Output_To_File (File_Name);
      end if;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Demo_DT_Run;
