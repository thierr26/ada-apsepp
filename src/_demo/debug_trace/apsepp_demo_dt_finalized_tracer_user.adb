-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Finalized_Debug_Tracer.Generic_Instantiator;

package body Apsepp_Demo_DT_Finalized_Tracer_User is

   ----------------------------------------------------------------------------

   procedure Finalized_Tracer_User_1 is

      -- Create an instance 'C_D_T' of generic package
      -- 'Apsepp.Finalized_Debug_Tracer.Generic_Instantiator', which
      -- instantiates "controlled debug tracer" object. Such an object outputs
      -- a trace on scope entry and scope exit (with default value for
      -- parameter 'Kind' at least as is the case here).
      --
      -- Instance 'C_D_T' also provides wrappers around
      -- 'Apsepp.Debug_Trace.Debug_Trace.Trace',
      -- 'Apsepp.Debug_Trace.Debug_Trace.Trace_E' and
      -- 'Apsepp.Debug_Trace.Debug_Trace.Trace_Time'.
      --
      -- The use of the Create function implies that both an "Entry" trace and
      -- an "Exit" trace are displayed when entering and exiting the scope.
      --
      -- Parameters of 'Apsepp.Finalized_Debug_Tracer.Generic_Instantiator'
      -- are:
      --
      -- 'Entity_Name' : A string that designates the scope. Can be an empty
      -- string (and in this case the string is not used in the traces).
      --
      -- 'Kind' : A value of enumeration type 'Controlled_Debug_Tracer_Kind'.
      -- Can be one of:
      --
      -- - 'A' (stands for "all"): Both the entry and exit traces are output.
      -- Default value.
      --
      -- - 'I' (stands for "initialization"): Only the entry trace is output.
      --
      -- - 'F' (stands for "finalization"): Only the exit trace is output.
      --
      -- - 'N' (stands for "none"): None of the entry and exit traces is
      -- output.
      package C_D_T is new Apsepp.Finalized_Debug_Tracer.Generic_Instantiator
        (Entity_Name =>
           "Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_1");

   begin
      -- An "Entry" trace is displayed by the controlled debug tracer
      -- instantiated in 'C_D_T' on scope exit.

      -- 'Trace' procedure defined in 'C_D_T' is a wrapper around the 'Trace'
      -- primitive operation of debug trace instance.
      C_D_T.Trace ("Calling Finalized_Tracer_User_2");

      Finalized_Tracer_User_2;

      -- An "Exit" trace is displayed by the controlled debug tracer
      -- instantiated in 'C_D_T' on scope exit.
   end Finalized_Tracer_User_1;

   ----------------------------------------------------------------------------

   procedure Finalized_Tracer_User_2 is

      package C_D_T is new Apsepp.Finalized_Debug_Tracer.Generic_Instantiator
        (Entity_Name =>
           "Apsepp_Demo_DT_Finalized_Tracer_User.Finalized_Tracer_User_2");

      pragma Unreferenced (C_D_T);

   begin

      null; -- Nothing done, only the automatic "Entry" and "Exit" traces of
            -- the controlled debug tracer instantiated in 'C_D_T' are
            -- displayed.

   end Finalized_Tracer_User_2;

   ----------------------------------------------------------------------------

end Apsepp_Demo_DT_Finalized_Tracer_User;
