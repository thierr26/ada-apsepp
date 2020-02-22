-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO,
     Ada.Tags,
     Generic_Char_Count_String,
     Apsepp_Demo_OSASI_Constants;

-- "With" the shared output sink instance access point package.
with Apsepp.Output;

-- "With" the generic instance set / reset package (useful to instantiate the
-- 'Finalized_S_R_Dealloc' child package of
-- 'Apsepp.Output.Output_Shared_Instance').
with Apsepp.Generic_Shared_Instance.Finalized_S_R_Dealloc;

-- "With" the generic instance set / reset package (useful to instantiate the
-- 'Finalized_S_R' child package of 'Apsepp.Output.Output_Shared_Instance').
with Apsepp.Generic_Shared_Instance.Finalized_S_R;

-- "With" the needed unit(s) needed to perform the application business. They
-- can reference the shared output sink instance access point package
-- ('Apsepp.Output'), and/or reference other units that reference it.
with Apsepp_Demo_OSASI_Business;

package body Apsepp_Demo_OSASI_Instance_Controllers is

   use Apsepp_Demo_OSASI_Constants;

   ----------------------------------------------------------------------------

   package A_Count_String is new Generic_Char_Count_String
     (Char        => 'A',
      Suffix      => " ",
      Count_Width => Line_Count_Width);

   function A return A_Count_String.Char_Count_String renames A_Count_String.S;

   ----------------------------------------------------------------------------

   package B_Count_String is new Generic_Char_Count_String
     (Char        => 'B',
      Suffix      => " ",
      Count_Width => Line_Count_Width);

   function B return B_Count_String.Char_Count_String renames B_Count_String.S;

   ----------------------------------------------------------------------------

   package C_Count_String is new Generic_Char_Count_String
     (Char        => 'C',
      Suffix      => " ",
      Count_Width => Line_Count_Width);

   function C return C_Count_String.Char_Count_String renames C_Count_String.S;

   ----------------------------------------------------------------------------

   procedure Show_Output_Sink_Instance_State is

      -- Make the output sink shared instance lock
      -- ('Apsepp.Output.Output_Shared_Instance.Instance_Lock') and the
      -- unguarded access function to the instance
      -- ('Apsepp.Output.Output_Shared_Instance.Unguarded_Instance_Access')
      -- visible.
      use Apsepp.Output.Output_Shared_Instance;

      use Ada.Tags;

   begin

      Ada.Text_IO.Put_Line (A
                            & "Output sink instance state: "
                            & (if Instance_Lock.Locked then
                                  "locked"
                               else
                                  "unlocked")
                            & ", "
                            & (if Unguarded_Instance_Access /= null then
                                  "instantiated"
                               else
                                  "not instantiated")
                            & ".");

      -- Note that the 'Unguarded_Instance_Access /= null' would be always
      -- false if non-null actual for the 'Fallback_Instance_Access' formal
      -- parameter had been provided when instantiating
      -- 'Apsepp.Generic_Shared_Instance' in 'Apsepp.Output'.
      --
      -- 'Unguarded_Instance_Access' returns the "raw" access to the shared
      -- instance if it is non-null, and 'Fallback_Instance_Access' otherwise
      -- (which can be null).
      --
      -- 'Instance_Access' is similar to 'Unguarded_Instance_Access', but it
      -- has a barrier. The barrier is closed if the shared instance is locked
      -- and the "raw" access to the shared instance is null. This prevents a
      -- client accessing the instance after locking but before instance access
      -- assignment from getting a null access.

      Ada.Text_IO.Put_Line (B
                            & "Output sink instance tag: "
                            & (if Unguarded_Instance_Access /= null then
                                  Expanded_Name (Unguarded_Instance_Access'Tag)
                               else
                                  "N/A")
                            & ".");

   end Show_Output_Sink_Instance_State;

   ----------------------------------------------------------------------------

   procedure Tell_If_Holds
     (Proc_Name : String;
      Holder    : Apsepp.Output.Output_Shared_Instance.Holder) is

   begin

      Ada.Text_IO.Put_Line (C
                            & Proc_Name
                            & " does "
                            & (if Holder.Holds then
                                  ""
                               else
                                  "not ")
                            & "hold.");

   end Tell_If_Holds;

   ----------------------------------------------------------------------------

   procedure Output_Sink_Instance_Controller is

      use Apsepp.Output; -- Makes package
                         -- 'Apsepp.Output.Output_Shared_Instance' visible.

      -- Declare a lock holder. It must of a type be derived from
      -- 'Apsepp.Scope_Bound_Locking.Controlled_Lock_Holder' and have an 'L'
      -- discriminant pointing to the output sink instance lock
      -- ('Apsepp.Output.Output_Shared_Instance.Instance_Lock'). The lock
      -- holder automatically takes the lock if it's not already held by
      -- another holder and releases it automatically when going out of scope.
      Output_Lock_Holder : Output_Shared_Instance.Holder;

      -- Allocate an output sink instance, but only if the lock holder declared
      -- above has actually taken the lock (i.e. "holds" the lock). Providing a
      -- non-null value when the lock holder does not hold is probably useless.
      -- And providing a null value when the lock holder holds is forbidden and
      -- causes an assertion failure (in
      -- 'Apsepp.Generic_Shared_Instance.Parameterized_S').
      Output_Instance_Access : constant Output_Standard_Access
        := (if Output_Lock_Holder.Holds then
               new Output_Standard
            else
               null);

      -- Instantiate the 'Finalized_S_R_Dealloc' child of
      -- 'Apsepp.Output.Output_Shared_Instance'. This causes the allocated
      -- instance to be automatically set as the output sink instance. The
      -- instance will be reset automatically on scope exit (before lock
      -- release). And the allocated storage is reclaimed (via a
      -- 'Ada.Unchecked_Deallocation' call in
      -- 'Apsepp.Generic_Shared_Instance.Parameterized_R').
      package Output_S_R is new Output_Shared_Instance.Finalized_S_R_Dealloc
        (Instance_Access      => Output_Instance_Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Output_S_R);

   begin

      Show_Output_Sink_Instance_State; -- Output line A02, B02.

      Tell_If_Holds
        ("Output_Sink_Instance_Controller",
         Output_Lock_Holder);          -- Output line C01.

      Apsepp_Demo_OSASI_Business.Run_Business;
                                       -- Output line D01.

      Deeper_Output_Sink_Instance_Controller;

   end Output_Sink_Instance_Controller;

   ----------------------------------------------------------------------------

   procedure Deeper_Output_Sink_Instance_Controller is

      -- Comments in 'Output_Sink_Instance_Controller' apply here as well.

      use Apsepp.Output;

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      Output_Instance_Access : constant Output_Quiet_Access
        := (if Output_Lock_Holder.Holds then
               new Output_Quiet
            else
               null);

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R_Dealloc
        (Instance_Access      => Output_Instance_Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Output_S_R);

   begin

      Show_Output_Sink_Instance_State;
                         -- Output line A03, B03, A05, B05, A07, B07, A10, B10.

      Tell_If_Holds
        ("Deeper_Output_Sink_Instance_Controller",
         Output_Lock_Holder);
                         -- Output line C02, C03, C04, C06.

      Apsepp_Demo_OSASI_Business.Run_Business;
                         -- Output line D02, D06.

   end Deeper_Output_Sink_Instance_Controller;

   ----------------------------------------------------------------------------

   procedure Output_Sink_Instance_Controller_Custom_Instance is

      -- This procedure is similar to 'Output_Sink_Instance_Controller' except
      -- that it instantiates the 'Finalized_S_R' child of
      -- 'Apsepp.Output.Output_Shared_Instance' instead of the
      -- 'Finalized_S_R_Dealloc' child. Instantiating the 'Finalized_S_R' child
      -- is appropriate when the shared instance is already declared or
      -- allocated (here 'Output_Exclam_Instance' is declared in the private
      -- part of the package specification).
      --
      -- Note that the 'Instance_Access' formal of the 'Finalized_S_R' child
      -- has null exclusion. And the 'Finalized_S_R' child does not attempt to
      -- deallocate the shared instance on scope exit.

      use Apsepp.Output;

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R
        (Instance_Access      => Output_Exclam_Instance'Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      pragma Unreferenced (Output_S_R);

   begin

      Show_Output_Sink_Instance_State; -- Output line A09, B09.

      Tell_If_Holds
        ("Output_Sink_Instance_Controller_Custom_Instance",
         Output_Lock_Holder);          -- Output line C05.

      Apsepp_Demo_OSASI_Business.Run_Business;
                                       -- Output line D05.

      Deeper_Output_Sink_Instance_Controller;

   end Output_Sink_Instance_Controller_Custom_Instance;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Instance_Controllers;
