-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;
with Ada.Tags;

-- "With" the shared output sink instance access point package.
with Apsepp.Output;

-- "With" the generic instance creator package (useful to instantiate the
-- Creator child package of Apsepp.Output.Shared_Instance).
with Apsepp.Generic_Shared_Instance.Creator;

-- "With" the needed creational function(s) for objects of class output sink.
with Apsepp.Output_Class.Standard;
with Apsepp.Output_Class.Quiet.Create;

-- "With" the needed unit(s) needed to perform the application business. They
-- can reference the shared output sink instance access point package
-- (Apsepp.Output), and or reference other units that reference it.
with Apsepp_Demo_OSASI_Business;

package body Apsepp_Demo_OSASI_Instance_Controllers is

   ----------------------------------------------------------------------------

   A_Count : Natural := 0;

   function A return String is

      N : constant Natural := A_Count + 1;
      Ret : String := Natural'Image (N) & " ";

   begin

      Ret (Ret'First) := 'A';

      A_Count := N;

      return Ret;

   end A;

   ----------------------------------------------------------------------------

   C_Count : Natural := 0;

   function C return String is

      N : constant Natural := C_Count + 1;
      Ret : String := Natural'Image (N) & " ";

   begin

      Ret (Ret'First) := 'C';

      C_Count := N;

      return Ret;

   end C;

   ----------------------------------------------------------------------------

   D_Count : Natural := 0;

   function D return String is

      N : constant Natural := D_Count + 1;
      Ret : String := Natural'Image (N) & " ";

   begin

      Ret (Ret'First) := 'D';

      D_Count := N;

      return Ret;

   end D;

   ----------------------------------------------------------------------------

   procedure Show_Output_Sink_Instance_State is

      use Ada.Text_IO;

      use Apsepp.Output.Shared_Instance;
                    -- Makes functions Apsepp.Output.Shared_Instance.Locked and
                    -- Apsepp.Output.Shared_Instance.Instantiated visible.

   begin

      Put_Line (A & "Output sink instance state: "
                  &
                (if Locked then "locked" else "unlocked")
                  &
                ", "
                  &
                (if Instantiated then "instantiated" else "not instantiated")
                  &
                ".");

   end Show_Output_Sink_Instance_State;

   ----------------------------------------------------------------------------

   procedure Tell_If_Has_Allocated (Proc_Name : String;
                                    F         : Boolean;
                                    J_P       : Boolean := False) is

      use Ada.Text_IO;

   begin

      Put_Line (C & Proc_Name & " has "
                  &
                (if F then "" else "not ")
                  &
                "allocated"
                  &
                (if J_P then " (well, has just pretented to allocate)" else "")
                  &
                ".");

   end Tell_If_Has_Allocated;

   ----------------------------------------------------------------------------

   procedure Output_Sink_Instance_Controller is

      use Apsepp.Output; -- Makes type Apsepp.Output.Output_Access visible.
      use Apsepp.Output_Class.Standard;
                         -- Makes type
                         -- Apsepp.Output_Class.Standard.Output_Standard
                         -- visible.

      -----------------------------------------------------

      -- Allocator function (allocates an instance of type
      -- Apsepp.Output_Class.Standard.Output_Standard).
      function Allocate_Output_Standard return Output_Access
        is (new Output_Standard);

      -----------------------------------------------------

      -- Callback procedure, provided as the CB parameter on instantiation of
      -- the Creator child package of Apsepp.Output.Shared_Instance.
      procedure Callback is

         use Ada.Text_IO;

      begin

         Put_Line (D & "Output sink instance tag is: "
                     & Ada.Tags.Expanded_Name (Apsepp.Output.Output'Tag));
                                                             -- Output line D1.

      end Callback;

      -----------------------------------------------------

      -- Instantiate the Creator child package of
      -- Apsepp.Output.Shared_Instance (which is itself an intance of
      -- Apsepp.Generic_Shared_Instance).
      --
      -- This will automatically allocate an output sink instance of type
      -- Apsepp.Output_Class.Standard.Output_Standard via a call to
      -- Allocate_Output_Standard and set it as the shared output sink instance
      -- is not locked (that is if none of the caller, grand-caller,
      -- great-grand-caller and so forth has already set (or pretented to set)
      -- an instance).
      --
      -- The instance is accessible from any point of the program via function
      -- Apsepp.Output.Output.
      --
      -- The instance will be deallocated at the exit point of the scope (here
      -- the return point of procedure Output_Sink_Instance_Controller).
      --
      -- Note the CB parameter in the generic instantiation: It is a callback
      -- procedure, called immediately after the instance allocation.
      --
      -- Other callbacks may occur on locking and or unlocking, depending on
      -- how Apsepp.Generic_Shared_Instance has been instantiated by
      -- Apsepp.Output. See the Lock_CB and Unlock_CB formal parameters of
      -- Apsepp.Generic_Shared_Instance.
      --
      -- Note also that Apsepp.Generic_Shared_Instance has another child
      -- (Access_Setter), similar to Creator, except that you provide an access
      -- to an already allocated instance instead of an allocator function.
      package Output_Standard_Creator
        is new Shared_Instance.Creator (Allocate => Allocate_Output_Standard,
                                        CB       => Callback);

      -----------------------------------------------------

   begin

      Show_Output_Sink_Instance_State; -- Output line A2.

      Tell_If_Has_Allocated ("Output_Sink_Instance_Controller",
                             Output_Standard_Creator.Has_Actually_Created);
                                       -- Output line C1.

      Apsepp_Demo_OSASI_Business.Run_Business;
                                       -- Output line B1.

      Deeper_Output_Sink_Instance_Controller;

   end Output_Sink_Instance_Controller;

   ----------------------------------------------------------------------------

   procedure Deeper_Output_Sink_Instance_Controller (J_P : Boolean := False) is

      use Apsepp.Output; -- Makes type Apsepp.Output.Output_Access visible.
      use Apsepp.Output_Class.Quiet;
                         -- Makes type
                         -- Apsepp.Output_Class.Standard.Output_Quiet
                         -- visible.

      -----------------------------------------------------

      -- Allocator function (allocates an instance of type
      -- Apsepp.Output_Class.Quiet.Output_Quiet).
      function Allocate_Output_Quiet return Output_Access
        is (new Output_Quiet'(Apsepp.Output_Class.Quiet.Create));

      -----------------------------------------------------

      -- Similar to Output_Standard_Creator in procedure
      -- Output_Sink_Instance_Controller, except that:
      --
      -- - It allocates an output sink instance of type
      --   Apsepp.Output_Class.Quiet.Output_Quiet.
      --
      -- - There's no callback procedure provided.
      --
      -- - The Just_Pretend parameter (which defaults to False) may be True
      --   (depending on the procedure parameter). A True value implies that
      --   the instance allocation won't be done and the callback procedure not
      --   called (if any). But locking is done normally. This "just pretend"
      --   thing makes it possible to do some kind of "dry runs" to check
      --   whether allocation would occur or not.
      package Output_Quiet_Creator
        is new Shared_Instance.Creator (Allocate     => Allocate_Output_Quiet,
                                        Just_Pretend => J_P);

      -----------------------------------------------------

   begin

      Show_Output_Sink_Instance_State; -- Output line A3, A5, A7.

      Tell_If_Has_Allocated ("Deeper_Output_Sink_Instance_Controller",
                             Output_Quiet_Creator.Has_Actually_Created,
                             J_P);
                                       -- Output line C2, C3, C4.

      if not J_P then
         Apsepp_Demo_OSASI_Business.Run_Business;
                                       -- Output line B2.
      end if;

   end Deeper_Output_Sink_Instance_Controller;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Instance_Controllers;
