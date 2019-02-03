-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp.Output;
with Apsepp.Output_Class.Standard.Create;
with Apsepp.Trace_Debugging;
with Apsepp.Trace_Debugging_Class.Output.Create;
with Apsepp.Controlled_Trace_Debugging;

package body Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure is

      -----------------------------------------------------

      function Allocate_Output_Standard return Apsepp.Output.Output_Access is

         use Apsepp.Output;
         use Apsepp.Output_Class.Standard;

         Instance_Access : constant Output_Access
           := new Output_Standard'(Create);

      begin

        return Instance_Access;

      end Allocate_Output_Standard;

         -----------------------------------------------------

      function Allocate_Trace_Debugging_Output
        return Apsepp.Trace_Debugging.Trace_Debugging_Access is

         use Apsepp.Trace_Debugging;
         use Apsepp.Trace_Debugging_Class.Output;

         Instance_Access : constant Trace_Debugging_Access
           := new Trace_Debugging_Output'(Create);

      begin

        return Instance_Access;

      end Allocate_Trace_Debugging_Output;

      -----------------------------------------------------

      use Ada.Assertions;
      use Apsepp.Output;

   begin

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         use Ada.Tags;

         package Output_Creator
           is new Shared_Instance.Creator (Allocate_Output_Standard);

         Output_Tag_Str : constant String := Expanded_Name (Output'Tag);

         package Trace_Debugging_Creator
           is new Apsepp.Trace_Debugging.Shared_Instance.Creator
           (Allocate_Trace_Debugging_Output);

         Trace_Debugging_Tag_Str : constant String
           := Expanded_Name (Apsepp.Trace_Debugging.Trace_Debugging'Tag);

      begin

         Assert (Output_Creator.Has_Actually_Created);
         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);
         Output.Put_Line ("Output sink instance tag:");
         Output.Put_Line (Output_Tag_Str);

         declare

            package Output_Creator
              is new Shared_Instance.Creator (Allocate_Output_Standard);

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);

         declare

            use Apsepp.Controlled_Trace_Debugging;

            package Output_Creator is new Shared_Instance.Creator
              (Allocate     => Allocate_Output_Standard,
               Just_Pretend => True);

            C_D_T : constant Controlled_Debug_Tracer := Create ("Block");

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

            C_D_T.Trace ("No exception raised in " & C_D_T.Entity_Name);
         end;

         Assert (Trace_Debugging_Creator.Has_Actually_Created);
         Assert (Apsepp.Trace_Debugging.Shared_Instance.Locked);
         Assert (Apsepp.Trace_Debugging.Shared_Instance.Instantiated);

         Apsepp.Trace_Debugging.Trace_Debugging.Trace
           ("Trace debugging facility instance tag:");
         Apsepp.Trace_Debugging.Trace_Debugging.Trace
           (Trace_Debugging_Tag_Str);

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         package Output_Creator is new Shared_Instance.Creator
           (Allocate     => Allocate_Output_Standard,
            Just_Pretend => True);

      begin

         Assert (Output_Creator.Has_Actually_Created);
         Assert (Shared_Instance.Locked);
         Assert (not Shared_Instance.Instantiated);

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
