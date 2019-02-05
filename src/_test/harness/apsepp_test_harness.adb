-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp.Output;
with Apsepp.Output_Class.Standard.Create;
with Apsepp.Debug_Trace;
with Apsepp.Debug_Trace_Class.Output.Create;
with Apsepp.Scope_Debug;

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

      function Allocate_Debug_Trace_Output
        return Apsepp.Debug_Trace.Debug_Trace_Access is

         use Apsepp.Debug_Trace;
         use Apsepp.Debug_Trace_Class.Output;

         Instance_Access : constant Debug_Trace_Access
           := new Debug_Trace_Output'(Create);

      begin

        return Instance_Access;

      end Allocate_Debug_Trace_Output;

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

         package Debug_Trace_Creator
           is new Apsepp.Debug_Trace.Shared_Instance.Creator
           (Allocate_Debug_Trace_Output);

         Debug_Trace_Tag_Str : constant String
           := Expanded_Name (Apsepp.Debug_Trace.Debug_Trace'Tag);

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

            use Apsepp.Scope_Debug;

            package Output_Creator is new Shared_Instance.Creator
              (Allocate     => Allocate_Output_Standard,
               Just_Pretend => True);

            C_D_T : constant Controlled_Debug_Tracer := Create ("Block");

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

            C_D_T.Trace ("No exception raised");

         end;

         Assert (Debug_Trace_Creator.Has_Actually_Created);
         Assert (Apsepp.Debug_Trace.Shared_Instance.Locked);
         Assert (Apsepp.Debug_Trace.Shared_Instance.Instantiated);

         Apsepp.Debug_Trace.Debug_Trace.Trace ("Debug trace instance tag:");
         Apsepp.Debug_Trace.Debug_Trace.Trace (Debug_Trace_Tag_Str);

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
