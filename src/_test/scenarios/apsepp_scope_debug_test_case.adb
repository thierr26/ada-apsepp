-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Scope_Debug;
with Apsepp.Generic_Fixture.Creator;
with Apsepp.Generic_Shared_Instance.Access_Setter;
with Apsepp.Debug_Trace;

package body Apsepp_Scope_Debug_Test_Case is

   use Apsepp.Scope_Debug;

   ----------------------------------------------------------------------------

   function SDF return Scope_Debug_Test_Fixture_Access
     renames Instance;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      declare
         C_D_T : Controlled_Debug_Tracer := Create ("Scope_Debug_Create_Test");
         pragma Unreferenced (C_D_T);
      begin
         Assert (SDF.Scope_Entry_Count = 1);
         Assert (SDF.Scope_Exit_Count = 0);
      end;

      Assert (SDF.Scope_Entry_Count = 1);
      Assert (SDF.Scope_Exit_Count = 1);

   end Scope_Debug_Create_Test;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_A_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      declare
         C_D_T : Controlled_Debug_Tracer
           := Create_A ("Scope_Debug_Create_A_Test");
         pragma Unreferenced (C_D_T);
      begin
         Assert (SDF.Scope_Entry_Count = 1);
         Assert (SDF.Scope_Exit_Count = 0);
      end;

      Assert (SDF.Scope_Entry_Count = 1);
      Assert (SDF.Scope_Exit_Count = 1);

   end Scope_Debug_Create_A_Test;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_A_E_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      begin

         declare
            C_D_T : Controlled_Debug_Tracer
              := Create_A ("Scope_Debug_Create_A_Test");
            pragma Unreferenced (C_D_T);
         begin
            Assert (SDF.Scope_Entry_Count = 1);
            Assert (SDF.Scope_Exit_Count = 0);
            raise Program_Error;
         end;

      exception

         when others =>
            Assert (SDF.Scope_Entry_Count = 1);
            Assert (SDF.Scope_Exit_Count = 1);

      end;

      Assert (SDF.Scope_Entry_Count = 1);
      Assert (SDF.Scope_Exit_Count = 1);

   end Scope_Debug_Create_A_E_Test;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_I_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      declare
         C_D_T : Controlled_Debug_Tracer
           := Create_I ("Scope_Debug_Create_I_Test");
         pragma Unreferenced (C_D_T);
      begin
         Assert (SDF.Scope_Entry_Count = 1);
         Assert (SDF.Scope_Exit_Count = 0);
      end;

      Assert (SDF.Scope_Entry_Count = 1);
      Assert (SDF.Scope_Exit_Count = 0);

   end Scope_Debug_Create_I_Test;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_F_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      declare
         C_D_T : Controlled_Debug_Tracer
           := Create_F ("Scope_Debug_Create_F_Test");
         pragma Unreferenced (C_D_T);
      begin
         Assert (SDF.Scope_Entry_Count = 0);
         Assert (SDF.Scope_Exit_Count = 0);
      end;

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 1);

   end Scope_Debug_Create_F_Test;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_Create_N_Test is

   begin

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

      declare
         C_D_T : Controlled_Debug_Tracer
           := Create_N ("Scope_Debug_Create_N_Test");
         pragma Unreferenced (C_D_T);
      begin
         Assert (SDF.Scope_Entry_Count = 0);
         Assert (SDF.Scope_Exit_Count = 0);
      end;

      Assert (SDF.Scope_Entry_Count = 0);
      Assert (SDF.Scope_Exit_Count = 0);

   end Scope_Debug_Create_N_Test;

   ----------------------------------------------------------------------------

   overriding
   procedure Setup_Routine (Obj : Apsepp_Scope_Debug_T_C) is

      pragma Unreferenced (Obj);

   begin

      SDF.Reset;

   end Setup_Routine;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Array (Obj : Apsepp_Scope_Debug_T_C)
     return Test_Routine_Array
     is (Scope_Debug_Create_Test'Access,
         Scope_Debug_Create_A_Test'Access,
         Scope_Debug_Create_A_E_Test'Access,
         Scope_Debug_Create_I_Test'Access,
         Scope_Debug_Create_F_Test'Access,
         Scope_Debug_Create_N_Test'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     :     Apsepp_Scope_Debug_T_C;
      Outcome : out Test_Outcome;
      Kind    :     Run_Kind                   := Assert_Cond_And_Run_Test)
   is

      use Apsepp.Debug_Trace;

      -----------------------------------------------------

      package Debug_Trace_Access_Setter is new Shared_Instance.Access_Setter
        (Inst_Access => SDFDT_Instance'Access);

      -----------------------------------------------------

      package Scope_Debug_T_F_Creator
        is new Scope_Debug_T_F.Creator
          (Just_Pretend => Check_Cond_Run (Kind));

      -----------------------------------------------------

      function Cond return Boolean
        is (Debug_Trace_Access_Setter.Has_Actually_Set
              and then
            Scope_Debug_T_F_Creator.Has_Actually_Created);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Debug_Test_Case;
