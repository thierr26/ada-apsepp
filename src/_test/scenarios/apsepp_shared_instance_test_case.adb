-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Access_Setter;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp.Generic_Shared_Instance.Fallback_Switch;
with Apsepp_Shared_Instance_Test_Fixture;
with Apsepp.Generic_Fixture.Creator;

package body Apsepp_Shared_Instance_Test_Case is

   use Apsepp_Shared_Instance_Test_Fixture;

   ----------------------------------------------------------------------------

   function SIF return Shared_Instance_Test_Fixture_Access
     renames Instance;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Access_Setter_L_Null_CB_Test is

      use Ada.Tags;
      use SIFLI_Shared_Instance_L;

   begin

      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 0);

      declare
         package Outer_A_S is new SIFLI_Shared_Instance_L.Access_Setter
           (Inst_Access => SIFLI_Concrete_1_Instance'Access);
      begin
         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance
                   =
                 SIFLI_Concrete_1_Instance'Access);
         Assert (SIFLI_Shared_Instance_L.Instance'Tag = SIFLI_Concrete_1'Tag);
         Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_A_S.Has_Actually_Set);

         declare
            package Inner_A_S is new SIFLI_Shared_Instance_L.Access_Setter
              (Inst_Access => SIFLI_Concrete_2_Instance'Access);
         begin
            Assert (SIFLI_Shared_Instance_L.Locked);
            Assert (SIFLI_Shared_Instance_L.Instantiated);
            Assert (SIFLI_Shared_Instance_L.Instance
                      =
                    SIFLI_Concrete_1_Instance'Access);
            Assert (SIFLI_Shared_Instance_L.Instance'Tag
                      =
                    SIFLI_Concrete_1'Tag);
            Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
            Assert (SIF.Lock_Count = 1);
            Assert (SIF.Unlock_Count = 0);
            Assert (Outer_A_S.Has_Actually_Set);
            Assert (not Inner_A_S.Has_Actually_Set);
         end;

         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance
                   =
                 SIFLI_Concrete_1_Instance'Access);
         Assert (SIFLI_Shared_Instance_L.Instance'Tag = SIFLI_Concrete_1'Tag);
         Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_A_S.Has_Actually_Set);
      end;

      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIF.Lock_Count = 1);
      Assert (SIF.Unlock_Count = 0);

   end Shared_Instance_Access_Setter_L_Null_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Access_Setter_U_Null_CB_Test is

      use Ada.Tags;
      use SIFLI_Shared_Instance_U;

   begin

      Assert (not SIFLI_Shared_Instance_U.Locked);
      Assert (not SIFLI_Shared_Instance_U.Instantiated);
      Assert (SIFLI_Shared_Instance_U.Instance = null);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 0);

      declare
         package Outer_A_S is new SIFLI_Shared_Instance_U.Access_Setter
           (Inst_Access => SIFLI_Concrete_2_Instance'Access);
      begin
         Assert (SIFLI_Shared_Instance_U.Locked);
         Assert (SIFLI_Shared_Instance_U.Instantiated);
         Assert (SIFLI_Shared_Instance_U.Instance
                   =
                 SIFLI_Concrete_2_Instance'Access);
         Assert (SIFLI_Shared_Instance_U.Instance'Tag = SIFLI_Concrete_2'Tag);
         Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
         Assert (SIF.Lock_Count = 0);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_A_S.Has_Actually_Set);

         declare
            package Inner_A_S is new SIFLI_Shared_Instance_U.Access_Setter
              (Inst_Access => SIFLI_Concrete_2_Instance'Access);
         begin
            Assert (SIFLI_Shared_Instance_U.Locked);
            Assert (SIFLI_Shared_Instance_U.Instantiated);
            Assert (SIFLI_Shared_Instance_U.Instance
                      =
                    SIFLI_Concrete_2_Instance'Access);
            Assert (SIFLI_Shared_Instance_U.Instance'Tag
                      =
                    SIFLI_Concrete_2'Tag);
            Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
            Assert (SIF.Lock_Count = 0);
            Assert (SIF.Unlock_Count = 0);
            Assert (Outer_A_S.Has_Actually_Set);
            Assert (not Inner_A_S.Has_Actually_Set);
         end;

         Assert (SIFLI_Shared_Instance_U.Locked);
         Assert (SIFLI_Shared_Instance_U.Instantiated);
         Assert (SIFLI_Shared_Instance_U.Instance
                   =
                 SIFLI_Concrete_2_Instance'Access);
         Assert (SIFLI_Shared_Instance_U.Instance'Tag = SIFLI_Concrete_2'Tag);
         Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
         Assert (SIF.Lock_Count = 0);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_A_S.Has_Actually_Set);
      end;

      Assert (SIFLI_Shared_Instance_U.Instance = null);
      Assert (not SIFLI_Shared_Instance_U.Locked);
      Assert (not SIFLI_Shared_Instance_U.Instantiated);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 1);

   end Shared_Instance_Access_Setter_U_Null_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Access_Setter_CB_Test is

   begin

      Assert (SIF.CB_Calls_Count = 0);

      declare
         package A_S is new SIFLI_Shared_Instance_L.Access_Setter
           (Inst_Access => SIFLI_Concrete_1_Instance'Access,
            CB          => CB);
         pragma Unreferenced (A_S);
      begin
         Assert (SIF.CB_Calls_Count = 1);
      end;

      Assert (SIF.CB_Calls_Count = 1);

   end Shared_Instance_Access_Setter_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Creator_L_Null_CB_Test is

      use Ada.Tags;
      use SIFLI_Shared_Instance_L;

   begin

      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 0);

      declare
         package Outer_C is new SIFLI_Shared_Instance_L.Creator
           (Allocate => Allocate_SIFLI_Concrete_1_L);
      begin
         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance'Tag = SIFLI_Concrete_1'Tag);
         Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);

         declare
            package Inner_C is new SIFLI_Shared_Instance_L.Creator
              (Allocate => Allocate_SIFLI_Concrete_2_L);
         begin
            Assert (SIFLI_Shared_Instance_L.Locked);
            Assert (SIFLI_Shared_Instance_L.Instantiated);
            Assert (SIFLI_Shared_Instance_L.Instance'Tag
                      =
                    SIFLI_Concrete_1'Tag);
            Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
            Assert (SIF.Lock_Count = 1);
            Assert (SIF.Unlock_Count = 0);
            Assert (Outer_C.Has_Actually_Created);
            Assert (not Inner_C.Has_Actually_Created);
         end;

         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance'Tag = SIFLI_Concrete_1'Tag);
         Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);
      end;

      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIF.Lock_Count = 1);
      Assert (SIF.Unlock_Count = 0);

   end Shared_Instance_Creator_L_Null_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Creator_L_Null_CB_J_P_Test is

      use SIFLI_Shared_Instance_L;

   begin

      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 0);

      declare
         package Outer_C is new SIFLI_Shared_Instance_L.Creator
           (Allocate     => Allocate_SIFLI_Concrete_1_L,
            Just_Pretend => True);
      begin
         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (not SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance = null);
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);

         declare
            package Inner_C is new SIFLI_Shared_Instance_L.Creator
              (Allocate => Allocate_SIFLI_Concrete_2_L);
         begin
            Assert (SIFLI_Shared_Instance_L.Locked);
            Assert (not SIFLI_Shared_Instance_L.Instantiated);
            Assert (SIFLI_Shared_Instance_L.Instance = null);
            Assert (SIF.Lock_Count = 1);
            Assert (SIF.Unlock_Count = 0);
            Assert (Outer_C.Has_Actually_Created);
            Assert (not Inner_C.Has_Actually_Created);
         end;

         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (not SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance = null);
         Assert (SIF.Lock_Count = 1);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);
      end;

      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIF.Lock_Count = 1);
      Assert (SIF.Unlock_Count = 0);

   end Shared_Instance_Creator_L_Null_CB_J_P_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Creator_U_Null_CB_Test is

      use Ada.Tags;
      use SIFLI_Shared_Instance_U;

   begin

      Assert (not SIFLI_Shared_Instance_U.Locked);
      Assert (not SIFLI_Shared_Instance_U.Instantiated);
      Assert (SIFLI_Shared_Instance_U.Instance = null);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 0);

      declare
         package Outer_C is new SIFLI_Shared_Instance_U.Creator
           (Allocate => Allocate_SIFLI_Concrete_2_U);
      begin
         Assert (SIFLI_Shared_Instance_U.Locked);
         Assert (SIFLI_Shared_Instance_U.Instantiated);
         Assert (SIFLI_Shared_Instance_U.Instance'Tag = SIFLI_Concrete_2'Tag);
         Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
         Assert (SIF.Lock_Count = 0);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);

         declare
            package Inner_C is new SIFLI_Shared_Instance_U.Creator
              (Allocate => Allocate_SIFLI_Concrete_2_U);
         begin
            Assert (SIFLI_Shared_Instance_U.Locked);
            Assert (SIFLI_Shared_Instance_U.Instantiated);
            Assert (SIFLI_Shared_Instance_U.Instance'Tag
                      =
                    SIFLI_Concrete_2'Tag);
            Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
            Assert (SIF.Lock_Count = 0);
            Assert (SIF.Unlock_Count = 0);
            Assert (Outer_C.Has_Actually_Created);
            Assert (not Inner_C.Has_Actually_Created);
         end;

         Assert (SIFLI_Shared_Instance_U.Locked);
         Assert (SIFLI_Shared_Instance_U.Instantiated);
         Assert (SIFLI_Shared_Instance_U.Instance'Tag = SIFLI_Concrete_2'Tag);
         Assert (SIFLI_Shared_Instance_U.Instance.Primitive = '2');
         Assert (SIF.Lock_Count = 0);
         Assert (SIF.Unlock_Count = 0);
         Assert (Outer_C.Has_Actually_Created);
      end;

      Assert (SIFLI_Shared_Instance_U.Instance = null);
      Assert (not SIFLI_Shared_Instance_U.Locked);
      Assert (not SIFLI_Shared_Instance_U.Instantiated);
      Assert (SIF.Lock_Count = 0);
      Assert (SIF.Unlock_Count = 1);

   end Shared_Instance_Creator_U_Null_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Creator_CB_Test is

   begin

      Assert (SIF.CB_Calls_Count = 0);

      declare
         package C is new SIFLI_Shared_Instance_L.Creator
           (Allocate => Allocate_SIFLI_Concrete_1_L,
            CB       => CB);
         pragma Unreferenced (C);
      begin
         Assert (SIF.CB_Calls_Count = 1);
      end;

      Assert (SIF.CB_Calls_Count = 1);

   end Shared_Instance_Creator_CB_Test;

   ----------------------------------------------------------------------------

   procedure Shared_Instance_Fallback_Switch_Test is

      use Ada.Tags;
      use SIFLI_Shared_Instance_L;

      package F_S is new SIFLI_Shared_Instance_L.Fallback_Switch
        (Fallback_Instance => SIFLI_Concrete_2_Instance'Access);

   begin

      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (F_S.Instance_FS'Tag = SIFLI_Concrete_2'Tag);
      Assert (F_S.Instance_FS.Primitive = '2');

      declare
         package A_S is new SIFLI_Shared_Instance_L.Access_Setter
           (Inst_Access => SIFLI_Concrete_1_Instance'Access);
      begin

         Assert (SIFLI_Shared_Instance_L.Locked);
         Assert (SIFLI_Shared_Instance_L.Instantiated);
         Assert (SIFLI_Shared_Instance_L.Instance'Tag = SIFLI_Concrete_1'Tag);
         Assert (SIFLI_Shared_Instance_L.Instance.Primitive = '1');
         Assert (A_S.Has_Actually_Set);
         Assert (F_S.Instance_FS'Tag = SIFLI_Concrete_1'Tag);
         Assert (F_S.Instance_FS.Primitive = '1');

      end;

      Assert (not SIFLI_Shared_Instance_L.Locked);
      Assert (not SIFLI_Shared_Instance_L.Instantiated);
      Assert (SIFLI_Shared_Instance_L.Instance = null);
      Assert (F_S.Instance_FS'Tag = SIFLI_Concrete_2'Tag);
      Assert (F_S.Instance_FS.Primitive = '2');

   end Shared_Instance_Fallback_Switch_Test;

   ----------------------------------------------------------------------------

   overriding
   procedure Setup_Routine (Obj : Apsepp_Shared_Instance_T_C) is

      pragma Unreferenced (Obj);

   begin

      SIF.Reset;

   end Setup_Routine;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Array (Obj : Apsepp_Shared_Instance_T_C)
     return Test_Routine_Array
     is (Shared_Instance_Access_Setter_L_Null_CB_Test'Access,
         Shared_Instance_Access_Setter_U_Null_CB_Test'Access,
         Shared_Instance_Access_Setter_CB_Test'Access,
         Shared_Instance_Creator_L_Null_CB_Test'Access,
         Shared_Instance_Creator_L_Null_CB_J_P_Test'Access,
         Shared_Instance_Creator_U_Null_CB_Test'Access,
         Shared_Instance_Creator_CB_Test'Access,
         Shared_Instance_Fallback_Switch_Test'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     :     Apsepp_Shared_Instance_T_C;
      Outcome : out Test_Outcome;
      Kind    :     Run_Kind                   := Assert_Cond_And_Run_Test)
   is

      -----------------------------------------------------

      package Shared_Instance_T_F_Creator
        is new Shared_Instance_T_F.Creator
          (Just_Pretend => Check_Cond_Run (Kind));

      -----------------------------------------------------

      function Cond return Boolean
        is (Shared_Instance_T_F_Creator.Has_Actually_Created);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Shared_Instance_Test_Case;
