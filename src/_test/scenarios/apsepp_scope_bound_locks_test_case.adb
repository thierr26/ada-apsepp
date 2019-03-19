-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Apsepp.Scope_Bound_Locks,
     Apsepp_Scope_Bound_Locks_Test_Fixture,
     Apsepp.Generic_Fixture.Creator;

package body Apsepp_Scope_Bound_Locks_Test_Case is

   use Apsepp.Scope_Bound_Locks,
       Apsepp_Scope_Bound_Locks_Test_Fixture;

   ----------------------------------------------------------------------------

   function SBLF return Scope_Bound_Locks_Test_Fixture_Access
     renames Instance;

   ----------------------------------------------------------------------------

   procedure SB_Lock_CB_procedure_Test is

      procedure P_Null is new SB_Lock_CB_procedure (null);

      procedure P_Increment_N is new SB_Lock_CB_procedure (Increment_N'Access);

   begin

      P_Null;
      Assert (SBLF.N = 0);

      P_Increment_N;
      Assert (SBLF.N = 1);

   end SB_Lock_CB_procedure_Test;

   ----------------------------------------------------------------------------

   procedure Locked_Test is

   begin

      Assert (not Locked (SBLF.Lock));

      declare
         Locker : SB_L_Locker (SBLF.Lock'Access);
         pragma Unreferenced (Locker);
      begin
         Assert (Locked (SBLF.Lock));
      end;

      Assert (not Locked (SBLF.Lock));

      declare
         Locker : SB_L_Locker (SBLF.Lock'Access);
         pragma Unreferenced (Locker);
      begin
         Assert (Locked (SBLF.Lock));
      end;

      Assert (not Locked (SBLF.Lock));

   end;

   ----------------------------------------------------------------------------

   procedure Locked_With_CB_Test is

   begin

      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      declare
         Locker : SB_L_Locker (SBLF.Lock_W_CB'Access);
         pragma Unreferenced (Locker);
      begin
         Assert (Locked (SBLF.Lock_W_CB));
         Assert (SBLF.N = 1);
      end;

      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      begin

         declare
            Locker : SB_L_Locker (SBLF.Lock_W_CB'Access);
            pragma Unreferenced (Locker);
         begin
            Assert (Locked (SBLF.Lock_W_CB));
            Assert (SBLF.N = 1);
            raise Program_Error;
         end;

      exception

         when others =>
            Assert (not Locked (SBLF.Lock_W_CB));
            Assert (SBLF.N = 0);

      end;

      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      declare
         Locker : SB_L_Locker (SBLF.Lock_W_CB'Access);
         pragma Unreferenced (Locker);
      begin
         Assert (Locked (SBLF.Lock_W_CB));
         Assert (SBLF.N = 1);
      end;

      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

   end;

   ----------------------------------------------------------------------------

   procedure Has_Actually_Locked_Test is

   begin

      declare
         Outer_Locker : SB_L_Locker (SBLF.Lock'Access);
      begin
         Assert (Outer_Locker.Has_Actually_Locked);

         declare
            Inner_Locker : SB_L_Locker (SBLF.Lock'Access);
         begin
            Assert (not Inner_Locker.Has_Actually_Locked);
         end;

         Assert (Outer_Locker.Has_Actually_Locked);
      end;

   end;

   ----------------------------------------------------------------------------

   procedure Has_Actually_Locked_With_CB_Test is

   begin

      declare
         Outer_Locker : SB_L_Locker (SBLF.Lock_W_CB'Access);
      begin
         Assert (Outer_Locker.Has_Actually_Locked);
         Assert (SBLF.N = 1);

         declare
            Inner_Locker : SB_L_Locker (SBLF.Lock_W_CB'Access);
         begin
            Assert (not Inner_Locker.Has_Actually_Locked);
            Assert (SBLF.N = 1);
         end;

         Assert (Outer_Locker.Has_Actually_Locked);
         Assert (SBLF.N = 1);
      end;

      Assert (SBLF.N = 0);

   end;

   ----------------------------------------------------------------------------

   procedure Allocator_Test is

      type SB_L_Locker_Access is access SB_L_Locker;

      procedure Free
        is new Ada.Unchecked_Deallocation (SB_L_Locker, SB_L_Locker_Access);

      Locker_Access_1, Locker_Access_2 : SB_L_Locker_Access;

   begin

      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      Locker_Access_1 := new SB_L_Locker (SBLF.Lock_W_CB'Access);
      Assert (Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 1);
      Assert (Locker_Access_1.Has_Actually_Locked);

      Locker_Access_2 := new SB_L_Locker (SBLF.Lock_W_CB'Access);
      Assert (Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 1);
      Assert (not Locker_Access_2.Has_Actually_Locked);

      Assert (Locker_Access_1.Has_Actually_Locked);
      Free (Locker_Access_1);
      Assert (Locker_Access_1 = null);
      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      Assert (not Locker_Access_2.Has_Actually_Locked);
      Free (Locker_Access_2);
      Assert (Locker_Access_2 = null);
      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

      Locker_Access_1 := new SB_L_Locker (SBLF.Lock_W_CB'Access);
      Assert (Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 1);
      Assert (Locker_Access_1.Has_Actually_Locked);

      Locker_Access_2 := new SB_L_Locker (SBLF.Lock_W_CB'Access);
      Assert (Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 1);

      Assert (not Locker_Access_2.Has_Actually_Locked);
      Free (Locker_Access_2);
      Assert (Locker_Access_2 = null);
      Assert (Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 1);

      Assert (Locker_Access_1.Has_Actually_Locked);
      Free (Locker_Access_1);
      Assert (Locker_Access_1 = null);
      Assert (not Locked (SBLF.Lock_W_CB));
      Assert (SBLF.N = 0);

   end;

   ----------------------------------------------------------------------------

   overriding
   procedure Setup_Routine (Obj : Apsepp_Scope_Bound_Locks_T_C) is

      pragma Unreferenced (Obj);

   begin

      SBLF.Reset;

   end Setup_Routine;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Array (Obj : Apsepp_Scope_Bound_Locks_T_C)
     return Test_Routine_Array
     is (SB_Lock_CB_procedure_Test'Access,
         Locked_Test'Access,
         Locked_With_CB_Test'Access,
         Has_Actually_Locked_Test'Access,
         Has_Actually_Locked_With_CB_Test'Access,
         Allocator_Test'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Apsepp_Scope_Bound_Locks_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      package Scope_Bound_Locks_T_F_Creator
        is new Scope_Bound_Locks_T_F.Creator
          (Just_Pretend => Check_Cond_Run (Kind));

      -----------------------------------------------------

      function Cond return Boolean
        is (Scope_Bound_Locks_T_F_Creator.Has_Actually_Created);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Bound_Locks_Test_Case;
