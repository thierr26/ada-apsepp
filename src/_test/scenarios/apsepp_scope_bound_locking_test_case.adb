-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Lock_Holder_Class;

package body Apsepp_Scope_Bound_Locking_Test_Case is

   ----------------------------------------------------------------------------

   procedure Scope_Bound_Locking_Test is

   begin

      -- TODO: Implement real test routine for Apsepp.Scope_Bound_Locking.
      -- <2020-02-20>
      Assert (SBL_T_F.I_A.Lock_Count = 0);

   end Scope_Bound_Locking_Test;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Array
     (Obj : Apsepp_Scope_Bound_Locking_T_C) return Test_Routine_Array
     is (1 => Scope_Bound_Locking_Test'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Apsepp_Scope_Bound_Locking_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test) is

      Check_Only : constant Boolean
        := (case Kind is
               when Check_Cond               => True,
               when Assert_Cond_And_Run_Test => False);

      SBL_L_H_C_H : Apsepp.Lock_Holder_Class.Lock_Holder_Controlled_Handler
        (L_H      => L_H_A(SBL),
         Disabled => Check_Only);

      pragma Unreferenced (SBL_L_H_C_H);

      -----------------------------------------------------

      function Cond return Boolean
        is (if Check_Only then None_Locked (L_H_A) else All_Holding (L_H_A));

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Bound_Locking_Test_Case;
