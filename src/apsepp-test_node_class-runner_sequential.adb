-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance.Access_Setter;
with Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class.Runner_Sequential is

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Runner_Sequential;
                   K   : Test_Node_Index) return Test_Node_Access
     is (Obj.Child_Access);

   ----------------------------------------------------------------------------

   overriding
   function Routine (Obj : Test_Runner_Sequential;
                     K   : Test_Routine_Index) return Test_Routine
     is (Null_Test_Routine'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind               := Assert_Cond_And_Run_Test) is

      use Private_Test_Reporter;

      R_A : constant Shared_Instance.Instance_Type_Access
        := Shared_Instance.Instance_Type_Access (Obj.Reporter_Access);

      procedure CB is new SB_Lock_CB_procedure (SBLCB_Access => Obj.R_A_S_CB);

      package Test_Reporter_Access_Setter is new Shared_Instance.Access_Setter
        (Inst_Access => R_A,
         CB          => CB);

      pragma Unreferenced (Test_Reporter_Access_Setter);

   begin

      Outcome := Passed;

      if Kind = Assert_Cond_And_Run_Test then
         Test_Suite_Stub (Obj).Run (Outcome,
                                    Check_Cond); -- Inherited procedure call.
      end if;

      if Outcome = Passed then
         Test_Suite_Stub (Obj).Run (Outcome,
                                    Kind);       -- Inherited procedure call.
      end if;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Runner_Sequential;
