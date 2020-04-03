-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Private_Test_Reporter,
     Apsepp.Test_Node_Class.Private_Suite_Run_Body,
     Apsepp.Test_Node_Class.Abstract_Test_Suite,
     Apsepp.Generic_Shared_Instance.Finalized_S_R;

package body Apsepp.Test_Node_Class.Runner_Sequential is

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Runner_Sequential;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class
     is (Obj.Child_Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind               := Assert_Cond_And_Run_Test) is

      use Private_Test_Reporter;

      Test_Reporter_Lock_Holder : Test_Reporter_Shared_Instance.Holder;

      package Test_Reporter_S_R
        is new Test_Reporter_Shared_Instance.Finalized_S_R
        (Instance_Access      => Obj.Reporter_Access,
         Lock_Holder_Type     => Test_Reporter_Shared_Instance.Holder,
         Lock_Holder_Instance => Test_Reporter_Lock_Holder);

      pragma Unreferenced (Test_Reporter_S_R);

      -----------------------------------------------------

      function Cond return Boolean is

         Outcome : Test_Outcome := Passed;

      begin

         case Kind is
            when Check_Cond               =>
               -- Check run condition for the child test node.
               Obj.Child_Access.Run (Outcome, Kind);
            when Assert_Cond_And_Run_Test =>
               -- Don't re-check run condition for the child test node.
               null;
         end case;

         return (case Outcome is
                    when Failed => False,
                    when Passed => True);

      end Cond;

      -----------------------------------------------------

      -- Make Private_Suite_Run_Body.Run_Body visible.
      use Private_Suite_Run_Body;

   begin

      case Kind is

         when Check_Cond               =>
            -- Just check the run condition and set 'Obj.Check_Cond_Run_Done'
            -- so that we know it's been done when doing the subsequent "real"
            -- run.

            Run_Body (Obj,
                      Outcome,
                      Kind,        -- 'Kind' is 'Check_Cond'.
                      Cond'Access);

            Obj.Check_Cond_Run_Done := True;

         when Assert_Cond_And_Run_Test =>
            -- Check the run condition if not already done (i.e. if
            -- 'Obj.Check_Cond_Run_Done' is false) and then do the "real" run
            -- (and finally reset 'Obj.Check_Cond_Run_Done').

            if not Obj.Check_Cond_Run_Done then

               Test_Runner_Sequential (Obj).Run
                 (Outcome,
                  Check_Cond); -- Recursive call.

            end if;

            Run_Body (Obj,
                      Outcome,
                      Kind,        -- 'Kind' is 'Assert_Cond_And_Run_Test'.
                      Cond'Access);

            Obj.Check_Cond_Run_Done := False;

      end case;

      declare
         Outc : Test_Outcome;
      begin
         Children_Early_Test_Handler (Obj).Run (Outc, Kind); -- Inherited
                                                             -- procedure call.
      end;

   end Run;

   ----------------------------------------------------------------------------

   function Runner_Sequential_Invariant
     (Obj : Test_Runner_Sequential'Class) return Boolean
     is (Obj.Child_Count = 1
           and then
         Obj.No_Subtasking
           and then
         Obj.Has_Early_Test);

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Runner_Sequential;
