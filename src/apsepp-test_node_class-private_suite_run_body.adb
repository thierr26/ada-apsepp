-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class.Private_Suite_Run_Body is

   ----------------------------------------------------------------------------

   procedure Run_Children (Obj     :     Test_Node_Interfa'Class;
                           Outcome : out Test_Outcome) is

      use Private_Test_Reporter;

      T : constant Tag := Obj'Tag;

      Current_Child     : access Test_Node_Interfa'Class;
      Child_Run_Outcome : Test_Outcome;

   begin

      Outcome := Passed;

      -- Loop over children test nodes.
      for K in 1 .. Obj.Child_Count loop

         begin

            -- Get access to current child.
            Current_Child := Obj.Child (K);

            begin

               -- Run current child.
               Current_Child.Run (Child_Run_Outcome, Assert_Cond_And_Run_Test);

               -- Set 'Outcome' to 'Failed' if current child run outcome is
               -- 'Failed' and don't change the value (that may be 'Passed' or
               -- 'Failed') in the opposite case.
               case Child_Run_Outcome is
                  when Failed => Outcome := Failed;
                  when Passed => null;
               end case;

            exception
               when E : others =>
                  -- 'Run' primitive of current child has raised.

                  Outcome := Failed;
                  Test_Reporter.Report_Unexpected_Node_Run_Error
                    (Node_Tag => T,
                     Error    => E);

            end;

         exception

            when Access_E : others =>
               -- The attempt to access the current child raised an exception.

               Outcome := Failed;
               Test_Reporter.Report_Failed_Child_Test_Node_Access
                 (Node_Tag           => T,
                  Previous_Child_Tag => (if K = 1 then
                                            No_Tag
                                         else
                                            Current_Child'Tag),
                  Error              => Access_E);

         end;

      end loop;

   end Run_Children;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Private_Suite_Run_Body;
