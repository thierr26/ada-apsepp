-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions,
     Ada.Unchecked_Deallocation,
     Apsepp.Test_Node_Class.Private_Test_Reporter,
     Apsepp.Generic_Shared_Instance.Finalized_S_R;

package body Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes is

   use Ada.Exceptions;

   type Test_Node_Access is not null access all Test_Node_Interfa'Class;

   ----------------------------------------------------------------------------

   task type Slave_Node_Task is
      entry Start_Run (Node : Test_Node_Access);
      entry Get_Outcome_And_E (Outcome : out Test_Outcome;
                               Error   : out Exception_Occurrence_Access);
   end Slave_Node_Task;

   type Slave_Node_Task_Array
     is array (Test_Node_Index range <>) of Slave_Node_Task;

      -----------------------------------------------------

   task body Slave_Node_Task is

      Nod  : access Apsepp.Test_Node_Class.Test_Node_Interfa'Class;
      Outc : Test_Outcome;
      Ex   : Exception_Occurrence_Access;

   begin

      accept Start_Run (Node : Test_Node_Access) do
         Nod := Node;
      end Start_Run;

      begin
         if not Nod.Early_Run_Done then
            Nod.Early_Run;
         end if;
         Nod.Run (Outc);
      exception
         when Error : others => Ex := Save_Occurrence (Error);
      end;

      accept Get_Outcome_And_E (Outcome : out Test_Outcome;
                                Error   : out Exception_Occurrence_Access) do
         Outcome := Outc;
         Error   := Ex;
      end Get_Outcome_And_E;

   end Slave_Node_Task;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Slaves_Array_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Test_Node_Array,
         Name   => Test_Node_Array_Access);

   begin

      Free (Obj.A);

   end Finalize;

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential_W_Slave_Nodes;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test) is

      use Private_Test_Reporter;

      Test_Reporter_Lock_Holder : Test_Reporter_Shared_Instance.Holder;

      package Test_Reporter_S_R
        is new Test_Reporter_Shared_Instance.Finalized_S_R
        (Instance_Access      => Obj.Reporter_Access,
         Lock_Holder_Type     => Test_Reporter_Shared_Instance.Holder,
         Lock_Holder_Instance => Test_Reporter_Lock_Holder);

      pragma Unreferenced (Test_Reporter_S_R);

      -----------------------------------------------------

      procedure Run_Test is

         Node_Task : Slave_Node_Task_Array (Obj.Slaves.A'Range);
         Ex        : Exception_Occurrence_Access;

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Exception_Occurrence,
            Name   => Exception_Occurrence_Access);

      begin

         for K in Node_Task'Range loop

            -- Provide 'Obj.Slaves.A(K)' to task 'Node_Task(K)' and let the
            -- task go past its 'Start_Run' entry.
            Node_Task(K).Start_Run (Test_Node_Access (Obj.Slaves.A(K)));

         end loop;

         -- Launch the run of 'Obj.Child_Access.all' (which is the job of the
         -- inherited 'Run' procedure. 'Obj.Child_Access.all' and all the test
         -- nodes pointed to by 'Obj.Slaves.A' are run concurrently.
         Test_Runner_Sequential (Obj).Run
           (Outcome); -- Inherited procedure call.

         -- Loop over the tasks in 'Node_Task'.
         for K in Node_Task'Range loop

            declare

               Outc  : Test_Outcome;
               Error : Exception_Occurrence_Access;

            begin

               -- The rendezvous with 'Node_Task(K).Get_Outcome_And_E' takes
               -- place when the run of 'Obj.Slaves.A(K).all' is done.
               Node_Task(K).Get_Outcome_And_E (Outc, Error);

               if Ex = null -- Initial value.
                    and then
                  Error /= null then
                  -- 'Error' points to the first exception raised by a node
                  -- pointed to by an element of 'Obj.Slaves.A'.

                  -- Store this exception in 'Ex'.
                  Ex := Error;

               elsif Error /= null then

                  -- 'Error' points to an exception raised by a node pointed to
                  -- by an element of 'Obj.Slaves.A' but it's not the first
                  -- one.

                  -- Free this exception.
                  Free (Error);

               end if;

               case Outc is
                  when Failed => Outcome := Failed;
                  when Passed => null;
               end case;
            end;

         end loop;

         -- Reraise and free 'Ex'.
         if Ex /= null then
            begin
               Reraise_Occurrence (Ex.all);
            exception
               when others =>
                  Free (Ex);
                  raise;
            end;
         end if;

      exception

         when others =>
            -- We should not be here. If that happens anyway, abort all tasks
            -- and reraise.
            for Ta of Node_Task loop
               abort Ta;
            end loop;
            raise;

      end Run_Test;

      -----------------------------------------------------

   begin

      case Kind is
         when Check_Cond               =>
            Test_Runner_Sequential (Obj).Run (Outcome, Kind);
                                                   -- Inherited procedure call.
         when Assert_Cond_And_Run_Test =>
            Run_Test;
      end case;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes;
