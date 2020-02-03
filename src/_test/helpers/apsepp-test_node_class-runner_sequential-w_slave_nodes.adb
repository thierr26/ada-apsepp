-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions,
     Ada.Unchecked_Deallocation,
     Apsepp.Generic_Shared_Instance.Access_Setter,
     Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes is

   use Ada.Exceptions;

   ----------------------------------------------------------------------------

   task type Slave_Node_Task is
      entry Start_Run (Node : Test_Node_Access);
      entry Get_Outcome_And_E (Outcome : out Test_Outcome;
                               E       : out Exception_Occurrence_Access);
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
         Nod.Run (Outc);
      exception
         when E : others => Ex := Save_Occurrence (E);
      end;

      accept Get_Outcome_And_E (Outcome : out Test_Outcome;
                                E       : out Exception_Occurrence_Access) do
         Outcome := Outc;
         E := Ex;
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
     (Obj     : in out Test_Runner_Sequential_W_Slave_Tasks;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test)
   is

      use Private_Test_Reporter;

      R_A : constant Shared_Instance.Instance_Type_Access
        := Shared_Instance.Instance_Type_Access (Obj.Reporter_Access);

      procedure CB is new SB_Lock_CB_procedure (SBLCB_Access => Obj.R_A_S_CB);

      package Test_Reporter_Access_Setter is new Shared_Instance.Access_Setter
        (Inst_Access => R_A,
         CB          => CB);

      pragma Unreferenced (Test_Reporter_Access_Setter);

      -----------------------------------------------------

      procedure Run_Test is

         Node_Task : Slave_Node_Task_Array (Obj.Slaves.A'Range);
         Ex        : Exception_Occurrence_Access;

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Exception_Occurrence,
            Name   => Exception_Occurrence_Access);

      begin

         for K in Node_Task'Range loop
            Node_Task(K).Start_Run (Obj.Slaves.A(K));
         end loop;

         Test_Runner_Sequential (Obj).Run (Outcome);
                                                   -- Inherited procedure call.

         for Ta of Node_Task loop
            declare
               Outc : Test_Outcome;
               E    : Exception_Occurrence_Access;
            begin
               Ta.Get_Outcome_And_E (Outc, E);
               if Ex /= null and then E /= null then
                  Free (E);
               elsif E /= null then
                  Ex := E; -- Ex will be freed later.
               end if;
               case Outc is
                  when Failed => Outcome := Failed;
                  when Passed => null;
               end case;
            end;
         end loop;

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
            for Ta of Node_Task loop
               abort Ta;
            end loop;
            raise;

      end Run_Test;

      -----------------------------------------------------

   begin

      case Kind is
         when Check_Cond =>
            Test_Runner_Sequential (Obj).Run (Outcome, Kind);
                                                   -- Inherited procedure call.
         when Assert_Cond_And_Run_Test =>
            Run_Test;
      end case;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes;
