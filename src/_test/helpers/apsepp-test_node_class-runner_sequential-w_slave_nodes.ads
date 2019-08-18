-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Finalization;

package Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes is

   type Test_Runner_Sequential_W_Slave_Tasks
     is limited new Test_Runner_Sequential with private;

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential_W_Slave_Tasks;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test);

private

   type Controlled_Slaves_Array_Access
     is new Ada.Finalization.Limited_Controlled with record
      A : Test_Node_Array_Access;
   end record;

   overriding
   procedure Finalize (Obj : in out Controlled_Slaves_Array_Access);

   type Test_Runner_Sequential_W_Slave_Tasks
     is limited new Test_Runner_Sequential with record

      Slaves : Controlled_Slaves_Array_Access;

   end record;

end Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes;
