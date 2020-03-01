-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler is

   -- TODOC: It's been considered making 'Children_Early_Test_Handler' a root
   -- tagged type (and thus not a descendant of 'Test_Node_Interfa') and using
   -- multiple inheritance to make
   -- 'Apsepp.Test_Node_Class.Abstract_Test_Suite.Test_Suite' and
   -- 'Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential'
   -- descendant of both 'Test_Node_Interfa' and 'Children_Early_Test_Handler',
   -- but then the same contracts would have had to be written twice (once for
   -- 'Test_Node_Interfa' and once for 'Children_Early_Test_Handler').
   -- <2020-02-28>
   type Children_Early_Test_Handler
     is abstract limited new Test_Node_Interfa with private;

   overriding
   function Has_Early_Test (Obj : Children_Early_Test_Handler) return Boolean
     is (True);

   overriding
   function Early_Run_Done (Obj : Children_Early_Test_Handler) return Boolean;

   -- TODOC: Run the 'Early_Run' primitive of evry child test node. No
   -- exception handling (the 'Early_Run' primitives are not supposed to
   -- raise). <2020-02-28>
   overriding
   procedure Early_Run (Obj : in out Children_Early_Test_Handler);

   overriding
   procedure Run
     (Obj     : in out Children_Early_Test_Handler;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test);

private

   type Children_Early_Test_Handler
     is abstract limited new Test_Node_Interfa with record

      Early_Run_Done_Flag : Boolean := False;

   end record;

end Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;
