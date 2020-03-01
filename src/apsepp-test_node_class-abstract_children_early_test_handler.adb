-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler is

   ----------------------------------------------------------------------------

   overriding
   function Early_Run_Done (Obj : Children_Early_Test_Handler) return Boolean
     is (Obj.Early_Run_Done_Flag);

   ----------------------------------------------------------------------------

   overriding
   procedure Early_Run (Obj : in out Children_Early_Test_Handler) is

   begin

      Obj.Early_Run_Done_Flag := True;

      for K in 1 .. Children_Early_Test_Handler'Class (Obj).Child_Count loop

         declare

            Child_Node : constant not null access Test_Node_Interfa'Class
              := Children_Early_Test_Handler'Class (Obj).Child (K);

         begin

            if not Child_Node.Early_Run_Done then

               Child_Node.Early_Run;

            end if;

         end;

      end loop;

   end Early_Run;

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Children_Early_Test_Handler;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind                  := Assert_Cond_And_Run_Test)
     is

      pragma Unreferenced (Outcome);

   begin

      -- Reset 'Obj.Early_Run_Done_Flag' after real run (not doing that would
      -- leave the object in an in a bad state and the 'Run' class-wide
      -- post-condition check would fail).
      case Kind is
         when Check_Cond               => null;
         when Assert_Cond_And_Run_Test => Obj.Early_Run_Done_Flag := False;
      end case;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;
