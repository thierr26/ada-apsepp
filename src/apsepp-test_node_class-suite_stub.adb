-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class.Suite_Stub is

   T_S_S : aliased Test_Suite_Stub;

   ----------------------------------------------------------------------------

   procedure Run_Children (Obj     :     Test_Node_Interfa'Class;
                           Outcome : out Test_Outcome;
                           Kind    :     Run_Kind) is

      use Private_Test_Reporter;

      Current_Child     : Test_Node_Access := T_S_S'Access;
      Child_Run_Outcome : Test_Outcome;

   begin

      Outcome := Passed;

      for K in 1 .. Obj.Child_Count loop

         begin

            Current_Child := Obj.Child (K);

            begin
               Current_Child.Run (Child_Run_Outcome, Kind);
               case Child_Run_Outcome is
                  when Passed => null;
                  when Failed => Outcome := Failed;
               end case;
            exception
               when others =>
                  Test_Reporter.Report_Unexpected_Node_Run_Error (Obj'Tag);
            end;

         exception

            when others =>
               Test_Reporter.Report_Failed_Child_Test_Node_Access
                 (Obj'Tag, K = 1, Current_Child'Tag);

         end;

      end loop;

   end Run_Children;

   ----------------------------------------------------------------------------

   overriding
   procedure Run (Obj     :     Test_Suite_Stub;
                  Outcome : out Test_Outcome;
                  Kind    :     Run_Kind        := Assert_Cond_And_Run_Test)
     is

      -----------------------------------------------------

      function Cond return Boolean is
         Outc : Test_Outcome := Passed;
      begin
         case Kind is
            when Check_Cond               => Run_Children (Obj, Outc, Kind);
            when Assert_Cond_And_Run_Test => null;
         end case;
         return (case Outc is
                    when Failed => False,
                    when Passed => True);
      end Cond;

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Suite_Stub;
                   K   : Test_Node_Index) return Test_Node_Access
     is (T_S_S'Access); -- Raises because of class-wide pre-condition violation
                        -- (K <= Obj.Child_Count).

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Suite_Stub;
