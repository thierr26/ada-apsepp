-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Apsepp.Test_Node_Class.Private_Test_Reporter,
     Apsepp.Generic_Discrete_Operations;

package body Apsepp.Test_Node_Class.Abstract_Test_Suite is

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

   overriding
   procedure Run (Obj     : in out Test_Suite;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind     := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      function Cond return Boolean is

         function Cond_OK
           (Child_Node : not null access Test_Node_Interfa'Class)
           return Boolean is
            Outcome : Test_Outcome;
         begin
            Child_Node.Run (Outcome, Check_Cond);
            return (case Outcome is
                       when Failed => False,
                       when Passed => True);
         end Cond_OK;

      begin

         return (
                  for all Child_Node of Test_Suite'Class (Obj).Child_Array =>
                    Cond_OK (Child_Node)
                );

      end Cond;

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

      declare
         Outc : Test_Outcome;
      begin
         Children_Early_Test_Handler (Obj).Run (Outc, Kind); -- Inherited
                                                             -- procedure call.
      end;

   end Run;

   ----------------------------------------------------------------------------

   package Test_Node_Count_Operations
     is new Generic_Discrete_Operations (Discrete_Type => Test_Node_Index,
                                         Diff_Type     => Test_Node_Count);

   use Test_Node_Count_Operations;

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Suite;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class is

      Child_Array_First : constant Test_Node_Index
        := Test_Suite'Class (Obj).Child_Array'First;

      function Child_Array return Test_Node_Array
        renames Test_Suite'Class (Obj).Child_Array;

   begin

      return Child_Array (Val (K, Child_Array_First));

   end Child;

   ----------------------------------------------------------------------------

   not overriding
   function Child_Array_Equiv_To_Child (Obj : Test_Suite) return Boolean is

      Child_Array_Length : constant Test_Node_Count
        := Test_Suite'Class (Obj).Child_Array'Length;

      Child_Array_First : constant Test_Node_Index
        := Test_Suite'Class (Obj).Child_Array'First;

      function Child_Array return Test_Node_Array
        renames Test_Suite'Class (Obj).Child_Array;

      Child_Count : constant Test_Node_Count
        := Test_Suite'Class (Obj).Child_Count;

      function Child
        (K : Test_Node_Index) return not null access Test_Node_Interfa'Class
        renames Test_Suite'Class (Obj).Child;

   begin

      return (
               Child_Count = Child_Array_Length
                 and then
               (
                 for all K in Test_Suite'Class (Obj).Child_Array'Range =>
                   Child (Rank (K, Child_Array_First)) = Child_Array(K)
               )
             );

   end Child_Array_Equiv_To_Child;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Test_Suite;
