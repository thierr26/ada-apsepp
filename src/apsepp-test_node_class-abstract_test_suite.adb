-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Discrete_Operations,
     Apsepp.Test_Node_Class.Private_Suite_Run_Body;

package body Apsepp.Test_Node_Class.Abstract_Test_Suite is

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

         return
           (
             case Kind is
                when Check_Cond               =>
                   -- Check run condition for every child test node.
                   (for all Child_Node of Test_Suite'Class (Obj).Child_Array =>
                     Cond_OK (Child_Node)),
                when Assert_Cond_And_Run_Test =>
                   -- Don't check run condition for child nodes. It's up to the
                   -- runner test node to do a run condition check if needed
                   -- with a call to this 'Run' primitive with parameter 'Kind'
                   -- set to 'Check_Cond'.
                   True
           );

      end Cond;

      -----------------------------------------------------

   begin

      Apsepp.Test_Node_Class.Private_Suite_Run_Body.Run_Body (Obj,
                                                              Outcome,
                                                              Kind,
                                                              Cond'Access);

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
