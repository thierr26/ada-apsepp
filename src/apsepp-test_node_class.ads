-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Node_Class is

   -- Force run-time class-wide pre-condition check in this package.
   pragma Assertion_Policy (Pre'Class => Check);

   type Test_Outcome is (Failed, Passed);

   type Run_Kind is (Check_Cond, Assert_Cond_And_Run_Test);

   type Test_Node_Count is new Natural;

   subtype Test_Node_Index is Test_Node_Count range 1 .. Test_Node_Count'Last;

   -- TODOC: A test node must not have two children with the same tag.
   -- <2019-03-02>
   -- TODOC: A runner must make sure that only one test node with a given tag
   -- is running at a given time. <2019-03-02>
   type Test_Node_Interfa is limited interface
     with Type_Invariant'Class =>
            Has_No_Children_W_Same_Tags (Test_Node_Interfa)
              and then
            (
              Test_Node_Interfa.Has_Early_Test
                or else
              Test_Node_Interfa.Early_Run_Done
            );

   not overriding
   function Child_Count (Obj : Test_Node_Interfa)
     return Test_Node_Count is abstract;

   not overriding
   function Child (Obj : Test_Node_Interfa;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class is abstract
     with Pre'Class => K <= Obj.Child_Count;

   not overriding
   function No_Subtasking (Obj : Test_Node_Interfa) return Boolean is abstract;

   not overriding
   function Has_Early_Test
     (Obj : Test_Node_Interfa) return Boolean is abstract;

   not overriding
   function Early_Run_Done (Obj : Test_Node_Interfa) return Boolean
     is abstract;

   not overriding
   procedure Early_Run (Obj : in out Test_Node_Interfa) is abstract
     with Pre'Class  => not Obj.Early_Run_Done,
          Post'Class => Obj.Early_Run_Done;

   not overriding
   procedure Run
     (Obj     : in out Test_Node_Interfa;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind          := Assert_Cond_And_Run_Test)
     is abstract
     with Pre'Class  => Obj.Early_Run_Done,
          Post'Class => (case Kind is
                            when Check_Cond =>
                               True,
                            when Assert_Cond_And_Run_Test =>
                               Obj.Has_Early_Test xor Obj.Early_Run_Done);

   function Has_No_Children_W_Same_Tags
     (Obj : Test_Node_Interfa'Class) return Boolean;

end Apsepp.Test_Node_Class;
