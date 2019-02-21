-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

package Apsepp.Test_Node_Class is

   -- Evaluate the pre-conditions and class-wide pre-conditions in this
   -- package.
   pragma Assertion_Policy (Pre'Class => Check);

   type Test_Outcome is (Failed, Passed);

   type Run_Kind is (Check_Cond, Assert_Cond_And_Run_Test);

   function Check_Cond_Run (Kind : Run_Kind) return Boolean
     is (case Kind is
            when Check_Cond               => True,
            when Assert_Cond_And_Run_Test => False);

   type Test_Node_Count is new Natural;

   subtype Test_Node_Index is Test_Node_Count range 1 .. Test_Node_Count'Last;

   type Test_Routine_Count is new Natural;

   subtype Test_Routine_Index
     is Test_Routine_Count range 1 .. Test_Routine_Count'Last;

   type Test_Routine is not null access procedure;

   procedure Null_Test_Routine is null;

   type Test_Node_Interfa is limited interface;

   -- PORT: Type_Invariant'Class aspect causes compilation failure.
   -- <2019-02-19 GNAT Community 2018 (20180523-73)>
   -- <2019-02-21 GNAT 8.2.0>

     -- with Type_Invariant'Class
     --        => (for all K_1 in 1 .. Test_Node_Interfa.Child_Count
     --             => (for all K_2 in 1 .. Test_Node_Interfa.Child_Count
     --                  => K_2 = K_1 or else Test_Node_Interfa.Child (K_1)
     --                                         /=
     --                                       Test_Node_Interfa.Child (K_2)));

   type Test_Node_Access is not null access all Test_Node_Interfa'Class;

   not overriding
   function Child_Count (Obj : Test_Node_Interfa)
     return Test_Node_Count is abstract;

   not overriding
   function Child (Obj : Test_Node_Interfa;
                   K   : Test_Node_Index) return Test_Node_Access is abstract

     with Pre'Class => K <= Obj.Child_Count;

   not overriding
   function Routine_Count (Obj : Test_Node_Interfa) return Test_Routine_Count
     is abstract;

   not overriding
   function Routine (Obj : Test_Node_Interfa;
                     K   : Test_Routine_Index) return Test_Routine is abstract

     with Pre'Class => K <= Obj.Routine_Count;

   not overriding
   procedure Setup_Routine (Obj : Test_Node_Interfa) is null;

   not overriding
   function No_Subtasking (Obj : Test_Node_Interfa)
     return Boolean is abstract;

   not overriding
   procedure Run
     (Obj     :     Test_Node_Interfa;
      Outcome : out Test_Outcome;
      Kind    :     Run_Kind          := Assert_Cond_And_Run_Test) is abstract;

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome;
                                Kind    :     Run_Kind)

     with Pre => Kind = Assert_Cond_And_Run_Test;

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "");

end Apsepp.Test_Node_Class;
