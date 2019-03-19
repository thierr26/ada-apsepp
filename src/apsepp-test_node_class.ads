-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Hash;
private with Apsepp.Generic_Prot_Integer;

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

   type Test_Assert_Count is new Natural;

   subtype Test_Assert_Index
     is Test_Assert_Count range 1 .. Test_Assert_Count'Last;

   type Test_Routine is not null access procedure;

   procedure Null_Test_Routine is null;

   -- TODOC: A test node must not have two children with the same tag.
   -- <2019-03-02>
   -- TODOC: A runner must make sure that only one test node with a given tag
   -- is running at a given time. <2019-03-02>
   type Test_Node_Interfa is limited interface

     with Type_Invariant'Class
            => (for all K_1 in 1 .. Test_Node_Interfa.Child_Count
                 => (for all K_2 in 1 .. Test_Node_Interfa.Child_Count
                      => K_2 = K_1 or else Test_Node_Interfa.Child (K_1)
                                             /=
                                           Test_Node_Interfa.Child (K_2)));

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
     (Obj     : in out Test_Node_Interfa;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind          := Assert_Cond_And_Run_Test)
     is abstract;

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome;
                                Kind    :     Run_Kind)

     with Pre => Kind = Assert_Cond_And_Run_Test;

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String  := "");

private

   use Ada.Containers;

   package Prot_Test_Assert_Count
     is new Generic_Prot_Integer (Test_Assert_Count);

   use Prot_Test_Assert_Count;

   subtype O_P_I_Test_Assert_Count is Prot_Test_Assert_Count.O_P_I_Type;

   function Tag_Hash (T : Tag) return Hash_Type
     is (Ada.Strings.Hash (Expanded_Name (T)));

   type Routine_State is record
      Routine_Index  : Test_Routine_Index;
      Assert_Count   : O_P_I_Test_Assert_Count;
      Assert_Outcome : Test_Outcome;
   end record;

   package Routine_State_Hashed_Maps
     is new Ada.Containers.Hashed_Maps (Key_Type        => Tag,
                                        Element_Type    => Routine_State,
                                        Hash            => Tag_Hash,
                                        Equivalent_Keys => "=");

   ----------------------------------------------------------------------------

   protected Routine_State_Map_Handler is

      procedure Reset_Routine_State (Node_Tag      : Tag;
                                     Routine_Index : Test_Routine_Index)

        with Pre => Node_Tag /= No_Tag;

      procedure Increment_Assert_Count (Node_Tag : Tag)

        with Pre => Node_Tag /= No_Tag;

      procedure Set_Failed_Outcome (Node_Tag : Tag)

        with Pre => Node_Tag /= No_Tag;

      procedure Get_Assert_Count (Node_Tag      :     Tag;
                                  Routine_Index : out Test_Routine_Index;
                                  Count         : out O_P_I_Test_Assert_Count)

        with Pre => Node_Tag /= No_Tag;

      procedure Get_Assert_Outcome (Node_Tag :     Tag;
                                    Outcome  : out Test_Outcome)

        with Pre => Node_Tag /= No_Tag;

      procedure Delete (Node_Tag : Tag)

        with Pre => Node_Tag /= No_Tag;

   private

      T : Tag                           := No_Tag;
      S : Routine_State;
      M : Routine_State_Hashed_Maps.Map;

   end Routine_State_Map_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
