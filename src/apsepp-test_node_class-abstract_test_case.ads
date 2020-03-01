-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body,
     Apsepp.Generic_Safe_Integer_Operations;

private with Ada.Containers.Hashed_Maps,
             Apsepp.Tags;

package Apsepp.Test_Node_Class.Abstract_Test_Case is

   type Test_Case is abstract limited new Test_Node_Interfa with private
     with Type_Invariant'Class => Test_Case.Routine_Array'Length > 0
                                    and then
                                  Test_Case.Routine_Array_Equiv_To_Routine
                                    and then
                                  Test_Case.Child_Count = 0;

   overriding
   function Child_Count (Obj : Test_Case) return Test_Node_Count
     is (0);

   -- TODOC: Always fails because a test case has no child.
   overriding
   function Child (Obj : Test_Case;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Test_Case) return Boolean
     is (True);

   overriding
   function Has_Early_Test (Obj : Test_Case) return Boolean
     is (False);

   overriding
   function Early_Run_Done (Obj : Test_Case) return Boolean
     is (True);

   overriding
   procedure Early_Run (Obj : in out Test_Case) is null;

   type Test_Routine_Count is new Natural;

   subtype Test_Routine_Index
     is Test_Routine_Count range 1 .. Test_Routine_Count'Last;

   type Test_Routine_Array
     is array (Test_Routine_Index range <>) of not null access procedure;

   procedure Null_Test_Routine is null;

   -- TODO: Check if a zero length 'Routine_Array' would be valid (this would
   -- at least imply a change of the output type of 'Routine_Count').
   -- <2020-02-18>
   not overriding
   function Routine_Array (Obj : Test_Case) return Test_Routine_Array
     is abstract;

   not overriding
   function Routine_Count (Obj : Test_Case) return Test_Routine_Index
     is (Test_Case'Class (Obj).Routine_Array'Length);

   not overriding
   function Routine
     (Obj : Test_Case;
      K   : Test_Routine_Index) return not null access procedure;

   not overriding
   procedure Setup_Routine (Obj : Test_Case) is null;

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Test_Routines);

   overriding
   procedure Run
     (Obj     : in out Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind     := Assert_Cond_And_Run_Test);

   not overriding
   function Routine_Array_Equiv_To_Routine (Obj : Test_Case) return Boolean;

   type Test_Assert_Count is new Natural;

   package Safe_Test_Assert_Count_Operations
     is new Generic_Safe_Integer_Operations
     (Integer_Type => Test_Assert_Count);

   subtype Safe_Test_Assert_Count
     is Safe_Test_Assert_Count_Operations.Safe_Integer;

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "");

private

   use Apsepp.Tags;

   type Test_Case is abstract limited new Test_Node_Interfa with null record;

   type Case_Status is record

      Routine_Index : Test_Routine_Index;

      Assert_Count : Safe_Test_Assert_Count;

      Assert_Outcome : Test_Outcome;

   end record;

   type Case_Tag_Status is record

      T : Tag;

      S : Case_Status;

   end record;

   type Case_Tag_Status_Array
     is array (Test_Node_Index range <>) of Case_Tag_Status;

   package Case_Status_Hashed_Maps
     is new Ada.Containers.Hashed_Maps (Key_Type        => Tag,
                                        Element_Type    => Case_Status,
                                        Hash            => Tag_Hash,
                                        Equivalent_Keys => "=");

   protected Case_Status_Map_Handler is

      procedure Reset_Case_Status (Node_Tag      : Tag;
                                   Routine_Index : Test_Routine_Index)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      procedure Increment_Assert_Count (Node_Tag : Tag)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      procedure Set_Failed_Outcome (Node_Tag : Tag)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      procedure Get_Assert_Count (Node_Tag      :     Tag;
                                  Routine_Index : out Test_Routine_Index;
                                  Count         : out Safe_Test_Assert_Count)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      procedure Get_Assert_Outcome (Node_Tag :     Tag;
                                    Outcome  : out Test_Outcome)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      procedure Delete (Node_Tag : Tag)
        with Pre  => Node_Tag /= No_Tag,
             Post => Invariant;

      -- TODOC: Invariant of the protected object, checked at package
      -- initialization and after each operation of the protected object via
      -- post-conditions. <2020-02-18>
      function Invariant return Boolean;

      function Count return Test_Node_Count;

      -- TODOC: For testing purposes. <2020-02-23>
      function To_Array return Case_Tag_Status_Array
        with Post => To_Array'Result'First = 1
                       and then
                     To_Array'Result'Length = Count
                       and then
                     (for all E of To_Array'Result => E.T /= No_Tag)
                       and then
                     (for all K_1 in To_Array'Result'Range =>
                       (for all K_2 in To_Array'Result'Range =>
                         K_1 = K_2 or else To_Array'Result(K_1).T
                                             /=
                                           To_Array'Result(K_2).T));

   private

      T : Tag;

      S : Case_Status;

      M : Case_Status_Hashed_Maps.Map;

   end Case_Status_Map_Handler;

end Apsepp.Test_Node_Class.Abstract_Test_Case;
