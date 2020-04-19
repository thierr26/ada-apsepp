-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags;                     use Ada.Tags;
with Apsepp.Test_Case_Count_Types; use Apsepp.Test_Case_Count_Types;

-- TODO: Use 'Apsepp.Generic_Safe_Counter' instead of
-- 'Apsepp.Generic_Safe_Integer_Operations'. <2020-04-03>
with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body,
     Apsepp.Generic_Safe_Integer_Operations;

private with Ada.Containers.Hashed_Maps,
             Apsepp.Tags,
             Apsepp.Generic_Discrete_Operations.Is_Lim_Array_Wo_Dup;

package Apsepp.Test_Node_Class.Abstract_Test_Case is

   type Test_Case is abstract limited new Test_Node_Interfa with private
     with Type_Invariant'Class => Test_Case.Routine_Array_Equiv_To_Routine
                                    and then
                                  Test_Case.Child_Count = 0
                                    and then
                                  not Test_Case.Has_Early_Test;

   overriding
   function Child_Count (Obj : Test_Case) return Test_Node_Count
     is (0);

   -- TODOC: Always fails because a test case has no child. <2020-03-08>
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

   type Test_Routine_Array
     is array (Test_Routine_Index range <>) of not null access procedure;

   procedure Null_Test_Routine is null;

   not overriding
   function Routine_Array (Obj : Test_Case) return Test_Routine_Array
     is abstract;

   not overriding
   function Routine_Count (Obj : Test_Case) return Test_Routine_Count
     is (Test_Case'Class (Obj).Routine_Array'Length);

   not overriding
   function Routine
     (Obj : Test_Case;
      K   : Test_Routine_Index) return not null access procedure;

   not overriding
   procedure Set_Up_Routine (Obj : Test_Case) is null;

   -- TODOC: 'Obj' must be a descendant of 'Test_Case' or of
   -- 'Abstract_Simu_Test_Case.Simu_Test_Case'. <2020-03-08>
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

   package Safe_Test_Assert_Count_Operations
     is new Generic_Safe_Integer_Operations
     (Integer_Type => Test_Assert_Count);

   subtype Safe_Test_Assert_Count
     is Safe_Test_Assert_Count_Operations.Safe_Integer;

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "");

private

   use Tags;

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

   function Tag_Value (X : Case_Tag_Status) return Tag
     is (X.T);

   function No_Duplicates
     is new Test_Node_Index_Operations.Is_Lim_Array_Wo_Dup
     (Element_Type          => Case_Tag_Status,
      Array_Type            => Case_Tag_Status_Array,
      Element_Func_Ret_Type => Tag,
      Element_Func          => Tag_Value);

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
      -- TODOC: The first element (if any) of the returned array contains 'T'
      -- and 'S'. <2020-03-18>
      function To_Array return Case_Tag_Status_Array
        with Post => To_Array'Result'First = 1
                       and then
                     To_Array'Result'Length = Count
                       and then
                     No_Duplicates (To_Array'Result);

   private

      T : Tag;

      S : Case_Status;

      M : Case_Status_Hashed_Maps.Map;

   end Case_Status_Map_Handler;

end Apsepp.Test_Node_Class.Abstract_Test_Case;
