-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags;                    use Ada.Tags;
with Ada.Containers;              use Ada.Containers;
with Apsepp.Containers;           use Apsepp.Containers;
with Apsepp.Tags;                 use Apsepp.Tags;
with Apsepp.Generic_Prot_Integer;
private with Ada.Containers.Hashed_Maps;

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

   type Test_Assert_Count is new Natural;

   package Prot_Test_Assert_Count
     is new Generic_Prot_Integer (Test_Assert_Count);

   subtype O_P_I_Test_Assert_Count is Prot_Test_Assert_Count.O_P_I_Type;

   type Routine_State is record
      Routine_Index  : Test_Routine_Index;
      Assert_Count   : O_P_I_Test_Assert_Count;
      Assert_Outcome : Test_Outcome;
   end record;

   type Tag_Routine_State is record
      T : Tag;
      S : Routine_State;
   end record;

   -- TODOC: A test node must not have two children with the same tag.
   -- <2019-03-02>
   -- TODOC: A runner must make sure that only one test node with a given tag
   -- is running at a given time. <2019-03-02>
   type Test_Node_Interfa is limited interface;

   -- PORT: Type_Invariant'Class aspect causes compiler error.
   -- 8.3.0 (x86_64-linux-gnu) GCC error:
   -- in gnat_to_gnu_entity, at ada/gcc-interface/decl.c:425
   -- <2019-06-10>
   --   with Type_Invariant'Class
   --          => (for all K_1 in 1 .. Test_Node_Interfa.Child_Count
   --               => (for all K_2 in 1 .. Test_Node_Interfa.Child_Count
   --                    => K_2 = K_1 or else Test_Node_Interfa.Child (K_1)'Tag
   --                                           /=
   --                                         Test_Node_Interfa.Child (K_2)'Tag))
   --               and then
   --             (
   --               Test_Node_Interfa.Has_Early_Test
   --                 or else
   --               Test_Node_Interfa.Early_Run_Done
   --             );

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

   -- TODOC: Never called in implementations where Routine_Count returns 0.
   -- <2019-03-19>
   not overriding
   procedure Setup_Routine (Obj : Test_Node_Interfa) is null;

   not overriding
   function No_Subtasking (Obj : Test_Node_Interfa)
     return Boolean is abstract;

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

     with Post'Class => (case Kind is
                            when Check_Cond =>
                               True,
                            when Assert_Cond_And_Run_Test =>
                               Obj.Has_Early_Test xor Obj.Early_Run_Done);

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome;
                                Kind    :     Run_Kind)

     with Pre => Kind = Assert_Cond_And_Run_Test;

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "");

private

   package Routine_State_Hashed_Maps
     is new Ada.Containers.Hashed_Maps (Key_Type        => Tag,
                                        Element_Type    => Routine_State,
                                        Hash            => Tag_Hash,
                                        Equivalent_Keys => "=");

   type Tag_Routine_State_Array
     is array (Index_Type range <>) of Tag_Routine_State;

   ----------------------------------------------------------------------------

   protected Routine_State_Map_Handler is

      procedure Reset_Routine_State (Node_Tag      : Tag;
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
                                  Count         : out O_P_I_Test_Assert_Count)

        with Pre  => Node_Tag /= No_Tag,

             Post => Invariant;

      procedure Get_Assert_Outcome (Node_Tag :     Tag;
                                    Outcome  : out Test_Outcome)

        with Pre  => Node_Tag /= No_Tag,

             Post => Invariant;

      procedure Delete (Node_Tag : Tag)

        with Pre  => Node_Tag /= No_Tag,

             Post => Invariant;

      function Invariant return Boolean;

      function Count return Count_Type;

      function To_Array return Tag_Routine_State_Array

        with Post => To_Array'Result'First = 1
                       and then
                     (for all R of To_Array'Result => R.T /= No_Tag);

   private

      T : Tag;
      S : Routine_State;
      M : Routine_State_Hashed_Maps.Map;

   end Routine_State_Map_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
