-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Case_Stub; use Apsepp.Test_Node_Class.Case_Stub;

package Apsepp.Test_Node_Class.Abstract_Simu_Case is

   type Test_Routine_Destiny_Kind is (No_Failure,
                                      Access_Failure,
                                      Setup_Failure,
                                      Test_Assertion_Failure,
                                      Handled_Test_Failure,
                                      Contract_Failure,
                                      Other_Failure);

   type Test_Routine_Destiny is record
      Kind                         : Test_Routine_Destiny_Kind;
      Successful_Test_Assert_Count : Test_Assert_Count;
   end record;

   type Simu_Test_Case_Story
     is array (Test_Routine_Index range <>) of Test_Routine_Destiny;

   type Simu_Test_Case is abstract limited new Test_Case_Stub with private;

   not overriding
   function Story (Obj : Simu_Test_Case)
     return Simu_Test_Case_Story is abstract

     with Post'Class => Story'Result'First = 1;

   overriding
   function Routine_Count (Obj : Simu_Test_Case) return Test_Routine_Count
     is (Simu_Test_Case'Class (Obj).Story'Length);

   -- PORT: Post'Class aspect causes compiler error.
   -- raised STORAGE_ERROR : stack overflow or erroneous memory access
   -- <2019-06-10>
   --   with Post'Class => Routine_Count'Result
   --                        =
   --                      Simu_Test_Case'Class (Obj).Story'Length;

   overriding
   function Routine (Obj : Simu_Test_Case;
                     K   : Test_Routine_Index) return Test_Routine;

   overriding
   procedure Setup_Routine (Obj : Simu_Test_Case);

private

   type Simu_Test_Case is abstract limited new Test_Case_Stub with null record;

   ----------------------------------------------------------------------------

   protected Data_Locker is

      function Locked return Boolean;

      -- TODOC: Post => Locked <2019-06-09>
      entry Set (T : Tag; D : Test_Routine_Destiny);

      -- TODOC: Pre => Locked <2019-06-09>
      function T return Tag;

      -- TODOC: Pre => Locked <2019-06-09>
      function D return Test_Routine_Destiny;

      -- TODOC: Pre => Locked, Post => not Locked <2019-06-09>
      procedure Reset;

   private

      Locked_Flag : Boolean              := False;
      T_Val       : Tag;
      D_Val       : Test_Routine_Destiny;

   end Data_Locker;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Simu_Case;
