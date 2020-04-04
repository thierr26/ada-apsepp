-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Calendar;    use Ada.Calendar;
with Ada.Tags;        use Ada.Tags;
with Apsepp.Calendar; use Apsepp.Calendar;

with Apsepp.Test_Case_Count_Types;
  use Apsepp.Test_Case_Count_Types;

package Apsepp.Test_Event_Class is

   -- Force run-time class-wide pre-condition check in this package.
   pragma Assertion_Policy (Pre'Class => Check);

   type Test_Event_Data is record
      Error              : Exception_Occurrence_Access;
      Date               : Time               := Time_First;
      Previous_Child_Tag : Tag                := No_Tag;
      Routine_Index      : Test_Routine_Index := Test_Routine_Index'First;
      Last_Routine_Index : Test_Routine_Index := Test_Routine_Index'First;
      Assert_Num         : Test_Assert_Count  := Test_Assert_Count'First;
   end record;

   type Test_Event_Base is abstract tagged private;

   not overriding
   procedure Set (Obj  : in out Test_Event_Base;
                  Data :        Test_Event_Data) is null;

   not overriding
   procedure Clean_Up (Obj : in out Test_Event_Base) is null;

   not overriding
   function Is_Node_Run_Final_Event (Obj : Test_Event_Base) return Boolean
     is abstract;

   not overriding
   function Exception_Access
     (Obj : Test_Event_Base) return Exception_Occurrence_Access
     is (null);

   not overriding
   procedure Free_Exception (Obj : in out Test_Event_Base) is null;

   not overriding
   function Has_Timestamp (Obj : Test_Event_Base) return Boolean
     is (False);

   not overriding
   function Timestamp (Obj : Test_Event_Base) return Time
     is (Time_First)
     with Pre'Class => Test_Event_Base'Class (Obj).Has_Timestamp;

   not overriding
   procedure Set_Timestamp (Obj  : in out Test_Event_Base;
                            Date :        Time            := Clock) is null
     with Pre'Class => Test_Event_Base'Class (Obj).Has_Timestamp;

   not overriding
   function Has_Previous_Child_Tag (Obj : Test_Event_Base) return Boolean
     is (False);

   not overriding
   function Previous_Child_Tag (Obj : Test_Event_Base) return Tag
     is (No_Tag)
     with Pre'Class => Test_Event_Base'Class (Obj).Has_Previous_Child_Tag;

   not overriding
   function Has_Routine_Index (Obj : Test_Event_Base) return Boolean
     is (False);

   not overriding
   function Routine_Index
     (Obj : Test_Event_Base) return Test_Routine_Index
     is (Test_Routine_Index'First)
     with Pre'Class => Test_Event_Base'Class (Obj).Has_Routine_Index;

   not overriding
   function Has_Last_Cancelled_Routine_Index
     (Obj : Test_Event_Base) return Boolean
     is (False);

   not overriding
   function Last_Cancelled_Routine_Index
     (Obj : Test_Event_Base) return Test_Routine_Index
     is (Test_Routine_Index'First)
     with Pre'Class =>
            Test_Event_Base'Class (Obj).Has_Last_Cancelled_Routine_Index;

   not overriding
   function Has_Assert_Num (Obj : Test_Event_Base) return Boolean
     is (False);

   not overriding
   function Assert_Num (Obj : Test_Event_Base) return Test_Assert_Count
     is (Test_Assert_Count'First)
     with Pre'Class => Test_Event_Base'Class (Obj).Has_Assert_Num;

   ----------------------------------------------------------------------------

   type Test_Event is new Test_Event_Base with private;

   overriding
   function Is_Node_Run_Final_Event (Obj : Test_Event) return Boolean
     is (False)
     with Post'Class => not Is_Node_Run_Final_Event'Result;

   ----------------------------------------------------------------------------

   type Test_Event_Final is new Test_Event_Base with private;

   overriding
   function Is_Node_Run_Final_Event (Obj : Test_Event_Final) return Boolean
     is (True)
     with Post'Class => Is_Node_Run_Final_Event'Result;

   ----------------------------------------------------------------------------

private

   type Test_Event_Base is abstract tagged null record;

   type Test_Event is new Test_Event_Base with null record;

   type Test_Event_Final is new Test_Event_Base with null record;

end Apsepp.Test_Event_Class;
