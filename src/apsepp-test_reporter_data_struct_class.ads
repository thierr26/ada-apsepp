-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags;                use Ada.Tags;
with Apsepp.Test_Event_Class; use Apsepp.Test_Event_Class;

package Apsepp.Test_Reporter_Data_Struct_Class is

   type Test_Event_Count is new Natural;

   subtype Test_Event_Index
     is Test_Event_Count range 1 .. Test_Event_Count'Last;

   type Test_Reporter_Data_Interfa is limited interface;

   not overriding
   function Is_Empty (Obj : Test_Reporter_Data_Interfa) return Boolean
     is abstract;

   not overriding
   procedure Reset (Obj : in out Test_Reporter_Data_Interfa) is abstract
     with Post'Class => Obj.Is_Empty;

   not overriding
   function Is_Active (Obj      : Test_Reporter_Data_Interfa;
                       Node_Tag : Tag) return Boolean is abstract;

   not overriding
   procedure Include_Node (Obj          : in out Test_Reporter_Data_Interfa;
                           Node_Lineage :        Tag_Array) is abstract
     with Pre'Class  => (for all Node_Tag of Node_Lineage =>
                           Node_Tag /= No_Tag),
          Post'Class => not Obj.Is_Empty;

   -- TODOC: A new event (copy of Event) is allocated by Add_Event. Event
   -- should be cleaned up after the Add_Event call.
   not overriding
   procedure Add_Event
     (Obj      : in out Test_Reporter_Data_Interfa;
      Node_Tag :        Tag;
      Event    :        Test_Event_Base'Class) is abstract
     with Post'Class => not Obj.Is_Empty;

end Apsepp.Test_Reporter_Data_Struct_Class;
