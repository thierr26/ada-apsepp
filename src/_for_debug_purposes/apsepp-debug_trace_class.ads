-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

package Apsepp.Debug_Trace_Class is

   type Debug_Trace_Interfa is limited interface;

   not overriding
   function Item_W_Entity (Obj         : Debug_Trace_Interfa;
                           Item        : String;
                           Entity_Name : String) return String is abstract;

   not overriding
   function E_To_String
     (Obj   : Debug_Trace_Interfa;
      Error : Exception_Occurrence) return String is abstract;

   not overriding
   function Clock_String
     (Obj           : Debug_Trace_Interfa;
      Reset_Elapsed : Boolean             := False) return String is abstract;

   not overriding
   procedure Trace (Obj         : in out Debug_Trace_Interfa;
                    Item        :        String;
                    Entity_Name :        String              := "") is null;

   not overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Interfa;
                      Error       :        Exception_Occurrence;
                      Entity_Name :        String               := "") is null;

   not overriding
   procedure Set_Time_Zone (Obj       : in out Debug_Trace_Interfa;
                            Time_Zone :        Time_Offset) is abstract;

   not overriding
   procedure Set_Local_Time_Zone (Obj : in out Debug_Trace_Interfa)
     is abstract;

   not overriding
   procedure Trace_Time
     (Obj           : in out Debug_Trace_Interfa;
      Entity_Name   :        String              := "";
      Reset_Elapsed :        Boolean             := False) is null;

end Apsepp.Debug_Trace_Class;
