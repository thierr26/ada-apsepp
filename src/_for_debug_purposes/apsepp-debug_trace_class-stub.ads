-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Debug_Trace_Class.Stub is

   type Debug_Trace_Stub is limited new Debug_Trace_Interfa with private;

   overriding
   function Message_W_Entity (Obj         : Debug_Trace_Stub;
                              Message     : String;
                              Entity_Name : String) return String
     is (Entity_Name & ": " & Message);

   -- TODO: Commonize with Abstract_Early_Test_Case and Instant_Standard.
   -- <2019-06-12>
   overriding
   function E_To_String (Obj : Debug_Trace_Stub;
                         E   : Exception_Occurrence) return String
     is (Exception_Name (E) & " (" & Exception_Message (E) & ")");

   -- TODOC: On first call, reset of elapsed time is done regardless of
   -- Reset_Elapsed parameter. The reset actually affects protected object
   -- Reset_Date_Handler, so the reset applies to all Debug_Trace_Interfa'Class
   -- objects with an implementation relying on Reset_Date_Handler.
   -- <2019-06-29>
   overriding
   function Clock_String
     (Obj           : Debug_Trace_Stub;
      Reset_Elapsed : Boolean          := False) return String;

   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Stub;
                      E           :        Exception_Occurrence;
                      Entity_Name :        String               := "");

   -- TODOC: Calls Reset_Date_Handler.Set_Time_Zone, so affects all
   -- Debug_Trace_Interfa'Class objects with an implementation relying on
   -- Reset_Date_Handler. <2019-06-29>
   overriding
   procedure Set_Time_Zone (Obj       : in out Debug_Trace_Stub;
                            Time_Zone :        Time_Offset);

   -- TODOC: Ditto. <2019-06-29>
   overriding
   procedure Set_Local_Time_Zone (Obj : in out Debug_Trace_Stub);

   -- TODOC: Calls Debug_Trace_Stub'Class (Obj).Clock_String. So the reset
   -- applies to all Debug_Trace_Interfa'Class objects with an implementation
   -- relying on Reset_Date_Handler. <2019-06-29>
   overriding
   procedure Trace_Time
     (Obj           : in out Debug_Trace_Stub;
      Entity_Name   :        String           := "";
      Reset_Elapsed :        Boolean          := False);

private

   type Debug_Trace_Stub is limited new Debug_Trace_Interfa with null record;

end Apsepp.Debug_Trace_Class.Stub;
