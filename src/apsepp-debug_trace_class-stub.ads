-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

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


   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Stub;
                      E           :        Exception_Occurrence;
                      Entity_Name :        String               := "");
private

   type Debug_Trace_Stub is limited new Debug_Trace_Interfa with null record;

end Apsepp.Debug_Trace_Class.Stub;
