-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Text_IO.Generic_Fixed_IO;

package Apsepp.Debug_Trace_Class.Quiet is

   type Debug_Trace_Quiet is limited new Debug_Trace_Interfa with private;

   not overriding
   procedure Set_Up
     (Obj                  : in out Debug_Trace_Quiet;
      Time_Fraction_Digits :        Text_IO.Positive_Field);

   overriding
   function Item_W_Entity (Obj         : Debug_Trace_Quiet;
                           Item        : String;
                           Entity_Name : String) return String
     is (Entity_Name & ": " & Item);

   overriding
   function E_To_String (Obj   : Debug_Trace_Quiet;
                         Error : Exception_Occurrence) return String
     is (Exception_Name (Error) & " (" & Exception_Message (Error) & ")");

   -- TODOC: On first call, reset of elapsed time is done regardless of
   -- 'Reset_Elapsed' parameter. The reset actually affects protected object
   -- 'Private_Protected_Clock', so the reset applies to all
   -- 'Debug_Trace_Interfa'Class' objects with an implementation relying on
   -- 'Private_Protected_Clock'. <2020-02-24>
   overriding
   function Clock_String
     (Obj           : Debug_Trace_Quiet;
      Reset_Elapsed : Boolean           := False) return String;

   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Quiet;
                      Error       :        Exception_Occurrence;
                      Entity_Name :        String               := "");

   -- TODOC: Calls 'Clock_Handler.Set_Time_Zone' (of package
   -- 'Private_Protected_Clock'), so affects all 'Debug_Trace_Interfa'Class'
   -- objects with an implementation relying on 'Private_Protected_Clock'.
   -- <2020-02-24>
   overriding
   procedure Set_Time_Zone (Obj       : in out Debug_Trace_Quiet;
                            Time_Zone :        Time_Offset);

   -- TODOC: Ditto. <2020-02-24>
   overriding
   procedure Set_Local_Time_Zone (Obj : in out Debug_Trace_Quiet);

   -- TODOC: Calls 'Debug_Trace_Quiet'Class (Obj).Clock_String'. So the reset
   -- applies to all 'Debug_Trace_Interfa'Class' objects with an implementation
   -- relying on 'Private_Protected_Clock'.
   -- <2020-02-24>
   overriding
   procedure Trace_Time
     (Obj           : in out Debug_Trace_Quiet;
      Entity_Name   :        String           := "";
      Reset_Elapsed :        Boolean          := False);

private

   package Duration_Image
     is new Text_IO.Generic_Fixed_IO (Fixed_Point_Type => Duration);

   type Debug_Trace_Quiet is limited new Debug_Trace_Interfa with record

      Time_Fraction_Digits : Text_IO.Positive_Field
        := Duration_Image.Default_Aft;

   end record;

end Apsepp.Debug_Trace_Class.Quiet;
