-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

with Apsepp.Text_IO; use Apsepp.Text_IO;
with Apsepp.Debug_Trace_Class.Quiet; use Apsepp.Debug_Trace_Class.Quiet;

with Apsepp.Debug_Trace;

private with Apsepp.Text_IO.Generic_Fixed_IO;

package Apsepp.Debug_Trace_Class.File is

   Dflt_Time_Fraction_Digits : constant Positive_Field;

   type String_Access is private;

   ----------------------------------------------------------------------------

   protected type Debug_Trace_File is new Debug_Trace_Interfa with

      not overriding
      procedure Set_Up
        (File_Name            : String;
         Time_Fraction_Digits : Positive_Field := Dflt_Time_Fraction_Digits;
         Flush_On_New_Line    : Boolean        := False);

      not overriding
      procedure Clean_Up;

      overriding
      function Item_W_Entity (Item        : String;
                              Entity_Name : String) return String;

      overriding
      function E_To_String (Error : Exception_Occurrence) return String;

      -- TODOC: Calls primitive 'Clock_String' of 'Quiet.Debug_Trace_Quiet'. So
      -- the reset applies to all 'Debug_Trace_Interfa'Class' objects with an
      -- implementation relying on 'Private_Protected_Clock'. <2020-02-24>
      overriding
      function Clock_String (Reset_Elapsed : Boolean := False) return String;

      overriding
      procedure Trace (Item        : String;
                       Entity_Name : String := "");

      overriding
      procedure Trace_E (Error       : Exception_Occurrence;
                         Entity_Name : String               := "");

      -- TODOC: Calls primitive 'Set_Time_Zone' of 'Quiet.Debug_Trace_Quiet' so
      -- affects all 'Debug_Trace_Interfa'Class' objects with an implementation
      -- relying on 'Private_Protected_Clock'. <2020-02-24>
      overriding
      procedure Set_Time_Zone (Time_Zone : Time_Offset);

      -- TODOC: Ditto. <2020-02-24>
      overriding
      procedure Set_Local_Time_Zone;

      -- TODOC: Calls 'Clock_String'. So the reset applies to all
      -- 'Debug_Trace_Interfa'Class' objects with an implementation relying on
      -- 'Private_Protected_Clock'. <2020-02-24>
      overriding
      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False);

   private

      Debug_Trace_Quiet_Instance : Debug_Trace_Quiet;

      File_Name_Access : String_Access;

      Output_File : Ada.Text_IO.File_Type;

      Flush_On_New_Line_Flag : Boolean := False;

   end Debug_Trace_File;

   ----------------------------------------------------------------------------

   type Debug_Trace_File_Holder
     (Instance_Access : not null access Debug_Trace_File)
     is new Apsepp.Debug_Trace.Debug_Trace_Shared_Instance.Holder
     with null record;

   overriding
   procedure On_Release (Obj : Debug_Trace_File_Holder);

private

   package Duration_Image
     is new Text_IO.Generic_Fixed_IO (Fixed_Point_Type => Duration);

   Dflt_Time_Fraction_Digits : constant Positive_Field
     := Duration_Image.Default_Aft;

   type String_Access is access String;

end Apsepp.Debug_Trace_Class.File;
