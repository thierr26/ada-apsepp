-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization;

package Apsepp.Text_Class.RO.Single_Line is

   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with private
     with Type_Invariant'Class => RO_Text_Single_Line.Line_Count = 1;

   overriding
   function Line_Count (Obj : RO_Text_Single_Line) return Text_Line_Count
     is (1);

   overriding
   function Is_Line (Obj : RO_Text_Single_Line;
                     K   : Text_Line_Index) return Boolean
     is (K = 1)
     with Post'Class => Is_Line'Result xor K /= 1;

   overriding
   function Is_Empty (Obj : RO_Text_Single_Line) return Boolean
     is (False);

   overriding
   function Character_Length
     (Obj : RO_Text_Single_Line) return Character_Count;

   overriding
   function Character_Length (Obj : RO_Text_Single_Line;
                              K   : Text_Line_Index) return Character_Count;

   overriding
   function Line
     (Obj : RO_Text_Single_Line;
      K   : Text_Line_Index) return not null access constant Character_Array;

   overriding
   function To_String
     (Obj              : RO_Text_Single_Line;
      EOL              : EOL_Kind            := LF;
      Include_Last_EOL : Boolean             := False) return String;

   overriding
   function To_Cursor
     (Obj        : RO_Text_Single_Line;
      Line_Index : Text_Line_Count := 1) return Cursor;

   overriding
   function First (Obj : RO_Text_Single_Line) return Cursor
     is (RO_Text_Single_Line'Class (Obj).To_Cursor (1));

   overriding
   function Last (Obj : RO_Text_Single_Line) return Cursor
     is (RO_Text_Single_Line'Class (Obj).To_Cursor (1));

   overriding
   procedure Adjust (Obj : in out RO_Text_Single_Line);

   overriding
   procedure Finalize (Obj : in out RO_Text_Single_Line);

private

   type Character_Array_Access is access Character_Array;

   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with record

      A : Character_Array_Access;

   end record;

end Apsepp.Text_Class.RO.Single_Line;
