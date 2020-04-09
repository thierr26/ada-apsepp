-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization;

package Apsepp.Text_Class.RO.Single_Line is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with private
     with Type_Invariant'Class => RO_Text_Single_Line.Line_Count = 1;

   overriding
   function Line_Count (Obj : RO_Text_Single_Line) return Text_Line_Count
     is (1);

   overriding
   function Is_Line (Obj : RO_Text_Single_Line;
                     K   : Text_Line_Count) return Boolean
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
      Line_Index : Text_Line_Count     := 1) return Cursor;

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

   -- TODOC: Identical to 'Apsepp.Text_Class.RO.Constant_Reference', but the
   -- local redeclaration seems to be necessary to make 'RO_Text_Single_Line'
   -- an indexable container. From ARM: 'Constant_Indexing' aspect "hall be
   -- specified by a name that denotes one or more functions declared
   -- immediately within the same declaration list in which" the container type
   -- is declared. <2020-04-09>
   -- REF: ARM 4.1.6(2/3). <2020-04-09>
   function Constant_Reference
     (Obj      : aliased RO_Text_Single_Line'Class;
      Position : Cursor) return Constant_Reference_Type
     with Pre => Constant_Text_Access (Position) = Obj'Access;

   -- TODOC: See above. <2020-04-09>
   function Constant_Reference
     (Obj : aliased RO_Text_Single_Line'Class;
      K   : Text_Line_Index) return Constant_Reference_Type
     with Pre => Obj.Is_Line (K);

private

   type Character_Array_Access is access Character_Array;

   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with record

      A : Character_Array_Access;

   end record;

   type Cursor_Internals_Single_Line is new Cursor_Internals with record

      Text_Access : access constant RO_Text_Single_Line'Class;

      Line_Idx : Text_Line_Count;

   end record;

   overriding
   function Constant_Text_Access
     (Obj : Cursor_Internals_Single_Line)
     return not null access constant Text_Interfa'Class;

   overriding
   function Line_Index
     (Obj : Cursor_Internals_Single_Line) return Text_Line_Count;

   overriding
   procedure Set_Line_Index (Obj   : in out Cursor_Internals_Single_Line;
                             Value :        Text_Line_Count);

   overriding
   procedure Shift_Line_Index (Obj : in out Cursor_Internals_Single_Line;
                               By  :        Text_Line_Count'Base);

end Apsepp.Text_Class.RO.Single_Line;
