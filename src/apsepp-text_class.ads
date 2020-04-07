-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Characters.Latin_1;

private with Apsepp.Generic_Safe_Integer_Operations.Conversions;

package Apsepp.Text_Class is

   -- Omit run-time class-wide pre-condition check in this package.
   pragma Assertion_Policy (Pre'Class => Ignore);

   type Text_Line_Count is new Natural;

   subtype Text_Line_Index is Text_Line_Count range 1 .. Text_Line_Count'Last;

   type Character_Count is new Natural;

   subtype Character_Index is Character_Count range 1 .. Character_Count'Last;

   type Character_Array is array (Character_Index range <>) of Character;

   type EOL_Kind is (LF, CR, CR_LF, None);

   function EOL_String (Kind : EOL_Kind) return String
     is (case Kind is
            when LF    => Ada.Characters.Latin_1.LF & "",
            when CR    => Ada.Characters.Latin_1.CR & "",
            when CR_LF => Ada.Characters.Latin_1.CR
                            &
                          Ada.Characters.Latin_1.LF,
            when None  => "")
     with Post => EOL_String'Result'First = 1;

   function EOL_String_Length (Kind : EOL_Kind) return Natural
     is (EOL_String (Kind)'Length);

   function EOL_Length (Kind : EOL_Kind) return Character_Count
     is (Character_Count (EOL_String_Length (Kind)));

   type Text_Interfa is limited interface
     with Type_Invariant'Class =>
            Text_Interfa.Is_Empty xor Text_Interfa.Is_Line (1);

   not overriding
   function Line_Count (Obj : Text_Interfa) return Text_Line_Count
     is abstract;

   -- TODOC: Useful to check if a text has a line with index 'K' even if the
   -- number of lines of the text is not precisely known at the time.
   -- <2020-04-07>
   not overriding
   function Is_Line (Obj : Text_Interfa;
                     K   : Text_Line_Index) return Boolean is abstract;

   not overriding
   function Is_Empty (Obj : Text_Interfa) return Boolean is abstract;

   -- TODOC: Not aware of the notion of "end of line sequence". <2020-03-29>
   not overriding
   function Character_Length (Obj : Text_Interfa) return Character_Count
     is abstract;

   not overriding
   function Character_Length (Obj : Text_Interfa;
                              K   : Text_Line_Index) return Character_Count
     is abstract
     with Pre'Class => Obj.Is_Line (K);

   not overriding
   function Line
     (Obj : Text_Interfa;
      K   : Text_Line_Index) return not null access constant Character_Array
     is abstract
     with Pre'Class  => Obj.Is_Line (K),
          Post'Class => Line'Result'Length = Obj.Character_Length (K);

   not overriding
   function To_String
     (Obj              : Text_Interfa;
      EOL              : EOL_Kind     := LF;
      Include_Last_EOL : Boolean      := False) return String is abstract
     with Post'Class => To_String'Result'First = 1
                          and then
                        To_String'Result'Length = Obj.To_String_Length
                                                    (EOL,
                                                     Include_Last_EOL);

   function To_String_Truncates
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Boolean;

   function To_String_Length
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Natural;

private

   package Safe_Character_Count_Operations
     is new Generic_Safe_Integer_Operations (Integer_Type => Character_Count);

   package Safe_Character_Count_Natural_Conversions
     is new Safe_Character_Count_Operations.Conversions
     (Alternative_Type => Natural);

   function EOL_Weight (Line_Count       : Text_Line_Count;
                        EOL              : EOL_Kind;
                        Include_Last_EOL : Boolean)
     return Safe_Character_Count_Operations.Natural_Safe_Integer;

   function Text_Weight
     (Character_Length : Character_Count;
      EOL_Weight       : Safe_Character_Count_Operations.Natural_Safe_Integer)
     return Safe_Character_Count_Operations.Natural_Safe_Integer;

end Apsepp.Text_Class;
