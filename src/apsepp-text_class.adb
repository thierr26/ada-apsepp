-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Text_Class is

   use all type Safe_Character_Count_Operations.Safe_Integer;

   subtype Character_Count_Line_Count_Constrained
     is Character_Count range Character_Count (Text_Line_Count'First)
                                ..
                              Character_Count (Text_Line_Count'Last);

   ----------------------------------------------------------------------------

   function EOL_Weight (Line_Count       : Text_Line_Count;
                        EOL              : EOL_Kind;
                        Include_Last_EOL : Boolean)
     return Safe_Character_Count_Operations.Natural_Safe_Integer
     is (
          Create (
                   Character_Count_Line_Count_Constrained
                     (Line_Count
                        -
                      (if Include_Last_EOL or else Line_Count = 0 then
                          0
                       else
                          1))
                 )
            *
          Create (EOL_Length (EOL))
        );

   ----------------------------------------------------------------------------

   function Text_Weight
     (Character_Length : Character_Count;
      EOL_Weight       : Safe_Character_Count_Operations.Natural_Safe_Integer)
     return Safe_Character_Count_Operations.Natural_Safe_Integer
     is (Inc (EOL_Weight, Character_Length));

   ----------------------------------------------------------------------------

   function To_String_Truncates
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Boolean is

      W : constant Safe_Character_Count_Operations.Safe_Integer
        := Text_Weight (Obj.Character_Length,
                        EOL_Weight (Obj.Line_Count,
                                    EOL,
                                    Include_Last_EOL));

      use Safe_Character_Count_Natural_Conversions;

   begin

      return Sat (W) or else Is_Over (W);

   end To_String_Truncates;

   ----------------------------------------------------------------------------

   function To_String_Length
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Natural is

   begin

      if Obj.To_String_Truncates then
         return Natural'Last; -- Early return.
      end if;

      -- We get there only if 'not Obj.To_String_Truncates', which ensures that
      -- the type conversion below won't raise 'Constraint_Error'.

      return Natural (Val (Text_Weight (Obj.Character_Length,
                                        EOL_Weight (Obj.Line_Count,
                                                    EOL,
                                                    Include_Last_EOL))));

   end To_String_Length;

   ----------------------------------------------------------------------------

end Apsepp.Text_Class;
