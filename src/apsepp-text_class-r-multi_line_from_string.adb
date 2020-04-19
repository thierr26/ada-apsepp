-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Text_Class.R.Multi_Line_From_String
  (Joined_Lines : String;
   New_Line     : New_Line_Index_Array := Null_New_Line_Index_Array)
  return RO_Text_Multi_Line is

begin

   if Positive'Pos (Joined_Lines'Last)
        >
      Character_Index'Pos (Character_Index'Last) then
      -- A conversion of 'Joined_Lines' to type 'Character_Array' would raise
      -- 'Constraint_Error'. Sliding solves the issue. We know, thanks to the
      -- pre-condition, that type 'Character_Array' can accomodate
      -- 'Joined_Lines'.
      --
      -- Sliding 'Joined_Lines' implies compensating the values in 'New_Line'.

      declare

         Slided_Joined_Lines : constant String (1 .. Joined_Lines'Length)
           := Joined_Lines;

         Compensation : constant Character_Count
           := Character_Count (Joined_Lines'First) - 1;

         Compensated_New_Line : New_Line_Index_Array := New_Line;

      begin

         -- Compensate.
         for E of Compensated_New_Line loop
            E := E - Compensation;
         end loop;

         return (Ada.Finalization.Controlled
                   with A => new Character_Array'(Character_Array
                                                    (Slided_Joined_Lines)),
                        N => new New_Line_Index_Array'(Compensated_New_Line));
                                                               -- Early return.

      end;

   end if;

   -- We get there only if the conversion of 'Joined_Lines' to type
   -- 'Character_Array' is safe.

   return (Ada.Finalization.Controlled
             with A => new Character_Array'(Character_Array (Joined_Lines)),
                  N => new New_Line_Index_Array'(New_Line));

end Apsepp.Text_Class.R.Multi_Line_From_String;
