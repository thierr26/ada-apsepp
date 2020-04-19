-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- Force run-time pre-condition check in this unit.
pragma Assertion_Policy (Pre => Check);

-- TODOC: The only input checking done is the pre-condition evaluation. No
-- search for end of lines sequences, escape sequences or any other special
-- sequences. <2020-04-10>
function Apsepp.Text_Class.R.Multi_Line_From_String
  (Joined_Lines : String;
   New_Line     : New_Line_Index_Array := Null_New_Line_Index_Array)
  return RO_Text_Multi_Line
  with Pre => Is_Monotonic_Incr_Character_Index_Array (A      => New_Line,
                                                       Strict => False)
                and then
              Joined_Lines'Length <= Character_Count'Last
                and then
              (
                (
                  Joined_Lines'Length = 0
                    and then
                  (
                    Positive'Pos (Joined_Lines'First)
                      <=
                    Character_Count'Pos (Character_Count'Last)
                  )
                    and then
                  (
                    for all Index of New_Line =>
                      Index = Character_Count (Joined_Lines'First)
                  )
                )
                  or else
                (
                  Joined_Lines'Length /= 0
                    and then
                  (
                    Positive'Pos (Joined_Lines'Last)
                      <=
                    Character_Count'Pos (Character_Count'Last)
                  )
                    and then
                  (
                    for all Index of New_Line =>
                      Index in Character_Count (Joined_Lines'First)
                                 ..
                               Character_Count (Joined_Lines'Last)
                        or else
                      (
                        (
                          Positive'Pos (Joined_Lines'Last)
                            <
                          Character_Count'Pos (Character_Count'Last)
                        )
                          and then
                        Index = Character_Count (Joined_Lines'Last) + 1
                      )
                  )
                )
              );
