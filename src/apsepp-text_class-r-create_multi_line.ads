-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- Force run-time pre-condition check in this unit.
pragma Assertion_Policy (Pre => Check);

-- TODOC: The only input checking done is the pre-condition evaluation. No
-- search for end of lines sequences, escape sequences or any other special
-- sequences. <2020-04-10>
function Apsepp.Text_Class.R.Create_Multi_Line
  (A        : Character_Array;
   New_Line : New_Line_Index_Array := Null_New_Line_Index_Array)
  return RO_Text_Multi_Line
  with Pre => Is_Monotonic_Incr_Character_Index_Array (A      => New_Line,
                                                       Strict => False)
                and then
              (
                (
                  A'Length = 0
                    and then
                  (for all Index of New_Line => Index = A'First)
                )
                  or else
                (
                  A'Length /= 0
                    and then
                  (
                    (
                      for all Index of New_Line =>
                        Index in A'Range
                          or else
                        (
                          A'Last < Character_Count'Last
                            and then
                          Index = A'Last + 1
                        )
                    )
                  )
                )
              );
