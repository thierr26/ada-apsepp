-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Discrete_Operations;

function Apsepp.Generic_Array_Slice_Copy
  (A           : Array_Type;
   First, Last : Index_Type) return Array_Type is

   Actual_Last : constant Index_Type := (if Last >= First then
                                            Last
                                         else
                                            Index_Type'Base'Pred (First));

   package D is new Apsepp.Generic_Discrete_Operations
     (Discrete_Type => Index_Type'Base,
      Diff_Type     => Diff_Type);
   use D;

   Copy_Last_Index : constant Index_Type
     := Val (Rank (Actual_Last, First), Copy_First_Index);

   Ret : constant Array_Type (Copy_First_Index .. Copy_Last_Index)
     := A(First .. Actual_Last);

begin

   return Ret;

end Apsepp.Generic_Array_Slice_Copy;
