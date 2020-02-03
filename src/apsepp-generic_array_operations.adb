-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Array_Operations is

   ----------------------------------------------------------------------------

   procedure Insert_Incr (A                : in out Array_Type;
                          Max_Insert_Index :        Index_Type;
                          Elem             :        Element_Type;
                          Rev              :        Boolean      := False) is

      -- TODO: Write a faster implementation (dichotomic search). <2019-04-06>

      -----------------------------------------------------

      type Insert_Index_Ini_Func is not null access function return Index_Type;

      function I_I_I return Index_Type
        is (A'First);

      function I_I_I_R return Index_Type
        is (Max_Insert_Index);

      Insert_Index_Ini : constant Insert_Index_Ini_Func := (if Rev then
                                                               I_I_I_R'Access
                                                            else
                                                               I_I_I'Access);

      -----------------------------------------------------

      Insert_Index : Index_Type := Insert_Index_Ini.all;

      -----------------------------------------------------

      type Shift_Insert_Index_Proc is not null access procedure;

      procedure S_I_I is
      begin
         Insert_Index := D.Su (Insert_Index);
      end S_I_I;

      procedure S_I_I_R is
      begin
         Insert_Index := D.Pr (Insert_Index);
      end S_I_I_R;

      Shift_Insert_Index : constant Shift_Insert_Index_Proc
        := (if Rev then
               S_I_I_R'Access
            else
               S_I_I'Access);

      -----------------------------------------------------

      type Insert_Index_Found_Func is not null access function return Boolean;

      function I_I_F return Boolean
        is (Insert_Index = Max_Insert_Index
              or else
            Elem < A(Insert_Index)
              or else
            Elem = A(Insert_Index));

      function I_I_F_R return Boolean
        is (Insert_Index = A'First
              or else
            A(D.Pr (Insert_Index)) < Elem
              or else
            A(D.Pr (Insert_Index)) = Elem);

      Insert_Index_Found : constant Insert_Index_Found_Func
        := (if Rev then
               I_I_F_R'Access
            else
               I_I_F'access);

      -----------------------------------------------------

   begin

      while not Insert_Index_Found.all loop
         Shift_Insert_Index.all;
      end loop;

      A(D.Su (Insert_Index) .. Max_Insert_Index)
        := A(Insert_Index .. D.Pr (Max_Insert_Index));

      A(Insert_Index) := Elem;

   end Insert_Incr;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Array_Operations;
