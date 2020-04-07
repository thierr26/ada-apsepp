-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Text_Class.RO is

   ----------------------------------------------------------------------------

   function Constant_Reference
     (Obj      : aliased RO_Text_Interfa'Class;
      Position : Cursor) return Character_Array
     is (Obj.Line (I (Position).Line_Index).all);

   ----------------------------------------------------------------------------

   function Parameterized_Iterate
     (Obj              : RO_Text_Interfa'Class;
      Start_Line_Index : Text_Line_Count)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class
     is (Iterator'(Text             => Obj'Unchecked_Access,
                   Start_Line_Index => Start_Line_Index));

   ----------------------------------------------------------------------------

   function Iterate (Obj : RO_Text_Interfa'Class)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class
     is (Obj.Parameterized_Iterate (0));

   ----------------------------------------------------------------------------

   function Iterate (Obj   : RO_Text_Interfa'Class;
                     Start : Cursor)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class
     is (Obj.Parameterized_Iterate (Line_Index (Start)));

   ----------------------------------------------------------------------------

   function Constant_Text_Access
     (Position : Cursor) return not null access constant RO_Text_Interfa'Class
     is (RO_Text_Interfa'Class (I (Position).Constant_Text_Access.all)'Access);

   ----------------------------------------------------------------------------

   function Has_Line (Position : Cursor) return Boolean is

      I_P : constant Cursor_Internals'Class := I (Position);

   begin

      return I_P.Constant_Text_Access.Is_Line (I_P.Line_Index);

   end Has_Line;

   ----------------------------------------------------------------------------

   function Line_Index (Position : Cursor) return Text_Line_Index
     is (I (Position).Line_Index);

   ----------------------------------------------------------------------------

   function Line
     (Position : Cursor) return not null access constant Character_Array is

      I_P : constant Cursor_Internals'Class := I (Position);

   begin

      return I_P.Constant_Text_Access.Line (I_P.Line_Index);

   end Line;

   ----------------------------------------------------------------------------

   type Shift_Kind is (Shift_Next, Shift_Previous);

   function Parameterized_Cursor_Shift (Position : Cursor;
                                        Kind     : Shift_Kind) return Cursor is

      Ret   :          Cursor                 := Position;
      I_P   : constant Cursor_Internals'Class := I (Ret);
      L_I   : constant Text_Line_Index        := I_P.Line_Index;
      Shift :          Boolean                := False; -- True means that a
                                                        -- line index shift is
                                                        -- needed, false means
                                                        -- that a line index
                                                        -- zeroing is needed.

      -----------------------------------------------------

      procedure Process (Element : in out Cursor_Internals'Class) is
      begin
         if Shift then
            Shift_Line_Index (Element, (case Kind is
                                           when Shift_Next     => +1,
                                           when Shift_Previous => -1));
         else
            Set_Line_Index (Element, 0);
         end if;
      end Process;

      -----------------------------------------------------

   begin

      Shift := (case Kind is
                   when Shift_Next     =>
                          L_I < Text_Line_Count'Last
                            and then
                          I_P.Constant_Text_Access.Is_Line (L_I + 1),
                   when Shift_Previous =>
                          L_I > 1);

      Ret.Internals.Update_Element (Process'Access);

      return Ret;

   end Parameterized_Cursor_Shift;

   ----------------------------------------------------------------------------

   function Next (Position : Cursor) return Cursor
     is (Parameterized_Cursor_Shift (Position => Position,
                                     Kind     => Shift_Next));

   ----------------------------------------------------------------------------

   function Previous (Position : Cursor) return Cursor
     is (Parameterized_Cursor_Shift (Position => Position,
                                     Kind     => Shift_Previous));

   ----------------------------------------------------------------------------

   type End_Kind is (End_First, End_Last);

   function Parameterized_End_Cursor (Obj  : Iterator;
                                      Kind : End_Kind) return Cursor
     is (if Obj.Start_Line_Index = 0 then
            (case Kind is
                when End_First =>
                   Obj.Text.First,
                when End_Last  =>
                   Obj.Text.Last)
         else
            Obj.Text.To_Cursor (Obj.Start_Line_Index));

   ----------------------------------------------------------------------------

   overriding
   function First (Obj : Iterator) return Cursor
     is (Obj.Parameterized_End_Cursor (End_First));

   ----------------------------------------------------------------------------

   overriding
   function Last (Obj : Iterator) return Cursor
     is (Obj.Parameterized_End_Cursor (End_Last));

   ----------------------------------------------------------------------------

   overriding
   function Next (Obj      : Iterator;
                  Position : Cursor) return Cursor
     is (Next (Position));

   ----------------------------------------------------------------------------

   overriding
   function Previous (Obj      : Iterator;
                      Position : Cursor) return Cursor
     is (Previous (Position));

   ----------------------------------------------------------------------------

end Apsepp.Text_Class.RO;
