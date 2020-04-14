-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed;

package body Apsepp.Text_Class.R is

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

   function Line (Position : Cursor) return Character_Array is

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

   overriding
   function Constant_Reference
     (Obj      : aliased RO_Text_Single_Line;
      Position : Cursor) return Constant_Reference_Type
     is (Obj.Constant_Reference (Line_Index (Position)));

   ----------------------------------------------------------------------------

   overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Single_Line;
      K   : Text_Line_Index) return Constant_Reference_Type
     is (Line  => Obj.A,
         Dummy => False);

   ----------------------------------------------------------------------------

   overriding
   function Constant_Text_Access
     (Obj : Cursor_Internals_Single_Line)
     return not null access constant Text_Interfa'Class
     is (Obj.Text_Access);

   ----------------------------------------------------------------------------

   overriding
   function Line_Index
     (Obj : Cursor_Internals_Single_Line) return Text_Line_Count
     is (Obj.Line_Idx);

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Line_Index (Obj   : in out Cursor_Internals_Single_Line;
                             Value :        Text_Line_Count) is

   begin

      Obj.Line_Idx := Value;

   end Set_Line_Index;

   ----------------------------------------------------------------------------

   overriding
   procedure Shift_Line_Index (Obj : in out Cursor_Internals_Single_Line;
                               By  :        Text_Line_Count'Base) is

   begin

      Obj.Line_Idx := Obj.Line_Idx + By;

   end Shift_Line_Index;

   ----------------------------------------------------------------------------

   overriding
   function Character_Length (Obj : RO_Text_Single_Line) return Character_Count
     is (if Obj.A = null then
            0
         else
            Obj.A'Length);

   ----------------------------------------------------------------------------

   procedure Check_K_Is_1 (K : Text_Line_Index) is

      Expected_K : constant Text_Line_Index := 1;

   begin

      if K /= Expected_K then
         raise Constraint_Error
           with "Invalid 'K' parameter value ("
                & Ada.Strings.Fixed.Trim (Text_Line_Index'Image (K),
                                          Ada.Strings.Left)
                & ", should have been"
                & Text_Line_Index'Image (Expected_K)
                & ").";
      end if;

   end Check_K_Is_1;

   ----------------------------------------------------------------------------

   overriding
   function Line_Character_Length
     (Obj : RO_Text_Single_Line;
      K   : Text_Line_Index) return Character_Count is

   begin

      Check_K_Is_1 (K);

      return Obj.Character_Length;

   end Line_Character_Length;

   ----------------------------------------------------------------------------

   overriding
   function Line (Obj : RO_Text_Single_Line;
                  K   : Text_Line_Index) return Character_Array is

   begin

      Check_K_Is_1 (K);

      return (if Obj.A = null then
                 ""
              else
                 Obj.A.all);

   end Line;

   ----------------------------------------------------------------------------

   overriding
   function To_String
     (Obj              : RO_Text_Single_Line;
      EOL              : EOL_Kind            := LF;
      Include_Last_EOL : Boolean             := False) return String is

      C_Len : constant Character_Count := Obj.Character_Length;

      Len : constant Natural := To_String_Length (Obj, EOL, Include_Last_EOL);

      Ret : String (1 .. Len);

   begin

      if Len /= 0 then

         if not To_String_Truncates (Obj) then
            -- There is room for at least the full line of text

            declare

               Natural_C_Len : constant Natural := Natural (C_Len);

            begin

               Ret(1 .. Natural_C_Len) := String (Obj.A.all);
                                         -- 'Len /= 0' implies 'Obj.A /= null'.

               -- Filling 'Ret' with the end of line sequence (or at least a
               -- (possibly null) slice of it).
               Ret(Natural_C_Len + 1 .. Len)
                 := EOL_String (EOL)(1 .. Len - Natural_C_Len);

            end;

         else
            -- The line of text must be truncated.

            Ret := String (Obj.A (Obj.A'First
                                    ..
                                  Obj.A'First - 1 + Ret'Length));
                                         -- 'Len /= 0' implies 'Obj.A /= null'.

         end if;

      end if;

      return Ret;

   end To_String;

   ----------------------------------------------------------------------------

   overriding
   function To_Cursor
     (Obj        : RO_Text_Single_Line;
      Line_Index : Text_Line_Count     := 1) return Cursor is

      T_A : constant not null access constant RO_Text_Single_Line'Class
        := Obj'Unchecked_Access;

      L_I : constant Text_Line_Count
        := (if RO_Text_Single_Line'Class (Obj).Is_Line (Line_Index) then
               Line_Index
            else
               0);

      I   : constant Cursor_Internals_Single_Line := (Text_Access => T_A,
                                                      Line_Idx    => L_I);

   begin

      return (Internals => Cursor_Internals_Holders.To_Holder (New_Item => I));

   end To_Cursor;

   ----------------------------------------------------------------------------

   overriding
   procedure Adjust (Obj : in out RO_Text_Single_Line) is

   begin

      -- Allocate a copy of the line.
      Obj.A := new Character_Array'(Obj.A.all);

   end Adjust;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out RO_Text_Single_Line) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Character_Array,
         Name   => Character_Array_Access);

   begin

      Free (Obj.A);

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Text_Class.R;
