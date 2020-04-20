-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with System,
     Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed;

package body Apsepp.Text_Class.R is

   ----------------------------------------------------------------------------

   overriding
   procedure Adjust (Obj : in out Controlled_Line_Copy_Key) is

      use type System.Address;

   begin

      if Text_Address_From_Key (Obj.Key) /= System.Null_Address then
         Add_Line_Copy_Reference (Obj.Key);
      end if;

   end Adjust;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Line_Copy_Key) is

      use type System.Address;

   begin

      if Text_Address_From_Key (Obj.Key) /= System.Null_Address then
         Remove_Line_Copy_Reference (Obj.Key);
      end if;

   end Finalize;

   ----------------------------------------------------------------------------

   function Constant_Text_Access
     (Position : Cursor) return not null access constant RO_Text_Interfa'Class
     is (Position.Text_Access);

   ----------------------------------------------------------------------------

   function Has_Line (Position : Cursor) return Boolean
     is (Position.Text_Access.Is_Line (Position.Line_Idx));

   ----------------------------------------------------------------------------

   function Line_Index (Position : Cursor) return Text_Line_Index
     is (Position.Line_Idx);

   ----------------------------------------------------------------------------

   function Line (Position : Cursor) return Character_Array
     is (Position.Text_Access.Line (Position.Line_Idx));

   ----------------------------------------------------------------------------

   type Shift_Kind is (Shift_Next, Shift_Previous);

   function Parameterized_Cursor_Shift (Position : Cursor;
                                        Kind     : Shift_Kind) return Cursor is

      Ret : Cursor := Position;

      -- True means that a line index shift is needed, false means that a line
      -- index zeroing is needed.
      Shift : constant Boolean
        := (case Kind is
               when Shift_Next     =>
                  Position.Line_Idx < Text_Line_Count'Last
                    and then
                  Position.Text_Access.Is_Line (Position.Line_Idx + 1),
               when Shift_Previous =>
                  Position.Line_Idx > 1);

   begin

      if Shift then
         case Kind is
            when Shift_Next     => Ret.Line_Idx := Ret.Line_Idx + 1;
            when Shift_Previous => Ret.Line_Idx := Ret.Line_Idx - 1;
         end case;
      else
         Ret.Line_Idx := 0;
      end if;

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

   procedure Check_K_Is_1 (Obj : RO_Text_Single_Line;
                           K   : Text_Line_Index) is

      Expected_K : constant Text_Line_Index := 1;

      pragma Unreferenced (Obj);

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
   function Constant_Reference
     (Obj      : aliased RO_Text_Single_Line;
      Position : Cursor) return Constant_Reference_Type
     is (Obj.Constant_Reference (Line_Index (Position)));

   ----------------------------------------------------------------------------

   overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Single_Line;
      K   : Text_Line_Index) return Constant_Reference_Type is

   begin

      Obj.Check_K_Is_1 (K);

      return (Line           => Obj.A,
              Controlled_Key => (Ada.Finalization.Controlled
                                   with Key => Null_Line_Copy_Key));

   end Constant_Reference;

   ----------------------------------------------------------------------------

   overriding
   function Character_Length (Obj : RO_Text_Single_Line) return Character_Count
     is (if Obj.A = null then
            0
         else
            Obj.A'Length);

   ----------------------------------------------------------------------------

   overriding
   function Line_Character_Length
     (Obj : RO_Text_Single_Line;
      K   : Text_Line_Index) return Character_Count is

   begin

      Obj.Check_K_Is_1 (K);

      return Obj.Character_Length;

   end Line_Character_Length;

   ----------------------------------------------------------------------------

   overriding
   function Line (Obj        : RO_Text_Single_Line;
                  K          : Text_Line_Index;
                  Max_Length : Character_Count     := Character_Count'Last)
     return Character_Array is

   begin

      Obj.Check_K_Is_1 (K);

      return (if Obj.A = null then
                 ""
              elsif Obj.A'Length <= Max_Length then
                 Obj.A.all
              else
                 Obj.A(Obj.A'First .. Obj.A'First - 1 + Max_Length));

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
   procedure Get_Line_As_Access_And_Slice_Bounds
     (Obj   :     RO_Text_Single_Line;
      K     :     Text_Line_Index;
      A     : out Constant_Character_Array_Access;
      First : out Character_Index;
      Last  : out Character_Count) is

   begin

      Obj.Check_K_Is_1 (K);

      A := Constant_Character_Array_Access (Obj.A);

      if Obj.A = null then

         First := 1; -- Could be any value.
         Last  := 0; -- Could be any value.

      else

         First := Obj.A'First;
         Last  := Obj.A'Last;

      end if;

   end Get_Line_As_Access_And_Slice_Bounds;

   ----------------------------------------------------------------------------

   overriding
   function To_Cursor
     (Obj        : RO_Text_Single_Line;
      Line_Index : Text_Line_Count     := 1) return Cursor
     is ((Text_Access => Obj'Unchecked_Access,
          Line_Idx    => (if Line_Index = 1 then
                             1
                          else
                             0)));

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

   begin

      Free_Character_Array (Obj.A);

   end Finalize;

   ----------------------------------------------------------------------------

   overriding
   function Constant_Reference
     (Obj      : aliased RO_Text_Multi_Line;
      Position : Cursor) return Constant_Reference_Type
     is (Obj.Constant_Reference (Line_Index (Position)));

   ----------------------------------------------------------------------------

   overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Multi_Line;
      K   : Text_Line_Index) return Constant_Reference_Type is

      Line_Copy_Data : Line_Copy;

   begin

      Get_Line_Copy (Text => Obj,
                     K    => K,
                     Data => Line_Copy_Data);

      return ((Line           => Line_Copy_Line_Access (Line_Copy_Data),
               Controlled_Key => (Ada.Finalization.Controlled
                                    with Key => Key (Line_Copy_Data))));

   end Constant_Reference;

   ----------------------------------------------------------------------------

   not overriding
   function Empty_Text return RO_Text_Multi_Line
     is (Ada.Finalization.Controlled with others => <>);

   ----------------------------------------------------------------------------

   overriding
   function Line_Count (Obj : RO_Text_Multi_Line) return Text_Line_Count
     is (if Obj.N = null then
            0
         else
            Obj.N'Length + 1);

   ----------------------------------------------------------------------------

   overriding
   function Is_Line (Obj : RO_Text_Multi_Line;
                     K   : Text_Line_Count) return Boolean
     is (K in 1 .. Obj.Line_Count);

   ----------------------------------------------------------------------------

   overriding
   function Is_Empty (Obj : RO_Text_Multi_Line) return Boolean
     is (Obj.A = null);

   ----------------------------------------------------------------------------

   overriding
   function Character_Length (Obj : RO_Text_Multi_Line) return Character_Count
     is (if Obj.A = null then
            0
         else
            Obj.A'Length);

   ----------------------------------------------------------------------------

   overriding
   function Line_Character_Length
     (Obj : RO_Text_Multi_Line;
      K   : Text_Line_Index) return Character_Count is

      A : Constant_Character_Array_Access;

      First : Character_Index;
      Last  : Character_Count;

   begin

      Obj.Get_Line_As_Access_And_Slice_Bounds (K, A, First, Last);

      -- 'A' is not supposed to be null here.

      return Last - First + 1;

   end Line_Character_Length;

   ----------------------------------------------------------------------------

   overriding
   function Line
     (Obj        : RO_Text_Multi_Line;
      K          : Text_Line_Index;
      Max_Length : Character_Count    := Character_Count'Last)
     return Character_Array is

      A : Constant_Character_Array_Access;

      First : Character_Index;
      Last  : Character_Count;

   begin

      Obj.Get_Line_As_Access_And_Slice_Bounds (K, A, First, Last);

      -- 'A' is not supposed to be null here.

      if Last - First + 1 > Max_Length then
         Last := First - 1 + Max_Length;
      end if;

      return A(First .. Last);

   end Line;

   ----------------------------------------------------------------------------

   overriding
   function To_String
     (Obj              : RO_Text_Multi_Line;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return String is

      EOL_Str : constant String := EOL_String (EOL);

      EOL_Str_Len : constant Natural := EOL_Str'Length;

      Len : constant Natural := To_String_Length (Obj, EOL, Include_Last_EOL);

      Last_Index : constant Text_Line_Count := Obj.Line_Count;

      Ret : String (1 .. Len);

      Level : Natural := 0; -- Filling level of 'Ret'.

      Index : Text_Line_Index := 1; -- Current line index.

   begin

      while Level < Len loop

         declare

            Room_Left : constant Positive := Len - Level;

            Cur_Line_Len : constant Character_Count
              := Obj.Line_Character_Length (Index);

            Truncation : constant Boolean
              := (
                   Character_Count'Pos (Cur_Line_Len)
                     >
                   Natural'Pos (Natural'Last)
                 )
                   or else
                 Natural (Cur_Line_Len) > Room_Left;

            New_Level : constant Natural
              := (if Truncation then
                     Len
                  else
                     Level + Natural (Cur_Line_Len));

            EOL_Slice_Len : constant Natural
              := (if Index = Last_Index and then not Include_Last_EOL then
                     0
                  else
                     Natural'Min (Len - New_Level,
                                  EOL_Str_Len));

         begin

            Ret(Level + 1 .. New_Level)
              := String (if Truncation then
                            Obj.Line
                              (K          => Index,
                               Max_Length => Character_Index (Len - Level))
                         else
                            Obj.Line (K => Index));

            Level := New_Level;

            if Index < Last_Index or else Include_Last_EOL then
               Ret(Level + 1 .. Level + EOL_Slice_Len)
                 := EOL_Str(1 .. EOL_Slice_Len);
            end if;

            Level := Level + EOL_Slice_Len;

         end;

         if Index < Last_Index then
            Index := Index + 1;
         end if;

      end loop;

      return Ret;

   end To_String;

   ----------------------------------------------------------------------------

   overriding
   procedure Get_Line_As_Access_And_Slice_Bounds
     (Obj   :     RO_Text_Multi_Line;
      K     :     Text_Line_Index;
      A     : out Constant_Character_Array_Access;
      First : out Character_Index;
      Last  : out Character_Count) is

   begin

      A := Constant_Character_Array_Access (Obj.A);

      -- 'A' cannot be null here. The contracts imply that 'Obj' has at least
      -- one line, so it's not an empty text object, so 'Obj.A' is not null
      -- (see the type and class invariants).

      if Obj.N'Length = 0 then
         -- The text has exactly one line. 'A.all' is this line.

         First := A'First;
         Last  := A'Last;

      elsif K = 1 then
         -- The text has at least two lines and we want the slice bounds for
         -- the first line.

         First := A'First;
         Last  := Obj.N(Obj.N'First) - 1;

      elsif K = Obj.N'Length + 1 then
         -- The text has at least two lines and we want the slice bounds for
         -- the last line.

         First := Obj.N(Obj.N'Last);
         Last  := A'Last;

      else
         -- The text has at least three lines and we want the slice bounds for
         -- any line but the first or the last one.

         First := Obj.N(K - 1);
         Last  := Obj.N(K) - 1;

      end if;

   end Get_Line_As_Access_And_Slice_Bounds;

   ----------------------------------------------------------------------------

   overriding
   function To_Cursor
     (Obj        : RO_Text_Multi_Line;
      Line_Index : Text_Line_Count    := 1) return Cursor
     is ((Text_Access => Obj'Unchecked_Access,
          Line_Idx    => (if Obj.Is_Line (Line_Index) then
                             Line_Index
                          else
                             0)));

   ----------------------------------------------------------------------------

   overriding
   procedure Adjust (Obj : in out RO_Text_Multi_Line) is

   begin

      -- Allocate a copy of the character array.
      Obj.A := new Character_Array'(Obj.A.all);

      -- Allocate a copy of the new lines indices array.
      Obj.N := new New_Line_Index_Array'(Obj.N.all);

   end Adjust;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out RO_Text_Multi_Line) is

      procedure Free_New_Line_Index_Array is new Ada.Unchecked_Deallocation
        (Object => New_Line_Index_Array,
         Name   => New_Line_Index_Array_Access);

   begin

      Free_Character_Array (Obj.A);
      Free_New_Line_Index_Array (Obj.N);

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Text_Class.R;
