-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed;

package body Apsepp.Text_Class.RO.Single_Line is

   ----------------------------------------------------------------------------

   overriding
   function Character_Length (Obj : RO_Text_Single_Line) return Character_Count
     is (Obj.A'Length);

   ----------------------------------------------------------------------------

   procedure Check_K (K : Text_Line_Index) is

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

   end Check_K;

   ----------------------------------------------------------------------------

   overriding
   function Character_Length (Obj : RO_Text_Single_Line;
                              K   : Text_Line_Index) return Character_Count is

   begin

      Check_K (K);

      return Obj.A'Length;

   end Character_Length;

   ----------------------------------------------------------------------------

   overriding
   function Line
     (Obj : RO_Text_Single_Line;
      K   : Text_Line_Index) return not null access constant Character_Array is

   begin

      Check_K (K);

      return Obj.A;

   end Line;

   ----------------------------------------------------------------------------

   overriding
   function To_String
     (Obj              : RO_Text_Single_Line;
      EOL              : EOL_Kind            := LF;
      Include_Last_EOL : Boolean             := False) return String is

      Ret : String (1 .. To_String_Length (Obj, EOL, Include_Last_EOL));

   begin

      if Ret'Length > Obj.A'Length then -- 'Universal_Integer' operands.
         -- There is room for at least a slice of the end of line sequence.

         Ret(1 .. Obj.A'Length) := String (Obj.A.all);

         declare
            EOL_Room : constant Positive := Ret'Length - Obj.A'Length;
         begin
            Ret(Obj.A'Length + 1 .. Obj.A'Length + EOL_Room)
              := EOL_String (EOL)(1 .. EOL_Room);
         end;

      else
         -- There is no room for the end of line sequence.

         Ret := String (Obj.A (1 .. Ret'Length));

      end if;

      return Ret;

   end To_String;

   ----------------------------------------------------------------------------

   overriding
   function To_Cursor
     (Obj        : RO_Text_Single_Line;
      Line_Index : Text_Line_Count := 1) return Cursor
     is (Text       => Obj'Unchecked_Access,
         Line_Index => (if Obj.Is_Line (Line_Index) then
                           Line_Index
                        else
                           0));

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

end Apsepp.Text_Class.RO.Single_Line;
