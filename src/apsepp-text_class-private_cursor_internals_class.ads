-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Containers.Indefinite_Holders;

private package Apsepp.Text_Class.Private_Cursor_Internals_Class is

   type Cursor_Internals is interface;

   not overriding
   function Constant_Text_Access
     (Obj : Cursor_Internals)
     return not null access constant Text_Interfa'Class
     is abstract;

   not overriding
   function Line_Index (Obj : Cursor_Internals) return Text_Line_Count
     is abstract;

   not overriding
   procedure Set_Line_Index (Obj   : in out Cursor_Internals;
                             Value :        Text_Line_Count) is abstract;

   not overriding
   procedure Shift_Line_Index (Obj : in out Cursor_Internals;
                               By  :        Text_Line_Count'Base) is abstract;

   package Cursor_Internals_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Cursor_Internals'Class);

end Apsepp.Text_Class.Private_Cursor_Internals_Class;
