-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Iterator_Interfaces;

package Apsepp.Text_Class.RO is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type Cursor is private;

   -- TODOC: Not limited. A type derived from a limited interface is limited
   -- only if reserved word "limited" is used in the type declaration.
   -- <2020-03-31>
   -- REF: ARM 7.5(3/3). <2020-03-31>
   type RO_Text_Interfa is interface and Text_Interfa
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Character_Array;

   not overriding
   function To_Cursor
     (Obj        : RO_Text_Interfa;
      Line_Index : Text_Line_Count := 1) return Cursor
     is abstract
     with Post'Class =>
            Constant_Text_Access (To_Cursor'Result) = Obj'Unchecked_Access
              and then
            Has_Line (To_Cursor'Result) = Obj.Is_Line (Line_Index);

   not overriding
   function First (Obj : RO_Text_Interfa) return Cursor is abstract
     with Post'Class =>
            Constant_Text_Access (First'Result) = Obj'Unchecked_Access
              and then
            (Has_Line (First'Result) xor Obj.Is_Empty);

   not overriding
   function Last (Obj : RO_Text_Interfa) return Cursor is abstract
     with Post'Class =>
            Constant_Text_Access (Last'Result) = Obj'Unchecked_Access
              and then
            (Has_Line (Last'Result) xor Obj.Is_Empty);

   function Constant_Text_Access
     (Position : Cursor) return not null access constant RO_Text_Interfa'Class;

   -- TODOC: True if 'Cursor' designates an existing line of its associated
   -- text object. <2020-04-03>
   function Has_Line (Position : Cursor) return Boolean;

   function Line_Index (Position : Cursor) return Text_Line_Index
     with Pre => Has_Line (Position);

   function Line
     (Position : Cursor) return not null access constant Character_Array
     with Pre => Has_Line (Position);

   function Next (Position : Cursor) return Cursor
     with Pre  => Has_Line (Position),
          Post => not Has_Line (Next'Result)
                    or else
                  Line_Index (Next'Result) = Line_Index (Position) + 1;

   function Previous (Position : Cursor) return Cursor
     with Pre  => Has_Line (Position),
          Post => not Has_Line (Previous'Result)
                    or else
                  Line_Index (Previous'Result) = Line_Index (Position) - 1;

   package RO_Text_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor      => Cursor,
                              Has_Element => Has_Line);

   function Constant_Reference
     (Obj      : aliased RO_Text_Interfa'Class;
      Position : Cursor) return Character_Array
     with Pre => Constant_Text_Access (Position) = Obj'Access;

   function Iterate (Obj : RO_Text_Interfa'Class)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Obj   : RO_Text_Interfa'Class;
                     Start : Cursor)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class
     with Pre => Constant_Text_Access (Start) = Obj'Unchecked_Access;

private

   type Cursor is record

      Text : access constant RO_Text_Interfa'Class;

      -- TODOC: 0 means "Cursor does not designate an existing line in text".
      -- <2020-04-06>
      Line_Index : Text_Line_Count;

   end record;

   type Iterator
     is new RO_Text_Iterator_Interfaces.Reversible_Iterator with record

      Text : access constant RO_Text_Interfa'Class;

      -- TODOC: 0 means "Iterate over the whole set of text lines".
      -- <2020-04-01>
      Start_Line_Index : Text_Line_Count;

   end record;

   overriding
   function First (Obj : Iterator) return Cursor;

   overriding
   function Last (Obj : Iterator) return Cursor;

   overriding
   function Next (Obj      : Iterator;
                  Position : Cursor) return Cursor;

   overriding
   function Previous (Obj      : Iterator;
                      Position : Cursor) return Cursor;

end Apsepp.Text_Class.RO;
