-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization,
     Ada.Iterator_Interfaces;

private with Apsepp.Text_Class.Private_Line_References;

package Apsepp.Text_Class.R is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   type Cursor is private;

   -- TODOC: Not limited. A type derived from a limited interface is limited
   -- only if reserved word "limited" is used in the type declaration.
   -- <2020-03-31>
   -- REF: ARM 7.5(3/3). <2020-03-31>
   -- TODOC: Indexable and iterable container interface. Indexing via cursor or
   -- via text line index. <2020-04-09>
   type RO_Text_Interfa is interface and Text_Interfa
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Character_Array;

   not overriding
   function To_Cursor
     (Obj        : RO_Text_Interfa;
      Line_Index : Text_Line_Count := 1) return Cursor
     is abstract
     with Pre'Class  => Obj.Is_Line (Line_Index),
          Post'Class =>
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

   type Constant_Reference_Type
     (Line : not null access constant Character_Array) is private
     with Implicit_Dereference => Line;

   not overriding
   function Constant_Reference
     (Obj      : aliased RO_Text_Interfa;
      Position : Cursor) return Constant_Reference_Type is abstract
     with Pre'Class  => Constant_Text_Access (Position) = Obj'Access
                          and then
                        Has_Line (Position)
                          and then
                        Obj.Is_Line (Line_Index (Position)),
          Post'Class => Constant_Reference'Result.Line.all
                          =
                        Obj.Line (Line_Index (Position));

   not overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Interfa;
      K   : Text_Line_Index) return Constant_Reference_Type is abstract
     with Pre'Class  => Obj.Is_Line (K),
          Post'Class => Constant_Reference'Result.Line.all = Obj.Line (K);

   -- TODOC: Useful for expressing contracts. <2020-04-09>
   function Constant_Text_Access
     (Position : Cursor) return not null access constant RO_Text_Interfa'Class;

   -- TODOC: True if 'Cursor' designates an existing line of its associated
   -- text object. <2020-04-03>
   function Has_Line (Position : Cursor) return Boolean;

   function Line_Index (Position : Cursor) return Text_Line_Index
     with Pre => Has_Line (Position);

   function Line (Position : Cursor) return Character_Array
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

   function Iterate (Obj : RO_Text_Interfa'Class)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Obj   : RO_Text_Interfa'Class;
                     Start : Cursor)
     return RO_Text_Iterator_Interfaces.Reversible_Iterator'Class
     with Pre => Constant_Text_Access (Start) = Obj'Unchecked_Access;

   -- TODOC: 'Is_Empty' primitive always false. <2020-04-12>
   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with private
     with Type_Invariant'Class => RO_Text_Single_Line.Line_Count = 1;

   overriding
   function Line_Count (Obj : RO_Text_Single_Line) return Text_Line_Count
     is (1);

   overriding
   function Is_Line (Obj : RO_Text_Single_Line;
                     K   : Text_Line_Count) return Boolean
     is (K = 1)
     with Post'Class => Is_Line'Result xor K /= 1;

   overriding
   function Is_Empty (Obj : RO_Text_Single_Line) return Boolean
     is (False);

   overriding
   function Character_Length
     (Obj : RO_Text_Single_Line) return Character_Count;

   overriding
   function Line_Character_Length
     (Obj : RO_Text_Single_Line;
      K   : Text_Line_Index) return Character_Count;

   overriding
   function Line (Obj        : RO_Text_Single_Line;
                  K          : Text_Line_Index;
                  Max_Length : Character_Count     := Character_Count'Last)
     return Character_Array;

   overriding
   function To_String
     (Obj              : RO_Text_Single_Line;
      EOL              : EOL_Kind            := LF;
      Include_Last_EOL : Boolean             := False) return String;

   overriding
   procedure Get_Line_As_Access_And_Slice_Bounds
     (Obj   :     RO_Text_Single_Line;
      K     :     Text_Line_Index;
      A     : out Constant_Character_Array_Access;
      First : out Character_Index;
      Last  : out Character_Count);

   overriding
   function To_Cursor
     (Obj        : RO_Text_Single_Line;
      Line_Index : Text_Line_Count     := 1) return Cursor;

   overriding
   function First (Obj : RO_Text_Single_Line) return Cursor
     is (RO_Text_Single_Line'Class (Obj).To_Cursor (1));

   overriding
   function Last (Obj : RO_Text_Single_Line) return Cursor
     is (RO_Text_Single_Line'Class (Obj).To_Cursor (1));

   overriding
   procedure Adjust (Obj : in out RO_Text_Single_Line);

   overriding
   procedure Finalize (Obj : in out RO_Text_Single_Line);

   overriding
   function Constant_Reference
     (Obj      : aliased RO_Text_Single_Line;
      Position : Cursor) return Constant_Reference_Type;

   overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Single_Line;
      K   : Text_Line_Index) return Constant_Reference_Type;

   type RO_Text_Multi_Line is new Ada.Finalization.Controlled
                                     and
                                  RO_Text_Interfa with private;

   not overriding
   function Empty_Text return RO_Text_Multi_Line;

   overriding
   function Line_Count (Obj : RO_Text_Multi_Line) return Text_Line_Count;

   overriding
   function Is_Line (Obj : RO_Text_Multi_Line;
                     K   : Text_Line_Count) return Boolean
     is (K in 1 .. Obj.Line_Count);

   overriding
   function Is_Empty (Obj : RO_Text_Multi_Line) return Boolean;

   overriding
   function Character_Length (Obj : RO_Text_Multi_Line) return Character_Count;

   overriding
   function Line_Character_Length
     (Obj : RO_Text_Multi_Line;
      K   : Text_Line_Index) return Character_Count;

   overriding
   function Line (Obj        : RO_Text_Multi_Line;
                  K          : Text_Line_Index;
                  Max_Length : Character_Count    := Character_Count'Last)
     return Character_Array;

   overriding
   function To_String
     (Obj              : RO_Text_Multi_Line;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return String;

   overriding
   procedure Get_Line_As_Access_And_Slice_Bounds
     (Obj   :     RO_Text_Multi_Line;
      K     :     Text_Line_Index;
      A     : out Constant_Character_Array_Access;
      First : out Character_Index;
      Last  : out Character_Count);

   overriding
   function To_Cursor
     (Obj        : RO_Text_Multi_Line;
      Line_Index : Text_Line_Count    := 1) return Cursor;

   overriding
   function First (Obj : RO_Text_Multi_Line) return Cursor
     is (RO_Text_Multi_Line'Class (Obj).To_Cursor (1));

   overriding
   function Last (Obj : RO_Text_Multi_Line) return Cursor
     is (RO_Text_Multi_Line'Class (Obj).To_Cursor (1));

   overriding
   procedure Adjust (Obj : in out RO_Text_Multi_Line);

   overriding
   procedure Finalize (Obj : in out RO_Text_Multi_Line);

   overriding
   function Constant_Reference
     (Obj      : aliased RO_Text_Multi_Line;
      Position : Cursor) return Constant_Reference_Type
     with Pre => Constant_Text_Access (Position) = Obj'Access;

   overriding
   function Constant_Reference
     (Obj : aliased RO_Text_Multi_Line;
      K   : Text_Line_Index) return Constant_Reference_Type
     with Pre => Obj.Is_Line (K);

private

   use Private_Line_References;

   type Cursor is record

      Text_Access : access constant RO_Text_Interfa'Class;

      Line_Idx : Text_Line_Count;

   end record;

   type Controlled_Line_Copy_Key
     is new Ada.Finalization.Controlled with record

      Key : Line_Copy_Key;

   end record;

   overriding
   procedure Adjust (Obj : in out Controlled_Line_Copy_Key);

   overriding
   procedure Finalize (Obj : in out Controlled_Line_Copy_Key);

   type Constant_Reference_Type
     (Line : not null access constant Character_Array) is record

      -- TODOC: 'Program_Error' is raised when a 'Constant_Reference_Type'
      -- object is instantiated with default initialization, as required by RM.
      -- <2020-04-11>
      -- REF: ARM A18.2(147.4/3), ARM A18.10(125/3), ... <2020-04-01>
      Controlled_Key : Controlled_Line_Copy_Key
        := raise Program_Error
          with "Uninitialized 'Constant_Reference_Type' instance creation "
               & "attempt.";

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
                  Position : Cursor) return Cursor
     is (Next (Position));

   overriding
   function Previous (Obj      : Iterator;
                      Position : Cursor) return Cursor
     is (Previous (Position));

   type RO_Text_Single_Line is new Ada.Finalization.Controlled
                                     and
                                   RO_Text_Interfa with record

      -- TODOC: Not guaranteed to be 1-based. <2020-04-10>
      A : Character_Array_Access;

   end record;

   type New_Line_Index_Array_Access is access New_Line_Index_Array;

   -- TODOC: After instantiation with defaults, 'Is_Empty' primitive returns
   -- true. <2020-04-12>
   type RO_Text_Multi_Line is new Ada.Finalization.Controlled
                                     and
                                  RO_Text_Interfa with record

      -- TODOC: Not guaranteed to be 1-based. <2020-04-10>
      A : Character_Array_Access;

      -- TODOC: Not guaranteed to be 1-based. <2020-04-10>
      N : New_Line_Index_Array_Access;

   end record
     with Type_Invariant => (
                              (RO_Text_Multi_Line.A = null)
                                =
                              (RO_Text_Multi_Line.N = null)
                            )
                              and then
                            (
                              (RO_Text_Multi_Line.A = null)
                                =
                              RO_Text_Multi_Line.Is_Empty
                            );

end Apsepp.Text_Class.R;
