-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with System;

private with Ada.Containers.Ordered_Sets,
             Apsepp.Generic_Safe_Counter,
             Apsepp.Protected_Barrier;

private package Apsepp.Text_Class.Private_Line_References is

   -- Omit run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Ignore);

   type Character_Array_Access is access Character_Array;

   -- TODOC: Wraps an access to a copy of a line of a text object. Needed
   -- because some text objects store lines as slices of a larger character
   -- array and there is no access to array slice in Ada. <2020-04-19>
   -- REF: ARM 4.1.2(8). <2020-04-19>
   -- TODOC: Also includes a reference counter. <2020-04-19>
   type Line_Copy is private;

   -- TODOC: Component of 'Line_Copy'. <2020-04-19>
   type Line_Copy_Key is private;

   Null_Line_Copy_Key : constant Line_Copy_Key;

   use type System.Address;

   function Text_Address_From_Key
     (Key : Line_Copy_Key) return System.Address;

   function Line_Index_From_Key
     (Key : Line_Copy_Key) return Text_Line_Index;

   -- TODOC: True if an 'Line_Copy' object with key 'Key' is in the line copy
   -- set ('L_C_R_Set' declared in private part). <2020-04-19>
   function Is_In_Set (Key : Line_Copy_Key) return Boolean;

   function Key (L_C : Line_Copy) return Line_Copy_Key;

   function Line_Copy_Line_Access
     (L_C : Line_Copy) return Character_Array_Access;

   -- TODOC: Creates a 'Line_Copy' object and adds it to the set if no object
   -- with the same key is already in the set. In the opposite case,
   -- increments the reference counter. <2020-04-19>
   procedure Get_Line_Copy (Text :     Text_Interfa'Class;
                            K    :     Text_Line_Index;
                            Data : out Line_Copy)
     with Pre  => Text.Is_Line (K),
          Post => Text_Address_From_Key (Key (Data)) /= System.Null_Address
                    and then
                  Is_In_Set (Key (Data));

   -- TODOC: Increments the reference counter for the 'Line_Copy' element of
   -- the set with key 'Key'. <2020-04-19>
   procedure Add_Line_Copy_Reference (Key : Line_Copy_Key)
     with Pre  => Is_In_Set (Key),
          Post => Is_In_Set (Key);

   -- TODOC: Decrements the reference counter for the 'Line_Copy' element of
   -- the set with key 'Key', or do nothing if the reference counter had
   -- reached stauration. <2020-04-19>
   procedure Remove_Line_Copy_Reference (Key : Line_Copy_Key)
     with Pre => Is_In_Set (Key);

private

   type Line_Copy_Key is record

      Text_Address : System.Address;

      Index : Text_Line_Index;

      Hash : Text_Hash;

   end record;

   Null_Line_Copy_Key : constant Line_Copy_Key
     := (Text_Address => System.Null_Address,
         Index        => 1,
         Hash         => 0);

   function "<" (Left, Right : Line_Copy_Key) return Boolean;

   type Reference_Count is mod 2 ** 8; -- Range 0 to 255.

   package Safe_Reference_Counter
     is new Generic_Safe_Counter (Integer_Type => Reference_Count);

   type Line_Copy is record

      Key : Line_Copy_Key;

      Ref_C : Safe_Reference_Counter.Safe_Counter;

      A : Character_Array_Access;

   end record;

   function "<" (Left, Right : Line_Copy) return Boolean;

   package L_C_R_Ordered_Sets
     is new Ada.Containers.Ordered_Sets (Element_Type => Line_Copy);

   package L_C_R_Ordered_Sets_Generic_Keys
     is new L_C_R_Ordered_Sets.Generic_Keys
     (Key_Type => Line_Copy_Key,
      Key      => Key);

   L_C_R_Set : L_C_R_Ordered_Sets.Set;

   L_C_R_Set_Protection_Barrier : aliased Protected_Barrier.Barrier;

end Apsepp.Text_Class.Private_Line_References;
