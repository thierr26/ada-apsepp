-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp_Scope_Debug_Test_Fixture is

   ----------------------------------------------------------------------------

   Scope_Entry_Count_Var : Natural := 0;
   Scope_Exit_Count_Var  : Natural := 0;

   ----------------------------------------------------------------------------

   procedure Increment_Scope_Entry_Count is

   begin

      Scope_Entry_Count_Var := Scope_Entry_Count_Var + 1;

   end Increment_Scope_Entry_Count;

   ----------------------------------------------------------------------------

   procedure Increment_Scope_Exit_Count is

   begin

      Scope_Exit_Count_Var := Scope_Exit_Count_Var + 1;

   end Increment_Scope_Exit_Count;

   ----------------------------------------------------------------------------

   not overriding
   procedure Reset (Obj : Scope_Debug_Test_Fixture) is

      pragma Unreferenced (Obj);

   begin

      Scope_Entry_Count_Var := 0;
      Scope_Exit_Count_Var  := 0;

   end Reset;

   ----------------------------------------------------------------------------

   not overriding
   function Scope_Entry_Count (Obj : Scope_Debug_Test_Fixture) return Natural
     is (Scope_Entry_Count_Var);

   ----------------------------------------------------------------------------

   not overriding
   function Scope_Exit_Count (Obj : Scope_Debug_Test_Fixture) return Natural
     is (Scope_Exit_Count_Var);

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj         : SDFDT;
                    Message     : String;
                    Entity_Name : String := "") is

      pragma Unreferenced (Obj, Entity_Name);

   begin

      if Message = "Entry" then
         Increment_Scope_Entry_Count;
      elsif Message = "Exit" then
         Increment_Scope_Exit_Count;
      end if;

   end Trace;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Debug_Test_Fixture;
