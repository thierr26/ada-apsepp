-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Ada.Assertions,
     Apsepp.Test_Node_Class.Private_Test_Reporter;

procedure Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body
  (Obj     :     Test_Node_Interfa'Class;
   Outcome : out Test_Outcome;
   Kind    :     Run_Kind;
   Cond    :     not null access function return Boolean) is

   use Private_Test_Reporter,
       Ada.Assertions;

   T : constant Tag := Obj'Tag;

      -----------------------------------------------------

   function Cond_Check_Succeed return Boolean is

      Ret : Boolean;

   begin

      begin
         Ret := Cond.all;
      exception
         when Cond_E : others =>
            Ret := False;
            Test_Reporter.Report_Unexpected_Node_Cond_Check_Error
              (Node_Tag => T,
               Error    => Cond_E);
      end;

      return Ret;

   end Cond_Check_Succeed;

      -----------------------------------------------------

begin

   case Kind is

      when Check_Cond =>
         Test_Reporter.Report_Node_Cond_Check_Start (Node_Tag => T);
         if Cond_Check_Succeed then
            Outcome := Passed;
            Test_Reporter.Report_Passed_Node_Cond_Check (Node_Tag => T);
         else
            Outcome := Failed;
            Test_Reporter.Report_Failed_Node_Cond_Check (Node_Tag => T);
         end if;

      when Assert_Cond_And_Run_Test =>
         Test_Reporter.Report_Node_Run_Start (Node_Tag => T);
         if not Cond_Check_Succeed then
            Test_Reporter.Report_Failed_Node_Cond_Assert (Node_Tag => T);
            raise Assertion_Error
              with "Unmet run condition for object with tag "
                   & Ada.Tags.Expanded_Name (T);
         else
            Test_Reporter.Report_Passed_Node_Cond_Assert (Node_Tag => T);
         end if;

         begin
            Work (Obj, Outcome);
         exception
            when Run_E : others =>
               Outcome := Failed;
               Test_Reporter.Report_Unexpected_Node_Run_Error
                 (Node_Tag => T,
                  Error    => Run_E);
         end;

         case Outcome is
            when Passed =>
               Test_Reporter.Report_Passed_Node_Run (Node_Tag => T);
            when Failed =>
               Test_Reporter.Report_Failed_Node_Run (Node_Tag => T);
         end case;

   end case;

end Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;
