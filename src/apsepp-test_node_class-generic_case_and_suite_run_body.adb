-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags;
with Ada.Assertions;
with Apsepp.Test_Node_Class.Private_Test_Reporter;

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
            Test_Reporter.Report_Unexpected_Node_Cond_Check_Error (T, Cond_E);
      end;

      return Ret;

   end Cond_Check_Succeed;

      -----------------------------------------------------

begin

   case Kind is

      when Check_Cond =>
         Test_Reporter.Report_Node_Cond_Check_Start (T);
         if Cond_Check_Succeed then
            Outcome := Passed;
            Test_Reporter.Report_Passed_Node_Cond_Check (T);
         else
            Outcome := Failed;
            Test_Reporter.Report_Failed_Node_Cond_Check (T);
         end if;

      when Assert_Cond_And_Run_Test =>
         Test_Reporter.Report_Node_Run_Start (T);
         if not Cond_Check_Succeed then
            Test_Reporter.Report_Failed_Node_Cond_Assert (T);
            raise Assertion_Error
              with "Unmet run condition for object with tag "
                   & Ada.Tags.Expanded_Name (T);
         else
            Test_Reporter.Report_Passed_Node_Cond_Assert (T);
         end if;

         begin
            Work (Obj, Outcome, Assert_Cond_And_Run_Test);
         exception
            when Run_E : others =>
               Outcome := Failed;
               Test_Reporter.Report_Unexpected_Node_Run_Error (T, Run_E);
         end;

         case Outcome is
            when Passed =>
               Test_Reporter.Report_Passed_Node_Run (T);
            when Failed =>
               Test_Reporter.Report_Failed_Node_Run (T);
         end case;

   end case;

end Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;
