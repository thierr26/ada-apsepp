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

   use Private_Test_Reporter;
   use Ada.Assertions;

      -----------------------------------------------------

   function Cond_Check_Succeed return Boolean is

      Ret : Boolean := False;

   begin

      begin
         Ret := Cond.all;
      exception
         when others =>
            Ret := False;
            Test_Reporter.Report_Unexpected_Node_Cond_Check_Error (Obj'Tag);
      end;

      return Ret;

   end Cond_Check_Succeed;

      -----------------------------------------------------

begin

   case Kind is

      when Check_Cond =>
         Test_Reporter.Report_Node_Cond_Check_Start (Obj'Tag);
         if Cond_Check_Succeed then
            Outcome := Passed;
            Test_Reporter.Report_Passed_Node_Cond_Check (Obj'Tag);
         else
            Outcome := Failed;
            Test_Reporter.Report_Failed_Node_Cond_Check (Obj'Tag);
         end if;

      when Assert_Cond_And_Run_Test =>
         Test_Reporter.Report_Node_Run_Start (Obj'Tag);
         if not Cond_Check_Succeed then
            Test_Reporter.Report_Failed_Node_Cond_Assert (Obj'Tag);
            raise Assertion_Error
              with "Unmet run condition for object with tag "
                   & Ada.Tags.Expanded_Name (Obj'Tag);
         else
            Test_Reporter.Report_Passed_Node_Cond_Assert (Obj'Tag);
         end if;

         begin
            Work (Obj, Outcome, Assert_Cond_And_Run_Test);
         exception
            when others =>
               Outcome := Failed;
               Test_Reporter.Report_Unexpected_Node_Run_Error (Obj'Tag);
         end;

         case Outcome is
            when Passed =>
               Test_Reporter.Report_Passed_Node_Run (Obj'Tag);
            when Failed =>
               Test_Reporter.Report_Failed_Node_Run (Obj'Tag);
         end case;

   end case;

end Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;
