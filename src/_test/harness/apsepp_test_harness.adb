-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp.Output;
with Apsepp.Output_Class.Standard.Create;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Ada.Assertions;
      use Apsepp.Output;

      function Create return Apsepp.Output_Class.Standard.Output_Standard
        renames Apsepp.Output_Class.Standard.Create;

   begin

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         use Ada.Tags;
         use Apsepp.Output_Class.Standard;

         package Output_Creator
           is new Shared_Instance.Creator (Output_Standard, Create);

         Output_Tag_Str : constant String := Expanded_Name (Output'Tag);

      begin

         Assert (Output_Creator.Has_Actually_Created);
         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);
         Output.Put_Line ("Output sink instance tag:");
         Output.Put_Line (Output_Tag_Str);

         declare

            package Output_Creator
              is new Shared_Instance.Creator (Output_Standard, Create);

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);

         declare

            package Output_Creator
              is new Shared_Instance.Creator (Instance_Type => Output_Standard,
                                              Create        => Create,
                                              Just_Pretend  => True);

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         use Apsepp.Output_Class.Standard;

         package Output_Creator
           is new Shared_Instance.Creator (Instance_Type => Output_Standard,
                                           Create        => Create,
                                           Just_Pretend  => True);

      begin

         Assert (Output_Creator.Has_Actually_Created);
         Assert (Shared_Instance.Locked);
         Assert (not Shared_Instance.Instantiated);

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
