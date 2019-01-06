-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Access_Setter;
with Apsepp.Output;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Ada.Assertions;
      use Apsepp.Output;

   begin

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         use Ada.Tags;

         package Output_Access_Setter
           is new Shared_Instance.Access_Setter (Output_Instance'Access);

         Output_Tag_Str : constant String := Expanded_Name (Output'Tag);

      begin

         Assert (Output_Access_Setter.Has_Actually_Set);
         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);
         Output.Put_Line ("Output sink instance tag:");
         Output.Put_Line (Output_Tag_Str);

         declare

            package Output_Access_Setter
              is new Shared_Instance.Access_Setter (Output_Instance'Access);

         begin

            Assert (not Output_Access_Setter.Has_Actually_Set);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         package Output_Access_Setter
           is new Shared_Instance.Access_Setter (null);

      begin

         Assert (Output_Access_Setter.Has_Actually_Set);
         Assert (Shared_Instance.Locked);
         Assert (not Shared_Instance.Instantiated);

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
