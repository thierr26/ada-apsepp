-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Ada.Tags;
with Apsepp.Generic_Shared_Instance.Creator;
with Apsepp.Output;
with Apsepp.Output_Class.Standard.Create;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   function Allocate_Output_Standard return Apsepp.Output.Output_Access is

      use Apsepp.Output;
      use Apsepp.Output_Class.Standard;

      Instance_Access : constant Output_Access := new Output_Standard'(Create);

   begin

     return Instance_Access;

   end Allocate_Output_Standard;

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Ada.Assertions;
      use Apsepp.Output;

   begin

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         use Ada.Tags;

         package Output_Creator
           is new Shared_Instance.Creator (Allocate_Output_Standard);

         Output_Tag_Str : constant String := Expanded_Name (Output'Tag);

      begin

         Assert (Output_Creator.Has_Actually_Created);
         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);
         Output.Put_Line ("Output sink instance tag:");
         Output.Put_Line (Output_Tag_Str);

         declare

            package Output_Creator
              is new Shared_Instance.Creator (Allocate_Output_Standard);

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

         Assert (Shared_Instance.Locked);
         Assert (Shared_Instance.Instantiated);

         declare

            package Output_Creator is new Shared_Instance.Creator
              (Allocate     => Allocate_Output_Standard,
               Just_Pretend => True);

         begin

            Assert (not Output_Creator.Has_Actually_Created);
            Assert (Shared_Instance.Locked);
            Assert (Shared_Instance.Instantiated);

         end;

      end;

      Assert (not Shared_Instance.Locked);
      Assert (not Shared_Instance.Instantiated);

      declare

         package Output_Creator is new Shared_Instance.Creator
           (Allocate     => Allocate_Output_Standard,
            Just_Pretend => True);

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
