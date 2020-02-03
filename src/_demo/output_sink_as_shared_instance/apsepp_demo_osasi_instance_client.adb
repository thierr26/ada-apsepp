-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- "With" the shared output sink instance access point package.
with Apsepp.Output;

package body Apsepp_Demo_OSASI_Instance_Client is

   ----------------------------------------------------------------------------

   B_Count : Natural := 0;

   function B return String is

      N : constant Natural := B_Count + 1;
      Ret : String := Natural'Image (N) & " ";

   begin

      Ret (Ret'First) := 'B';

      B_Count := N;

      return Ret;

   end B;

   ----------------------------------------------------------------------------

   procedure Output_Sink_Client is

      use Apsepp.Output; -- Makes function Apsepp.Output.Output visible (access
                         -- to the shared output sink instance).

   begin

      -- Call primitive operation of output sink instance.
      Output.Put_Line (B & "Hello!");
                -- Displayed if current shared output sink instance is of type
                -- Apsepp.Output_Class.Standard.Output_Standard but not if
                -- it is of type Apsepp.Output_Class.Quiet.Output_Quiet.

   end Output_Sink_Client;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Instance_Client;
