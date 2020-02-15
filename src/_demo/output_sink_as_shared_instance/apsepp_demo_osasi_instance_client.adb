-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Generic_Char_Count_String,
     Apsepp_Demo_OSASI_Constants;

-- "With" the shared output sink instance access point package.
with Apsepp.Output;

package body Apsepp_Demo_OSASI_Instance_Client is

   use Apsepp_Demo_OSASI_Constants;

   ----------------------------------------------------------------------------

   package D_Count_String is new Generic_Char_Count_String
     (Char        => 'D',
      Suffix      => " ",
      Count_Width => Line_Count_Width);

   function D return D_Count_String.Char_Count_String renames D_Count_String.S;

   ----------------------------------------------------------------------------

   procedure Output_Sink_Client is

      use Apsepp.Output; -- Makes function 'Apsepp.Output.Output' visible
                         -- (access to the shared output sink instance).

   begin

      -- Call primitive operation of output sink instance. This prints a line
      -- on the standard output if current shared output sink instance is of
      -- type 'Apsepp.Output_Class.Standard.Output_Standard' but not if it is
      -- of type 'Apsepp.Output_Class.Quiet.Output_Quiet'.
      Output.Put_Line (D & "Sample output.");
                                             -- Output line D01, D02, D05, D06.

   end Output_Sink_Client;

   ----------------------------------------------------------------------------

end Apsepp_Demo_OSASI_Instance_Client;
