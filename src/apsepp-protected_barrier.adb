-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Protected_Barrier is

   ----------------------------------------------------------------------------

   protected body Barrier is

      -----------------------------------------------------

      function Closed return Boolean
        is (Closed_Flag);

      -----------------------------------------------------

      entry Close when not Closed_Flag is
      begin
         Closed_Flag := True;
      end Close;

      -----------------------------------------------------

      procedure Open is
      begin
         Closed_Flag := False;
      end Open;

      -----------------------------------------------------

   end Barrier;

   ----------------------------------------------------------------------------

end Apsepp.Protected_Barrier;
