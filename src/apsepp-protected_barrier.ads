-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Barrier_Class; use Apsepp.Barrier_Class;

package Apsepp.Protected_Barrier is

   protected type Barrier is new Barrier_Interfa with

      overriding
      function Closed return Boolean;

      overriding
      entry Close;

      overriding
      procedure Open;

   private

      Closed_Flag : Boolean := False;

   end Barrier;

end Apsepp.Protected_Barrier;
