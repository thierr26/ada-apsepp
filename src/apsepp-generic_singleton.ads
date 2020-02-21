-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Finalization;

private with Apsepp.Protected_Barrier;

generic
package Apsepp.Generic_Singleton is

   -- Force pre-conditions evaluation in this package.
   pragma Assertion_Policy (Pre => Check);

   type Singleton
     is limited new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Initialize (Obj : in out Singleton);

   overriding
   procedure Finalize (Obj : in out Singleton);

   function Instantiated return Boolean;

private

   Instantiated_Flag_Barrier : aliased Protected_Barrier.Barrier;

   Instantiated_Flag : Boolean := False;

   type Singleton
     is limited new Ada.Finalization.Limited_Controlled with null record;

end Apsepp.Generic_Singleton;
