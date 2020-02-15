-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.
--
with Ada.Finalization; use Ada.Finalization;

package Apsepp.Barrier_Class.Finalized_Handler is

   type Controlled_Barrier_Handler
     (B : not null access Barrier_Interfa'Class)
     is limited new Limited_Controlled with private
     with Type_Invariant'Class => B.Closed
                                    xor
                                  Controlled_Barrier_Handler.Finalized;

   overriding
   procedure Initialize (Obj : in out Controlled_Barrier_Handler);

   overriding
   procedure Finalize (Obj : in out Controlled_Barrier_Handler);

   not overriding
   function Finalized (Obj : Controlled_Barrier_Handler) return Boolean;

private

   type Controlled_Barrier_Handler
     (B : not null access Barrier_Interfa'Class)
     is limited new Limited_Controlled with record

      Finalized_Flag : Boolean := False;

   end record;

end Apsepp.Barrier_Class.Finalized_Handler;
