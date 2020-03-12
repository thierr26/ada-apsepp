-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.
--
with Ada.Finalization; use Ada.Finalization;

package Apsepp.Barrier_Class.Finalized_Handler is

   type Controlled_Barrier_Handler
     (B : not null access Barrier_Interfa'Class)
     is limited new Limited_Controlled with private;

   overriding
   procedure Initialize (Obj : in out Controlled_Barrier_Handler);

   overriding
   procedure Finalize (Obj : in out Controlled_Barrier_Handler);

private

   type Controlled_Barrier_Handler
     (B : not null access Barrier_Interfa'Class)
     is limited new Limited_Controlled with null record;

end Apsepp.Barrier_Class.Finalized_Handler;
