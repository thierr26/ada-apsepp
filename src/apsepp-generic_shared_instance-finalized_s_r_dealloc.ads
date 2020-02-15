-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Ada.Finalization;

generic

   Instance_Access : access Instance_Ancestor_Type'Class;

   type Lock_Holder_Type (<>)
     is limited new Controlled_Lock_Holder with private;

   Lock_Holder : in out Lock_Holder_Type;

package Apsepp.Generic_Shared_Instance.Finalized_S_R_Dealloc is

private

   type Controlled_Set_Reset_Runner
     is limited new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Initialize (Obj : in out Controlled_Set_Reset_Runner);

   overriding
   procedure Finalize (Obj : in out Controlled_Set_Reset_Runner);

   S_R_Kind_Value : constant S_R_Kind := W_Deallocation;

end Apsepp.Generic_Shared_Instance.Finalized_S_R_Dealloc;
