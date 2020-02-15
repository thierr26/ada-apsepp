-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Shared_Instance.Finalized_S_R is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Controlled_Set_Reset_Runner) is

      pragma Unreferenced (Obj);

   begin

      Parameterized_S (S_R_Kind_Value, Lock_Holder, Instance_Access);

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Set_Reset_Runner) is

      pragma Unreferenced (Obj);

   begin

      Parameterized_R (S_R_Kind_Value, Lock_Holder);

   end Finalize;

   ----------------------------------------------------------------------------

   Set_Reset_Runner : Controlled_Set_Reset_Runner;

   pragma Unreferenced (Set_Reset_Runner);

   ----------------------------------------------------------------------------

end Apsepp.Generic_Shared_Instance.Finalized_S_R;
