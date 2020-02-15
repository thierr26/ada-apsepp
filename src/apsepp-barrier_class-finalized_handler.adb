-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Barrier_Class.Finalized_Handler is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Controlled_Barrier_Handler) is

   begin

      Obj.B.Close;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Barrier_Handler) is

   begin

      Obj.B.Open;
      Obj.Finalized_Flag := True;

   end Finalize;

   ----------------------------------------------------------------------------

   not overriding
   function Finalized (Obj : Controlled_Barrier_Handler) return Boolean
     is (Obj.Finalized_Flag);

   ----------------------------------------------------------------------------

end Apsepp.Barrier_Class.Finalized_Handler;
