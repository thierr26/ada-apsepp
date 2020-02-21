-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Barrier_Class.Finalized_Handler;

package body Apsepp.Generic_Singleton is

   ----------------------------------------------------------------------------

   function Instantiated return Boolean is

      -- Instantiate a controlled barrier handler to protect access to
      -- 'Instantiated_Flag'.
      use Apsepp.Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (Instantiated_Flag_Barrier'Access);

      pragma Unreferenced (H);

   begin

      return Instantiated_Flag;

   end Instantiated;

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Singleton) is

      -- Instantiate a controlled barrier handler to protect access to
      -- 'Instantiated_Flag'.
      use Apsepp.Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (Instantiated_Flag_Barrier'Access);

      Error_Message : constant String
        := "'Singleton' type already instantiated.";

      pragma Unreferenced (Obj, H);

   begin

      Instantiated_Flag := (if not Instantiated_Flag then
                               True
                            else
                               raise Program_Error with Error_Message);

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Singleton) is

      -- Instantiate a controlled barrier handler to protect access to
      -- 'Instantiated_Flag'.
      use Apsepp.Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (Instantiated_Flag_Barrier'Access);

      pragma Unreferenced (Obj, H);

   begin

      Instantiated_Flag := False;

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Singleton;
