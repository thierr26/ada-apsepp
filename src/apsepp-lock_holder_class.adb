-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Lock_Holder_Class is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Lock_Holder_Controlled_Handler) is

   begin

      if not Obj.Disabled then

         declare

            Unreferenced_Return_Value : Boolean;
            pragma Unreferenced (Unreferenced_Return_Value);

         begin

            -- Attempt to take the lock.
            Unreferenced_Return_Value := Obj.L_H.Take;

         end;

      end if;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Lock_Holder_Controlled_Handler) is

   begin

      if not Obj.Disabled then

         declare

            Unreferenced_Return_Value : Boolean;
            pragma Unreferenced (Unreferenced_Return_Value);

         begin

            -- Release the lock if we hold it.
            Unreferenced_Return_Value := Obj.L_H.Release;

         end;

      end if;

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Lock_Holder_Class;
