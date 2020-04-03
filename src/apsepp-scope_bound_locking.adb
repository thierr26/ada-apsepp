-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Barrier_Class.Finalized_Handler;

package body Apsepp.Scope_Bound_Locking is

   ----------------------------------------------------------------------------

   overriding
   function Locked (Obj : Lock) return Boolean is

      -- Instantiate a controlled barrier handler to protect access to the lock
      -- ('Lock').
      use Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

      pragma Unreferenced (H);

   begin

      return Obj.Locked_Flag;

   end Locked;

   ----------------------------------------------------------------------------

   package body Lock_Holder_Package is

      -----------------------------------------------------

      overriding
      function Holds (Obj : Lock_Holder) return Boolean
        is (Obj.Holds_Flag);

      -----------------------------------------------------

      overriding
      function Take (Obj : in out Lock_Holder) return Boolean is

         Ret : Boolean := False; -- Return value (returned as is if we already
                                 -- hold the lock).

      begin

         if not Obj.Holds_Flag then
            -- We don't already hold the lock.

            declare

               -- Instantiate a controlled barrier handler to protect access to
               -- the lock ('Obj.L').
               use Barrier_Class.Finalized_Handler;
               H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

               pragma Unreferenced (H);

            begin
               -- Here we access the lock ('Obj.L'). Calling Obj.L.Locked in
               -- this block would cause a deadlock.

               -- We are the new holder of the lock if it was not already held.
               Obj.Holds_Flag := not Obj.L.Locked_Flag;

               -- 'Obj.L.Locked_Flag' has to be true because we take the lock
               -- or because we don't take it (because it's already held).
               Obj.L.Locked_Flag := True;

            end;

            if Obj.Holds_Flag then
               -- We are now holding the lock.

               Lock_Holder'Class (Obj).On_Take;
               Obj.L.On_Lock;

            end if;

            Ret := Obj.Holds_Flag;

         end if;

         return Ret;

      end Take;

      -----------------------------------------------------

      overriding
      function Release (Obj : in out Lock_Holder) return Boolean is

         Ret : Boolean := False; -- Return value (returned as is if we were not
                                 -- holding the lock).

      begin

         if Obj.Holds_Flag then
            -- We are (still) holding the lock.

            Lock_Holder'Class (Obj).On_Release;

            declare

               -- Instantiate a controlled barrier handler to protect access to
               -- the lock ('Obj.L').
               use Barrier_Class.Finalized_Handler;
               H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

               pragma Unreferenced (H);

            begin
               -- Here we access the lock ('Obj.L'). Calling Obj.L.Locked in
               -- this block would cause a deadlock.

               Obj.L.Locked_Flag := False;

            end;

            -- We are not the lock holder any more.
            Obj.Holds_Flag := False;
            Obj.L.On_Unlock;

            Ret := True;

         end if;

         return Ret;

      end Release;

      -----------------------------------------------------

   end Lock_Holder_Package;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locking;
