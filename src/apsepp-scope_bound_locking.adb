-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Barrier_Class.Finalized_Handler;

package body Apsepp.Scope_Bound_Locking is

   ----------------------------------------------------------------------------

   overriding
   function Locked (Obj : Lock) return Boolean is

      -- Instantiate a controlled barrier handler to protect access to the lock
      -- ('Lock').
      use Apsepp.Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

      pragma Unreferenced (H);

   begin

      return Obj.Locked_Flag;

   end Locked;

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Controlled_Lock_Holder) is

      Unreferenced_Return_Value : Boolean;
      pragma Unreferenced (Unreferenced_Return_Value);

   begin

      -- Attempt to take the lock.
      Unreferenced_Return_Value := Controlled_Lock_Holder'Class (Obj).Take;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Lock_Holder) is

      Unreferenced_Return_Value : Boolean;
      pragma Unreferenced (Unreferenced_Return_Value);

   begin

      -- Release the lock if we hold it.
      Unreferenced_Return_Value := Controlled_Lock_Holder'Class (Obj).Release;

   end Finalize;

   ----------------------------------------------------------------------------

   overriding
   function Holds (Obj : Controlled_Lock_Holder) return Boolean
     is (Obj.Holds_Flag);

   ----------------------------------------------------------------------------

   overriding
   function Take (Obj : in out Controlled_Lock_Holder) return Boolean is

      Ret : Boolean := False; -- Return value (returned as is if we already
                              -- hold the lock).

   begin

      if not Controlled_Lock_Holder'Class (Obj).Holds then
         -- We don't already hold the lock.

         declare

            -- Instantiate a controlled barrier handler to protect access to
            -- the lock ('Obj.L').
            use Apsepp.Barrier_Class.Finalized_Handler;
            H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

            pragma Unreferenced (H);

         begin
            -- Here we access the lock ('Obj.L'). Calling Obj.L.Locked in this
            -- block would cause a deadlock.

            -- We are the new holder of the lock if it was not already held.
            Obj.Holds_Flag := not Obj.L.Locked_Flag;

            -- 'Obj.L.Locked_Flag' has to be true because we take the lock or
            -- because we don't take it (because it's already held).
            Obj.L.Locked_Flag := True;

         end;

         if Controlled_Lock_Holder'Class (Obj).Holds then
            -- We are now holding the lock.

            Controlled_Lock_Holder'Class (Obj).On_Take;

         end if;

         Ret := Controlled_Lock_Holder'Class (Obj).Holds;

      end if;

      return Ret;

   end Take;

   ----------------------------------------------------------------------------

   overriding
   function Release (Obj : in out Controlled_Lock_Holder) return Boolean is

      Ret : Boolean := False; -- Return value (returned as is if we were not
                              -- holding the lock).

   begin

      if Controlled_Lock_Holder'Class (Obj).Holds then
         -- We are (still) holding the lock.

         Controlled_Lock_Holder'Class (Obj).On_Release;

         declare

            -- Instantiate a controlled barrier handler to protect access to
            -- the lock ('Obj.L').
            use Apsepp.Barrier_Class.Finalized_Handler;
            H : Controlled_Barrier_Handler (Lock_Protection_Barrier'Access);

            pragma Unreferenced (H);

         begin
            -- Here we access the lock ('Obj.L'). Calling Obj.L.Locked in this
            -- block would cause a deadlock.

            Obj.L.Locked_Flag := False;

         end;

         -- We are not the lock holder any more.
         Obj.Holds_Flag := False;

         Ret := True;

      end if;

      return Ret;

   end Release;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Bound_Locking;
