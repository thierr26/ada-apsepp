-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Generic_Shared_Instance is

   ----------------------------------------------------------------------------

   protected body Protected_Instance_Access is

      -----------------------------------------------------

      procedure Set_Instance_Lock_Locked_State (State : Locked_State) is

      begin

         Instance_Lock_Locked_State := State;

      end Set_Instance_Lock_Locked_State;

      -----------------------------------------------------

      procedure Set (I_A : Instance_Ancestor_Access) is

      begin

         Instance_Access := I_A;

      end Set;

      -----------------------------------------------------

      procedure Get (I_A : out Instance_Ancestor_Access) is

      begin

         I_A := Instance_Access;

      end Get;

      -----------------------------------------------------

      entry Get_W_Barrier
        (I_A : out Instance_Ancestor_Access)
        when Instance_Lock_Locked_State /= Locked
               or else
             Instance_Access /= null is

      begin

         Get (I_A);

      end Get_W_Barrier;

      -----------------------------------------------------

   end Protected_Instance_Access;

   ----------------------------------------------------------------------------

   function Fallback (I_A : Instance_Ancestor_Access)
     return access Instance_Ancestor_Type'Class
     is (if I_A = null then
            Fallback_Instance_Access
         else
            I_A);

   ----------------------------------------------------------------------------

   function Unguarded_Instance_Access
     return access Instance_Ancestor_Type'Class is

      I_A : Instance_Ancestor_Access;

   begin

      Protected_Instance_Access.Get (I_A);

      return Fallback (I_A);

   end Unguarded_Instance_Access;

   ----------------------------------------------------------------------------

   function Instance_Access return access Instance_Ancestor_Type'Class is

      I_A : Instance_Ancestor_Access;

   begin

      Protected_Instance_Access.Get_W_Barrier (I_A);

      return Fallback (I_A);

   end Instance_Access;

   ----------------------------------------------------------------------------

   procedure Set
     (L_H : Controlled_Lock_Holder'Class;
      I_A : access Instance_Ancestor_Type'Class) is

   begin

      if L_H.Holds then
         -- The lock holder actually holds the lock.

         Protected_Instance_Access.Set (Instance_Ancestor_Access (I_A));

      end if;

   end Set;

   ----------------------------------------------------------------------------

   procedure Reset (L_H : Controlled_Lock_Holder'Class) is

   begin

      Set (L_H, null);

   end Reset;

   ----------------------------------------------------------------------------

   procedure Parameterized_S
     (Kind : S_R_Kind;
      L_H  : Controlled_Lock_Holder'Class;
      I_A  : access Instance_Ancestor_Type'Class) is

      pragma Unreferenced (Kind);

   begin

      -- Store the state of the instance lock (used in barrier of
      -- 'Protected_Instance_Access.Get_W_Barrier').
      Protected_Instance_Access.Set_Instance_Lock_Locked_State
        (if Instance_Lock.Locked then
            Locked
         else
            Unlocked);

      -- The next statement fails if 'L_H.L /= Instance_Lock'Access' (see the
      -- pre-condition), and this is the wanted behaviour.
      Set (L_H, I_A);

   end Parameterized_S;

   ----------------------------------------------------------------------------

   procedure Parameterized_R
     (Kind : S_R_Kind;
      L_H  : Controlled_Lock_Holder'Class) is

      procedure Call_Reset is
      begin
         -- The next statement fails if 'L_H.L /= Instance_Lock'Access' (see
         -- the pre-condition), and this is the wanted behaviour.
         Reset (L_H);
      end Call_Reset;

   begin

      -- Reset the stored value of the state of the instance lock (used in
      -- barrier of 'Protected_Instance_Access.Get_W_Barrier').
      Protected_Instance_Access.Set_Instance_Lock_Locked_State (Unknown);

      case Kind is

         when W_Deallocation =>

            if L_H.Holds then
               -- The lock holder actually holds the lock.

               declare

                  procedure Free is new Ada.Unchecked_Deallocation
                    (Object => Instance_Ancestor_Type'Class,
                     Name   => Instance_Ancestor_Access);

                  Local_Instance_Access : Instance_Ancestor_Access;

               begin

                  Protected_Instance_Access.Get (Local_Instance_Access);

                  Call_Reset;

                  Free (Local_Instance_Access);

               end;

            end if;

         when Wo_Deallocation =>

            Call_Reset;

      end case;

   end Parameterized_R;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Shared_Instance;
