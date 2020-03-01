-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locking; use Apsepp.Scope_Bound_Locking;

generic

   type Instance_Ancestor_Type (<>) is abstract tagged limited private;

   type Lock_Type is limited new Lock with private;

   Fallback_Instance_Access : access Instance_Ancestor_Type'Class := null;

package Apsepp.Generic_Shared_Instance is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   Instance_Lock : aliased Lock_Type;

   function Unguarded_Instance_Access
     return access Instance_Ancestor_Type'Class;

   function U return access Instance_Ancestor_Type'Class
     renames Unguarded_Instance_Access;

   function Instance_Access return access Instance_Ancestor_Type'Class
     with Post => Instance_Access'Result /= null
                    or else
                  not Instance_Lock.Locked;

   function A return access Instance_Ancestor_Type'Class
     renames Instance_Access;

   type Holder
     is limited new Controlled_Lock_Holder (L => Instance_Lock'Access)
     with null record;

   procedure Set
     (L_H : Controlled_Lock_Holder'Class;
      I_A : access Instance_Ancestor_Type'Class)
     with Pre  => L_H.L = Instance_Lock'Access;

   procedure Reset (L_H : Controlled_Lock_Holder'Class)
     with Pre  => L_H.L = Instance_Lock'Access;

private

   type Locked_State is (Unknown, Unlocked, Locked);

   type Instance_Ancestor_Access is access all Instance_Ancestor_Type'Class;

   protected Protected_Instance_Access is

      procedure Set_Instance_Lock_Locked_State (State : Locked_State);

      procedure Set (I_A : Instance_Ancestor_Access);

      procedure Reset;

      procedure Get (I_A : out Instance_Ancestor_Access);

      entry Get_W_Barrier (I_A : out Instance_Ancestor_Access);

   private

      Instance_Access : Instance_Ancestor_Access;

      Instance_Lock_Locked_State : Locked_State := Unknown;

      Set_Done : Boolean := False;

   end Protected_Instance_Access;

   type S_R_Kind is (W_Deallocation, Wo_Deallocation);

   procedure Parameterized_S
     (Kind : S_R_Kind;
      L_H  : Controlled_Lock_Holder'Class;
      I_A  : access Instance_Ancestor_Type'Class)
     with Pre => (case Kind is
                     when W_Deallocation  =>
                        not L_H.Holds or else I_A /= null,
                     when Wo_Deallocation =>
                        True);

   procedure Parameterized_R
     (Kind : S_R_Kind;
      L_H  : Controlled_Lock_Holder'Class);

end Apsepp.Generic_Shared_Instance;
