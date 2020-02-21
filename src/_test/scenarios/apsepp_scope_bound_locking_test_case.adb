-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp_Scope_Bound_Locking_Test_Case is

   use Apsepp.Scope_Bound_Locking,
       Apsepp_Scope_Bound_Locking_Test_Fixture;

   ----------------------------------------------------------------------------

   Global_Lock_Holder_Access : access Controlled_Lock_Holder;

   function SBL_TF return not null access Apsepp_Scope_Bound_Locking_T_F is

   begin

      return Fixture_Instance_Access (Global_Lock_Holder_Access.all);

   end SBL_TF;

   ----------------------------------------------------------------------------

   procedure Scope_Bound_Locking_Test is

   begin

      -- TODO: Implement real test routine for Apsepp.Scope_Bound_Locking.
      -- <2020-02-20>
      Assert (SBL_TF.Lock_Count = 0);

   end Scope_Bound_Locking_Test;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Array
     (Obj : Apsepp_Scope_Bound_Locking_T_C) return Test_Routine_Array
     is (1 => Scope_Bound_Locking_Test'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Apsepp_Scope_Bound_Locking_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test) is

   begin

      case Kind is

         when Check_Cond =>

            declare

               function Cond return Boolean
                 is (not SBL_TF_LA.Locked);

            begin

               Run_Body (Obj, Outcome, Kind, Cond'Access);

            end;

         when Assert_Cond_And_Run_Test =>

            declare

               SBL_TF_LH : aliased Controlled_Lock_Holder (SBL_TF_LA);

               function Cond return Boolean
                 is (SBL_TF_LH.Holds);

            begin

               Global_Lock_Holder_Access := SBL_TF_LH'Unchecked_Access;

               Run_Body (Obj, Outcome, Kind, Cond'Access);

            end;

      end case;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Bound_Locking_Test_Case;
