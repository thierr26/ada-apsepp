-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions;

package body Apsepp.Test_Node_Class.Abstract_Simu_Case is

   ----------------------------------------------------------------------------

   protected body Data_Locker is

      -----------------------------------------------------

      function Locked return Boolean
        is (Locked_Flag);

      -----------------------------------------------------

      entry Set (T : Tag; D : Test_Routine_Destiny) when not Locked is

      begin

         T_Val := T;
         D_Val := D;
         Locked_Flag := True;

      end Set;

      -----------------------------------------------------

      function T return Tag is

      begin

         Ada.Assertions.Assert (Locked); -- Precondition.
         return T_Val;

      end T;

      -----------------------------------------------------

      function D return Test_Routine_Destiny is

      begin

         Ada.Assertions.Assert (Locked); -- Precondition.
         return D_Val;

      end D;

      -----------------------------------------------------

      procedure Reset is

      begin

         Ada.Assertions.Assert (Locked); -- Precondition.
         Locked_Flag := False;

      end Reset;

      -----------------------------------------------------

   end Data_Locker;

   ----------------------------------------------------------------------------

   procedure Simu_Test_Routine is

      Simu_Test_Routine_Other_Error : exception;

      D       : constant Test_Routine_Destiny := Data_Locker.D;
      T       : constant Tag                  := Data_Locker.T;
      Handled :          Boolean              := False;

   begin

      Data_Locker.Reset;

      for K in 1 .. D.Successful_Test_Assert_Count loop

         Assert (T, True);

      end loop;

      case D.Kind is

         when No_Failure =>
            null;

         when Test_Assertion_Failure =>
            Assert (T, False, "Simulated test assertion failure");

         when Handled_Test_Failure =>
            Handled := True;
            Assert (T, False,
                    "Simulated test assertion failure (handled)");

         when Contract_Failure =>
            raise Ada.Assertions.Assertion_Error
              with "Simulated contract failure";

         when Other_Failure =>
            raise Simu_Test_Routine_Other_Error
              with "Simulated unexpected test routine failure";

         when Access_Failure =>
            null; -- Impossible case, has already
                  -- caused error and test routine
                  -- abortion in Routine.

         when Setup_Failure =>
            null; -- Impossible case, has already
                  -- caused error and test routine
                  -- abortion in Setup_Routine.

      end case;

   exception
      when others =>
         if not Handled then
            raise;
         end if;
   end Simu_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   function Routine (Obj : Simu_Test_Case;
                     K   : Test_Routine_Index) return Test_Routine is

      Simu_Test_Routine_Access_Error : exception;

      D : constant Test_Routine_Destiny
        := Simu_Test_Case'Class (Obj).Story (K);

   begin

      case D.Kind is

         when No_Failure
            | Setup_Failure
            | Test_Assertion_Failure
            | Handled_Test_Failure
            | Contract_Failure
            | Other_Failure =>
            null;

         when Access_Failure =>
            raise Simu_Test_Routine_Access_Error
              with "Simulated test routine access failure";

      end case;

      Data_Locker.Set (Simu_Test_Case'Class (Obj)'Tag, D);

      return Simu_Test_Routine'Access;

   end Routine;

   ----------------------------------------------------------------------------

   overriding
   procedure Setup_Routine (Obj : Simu_Test_Case) is

      Simu_Test_Routine_Setup_Error : exception;

      D : constant Test_Routine_Destiny := Data_Locker.D;

      pragma Unreferenced (Obj);

   begin

      case D.Kind is

         when No_Failure
            | Test_Assertion_Failure
            | Handled_Test_Failure
            | Contract_Failure
            | Other_Failure =>
            null;

         when Setup_Failure =>
            Data_Locker.Reset;
            raise Simu_Test_Routine_Setup_Error
              with "Simulated test routine access failure";

         when Access_Failure =>
            null; -- Impossible case, has already caused
                  -- error and test routine abortion in
                  -- Routine.

      end case;

   end Setup_Routine;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Simu_Case;
