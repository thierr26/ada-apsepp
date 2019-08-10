-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;
with Ada.Assertions;

package body Apsepp.Abstract_Early_Test_Case is

   ----------------------------------------------------------------------------

   protected body Data_Store is

      -----------------------------------------------------

      function Locked return Boolean
        is (Locked_Flag);

      -----------------------------------------------------

      entry Set (T : Tag;
                 E : Exception_Occurrence_Access) when not Locked_Flag is

      begin

         T_Val       := T;
         E_Access    := E;
         Locked_Flag := True;

      end Set;

      -----------------------------------------------------

      function T return Tag is

      begin

         Ada.Assertions.Assert (Locked);
         return T_Val;

      end T;

      -----------------------------------------------------

      function E return Exception_Occurrence_Access is

      begin

         Ada.Assertions.Assert (Locked);
         return E_Access;

      end E;


      -----------------------------------------------------

      procedure Reset is

      begin

         Locked_Flag := False;

      end Reset;

      -----------------------------------------------------

   end Data_Store;

   ----------------------------------------------------------------------------

   procedure Early_Run_Body (Obj : in out Early_Test_Case'Class) is

   begin

      Obj.Early_Run_Done_Flag := True;
      Obj.Early_Routine.all;

   exception

      when E : others => Obj.E := Save_Occurrence (E);

   end Early_Run_Body;

   ----------------------------------------------------------------------------

   procedure R is

      E : constant Exception_Occurrence_Access
        := Data_Store.E;

      E_M_Str : constant String
        := (if E = null or else Exception_Message (E.all)'Length = 0 then
               ""
            else
               " with """ & Exception_Message (E.all) & """");

      Msg : constant String := (if E = null then
                                   ""
                                else
                                   "Early test routine has raised "
                                     & Exception_Name (E.all) & E_M_Str);

   begin

      Assert (Data_Store.T, E = null, Msg);

   end R;

   ----------------------------------------------------------------------------

   overriding
   function Early_Run_Done (Obj : Early_Test_Case) return Boolean
     is (Obj.Early_Run_Done_Flag);

   ----------------------------------------------------------------------------

   overriding
   procedure Early_Run (Obj : in out Early_Test_Case) is

   begin

      Obj.Early_Run_Body;

   end Early_Run;

   ----------------------------------------------------------------------------

   overriding
   function Routine (Obj : Early_Test_Case;
                     K   : Test_Routine_Index) return Test_Routine
     is (R'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Early_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind        := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      procedure Free_Exception is new Ada.Unchecked_Deallocation
        (Object => Exception_Occurrence,
         Name   => Exception_Occurrence_Access);

      -----------------------------------------------------

      function Cond return Boolean
        is (Obj.Early_Run_Done_Flag);

      -----------------------------------------------------

   begin

      Data_Store.Set (Early_Test_Case'Class (Obj)'Tag, Obj.E);
      Run_Body (Obj, Outcome, Kind, Cond'Access);
      Data_Store.Reset;

      case Kind is
         when Check_Cond               => null;
         when Assert_Cond_And_Run_Test => Obj.Early_Run_Done_Flag := False;
                                          Free_Exception (Obj.E);
      end case;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Abstract_Early_Test_Case;
