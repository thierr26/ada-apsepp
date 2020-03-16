-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Event_Class.Generic_Routine_Index_Mixin,
     Apsepp.Test_Event_Class.Generic_Assert_Num_Mixin,
     Apsepp.Test_Event_Class.Generic_Exception_Mixin,
     Apsepp.Test_Event_Class.Generic_Timestamp_Mixin;

package Apsepp.Test_Event_Class.Impl is

   ----------------------------------------------------------------------------

   package Derivation is

      -----------------------------------------------------

      type Test_Event_FCTNA is new Test_Event with private;

      overriding
      procedure Set (Obj  : in out Test_Event_FCTNA;
                     Data :        Test_Event_Data);

      overriding
      function Has_Previous_Child_Tag (Obj : Test_Event_FCTNA) return Boolean
        is (True);

      overriding
      function Previous_Child_Tag (Obj : Test_Event_FCTNA) return Tag;

      -----------------------------------------------------

      type Test_Event_NCCS is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_PNCC is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_FNCC is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_PNCA is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_FNCA is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_NRS is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_TRS is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_TRC is new Test_Event with private;

      overriding
      procedure Set (Obj  : in out Test_Event_TRC;
                     Data :        Test_Event_Data);

      overriding
      function Has_Last_Cancelled_Routine_Index
        (Obj : Test_Event_TRC) return Boolean
        is (True);

      overriding
      function Last_Cancelled_Routine_Index
        (Obj : Test_Event_TRC) return Test_Routine_Index;

      -----------------------------------------------------

      type Test_Event_FTRA is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_FTRS is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_PTA is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_FTA is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_URE is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_PTR is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_FTR is new Test_Event with private;

      -----------------------------------------------------

      type Test_Event_PNR is new Test_Event_Final with private;

      -----------------------------------------------------

      type Test_Event_FNR is new Test_Event_Final with private;

      -----------------------------------------------------

   private

      type Test_Event_FCTNA is new Test_Event with record
         Previous_Child_Tag : Tag;
      end record;

      type Test_Event_NCCS is new Test_Event with null record;

      type Test_Event_PNCC is new Test_Event with null record;

      type Test_Event_FNCC is new Test_Event with null record;

      type Test_Event_PNCA is new Test_Event with null record;

      type Test_Event_FNCA is new Test_Event with null record;

      type Test_Event_NRS is new Test_Event with null record;

      type Test_Event_TRS is new Test_Event with null record;

      type Test_Event_TRC is new Test_Event with record
         Last_Cancelled_Routine_Index : Test_Routine_Index;
      end record;

      type Test_Event_FTRA is new Test_Event with null record;

      type Test_Event_FTRS is new Test_Event with null record;

      type Test_Event_PTA is new Test_Event with null record;

      type Test_Event_FTA is new Test_Event with null record;

      type Test_Event_URE is new Test_Event with null record;

      type Test_Event_PTR is new Test_Event with null record;

      type Test_Event_FTR is new Test_Event with null record;

      type Test_Event_PNR is new Test_Event_Final with null record;

      type Test_Event_FNR is new Test_Event_Final with null record;

   end Derivation;

   ----------------------------------------------------------------------------

   package Routine_Index_Mixin is

      package TRS_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_TRS);

      package FTRA_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_FTRA);

      package FTRS_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_FTRS);

      package PTA_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_PTA);

      package FTA_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_FTA);

      package URE_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_URE);

      package PTR_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_PTR);

      package FTR_R_Mixin
        is new Generic_Routine_Index_Mixin (Derivation.Test_Event_FTR);

   end Routine_Index_Mixin;

   ----------------------------------------------------------------------------

   package Assert_Num_Mixin is

      package PTA_R_A_Mixin
        is new Generic_Assert_Num_Mixin
        (Routine_Index_Mixin.PTA_R_Mixin.Child_W_Routine_Index);

      package FTA_R_A_Mixin
        is new Generic_Assert_Num_Mixin
        (Routine_Index_Mixin.FTA_R_Mixin.Child_W_Routine_Index);

   end Assert_Num_Mixin;

   ----------------------------------------------------------------------------

   package Exception_Mixin is

      package FCTNA_Ex_Mixin
        is new Generic_Exception_Mixin (Derivation.Test_Event_FCTNA);

      package UNCCE_Ex_Mixin
        is new Generic_Exception_Mixin (Test_Event);

      package UNRE_Ex_Mixin
        is new Generic_Exception_Mixin (Test_Event);

      package FTRA_R_Ex_Mixin
        is new Generic_Exception_Mixin
        (Routine_Index_Mixin.FTRA_R_Mixin.Child_W_Routine_Index);

      package FTRS_R_Ex_Mixin
        is new Generic_Exception_Mixin
        (Routine_Index_Mixin.FTRS_R_Mixin.Child_W_Routine_Index);

      package FTA_R_Ex_Mixin
        is new Generic_Exception_Mixin
        (Routine_Index_Mixin.FTA_R_Mixin.Child_W_Routine_Index);

      package FTA_R_A_Ex_Mixin
        is new Generic_Exception_Mixin
        (Assert_Num_Mixin.FTA_R_A_Mixin.Child_W_Assert_Num);

      package URE_R_Ex_Mixin
        is new Generic_Exception_Mixin
        (Routine_Index_Mixin.URE_R_Mixin.Child_W_Routine_Index);

   end Exception_Mixin;

   ----------------------------------------------------------------------------

   package Timestamp_Mixin is

      package FCTNA_Ex_Ti_Mixin is new Generic_Timestamp_Mixin
        (Exception_Mixin.FCTNA_Ex_Mixin.Child_W_Exception);

      package UNCCE_Ex_Ti_Mixin is new Generic_Timestamp_Mixin
        (Exception_Mixin.UNCCE_Ex_Mixin.Child_W_Exception);

      package UNRE_Ex_Ti_Mixin is new Generic_Timestamp_Mixin
        (Exception_Mixin.UNRE_Ex_Mixin.Child_W_Exception);

      package NCCS_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_NCCS);

      package PNCC_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_PNCC);

      package FNCC_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_FNCC);

      package PNCA_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_PNCA);

      package FNCA_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_FNCA);

      package NRS_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_NRS);

      package TRS_R_Ti_Mixin is new Generic_Timestamp_Mixin
        (Routine_Index_Mixin.TRS_R_Mixin.Child_W_Routine_Index);

      package TRC_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_TRC);

      package FTRA_R_Ex_Ti_Mixin
        is new Generic_Timestamp_Mixin
        (Exception_Mixin.FTRA_R_Ex_Mixin.Child_W_Exception);

      package FTRS_R_Ex_Ti_Mixin
        is new Generic_Timestamp_Mixin
        (Exception_Mixin.FTRS_R_Ex_Mixin.Child_W_Exception);

      package PTA_R_Ti_Mixin is new Generic_Timestamp_Mixin
        (Routine_Index_Mixin.PTA_R_Mixin.Child_W_Routine_Index);

      package PTA_R_A_Ti_Mixin is new Generic_Timestamp_Mixin
        (Assert_Num_Mixin.PTA_R_A_Mixin.Child_W_Assert_Num);

      package FTA_R_Ex_Ti_Mixin is new Generic_Timestamp_Mixin
        (Exception_Mixin.FTA_R_Ex_Mixin.Child_W_Exception);

      package FTA_R_A_Ex_Ti_Mixin is new Generic_Timestamp_Mixin
        (Exception_Mixin.FTA_R_A_Ex_Mixin.Child_W_Exception);

      package URE_R_Ex_Ti_Mixin
        is new Generic_Timestamp_Mixin
        (Exception_Mixin.URE_R_Ex_Mixin.Child_W_Exception);

      package PTR_R_Ti_Mixin is new Generic_Timestamp_Mixin
        (Routine_Index_Mixin.PTR_R_Mixin.Child_W_Routine_Index);

      package FTR_R_Ti_Mixin is new Generic_Timestamp_Mixin
        (Routine_Index_Mixin.FTR_R_Mixin.Child_W_Routine_Index);

      package PNR_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_PNR);

      package FNR_Ti_Mixin
        is new Generic_Timestamp_Mixin (Derivation.Test_Event_FNR);

   end Timestamp_Mixin;

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Impl;
