-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Event_Class.Impl.Abbrev is

   use Derivation,
       Routine_Index_Mixin,
       Assert_Num_Mixin,
       Exception_Mixin,
       Timestamp_Mixin;

   ----------------------------------------------------------------------------

   subtype Ev_FCTNA is FCTNA_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_FCTNA_Ti is FCTNA_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_UNCCE is UNCCE_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_UNCCE_Ti is UNCCE_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_UNRE is UNRE_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_UNRE_Ti is UNRE_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_NCCS is Test_Event_NCCS;

      -----------------------------------------------------

   subtype Ev_NCCS_Ti is NCCS_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_PNCC is Test_Event_PNCC;

      -----------------------------------------------------

   subtype Ev_PNCC_Ti is PNCC_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FNCC is Test_Event_FNCC;

      -----------------------------------------------------

   subtype Ev_FNCC_Ti is FNCC_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_PNCA is Test_Event_PNCA;

      -----------------------------------------------------

   subtype Ev_PNCA_Ti is PNCA_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FNCA is Test_Event_FNCA;

      -----------------------------------------------------

   subtype Ev_FNCA_Ti is FNCA_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_NRS is Test_Event_NRS;

      -----------------------------------------------------

   subtype Ev_NRS_Ti is NRS_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_TRS is TRS_R_Mixin.Child_W_Routine_Index;

      -----------------------------------------------------

   subtype Ev_TRS_Ti is TRS_R_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_TRC is TRC_R_Mixin.Child_W_Routine_Index;

      -----------------------------------------------------

   subtype Ev_TRC_Ti is TRC_R_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FTRA is FTRA_R_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_FTRA_Ti is FTRA_R_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FTRS is FTRS_R_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_FTRS_Ti is FTRS_R_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_PTA is PTA_R_Mixin.Child_W_Routine_Index;

      -----------------------------------------------------

   subtype Ev_PTA_Ti is PTA_R_Ti_Mixin.Child_W_Timestamp;

      -----------------------------------------------------

   subtype Ev_PTA_A is PTA_R_A_Mixin.Child_W_Assert_Num;

      -----------------------------------------------------

   subtype Ev_PTA_A_Ti is PTA_R_A_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FTA is FTA_R_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_FTA_Ti is FTA_R_Ex_Ti_Mixin.Child_W_Timestamp;

      -----------------------------------------------------

   subtype Ev_FTA_A is FTA_R_A_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_FTA_A_Ti is FTA_R_A_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_URE is URE_R_Ex_Mixin.Child_W_Exception;

      -----------------------------------------------------

   subtype Ev_URE_Ti is URE_R_Ex_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_PTR is PTR_R_Mixin.Child_W_Routine_Index;

      -----------------------------------------------------

   subtype Ev_PTR_Ti is PTR_R_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FTR is FTR_R_Mixin.Child_W_Routine_Index;

      -----------------------------------------------------

   subtype Ev_FTR_Ti is FTR_R_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_PNR is Test_Event_PNR;

      -----------------------------------------------------

   subtype Ev_PNR_Ti is PNR_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

   subtype Ev_FNR is Test_Event_FNR;

      -----------------------------------------------------

   subtype Ev_FNR_Ti is FNR_Ti_Mixin.Child_W_Timestamp;

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Impl.Abbrev;
