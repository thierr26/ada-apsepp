-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar,
     Apsepp.Test_Event_Class.Impl.Abbrev;

package body Apsepp.Test_Reporter_Class.Struct_Builder is

   use Ada.Calendar,
       Apsepp.Test_Event_Class.Impl.Abbrev;

   ----------------------------------------------------------------------------

   protected body Test_Reporter_Struct_Builder is

      -----------------------------------------------------

      function Is_Conflicting_Node_Tag (Node_Tag : Tag) return Boolean
        is (Data.Is_Active (Node_Tag));

      -----------------------------------------------------

      procedure Provide_Node_Lineage (Node_Lineage : Tag_Array) is

      begin

         Data.Include_Node (Node_Lineage);

      end Provide_Node_Lineage;

      -----------------------------------------------------

      procedure Report_Failed_Child_Test_Node_Access
        (Node_Tag           : Tag;
         Previous_Child_Tag : Tag;
         Error              : Exception_Occurrence) is

         Ev : Ev_FCTNA_Ti;

      begin

         Ev.Set ((Error              => Save_Occurrence (Error),
                  Date               => Clock,
                  Previous_Child_Tag => Previous_Child_Tag,
                  others             => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Child_Test_Node_Access;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Cond_Check_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence) is

         Ev : Ev_UNCCE_Ti;

      begin

         Ev.Set ((Error  => Save_Occurrence (Error),
                  Date   => Clock,
                  others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Unexpected_Node_Cond_Check_Error;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Run_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence) is

         Ev : Ev_UNRE_Ti;

      begin

         Ev.Set ((Error  => Save_Occurrence (Error),
                  Date   => Clock,
                  others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Unexpected_Node_Run_Error;

      -----------------------------------------------------

      procedure Report_Node_Cond_Check_Start (Node_Tag : Tag) is

         Ev : Ev_NCCS;

      begin

         Ev.Set ((Date   => Clock,
                  others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Node_Cond_Check_Start;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Check (Node_Tag : Tag) is

         Ev : Ev_PNCC_Ti;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Passed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Check (Node_Tag : Tag) is

         Ev : Ev_FNCC_Ti;

      begin

         Ev.Set ((Date   => Clock,
                  others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Assert (Node_Tag : Tag) is

         Ev : Ev_PNCA;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Passed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Assert (Node_Tag : Tag) is

         Ev : Ev_FNCA_Ti;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Node_Run_Start (Node_Tag : Tag) is

         Ev : Ev_NRS_Ti;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Node_Run_Start;

      -----------------------------------------------------

      procedure Report_Test_Routine_Start
        (Node_Tag : Tag;
         K        : Test_Routine_Index) is

         Ev : Ev_TRS;

      begin

         Ev.Set ((Routine_Index => K,
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Test_Routine_Start;

      -----------------------------------------------------

      procedure Report_Test_Routines_Cancellation
        (Node_Tag        : Tag;
         First_K, Last_K : Test_Routine_Index) is

         Ev : Ev_TRC;

         pragma Unreferenced (First_K);

      begin

         Ev.Set ((Routine_Index => Last_K,
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Test_Routines_Cancellation;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Access
        (Node_Tag : Tag;
         K        : Test_Routine_Index;
         Error    : Exception_Occurrence) is

         Ev : Ev_FTRA_Ti;

      begin

         Ev.Set ((Routine_Index => K,
                  Error         => Save_Occurrence (Error),
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Test_Routine_Access;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Setup
        (Node_Tag : Tag;
         K        : Test_Routine_Index;
         Error    : Exception_Occurrence) is

         Ev : Ev_FTRS_Ti;

      begin

         Ev.Set ((Routine_Index => K,
                  Error         => Save_Occurrence (Error),
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Test_Routine_Setup;

      -----------------------------------------------------

      procedure Report_Passed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index) is

         Ev   : Ev_PTA;
         Ev_A : Ev_PTA_A;

      begin

         if Assert_Num_Avail then
            Ev_A.Set ((Routine_Index => K,
                       Assert_Num    => Assert_Num,
                       others        => <>));
            Data.Add_Event (Node_Tag, Ev_A);
         else
            Ev.Set ((Routine_Index => K,
                     others        => <>));
            Data.Add_Event (Node_Tag, Ev);
         end if;

      end Report_Passed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index;
         Error            : Exception_Occurrence) is

         Ev   : Ev_FTA;
         Ev_A : Ev_FTA_A;

      begin

         if Assert_Num_Avail then
            Ev_A.Set ((Routine_Index => K,
                       Assert_Num    => Assert_Num,
                       Error         => Save_Occurrence (Error),
                       others        => <>));
            Data.Add_Event (Node_Tag, Ev_A);
         else
            Ev.Set ((Routine_Index => K,
                     Error         => Save_Occurrence (Error),
                     others        => <>));
            Data.Add_Event (Node_Tag, Ev);
         end if;

      end Report_Failed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Unexpected_Routine_Exception
        (Node_Tag : Tag;
         K        : Test_Routine_Index;
         Error    : Exception_Occurrence) is

         Ev : Ev_URE_Ti;

      begin

         Ev.Set ((Routine_Index => K,
                  Error         => Save_Occurrence (Error),
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Unexpected_Routine_Exception;

      -----------------------------------------------------

      procedure Report_Passed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Routine_Index) is

         Ev : Ev_PTR;

      begin

         Ev.Set ((Routine_Index => K,
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Passed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Routine_Index) is

         Ev : Ev_FTR;

      begin

         Ev.Set ((Routine_Index => K,
                  others        => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Passed_Node_Run (Node_Tag : Tag) is

         Ev : Ev_PNR_Ti;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Passed_Node_Run;

      -----------------------------------------------------

      procedure Report_Failed_Node_Run (Node_Tag : Tag) is

         Ev : Ev_FNR_Ti;

      begin

         Ev.Set ((others => <>));

         Data.Add_Event (Node_Tag, Ev);

      end Report_Failed_Node_Run;

      -----------------------------------------------------

      procedure Render is

      begin

         if Rendering_Procedure /= null then
            Rendering_Procedure (Data'Access);
         end if;

         Data.Reset;

      end Render;

      -----------------------------------------------------

      function Data_Access return not null access constant Test_Reporter_Data
        is (Data'Unchecked_Access);

      -----------------------------------------------------

   end Test_Reporter_Struct_Builder;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Struct_Builder;
