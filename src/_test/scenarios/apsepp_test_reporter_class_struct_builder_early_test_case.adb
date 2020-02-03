-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags,
     Ada.Characters.Handling,
     Ada.Assertions,
     Apsepp_Testing_System_Test_Fixture,
     Apsepp.Generic_Fixture.Creator,
     Apsepp.Tags,
     Apsepp_Test_Node_Barrier,
     Apsepp_Test_Node_Class_Early_Test_Case,
     Apsepp.Test_Reporter_Class.Struct_Builder,
     Apsepp.Test_Event_Class,
     Apsepp.Test_Node_Class,
     Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing,
     Apsepp.Scope_Debug;

package body Apsepp_Test_Reporter_Class_Struct_Builder_Early_Test_Case is

   use Ada.Tags,
       Ada.Characters.Handling,
       Apsepp_Testing_System_Test_Fixture,
       Apsepp_Test_Node_Barrier,
       Apsepp.Test_Reporter_Class.Struct_Builder,
       Apsepp.Test_Event_Class,
       Apsepp.Test_Node_Class,
       Apsepp.Test_Reporter_Data_Struct_Class.Impl,
       Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing;

   ----------------------------------------------------------------------------

   function TSF return Testing_System_Test_Fixture_Access
     renames Instance;

   ----------------------------------------------------------------------------

   TRSB : Test_Reporter_Struct_Builder;

   ----------------------------------------------------------------------------

   procedure Validate (K           : Positive;
                       Event_Kind  : Test_Event_Kind;
                       Event_Data  : Test_Event_Data;
                       Char        : ISO_646;
                       Char_To_Tag : Char_To_Tag_Func;
                       Msg_Pref    : String) is

      -----------------------------------------------------

      procedure Put_Array (A : Node_Tag_Tree_As_Array; Msg_Pref : String) is

         use Apsepp.Scope_Debug;

         Entity_Name : constant String
           := "Apsepp_Test_Reporter_Class_Struct_Builder_Early_Test_Case"
              & ".Put_Array";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

         Line : constant String (1 .. 53) := (others => '-');

         First_Done : Boolean := False;

      begin

         for E of A loop

            if not First_Done then
               First_Done := True;
               C_D_T.Trace (Msg_Pref & Line);
            end if;

            declare

               Active_String : constant String := (if E.Active then
                                                      "x"
                                                   else
                                                      " ");

               Parent_Index_Image : constant String
                 := Natural'Image (E.Parent_Index);

               N   : constant Positive := 3;
               Pad : constant String (1 .. N - Parent_Index_Image'Length)
                 := (others => ' ');
               Parent_Index_String : constant String (1 .. 3)
                 := Pad & Parent_Index_Image;

            begin

               C_D_T.Trace (Active_String
                              &
                            Parent_Index_String
                              &
                            " "
                              &
                            Expanded_Name (E.Node_Tag));

            end;

         end loop;

         if First_Done then
            C_D_T.Trace (Msg_Pref & Line);
         end if;

      end Put_Array;

      -----------------------------------------------------

      function Node_Data_Tree_Change_OK
        (Node_Tag_Tree_Old   , Node_Tag_Tree_New   : Node_Tag_Tree_As_Array;
         New_Node_Tag        , New_Node_Parent_Tag : Tag := No_Tag;
         Deactivated_Node_Tag                      : Tag := No_Tag)
        return Boolean is

         Old_Length : constant Natural := Node_Tag_Tree_Old'Length;
         New_Length : constant Natural := Node_Tag_Tree_New'Length;

         K_Added           : Natural := 0;
         Deactivated_Found : Boolean := False;

         function Offs return Natural
           is (if K_Added = 0 then
                  0
               else
                  1);

         function O_Ta (K : Natural) return Tag
           is (Tree_Tag (Node_Tag_Tree_Old, K));

         function N_Ta (K : Natural) return Tag
           is (Tree_Tag (Node_Tag_Tree_New, K));

         function O_P_Ta (K : Natural) return Tag
           is (Tree_Parent_Tag (Node_Tag_Tree_Old, K));

         function N_P_Ta (K : Natural) return Tag
           is (Tree_Parent_Tag (Node_Tag_Tree_New, K));

         function O_Ac (K : Positive) return Boolean
           is (Tree_Active (Node_Tag_Tree_Old, K));

         function N_Ac (K : Positive) return Boolean
           is (Tree_Active (Node_Tag_Tree_New, K));

         Ret : Boolean := New_Length = Old_Length
                            or else
                          New_Length = Old_Length + 1;

      begin

         if not Ret then return Ret; end if;

         for K in Node_Tag_Tree_New'Range loop

            if K <= Node_Tag_Tree_Old'Last then

               if N_Ta (K) /= O_Ta (K - Offs) then
                  Ret     := K_Added = 0 and then N_Ac (K);
                  K_Added := K;
               else
                  Ret := N_P_Ta (K) = O_P_Ta (K - Offs);
                  if N_Ac (K) xor O_Ac (K - Offs) then
                     Ret := Ret and then O_Ac (K - Offs)
                                           and then
                                         not Deactivated_Found
                                           and then
                                         N_Ta (K) = Deactivated_Node_Tag;
                     Deactivated_Found := True;
                  end if;
               end if;

            else

               if K_Added = 0 then
                  K_Added := K;
               else
                  Ret := N_Ta (K) = O_Ta (K - Offs)
                           and then
                         N_P_Ta (K) = O_P_Ta (K - Offs);
               end if;

            end if;

            exit when not Ret;
         end loop;

         if not Ret then return Ret; end if;

         Ret := Deactivated_Node_Tag = No_Tag or else Deactivated_Found;

         if not Ret then return Ret; end if;

         if K_Added = 0 then

            Ret := New_Node_Tag = No_Tag and then New_Node_Parent_Tag = No_Tag;

         else

            Ret := N_Ta (K_Added) = New_Node_Tag
                     and then
                   N_P_Ta (K_Added) = New_Node_Parent_Tag;

         end if;

         return Ret;

      end Node_Data_Tree_Change_OK;

      -----------------------------------------------------

      function Event_Data_As_Expected
        (Data       : Flattened_Event_Data;
         Node_Tag   : Tag;
         Has_E      : Boolean            := False;
         R_Index    : Test_Routine_Count := 0;
         Assert_Num : Test_Assert_Count  := 0) return Boolean
        is (Data.Node_Tag           = Node_Tag
              and then
            Data.Has_E              = Has_E
              and then
            Data.R_Index            = R_Index
              and then
            Data.Assert_Num         = Assert_Num
              and then
            Data.Previous_Child_Tag = No_Tag);

      -----------------------------------------------------

      Is_Runner : constant Boolean := Is_Lower (Char);
      Temp_Tag  : constant Tag     := Char_To_Tag (To_Upper (Char));
      Node_Tag  : constant Tag     := (if Is_Runner then
                                          (if    Temp_Tag = TSF.A then TSF.A_R
                                           elsif Temp_Tag = TSF.B then TSF.B_R
                                           elsif Temp_Tag = TSF.C then TSF.C_R
                                           elsif Temp_Tag = TSF.D then TSF.D_R
                                           else                        TSF.E_R)
                                       else
                                          Temp_Tag);

      Data_Bef : constant access constant Test_Reporter_Data
        := TRSB.Struct_Pointer;
      Data_Aft :          access constant Test_Reporter_Data;

      Parent_Node_Tag : constant Tag := (if Node_Tag = TSF.A then
                                            TSF.A_R
                                         elsif Node_Tag = TSF.B then
                                            TSF.B_R
                                         elsif Node_Tag = TSF.C then
                                            TSF.C_R
                                         elsif Node_Tag = TSF.D then
                                            TSF.C_R -- Intentional.
                                         elsif Node_Tag = TSF.E then
                                            TSF.E_R
                                         else
                                            No_Tag);

      T_B_L   : constant Natural := Node_Tag_Tree_Node_Count (Data_Bef.all);
      T_B     :          Node_Tag_Tree_As_Array (1 .. T_B_L);

      Ev_B_L  : constant Natural := Event_Vector_Length (Data_Bef.all);
      Ev_B    :          Event_Data_Array (1 .. Ev_B_L);

   begin

      To_Arrays (Data_Bef.all, T_B, Ev_B);

      if Parent_Node_Tag /= No_Tag then
         TRSB.Provide_Node_Lineage ((Parent_Node_Tag, Node_Tag));
      end if;

      Ada.Assertions.Assert
        (K > 1 or else Data_Bef.Is_Empty,
         Msg_Pref & "Non-empty data structure before first event");

      case Event_Kind is

         when Failed_Child_Test_Node_Access =>
            TRSB.Report_Failed_Child_Test_Node_Access
              (Node_Tag,
               Event_Data.Previous_Child_Tag,
               Event_Data.E.all);
         when Unexpected_Node_Cond_Check_Error =>
            TRSB.Report_Unexpected_Node_Cond_Check_Error (Node_Tag,
                                                          Event_Data.E.all);
         when Unexpected_Node_Run_Error =>
            TRSB.Report_Unexpected_Node_Run_Error (Node_Tag, Event_Data.E.all);
         when Node_Cond_Check_Start =>
            TRSB.Report_Node_Cond_Check_Start (Node_Tag);
         when Passed_Node_Cond_Check =>
            TRSB.Report_Passed_Node_Cond_Check (Node_Tag);
         when Failed_Node_Cond_Check =>
            TRSB.Report_Failed_Node_Cond_Check (Node_Tag);
         when Passed_Node_Cond_Assert =>
            TRSB.Report_Passed_Node_Cond_Assert (Node_Tag);
         when Failed_Node_Cond_Assert =>
            TRSB.Report_Failed_Node_Cond_Assert (Node_Tag);
         when Node_Run_Start =>
            TRSB.Report_Node_Run_Start (Node_Tag);
         when Test_Routine_Start =>
            TRSB.Report_Test_Routine_Start (Node_Tag, Event_Data.R_Index);
         when Test_Routines_Cancellation =>
            TRSB.Report_Test_Routines_Cancellation
              (Node_Tag,
               Latest_R_Index (Data_Bef.all, Node_Tag) + 1,
               Event_Data.R_Index);
         when Failed_Test_Routine_Access =>
            TRSB.Report_Failed_Test_Routine_Access (Node_Tag,
                                                    Event_Data.R_Index,
                                                    Event_Data.E.all);
         when Failed_Test_Routine_Setup =>
            TRSB.Report_Failed_Test_Routine_Setup (Node_Tag,
                                                   Event_Data.R_Index,
                                                   Event_Data.E.all);
         when Passed_Test_Assert =>
            TRSB.Report_Passed_Test_Assert (Node_Tag,
                                            Event_Data.R_Index,
                                            Event_Data.Assert_Num /= 0,
                                            Event_Data.Assert_Num);
         when Failed_Test_Assert =>
            TRSB.Report_Failed_Test_Assert (Node_Tag,
                                            Event_Data.R_Index,
                                            Event_Data.Assert_Num /= 0,
                                            Event_Data.Assert_Num,
                                            Event_Data.E.all);
         when Unexpected_Routine_Exception =>
            TRSB.Report_Unexpected_Routine_Exception (Node_Tag,
                                                      Event_Data.R_Index,
                                                      Event_Data.E.all);
         when Passed_Test_Routine =>
            TRSB.Report_Passed_Test_Routine (Node_Tag, Event_Data.R_Index);
         when Failed_Test_Routine =>
            TRSB.Report_Failed_Test_Routine (Node_Tag, Event_Data.R_Index);
         when Passed_Node_Run =>
            TRSB.Report_Passed_Node_Run (Node_Tag);
         when Failed_Node_Run =>
            TRSB.Report_Failed_Node_Run (Node_Tag);

      end case;

      Data_Aft := TRSB.Struct_Pointer;

      Ada.Assertions.Assert
        (not Data_Aft.Is_Empty,
         Msg_Pref & "Empty data structure after first event");
      Ada.Assertions.Assert
        (Event_Vector_Length (Data_Aft.all) = K,
         Msg_Pref & "K is" & Positive'Image (K) & " but "
         & "Data_Aft.Event_Vector_Length is"
         & Positive'Image (Event_Vector_Length (Data_Aft.all)));

      declare

         T_A_L   : constant Natural := Node_Tag_Tree_Node_Count (Data_Aft.all);
         T_A     :          Node_Tag_Tree_As_Array (1 .. T_A_L);

         Ev_A_L  : constant Natural := Event_Vector_Length (Data_Aft.all);
         Ev_A    :          Event_Data_Array (1 .. Ev_A_L);

         Ev_A_1  :          Event_Data_Array (1 .. Ev_B_L);

         C_R_I   : Test_Routine_Count;
         C_A_N   : Test_Assert_Count;

         N_D_T_C_OK, E_D_A_E : Boolean;

      begin

         To_Arrays (Data_Aft.all, T_A, Ev_A);
         Ev_A_1 := Ev_A (1 .. Ev_B_L);

         Ada.Assertions.Assert (Ev_A_1 = Ev_B, "Corrupted test event vector");

         Put_Array (T_A, Msg_Pref);

         C_R_I := Current_R_Index (Ev_A, Ev_A'Last);
         C_A_N := Current_Assert_Num (Ev_A, Ev_A'Last);

         case Event_Kind is

            when Failed_Child_Test_Node_Access =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B,
                                                       T_A,
                                                       Node_Tag,
                                                       Parent_Node_Tag);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag,
                                                     Has_E => True);
            when Unexpected_Node_Cond_Check_Error =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B,
                                                       T_A,
                                                       Node_Tag,
                                                       Parent_Node_Tag);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag,
                                                     Has_E => True);
            when Unexpected_Node_Run_Error =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag,
                                                     Has_E => True);
            when Node_Cond_Check_Start =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B,
                                                       T_A,
                                                       Node_Tag,
                                                       Parent_Node_Tag);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag);
            when Passed_Node_Cond_Check =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag);
            when Failed_Node_Cond_Check =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag,
                                                     Has_E => True);
            when Passed_Node_Cond_Assert =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E    := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                     Node_Tag);
            when Failed_Node_Cond_Assert =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  Has_E      => True);
            when Node_Run_Start =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag);
            when Test_Routine_Start =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  R_Index => C_R_I + 1);
            when Test_Routines_Cancellation =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag);
            when Failed_Test_Routine_Access =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  Has_E   => True,
                                                  R_Index => C_R_I);
            when Failed_Test_Routine_Setup =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  Has_E   => True,
                                                  R_Index => C_R_I);
            when Passed_Test_Assert =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  R_Index    => C_R_I,
                                                  Assert_Num => C_A_N + 1);
            when Failed_Test_Assert =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  Has_E      => True,
                                                  R_Index    => C_R_I,
                                                  Assert_Num => C_A_N + 1);
            when Unexpected_Routine_Exception =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  Has_E   => True,
                                                  R_Index => C_R_I);
            when Passed_Test_Routine =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  R_Index => C_R_I);
            when Failed_Test_Routine =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK (T_B, T_A);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag,
                                                  R_Index => C_R_I);
            when Passed_Node_Run =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK
                 (T_B,
                  T_A,
                  Deactivated_Node_Tag => Node_Tag);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag);
            when Failed_Node_Run =>
               N_D_T_C_OK := Node_Data_Tree_Change_OK
                 (T_B,
                  T_A,
                  Deactivated_Node_Tag => Node_Tag);
               E_D_A_E := Event_Data_As_Expected (Ev_A(Ev_A'Last),
                                                  Node_Tag);

         end case;

         Ada.Assertions.Assert (N_D_T_C_OK, "Corrupted node data tree");
         Ada.Assertions.Assert (E_D_A_E, "Current event not as expected");

      end;

   end Validate;

   ----------------------------------------------------------------------------

   procedure Early_Test_TRCSBETC is

      use Apsepp_Test_Node_Class_Early_Test_Case;

      package Testing_System_T_F_Creator is new Testing_System_T_F.Creator;

      Expected_Tag : Apsepp.Tags.Tag_Array_Access;

   begin

      Ada.Assertions.Assert (Testing_System_T_F_Creator.Has_Actually_Created,
        "Test fixture already locked");

      Expected_Tag := new Tag_Array'(Routine_State_Array_To_Tag_Array
                                       (Expected_Routine_State_Array));

      TSF.Run_Test (Expected_Tag, Validate'Access);

      TRSB.Process;

      Apsepp.Tags.Free (Expected_Tag);

   exception

      when others => Apsepp.Tags.Free (Expected_Tag);
                     raise;

   end Early_Test_TRCSBETC;

   ----------------------------------------------------------------------------

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Reporter_Class_Struct_Builder_E_T_C)
     return Apsepp.Abstract_Early_Test_Case.Test_Routine
     is (Early_Test_TRCSBETC'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Reporter_Class_Struct_Builder_Early_Test_Case;
