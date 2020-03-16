-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Characters.Handling,
     Apsepp.Debug_Trace,
     Apsepp.Test_Event_Class;

package body Apsepp.Test_Reporter_Class.W_Node_Barrier is

   ----------------------------------------------------------------------------

   function Test_Reporter_Proc_Name
     (Event_Kind : Test_Event_Kind) return String is

      Event_Kind_Str : String := Test_Event_Kind'Image (Event_Kind);

   begin

      for K in Event_Kind_Str'First + 1 .. Event_Kind_Str'Last loop
         if Event_Kind_Str(K - 1) /= '_' then
            Event_Kind_Str(K)
              := Ada.Characters.Handling.To_Lower (Event_Kind_Str(K));
         end if;
      end loop;

      return "Report_" & Event_Kind_Str;

   end Test_Reporter_Proc_Name;

   ----------------------------------------------------------------------------

   not overriding
   procedure Set_Up
     (Obj                      : in out Test_Reporter_W_Node_Barrier;
      Barrier_Access           :        not null access Test_Node_Barrier;
      Char_Name_Image_Function :        Char_Name_Image_Func;
      Tag_To_Char_Function     :        Tag_To_Char_Func) is

   begin

      Obj.Barrier         := Barrier_Access;
      Obj.Char_Name_Image := Char_Name_Image_Function;
      Obj.Tag_To_Char     := Tag_To_Char_Function;

   end Set_Up;

   ----------------------------------------------------------------------------

   not overriding
   function Arriv_To_Cross_Message
     (Obj            : Test_Reporter_W_Node_Barrier;
      Operation_Name : String;
      Node_Tag       : Tag) return String
     is ("Apsepp_Test_Node_Barrier."
         & Operation_Name
         & Obj.Char_Name_Image (Obj.Tag_To_Char (Node_Tag))
         & "Arriving to test node barrier");

   ----------------------------------------------------------------------------

   -- TODO: Use a generic procedure for the 'Report_' bodies. <2020-03-08>
   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_W_Node_Barrier;
      Node_Tag           :        Tag;
      Previous_Child_Tag :        Tag;
      Error              :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Child_Test_Node_Access;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error              => Save_Occurrence (Error),
            Previous_Child_Tag => Previous_Child_Tag,
            others             => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Child_Test_Node_Access;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind
        := Unexpected_Node_Cond_Check_Error;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error  => Save_Occurrence (Error),
            others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Unexpected_Node_Cond_Check_Error;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Unexpected_Node_Run_Error;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error  => Save_Occurrence (Error),
            others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Unexpected_Node_Run_Error;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Node_Cond_Check_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Node_Cond_Check_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Passed_Node_Cond_Check;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Passed_Node_Cond_Check;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Node_Cond_Check;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Node_Cond_Check;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Passed_Node_Cond_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Passed_Node_Cond_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Node_Cond_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Node_Cond_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Node_Run_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Node_Run_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Test_Routine_Start
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Test_Routine_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Test_Routine_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj                 : in out Test_Reporter_W_Node_Barrier;
      Node_Tag            :        Tag;
      First_Routine_Index,
      Last_Routine_Index  :        Test_Routine_Index) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Test_Routines_Cancellation;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Routine_Index      => First_Routine_Index,
            Last_Routine_Index => Last_Routine_Index,
            others             => <>);

      pragma Unreferenced (First_Routine_Index);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Test_Routines_Cancellation;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine_Access;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error         => Save_Occurrence (Error),
            Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Test_Routine_Access;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine_Setup;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error         => Save_Occurrence (Error),
            Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Test_Routine_Setup;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_W_Node_Barrier;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Routine_Index;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Assert_Index) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Passed_Test_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (if Assert_Num_Avail then
               (Routine_Index => Routine_Index,
                Assert_Num    => Assert_Num,
                others        => <>)
            else
               (Routine_Index => Routine_Index,
                others        => <>));

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Passed_Test_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_W_Node_Barrier;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Routine_Index;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Assert_Index;
      Error            :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Test_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (if Assert_Num_Avail then
               (Error         => Save_Occurrence (Error),
                Routine_Index => Routine_Index,
                Assert_Num    => Assert_Num,
                others        => <>)
            else
               (Error         => Save_Occurrence (Error),
                Routine_Index => Routine_Index,
                others        => <>));

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Test_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Unexpected_Routine_Exception;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Error         => Save_Occurrence (Error),
            Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Unexpected_Routine_Exception;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Test_Routine
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Passed_Test_Routine;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Passed_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (Routine_Index => Routine_Index,
            others        => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Passed_Node_Run;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Passed_Node_Run;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag) is

      Dispatching_Obj : constant access Test_Reporter_W_Node_Barrier'Class
        := Obj'Access;

      Event_Kind : constant Test_Event_Kind := Failed_Node_Run;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      Event_Data : Apsepp.Test_Event_Class.Test_Event_Data
        := (others => <>);

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item => Dispatching_Obj.Arriv_To_Cross_Message
                   (Proc_Name,
                    Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind,
                                                      Event_Data);

   end Report_Failed_Node_Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.W_Node_Barrier;
