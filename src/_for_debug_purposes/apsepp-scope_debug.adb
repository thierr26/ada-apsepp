-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace;

package body Apsepp.Scope_Debug is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Obj.Entry_Trace then

         Debug_Trace.Debug_Trace.Trace
           ("Entry",
            Controlled_Debug_Tracer'Class (Obj).Entity_Name);

      end if;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Obj.Exit_Trace then

         Debug_Trace.Debug_Trace.Trace
           ("Exit",
            Controlled_Debug_Tracer'Class (Obj).Entity_Name);

      end if;

   end Finalize;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer;
                    Item : String) is

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item,
         Controlled_Debug_Tracer'Class (Obj).Entity_Name);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer;
                      Error : Exception_Occurrence) is

   begin

      Debug_Trace.Debug_Trace.Trace_E
        (Error,
         Controlled_Debug_Tracer'Class (Obj).Entity_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer;
      Reset_Elapsed : Boolean                 := False) is

   begin

      Debug_Trace.Debug_Trace.Trace_Time
        (Controlled_Debug_Tracer'Class (Obj).Entity_Name,
         Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_A;
                    Item : String) is

   begin

      Obj.C_D_T.Trace (Item);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_A;
                      Error : Exception_Occurrence) is

   begin

      Obj.C_D_T.Trace_E (Error);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_A;
      Reset_Elapsed : Boolean                   := False) is

   begin

      Obj.C_D_T.Trace_Time (Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_I;
                    Item : String) is

   begin

      Obj.C_D_T.Trace (Item);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_I;
                      Error : Exception_Occurrence) is

   begin

      Obj.C_D_T.Trace_E (Error);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_I;
      Reset_Elapsed : Boolean                   := False) is

   begin

      Obj.C_D_T.Trace_Time (Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_F;
                    Item : String) is

   begin

      Obj.C_D_T.Trace (Item);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_F;
                      Error : Exception_Occurrence) is

   begin

      Obj.C_D_T.Trace_E (Error);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_F;
      Reset_Elapsed : Boolean                   := False) is

   begin

      Obj.C_D_T.Trace_Time (Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_N;
                    Item : String) is

   begin

      Obj.C_D_T.Trace (Item);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_N;
                      Error : Exception_Occurrence) is

   begin

      Obj.C_D_T.Trace_E (Error);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_N;
      Reset_Elapsed : Boolean                   := False) is

   begin

      Obj.C_D_T.Trace_Time (Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Debug;
