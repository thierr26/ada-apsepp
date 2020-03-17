-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace;

package body Apsepp.Finalized_Debug_Tracer is

   ----------------------------------------------------------------------------

   overriding
   procedure Initialize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Entry_Trace_Required (Obj.Kind) then

         Debug_Trace.Debug_Trace.Trace
           (Item        => "Entry",
            Entity_Name => Controlled_Debug_Tracer'Class (Obj).Scope_Name);

      end if;

   end Initialize;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Exit_Trace_Required (Obj.Kind) then

         Debug_Trace.Debug_Trace.Trace
           (Item        => "Exit",
            Entity_Name => Controlled_Debug_Tracer'Class (Obj).Scope_Name);

      end if;

   end Finalize;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer;
                    Item : String) is

   begin

      Debug_Trace.Debug_Trace.Trace
        (Item        => Item,
         Entity_Name => Controlled_Debug_Tracer'Class (Obj).Scope_Name);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer;
                      Error : Exception_Occurrence) is

   begin

      Debug_Trace.Debug_Trace.Trace_E
        (Error       => Error,
         Entity_Name => Controlled_Debug_Tracer'Class (Obj).Scope_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer;
      Reset_Elapsed : Boolean                 := False) is

   begin

      Debug_Trace.Debug_Trace.Trace_Time
        (Entity_Name   => Controlled_Debug_Tracer'Class (Obj).Scope_Name,
         Reset_Elapsed => Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

end Apsepp.Finalized_Debug_Tracer;
