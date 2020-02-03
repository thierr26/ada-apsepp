-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace;

package body Apsepp.Scope_Debug is

   ----------------------------------------------------------------------------

   function C (Entity_Name : String;
               En, Ex      : Boolean) return Controlled_Debug_Tracer is

   begin

      if En then

         Debug_Trace.Debug_Trace.Trace ("Entry", Entity_Name);

      end if;

      return (Limited_Controlled
                with Entity_Name_Length  => Entity_Name'Length,
                     Entity_Name_Str     => Entity_Name,
                     Exit_Trace_Required => Ex);

   end C;

   ----------------------------------------------------------------------------

   not overriding
   function Create_A (Entity_Name : String) return Controlled_Debug_Tracer
     is (C (Entity_Name, True, True));

   ----------------------------------------------------------------------------

   not overriding
   function Create_I (Entity_Name : String) return Controlled_Debug_Tracer
     is (C (Entity_Name, True, False));

   ----------------------------------------------------------------------------

   not overriding
   function Create_F (Entity_Name : String) return Controlled_Debug_Tracer
     is (C (Entity_Name, False, True));

   ----------------------------------------------------------------------------

   not overriding
   function Create_N (Entity_Name : String) return Controlled_Debug_Tracer
     is (C (Entity_Name, False, False));

   ----------------------------------------------------------------------------

   not overriding
   function Entity (Obj : Controlled_Debug_Tracer) return String
     is (Obj.Entity_Name_Str);

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj : Controlled_Debug_Tracer; Message : String) is

   begin

      Debug_Trace.Debug_Trace.Trace
        (Message, Controlled_Debug_Tracer'Class (Obj).Entity);

   end Trace;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_E (Obj : Controlled_Debug_Tracer;
                      E   : Exception_Occurrence) is

   begin

      Debug_Trace.Debug_Trace.Trace_E
        (E, Controlled_Debug_Tracer'Class (Obj).Entity);

   end Trace_E;

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer;
      Reset_Elapsed : Boolean                 := False) is

   begin

      Debug_Trace.Debug_Trace.Trace_Time
        (Controlled_Debug_Tracer'Class (Obj).Entity, Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Obj.Exit_Trace_Required then

         Debug_Trace.Debug_Trace.Trace
           ("Exit", Controlled_Debug_Tracer'Class (Obj).Entity);

      end if;

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Scope_Debug;
