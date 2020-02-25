-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Finalization; use Ada.Finalization;

package Apsepp.Finalized_Debug_Tracer is

   type Controlled_Debug_Tracer_Kind is (A, I, F, N);

   function Entry_Trace_Required
     (X : Controlled_Debug_Tracer_Kind) return Boolean
     is (case X is
            when A | I => True,
            when F | N => False);

   function Exit_Trace_Required
     (X : Controlled_Debug_Tracer_Kind) return Boolean
     is (case X is
            when A | F => True,
            when I | N => False);

private

   type Controlled_Debug_Tracer
     (Entity_Name_Access : access constant String;
      Kind               : Controlled_Debug_Tracer_Kind)
     is limited new Limited_Controlled with null record;

   overriding
   procedure Initialize (Obj : in out Controlled_Debug_Tracer);

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer);

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer;
                    Item : String);

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer;
                      Error : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer;
      Reset_Elapsed : Boolean                 := False);

   not overriding
   function Entity_Name (Obj : Controlled_Debug_Tracer) return String
     is (if Obj.Entity_Name_Access = null then
            ""
         else
            Obj.Entity_Name_Access.all);

end Apsepp.Finalized_Debug_Tracer;
