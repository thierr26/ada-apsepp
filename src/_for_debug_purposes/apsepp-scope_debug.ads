-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Finalization;        use Ada.Finalization;

package Apsepp.Scope_Debug is

   type Controlled_Debug_Tracer
     (Entity_Name_Access      : access constant String := null;
      Entry_Trace, Exit_Trace : Boolean                := True)
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

   type Controlled_Debug_Tracer_A
     (Entity_Name_Access : access constant String := null)
     is tagged limited private;

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_A;
                    Item : String);

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_A;
                      Error : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_A;
      Reset_Elapsed : Boolean                   := False);

   type Controlled_Debug_Tracer_I
     (Entity_Name_Access : access constant String := null)
     is tagged limited private;

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_I;
                    Item : String);

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_I;
                      Error : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_I;
      Reset_Elapsed : Boolean                   := False);

   type Controlled_Debug_Tracer_F
     (Entity_Name_Access : access constant String := null)
     is tagged limited private;

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_F;
                    Item : String);

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_F;
                      Error : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_F;
      Reset_Elapsed : Boolean                   := False);

   type Controlled_Debug_Tracer_N
     (Entity_Name_Access : access constant String := null)
     is tagged limited private;

   not overriding
   procedure Trace (Obj  : Controlled_Debug_Tracer_N;
                    Item : String);

   not overriding
   procedure Trace_E (Obj   : Controlled_Debug_Tracer_N;
                      Error : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer_N;
      Reset_Elapsed : Boolean                   := False);

private

   not overriding
   function Entity_Name (Obj : Controlled_Debug_Tracer) return String
     is (if Obj.Entity_Name_Access = null then
            ""
         else
            Obj.Entity_Name_Access.all);

   type Controlled_Debug_Tracer_A
     (Entity_Name_Access : access constant String := null)
     is tagged limited record

      C_D_T : Controlled_Debug_Tracer
        (Entity_Name_Access => Entity_Name_Access,
         Entry_Trace        => True,
         Exit_Trace         => True);

   end record;

   type Controlled_Debug_Tracer_I
     (Entity_Name_Access : access constant String := null)
     is tagged limited record

      C_D_T : Controlled_Debug_Tracer
        (Entity_Name_Access => Entity_Name_Access,
         Entry_Trace        => True,
         Exit_Trace         => False);

   end record;

   type Controlled_Debug_Tracer_F
     (Entity_Name_Access : access constant String := null)
     is tagged limited record

      C_D_T : Controlled_Debug_Tracer
        (Entity_Name_Access => Entity_Name_Access,
         Entry_Trace        => False,
         Exit_Trace         => True);

   end record;

   type Controlled_Debug_Tracer_N
     (Entity_Name_Access : access constant String := null)
     is tagged limited record

      C_D_T : Controlled_Debug_Tracer
        (Entity_Name_Access => Entity_Name_Access,
         Entry_Trace        => False,
         Exit_Trace         => False);

   end record;

end Apsepp.Scope_Debug;
