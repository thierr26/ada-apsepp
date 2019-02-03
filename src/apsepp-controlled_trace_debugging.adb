-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Trace_Debugging;

package body Apsepp.Controlled_Trace_Debugging is

   ----------------------------------------------------------------------------

   function C (Entity_Name : String; I_T_R, F_T_R : Boolean)
     return Controlled_Debug_Tracer is

   begin

      if I_T_R then

         Apsepp.Trace_Debugging.Trace_Debugging.Trace
           ("Initialize", Entity_Name);

      end if;

     return (Limited_Controlled
               with Entity_Name_Length          => Entity_Name'Length,
                    Entity_Name                 => Entity_Name,
                    Finalization_Trace_Required => F_T_R);

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
   function Entity_Name (Obj : Controlled_Debug_Tracer) return String
     is (Obj.Entity_Name);

   ----------------------------------------------------------------------------

   not overriding
   procedure Trace (Obj : Controlled_Debug_Tracer; Message : String) is

   begin

      Apsepp.Trace_Debugging.Trace_Debugging.Trace (Message, Obj.Entity_Name);

   end Trace;

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer) is

   begin

      if Obj.Finalization_Trace_Required then

         Apsepp.Trace_Debugging.Trace_Debugging.Trace
           ("Finalize", Obj.Entity_Name);

      end if;

   end Finalize;

   ----------------------------------------------------------------------------

end Apsepp.Controlled_Trace_Debugging;
