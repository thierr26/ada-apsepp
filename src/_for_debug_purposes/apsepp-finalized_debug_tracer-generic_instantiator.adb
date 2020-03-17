-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Finalized_Debug_Tracer.Generic_Instantiator is

   ----------------------------------------------------------------------------

   overriding
   procedure Finalize
     (Obj : in out Controlled_Debug_Tracer_W_Scope_Name_Dealloc) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => String,
         Name   => String_Access);

   begin

      Controlled_Debug_Tracer (Obj).Finalize; -- Inherited procedure call.

      Free (Scope_Name_Access);

   end Finalize;

   ----------------------------------------------------------------------------

   procedure Trace (Item : String) is

   begin

      C_D_T.Trace (Item);

   end Trace;

   ----------------------------------------------------------------------------

   procedure Trace_E (Error : Exception_Occurrence) is

   begin

      C_D_T.Trace_E (Error);

   end Trace_E;

   ----------------------------------------------------------------------------

   procedure Trace_Time (Reset_Elapsed : Boolean := False) is

   begin

      C_D_T.Trace_Time (Reset_Elapsed);

   end Trace_Time;

   ----------------------------------------------------------------------------

end Apsepp.Finalized_Debug_Tracer.Generic_Instantiator;
