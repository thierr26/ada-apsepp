-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   Entity_Name : String := "";

   Kind : Controlled_Debug_Tracer_Kind := A;

package Apsepp.Finalized_Debug_Tracer.Generic_Instantiator is

   procedure Trace (Item : String);

   procedure Trace_E (Error : Exception_Occurrence);

   procedure Trace_Time (Reset_Elapsed : Boolean := False);

private

   type String_Access is access String;

   Entity_Name_Access : String_Access := new String'(Entity_Name);

   type Controlled_Debug_Tracer_W_Entity_Name_Dealloc
     is limited new Controlled_Debug_Tracer with null record;

   overriding
   procedure Finalize
     (Obj : in out Controlled_Debug_Tracer_W_Entity_Name_Dealloc);

   C_D_T : Controlled_Debug_Tracer_W_Entity_Name_Dealloc
     (Entity_Name_Access => Entity_Name_Access,
      Kind               => Kind);

end Apsepp.Finalized_Debug_Tracer.Generic_Instantiator;