-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Finalization; use Ada.Finalization;

package Apsepp.Controlled_Trace_Debugging is

   Debug_Tracer_Entity_Name_Max_Length : constant := 220;

   subtype Debug_Tracer_Entity_Name_Length
     is Positive range 1 .. Debug_Tracer_Entity_Name_Max_Length;

   type Controlled_Debug_Tracer
     (Entity_Name_Length : Debug_Tracer_Entity_Name_Length)
     is limited new Limited_Controlled with private;

   not overriding
   function Create_A (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length
                         <=
                       Debug_Tracer_Entity_Name_Max_Length;

   function Create (Entity_Name : String) return Controlled_Debug_Tracer
     renames Create_A;

   not overriding
   function Create_I (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length
                         <=
                       Debug_Tracer_Entity_Name_Max_Length;

   not overriding
   function Create_F (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length
                         <=
                       Debug_Tracer_Entity_Name_Max_Length;

   not overriding
   function Create_N (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length
                         <=
                       Debug_Tracer_Entity_Name_Max_Length;

   not overriding
   function Entity_Name (Obj : Controlled_Debug_Tracer) return String

     with Post'Class => Entity_Name'Result'Length
                          <=
                        Debug_Tracer_Entity_Name_Max_Length;

   not overriding
   procedure Trace (Obj : Controlled_Debug_Tracer; Message : String);

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer);

private

   type Controlled_Debug_Tracer
     (Entity_Name_Length : Debug_Tracer_Entity_Name_Length)
     is limited new Limited_Controlled with record

      Entity_Name                 : String (1 .. Entity_Name_Length);
      Finalization_Trace_Required : Boolean;

   end record;

end Apsepp.Controlled_Trace_Debugging;
