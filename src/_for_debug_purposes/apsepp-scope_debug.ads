-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Finalization; use Ada.Finalization;

package Apsepp.Scope_Debug is

   -- Don't evaluate the class-wide pre-conditions in this package. Just let
   -- Constraint_Error be raised in case of violation.
   pragma Assertion_Policy (Pre'Class => Ignore);

   Debug_Entity_Name_Max_Length : constant := 220;

   subtype Debug_Entity_Name_Length
     is Natural range 0 .. Debug_Entity_Name_Max_Length;

   type Controlled_Debug_Tracer (Entity_Name_Length : Debug_Entity_Name_Length)
     is limited new Limited_Controlled with private;

   not overriding
   function Create_A (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length <= Debug_Entity_Name_Max_Length;

   function Create (Entity_Name : String)
     return Controlled_Debug_Tracer renames Create_A;

   not overriding
   function Create_I (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length <= Debug_Entity_Name_Max_Length;

   not overriding
   function Create_F (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length <= Debug_Entity_Name_Max_Length;

   not overriding
   function Create_N (Entity_Name : String) return Controlled_Debug_Tracer

     with Pre'Class => Entity_Name'Length <= Debug_Entity_Name_Max_Length;

   not overriding
   function Entity (Obj : Controlled_Debug_Tracer) return String

     with Post'Class => Entity'Result'Length
                          <=
                        Debug_Entity_Name_Max_Length;

   not overriding
   procedure Trace (Obj : Controlled_Debug_Tracer; Message : String);

   not overriding
   procedure Trace_E (Obj : Controlled_Debug_Tracer; E : Exception_Occurrence);

   not overriding
   procedure Trace_Time
     (Obj           : Controlled_Debug_Tracer;
      Reset_Elapsed : Boolean                 := False);

   overriding
   procedure Finalize (Obj : in out Controlled_Debug_Tracer);

private

   type Controlled_Debug_Tracer (Entity_Name_Length : Debug_Entity_Name_Length)
     is limited new Limited_Controlled with record

      Entity_Name_Str     : String (1 .. Entity_Name_Length);
      Exit_Trace_Required : Boolean;

   end record;

end Apsepp.Scope_Debug;
