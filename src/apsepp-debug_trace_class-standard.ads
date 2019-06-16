-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private with Apsepp.Debug_Trace_Class.Stub.Create;

package Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected type Debug_Trace_Standard is new Debug_Trace_Interfa with

      overriding
      function Message_W_Entity (Message     : String;
                                 Entity_Name : String) return String;

      overriding
      function E_To_String (E : Exception_Occurrence) return String;

      overriding
      procedure Trace (Message : String; Entity_Name : String := "");

      overriding
      procedure Trace_E (E : Exception_Occurrence; Entity_Name : String := "");

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

private

   use Stub;

   Debug_Trace_Stub_Instance : Debug_Trace_Stub := Create;

end Apsepp.Debug_Trace_Class.Standard;
