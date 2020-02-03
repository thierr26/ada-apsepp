-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Debug_Trace_Class.Stub.Create;

package Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected type Debug_Trace_Standard is new Debug_Trace_Interfa with

      overriding
      function Message_W_Entity (Message     : String;
                                 Entity_Name : String) return String;

      overriding
      function E_To_String (E : Exception_Occurrence) return String;

      -- TODOC: Calls primitive Clock_String of
      -- Apsepp.Debug_Trace_Class.Stub.Debug_Trace_Stub. So the reset
      -- applies to all Debug_Trace_Interfa'Class objects with an
      -- implementation relying on Reset_Date_Handler. <2019-06-29>
      overriding
      function Clock_String (Reset_Elapsed : Boolean := False) return String;

      overriding
      procedure Trace (Message : String; Entity_Name : String := "");

      overriding
      procedure Trace_E (E : Exception_Occurrence; Entity_Name : String := "");

      -- TODOC: Calls primitive Set_Time_Zone of
      -- Apsepp.Debug_Trace_Class.Stub.Debug_Trace_Stub so affects all
      -- Debug_Trace_Interfa'Class objects with an implementation relying on
      -- Reset_Date_Handler. <2019-06-29>
      overriding
      procedure Set_Time_Zone (Time_Zone : Time_Offset);

      -- TODOC: Ditto. <2019-06-29>
      overriding
      procedure Set_Local_Time_Zone;

      -- TODOC: CallsClock_String. So the reset applies to all
      -- Debug_Trace_Interfa'Class objects with an implementation relying on
      -- Reset_Date_Handler. <2019-06-29>
      overriding
      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False);

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

private

   use Stub;

   Debug_Trace_Stub_Instance : Debug_Trace_Stub := Create;

end Apsepp.Debug_Trace_Class.Standard;
