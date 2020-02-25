-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Debug_Trace_Class.Quiet;

package Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected type Debug_Trace_Standard is new Debug_Trace_Interfa with

      overriding
      function Item_W_Entity (Item        : String;
                              Entity_Name : String) return String;

      overriding
      function E_To_String (Error : Exception_Occurrence) return String;

      -- TODOC: Calls primitive 'Clock_String' of
      -- 'Apsepp.Debug_Trace_Class.Quiet.Debug_Trace_Quiet'. So the reset
      -- applies to all 'Debug_Trace_Interfa'Class' objects with an
      -- implementation relying on
      -- 'Apsepp.Debug_Trace_Class.Private_Protected_Clock'. <2020-02-24>
      overriding
      function Clock_String (Reset_Elapsed : Boolean := False) return String;

      overriding
      procedure Trace (Item        : String;
                       Entity_Name : String := "");

      overriding
      procedure Trace_E (Error : Exception_Occurrence;
                         Entity_Name : String := "");

      -- TODOC: Calls primitive 'Set_Time_Zone' of
      -- 'Apsepp.Debug_Trace_Class.Quiet.Debug_Trace_Quiet' so affects all
      -- 'Debug_Trace_Interfa'Class' objects with an implementation relying on
      -- 'Apsepp.Debug_Trace_Class.Private_Protected_Clock'. <2020-02-24>
      overriding
      procedure Set_Time_Zone (Time_Zone : Time_Offset);

      -- TODOC: Ditto. <2020-02-24>
      overriding
      procedure Set_Local_Time_Zone;

      -- TODOC: Calls 'Clock_String'. So the reset applies to all
      -- 'Debug_Trace_Interfa'Class' objects with an implementation relying on
      -- 'Apsepp.Debug_Trace_Class.Private_Protected_Clock'. <2020-02-24>
      overriding
      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False);

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

private

   use Quiet;

   Debug_Trace_Quiet_Instance : Debug_Trace_Quiet;

end Apsepp.Debug_Trace_Class.Standard;
