-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Debug_Trace_Class.File is

   ----------------------------------------------------------------------------

   protected body Debug_Trace_File is

      -----------------------------------------------------

      procedure Set_Up
        (File_Name            : String;
         Time_Fraction_Digits : Positive_Field := Dflt_Time_Fraction_Digits) is

      begin

         Clean_Up;
         File_Name_Access := new String'(File_Name);

         Debug_Trace_Quiet_Instance.Set_Up (Time_Fraction_Digits);

      end Set_Up;

      -----------------------------------------------------

      procedure Clean_Up is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => String,
            Name   => String_Access);

      begin

         if Ada.Text_IO.Is_Open (Output_File) then
            Ada.Text_IO.Close (Output_File);
         end if;
         Free (File_Name_Access);

      end Clean_Up;

      -----------------------------------------------------

      function Item_W_Entity (Item        : String;
                              Entity_Name : String) return String
        is (Debug_Trace_Quiet_Instance.Item_W_Entity (Item,
                                                      Entity_Name));

      -----------------------------------------------------

      function E_To_String (Error : Exception_Occurrence) return String
        is (Debug_Trace_Quiet_Instance.E_To_String (Error));

      -----------------------------------------------------

      function Clock_String (Reset_Elapsed : Boolean := False) return String
        is (Debug_Trace_Quiet_Instance.Clock_String (Reset_Elapsed));

      -----------------------------------------------------

      procedure Trace (Item        : String;
                       Entity_Name : String := "") is

      begin

         if not Ada.Text_IO.Is_Open (Output_File) then
            Ada.Text_IO.Create (File => Output_File,
                                Name => File_Name_Access.all);
         end if;

         Ada.Text_IO.Put_Line (Output_File,
                               (if Entity_Name'Length /= 0 then
                                   Item_W_Entity (Item,
                                                  Entity_Name)
                                else
                                   Item));

      end Trace;

      -----------------------------------------------------

      procedure Trace_E (Error       : Exception_Occurrence;
                         Entity_Name : String               := "") is

      begin

         Trace (Item        => E_To_String (Error),
                Entity_Name => Entity_Name);

      end Trace_E;

      -----------------------------------------------------

      procedure Set_Time_Zone (Time_Zone : Time_Offset) is

      begin

         Debug_Trace_Quiet_Instance.Set_Time_Zone (Time_Zone);

      end Set_Time_Zone;

      -----------------------------------------------------

      procedure Set_Local_Time_Zone is

      begin

         Debug_Trace_Quiet_Instance.Set_Local_Time_Zone;

      end Set_Local_Time_Zone;

      -----------------------------------------------------

      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False) is

      begin

         Trace (Item        => Clock_String (Reset_Elapsed),
                Entity_Name => Entity_Name);

      end Trace_Time;

      -----------------------------------------------------

   end Debug_Trace_File;

   ----------------------------------------------------------------------------

   overriding
   procedure On_Release (Obj : Debug_Trace_File_Holder) is

      Instance_Access : constant not null access Debug_Trace_File
        := Obj.Instance_Access;

   begin

      Instance_Access.Clean_Up;
      -- PORT: Instance_Access intermediate variable used because direct
      -- 'Clean_Up' calls are rejected by compiler (like
      -- 'Obj.Instance_Access.Clean_Up' or 'Obj.Instance_Access.all.Clean_Up').
      -- Don't know why. <2020-07-27>

      Apsepp.Debug_Trace.Debug_Trace_Shared_Instance.Holder (Obj).On_Release;
                                                   -- Inherited procedure call.

   end On_Release;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.File;
