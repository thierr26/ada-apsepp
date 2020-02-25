-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace_Class.Quiet; use Apsepp.Debug_Trace_Class.Quiet;

package Apsepp.Debug_Trace_Class.Output is

   -- TODOC: Not implemented as a protected type (but output sink may be).
   -- <2019-03-02>
   type Debug_Trace_Output is limited new Debug_Trace_Quiet with private;

   overriding
   procedure Trace (Obj         : in out Debug_Trace_Output;
                    Item        :        String;
                    Entity_Name :        String             := "");

private

   type Debug_Trace_Output is limited new Debug_Trace_Quiet with null record;

end Apsepp.Debug_Trace_Class.Output;
