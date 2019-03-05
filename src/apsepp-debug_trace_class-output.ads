-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Debug_Trace_Class.Stub; use Apsepp.Debug_Trace_Class.Stub;

package Apsepp.Debug_Trace_Class.Output is

   -- TODOC: Not implemented as a protected type (but Output sink may be).
   -- <2019-03-02>
   type Debug_Trace_Output is limited new Debug_Trace_Stub with private;

   overriding
   procedure Trace (Obj         : in out Debug_Trace_Output;
                    Message     :        String;
                    Entity_Name :        String             := "");

private

   type Debug_Trace_Output is limited new Debug_Trace_Stub with null record;

end Apsepp.Debug_Trace_Class.Output;
