-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Debug_Trace_Class is

   pragma Pure (Debug_Trace_Class);

   type Debug_Trace_Interfa is limited interface;

   not overriding
   function Message_W_Entity (Obj         : Debug_Trace_Interfa;
                              Message     : String;
                              Entity_Name : String) return String is abstract;

   not overriding
   procedure Trace (Obj         : in out Debug_Trace_Interfa;
                    Message     : String;
                    Entity_Name : String := "") is null;

end Apsepp.Debug_Trace_Class;