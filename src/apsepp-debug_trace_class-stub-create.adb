-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

function Apsepp.Debug_Trace_Class.Stub.Create return Debug_Trace_Stub is

begin

   return (Debug_Trace_Interfa with null record);

end Apsepp.Debug_Trace_Class.Stub.Create;
