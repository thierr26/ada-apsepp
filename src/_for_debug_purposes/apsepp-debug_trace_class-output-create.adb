-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Debug_Trace_Class.Stub.Create;

function Apsepp.Debug_Trace_Class.Output.Create return Debug_Trace_Output is

begin

   return (Stub.Create with null record);

end Apsepp.Debug_Trace_Class.Output.Create;
