-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Trace_Debugging_Class.Stub.Create;

function Apsepp.Trace_Debugging_Class.Output.Create
  return Trace_Debugging_Output is

begin

   return (Apsepp.Trace_Debugging_Class.Stub.Create with null record);

end Apsepp.Trace_Debugging_Class.Output.Create;
