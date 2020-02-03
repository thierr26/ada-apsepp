-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- with GNAT.Exception_Traces; -- For debugging.
with Apsepp_Test_Harness;

procedure Apsepp_Test is

   -- use GNAT.Exception_Traces; -- For debugging.

begin

   -- Trace_On (Every_Raise); -- For debugging.

   Apsepp_Test_Harness.Apsepp_Test_Procedure;

   -- Trace_Off; -- For debugging.

end Apsepp_Test;
