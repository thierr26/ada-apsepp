-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

function Apsepp.Test_Reporter_Class.Instant_Standard.Create
  return Test_Reporter_Instant_Standard is

begin

   return (Test_Reporter_Interfa with others => <>);

end Apsepp.Test_Reporter_Class.Instant_Standard.Create;
