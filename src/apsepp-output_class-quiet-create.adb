-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

function Apsepp.Output_Class.Quiet.Create return Output_Quiet is

begin

   return (Output_Interfa with null record);

end Apsepp.Output_Class.Quiet.Create;
