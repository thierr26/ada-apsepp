-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Output.Config is

   ----------------------------------------------------------------------------

   procedure Setup (Locker : not null access SB_L_Locker'Class;
                    Inst   : Output_Access) is

   begin

      if Locker.Lock = Lock'Access and then Locker.Has_Actually_Locked then
         Instance_Access := Inst;
      end if;

   end Setup;

   ----------------------------------------------------------------------------

end Apsepp.Output.Config;
