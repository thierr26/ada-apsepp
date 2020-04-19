-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Safe_Counter is

   ----------------------------------------------------------------------------

   function Val (X : Safe_Counter) return Integer_Type
     is (Safe_Integer_Operations.Val
           (Safe_Integer_Operations.Safe_Integer (X)));

   ----------------------------------------------------------------------------

   function Sat (X : Safe_Counter) return Boolean
     is (Safe_Integer_Operations.Sat
           (Safe_Integer_Operations.Safe_Integer (X)));

   ----------------------------------------------------------------------------

   function "=" (Left  : Safe_Counter;
                 Right : Integer_Type) return Boolean
     is (Val (Left) = Right);

   ----------------------------------------------------------------------------

   procedure Inc (X : in out Safe_Counter) is

   begin

      Safe_Integer_Operations.Inc (Safe_Integer_Operations.Safe_Integer (X));

   end Inc;

   ----------------------------------------------------------------------------

   procedure Dec (X : in out Safe_Counter) is

   begin

      if not Sat (X)      -- 'Sat (X)' implies 'X = Integer_Type'Last'.
           and then
         Val (X) > Integer_Type'Val (0) then
         -- 'X' is not saturated (which means: not saturated at the upper
         -- bound) and not zero.

         -- Decrement.
         Safe_Integer_Operations.Dec
           (Safe_Integer_Operations.Safe_Integer (X));

      else
         -- 'Val (X) = Integer_Type'Last' or 'X = 0'.

         -- Do nothing. This ensures that a 'Safe_Counter' object is never
         -- unsaturated and that it is never saturated because of a 'Dec' call.
         -- Only an 'Inc' call can saturate a 'Safe_Counter'.
         null;

      end if;

   end Dec;

   ----------------------------------------------------------------------------

   procedure Reset (X : in out Safe_Counter) is

      Default_Safe_Counter_Value : Safe_Counter;

   begin

      -- Here we rely on the default value of 'Safe_Counter', inherited from
      -- 'Safe_Integer_Operations.Safe_Integer'.
      X := Default_Safe_Counter_Value;

   end Reset;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Safe_Counter;
