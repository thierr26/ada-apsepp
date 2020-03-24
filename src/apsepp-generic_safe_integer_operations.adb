-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Safe_Integer_Operations is

   ----------------------------------------------------------------------------

   function Create (Value     : Integer_Type;
                    Saturated : Boolean      := False) return Safe_Integer
     is (V => Value,
         S => Saturated);

   ----------------------------------------------------------------------------

   function Val (X : Safe_Integer) return Integer_Type
     is (X.V);

   ----------------------------------------------------------------------------

   function Sat (X : Safe_Integer) return Boolean
     is (X.S);

   ----------------------------------------------------------------------------

   function Sat_U (X : Safe_Integer) return Boolean
     is (Sat (X) and then Val (X) = Integer_Type'Last);

   ----------------------------------------------------------------------------

   function "+" (X_1 : Safe_Integer;
                 X_2 : Natural_Safe_Integer) return Safe_Integer is

      Ret : Safe_Integer := (if Sat_U (X_2) then
                                X_2
                             else
                                X_1);

   begin

      if not Sat_U (Ret) then
         -- 'not (Sat_U (X_1) or Sat_U (X_2))' is true.

         declare
            V_1 : constant Integer_Type := Val (X_1);
            V_2 : constant Integer_Type := Val (X_2);
         begin

            if V_1 <= 0 then
               -- 'V1 + V_2' is safe because 'V_2 >= 0'.

               Ret := Create (Value     => V_1 + V_2,
                              Saturated => False);

            else

               declare

                  -- Compute distance from 'V_1' to Integer_Type's greatest
                  -- value.
                  D_L : constant Integer_Type'Base := Integer_Type'Last - V_1;

                  -- Compute the saturated state of the returned value.
                  S   : constant Boolean := D_L < V_2;

               begin

                  Ret := Create (Value     => (if S then
                                                  Integer_Type'Last
                                               else
                                                  V_1 + V_2),
                                 Saturated => S);

               end;

            end if;

         end;

      else
         -- 'Sat_U (X_1) or Sat_U (X_2)' is true.

         -- Nothing more to do.
         null;

      end if;

      return Ret;

   end "+";

   ----------------------------------------------------------------------------

   procedure Inc (X : in out Safe_Integer; By : Natural_Base := 1) is

   begin

      if not Sat_U (X) and then By /= 0 then
         -- 'X' has to be changed.

         declare

            V : constant Integer_Type := Val (X);

         begin

            if By = 1 and then V < Integer_Type'Last then
               -- Easy case.

               -- Just increment the value.
               X.V := X.V + 1;

            elsif By = 1 then
               -- Still easy.

               -- Just turn the saturation flag on.
               X.S := True;

            else
               -- Not so easy.

               if V <= 0 then
                  -- 'V + By' is safe because 'By >= 0'.

                  declare
                     Sum : constant Integer_Type'Base := V + By;
                     S   : constant Boolean := Sum > Integer_Type'Last;
                  begin
                     X := Create (Value     => (if S then
                                                   Integer_Type'Last
                                                else
                                                   Sum),
                                  Saturated => S);
                  end;

               else

                  declare

                     -- Compute distance from 'V' to Integer_Type's greatest
                     -- value.
                     D_L : constant Integer_Type'Base := Integer_Type'Last - V;

                     -- Compute the saturated state of the returned value.
                     S : constant Boolean := D_L < By;

                  begin

                     X := Create (Value     => (if S then
                                                   Integer_Type'Last
                                                else
                                                   V + By),
                                  Saturated => S);

                  end;

               end if;

            end if;

         end;

      else
         -- 'X' must be left unchanged.

         -- Nothing more to do.
         null;

      end if;

   end Inc;

   ----------------------------------------------------------------------------

   function Inc (X  : Safe_Integer;
                 By : Natural_Base := 1) return Safe_Integer is

      Ret : Safe_Integer := X;

   begin

      Inc (Ret, By);

      return Ret;

   end Inc;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Safe_Integer_Operations;
