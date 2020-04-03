-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Safe_Integer_Operations is

   ----------------------------------------------------------------------------

   function Val (X : Safe_Integer) return Integer_Type
     is (X.V);

   ----------------------------------------------------------------------------

   function Sat (X : Safe_Integer) return Boolean
     is (X.S);

   ----------------------------------------------------------------------------

   function Create (Value     : Integer_Type;
                    Saturated : Boolean      := False) return Safe_Integer
     is (V => Value,
         S => Saturated);

   Sat_First : constant Safe_Integer
     := Create (Value     => Integer_Type'First,
                Saturated => True);

   Sat_Last : constant Safe_Integer
     := Create (Value     => Integer_Type'Last,
                Saturated => True);

   ----------------------------------------------------------------------------

   function "+" (X_1 : Safe_Integer;
                 X_2 : Natural_Safe_Integer) return Safe_Integer is

      Ret : Safe_Integer := (if X_2 = Sat_Last then
                                X_2
                             else
                                X_1);

   begin

      if Ret /= Sat_Last then
         -- 'X_1 /= Sat_Last and X_2 /= Sat_Last' is true.

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
         -- 'X_1 = Sat_Last or X_2 = Sat_Last' is true.

         -- Nothing more to do.
         null;

      end if;

      return Ret;

   end "+";

   ----------------------------------------------------------------------------

   procedure Inc (X : in out Safe_Integer; By : Natural_Base := 1) is

   begin

      if X /= Sat_Last and then By /= 0 then
         -- 'X' has to be changed.

         declare

            V : constant Integer_Type := Val (X);

         begin

            if By = 1 and then V < Integer_Type'Last then
               -- Easy case.

               -- Just increment the value and reset the saturation flag.
               X.V := X.V + 1;
               X.S := False;

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

   procedure Dec (X : in out Safe_Integer) is

   begin

      if X /= Sat_First then
         -- 'X' has to be changed.

         declare

            V : constant Integer_Type := Val (X);

         begin

            if V > Integer_Type'First then

               -- Just decrement the value and reset the saturation flag.
               X.V := X.V - 1;
               X.S := False;

            else

               -- Just set the saturation flag.
               X.S := True;

            end if;

         end;

      else
         -- 'X' must be left unchanged.

         -- Nothing more to do.
         null;

      end if;

   end Dec;

   ----------------------------------------------------------------------------

   function Dec (X  : Safe_Integer) return Safe_Integer is

      Ret : Safe_Integer := X;

   begin

      Dec (Ret);

      return Ret;

   end Dec;

   ----------------------------------------------------------------------------

   function "*" (X_1, X_2 : Natural_Safe_Integer)
     return Natural_Safe_Integer is

      Sat_U_Value : constant Safe_Integer
        := Create (Value => Integer_Type'Last,
                   Saturated => True);

   begin

      if Val (X_1) = 0 or else Val (X_2) = 0 then
         -- The return value is 0, unsaturated.

         return Create (Value     => 0,
                        Saturated => False); -- Early return.

      elsif not Sat (X_1) and not Sat (X_2) then
         -- The return value is a non-zero value, possibly the largest possible
         -- value (saturated or not).

         declare

            function Product_Of_Unsaturated
              (Greatest,
               Smallest : Natural_Safe_Integer) return Natural_Safe_Integer is

               Gr_V : constant Integer_Type := Val (Greatest);
               Sm_V : constant Integer_Type := Val (Smallest);

            begin

              return (if Integer_Type'Last / Gr_V < Sm_V then
                         Sat_U_Value
                      else
                         Create (Value     => Gr_V * Sm_V,
                                 Saturated => False));

            end Product_Of_Unsaturated;

         begin

            if Val (X_1) > Val (X_2) then

               return Product_Of_Unsaturated (X_1, X_2); -- Early return.

            else

               return Product_Of_Unsaturated (X_2, X_1); -- Early return.

            end if;

         end;

      else
         -- The return value is the largest possible value, with saturation.

         return Sat_U_Value; -- Early return.

      end if;

   end "*";

   ----------------------------------------------------------------------------

end Apsepp.Generic_Safe_Integer_Operations;
