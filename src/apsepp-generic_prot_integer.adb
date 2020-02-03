-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Prot_Integer is

   ----------------------------------------------------------------------------

   function Create (V : Integer_Type; S : Boolean := False) return O_P_I_Type
     is (Value     => V,
         Saturated => False);

   ----------------------------------------------------------------------------

   function Val (X : O_P_I_Type) return Integer_Type
     is (X.Value);

   ----------------------------------------------------------------------------

   function Sat (X : O_P_I_Type) return Boolean
     is (X.Saturated);

   ----------------------------------------------------------------------------

   function "+" (X_1, X_2 : O_P_I_Type) return O_P_I_Type is

      Ret : O_P_I_Type := (if Sat (X_2) then
                              X_2
                           else
                              X_1);

   begin

      if not Sat (Ret) then
         -- Here, (not (Sat (X_1) or Sat (X_2))) is guaranteed.

         declare
            V_1 : constant Integer_Type := Val (X_1);
            V_2 : constant Integer_Type := Val (X_2);
         begin

            if V_1 <= 0 then
               Ret := Create (V_1 + V_2);
            else

               declare
                  D_L : constant Integer_Type'Base := Integer_Type'Last - V_1;
                  S   : constant Boolean           := D_L < V_2;
               begin
                  Ret := (if S then
                             Create (Integer_Type'Last, True)
                          else
                             Create (V_1 + V_2));
               end;

            end if;

         end;

      else
         -- Nothing to do here, Ret already computed.
         null;
      end if;

      return Ret;

   end "+";

   ----------------------------------------------------------------------------

   procedure Inc (X : in out O_P_I_Type; By : Integer_Type'Base := 1) is

   begin

      if not Sat (X) and then By /= 0 then

         declare

            V : constant Integer_Type := Val (X);

         begin

            if By = 1 then

               declare
                  S     : constant Boolean      := V = Integer_Type'Last;
                  New_V : constant Integer_Type := (if S then
                                                         V
                                                      else
                                                         V + 1);
               begin
                  X := Create (New_V, S);
               end;

            else

               if V <= 0 then

                  declare
                     New_V_B : constant Integer_Type'Base := V + By;
                     S       : constant Boolean           := New_V_B
                                                               >
                                                             Integer_Type'Last;
                  begin
                     X := (if S then
                              Create (Integer_Type'Last, True)
                           else
                              Create (New_V_B));
                  end;

               else

                  declare
                     D_L : constant Integer_Type'Base := Integer_Type'Last - V;
                     S   : constant Boolean           := D_L < By;
                  begin
                     X := (if S then
                              Create (Integer_Type'Last, True)
                           else
                              Create (V + By));
                  end;

               end if;

            end if;

         end;

      end if;

   end Inc;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Prot_Integer;
