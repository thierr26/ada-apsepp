-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Generic_Discrete_Operations.Is_Monotonic_Incr_Lim_Array
  (A      : Array_Type;
   Strict : Boolean    := True) return Boolean is

   type Operand_Pair is record
      Left, Right : Element_Func_Ret_Type;
   end record;

   type Boolean_Operator
     is not null access function (Left, Right : Discrete_Type) return Boolean;

   ----------------------------------------------------------------------------

   function Operator_Strict (Left, Right : Discrete_Type) return Boolean is

      Operand : constant Operand_Pair := (Left  => Element_Func (A(Left)),
                                          Right => Element_Func (A(Right)));

   begin

      return Operand.Left < Operand.Right;

   end Operator_Strict;

   ----------------------------------------------------------------------------

   function Operator_Not_Strict (Left, Right : Discrete_Type) return Boolean is

      Operand : constant Operand_Pair := (Left  => Element_Func (A(Left)),
                                          Right => Element_Func (A(Right)));

   begin

      return Operand.Left < Operand.Right
               or else
             Operand.Left = Operand.Right;

   end Operator_Not_Strict;

   ----------------------------------------------------------------------------

   Is_Lower_Than : constant Boolean_Operator
     := (if Strict then
            Operator_Strict'Access
         else
            Operator_Not_Strict'Access);

begin

   return (for all K in A'First .. Discrete_Type'Pred (A'Last) =>
            Is_Lower_Than (K, Discrete_Type'Succ (K)));

end Apsepp.Generic_Discrete_Operations.Is_Monotonic_Incr_Lim_Array;
