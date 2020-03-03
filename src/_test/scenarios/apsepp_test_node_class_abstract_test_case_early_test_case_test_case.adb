-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions;

package body
  Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_Test_Case is

   ----------------------------------------------------------------------------

   procedure TNCATCETC_Early_Test is

   begin

      -- TODO: Implement real early test routine for
      -- 'Apsepp.Test_Node_Class.Abstract_Test_Case'.
      -- <2020-03-03>
      Ada.Assertions.Assert (True);

   end TNCATCETC_Early_Test;

   ----------------------------------------------------------------------------

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_T_C)
     return not null access procedure
     is (TNCATCETC_Early_Test'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_Test_Case;
