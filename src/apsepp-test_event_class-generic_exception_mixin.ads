-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic
   type Parent is new Test_Event_Base with private;
package Apsepp.Test_Event_Class.Generic_Exception_Mixin is

   type Child_W_Exception is new Parent with private

     with Type_Invariant'Class =>
            Child_W_Exception.Exception_Access = null
              or else
            (
              Exception_Identity (Child_W_Exception.Exception_Access.all)
                /=
              Null_Id
            );

   overriding
   procedure Set (Obj : in out Child_W_Exception; Data : Test_Event_Data);

   overriding
   procedure Clean_Up (Obj : in out Child_W_Exception);

   overriding
   function Exception_Access
     (Obj : Child_W_Exception) return Exception_Occurrence_Access;

   overriding
   procedure Free_Exception (Obj : in out Child_W_Exception);

private

   type Child_W_Exception is new Parent with record
      Error : Exception_Occurrence_Access;
   end record;

end Apsepp.Test_Event_Class.Generic_Exception_Mixin;
