-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class.Stub.Create;

function Apsepp_Test_Node_Barrier.Create_Test_Reporter
  (Barrier         : Test_Node_Barrier_Access;
   Char_Name_Image : Char_Name_Image_Func;
   Tag_To_Char     : Tag_To_Char_Func) return Test_Reporter_W_Barrier is

begin

   return (Test_Reporter_W_Barrier'(Create
             with C_D_T      => <>,
             Barrier         => Barrier,
             Char_Name_Image => Char_Name_Image,
             Tag_To_Char     => Tag_To_Char));

end Apsepp_Test_Node_Barrier.Create_Test_Reporter;
