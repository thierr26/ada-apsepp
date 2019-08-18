-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

function Apsepp_Test_Node_Barrier.Create_Test_Reporter
  (Barrier         : Test_Node_Barrier_Access;
   Char_Name_Image : Char_Name_Image_Func;
   Tag_To_Char     : Tag_To_Char_Func) return Test_Reporter_W_Barrier;
