-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Test_Reporter_Class.W_Node_Barrier.Create
  (Barrier         : access Test_Node_Barrier;
   Char_Name_Image : Char_Name_Image_Func;
   Tag_To_Char     : Tag_To_Char_Func) return Test_Reporter_W_Node_Barrier is

begin

   return (Test_Reporter_Sink with Barrier         => Barrier,
                                   Char_Name_Image => Char_Name_Image,
                                   Tag_To_Char     => Tag_To_Char);

end Apsepp.Test_Reporter_Class.W_Node_Barrier.Create;
