-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions,
     Ada.Characters.Handling,
     Apsepp_Testing_System_Test_Fixture,
     Apsepp.Generic_Fixture.Creator,
     Apsepp.Scope_Debug,
     Apsepp.Tags,
     Apsepp_Test_Node_Barrier;

package body Apsepp_Test_Node_Class_Early_Test_Case is

   use Ada.Characters.Handling,
       Apsepp.Test_Node_Class,
       Apsepp_Testing_System_Test_Fixture,
       Apsepp_Test_Node_Barrier;

   ----------------------------------------------------------------------------

   function TSF return Testing_System_Test_Fixture_Access
     renames Instance;

   ----------------------------------------------------------------------------

   Expected : constant Routine_State_Array
     := ((T => TSF.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 6, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => TSF.A,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 3, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 4, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 5, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => TSF.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => TSF.E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => TSF.E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 2, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 3, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => TSF.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed));

   ----------------------------------------------------------------------------

   function Routine_State_Array_To_Tag_Array
     (A : Routine_State_Array) return Tag_Array is

      Ret : Tag_Array (A'Range);

   begin

      for K in A'Range loop
         Ret(K) := A(K).T;
      end loop;

      return Ret;

   end Routine_State_Array_To_Tag_Array;

   ----------------------------------------------------------------------------

   procedure Validate (K           : Positive;
                       Event_Kind  : Test_Event_Kind;
                       Char        : ISO_646;
                       Char_To_Tag : Char_To_Tag_Func;
                       Msg_Pref    : String) is

      pragma Unreferenced (Event_Kind);

      -----------------------------------------------------

      procedure Put_Array (A : Routine_State_Array; Msg_Pref : String) is

         use Apsepp.Scope_Debug,
             Apsepp.Tags;

         Entity_Name : constant String
           := "Apsepp_Test_Node_Class_Early_Test_Case.Put_Array";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

         Line : constant String (1 .. 53) := (others => '-');

         First_Done : Boolean := False;

      begin

         for E of A loop

            if not First_Done then
               First_Done := True;
               C_D_T.Trace (Msg_Pref & Line);
            end if;

            C_D_T.Trace
              (Test_Routine_Count'Image (E.Routine_I)
               & " " & Test_Assert_Count'Image (E.Assert_C)
               & " " & Test_Outcome'Image (E.Assert_O)
               & " " & Total_Expanded_Name (E.T));

         end loop;

         if First_Done then
            C_D_T.Trace (Msg_Pref & Line);
         end if;

      end Put_Array;

      -----------------------------------------------------

      Is_Runner : constant Boolean := Ada.Characters.Handling.Is_Lower (Char);

      Exp     : constant Flattened_Routine_State := Expected(K);
      Exp_R_I : constant Test_Routine_Count      := Exp.Routine_I;
      Exp_A_C : constant Test_Assert_Count       := Exp.Assert_C;
      Exp_A_O : constant Test_Outcome            := Exp.Assert_O;

   begin

      if Is_Runner then
         -- Just check that Expected contains conventional values (0 / Passed).
         -- TODO: Refactor (use generics). <2019-08-11>

         Ada.Assertions.Assert (Exp_R_I = 0,
           "Exp_R_I =" & Test_Routine_Count'Image (Exp_R_I)
           & ", 0 expected");
         Ada.Assertions.Assert (Exp_A_C = 0,
           "Exp_A_C =" & Test_Assert_Count'Image (Exp_A_C)
           & ", 0 expected");
         Ada.Assertions.Assert (Exp_A_O = Passed,
           "Exp_A_O = " & Test_Outcome'Image (Exp_A_O)
           & ", PASSED expected");

      else
         -- Do a real validation.

         declare

            A   : constant Routine_State_Array := To_Array;
            K_T :          Positive            := A'First;
            T   : constant Tag                 := Char_To_Tag (Char);

            function Positive_Image (K : Positive) return String is
               Img : constant String := Positive'Image (K);
            begin
               return Img(Img'First + 1 .. Img'Last);
            end Positive_Image;

            -- TODO: Refactor (use generics). <2019-06-30>
            function A_K_R_I (K : Natural) return Test_Routine_Count
              is (if K in A'Range then
                     A(K).Routine_I
                  else
                     0);

            function A_K_A_C (K : Natural) return Test_Assert_Count
              is (if K in A'Range then
                     A(K).Assert_C
                  else
                     0);

            function A_K_A_O (K : Natural) return Test_Outcome
              is (if K in A'Range then
                     A(K).Assert_O
                  else
                     Passed);

         begin

            Put_Array (A, Msg_Pref);

            -- Validate the "Routine_State_Map_Handler in initial state" case.
            -- TODO: Refactor (use generics). <2019-08-11>
            Ada.Assertions.Assert (A'Length > 0
                                     or else
                                   Exp_R_I = 0,
              "A'Length =" & Integer'Image (A'Length)
              & ", Exp_R_I should be 0");
            Ada.Assertions.Assert (A'Length > 0
                                     or else
                                   Exp_A_C = 0,
              "A'Length =" & Integer'Image (A'Length)
              & ", Exp_A_C should be 0");
            Ada.Assertions.Assert (A'Length > 0
                                     or else
                                   Exp_A_O = Passed,
              "A'Length =" & Integer'Image (A'Length)
              & ", Exp_A_O should be PASSED");

            -- TODO: Write function Find or Find_First in
            -- Generic_Array_Operations. <2019-08-11>
            if A'Length > 0 then
               while A(K_T).T /= T and then K_T < A'Last loop
                  K_T := K_T + 1;
               end loop;
            end if;

            -- Validate the Routine_State_Map_Handler private component S
            -- (retrieved in A at index K_T) in the
            -- "Routine_State_Map_Handler no more in initial state" case.
            Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                     or else
                                   A_K_R_I (K_T) = Exp_R_I,
              "A'Length =" & Integer'Image (A'Length)
              & "," & Test_Routine_Count'Image (A_K_R_I (K_T))
              & " (A_K_R_I (" & Positive_Image (K_T) & ")) /="
              & Test_Routine_Count'Image (Exp_R_I) & " (Exp_R_I)");
            Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                     or else
                                   A_K_A_C (K_T) = Exp_A_C,
              "A'Length =" & Integer'Image (A'Length)
              & "," & Test_Assert_Count'Image (A_K_A_C (K_T))
              & " (A_K_A_C (" & Positive_Image (K_T) & ")) /="
              & Test_Assert_Count'Image (Exp_A_C) & " (Exp_A_C)");
            Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                     or else
                                   A_K_A_O (K_T) = Exp_A_O,
              "A'Length =" & Integer'Image (A'Length)
              & ", " & Test_Outcome'Image (A_K_A_O (K_T))
              & " (A_K_A_O (" & Positive_Image (K_T) & ")) /= "
              & Test_Outcome'Image (Exp_A_O) & " (Exp_A_O)");

         end;

      end if;

   end Validate;

   ----------------------------------------------------------------------------

   procedure Early_Test_TNCETC is

      package Testing_System_T_F_Creator is new Testing_System_T_F.Creator;

      Expected_Tag : Apsepp.Tags.Tag_Array_Access;

   begin

      Ada.Assertions.Assert (Testing_System_T_F_Creator.Has_Actually_Created,
        "Test fixture already locked");

      Expected_Tag
        := new Tag_Array'(Routine_State_Array_To_Tag_Array (Expected));

      TSF.Run_Test (Expected_Tag, Validate'Access);

      Apsepp.Tags.Free (Expected_Tag);

   exception

      when others => Apsepp.Tags.Free (Expected_Tag);
                     raise;

   end Early_Test_TNCETC;

   ----------------------------------------------------------------------------

   overriding
   function Early_Routine (Obj : Apsepp_Test_Node_Class_E_T_C)
     return Apsepp.Abstract_Early_Test_Case.Test_Routine
     is (Early_Test_TNCETC'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Node_Class_Early_Test_Case;
