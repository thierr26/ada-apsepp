-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Ada.Assertions,
     Ada.Characters.Handling,
     Ada.Tags,
     Ada.Strings.Fixed,
     Apsepp.Tags,
     Apsepp.Lock_Holder_Class,
     Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array,
     Apsepp.Test_Node_Class.Protected_Test_Node_Barrier,
     Apsepp.Test_Event_Class,
     Apsepp.Finalized_Debug_Tracer.Generic_Instantiator,
     Apsepp.Test_Case_Count_Types;

package body Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case is

   use Ada.Characters.Handling,
       Ada.Tags,
       Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array,
       Apsepp_Testing_System_Test_Fixture,
       Apsepp.Test_Node_Class.Protected_Test_Node_Barrier,
       Apsepp.Test_Event_Class,
       Apsepp.Finalized_Debug_Tracer;

   ----------------------------------------------------------------------------

   function T_F return not null access Apsepp_Testing_System_T_F
     renames TS_T_F.I_A;

   ----------------------------------------------------------------------------

   function Expected_Case_Status_Array return Flat_Tag_Case_Status_Array
     is ((T => T_F.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 6, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => T_F.A,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 3, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 4, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 5, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => T_F.C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => T_F.E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => T_F.E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 2, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 3, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => T_F.A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed));

   ----------------------------------------------------------------------------

   -- Extract tag values from a 'Flat_Tag_Case_Status_Array' array.
   function Flat_Tag_Case_Status_Array_To_Tag_Array
     (A : Flat_Tag_Case_Status_Array) return Tag_Array is

      Ret : Tag_Array (A'Range);

   begin

      for K in A'Range loop
         Ret(K) := A(K).T;
      end loop;

      return Ret;

   end Flat_Tag_Case_Status_Array_To_Tag_Array;

   ----------------------------------------------------------------------------

   procedure Validate (K           : Positive;
                       Event_Kind  : Test_Event_Kind;
                       Event_Data  : Test_Event_Data;
                       Char        : ISO_646;
                       Char_To_Tag : Char_To_Tag_Func;
                       Msg_Pref    : String) is

      use Apsepp.Test_Case_Count_Types;

      pragma Unreferenced (Event_Kind, Event_Data);

      -----------------------------------------------------

      -- Output a debug trace of a 'Flat_Tag_Case_Status_Array' array.
      procedure Put_Array (A        : Flat_Tag_Case_Status_Array;
                           Msg_Pref : String) is

         use Apsepp.Tags;

         package C_D_T
         is new Apsepp.Finalized_Debug_Tracer.Generic_Instantiator
           (Entity_Name =>
              "Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case"
              & ".Validate.Put_Array",
            Kind        => N);

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

      generic

         type Discrete_Type is (<>);

         Var_Name : String;

      procedure Generic_Assert_Discrete_Var_Value
        (X,
         Expected      : Discrete_Type;
         Null_A_Length : Boolean       := False);

      procedure Generic_Assert_Discrete_Var_Value
        (X,
         Expected      : Discrete_Type;
         Null_A_Length : Boolean       := False) is

         use Ada.Strings,
             Ada.Strings.Fixed;

      begin

         Ada.Assertions.Assert
           (X = Expected,
            Var_Name
            & " = "
            & Trim (Discrete_Type'Image (X), Left)
            & ", "
            & Trim (Discrete_Type'Image (Expected), Left)
            & " expected"
            & (if Null_A_Length then
                  " (A'Length = 0)"
               else
                  ""));

      end Generic_Assert_Discrete_Var_Value;

      procedure Assert_Test_Routine_Count_Var_Value
        is new Generic_Assert_Discrete_Var_Value
        (Discrete_Type => Test_Routine_Count,
         Var_Name      => "Exp_R_I");

      procedure Assert_Test_Assert_Count_Var_Value
        is new Generic_Assert_Discrete_Var_Value
        (Discrete_Type => Test_Assert_Count,
         Var_Name      => "Exp_A_C");

      procedure Assert_Test_Outcome_Var_Value
        is new Generic_Assert_Discrete_Var_Value
        (Discrete_Type => Test_Outcome,
         Var_Name      => "Exp_A_O");

      Is_Runner : constant Boolean := Ada.Characters.Handling.Is_Lower (Char);

      Exp     : constant Flat_Tag_Case_Status := Expected_Case_Status_Array(K);
      Exp_R_I : constant Test_Routine_Count   := Exp.Routine_I;
      Exp_A_C : constant Test_Assert_Count    := Exp.Assert_C;
      Exp_A_O : constant Test_Outcome         := Exp.Assert_O;

   begin

      if Is_Runner then
         -- Just check that Expected contains conventional values (0 / Passed).

         Assert_Test_Routine_Count_Var_Value (Exp_R_I, 0);
         Assert_Test_Assert_Count_Var_Value (Exp_A_C, 0);
         Assert_Test_Outcome_Var_Value (Exp_A_O, Passed);

      else
         -- Do a real validation.

         declare

            A   : constant Flat_Tag_Case_Status_Array := Case_Status_Array;
            K_T :          Positive                   := A'First;
            T   : constant Tag                        := Char_To_Tag (Char);

            generic

               type Discrete_Type is (<>);

               with function Get_Field (K : Positive) return Discrete_Type;

               Fallback_Value : Discrete_Type;

            function Generic_A_Component_Or_Fallback
              (K : Positive) return Discrete_Type;

            function Generic_A_Component_Or_Fallback
              (K : Positive) return Discrete_Type
              is (if K in A'Range then
                     Get_Field (K)
                  else
                     Fallback_Value);

            function Get_Routine_I_Field
              (K : Positive) return Test_Routine_Count
              is (A(K).Routine_I);

            function A_K_R_I is new Generic_A_Component_Or_Fallback
              (Discrete_Type  => Test_Routine_Count,
               Get_Field      => Get_Routine_I_Field,
               Fallback_Value => 0);

            function Get_Assert_C_Field
              (K : Positive) return Test_Assert_Count
              is (A(K).Assert_C);

            function A_K_A_C is new Generic_A_Component_Or_Fallback
              (Discrete_Type  => Test_Assert_Count,
               Get_Field      => Get_Assert_C_Field,
               Fallback_Value => 0);

            function Get_Assert_O_Field
              (K : Positive) return Test_Outcome
              is (A(K).Assert_O);

            function A_K_A_O is new Generic_A_Component_Or_Fallback
              (Discrete_Type  => Test_Outcome,
               Get_Field      => Get_Assert_O_Field,
               Fallback_Value => Passed);

            generic

               type Discrete_Type is (<>);

               with function A_Component_Or_Fallback
                 (K : Positive) return Discrete_Type;

               Var_Name,
               Func_Name: String;

            procedure Generic_Validate_S (K_T      : Positive;
                                          Expected : Discrete_Type);

            procedure Generic_Validate_S (K_T      : Positive;
                                          Expected : Discrete_Type) is

               use Ada.Strings,
                   Ada.Strings.Fixed;

            begin

               Ada.Assertions.Assert
                 ((A'Length = 0 or else A(K_T).T /= T)
                    or else
                   A_Component_Or_Fallback (K_T) = Expected,
                  "A'Length ="
                  & Integer'Image (A'Length)
                  & ", "
                  & Trim (Discrete_Type'Image (A_Component_Or_Fallback (K_T)),
                          Left)
                  & " ("
                  & Func_Name
                  & " ("
                  & Trim (Positive'Image (K_T), Left)
                  & ")) /= "
                  & Trim (Discrete_Type'Image (Expected), Left)
                  & " ("
                  & Var_Name
                  & ")");

            end Generic_Validate_S;

            procedure Validate_S_Routine_I
              is new Generic_Validate_S
              (Discrete_Type           => Test_Routine_Count,
               A_Component_Or_Fallback => A_K_R_I,
               Var_Name                => "Exp_R_I",
               Func_Name               => "A_K_R_I");

            procedure Validate_S_Assert_C
              is new Generic_Validate_S
              (Discrete_Type           => Test_Assert_Count,
               A_Component_Or_Fallback => A_K_A_C,
               Var_Name                => "Exp_A_C",
               Func_Name               => "A_K_A_C");

            procedure Validate_S_Assert_O
              is new Generic_Validate_S
              (Discrete_Type           => Test_Outcome,
               A_Component_Or_Fallback => A_K_A_O,
               Var_Name                => "Exp_A_O",
               Func_Name               => "A_K_A_O");

         begin

            Put_Array (A, Msg_Pref);

            if A'Length = 0 then
               -- Validate the "'Case_Status_Map_Handler' in initial state"
               -- case.
               Assert_Test_Routine_Count_Var_Value (Exp_R_I, 0, True);
               Assert_Test_Assert_Count_Var_Value (Exp_A_C, 0, True);
               Assert_Test_Outcome_Var_Value (Exp_A_O, Passed, True);
            end if;

            -- TODO: Write function Find or Find_First in
            -- Generic_Array_Operations. <2020-03-12>
            if A'Length > 0 then
               while A(K_T).T /= T and then K_T < A'Last loop
                  K_T := K_T + 1;
               end loop;
            end if;

            -- Validate the 'Case_Status_Map_Handler' private component S
            -- (retrieved in A at index K_T) in the
            -- "'Case_Status_Map_Handler' no more in initial state" case.
            Validate_S_Routine_I (K_T, Exp_R_I);
            Validate_S_Assert_C (K_T, Exp_A_C);
            Validate_S_Assert_O (K_T, Exp_A_O);

         end;

      end if;

   end Validate;

   ----------------------------------------------------------------------------

   -- Declared outside procedure
   -- 'Test_Node_Class_Abstract_Test_Case_Early_Test' to avoid an accessibility
   -- check failure at run-time.
   type Tag_Array_Access is access Tag_Array;

   procedure Test_Node_Class_Abstract_Test_Case_Early_Test is

      TS_L_H_C_H : Apsepp.Lock_Holder_Class.Lock_Holder_Controlled_Handler
        (L_H      => L_H_A(TS),
         Disabled => False);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Tag_Array,
         Name   => Tag_Array_Access);

      Expected_Tag : Tag_Array_Access;

      pragma Unreferenced (TS_L_H_C_H);

   begin

      Ada.Assertions.Assert (All_Holding (L_H_A),
                             "Test fixture already locked.");

      Expected_Tag := new Tag_Array'(Flat_Tag_Case_Status_Array_To_Tag_Array
                                       (Expected_Case_Status_Array));

      T_F.Run_Test (Expected_Tag, Validate'Access);

      Free (Expected_Tag);

   exception

      when others => Free (Expected_Tag);
                     raise;

   end Test_Node_Class_Abstract_Test_Case_Early_Test;

   ----------------------------------------------------------------------------

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_Abstract_Test_Case_E_T_C)
     return not null access procedure
     is (Test_Node_Class_Abstract_Test_Case_Early_Test'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case;
