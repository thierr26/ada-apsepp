-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions,
     Ada.Characters.Handling,
     Apsepp.Characters,
     Apsepp.Test_Node_Class.Abstract_Test_Suite,
     Apsepp.Test_Node_Class.Abstract_Simu_Test_Case,
     Apsepp.Test_Node_Class.Runner_Sequential.Create,
     Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create,
     Apsepp.Test_Reporter_Class.W_Node_Barrier.Create,
     Apsepp.Debug_Trace,
     Apsepp.Debug_Trace_Class.Standard,
     Apsepp.Generic_Shared_Instance.Finalized_S_R,
     Apsepp.Generic_Discrete_Operations.Is_Lim_Array_Wo_Dup;

package body Apsepp_Testing_System_Test_Fixture is

   use Ada.Characters.Handling,
       Apsepp.Characters,
       Apsepp.Test_Node_Class.Abstract_Simu_Test_Case,
       Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes,
       Apsepp.Debug_Trace,
       Apsepp.Test_Reporter_Class.W_Node_Barrier;

   ----------------------------------------------------------------------------

   -- Define simulated test case types.

   type Simu_Test_Case_A is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_A) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 3),
         (Kind => Test_Assertion_Failure, Successful_Test_Assert_Count => 2),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));

   type Simu_Test_Case_B is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_B) return Simu_Test_Case_Story
     is ((Kind => Other_Failure,          Successful_Test_Assert_Count => 5),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 2nd test routine not run (unexpected failure on 1st test routine causes
   -- the following test routines to be cancelled).

   type Simu_Test_Case_C is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_C) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 7),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 6));

   type Simu_Test_Case_D is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_D) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => Access_Failure,         Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 3rd test routine not run (access failure on 2nd test routine causes the
   -- following test routines to be cancelled).

   type Simu_Test_Case_E is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_E) return Simu_Test_Case_Story
     is ((Kind => Handled_Test_Failure,   Successful_Test_Assert_Count => 3),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 2),
         (Kind => Contract_Failure,       Successful_Test_Assert_Count => 4),
         (Kind => Setup_Failure,          Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 5th test routine not run (setup failure on 4th test routine causes the
   -- following test routines to be cancelled).

   ----------------------------------------------------------------------------

   -- Declare one instance for simulated test case type.

   Simu_Case_A : aliased Simu_Test_Case_A;
   Simu_Case_B : aliased Simu_Test_Case_B;
   Simu_Case_C : aliased Simu_Test_Case_C;
   Simu_Case_D : aliased Simu_Test_Case_D;
   Simu_Case_E : aliased Simu_Test_Case_E;

   ----------------------------------------------------------------------------

   -- Build an array of accesses to simulated test cases (one access for each
   -- of the simulated test case types defined above). The array index type is
   -- an upper case letter type.

   type Test_Node_Access
     is not null access all Apsepp.Test_Node_Class.Test_Node_Interfa'Class;

   type Test_Node_Char_Array
     is array (ISO_646_Upper_Letter range <>) of Test_Node_Access;

   subtype Simu_Case_Char_Name is ISO_646_Upper_Letter range 'A' .. 'E';
   Simu_Case : constant Test_Node_Char_Array (Simu_Case_Char_Name)
     := (Simu_Case_A'Access,
         Simu_Case_B'Access,
         Simu_Case_C'Access,
         Simu_Case_D'Access,
         Simu_Case_E'Access);

   ----------------------------------------------------------------------------

   not overriding
   function A (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Simu_Case('A')'Tag);

      -----------------------------------------------------

   not overriding
   function B (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Simu_Case('B')'Tag);

      -----------------------------------------------------

   not overriding
   function C (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Simu_Case('C')'Tag);

      -----------------------------------------------------

   not overriding
   function D (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Simu_Case('D')'Tag);

      -----------------------------------------------------

   not overriding
   function E (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Simu_Case('E')'Tag);

   ----------------------------------------------------------------------------

   -- Declare extensions of
   -- 'Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential'. This
   -- is useful here as it makes it possible to use runners with the same
   -- behavior but different tags.

   type Test_Runner_Sequential_B
     is limited
     new Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential
     with null record;

   type Test_Runner_Sequential_C
     is limited
     new Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential
     with null record;

   type Test_Runner_Sequential_D
     is limited
     new Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential
     with null record;

   type Test_Runner_Sequential_E
     is limited
     new Apsepp.Test_Node_Class.Runner_Sequential.Test_Runner_Sequential
     with null record;

   ----------------------------------------------------------------------------

   -- For each of the simulated test cases in 'Simu_Case' except the first one,
   -- create a runner instance (each with a different tag). For the first
   -- element of 'Simu_Case', the runner is created in primitive 'Run_Test' of
   -- 'Apsepp_Testing_System_T_F'.

   Runner_B : aliased Test_Runner_Sequential_B
     := (Apsepp.Test_Node_Class.Runner_Sequential.Create
           (Simu_Case('B')) with null record);

   Runner_C : aliased Test_Runner_Sequential_C
     := (Apsepp.Test_Node_Class.Runner_Sequential.Create
           (Simu_Case('C')) with null record);

   Runner_D : aliased Test_Runner_Sequential_D
     := (Apsepp.Test_Node_Class.Runner_Sequential.Create
           (Simu_Case('D')) with null record);

   Runner_E : aliased Test_Runner_Sequential_E
     := (Apsepp.Test_Node_Class.Runner_Sequential.Create
           (Simu_Case('E')) with null record);

   subtype Simu_Case_Char_Name_Wo_First
     is Simu_Case_Char_Name
     range Simu_Case_Char_Name'Succ (Simu_Case_Char_Name'First)
             ..
           Simu_Case_Char_Name'Last;

   Slave_Runner_Arr : constant Test_Node_Char_Array
     (Simu_Case_Char_Name_Wo_First) := (Runner_B'Access,
                                        Runner_C'Access,
                                        Runner_D'Access,
                                        Runner_E'Access);

   ----------------------------------------------------------------------------

   not overriding
   function A_R (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Test_Runner_Sequential_W_Slave_Nodes'Tag);

      -----------------------------------------------------

   not overriding
   function B_R (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Slave_Runner_Arr('B')'Tag);

      -----------------------------------------------------

   not overriding
   function C_R (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Slave_Runner_Arr('C')'Tag);

      -----------------------------------------------------

   not overriding
   function D_R (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Slave_Runner_Arr('D')'Tag);

      -----------------------------------------------------

   not overriding
   function E_R (Obj : Apsepp_Testing_System_T_F) return Tag
     is (Slave_Runner_Arr('E')'Tag);

   ----------------------------------------------------------------------------

   procedure Assert_Node_Tags is

      Not_All_Desc : constant String
        := " elements are not all descendant of ";

      Dup_Tag : constant String := "Duplicate tag in array ";

      -- 'Short_Short_Integer' would be more appropriate as the actual for
      -- 'Diff_Type', but the compiler may not support it.
      -- REF: ARM 3.5.4(25). <2020-03-18>
      package ISO_646_Upper_Letter_Operations
        is new Apsepp.Generic_Discrete_Operations
        (Discrete_Type => ISO_646_Upper_Letter,
         Diff_Type     => Integer);

      function Tag_Value (X : Test_Node_Access) return Tag
        is (X'Tag);

      function No_Duplicates
        is new ISO_646_Upper_Letter_Operations.Is_Lim_Array_Wo_Dup
        (Element_Type          => Test_Node_Access,
         Array_Type            => Test_Node_Char_Array,
         Element_Func_Ret_Type => Tag,
         Element_Func          => Tag_Value);

      use Apsepp.Test_Node_Class.Runner_Sequential;

   begin

      Ada.Assertions.Assert
        ((for all E of Simu_Case =>
           Is_Descendant_At_Same_Level (E'Tag, Simu_Test_Case'Tag)),
         "Simu_Case" & Not_All_Desc & "Simu_Test_Case");

      Ada.Assertions.Assert (No_Duplicates (Simu_Case), Dup_Tag & "Simu_Case");

      Ada.Assertions.Assert
        ((for all E of Slave_Runner_Arr =>
           Is_Descendant_At_Same_Level(E'Tag, Test_Runner_Sequential'Tag)),
         "Slave_Runner_Arr" & Not_All_Desc & "Test_Runner_Sequential");

      Ada.Assertions.Assert
        (not (for some E of Slave_Runner_Arr =>
               E'Tag = Test_Runner_Sequential_W_Slave_Nodes'Tag),
         "No element in Slave_Runner_Arr should have the tag of "
         & "Test_Runner_Sequential_W_Slave_Nodes");

      Ada.Assertions.Assert (No_Duplicates (Slave_Runner_Arr),
                             Dup_Tag & "Slave_Runner_Arr");

   end Assert_Node_Tags;

   ----------------------------------------------------------------------------

   procedure Assert_Barrier_Status
     (Barrier             : not null access Test_Node_Barrier;
      Expected_Tag_Length : Natural) is

      F : constant Boolean := Barrier.Timed_Out;
      N : constant Natural := Barrier.Cross_Call_Count_On_Time_Out;
      M : constant String  := (if F then
                                        (if N = 0 then
                                            ", no crossing happened"
                                         else
                                            " after crossing ")
                                     else
                                        "");
      Image_N : String := (if F and then N > 0 then
                              Positive'Image (N)
                           else
                              "");

   begin

      if Image_N'Length > 0 then
         Image_N(Image_N'First) := '#';
      end if;
      Ada.Assertions.Assert (not Barrier.Timed_Out,
                             "Barrier timed out" & M & Image_N);

      Ada.Assertions.Assert (not Barrier.Saturated,
                             "Barrier saturated (quite an exploit)");

      Ada.Assertions.Assert (not Barrier.Overflowed, "Barrier overflowed, "
        & "looks like Expected_Tag.all'Length should be greater (than"
        & Integer'Image (Barrier.Cross_Call_Count_On_Overflow) & ")");

      Ada.Assertions.Assert (Barrier.Completed,
        "Expected_Tag'Length is" & Integer'Image (Expected_Tag_Length)
        & (if Barrier.Cross_Call_Count = 0 then
              ", 0 crossing done"
           else
              ", only" & Natural'Image (Barrier.Cross_Call_Count)
              & " crossing(s) done"));

      Ada.Assertions.Assert (not Barrier.Failed_Validation,
         "Failed validation, please analyse trace for details");

   end Assert_Barrier_Status;

   ----------------------------------------------------------------------------

   -- Convert 'Char' to upper case and append "_R" if 'Char' was lower case.
   function Char_Name_Image (Char : ISO_646) return String
     is (" (" & To_Upper (Char) & (if Is_Lower (Char) then
                                      "_R"
                                   else
                                      "") & "): ");

   ----------------------------------------------------------------------------

   function Runner_Tag (Char : ISO_646_Upper_Letter) return Tag
     is (if Char in Simu_Case_Char_Name_Wo_First then
            Slave_Runner_Arr(Char)'Tag
         else
            Test_Runner_Sequential_W_Slave_Nodes'Tag);

   ----------------------------------------------------------------------------

   -- Return the letter corresponding to the tag (converted to lower case if
   -- the tag is a runner tag, not a simulated test case tag).
   function Tag_To_Char (T : Tag) return ISO_646 is

   begin

      -- Loop over elements in 'Simu_Case' (which has 'Simu_Case_Char_Name' as
      -- index range).
      for K in Simu_Case_Char_Name loop

         if Simu_Case(K)'Tag = T then
            return K;            -- Early return.
         elsif Runner_Tag(K) = T then
            return To_Lower (K); -- Early return.
         end if;

      end loop;

      -- We get there only if we haven't found what we were looking for.
      raise Ada.Assertions.Assertion_Error
        with "Unexpected tag: " & (if T = No_Tag then
                                      "No_Tag"
                                   else
                                      Expanded_Name (T));

   end Tag_To_Char;

   ----------------------------------------------------------------------------

   -- Return the simulated test case tag corresponding to the letter.
   function Char_To_Tag (Char : ISO_646_Upper_Letter) return Tag is

   begin

      Ada.Assertions.Assert
        (Char in Simu_Case_Char_Name,
         Char
         & " not in "
         & Simu_Case_Char_Name'First
         & " .. "
         & Simu_Case_Char_Name'Last);

      for K in Simu_Case_Char_Name loop
         if Tag_To_Char (Simu_Case(K)'Tag) = Char then
            return Simu_Case(K)'Tag; -- Early return.
         end if;
      end loop;

      -- We get there only if we haven't found what we were looking for.
      raise Ada.Assertions.Assertion_Error
        with "Cannot find tag corresponding to '" & Char & "'";

   end Char_To_Tag;

   ----------------------------------------------------------------------------

   use Apsepp.Test_Node_Class,
       Apsepp.Test_Node_Class.Abstract_Test_Suite;

   -- Return an array identical to 'A' except that the index is of an integer
   -- type ('Test_Node_Index') (and 1-based) instead of a letter type
   -- ('ISO_646_Upper_Letter').
   function Ind (A : Test_Node_Char_Array) return Test_Node_Array is

      Ret : Test_Node_Array (1 .. A'Length) := (others => A(A'First));

      package ISO_646_Upper_Letter_Operations
        is new Apsepp.Generic_Discrete_Operations
        (Discrete_Type => ISO_646_Upper_Letter,
         Diff_Type     => Test_Node_Count);
      use ISO_646_Upper_Letter_Operations;

   begin

      for K in A'Range loop
         Ret(Rank (K, A'First)) := A(K);
      end loop;

      return Ret;

   end Ind;

   ----------------------------------------------------------------------------

   Debug_Trace : Apsepp.Debug_Trace_Class.Standard.Debug_Trace_Standard;

   Barrier : aliased Test_Node_Barrier;

   Reporter : aliased Test_Reporter_W_Node_Barrier
     := Apsepp.Test_Reporter_Class.W_Node_Barrier.Create
     (Barrier'Access,
      Char_Name_Image'Access,
      Tag_To_Char'Access);

   not overriding
   procedure Run_Test
     (Obj                        : Apsepp_Testing_System_T_F;
      Expected_Tags_Array_Access : not null access Tag_Array;
      Validate_Procedure         : Validate_Proc) is

      Debug_Trace_Lock_Holder : Debug_Trace_Shared_Instance.Holder;

      package Debug_Trace_S_R is new Debug_Trace_Shared_Instance.Finalized_S_R
        (Instance_Access      => Debug_Trace'Access,
         Lock_Holder_Type     => Debug_Trace_Shared_Instance.Holder,
         Lock_Holder_Instance => Debug_Trace_Lock_Holder);

      pragma Unreferenced (Obj, Debug_Trace_S_R);

   begin

      Assert_Node_Tags;

      Barrier.Set_Up (Char_Name_Image_Function   => Char_Name_Image'Access,
                     Tag_To_Char_Function       => Tag_To_Char'Access,
                     Char_To_Tag_Function       => Char_To_Tag'Access,
                     Validate_Procedure         => Validate_Procedure,
                     Expected_Tags_Array_Access => Expected_Tags_Array_Access);
      declare

         Time_Out_Trigger
           : Test_Node_Barrier_Time_Out_Trigger (Barrier'Access);

         Runner_A : Test_Runner_Sequential_W_Slave_Nodes
           := Create (Root_Test_Node_Access         => Simu_Case('A'),
                      Test_Reporter_Instance_Access => Reporter'Access,
                      Slaves                        => Ind (Slave_Runner_Arr));

         Outcome : Apsepp.Test_Node_Class.Test_Outcome;

         pragma Unreferenced (Outcome);

      begin

         Time_Out_Trigger.Set_Up;
         if not Runner_A.Early_Run_Done then
            Runner_A.Early_Run;
         end if;
         Runner_A.Run (Outcome);
         abort Time_Out_Trigger;

      exception

         when others =>
            abort Time_Out_Trigger;

      end;

      Assert_Barrier_Status (Barrier'Access,
                             Expected_Tags_Array_Access'Length);

   end Run_Test;

   ----------------------------------------------------------------------------

end Apsepp_Testing_System_Test_Fixture;
