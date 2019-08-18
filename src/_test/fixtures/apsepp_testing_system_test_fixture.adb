-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions,
     Apsepp.Debug_Trace,
     Apsepp.Debug_Trace_Class.Standard,
     Apsepp.Generic_Shared_Instance.Access_Setter,
     Apsepp.Test_Node_Class.Abstract_Simu_Case,
     Apsepp.Test_Node_Class.Runner_Sequential.Create,
     Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create,
     Apsepp_Test_Node_Barrier.Create_Test_Reporter;

package body Apsepp_Testing_System_Test_Fixture is

   use Apsepp.Test_Node_Class.Abstract_Simu_Case,
       Apsepp.Test_Node_Class.Runner_Sequential,
       Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes,
       Apsepp.Test_Node_Class,
       Apsepp_Test_Node_Barrier;

   ----------------------------------------------------------------------------

   function Char_Name_Image (Char : ISO_646) return String
     is (" (" & To_Upper (Char) & (if Is_Lower (Char) then
                                      "_R"
                                   else
                                      "") & "): ");

   ----------------------------------------------------------------------------

   subtype Simu_Case_Char_Name is ISO_646_Upper_Letter range 'A' .. 'E';

   type Test_Node_Char_Array
     is array (ISO_646_Upper_Letter range <>) of Test_Node_Access;

   ----------------------------------------------------------------------------

   type Simu_Test_Case_A is limited new Simu_Test_Case with null record;

      -----------------------------------------------------

   overriding
   function Story (Obj : Simu_Test_Case_A) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_A) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 3),
         (Kind => Test_Assertion_Failure, Successful_Test_Assert_Count => 2),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));

      -----------------------------------------------------

   type Simu_Test_Case_B is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_B) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_B) return Simu_Test_Case_Story
     is ((Kind => Other_Failure,          Successful_Test_Assert_Count => 5),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 2nd test routine not run (unexpected failure on 1st test routine causes
   -- the following test routines to be cancelled).

      -----------------------------------------------------

   type Simu_Test_Case_C is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_C) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_C) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 7),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 6));

      -----------------------------------------------------

   type Simu_Test_Case_D is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_D) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_D) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => Access_Failure,         Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 3rd test routine not run (access failure on 2nd test routine causes the
   -- following test routines to be cancelled).

      -----------------------------------------------------

   type Simu_Test_Case_E is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_E) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_E) return Simu_Test_Case_Story
     is ((Kind => Handled_Test_Failure,   Successful_Test_Assert_Count => 3),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 2),
         (Kind => Contract_Failure,       Successful_Test_Assert_Count => 4),
         (Kind => Setup_Failure,          Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 4th test routine not run (setup failure on 3rd test routine causes the
   -- following test routines to be cancelled).

      -----------------------------------------------------

   Simu_Case_A : aliased Simu_Test_Case_A;
   Simu_Case_B : aliased Simu_Test_Case_B;
   Simu_Case_C : aliased Simu_Test_Case_C;
   Simu_Case_D : aliased Simu_Test_Case_D;
   Simu_Case_E : aliased Simu_Test_Case_E;
   Simu_Case : constant Test_Node_Char_Array (Simu_Case_Char_Name)
     := (Simu_Case_A'Access,
         Simu_Case_B'Access,
         Simu_Case_C'Access,
         Simu_Case_D'Access,
         Simu_Case_E'Access);

   ----------------------------------------------------------------------------

   type Test_Runner_Sequential_B
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_C
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_D
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_E
     is limited new Test_Runner_Sequential with null record;

      -----------------------------------------------------

   -- Runner "A" is created in procedure Run_Test.

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

   Slave_Runner_Arr : constant Test_Node_Char_Array
     (ISO_646_Upper_Letter'Succ (Simu_Case_Char_Name'First)
        ..
      Simu_Case_Char_Name'Last) := (Runner_B'Access,
                                    Runner_C'Access,
                                    Runner_D'Access,
                                    Runner_E'Access);

   ----------------------------------------------------------------------------

   function Runner_Tag (Char : ISO_646_Upper_Letter) return Tag
     is (if Char = Simu_Case_Char_Name'First then
            Test_Runner_Sequential_W_Slave_Tasks'Tag
         else
            Slave_Runner_Arr(Char)'Tag);

   ----------------------------------------------------------------------------

   function Tag_To_Char (T : Tag) return ISO_646 is

      First                           : Boolean := True;
      Found, Loop_Done, Is_Runner_Tag : Boolean := False;
      Ret                             : ISO_646 := Simu_Case'First;

   begin

      -- TODO: Write function Find or Find_First in Generic_Array_Operations.
      -- <2019-06-09>
      while not (Found or else Loop_Done) loop
         Loop_Done := Ret >= Simu_Case'Last;
         if not (Loop_Done or else First) then
            Ret := ISO_646'Succ (Ret);
         end if;
         First := False;
         if Ret <= Simu_Case'Last then
            if Simu_Case(Ret)'Tag = T then
               Found := True;
            elsif Runner_Tag(Ret) = T then
               Found := True;
               Is_Runner_Tag := True;
            end if;
         end if;
      end loop;

      Ada.Assertions.Assert (Found,
                             "Unexpected tag: " & (if T = No_Tag then
                                                      "No_Tag"
                                                   else
                                                      Expanded_Name (T)));

      if Is_Runner_Tag then
         Ret := To_Lower (Ret);
      end if;

      return Ret;

   end Tag_To_Char;

   ----------------------------------------------------------------------------

   function Char_To_Tag (Char : ISO_646_Upper_Letter) return Tag is

      K         : Simu_Case_Char_Name := Simu_Case_Char_Name'First;
      Last_Done : Boolean             := False;

      function Done return Boolean
        is (Last_Done or else Char not in Simu_Case_Char_Name);

   begin

      Ada.Assertions.Assert
        (not Done, Char & " not in " & K & " .. " & Simu_Case_Char_Name'Last);

      while not Done loop

         if Tag_To_Char (Simu_Case(K)'Tag) = Char
              or else
            K = Simu_Case_Char_Name'Last then

            Last_Done := True;

         else
            K := Simu_Case_Char_Name'Succ (K);
         end if;

      end loop;

      Ada.Assertions.Assert
        (Tag_To_Char (Simu_Case(K)'Tag) = Char,
         "Cannot find tag corresponding to '" & Char & "'");

      return Simu_Case(K)'Tag;

   end Char_To_Tag;

   ----------------------------------------------------------------------------

   not overriding
   function A (Obj : Testing_System_Test_Fixture) return Tag
     is (Simu_Case('A')'Tag);

   not overriding
   function B (Obj : Testing_System_Test_Fixture) return Tag
     is (Simu_Case('B')'Tag);

   not overriding
   function C (Obj : Testing_System_Test_Fixture) return Tag
     is (Simu_Case('C')'Tag);

   not overriding
   function D (Obj : Testing_System_Test_Fixture) return Tag
     is (Simu_Case('D')'Tag);

   not overriding
   function E (Obj : Testing_System_Test_Fixture) return Tag
     is (Simu_Case('E')'Tag);

   not overriding
   function A_R (Obj : Testing_System_Test_Fixture) return Tag
     is (Test_Runner_Sequential_W_Slave_Tasks'Tag);

   not overriding
   function B_R (Obj : Testing_System_Test_Fixture) return Tag
     is (Slave_Runner_Arr('B')'Tag);

   not overriding
   function C_R (Obj : Testing_System_Test_Fixture) return Tag
     is (Slave_Runner_Arr('C')'Tag);

   not overriding
   function D_R (Obj : Testing_System_Test_Fixture) return Tag
     is (Slave_Runner_Arr('D')'Tag);

   not overriding
   function E_R (Obj : Testing_System_Test_Fixture) return Tag
     is (Slave_Runner_Arr('E')'Tag);

   ----------------------------------------------------------------------------

   procedure Assert_Node_Tags is

      Not_All_Desc : constant String
        := " elements are not all descendant of ";

      Dup_Tag : constant String := "Duplicate tag in array ";

   begin

      Ada.Assertions.Assert
        ((for all E of Simu_Case =>
           Is_Descendant_At_Same_Level (E'Tag, Simu_Test_Case'Tag)),
         "Simu_Case" & Not_All_Desc & "Simu_Test_Case");

      Ada.Assertions.Assert
        ((for all K_1 in Simu_Case'Range =>
           (for all K_2 in Simu_Case'Range =>
             K_1 = K_2
               or else
             Simu_Case(K_1)'Tag /= Simu_Case(K_2)'Tag)),
         Dup_Tag & "Simu_Case");

      Ada.Assertions.Assert
        ((for all E of Slave_Runner_Arr =>
           Is_Descendant_At_Same_Level (E'Tag, Test_Runner_Sequential'Tag)),
         "Slave_Runner_Arr" & Not_All_Desc & "Test_Runner_Sequential");

      Ada.Assertions.Assert
        (not (for some E of Slave_Runner_Arr =>
               E'Tag = Test_Runner_Sequential_W_Slave_Tasks'Tag),
         "No element in Slave_Runner_Arr should have the tag of "
         & "Test_Runner_Sequential_W_Slave_Tasks");

      Ada.Assertions.Assert
        ((for all K_1 in Slave_Runner_Arr'Range =>
           (for all K_2 in Slave_Runner_Arr'Range =>
             K_1 = K_2
               or else
             Slave_Runner_Arr(K_1)'Tag /= Slave_Runner_Arr(K_2)'Tag)),
         Dup_Tag & "Slave_Runner_Arr");

   end Assert_Node_Tags;

   ----------------------------------------------------------------------------

   procedure Assert_Barrier_Status
     (Barrier             : not null Test_Node_Barrier_Access;
      Expected_Tag_Length :          Natural) is

      F : constant Boolean := Barrier.Timed_Out;
      N : constant Natural := Barrier.Cross_Count_On_Time_Out;
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
        & Integer'Image (Barrier.Cross_Count_On_Overflow) & ")");

      Ada.Assertions.Assert (Barrier.Completed,
        "Expected_Tag'Length is" & Integer'Image (Expected_Tag_Length)
        & (if Barrier.Cross_Count = 0 then
              ", O crossing done"
           else
              ", only" & Natural'Image (Barrier.Cross_Count)
              & " crossing(s) done"));

      Ada.Assertions.Assert (not Barrier.Failed_Validation,
         "Failed validation, please analyse trace for details");

   end Assert_Barrier_Status;

   ----------------------------------------------------------------------------

   function Ind (A : Test_Node_Char_Array) return Test_Node_Array is

      Ret : Test_Node_Array (1 .. A'Length) := (others => A(A'First));
      K   : Test_Node_Count := 0;

   begin

      for E of A loop
         K := K + 1;
         Ret(K) := E;
      end loop;

      return Ret;

   end;

   ----------------------------------------------------------------------------

   Debug_Trace  : Apsepp.Debug_Trace_Class.Standard.Debug_Trace_Standard;
   Barrier      : aliased Test_Node_Barrier;
   Reporter     : aliased Test_Reporter_W_Barrier
     := Create_Test_Reporter (Barrier'Access,
                              Char_Name_Image'Access,
                              Tag_To_Char'Access);

   not overriding
   procedure Run_Test (Obj : Testing_System_Test_Fixture;
                       Exp : Tag_Array_Access;
                       V   : Validate_Proc) is

      package Debug_Trace_Standard_Access_Setter
        is new Apsepp.Debug_Trace.Shared_Instance.Access_Setter
        (Inst_Access => Debug_Trace'Access);

      Unused_Outcome : Test_Outcome;

      pragma Unreferenced (Obj, Debug_Trace_Standard_Access_Setter);

   begin

      Assert_Node_Tags;

      Barrier.Setup (Char_Name_Image'Access,
                     Tag_To_Char'Access,
                     Char_To_Tag'Access,
                     V,
                     Exp);

      declare
         Monitor : Test_Node_Barrier_Monitor;
         Runner_A : Test_Runner_Sequential_W_Slave_Tasks
           := Create (Root_Node_Access              => Simu_Case('A'),
                      Test_Reporter_Instance_Access => Reporter'Access,
                      Slaves                        => Ind (Slave_Runner_Arr));
      begin
         Monitor.Setup (Barrier'Access);
         Runner_A.Run (Unused_Outcome);
         abort Monitor;
      exception
         when others =>
            abort Monitor;
      end;

      Assert_Barrier_Status (Barrier'Access, Exp'Length);

   end Run_Test;

   ----------------------------------------------------------------------------

end Apsepp_Testing_System_Test_Fixture;
