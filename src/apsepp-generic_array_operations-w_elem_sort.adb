-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Generic_Array_Operations.W_Elem_Sort is

   ----------------------------------------------------------------------------

   generic

      Reverse_Loop : Boolean := False; -- False if the insert index search
                                       -- strategy should start the search at
                                       -- lower indices, true otherwise.

      -- Return true if 'Candidate_Insert_Index' is the proper insetion index.
      -- Make sure to provide an implementation consistent with 'Reverse_Loop'
      -- value.
      with function Pre_Loop_Crit
        (A                      : Array_Type;   -- Array.
         Max_Insert_Index       : Index_Type;   -- First "free" element in the
                                                -- array.
         Elem                   : Element_Type; -- Element to be inserted.
         Candidate_Insert_Index : Index_Type)   -- Candidate value for
                                                -- insertion index (actual will
                                                -- be 'A'First' (if
                                                -- 'not Reverse_Loop') or
                                                -- 'Max_Insert_Index' (if
                                                -- 'Reverse_Loop')).
        return Boolean;

      -- Return true if 'Candidate_Insert_Index' is the proper insetion index.
      -- Make sure to provide an implementation consistent with 'Reverse_Loop'
      -- value.
      with function Loop_Exit_Crit
        (A                      : Array_Type;   -- Array.
         Max_Insert_Index       : Index_Type;   -- First "free" element in the
                                                -- array.
         Elem                   : Element_Type; -- Element to be inserted.
         Candidate_Insert_Index : Index_Type)   -- Candidate value for
                                                -- insertion index (actual
                                                -- will be the successors (if
                                                -- 'not Reverse_Loop') or the
                                                -- predecessors (if
                                                -- 'Reverse_Loop') to the
                                                -- value provided to
                                                -- 'Pre_Loop_Crit', until
                                                -- the function returns true.
        return Boolean;

   function Generic_Insert_Incr_Index
     (A                : Array_Type;
      Max_Insert_Index : Index_Type;
      Elem             : Element_Type) return Index_Type;

      -----------------------------------------------------

   function Generic_Insert_Incr_Index
     (A                : Array_Type;
      Max_Insert_Index : Index_Type;
      Elem             : Element_Type) return Index_Type is

      Ret : Index_Type := (if Reverse_Loop then
                              Max_Insert_Index
                           else
                              A'First);

   begin

      -- TODO: Write a faster implementation (dichotomic search). <2020-03-12>

      if not Pre_Loop_Crit (A, Max_Insert_Index, Elem, Ret) then

         if Reverse_Loop then

            Ret := A'First; -- Return value if the loop below is not exited
                            -- early.
            for K in reverse First_Succ (A)
                               ..
                             Index_Type'Pred (Max_Insert_Index) loop
               if Loop_Exit_Crit (A, Max_Insert_Index, Elem, K) then
                  Ret := K;
                  exit; -- Early exit.
               end if;
            end loop;

         else

            Ret := Max_Insert_Index; -- Return value if the loop below is not
                                     -- exited early.
            for K in First_Succ (A)
                       ..
                     Index_Type'Pred (Max_Insert_Index) loop
               if Loop_Exit_Crit (A, Max_Insert_Index, Elem, K) then
                  Ret := K;
                  exit; -- Early exit.
               end if;
            end loop;

         end if;

      end if;

      return Ret;

   end Generic_Insert_Incr_Index;

   ----------------------------------------------------------------------------

   procedure Insert_Incr (A                : in out Array_Type;
                          Max_Insert_Index :        Index_Type;
                          Elem             :        Element_Type;
                          Search_From_Last :        Boolean      := False) is

      function Pre_Loop_Crit_Search_From_Last
        (A                      : Array_Type;
         Max_Insert_Index       : Index_Type;
         Elem                   : Element_Type;
         Candidate_Insert_Index : Index_Type) return Boolean
        is (Max_Insert_Index = A'First
              or else
            (
              not (Elem < A(Index_Type'Pred (Candidate_Insert_Index))
                     or else
                   Elem = A(Index_Type'Pred (Candidate_Insert_Index)))
            ));

      function Loop_Exit_Crit_Search_From_Last
        (A                      : Array_Type;
         Max_Insert_Index       : Index_Type;
         Elem                   : Element_Type;
         Candidate_Insert_Index : Index_Type) return Boolean
        is (not Pre_Loop_Crit_Search_From_Last
                  (A,
                   Max_Insert_Index,
                   Elem,
                   Index_Type'Pred (Candidate_Insert_Index))
              and then
            Pre_Loop_Crit_Search_From_Last (A,
                                            Max_Insert_Index,
                                            Elem,
                                            Candidate_Insert_Index));

      function Pre_Loop_Crit_Search_From_First
        (A                      : Array_Type;
         Max_Insert_Index       : Index_Type;
         Elem                   : Element_Type;
         Candidate_Insert_Index : Index_Type) return Boolean
        is (Max_Insert_Index = A'First
              or else
            (
              Elem < A(Candidate_Insert_Index)
                or else
              Elem = A(Candidate_Insert_Index)
            ));

      function Loop_Exit_Crit_Search_From_First
        (A                      : Array_Type;
         Max_Insert_Index       : Index_Type;
         Elem                   : Element_Type;
         Candidate_Insert_Index : Index_Type) return Boolean
        is (not Pre_Loop_Crit_Search_From_First
                  (A,
                   Max_Insert_Index,
                   Elem,
                   Index_Type'Pred (Candidate_Insert_Index))
              and then
            Pre_Loop_Crit_Search_From_First (A,
                                             Max_Insert_Index,
                                             Elem,
                                             Candidate_Insert_Index));

      function Insert_Incr_Index_Search_From_Last
        is new Generic_Insert_Incr_Index
        (Reverse_Loop   => Search_From_Last,
         Pre_Loop_Crit  => Pre_Loop_Crit_Search_From_Last,
         Loop_Exit_Crit => Loop_Exit_Crit_Search_From_Last);

      function Insert_Incr_Index_Search_From_First
        is new Generic_Insert_Incr_Index
        (Reverse_Loop   => not Search_From_Last,
         Pre_Loop_Crit  => Pre_Loop_Crit_Search_From_First,
         Loop_Exit_Crit => Loop_Exit_Crit_Search_From_First);

      type Insert_Incr_Index_Func
        is access function (A                : Array_Type;
                            Max_Insert_Index : Index_Type;
                            Elem             : Element_Type) return Index_Type;

      Insert_Incr_Index_Function : Insert_Incr_Index_Func;

      Insert_Index : Index_Type;

   begin

      -- Select the appropriate 'Insert_Incr_Index' function.
      if Search_From_Last then
         Insert_Incr_Index_Function
           := Insert_Incr_Index_Search_From_Last'Access;
      else
         Insert_Incr_Index_Function
           := Insert_Incr_Index_Search_From_First'Access;
      end if;

      -- Compute insertion index.
      Insert_Index := Insert_Incr_Index_Function (A,
                                                  Max_Insert_Index,
                                                  Elem);

      -- Shift elements with index above insetion index.
      A(Index_Type'Succ (Insert_Index) .. Max_Insert_Index)
        := A(Insert_Index .. Index_Type'Pred (Max_Insert_Index));

      -- Assign inserted element.
      A(Insert_Index) := Elem;

   end Insert_Incr;

   ----------------------------------------------------------------------------

end Apsepp.Generic_Array_Operations.W_Elem_Sort;
