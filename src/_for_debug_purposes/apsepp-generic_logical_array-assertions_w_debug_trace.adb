-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Strings.Fixed,
     Apsepp.Debug_Trace,
     Apsepp.Generic_Discrete_Operations,
     Apsepp.Generic_Shared_Instance.Finalized_S_R_Dealloc;

package body Apsepp.Generic_Logical_Array.Assertions_W_Debug_Trace is

   type Quantifier is (Quantifier_All, Quantifier_Some);

   ----------------------------------------------------------------------------

   function Parameterized_Quantifier_Func (Q : Quantifier;
                                           A : Logical_Array) return Boolean is

      use Ada.Strings,
          Ada.Strings.Fixed,
          Debug_Trace;

      Initial_Ret : constant Boolean := (case Q is
                                            when Quantifier_All  => True,
                                            when Quantifier_Some => False);

      Ret : Boolean := Initial_Ret;

   begin

      for K in A'Range loop

         Ret := A(K);

         if Ret xor Initial_Ret then

            declare

               package Index_Type_Operations is new Generic_Discrete_Operations
                 (Discrete_Type => Index_Type,
                  Diff_Type     => Integer);
               use Index_Type_Operations;

               Remaining_Count : constant Natural := Diff (K, A'Last);

               Plural_Mark : constant String := (if Remaining_Count > 1 then
                                                    "s"
                                                 else
                                                    "");

               Debug_Trace_Lock_Holder : Debug_Trace_Shared_Instance.Holder;

               Debug_Trace_Instance_Access : constant Debug_Trace_Access
                 := (if Debug_Trace_Lock_Holder.Holds then
                        new Debug_Trace_Standard
                     else
                        null);

               package Debug_Trace_S_R
                 is new Debug_Trace_Shared_Instance.Finalized_S_R_Dealloc
                 (Instance_Access      => Debug_Trace_Instance_Access,
                  Lock_Holder_Type     => Debug_Trace_Shared_Instance.Holder,
                  Lock_Holder_Instance => Debug_Trace_Lock_Holder);

               pragma Unreferenced (Debug_Trace_S_R);

            begin

               Debug_Trace.Debug_Trace.Trace
                 ("Component "
                  & Trim (Index_Type'Image (K), Left)
                  & " is "
                  & (case Q is
                        when Quantifier_All  => "false",
                        when Quantifier_Some => "true")
                  & (if Remaining_Count > 0 then
                        " ("
                        & Trim (Positive'Image (Remaining_Count), Left)
                        & " subsequent component"
                        & Plural_Mark
                        & " not tested)"
                     else
                        ""));

            end;

            exit; -- Early exit.

         end if;

      end loop;

      return Ret;

   end Parameterized_Quantifier_Func;

   ----------------------------------------------------------------------------

   function All_True (A : Logical_Array) return Boolean
     is (Parameterized_Quantifier_Func (Quantifier_All, A));

   ----------------------------------------------------------------------------

   function Some_True (A : Logical_Array) return Boolean
     is (Parameterized_Quantifier_Func (Quantifier_Some, A));

   ----------------------------------------------------------------------------

end Apsepp.Generic_Logical_Array.Assertions_W_Debug_Trace;
