-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Apsepp.Debug_Trace_Class.Standard;

generic
package Apsepp.Generic_Logical_Array.Assertions_W_Debug_Trace is

   -- TODOC: Stops on first false element. <2020-03-24>
   -- TODOC: Aimed to be used in contract aspects, in replacement of any
   -- '... and then ... and then ...' expression when a debug trace is needed
   -- to know which condition caused the contract failure. <2020-03-24>
   function All_True (A : Logical_Array) return Boolean;

   -- TODOC: Stops on first true element. <2020-03-24>
   -- TODOC: Aimed to be used in contract aspects, in replacement of any
   -- 'not (... or else ... or else ...)' expression when a debug trace is
   -- needed to know which condition caused the contract failure. <2020-03-24>
   function Some_True (A : Logical_Array) return Boolean;

private

   use Apsepp.Debug_Trace_Class.Standard;

   type Debug_Trace_Access is access Debug_Trace_Standard;

end Apsepp.Generic_Logical_Array.Assertions_W_Debug_Trace;
