-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Scope_Debug;

package body Apsepp_Demo_DT_Scope_Debug_User is

   ----------------------------------------------------------------------------

   procedure Scope_Debug_User_1 is

      use Apsepp.Scope_Debug; -- Makes type
                              -- Apsepp.Scope_Debug.Controlled_Debug_Tracer
                              -- visible.

      -- Create an instance of the Controlled_Debug_Tracer controlled type.
      --
      -- The use of the Create construtor function implies that both an "Entry"
      -- trace and an "Exit" trace are displayed when entering and exiting the
      -- scope. Other constructor functions are:
      --
      -- - Create_A (equivalent to Create ("A" stands for "all")).
      --
      -- - Create_I (outputs "Entry" trace only ("I" stands for
      --   "initialization")).
      --
      -- - Create_F (outputs "Exit" trace only ("F" stands for
      --   "finalization")).
      --
      -- - Create_N (Does not output any trace ("N" stands for "none")).
      C_D_T : Controlled_Debug_Tracer
        := Create ("Apsepp_Demo_DT_Scope_Debug_User.Scope_Debug_User_1");

   begin
      -- An "Entry" trace is displayed by C_D_T.

      -- Trace primitive of Controlled_Debug_Tracer is a wrapper around the
      -- Trace primitive operation of debug trace instance.
      C_D_T.Trace ("Calling Scope_Debug_User_2");

      Scope_Debug_User_2;

      -- An "Exit" trace is displayed by C_D_T.
   end Scope_Debug_User_1;

   ----------------------------------------------------------------------------

   procedure Scope_Debug_User_2 is

      use Apsepp.Scope_Debug;

      C_D_T : Controlled_Debug_Tracer
        := Create ("Apsepp_Demo_DT_Scope_Debug_User.Scope_Debug_User_2");

      pragma Unreferenced (C_D_T);

   begin

      null; -- Nothing done, only the automatic "Entry" and "Exit" traces of
            -- C_D_T are displayed.

   end Scope_Debug_User_2;

   ----------------------------------------------------------------------------

end Apsepp_Demo_DT_Scope_Debug_User;
