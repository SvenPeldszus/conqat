------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                            F M . L O C K S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $                              --
--                                                                          --
--           Copyright (C) 1995-1998 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an array of locks to be used for multiple synchro-
--  nization. Typically each lock provides barrier synchronization for the
--  iterates of one loop. It is unlikely that more that two locks would ever
--  be used concurrently, but this model will be just as usable on an SP2
--  with 2000 nodes as on a dual-processor SPARC.

private package FM.Locks is
   type Lock_Type  is array (Integer range <>) of Integer;
   type Flags      is array (Integer range <>) of Boolean;

   procedure Inc (Synch : Integer; Val : Integer := 1);

   protected Lock_Manager is
      procedure Get_Synch (Synch : in out Integer);
      entry Wait (I : Integer);
      procedure Inc (Synch : Integer; Val : Integer);
      entry Lock;
      procedure Unlock;
   private

      Locks       : Lock_Type (1.. NB_Sync) := (others => 0);
      Available   : Flags (1 .. NB_Sync)    := (others => True);
      Global_Lock : Boolean := False;

      entry  Barrier (1 .. NB_Sync);
   end Lock_Manager;
end FM.Locks;
