--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Main.Router.GNAT_Stack                  Luebeck            --
--  Interface                                      Autumn, 2007       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This  package  provides  tracing  with  symbolick  stack   traceback
--  information provided by the GNAT Ada  compiler.  The  implementation
--  uses the package GNAT.Traceback.Symbolic. See GNAT documentation for
--  the prerequisites of its use.
--
--  The  procedures  declared  in  the  package  are  equivaent to their
--  counterparts from the parent package with addition of  the  symbolic
--  callstack traceback following the text message.
--
with GLib.Messages;  use GLib.Messages;

package Gtk.Main.Router.GNAT_Stack is
--
-- Indent -- Messages trace box
--
--    Message - To show
--    Break   - Force break flag
--    Step    - Identation step per call
--
-- This procedure showns Message in the messages box prefixed by a chain
-- of spaces. The length of the chain is Step multiplied by the caller's
-- depth.
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Indent
             (  Message : UTF8_String;
                Break   : Boolean  := Standard.False;
                Step    : Positive := 2
             );
--
-- Say -- Message box
--
--    Message       - To show
--    Title         - Of the message box
--    Justification - Of the message text
--    Parent        - Of the box
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Say
             (  Message       : UTF8_String;
                Title         : UTF8_String         := "";
                Dialog_Type   : Message_Dialog_Type := Information;
                Justification : Gtk_Justification   := Justify_Left;
                Parent        : Gtk_Window          := null
             );
--
-- Trace -- Messages trace box
--
--    Message - To show
--    Break   - Force break flag
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Trace
             (  Message : UTF8_String;
                Break   : Boolean := Standard.False
             );
--
-- Trace -- Messages trace box
--
--    Error - Exception
--    Break - Force break flag
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Trace
             (  Error : Exception_Occurrence;
                Break : Boolean := Standard.True
             );
--
-- Set_Log_Trace -- GTK+ errors
--
--    Domain - The GTK+ domain log messages to trace
--    Level  - The log messages mask
--
-- This procedure is intended for catching GTK+ log messages. It  causes
-- a messages from the Domain of severity levels indicated by  Level  to
-- be  raced together with the call stack. The application is stopped at
-- the spot. Set_Log_Trace can be called multiple times, the effects  of
-- are  accumulated.   Without  parameters  the  procedure  catches  all
--  messages (which are not catched by other means).
--
   procedure Set_Log_Trace
             (  Domain : String;
                Level  : Log_Level_Flags :=
                            Log_Fatal_Mask or Log_Level_Critical
             );
   procedure Set_Log_Trace;
--
-- Dump_Call_Stack -- Write call stack to the standard output
--
--    Prefix - To the stack trace
--
   procedure Dump_Call_Stack (Prefix : String := "");

end Gtk.Main.Router.GNAT_Stack;
