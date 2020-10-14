--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Main.Router.GNAT_Stack                  Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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

with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;

with Ada.Text_IO;
with GNAT.Most_Recent_Exception;

package body Gtk.Main.Router.GNAT_Stack is

   Traceback_Depth : constant := 1_000;

   procedure Dump_Call_Stack (Prefix : String := "") is
      use Ada.Text_IO, GNAT.Most_Recent_Exception;
      TB  : Tracebacks_Array (1..Traceback_Depth);
      Len : Natural;
   begin
      Call_Chain (TB, Len);
      if Prefix'Length > 0 then
         Put_Line (Prefix);
      end if;
      Put_Line (Symbolic_Traceback (TB (1..Len)));
      if Occurrence_Access /= null then
         Put_Line
         (  "Most recent exception: "
         &  Exception_Information (Occurrence_Access.all)
         );
      end if;
   end Dump_Call_Stack;

   procedure Indent
             (  Message : UTF8_String;
                Break   : Boolean  := Standard.False;
                Step    : Positive := 2
             )  is
      TB  : Tracebacks_Array (1..Traceback_Depth);
      Len : Natural;
   begin
      Call_Chain (TB, Len);
      Len := Integer'Max (0, Len - 1) * Step;
      Gtk.Main.Router.Trace
      (  Message => Len * ' ' & Message,
         Break   => Break
      );
   end Indent;

   procedure Say
             (  Message       : UTF8_String;
                Title         : UTF8_String         := "";
                Dialog_Type   : Message_Dialog_Type := Information;
                Justification : Gtk_Justification   := Justify_Left;
                Parent        : Gtk_Window          := null
             )  is
      TB  : Tracebacks_Array (1..Traceback_Depth);
      Len : Natural;
   begin
      Call_Chain (TB, Len);
      Gtk.Main.Router.Say
      (  Message => Message & LF & Symbolic_Traceback (TB (1..Len)),
         Title         => Title,
         Dialog_Type   => Dialog_Type,
         Justification => Justification,
         Parent        => Parent
      );
   end Say;

   procedure Trace
             (  Message : UTF8_String;
                Break   : Boolean := Standard.False
             )  is
      TB  : Tracebacks_Array (1..Traceback_Depth);
      Len : Natural;
   begin
      Call_Chain (TB, Len);
      Gtk.Main.Router.Trace
      (  Message => Message & LF & Symbolic_Traceback (TB (1..Len)),
         Break   => Break
      );
   end Trace;

   procedure Trace
             (  Error : Exception_Occurrence;
                Break : Boolean := Standard.True
             )  is
   begin
      Gtk.Main.Router.Trace
      (  Message =>
            (  Exception_Information (Error) & LF
            &  Symbolic_Traceback (Error)
            ),
         Break => Break
      );
   end Trace;

   type Log_Function_Ptr is access procedure
        (  Domain  : String;
           Level   : Log_Level_Flags;
           Message : UTF8_String
        );

   procedure Log_Function
             (  Domain  : String;
                Level   : Log_Level_Flags;
                Message : UTF8_String
             )  is
      TB  : Tracebacks_Array (1..Traceback_Depth);
      Len : Natural;
   begin
      Call_Chain (TB, Len);
      Log_Default_Handler (Domain, Level, Message);
      Gtk.Main.Router.Trace
      (  Message =>
            (  Domain & " " & Message & LF
            &  Symbolic_Traceback (TB (1..Len))
            ),
         Break => Standard.True
      );
   end Log_Function;

   procedure Set_Log_Trace
             (  Domain : String;
                Level  : Log_Level_Flags :=
                            Log_Fatal_Mask or Log_Level_Critical
             )  is
      ID : Log_Handler_ID;
   begin
      ID := Log_Set_Handler (Domain, Level, Log_Function'Access);
   end Set_Log_Trace;

   type Log_Func_Ptr is access procedure
        (  Domain  : Chars_Ptr;
           Level   : Log_Level_Flags;
           Message : chars_ptr;
           Handler : Log_Function_Ptr
        );
   pragma Convention (C, Log_Func_Ptr);
   procedure Log_Func
             (  Domain  : Chars_Ptr;
                Level   : Log_Level_Flags;
                Message : chars_ptr;
                Handler : Log_Function_Ptr
             );
   pragma Convention (C, Log_Func);

   procedure Log_Func
             (  Domain  : chars_ptr;
                Level   : Log_Level_Flags;
                Message : chars_ptr;
                Handler : Log_Function_Ptr
             )  is
   begin
      if Domain = Null_Ptr then
         Handler ("", Level, Value (Message));
      else
         Handler (Value (Domain), Level, Value (Message));
      end if;
   end Log_Func;

   procedure Set_Log_Trace is
      function Internal
               (  Func   : Log_Func_Ptr := Log_Func'Access;
                  Data   : Address      := Log_Function'Address
               )  return Log_Handler_ID;
      pragma Import (C, Internal, "g_log_set_default_handler");
      ID : Log_Handler_ID;
   begin
      ID := Internal;
   end Set_Log_Trace;

end Gtk.Main.Router.GNAT_Stack;
