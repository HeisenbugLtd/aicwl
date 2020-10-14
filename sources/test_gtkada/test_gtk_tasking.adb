--                                                                    --
--  procedure Test_Gtk_Tasking      Copyright (c)  Dmitry A. Kazakov  --
--  Test for Gtk.Main.Router                       Luebeck            --
--                                                 Spring, 2006       --
--                                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Gtk.Box;              use Gtk.Box;
with Gtk.Label;            use Gtk.Label;
with Gtk.Main.Router;      use Gtk.Main.Router;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Text_View;        use Gtk.Text_View;
with Gtk.Window;           use Gtk.Window;
with Gtk.Widget;           use Gtk.Widget;

with Ada.Unchecked_Conversion;
with Gtk.Missed;

procedure Test_Gtk_Tasking is
   --
   -- All data are global, for the sake of  simplicity
   --
   Window  : Gtk_Window;
   Box     : Gtk_VBox;
   Label   : Gtk_Label;
   View    : Gtk_Text_View;
   Scroll  : Gtk_Scrolled_Window;
   Counter : Integer;

   -- Circumvention of access rules, don't do it, it is here only to
   -- simplify the test
   type Local_Callback is access procedure;
   function "+" is
      new Ada.Unchecked_Conversion (Local_Callback, Gtk_Callback);

   task type Process;

   -- Update will write the label
   procedure Update is
   begin
      Label.Set_Text ("Counter" & Integer'Image (Counter));
   end Update;

   -- The task that calls to Update
   task body Process is
   begin
      for Index in Positive'Range loop
         delay 0.5;
         Counter := Index;
         Request (+Update'Access); -- Request execution of Update
      end loop;
   exception
      when Quit_Error => -- Main loop was quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error)); -- This is safe
   end Process;

   task type Flood;
   package Messages is new Generic_Message (Integer);

   procedure Handler (Data : in out Integer) is
      Buffer : constant Gtk_Text_Buffer := View.Get_Buffer;
      End_Of : Gtk_Text_Iter;
   begin
      Buffer.Get_End_Iter (End_Of);
      Buffer.Insert
      (  End_Of,
         (  "Counter"
         &  Integer'Image (Data)
         &  " statistic "
         &  Get_Request_Info
         &  Character'Val (10)
      )  );
   end Handler;

   -- The task that calls to Update
   task body Flood is
   begin
      for Index in Positive'Range loop
         delay 0.01;
         Messages.Send (Handler'Access, Index);
      end loop;
   exception
      when Quit_Error => -- Main loop was quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error)); -- This is safe
   end Flood;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window); -- This must be called once
   Window.Set_Title ("Test Tasking");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Box := Gtk_VBox_New;
   Window.Add (Box);
   Gtk_New (Label, "label");
   Box.Pack_Start (Child => Label, Expand => False, Fill => False);
   Gtk_New (View);
   Gtk_New (Scroll);
   Scroll.Add (View);
   Box.Pack_Start (Child => Scroll, Expand => True, Fill => True);
   Set_Max_Asynchronous (10);

   Box.Show_All;
   Window.Show;
   declare
      Worker : Process; -- Now the task is on
      Spamer : Flood;
   begin
      -- Enter the events processing loop
      Gtk.Main.Main;
   end;
exception
   when Error : others =>
      Say (Exception_Information (Error)); -- This is safe
end Test_Gtk_Tasking;
