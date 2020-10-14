--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_RSVG                               Luebeck            --
--  Test for Gtk.RSVG                              Winter, 2017       --
--                                                                    --
--                                Last revision :  13:14 14 Sep 2019  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Cairo;                        use Cairo;
with GLib;                         use GLib;
with GLib.Error;                   use GLib.Error;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.File_Chooser;             use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;      use Gtk.File_Chooser_Dialog;
with Gtk.File_Filter;              use Gtk.File_Filter;
with Gtk.Drawing_Area;             use Gtk.Drawing_Area;
with Gtk.Main.Router.GNAT_Stack;   use Gtk.Main.Router;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Window;                   use Gtk.Window;
with Gtk.Widget;                   use Gtk.Widget;
with RSVG.Handle;                  use RSVG.Handle;

with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with GLib.Messages;
with GLib.Object.Checked_Destroy;

procedure Test_Gtk_RSVG is
   Window  : Gtk_Window;
   Area    : Gtk_Drawing_Area;
   Windows : Boolean := False;
   SVG     : RSVG_Handle;

   type Local_Draw is not null access function
        (  Widget  : access Gtk_Widget_Record'Class;
           Context : Cairo.Cairo_Context
        )  return Boolean;
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Draw,
             Cb_Gtk_Widget_Cairo_Context_Boolean
          );

   function Get_File return String is
      Dialog : Gtk_File_Chooser_Dialog;
      Filter : Gtk_File_Filter;
      Cancel : Gtk_Button;
      OK     : Gtk_Button;
   begin
      Dialog :=
         Gtk_File_Chooser_Dialog_New
         (  "Select an SVG file",
            Window,
            Action_Save
         );
      Filter := Gtk_File_Filter_New;
      Filter.Set_Name ("SVG file");
      Filter.Add_Pattern ("*.svg");
      Dialog.Add_Filter (Filter);
      Cancel := Add_Button_From_Stock
                (  Dialog   => Dialog,
                   Response => Gtk_Response_Cancel,
                   Icon     => Stock_Cancel,
                   Label    => "_Cancel",
                   Tip      => "Close the file selection dialog"
                );
      OK :=     Add_Button_From_Stock
                (  Dialog   => Dialog,
                   Response => Gtk_Response_Accept,
                   Icon     => Stock_OK,
                   Label    => "_OK",
                   Tip      => "Use the selected file"
                );
      OK.Set_Can_Default (True);
      Dialog.Show_All;
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      if Gtk_Response_Accept = Dialog.Run then
         declare
            File_Name : constant String := Dialog.Get_Filename;
         begin
            GLib.Object.Checked_Destroy (Dialog);
            return File_Name;
         end;
      else
         GLib.Object.Checked_Destroy (Dialog);
         return "";
      end if;
   end Get_File;

   function Draw
            (  Widget  : access Gtk_Widget_Record'Class;
               Context : Cairo_Context
            )  return Boolean is
   begin
      if SVG = null then
         declare
            File_Name : constant String := Get_File;
            Result    : Create_Result;
            Size      : Dimension_Data;
         begin
            Result := Gtk_New_From_File (File_Name);
            if Result.Success then
               SVG  := Result.Handle;
               Size := SVG.Get_Dimensions;
               Ada.Text_IO.Put_Line
               (  "SVG dimension width ="
               &  Integer'Image (Integer (Size.Width))
               &  ", height ="
               &  Integer'Image (Integer (Size.Height))
               );
               begin
                  Size := SVG.Get_Dimensions_Sub ("#Radiator");
                  Ada.Text_IO.Put_Line
                  (  "SVG object dimension width ="
                  &  Integer'Image (Integer (Size.Width))
                  &  ", height ="
                  &  Integer'Image (Integer (Size.Height))
                  );
               exception
                  when Error : others =>
                     Ada.Text_IO.Put_Line (Exception_Message (Error));
               end;
            else
               Trace
               (  "Error loading SVG: "
               &  Get_Message (Result.Error)
               );
               Error_Free (Result.Error);
            end if;
         end;
      end if;
      if SVG /= null then
         declare
            Size   : constant Dimension_Data := SVG.Get_Dimensions;
            Factor : GDouble;
         begin
            Factor := GDouble'Min
                      (  (  GDouble (Widget.Get_Allocated_Width)
                         /  GDouble (Size.Width)
                         ),
                         (  GDouble (Widget.Get_Allocated_Height)
                         /  GDouble (Size.Height)
                      )  );
            Scale (Context, Factor, Factor);
         end;
         if not SVG.Render_Cairo (Context) then
            Trace ("Error rendering SVG");
         end if;
      end if;
      return False;
   exception
      when Error : others =>
         Trace ("Draw fault: " & Exception_Information (Error));
         return False;
   end Draw;

begin
   Gtk.Main.Init;
   GNAT_Stack.Set_Log_Trace
   (  "",
      GLib.Messages.Log_Level_Flags'Last
   );
   Gtk.Window.Gtk_New (Window);
   Init (Window);
   Set_Title (Window, "Test RSVG");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);


   Gtk_New (Area);
   Add (Window, Area);
   Area.On_Draw (+Draw'Access);

   Show_All (Area);
   Show (Window);

   Gtk.Main.Main;
   if SVG /= null then
      SVG.Unref;
   end if;
exception
   when Error : others =>
      Trace ("Error: " & Exception_Information (Error));
end Test_Gtk_RSVG;
