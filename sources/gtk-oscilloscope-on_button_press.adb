--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Button_Press                          Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  15:58 22 Jan 2012  --
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

separate (Gtk.Oscilloscope)
   function On_Button_Press
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is
   use Menu_Handlers;
   Menu : Gtk_Menu;
   Item : Gtk_Image_Menu_Item;
   Icon : Gtk_Image;

   procedure Save is
      First : Boolean := True;
   begin
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Save_Amplifier (Group, First);
         Oscilloscope.Set_Auto_Scaling (Group, False);
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Save_Sweeper (Index, First);
         Oscilloscope.Set_Frozen (Index, True);
      end loop;
      Oscilloscope.Selection.Saved := True;
   end Save;
begin
   case Get_Button (Event) is
      when 1 =>
         if Oscilloscope.Selection.Area = null then
            Oscilloscope.Selection.Engaged := True;
            declare
               Box   : Cairo_Box := Oscilloscope.Get_Box;
               Point : Cairo_Tuple :=
                          Oscilloscope.Mouse_Event (Event, False);
            begin
               if Point.X in Box.X1..Box.X2 and then
                  Point.Y in Box.Y1..Box.Y2
               then
                  Oscilloscope.Selection.Area :=
                     Add_Rectangle
                     (  Under => Oscilloscope.Layers,
                        Box   => (  X1 => Point.X,
                                    X2 => Point.X,
                                    Y1 => Point.Y,
                                    Y2 => Point.Y
                                 ),
                        Line_Width => 1.0,
                        Opacity    => 0.0,
                        Color      => Style_Get
                                      (  Oscilloscope,
                                         "selection-color",
                                         Selection_Color
                     )                ) .all'Unchecked_Access;
                  Oscilloscope.Selection.Right := True;
                  Oscilloscope.Selection.Below := True;
                  Save;
               end if;
            end;
            Oscilloscope.Selection.Engaged := False;
         else
            Oscilloscope.Change_Selection
            (  Oscilloscope.Mouse_Event (Event, False)
            );
         end if;
      when 3 =>
         Gtk_New (Menu);
         if Oscilloscope.Manual_Sweep then
            for Sweeper in Oscilloscope.Time_Axis'Range loop
               if Oscilloscope.Get_Frozen (Sweeper) then
                  -- Add release button
                  Gtk_New
                  (  Item,
                     Style_Get (Oscilloscope, "menu-release")
                  );
                  Gtk_New (Icon, Stock_Media_Play, Icon_Size_Menu);
                  Set_Image (Item, Icon);
                  Append (Menu, Item);
                  Connect
                  (  Item,
                     "activate",
                     On_Release'Access,
                     Oscilloscope
                  );
                  exit;
               end if;
            end loop;
            for Sweeper in Oscilloscope.Time_Axis'Range loop
               if not Oscilloscope.Get_Frozen (Sweeper) then
                  -- Add hold button
                  Gtk_New
                  (  Item,
                     Style_Get (Oscilloscope, "menu-pause")
                  );
                  Gtk_New (Icon, Stock_Media_Pause, Icon_Size_Menu);
                  Set_Image (Item, Icon);
                  Append (Menu, Item);
                  Connect
                  (  Item,
                     "activate",
                     On_Pause'Access,
                     Oscilloscope
                  );
                  exit;
               end if;
            end loop;
         end if;
         -- Add latest
         Gtk_New
         (  Item,
            Style_Get (Oscilloscope, "menu-latest")
         );
         Gtk_New (Icon, Stock_Media_Forward, Icon_Size_Menu);
         Set_Image (Item, Icon);
         Append (Menu, Item);
         Connect
         (  Item,
            "activate",
            On_Latest'Access,
            Oscilloscope
         );
         -- Add undo
         if Oscilloscope.Undo_Stack /= null then
            Gtk_New
            (  Item,
               Style_Get (Oscilloscope, "menu-undo")
            );
            Gtk_New (Icon, Stock_Undo, Icon_Size_Menu);
            Set_Image (Item, Icon);
            Append (Menu, Item);
            Connect
            (  Item,
               "activate",
               On_Undo'Access,
               Oscilloscope
            );
         end if;
         -- Add redo
         if Oscilloscope.Redo_Stack /= null then
            Gtk_New
            (  Item,
               Style_Get (Oscilloscope, "menu-redo")
            );
            Gtk_New (Icon, Stock_Redo, Icon_Size_Menu);
            Set_Image (Item, Icon);
            Append (Menu, Item);
            Connect
            (  Item,
               "activate",
               On_Redo'Access,
               Oscilloscope
            );
         end if;
         -- Add toggle grid
         Gtk_New
         (  Item,
            Style_Get (Oscilloscope, "menu-toggle-grid")
         );
         Gtk_New (Icon, Stock_Index, Icon_Size_Menu);
         Set_Image (Item, Icon);
         Append (Menu, Item);
         Connect
         (  Item,
            "activate",
            On_Toggle_Grid'Access,
            Oscilloscope
         );
         -- Add toggle interpolation
         Gtk_New
         (  Item,
            Style_Get (Oscilloscope, "menu-toggle-interpolation")
         );
         Gtk_New (Icon, Stock_Italic, Icon_Size_Menu);
         Set_Image (Item, Icon);
         Append (Menu, Item);
         Connect
         (  Item,
            "activate",
            On_Toggle_Interpolation'Access,
            Oscilloscope
         );
         if (  Oscilloscope.Format /= No_Snapshot
            and then
               Oscilloscope.File /= null
            and then
               Oscilloscope.File'Length > 0
            )
         then -- Add snapshot button
            Gtk_New
            (  Item,
               (  Style_Get (Oscilloscope, "menu-snapshot")
               &  " to "
               &  Oscilloscope.File.all
            )  );
            Gtk_New (Icon, Stock_Save, Icon_Size_Menu);
            Set_Image (Item, Icon);
            Append (Menu, Item);
            Connect
            (  Item,
               "activate",
               On_Snapshot'Access,
               Oscilloscope
            );
         end if;
         Show_All (Menu);
         Popup
         (  Menu,
            Button => Gdk.Event.Get_Button (Event),
            Activate_Time => Gdk.Event.Get_Time (Event)
         );
      when others =>
         null;
   end case;
   return True;
exception
   when Error : others =>
      Log
      (  GtkAda_Contributions_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Button_Press")
      )  );
      return True;
end On_Button_Press;
