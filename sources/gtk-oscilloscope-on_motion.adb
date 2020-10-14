--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Motion                                Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  22:46 07 Apr 2016  --
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
   function On_Motion
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                X1, X2      : GInt;
                V1, V2, V   : GDouble
             )  is
      use Strings_Edit;
      use Edit;
   begin
      if X2 <= X1 or else V2 <= V1 or else V = 0.0 then
         Put (Destination, Pointer, V);
      else
         declare
            Power : constant Integer :=
                       (  Integer
                          (  GDouble'Truncation
                             (  Log (GDouble (abs V), 10.0)
                             /  3.0
                          )  )
                       *  3
                       );
         begin
            if Power = 0 then
               Put
               (  Destination => Destination,
                  Pointer     => Pointer,
                  Value       => V,
                  RelSmall    => 3
               );
            else
               Put
               (  Destination => Destination,
                  Pointer     => Pointer,
                  Value       => V / 10.0 ** Power,
                  RelSmall    => 3
               );
               Put (Destination, Pointer, Character'Val (16#C2#));
               Put (Destination, Pointer, Character'Val (16#B7#));
               Put (Destination, Pointer, "10<sup>");
               Put
               (  Destination => Destination,
                  Pointer     => Pointer,
                  Value       => Power
               );
               Put (Destination, Pointer, "</sup>");
            end if;
         end;
      end if;
   end Put;
   Box     : constant Cairo_Box := Oscilloscope.Get_Box;
   Point   : constant Cairo_Tuple :=
             Oscilloscope.Mouse_Event (Event, True);
   Pointer : Integer := 1;
begin
   if Oscilloscope.Selection.Area /= null then
      Oscilloscope.Change_Selection (Point);
   end if;
   if Point.X in Box.X1..Box.X2 and then Point.Y in Box.Y1..Box.Y2 then
      for Index in 1..Oscilloscope.Channels_Number loop
         declare
            use Strings_Edit;
            use Strings_Edit.Integers;
            Waveform : Waveform_Layer renames
                       Oscilloscope.Channels (Index).Waveform.all;
            T        : X_Axis;
            V        : Y_Axis;
            Y        : GDouble;
            Color    : Gdk_Color;
            Got_It   : Boolean;
            Position : Integer;
         begin
            Waveform.Get_Point (Point.X, T, V, Got_It);
            if Got_It then
               Y := Waveform.Get_Y (V);
               if abs (Point.Y - Y) <= Oscilloscope.Proximity then
                  Position := Pointer;
                  if Position > 1 then
                     Put
                     (  Oscilloscope.Tip_Text,
                        Position,
                        Character'Val (10)
                     );
                  end if;
                  Color := Waveform.Get_Line.Color;
                  Put
                  (  Oscilloscope.Tip_Text,
                     Position,
                     "<span background='#"
                  );
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     Value       => Integer (Red (Color) / 256),
                     Base        => 16,
                     Field       => 2,
                     Fill        => '0',
                     Justify     => Right
                  );
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     Value       => Integer (Green (Color) / 256),
                     Base        => 16,
                     Field       => 2,
                     Fill        => '0',
                     Justify     => Right
                  );
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     Value       => Integer (Blue (Color) / 256),
                     Base        => 16,
                     Field       => 2,
                     Fill        => '0',
                     Justify     => Right
                  );
                  Put (Oscilloscope.Tip_Text, Position, "'>  </span> ");
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     Value       =>
                        +Oscilloscope.Channels (Index).Tip_Prefix
                  );
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     X1          => GInt (Box.Y1),
                     X2          => GInt (Box.Y2),
                     V1          => GDouble (Waveform.Get_V2),
                     V2          => GDouble (Waveform.Get_V1),
                     V           => GDouble (V)
                  );
                  Put
                  (  Destination => Oscilloscope.Tip_Text,
                     Pointer     => Position,
                     Value       =>
                        +Oscilloscope.Channels (Index).Tip_Y_Suffix
                  );
                  if Oscilloscope.Show_Time then
                     declare
                        Sweeper : Time_Axis_Data renames
                                  Oscilloscope.Time_Axis
                                  (  Oscilloscope.Get_Sweeper (Index)
                                  );
                     begin
                        Put (Oscilloscope.Tip_Text, Position, " at ");
                        if Sweeper.Time_Mode then
                           Put
                           (  Oscilloscope.Tip_Text,
                              Position,
                              Gtk.Layered.Graph_Paper_Annotation.Image
                              (  To_Time (GDouble (T))
                           )  );
                        else
                           Put
                           (  Destination => Oscilloscope.Tip_Text,
                              Pointer     => Position,
                              X1          => GInt (Box.X1),
                              X2          => GInt (Box.X2),
                              V1          => GDouble (Waveform.Get_T1),
                              V2          => GDouble (Waveform.Get_T2),
                              V           => GDouble (T)
                           );
                        end if;
                        Put
                        (  Destination => Oscilloscope.Tip_Text,
                           Pointer     => Position,
                           Value       =>
                             +Oscilloscope.Channels (Index).Tip_X_Suffix
                        );
                     end;
                  end if;
                  Pointer := Position;
               end if;
            end if;
         exception
            when others =>
               null;
         end;
      end loop;
   end if;
   Oscilloscope.Set_Tooltip_Markup
   (  Oscilloscope.Tip_Text (1..Pointer - 1)
   );
   return True;
exception
   when Error : others =>
      Log
      (  GtkAda_Contributions_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Motion")
      )  );
      return True;
end On_Motion;
