--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Round_90                          Luebeck            --
--  Implementation                                 Winter, 2017       --
--                                                                    --
--                                Last revision :  13:15 14 Sep 2019  --
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

with Ada.Numerics;              use Ada.Numerics;
with Cairo;                     use Cairo;
with Cairo.Ellipses;            use Cairo.Ellipses;
with Cairo.Line_Cap_Property;   use Cairo.Line_Cap_Property;
with Gdk.Color;                 use Gdk.Color;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with GLib.Object.Checked_Destroy;

with Gtk.Widget.Styles.Line_Cap_Property;
use  Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Valve.Round_90 is

   R1 : constant GDouble := 0.36;
   R2 : constant GDouble := 0.39;
   R3 : constant GDouble := 0.40;
   R4 : constant GDouble := 0.43;

   Reflection_X1       : constant := -0.2;
   Reflection_X2       : constant := -0.2;
   Reflection_Y1       : constant := -0.2;
   Reflection_Y2       : constant := -0.2;
   Reflection_Width    : constant := 0.25;
   Reflection_Opacity  : constant := 0.5;

   Reflection_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Cover_Color       : constant Gdk_Color := RGB (0.1, 0.1, 0.1);
   Pin_Color         : constant Gdk_Color := RGB (0.2, 0.2, 0.2);
   Background_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Length : constant GDouble := Pi / 2.0;
   First  : constant GDouble := 5.0 * Pi / 4.0;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Layered.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "needle-on-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Needle on color",
               Blurb      => "The color of the needle's half " &
                             "corresponding to the on state"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "needle-off-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Needle off color",
               Blurb      => "The color of the needle's half " &
                             "corresponding to the off state"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the needle tip",
               Default => CAIRO_LINE_CAP_ROUND
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the needle rear",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "backgound-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Background color",
               Blurb      => "The background color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "major-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Major ticks color",
               Blurb      => "Major ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "major-tick-line-cap",
               Nick    => "Major tick cap",
               Blurb   => "The line cap style used for major ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "minor-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Minor ticks color",
               Blurb      => "Minor ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "minor-tick-line-cap",
               Nick    => "Minor tick cap",
               Blurb   => "The line cap style used for minor ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "pin-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Pin color",
               Blurb      => "Arrow pin color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Create_Background
             (  Widget  : not null access
                          Gtk_Valve_Round_90_Record'Class;
                Sectors : Positive
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Widget.Background :=
         Add_Elliptic_Background
         (  Under  => Widget,
            Color  => Background_Color,
            Outer  => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
            Border_Width  => 0.01,
            Border_Depth  => 0.005,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
   end Create_Background;

   procedure Create_Foreground
             (  Widget  : not null access
                          Gtk_Valve_Round_90_Record'Class;
                Sectors : Positive
             )  is
   begin
      Widget.Upper.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / R1, R1, First),
            Outer   => ((0.0, 0.0), 1.0 / R2, R2, First),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 300.0,
            Skipped => 5,
            Step    => 0.2 * Length / GDouble (Widget.Sectors),
            First   => 5,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Upper.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / R1, R1,  First),
            Outer   => ((0.0, 0.0), 1.0 / R3, R3, First),
            Color   => Major_Tick_Color,
            Width   => 2.0 / 300.0,
            Skipped => Sectors,
            Step    => Length / GDouble (Widget.Sectors),
            First   => Sectors,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Lower.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / R1, R1, First),
            Outer   => ((0.0, 0.0), 1.0 / R2, R2, First),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 300.0,
            Skipped => 5,
            Step    => 0.2 * Length / GDouble (Widget.Sectors),
            First   => 5,
            From    => First - Pi,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Lower.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / R1, R1,  First),
            Outer   => ((0.0, 0.0), 1.0 / R3, R3, First),
            Color   => Major_Tick_Color,
            Width   => 2.0 / 300.0,
            Skipped => Sectors,
            Step    => Length / GDouble (Widget.Sectors),
            First   => Sectors,
            From    => First - Pi,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Left_Cover :=
         Add_Sector_Needle
         (  Under  => Widget.Background.Get_Foreground,
            From   => First - Length,
            Outer  => Unit_Circle / 2.0,
            Center => (0.0, 0.0),
            Length => Length,
            Color  => Cover_Color,
            Scaled => True
         );
      Widget.Left_Cover.Set_Value (1.0);
      Widget.Right_Cover :=
         Add_Sector_Needle
         (  Under  => Widget.Background.Get_Foreground,
            From   => First + Length,
            Outer  => Unit_Circle / 2.0,
            Center => (0.0, 0.0),
            Length => Length,
            Color  => Cover_Color,
            Scaled => True
         );
      Widget.Right_Cover.Set_Value (1.0);
      Widget.Pin :=
         Add_Elliptic_Background
         (  Under         => Widget.Background.Get_Foreground,
            Color         => Pin_Color,
            Outer         => ((0.0, 0.0), 1.0 / 0.09, 0.09, 0.0),
            Border_Shadow => Shadow_Out,
            Scaled        => True
         );
   end Create_Foreground;

   procedure Create_Needle
             (  Widget  : not null access
                          Gtk_Valve_Round_90_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Disk_Needle
         (  Under       => Widget.Background.Get_Foreground,
            Center      => (0.0, 0.0),
            From        => First,
            Radius      => 0.5,
            Length      => Length,
            Sectors     => True,
            On_Color    => Needle_On_Color,
            Off_Color   => Needle_Off_Color,
            Adjustment  => Adjustment,
            Scaled      => True
         );
   end Create_Needle;

   function Get_Annotation
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Upper.Annotation;
   end Get_Annotation;

   function Get_Needle
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Disk_Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Create_Needle (Widget, Adjustment);
      Create_Foreground (Widget, Sectors);
      Widget.Upper.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget,
            Ellipse => ((0.0, 0.0), 1.0 / R4, R4, First),
            Texts   => Texts,
            Face    => Create_Toy
                       (  Family => "arial",
                          Slant  => CAIRO_FONT_SLANT_NORMAL,
                          Weight => CAIRO_FONT_WEIGHT_BOLD
                       ),
            Step    => Length / GDouble (Sectors),
            Height  => 0.05,
            Color   => Text_Color,
            From    => First,
            Length  => Length,
            Mode    => Rotated,
            Scaled  => True
         );
      Widget.Lower.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget,
            Ellipse => ((0.0, 0.0), 1.0 / R4, R4, First + Pi),
            Texts   => Texts,
            Face    => Create_Toy
                       (  Family => "arial",
                          Slant  => CAIRO_FONT_SLANT_NORMAL,
                          Weight => CAIRO_FONT_WEIGHT_BOLD
                       ),
            Step    => Length / GDouble (Sectors),
            Height  => 0.05,
            Color   => Text_Color,
            From    => First + Pi,
            Length  => Length,
            Mode    => Rotated,
            Scaled  => True
         );
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Initialize (Widget, Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Create_Needle (Widget, Adjustment);
      Create_Foreground (Widget, Sectors);
      Widget.Upper.Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget,
            Ellipse   => ((0.0, 0.0), 1.0 / R4, R4, First),
            Texts     => Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => Length / GDouble (Sectors),
            Height    => 0.05,
            Color     => Text_Color,
            From      => First,
            Length    => Length,
            Mode      => Rotated,
            Scaled    => True
         );
      Widget.Lower.Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget,
            Ellipse   => ((0.0, 0.0), 1.0 / R4, R4, First + Pi),
            Texts     => Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => Length / GDouble (Sectors),
            Height    => 0.05,
            Color     => Text_Color,
            From      => First + Pi,
            Length    => Length,
            Mode      => Rotated,
            Scaled    => True
         );
   end Initialize;

   procedure Set_Value
             (  Widget : not null access
                         Gtk_Valve_Round_90_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Valve_Round_90_Record
             )  is
   begin
      Widget.Needle.Set
      (  Center   => Widget.Needle.Get_Center,
         From     => Widget.Needle.Get_From,
         Length   => Widget.Needle.Get_Length,
         Radius   => Widget.Needle.Get_Radius,
         Sectors  => Widget.Needle.Get_Sectors,
         On_Color =>
                Style_Get (Widget, "needle-on_color", Needle_On_Color),
         Off_Color =>
                Style_Get (Widget, "needle-off_color", Needle_Off_Color)
      );
      Widget.Pin.Set
      (  Outer         => Widget.Pin.Get_Outer,
         Inner         => Widget.Pin.Get_Inner,
         From          => Widget.Pin.Get_From,
         Length        => Widget.Pin.Get_Length,
         Border_Width  => Widget.Pin.Get_Border_Width,
         Border_Depth  => Widget.Pin.Get_Border_Depth,
         Border_Color  => Widget.Pin.Get_Border_Color,
         Border_Shadow => Widget.Pin.Get_Border_Shadow,
         Lens_Reflex   => Widget.Pin.Get_Lens_Reflex,
         Lens_Shadow   => Widget.Pin.Get_Lens_Shadow,
         Color         => Style_Get (Widget, "pin-color", Pin_Color)
      );
      Widget.Background.Set
      (  Outer         => Widget.Background.Get_Outer,
         Inner         => Widget.Background.Get_Inner,
         From          => Widget.Background.Get_From,
         Length        => Widget.Background.Get_Length,
         Border_Width  => Widget.Background.Get_Border_Width,
         Border_Depth  => Widget.Background.Get_Border_Depth,
         Border_Color  => Widget.Background.Get_Border_Color,
         Border_Shadow => Widget.Background.Get_Border_Shadow,
         Lens_Reflex   => Widget.Background.Get_Lens_Reflex,
         Lens_Shadow   => Widget.Background.Get_Lens_Shadow,
         Color  =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Widget.Upper.Minor_Ticks.Set
      (  Inner  => Widget.Upper.Minor_Ticks.Get_Inner,
         Outer  => Widget.Upper.Minor_Ticks.Get_Outer,
         Ticks  => Widget.Upper.Minor_Ticks.Get_Ticks,
         From   => Widget.Upper.Minor_Ticks.Get_From,
         Length => Widget.Upper.Minor_Ticks.Get_Length,
         Line =>
            (  Widget.Upper.Minor_Ticks.Get_Line.Width,
               Style_Get (Widget, "minor-tick-color", Minor_Tick_Color),
               Style_Get (Widget, "minor-tick-line-cap")
      )     );
      Widget.Upper.Major_Ticks.Set
      (  Inner  => Widget.Upper.Major_Ticks.Get_Inner,
         Outer  => Widget.Upper.Major_Ticks.Get_Outer,
         Ticks  => Widget.Upper.Major_Ticks.Get_Ticks,
         From   => Widget.Upper.Major_Ticks.Get_From,
         Length => Widget.Upper.Major_Ticks.Get_Length,
         Line =>
            (  Widget.Upper.Major_Ticks.Get_Line.Width,
               Style_Get (Widget, "major-tick-color", Major_Tick_Color),
               Style_Get (Widget, "major-tick-line-cap")
      )     );
      Widget.Upper.Annotation.Set
      (  Ellipse => Widget.Upper.Annotation.Get_Ellipse,
         Ticks   => Widget.Upper.Annotation.Get_Ticks,
         From    => Widget.Upper.Annotation.Get_From,
         Length  => Widget.Upper.Annotation.Get_Length,
         Face    => Widget.Upper.Annotation.Get_Face,
         Mode    => Widget.Upper.Annotation.Get_Mode,
         Height  => Widget.Upper.Annotation.Get_Height,
         Stretch => Widget.Upper.Annotation.Get_Stretch,
         Color   => Style_Get (Widget, "text-color", Text_Color)
      );
      Widget.Lower.Annotation.Set
      (  Ellipse => Widget.Lower.Annotation.Get_Ellipse,
         Ticks   => Widget.Lower.Annotation.Get_Ticks,
         From    => Widget.Lower.Annotation.Get_From,
         Length  => Widget.Lower.Annotation.Get_Length,
         Face    => Widget.Lower.Annotation.Get_Face,
         Mode    => Widget.Lower.Annotation.Get_Mode,
         Height  => Widget.Lower.Annotation.Get_Height,
         Stretch => Widget.Lower.Annotation.Get_Stretch,
         Color   => Style_Get (Widget, "text-color", Text_Color)
      );
   end Style_Changed;

end Gtk.Valve.Round_90;
