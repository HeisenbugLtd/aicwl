--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Disk_Needle                   Luebeck            --
--  Implementation                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  19:07 02 Jan 2018  --
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

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Disk_Needle is

   type Needle_Ptr is access all Disk_Needle_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Center_X,
           Property_Center_Y,
           Property_Radius,
           Property_From,
           Property_Length,
           Property_Sectors,
           Property_On_Color,
           Property_Off_Color,
           Property_Value
        );

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Needle_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Disk_Needle_Layer, Needle_Ptr);

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle     : Needle_Ptr
             );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Disk_Needle_Layer is
      Ptr : Needle_Ptr := new Disk_Needle_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Adjustment
             (  Layer      : in out Disk_Needle_Layer;
                Adjustment : not null access Gtk_Adjustment_Record'Class
             )  is
   begin
      Ref (Adjustment);
      Layer.Adjustment := Adjustment.all'Unchecked_Access;
      Layer.Changed :=
         Handlers.Connect
         (  Adjustment,
            "changed",
            Handlers.To_Marshaller (Changed'Access),
            Layer'Unchecked_Access
         );
      Layer.Value_Changed :=
         Handlers.Connect
         (  Adjustment,
            "value_changed",
            Handlers.To_Marshaller (Changed'Access),
            Layer'Unchecked_Access
         );
      declare
         Lower : constant GDouble := Adjustment.Get_Lower;
         Upper : constant GDouble := Adjustment.Get_Upper;
         Value : constant GDouble := Adjustment.Get_Value;
      begin
         if Upper <= Lower or else Value <= Lower then
            Layer.Set_Value (0.0);
         elsif Value >= Upper then
            Layer.Set_Value (1.0);
         else
            Layer.Set_Value ((Value - Lower) / (Upper - Lower));
         end if;
      end;
   end Add_Adjustment;

   procedure Add_Disk_Needle
             (  Under      : not null access Layer_Location'Class;
                Center     : Cairo_Tuple := (0.0, 0.0);
                Radius     : GDouble     := 0.5;
                From       : GDouble     := Pi;
                Length     : GDouble     := Pi;
                Sectors    : Boolean     := False;
                On_Color   : Gdk_Color   := Needle_On_Color;
                Off_Color  : Gdk_Color   := Needle_Off_Color;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean     := False
             )  is
      Ptr   : Needle_Ptr := new Disk_Needle_Layer;
      Layer : Disk_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Disk_Needle;

   function Add_Disk_Needle
            (  Under      : not null access Layer_Location'Class;
               Center     : Cairo_Tuple := (0.0, 0.0);
               Radius     : GDouble     := 0.5;
               From       : GDouble     := Pi;
               Length     : GDouble     := Pi;
               Sectors    : Boolean     := False;
               On_Color   : Gdk_Color   := Needle_On_Color;
               Off_Color  : Gdk_Color   := Needle_Off_Color;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean     := False
            )  return not null access Disk_Needle_Layer is
      Ptr   : Needle_Ptr := new Disk_Needle_Layer;
      Layer : Disk_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Disk_Needle;

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle      : Needle_Ptr
             )  is
      Lower : constant GDouble := Get_Lower (Needle.Adjustment);
      Upper : constant GDouble := Get_Upper (Needle.Adjustment);
      Value : constant GDouble := Get_Value (Needle.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Needle.Set_Value (0.0);
      elsif Value >= Upper then
         Needle.Set_Value (1.0);
      else
         Needle.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Needle.Widget.Drawing and then Needle.Updated then
         Queue_Draw (Needle.Widget);  -- Signal draw to the widget
      end if;
   end Changed;

   procedure Draw
             (  Layer   : in out Disk_Needle_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      procedure Do_Half
                (  Center : Cairo_Tuple;
                   Radius : GDouble;
                   From   : GDouble;
                   Color : Gdk_Color
                )  is
         X : aliased GDouble;
         Y : aliased GDouble;
      begin
         New_Path (Context);
         Arc
         (  Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From,
            Angle2 => From + Pi
         );
         Get_Current_Point (Context, X'Access, Y'Access);
         Line_To
         (  Cr => Context,
            X  => Center.X - Radius * cos (From),
            Y  => Center.Y - Radius * sin (From)
         );
         Close_Path (Context);
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (Guint16'Last),
            GDouble (Green (Color)) / GDouble (Guint16'Last),
            GDouble (Blue  (Color)) / GDouble (Guint16'Last)
         );
         Fill (Context);
      end Do_Half;

      procedure Do_Sector
                (  Center : Cairo_Tuple;
                   Radius : GDouble;
                   From   : GDouble;
                   Color : Gdk_Color
                )  is
      begin
         New_Path (Context);
         Move_To
         (  Cr => Context,
            X  => Center.X,
            Y  => Center.Y
         );
         Arc
         (  Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From,
            Angle2 => From + Pi / 2.0
         );
         Arc_Negative
         (  Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From + Pi + Pi / 2.0,
            Angle2 => From + Pi
         );
         Line_To
         (  Cr => Context,
            X  => Center.X,
            Y  => Center.Y
         );
         Close_Path (Context);
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (Guint16'Last),
            GDouble (Green (Color)) / GDouble (Guint16'Last),
            GDouble (Blue  (Color)) / GDouble (Guint16'Last)
         );
         Fill (Context);
      end Do_Sector;

      Start : constant GDouble :=
                       Layer.From + Layer.Length * Layer.Value;
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo_Tuple :=
                              Layer.Get_Widget.Get_Center;
            Size   : constant GDouble := Layer.Get_Widget.Get_Size;
         begin
            if Layer.Sectors then
               Do_Sector
               (  Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start,
                  Color  => Layer.Off_Color
               );
               Do_Sector
               (  Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start + Pi / 2.0,
                  Color  => Layer.On_Color
               );
            else
               Do_Half
               (  Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start,
                  Color  => Layer.Off_Color
               );
               Do_Half
               (  Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start + Pi,
                  Color  => Layer.On_Color
               );
            end if;
         end;
      else
         if Layer.Sectors then
            Do_Sector
            (  Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start,
               Color  => Layer.Off_Color
            );
            Do_Sector
            (  Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start + Pi / 2.0,
               Color  => Layer.On_Color
            );
         else
            Do_Half
            (  Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start,
               Color  => Layer.Off_Color
            );
            Do_Half
            (  Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start + Pi,
               Color  => Layer.On_Color
            );
         end if;
      end if;
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Disk_Needle_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Adjustment (Layer : Disk_Needle_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Center (Layer : Disk_Needle_Layer) return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_From (Layer : Disk_Needle_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Disk_Needle_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Off_Color (Layer : Disk_Needle_Layer)
      return Gdk_Color is
   begin
      return Layer.Off_Color;
   end Get_Off_Color;

   function Get_On_Color (Layer : Disk_Needle_Layer)
      return Gdk_Color is
   begin
      return Layer.On_Color;
   end Get_On_Color;

   function Get_Radius (Layer : Disk_Needle_Layer) return GDouble is
   begin
      return Layer.Radius;
   end Get_Radius;

   function Get_Properties_Number
            (  Layer : Disk_Needle_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Disk_Needle_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the center"
                  );
            when Property_Radius =>
               return
                  Gnew_Double
                  (  Name    => "r",
                     Nick    => "r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb   => "The radius"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of corresponding to the " &
                                "value 0"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum => -Pi,
                     Maximum => Pi,
                     Default => Pi,
                     Blurb   => "The length added to the value of " &
                                "the property from is the angle " &
                                "coresponding to the value 1"
                  );
            when Property_Value =>
               return
                  Gnew_Double
                  (  Name    => "value",
                     Nick    => "value",
                     Minimum => 0.0,
                     Maximum => 1.0,
                     Default => 0.0,
                     Blurb   => "The indicated value"
                  );
            when Property_Sectors =>
               return
                  Gnew_Boolean
                  (  Name    => "sectors",
                     Nick    => "sectors",
                     Default => False,
                     Blurb   => "The shape of the needle. When True, " &
                                "it is four sectors of interleaving " &
                                "colors. When False, it is two " &
                                "halves"
                  );
            when Property_On_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "on-color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "on-color",
                     Blurb      => "The needle's on color"
                  );
            when Property_Off_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "off-color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "off-color",
                     Blurb      => "The needle's off color"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The needle size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Disk_Needle_Layer;
               Property : Positive
            )  return GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.Y);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Radius);
               when Property_From =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Sectors =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Sectors);
               when Property_On_Color =>
                  Set_Value (Value, Layer.On_Color);
               when Property_Off_Color =>
                  Set_Value (Value, Layer.Off_Color);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Value =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Value);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Sectors (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Sectors;
   end Get_Sectors;

   function Get_Value (Layer : Disk_Needle_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Is_Updated (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Disk_Needle_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated  := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Disk_Needle_Layer
             )  is
      Center     : Cairo_Tuple;
      Radius     : GDouble;
      From       : GDouble;
      Length     : GDouble;
      Sectors    : Boolean;
      On_Color   : Gdk_Color;
      Off_Color  : Gdk_Color;
      Adjustment : Boolean;
   begin
      Restore (Stream, Center);
      Restore (Stream, Radius);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, On_Color);
      Restore (Stream, Off_Color);
      Restore (Stream, Layer.Scaled, Adjustment, Layer.Sectors);
      Set
      (  Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color
      );
      if Adjustment then
         declare
            Adjustment : Gtk_Adjustment;
         begin
            Restore (Stream, Adjustment);
            Add_Adjustment (Layer, Adjustment);
         end;
      else
         declare
            Value : GDouble;
         begin
            Restore (Stream, Value);
            Set_Value (Layer, Value);
         end;
      end if;
   end Restore;

   procedure Scale
             (  Layer  : in out Disk_Needle_Layer;
                Factor : GDouble
             )  is
      Radius : constant GDouble := Layer.Radius * Factor;
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Radius  := Radius;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer     : in out Disk_Needle_Layer;
                Center    : Cairo_Tuple;
                Radius    : GDouble;
                From      : GDouble;
                Length    : GDouble;
                Sectors   : Boolean;
                On_Color  : Gdk_Color;
                Off_Color : Gdk_Color
             )  is
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Center    := Center;
      Layer.Radius    := Radius;
      Layer.From      := From;
      Layer.Length    := GDouble'Min (GDouble'Max (Length, -Pi), Pi);
      Layer.Sectors   := Sectors;
      Layer.On_Color  := On_Color;
      Layer.Off_Color := Off_Color;
      Layer.Updated   := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Disk_Needle_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Radius =>
               Layer.Radius := Get_Double (Value);
               if Layer.Radius < 1.0E-6 then
                  Layer.Radius := 1.0E-6;
               end if;
            when Property_From =>
               Layer.From := Get_Double (Value);
               if Layer.From not in -2.0 * Pi..2.0 * Pi then
                  Layer.From :=
                     GDouble'Remainder (Layer.From, 2.0 * Pi);
               end if;
            when Property_Length =>
               Layer.Length := Get_Double (Value);
               if Layer.Length < -Pi then
                  Layer.Length := -Pi;
               elsif Layer.Length > -Pi then
                  Layer.Length := Pi;
               end if;
            when Property_Sectors =>
               Layer.Sectors := Get_Boolean (Value);
            when Property_On_Color =>
               Layer.On_Color := Get_Value (Value);
            when Property_Off_Color =>
               Layer.Off_Color := Get_Value (Value);
            when Property_Value =>
               Set_Value (Layer, Get_Double (Value));
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Disk_Needle_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Value
             (  Layer : in out Disk_Needle_Layer;
                Value : GDouble
             )  is
   begin
      if Value <= 0.0 then
         if Layer.Value /= 0.0 then
            Layer.Value   := 0.0;
            Layer.Updated := True;
         end if;
      elsif Value >= 1.0 then
         if Layer.Value /= 1.0 then
            Layer.Value   := 1.0;
            Layer.Updated := True;
         end if;
      else
         if Layer.Value /= Value then
            Layer.Value   := Value;
            Layer.Updated := True;
         end if;
      end if;
   end Set_Value;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Disk_Needle_Layer
             )  is
   begin
      Store (Stream, Layer.Center);
      Store (Stream, Layer.Radius);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.On_Color);
      Store (Stream, Layer.Off_Color);
      Store
      (  Stream,
         Layer.Scaled,
         Layer.Adjustment /= null,
         Layer.Sectors
      );
      if Layer.Adjustment = null then
         Store (Stream, Layer.Value);
      else
         Store (Stream, Layer.Adjustment);
      end if;
   end Store;

end Gtk.Layered.Disk_Needle;
