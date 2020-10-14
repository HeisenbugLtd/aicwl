--                                                                    --
--  package Gtk.Layered.SVG         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2017       --
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

with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;
with Gtk.Missed;                use Gtk.Missed;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.SVG is
   type SVG_Ptr is access all SVG_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_X,
           Property_Y,
           Property_Image
        );

   procedure Free is
      new Ada.Unchecked_Deallocation (SVG_Layer, SVG_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access SVG_Layer is
      Ptr : SVG_Ptr := new SVG_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_SVG
             (  Under   : not null access Layer_Location'Class;
                Image   : RSVG_Handle;
                Center  : Cairo_Tuple := (0.0, 0.0);
                Scaled  : Boolean     := False
             )  is
      Ptr   : SVG_Ptr := new SVG_Layer;
      Layer : SVG_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Center => Center
      );
      if Image /= null then
         Layer.Image := Image;
         Image.Ref;
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_SVG;

   function Add_SVG
            (  Under   : not null access Layer_Location'Class;
               Image   : RSVG_Handle;
               Center  : Cairo_Tuple := (0.0, 0.0);
               Scaled  : Boolean     := False
            )  return not null access SVG_Layer is
      Ptr   : SVG_Ptr := new SVG_Layer;
      Layer : SVG_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      if Image /= null then
         Layer.Image := Image;
         Image.Ref;
      end if;
      Set
      (  Layer  => Layer,
         Center => Center
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_SVG;

   procedure Draw
             (  Layer   : in out SVG_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Size : Dimension_Data;
   begin
      if Layer.Image = null then
         Layer.Updated := False;
         return;
      end if;
      Size := Layer.Image.Get_Dimensions;
      if Layer.Scaled then
         declare
            State  : constant Context_State := Save (Context);
            Size_X : constant GDouble :=
                        GDouble (Layer.Widget.Get_Allocated_Width);
            Size_Y : constant GDouble :=
                        GDouble (Layer.Widget.Get_Allocated_Height);
            Factor : constant GDouble :=
                              GDouble'Min
                              (  Size_X / GDouble (Size.Width),
                                 Size_Y / GDouble (Size.Height)
                              );
         begin
            Scale (Context, Factor, Factor);
            Translate
            (  Cr => Context,
               Tx => (  Layer.Center.X * Size_X / Factor
                     -  GDouble (Size.Width) / 2.0
                     ),
               Ty => (  Layer.Center.Y * Size_Y / Factor
                     -  GDouble (Size.Height) / 2.0
            )        );
            if not Layer.Image.Render_Cairo (Context) then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Error,
                  "Failed to render scaled SVG image"
               );
            end if;
         end;
      else
         Move_To
         (  Cr => Context,
            X  => Layer.Center.X - GDouble (Size.Width)  / 2.0,
            Y  => Layer.Center.Y - GDouble (Size.Height) / 2.0
         );
         if not Layer.Image.Render_Cairo (Context) then
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Error,
               "Failed to render SVG image"
            );
         end if;
      end if;
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out SVG_Layer) is
   begin
      if Layer.Image /= null then
         Layer.Image.Unref;
         Layer.Image := null;
      end if;
      Abstract_Layer (Layer).Finalize;
   end Finalize;

   function Get_Center (Layer : SVG_Layer) return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_SVG (Layer : SVG_Layer) return RSVG_Handle is
   begin
      if Layer.Image = null then
         return null;
      else
         Layer.Image.Ref;
         return Layer.Image;
      end if;
   end Get_SVG;

   function Get_Properties_Number (Layer : SVG_Layer)
      return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : SVG_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the image center"
                  );
            when Property_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the image center"
                  );
            when Property_Image =>
               return
                  Gnew_Object
                  (  Name        => "image",
                     Nick        => "image",
                     Object_Type => RSVG.Handle.Get_Type,
                     Blurb       => "The handle of the SVG object"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The bar size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : SVG_Layer;
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
               when Property_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.X);
               when Property_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.Y);
               when Property_Image =>
                  Init (Value, RSVG.Handle.Get_Type);
                  Set_Object (Value, Layer.Image);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : SVG_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Is_Updated (Layer : SVG_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out SVG_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out SVG_Layer
             )  is
      Center : Cairo_Tuple;
   begin
      Restore (Stream, Center);
      Restore (Stream, Layer.Scaled);
      Set
      (  Layer  => Layer,
         Center => Center
      );
   end Restore;

   procedure Scale
             (  Layer  : in out SVG_Layer;
                Factor : GDouble
             )  is
   begin
      Layer.Center.X := Layer.Center.X * Factor;
      Layer.Center.Y := Layer.Center.Y * Factor;
      Layer.Updated  := True;
   end Scale;

   procedure Set
             (  Layer  : in out SVG_Layer;
                Center : Cairo_Tuple
             )  is
   begin
      Layer.Center  := Center;
      Layer.Updated := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out SVG_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Image =>
               declare
                  Image : constant Glib.Object.GObject :=
                                   Get_Object (Value);
               begin
                  if Image.Get_Type /= RSVG.Handle.Get_Type then
                     return;
                  elsif Image = null then
                     if Layer.Image /= null then
                        Layer.Image.Unref;
                        Layer.Image := null;
                     end if;
                  else
                     declare
                        Handle : constant RSVG_Handle :=
                                          new RSVG_Handle_Record;
                     begin
                        RSVG.Handle.Initialize (Handle);
                        Handle.Set_Object (Image.Get_Object);
                        Handle.Ref;
                        if Layer.Image /= null then
                           Layer.Image.Unref;
                        end if;
                        Layer.Image := Handle;
                     end;
                  end if;
               end;
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out SVG_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : SVG_Layer
             )  is
   begin
      Store (Stream, Layer.Center);
      Store (Stream, Layer.Scaled);
   end Store;

end Gtk.Layered.SVG;
