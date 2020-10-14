--                                                                    --
--  package Gtk.Layered.SVG         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2017       --
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

with RSVG.Handle;  use RSVG.Handle;

package Gtk.Layered.SVG is
--
-- SVG_Layer -- A layer shaped as a SVG
--
   type SVG_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer with private;
--
-- Add_SVG -- Add SVG
--
--    Under   - The layer or widget to place the SVG under
--    Image   - The SVG object to use
--    Center  - The location of the image center
--    Scaled  - The layer is scaled together with the parent widget
--
-- The scaling is performed as follows:
--
-- (o)  The SVG's width and height  are extended to fill the box without
--      clipping.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_SVG
             (  Under   : not null access Layer_Location'Class;
                Image   : RSVG_Handle;
                Center  : Cairo_Tuple := (0.0, 0.0);
                Scaled  : Boolean     := False
             );
   function Add_SVG
            (  Under   : not null access Layer_Location'Class;
               Image   : RSVG_Handle;
               Center  : Cairo_Tuple := (0.0, 0.0);
               Scaled  : Boolean     := False
            )  return not null access SVG_Layer;
--
-- Get_Center -- The SVG image center
--
--    Layer - The SVG layer
--
-- Returns :
--
--    The image center
--
   function Get_Center (Layer : SVG_Layer) return Cairo_Tuple;
--
-- Get_SVG -- The SVG image
--
--    Layer - The SVG layer
--
-- Returns :
--
--    The SVG image, when not null the client is responsible to Unref it
--
   function Get_SVG (Layer : SVG_Layer) return RSVG_Handle;
--
-- Set -- Parameters of the SVG
--
--    Layer  - The SVG layer
--    Center - The location of the image center
--
   procedure Set
             (  Layer  : in out SVG_Layer;
                Center : Cairo_Tuple
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access SVG_Layer;
   overriding
      procedure Draw
                (  Layer   : in out SVG_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding procedure Finalize (Layer : in out SVG_Layer);
   overriding
      function Get_Properties_Number
               (  Layer : SVG_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : SVG_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : SVG_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : SVG_Layer) return Boolean;
   overriding function Is_Updated (Layer : SVG_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out SVG_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out SVG_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out SVG_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out SVG_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out SVG_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : SVG_Layer
                );
private
   type SVG_Layer is new Abstract_Layer and Scalable_Layer with record
      Image   : RSVG_Handle;
      Center  : Cairo_Tuple;
      Scaled  : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.SVG;
