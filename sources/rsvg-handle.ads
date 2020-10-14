--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     RSVG.Handle                                 Luebeck            --
--  Interface                                      Winter, 2017       --
--                                                                    --
--                                Last revision :  19:06 02 Jan 2018  --
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

with Ada.Streams;   use Ada.Streams;
with Cairo;         use Cairo;
with Interfaces.C;  use Interfaces.C;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with GLib.Error;    use GLib.Error;
with GLib.Object;   use GLib;

package RSVG.Handle is

   type Handle_Flags is mod 2**2;
   pragma Convention (C, Handle_Flags);
   Handle_Flags_None           : constant := 0;
   Handle_Flag_Unlimited       : constant := 1;
   Handle_Flag_Keep_Image_Data : constant := 2;
--
-- RSVG_Handle_Record -- SVG object
--
   type RSVG_Handle_Record is
      new Glib.Object.GObject_Record with null record;
   type RSVG_Handle is access all RSVG_Handle_Record'Class;

   type Position_Data is record
      X : int;
      Y : int;
   end record;
   pragma Convention (C, Position_Data);

   type Dimension_Data is record
      Width  : int;
      Height : int;
      EM     : GDouble;
      EX     : GDouble;
   end record;
   pragma Convention (C, Dimension_Data);
--
-- Create_Result -- Result of a handle creation
--
   type Create_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Handle : RSVG_Handle;
         when False =>
            Error  : GError;
      end case;
   end record;
--
-- IO_Result -- Result of an I/O operation
--
   type IO_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            null;
         when False =>
            Error : GError;
      end case;
   end record;
--
-- Close -- Loads image bytes
--
--    Handle - An RSVG handle
--
-- Closes handle  to indicate that  loading the image is complete.  This
-- will return IO_Result (True) if the loader closed successfully.  Note
-- that handle  isn't freed until Unref is called.
--
-- Returns :
--
--    I/O result Error must be freed using Error_Free if present
--
   function Close
            (  Handle : not null access RSVG_Handle_Record
            )  return IO_Result;
--
-- Get_Base_URI -- Get base URI
--
--    Handle - An RSVG handle
--
-- This function gets the base URI for this handle.
--
-- Result :
--
--    The URI, can be empty
--
   function Get_Base_URI
            (  Handle : not null access RSVG_Handle_Record
            )  return String;
--
-- Get_Dimensions -- Get SVG dimensions
--
--    Handle - An RSVG handle
--
-- Get the SVG's size.
--
-- Result :
--
--    The dimension data
--
   function Get_Dimensions
            (  Handle : not null access RSVG_Handle_Record
            )  return Dimension_Data;
--
-- Get_Dimensions_Sub -- Get SVG element's dimensions
--
--    Handle - An RSVG handle
--    ID     - The element name, e.g. "#layer1" of "layer1"
--
-- Get the SVG's element size.
--
-- Result :
--
--    The dimension data
--
-- Exceptions :
--
--    Layout_Error - On errors
--
   function Get_Dimensions_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Dimension_Data;
--
-- Get_Pixbuf -- Get from SVG
--
--    Handle - An RSVG handle
--
-- Returns the pixbuf loaded by handle.  The  pixbuf  returned  will  be
-- reffed, so the caller of this  function  must  assume  that  ref.  If
-- insufficient data has been read to create the  pixbuf,  or  an  error
-- occurred in loading, then Null_Pixbuf will be returned. Note that the
-- pixbuf may not be complete until Close has been called.
--
-- Result :
--
--    The pixbuf or Null_Pixbuf
--
   function Get_Pixbuf
            (  Handle : not null access RSVG_Handle_Record
            )  return Gdk_Pixbuf;
--
-- Get_Pixbuf_Sub -- Get from SVG's element
--
--    Handle - An RSVG handle
--    ID     - The element name, e.g. "#layer1" of "layer1"
--
-- Returns the pixbuf loaded by handle.  The  pixbuf  returned  will  be
-- reffed, so the caller of this  function  must  assume  that  ref.  If
-- insufficient data has been read to create the  pixbuf,  or  an  error
-- occurred in loading, then Null_Pixbuf will be returned. Note that the
-- pixbuf may not be complete until rsvg_handle_close has been called.
--
-- Result :
--
--    The pixbuf or Null_Pixbuf
--
   function Get_Pixbuf_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Gdk_Pixbuf;
--
-- Get_Position_Sub -- Get SVG element's position
--
--    Handle - An RSVG handle
--    ID     - The element name, e.g. "#layer1" of "layer1"
--
-- Get  the SVG's  position.  Do not  call  from  within  the  Size_Func
-- callback, because an infinite loop will occur.
--
-- Result :
--
--    The dimension data
--
-- Exceptions :
--
--    Layout_Error - On errors
--
   function Get_Position_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Position_Data;
--
-- Get_Type -- Get type of handle
--
-- Returns :
--
--    The type of
--
   function Get_Type return GType;
--
-- Gtk_New -- Create a new handle
--
--    Handle - To be created
--
-- Returns a new handle.  The result  must  be freed  with  Unref.  This
-- handle can be used for dynamically loading an image. You need to feed
-- it data using Write, then call Close when done.  Afterwards,  you can
-- render it using Cairo  or  get a Gdk_Pixbuf from it.  When  finished,
-- free  with Unref.  No more  than one  image  can be  loaded  with one
-- handle.
--
   procedure Gtk_New (Handle : out RSVG_Handle);
--
-- Gtk_New_From_Data -- Create a new handle
--
--    Data - The data
--
-- Loads the SVG specified by Data.
--
-- Returns :
--
--    Error must be freed using Error_Free if present
--
   function Gtk_New_From_Data
            (  Data : Stream_Element_Array
            )  return Create_Result;
--
-- Gtk_New_From_Data -- Create a new handle
--
--    File_Name - The file name to load
--
-- Loads  the SVG specified by File_Name.  If built with gnome-vfs,  the
-- name can be a URI.
--
-- Returns :
--
--    Error must be freed using Error_Free if present
--
   function Gtk_New_From_File
            (  File_Name : String
            )  return Create_Result;
--
-- Gtk_New_With_Flags -- Create a new handle
--
--    Handle - To be created
--    Flags  - The flags
--
   procedure Gtk_New_With_Flags
             (  Handle : out RSVG_Handle;
                Flags  : Handle_Flags
             );
--
-- Has_Sub -- Check an SVG element
--
--    Handle - An RSVG handle
--    ID     - The element name, e.g. "layer1"
--
-- Result :
--
--    True of the element is present
--
   function Has_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Boolean;
--
-- Initialize -- Construction
--
--    Handle    - An RSVG handle
--  [ Flags ]   - The flags
--  [ Data      - Data to create from
--    Error ]   - Creation I/O result
--  [ File_Name - File to create from
--    Error ]   - Creation I/O result
--
-- This procedure has to be called by any derived type. The variant with
-- Data or File_Name and Error leaves Handle  not initialized when Error
-- is present. In that case Error must be freed using Error_Free.
--
   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class
             );
   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class;
                Flags  : Handle_Flags
             );
   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class;
                Data   : Stream_Element_Array;
                Error  : out IO_Result
             );
   procedure Initialize
             (  Handle    : not null access RSVG_Handle_Record'Class;
                File_Name : UTF8_String;
                Error     : out IO_Result
             );
--
-- Render_Cairo -- Draw SVG on a Cairo surface
--
--    Handle  - An RSVG handle
--    Context - The Cairo context
--
-- Returns :
--
--    True if successfull
--
   function Render_Cairo
            (  Handle  : not null access RSVG_Handle_Record;
               Context : Cairo_Context
            )  return Boolean;
--
-- Render_Cairo_Sub -- Draw SVG's element on a Cairo surface
--
--    Handle  - An RSVG handle
--    Context - The Cairo context
--    ID      - The element name, e.g. "#layer1" of "layer1"
--
-- Returns :
--
--    True if successfull
--
   function Render_Cairo_Sub
            (  Handle  : not null access RSVG_Handle_Record;
               Context : Cairo_Context;
               ID      : String
            )  return Boolean;
--
-- Set_Base_URI -- Set base URI
--
--    Handle - An RSVG handle
--    URI    - The URI to set
--
-- Set the base URI for this SVG.  This can only  be called before Write
-- has been called.
--
   procedure Set_Base_URI
             (  Handle : not null access RSVG_Handle_Record;
                URI    : String
             );
--
-- Set_DPI -- Set DPI
--
--    Handle - An RSVG handle
--    DPI    - Dots per inch (aka pixels per inch)
--
-- This procedure sets  the DPI for the outgoing pixbuf.  Common  values
-- are 75, 90, and 300 DPI. Passing a number <= 0 to DPI will reset  the
-- DPI to whatever the default value happens to be.
--
   procedure Set_DPI
             (  Handle : not null access RSVG_Handle_Record;
                DPI    : double
             );
--
-- Set_DPI_X_Y -- Set DPI for both coordinates
--
--    Handle - An RSVG handle
--    DPI    - Dots per inch (aka pixels per inch)
--
-- This procedure sets  the DPI for the outgoing pixbuf.  Common  values
-- are 75, 90, and 300 DPI. Passing a number <= 0 to DPI_X or DPI_Y will
-- reset the DPI to whatever the default value happens to be.
--
   procedure Set_DPI_X_Y
             (  Handle : not null access RSVG_Handle_Record;
                DPI_X  : double;
                DPI_Y  : double
             );
--
-- Write -- Loads image bytes
--
--    Handle - An RSVG handle
--    Buffer - To load from
--
-- Loads the bytes  of the image.  This will return IO_Result (True)  if
-- the data  was loaded successful,  and  IO_Result (False)  if an error
-- occurred. In the latter case, the loader will be closed, and will not
-- accept further writes.  The error will be set to  an error  from  the
-- RsvgError domain. Errors from GIOErrorEnum are also possible.
--
-- Returns :
--
--    I/O result Error must be freed using Error_Free if present
--
   function Write
            (  Handle : not null access RSVG_Handle_Record;
               Buffer : Stream_Element_Array
            )  return IO_Result;

private
   pragma Import (C, Get_Type, "rsvg_handle_get_type");

end RSVG.Handle;
