--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     RSVG.Handle                                 Luebeck            --
--  Implementation                                 Winter, 2017       --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

with Ada.Unchecked_Deallocation;
with System;

package body RSVG.Handle is
   use GLib.Object;

   function Close
            (  Handle : not null access RSVG_Handle_Record
            )  return IO_Result is
      function Internal
               (  Handle : System.Address;
                  Error  : access GError
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_close");
      Error : aliased GError;
   begin
      if 0 /= Internal (Get_Object (Handle), Error'Access) then
         return (Success => True);
      else
         return (Success => False, Error => Error);
      end if;
   end Close;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  RSVG_Handle_Record'Class,
             RSVG_Handle
          );

   function Get_Base_URI
            (  Handle : not null access RSVG_Handle_Record
            )  return String is
      function Internal
               (  Handle : System.Address
               )  return chars_ptr;
      pragma Import (C, Internal, "rsvg_handle_get_base_uri");
      Result : constant chars_ptr := Internal (Get_Object (Handle));
   begin
      if Null_Ptr = Result then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Base_URI;

   function Get_Dimensions
            (  Handle : not null access RSVG_Handle_Record
            )  return Dimension_Data is
      procedure Internal
                (  Handle : System.Address;
                   Data   : out Dimension_Data
                );
      pragma Import (C, Internal, "rsvg_handle_get_dimensions");
      Result : Dimension_Data;
   begin
      Internal (Get_Object (Handle), Result);
      return Result;
   end Get_Dimensions;

   function Get_Dimensions_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
           )  return Dimension_Data is
      function Internal
               (  Handle : System.Address;
                  Data   : access Dimension_Data;
                  ID     : char_array
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_get_dimensions_sub");
      Result : aliased Dimension_Data;
   begin
      if 0 = Internal (Get_Object (Handle), Result'Access, To_C (ID))
      then
         Raise_Exception (Layout_Error'Identity, "No element found");
      end if;
      return Result;
   end Get_Dimensions_Sub;

   function Get_Pixbuf
            (  Handle : not null access RSVG_Handle_Record
            )  return Gdk_Pixbuf is
      use System;
      function Internal
               (  Handle : System.Address
               )  return System.Address;
      pragma Import (C, Internal, "rsvg_handle_get_pixbuf");
      Result : Gdk_Pixbuf := Null_Pixbuf;
      Object : constant System.Address :=
                        Internal (Get_Object (Handle));
   begin
      if Null_Address /= Object then
         Result := new Gdk_Pixbuf_Record;
         Set_Object (Result, Object);
      end if;
      return Result;
   end Get_Pixbuf;

   function Get_Pixbuf_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Gdk_Pixbuf is
      use System;
      function Internal
               (  Handle : System.Address;
                  ID     : char_array
               )  return System.Address;
      pragma Import (C, Internal, "rsvg_handle_get_pixbuf_sub");
      Result : Gdk_Pixbuf := Null_Pixbuf;
      Object : constant System.Address :=
                        Internal (Get_Object (Handle), To_C (ID));
   begin
      if Null_Address /= Object then
         Result := new Gdk_Pixbuf_Record;
         Set_Object (Result, Object);
      end if;
      return Result;
   end Get_Pixbuf_Sub;

   function Get_Position_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Position_Data is
      function Internal
               (  Handle : System.Address;
                  Data   : access Position_Data;
                  ID     : char_array
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_get_position_sub");
      Result : aliased Position_Data;
   begin
      if 0 = Internal (Get_Object (Handle), Result'Access, To_C (ID))
      then
         Raise_Exception (Layout_Error'Identity, "No element found");
      end if;
      return Result;
   end Get_Position_Sub;

   procedure Gtk_New (Handle : out RSVG_Handle) is
   begin
      Handle := new RSVG_Handle_Record;
      RSVG.Handle.Initialize (Handle);
   end Gtk_New;

   function Gtk_New_From_Data
            (  Data : Stream_Element_Array
            )  return Create_Result is
      Error  : IO_Result;
      Handle : RSVG_Handle;
   begin
      Handle := new RSVG_Handle_Record;
      RSVG.Handle.Initialize (Handle, Data, Error);
      if Error.Success then
         return (Success => True, Handle => Handle);
      else
         Free (Handle);
         return (Success => False, Error => Error.Error);
      end if;
   end Gtk_New_From_Data;

   function Gtk_New_From_File
            (  File_Name : String
            )  return Create_Result is
      Error  : IO_Result;
      Handle : RSVG_Handle;
   begin
      Handle := new RSVG_Handle_Record;
      RSVG.Handle.Initialize (Handle, File_Name, Error);
      if Error.Success then
         return (Success => True, Handle => Handle);
      else
         Free (Handle);
         return (Success => False, Error => Error.Error);
      end if;
   end Gtk_New_From_File;

   procedure Gtk_New_With_Flags
             (  Handle : out RSVG_Handle;
                Flags  : Handle_Flags
             )  is
   begin
      Handle := new RSVG_Handle_Record;
      RSVG.Handle.Initialize (Handle, Flags);
   end Gtk_New_With_Flags;

   function Has_Sub
            (  Handle : not null access RSVG_Handle_Record;
               ID     : String
            )  return Boolean is
      function Internal
               (  Handle : System.Address;
                  ID     : char_array
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_has_sub");
   begin
      return 0 /= Internal (Get_Object (Handle), To_C (ID));
   end Has_Sub;

   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class
             )  is
      function Internal return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new");
   begin
      Set_Object (Handle, Internal);
   end Initialize;

   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class;
                Flags  : Handle_Flags
             )  is
      function Internal (Flags : Handle_Flags) return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new_with_flags");
   begin
      Set_Object (Handle, Internal (Flags));
   end Initialize;

   procedure Initialize
             (  Handle : not null access RSVG_Handle_Record'Class;
                Data   : Stream_Element_Array;
                Error  : out IO_Result
             )  is
      function Internal
               (  Data  : System.Address;
                  Count : GSize;
                  Error : access GError
               )  return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new_from_data");
      use System;
      Object : System.Address;
      Status : aliased GError;
   begin
      Object := Internal
                (  Data (Data'First)'Address,
                   Data'Length,
                   Status'Access
                );
      if Object = Null_Address then
         Error := (Success => False, Error => Status);
      else
         Set_Object (Handle, Object);
         Error := (Success => True);
      end if;
   end Initialize;

   procedure Initialize
             (  Handle    : not null access RSVG_Handle_Record'Class;
                File_Name : String;
                Error     : out IO_Result
             )  is
      function Internal
               (  File_Name : char_array;
                  Error     : access GError
               )  return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new_from_file");
      use System;
      Object : System.Address;
      Status : aliased GError;
   begin
      Object := Internal (To_C (File_Name), Status'Access);
      if Object = Null_Address then
         Error := (Success => False, Error => Status);
      else
         Set_Object (Handle, Object);
         Error := (Success => True);
      end if;
   end Initialize;

   function Render_Cairo
            (  Handle  : not null access RSVG_Handle_Record;
               Context : Cairo_Context
            )  return Boolean is
      function Internal
               (  Handle  : System.Address;
                  Context : Cairo_Context
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_render_cairo");
   begin
      return 0 /= Internal (Get_Object (Handle), Context);
   end Render_Cairo;

   function Render_Cairo_Sub
            (  Handle  : not null access RSVG_Handle_Record;
               Context : Cairo_Context;
               ID      : String
            )  return Boolean is
      function Internal
               (  Handle  : System.Address;
                  Context : Cairo_Context;
                  ID      : char_array
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_render_cairo_sub");
   begin
      return 0 /= Internal (Get_Object (Handle), Context, To_C (ID));
   end Render_Cairo_Sub;

   procedure Set_Base_URI
             (  Handle : not null access RSVG_Handle_Record;
                URI    : String
             )  is
      procedure Internal
                (  Handle : System.Address;
                   URI    : char_array
                );
      pragma Import (C, Internal, "rsvg_handle_set_base_uri");
   begin
      Internal (Get_Object (Handle), To_C (URI));
   end Set_Base_URI;

   procedure Set_DPI
             (  Handle : not null access RSVG_Handle_Record;
                DPI    : double
             )  is
      procedure Internal
                (  Handle : System.Address;
                   DPI    : double
                );
      pragma Import (C, Internal, "rsvg_handle_set_dpi");
   begin
      Internal (Get_Object (Handle), DPI);
   end Set_DPI;

   procedure Set_DPI_X_Y
             (  Handle : not null access RSVG_Handle_Record;
                DPI_X  : double;
                DPI_Y  : double
             )  is
      procedure Internal
                (  Handle : System.Address;
                   DPI_X  : double;
                   DPI_Y  : double
                );
      pragma Import (C, Internal, "rsvg_handle_set_dpi_x_y");
   begin
      Internal (Get_Object (Handle), DPI_X, DPI_Y);
   end Set_DPI_X_Y;

   function Write
            (  Handle : not null access RSVG_Handle_Record;
               Buffer : Stream_Element_Array
            )  return IO_Result is
      pragma Assert (Stream_Element'Size = Guchar'Size);
      function Internal
               (  Handle : System.Address;
                  Buffer : System.Address;
                  Count  : GSize;
                  Error  : access GError
               )  return GBoolean;
      pragma Import (C, Internal, "rsvg_handle_write");
      Error : aliased GError;
   begin
      if 0 /= Internal
              (  Get_Object (Handle),
                 Buffer (Buffer'First)'Address,
                 Buffer'Length,
                 Error'Access
              )
      then
         return (Success => True);
      else
         return (Success => False, Error => Error);
      end if;
   end Write;

end RSVG.Handle;
