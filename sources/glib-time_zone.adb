--                                                                    --
--  package GLib.Time_Zone          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  00:00 10 Dec 2019  --
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

with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with System.Address_To_Access_Conversions;

package body GLib.Time_Zone is

   Epoch : constant Time :=
                    Ada.Calendar.Formatting.Time_Of (1970, 01, 01);

   package Conversions is
      new System.Address_To_Access_Conversions (GTime_Zone);

   function Get_Identifier_Fallback (TZ : GTime_Zone) return chars_ptr;
   pragma External
          (  C,
             Get_Identifier_Fallback,
             "g_time_zone_get_identifier"
          );
   pragma Weak_External (Get_Identifier_Fallback);

   function Adjust_Time
            (  TZ         : GTime_Zone;
               Time_Type  : GTime_Type;
               Time_Stamp : GInt64
            )  return GInt is
      function Internal
               (  TZ         : Address;
                  Time_Type  : GTime_Type;
                  Time_Stamp : access GInt64
               )  return GInt;
      pragma Import (C, Internal, "g_time_zone_adjust_time");
      Stamp : aliased GInt64 := Time_Stamp;
   begin
      return Internal (TZ'Address, Time_Type, Stamp'Access);
   end Adjust_Time;

   function Adjust_Time
            (  TZ         : GTime_Zone;
               Time_Stamp : Time
            )  return GInt is
      use Ada.Calendar.Time_Zones;
   begin
      return Adjust_Time
             (  TZ,
                G_TIME_TYPE_UNIVERSAL,
                GInt64
                (  Time_Stamp
                -  Duration (UTC_Time_Offset (Time_Stamp)) * 60.0
                -  Epoch
             )  );
   end Adjust_Time;

   function Find_Interval
            (  TZ         : GTime_Zone;
               Time_Type  : GTime_Type;
               Time_Stamp : GInt64
            )  return GInt is
      function Internal
               (  TZ         : Address;
                  Time_Type  : GTime_Type;
                  Time_Stamp : access GInt64
               )  return GInt;
      pragma Import (C, Internal, "g_time_zone_find_interval");
      Stamp : aliased GInt64 := Time_Stamp;
   begin
      return Internal (TZ'Address, Time_Type, Stamp'Access);
   end Find_Interval;

   function Find_Interval
            (  TZ         : GTime_Zone;
               Time_Stamp : Time
            )  return GInt is
      use Ada.Calendar.Time_Zones;
   begin
      return Find_Interval
             (  TZ,
                G_TIME_TYPE_UNIVERSAL,
                GInt64
                (  Time_Stamp
                -  Duration (UTC_Time_Offset (Time_Stamp)) * 60.0
                -  Epoch
             )  );
   end Find_Interval;

   function Get_Abbreviation
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return UTF8_String is
      function Internal
               (  TZ       : Address;
                  Interval : GInt
               )  return chars_ptr;
      pragma Import (C, Internal, "g_time_zone_get_abbreviation");
      Ptr : constant chars_ptr := Internal (TZ'Address, Interval);
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Value (Ptr);
      end if;
   end Get_Abbreviation;

   function Get_Identifier (TZ : GTime_Zone) return UTF8_String is
      function Internal (TZ : Address) return chars_ptr;
      pragma Import (C, Internal, "g_time_zone_get_identifier");

      Ptr : constant chars_ptr := Internal (TZ'Address);
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Value (Ptr);
      end if;
   end Get_Identifier;

   function Get_Identifier_Fallback (TZ : GTime_Zone)
      return chars_ptr is
      Result : chars_ptr;
      for Result'Address use TZ'Address;
   begin
      return Result;
   end Get_Identifier_Fallback;

   function Get_Offset
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return Duration is
      function Internal
               (  TZ       : Address;
                  Interval : GInt
               )  return GInt32;
      pragma Import (C, Internal, "g_time_zone_get_offset");
   begin
      return Duration (Internal (TZ'Address, Interval));
   end Get_Offset;

   function Gtk_New (Identifier : String) return access GTime_Zone is
      function Internal
               (  Identifier : char_array
               )  return Address;
      pragma Import (C, Internal, "g_time_zone_new");
   begin
      return Conversions.To_Pointer
             (  Internal (To_C (Identifier))
             ) .all'Unchecked_Access;
   end Gtk_New;

   function Gtk_New_Local return access GTime_Zone is
      function Internal return Address;
      pragma Import (C, Internal, "g_time_zone_new_local");
   begin
      return Conversions.To_Pointer (Internal).all'Unchecked_Access;
   end Gtk_New_Local;

   function Gtk_New_Offset
            (  Offset : Duration
            )  return access GTime_Zone is
      subtype Figure is Integer range 0..9;
      function Decimal (Value : Figure) return Character is
      begin
         return Character'Val (Character'Pos ('0') + Value);
      end Decimal;
      function Image (Value : Integer) return String is
      begin
         return Decimal ((Value / 10) mod 10) & Decimal (Value mod 10);
      end Image;
      Seconds : Integer := Integer (abs Offset);
      Hours   : Integer;
      Minutes : Integer;
   begin
      Hours   := Seconds / 3_600;
      Seconds := Seconds mod 3_600;
      Minutes := Seconds / 60;
      Seconds := Seconds mod 60;
      if Offset >= 0.0 then
         return Gtk_New
                (  '+' & Image (Hours)
                &  ':' & Image (Minutes)
                &  ':' & Image (Seconds)
                );
      else
         return Gtk_New
                (  '-' & Image (Hours)
                &  ':' & Image (Minutes)
                &  ':' & Image (Seconds)
                );
      end if;
   end Gtk_New_Offset;

   function Gtk_New_UTC return access GTime_Zone is
      function Internal return Address;
      pragma Import (C, Internal, "g_time_zone_new_utc");
   begin
      return Conversions.To_Pointer (Internal).all'Unchecked_Access;
   end Gtk_New_UTC;

   function Is_DST
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return Boolean is
      function Internal
               (  TZ       : Address;
                  Interval : GInt
               )  return GBoolean;
      pragma Import (C, Internal, "g_time_zone_is_dst");
   begin
      return Internal (TZ'Address, Interval) /= 0;
   end Is_DST;

   procedure Ref (TZ : GTime_Zone) is
      function Internal (TZ : Address) return Address;
      pragma Import (C, Internal, "g_time_zone_ref");
      Result : Address;
   begin
      Result := Internal (TZ'Address);
   end Ref;

end GLib.Time_Zone;
