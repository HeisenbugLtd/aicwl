--                                                                    --
--  package GLib.Time_Zone          Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Spring, 2019       --
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

with GLib;          use GLib;
with GLib.Object;   use GLib.Object;
with Ada.Calendar;  use Ada.Calendar;

package GLib.Time_Zone is

   type GTime_Type is
        (  G_TIME_TYPE_STANDARD,
           G_TIME_TYPE_DAYLIGHT,
           G_TIME_TYPE_UNIVERSAL
        );
--
-- GTime_Zone -- The time zone object
--
   type GTime_Zone (<>) is limited private;
--
-- Adjust_Time -- Find an interval corresponding to time
--
--    TZ          - The time zone
--  [ Time_Type ] - The type of time stamp
--    Time_Stamp  - The time stamp (seconds since January 1, 1970)
--
-- This function adjusts the time if necessary
--
-- Returns :
--
--    The interval containing Time_Stamp, or -1 in case of failure
--
   function Adjust_Time
            (  TZ         : GTime_Zone;
               Time_Type  : GTime_Type;
               Time_Stamp : GInt64
            )  return GInt;
   function Adjust_Time
            (  TZ         : GTime_Zone;
               Time_Stamp : Time
            )  return GInt;
--
-- Find_Interval -- Find an interval corresponding to time
--
--    TZ          - The time zone
--  [ Time_Type ] - The type of time stamp
--    Time_Stamp  - The time stamp (seconds since January 1, 1970)
--
-- Returns :
--
--    The interval containing Time_Stamp, or -1 in case of failure
--
   function Find_Interval
            (  TZ         : GTime_Zone;
               Time_Type  : GTime_Type;
               Time_Stamp : GInt64
            )  return GInt;
   function Find_Interval
            (  TZ         : GTime_Zone;
               Time_Stamp : Time
            )  return GInt;
--
-- Get_Abbreviation -- The time zone interval abbreviation
--
--    TZ      - The time zone
--    Interval - The interval
--
-- Returns :
--
--    The identifier string
--
   function Get_Abbreviation
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return UTF8_String;
--
-- Get_Identifier -- The identifier of the time zone
--
--    TZ - The time zone
--
-- Returns :
--
--    The identifier string
--
   function Get_Identifier (TZ : GTime_Zone) return UTF8_String;
--
-- Get_Offset -- The UTC offset of the time zone interval
--
--    TZ      - The time zone
--    Interval - The interval
--
-- Returns :
--
--    The UTC offset
--
   function Get_Offset
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return Duration;
--
-- Gtk_New -- Creates a time zone corresponding to identifier
--
--    Identifier - The time zone identifier
--
-- The identifier describes the time zone.
--
-- Returns :
--
--    The requested timezone
--
   function Gtk_New (Identifier : String) return access GTime_Zone;
--
-- Gtk_New_Local -- Creates a time zone corresponding to local time
--
-- Returns :
--
--    The requested timezone
--
   function Gtk_New_Local return access GTime_Zone;
--
-- Gtk_New_Offset -- Creates a time zone corresponding to UTC offset
--
--    Offset - The UTC offset
--
-- Returns :
--
--    The requested timezone
--
   function Gtk_New_Offset (Offset : Duration) return access GTime_Zone;
--
-- Gtk_New_UTC -- Creates a time zone corresponding to UTC
--
-- Returns :
--
--    The requested timezone
--
   function Gtk_New_UTC return access GTime_Zone;
--
-- Is_DST -- Is day saving time
--
--    TZ      - The time zone
--    Interval - The interval
--
-- Returns :
--
--    True if the interval corresponds to day saving time
--
   function Is_DST
            (  TZ       : GTime_Zone;
               Interval : GInt
            )  return Boolean;
--
-- Ref -- Increase the reference count
--
--    TZ - The time zone object
--
   procedure Ref (TZ : GTime_Zone);
--
-- Unref -- Decrease the reference count
--
--    TZ - The time zone object
--
   procedure Unref (TZ : GTime_Zone);

private
   pragma Convention (C, GTime_Type);

   type GTime_Zone is limited null record;
   pragma Convention (C, GTime_Zone);

   pragma Import (C, Unref, "g_time_zone_unref");

end GLib.Time_Zone;
