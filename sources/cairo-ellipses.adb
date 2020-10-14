--                                                                    --
--  package Cairo.Ellipses          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
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

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;

package body Cairo.Ellipses is

   Eps : constant := 1.0E-6;
--
-- Get_Relative_Point -- Get ellipse point in relative coordinates
--
--    Ellipse - The ellipse
--    Angle   - The angle in polar coordinates
--    X, Y    - The coordinates of the point
--
   procedure Get_Relative_Point
             (  Ellipse : Ellipse_Parameters;
                Angle   : Ellipse_Angle;
                X, Y    : out GDouble
             );
   pragma Inline (Get_Relative_Point);

   procedure Elliptic_Arc
             (  Context : Cairo_Context;
                Ellipse : Ellipse_Parameters;
                From    : Ellipse_Angle := 0.0;
                Length  : Ellipse_Angle := 2.0 * Pi
             )  is
      To : constant Ellipse_Angle := From + Length;
   begin
      if Ellipse.Major_Curvature < Eps then
         if abs Length >= Pi then
            raise Constraint_Error with
                  "Greater than Pi angular length of flat elliptic arc";
         end if;
         declare
            Start : constant Ellipse_Angle :=
                    Ellipse_Angle'Remainder (From, Pi);
         begin
            if Start = 0.0 then
               raise Constraint_Error;
            elsif Start > 0.0 then
               if Start + Length not in Eps..Pi - Eps then
                  raise Constraint_Error;
               end if;
            else
               if Start + Length not in Eps - Pi..Eps then
                  raise Constraint_Error;
               end if;
            end if;
         end;
         Line_To
         (  Context,
            Get_X (Ellipse, From),
            Get_Y (Ellipse, From)
         );
         Line_To
         (  Context,
            Get_X (Ellipse, To),
            Get_Y (Ellipse, To)
         );
      else
         declare
            State : Context_State := Save (Context);
         begin
            Translate (Context, Ellipse.Center.X, Ellipse.Center.Y);
            Rotate (Context, Ellipse.Angle);
            Scale
            (  Context,
               1.0 / Ellipse.Major_Curvature,
               Ellipse.Minor_Radius
            );
            if Length > 0.0 then
               Arc
               (  Cr     => Context,
                  Xc     => 0.0,
                  Yc     => 0.0,
                  Radius => 1.0,
                  Angle1 => GDouble (From),
                  Angle2 => GDouble (To)
               );
            else
               Arc_Negative
               (  Cr     => Context,
                  Xc     => 0.0,
                  Yc     => 0.0,
                  Radius => 1.0,
                  Angle1 => GDouble (From),
                  Angle2 => GDouble (To)
               );
            end if;
         end;
      end if;
   end Elliptic_Arc;

   procedure Elliptic_Arc_Abs
             (  Context : Cairo_Context;
                Ellipse : Ellipse_Parameters;
                From    : GDouble := 0.0;
                Length  : GDouble := 2.0 * Pi
             )  is
      Start : constant Ellipse_Angle := Ellipse * From;
      Angle : Ellipse_Angle := Ellipse * (From + Length) - Start;
   begin
      if Length > 0.0 then
         if Angle < 0.0 then
            Angle := Angle + 2.0 * Pi;
         end if;
      elsif Length < 0.0 then
         if Angle > 0.0 then
            Angle := Angle - 2.0 * Pi;
         end if;
      end if;
      Elliptic_Arc (Context, Ellipse, Start, Angle);
   end Elliptic_Arc_Abs;

   procedure Finalize (Save : in out Context_State) is
   begin
      if Save.Context /= Null_Context then
         Restore (Save.Context);
         Save.Context := Null_Context;
      end if;
   end Finalize;

   function Get_Point
            (  Ellipse : Ellipse_Parameters;
               Angle   : Ellipse_Angle
            )  return Cairo_Tuple is
      X, Y : GDouble;
   begin
      Get_Relative_Point (Ellipse, Angle, X, Y);
      return
      (  X => Ellipse.Center.X + X,
         Y => Ellipse.Center.Y + Y
      );
   end Get_Point;

   procedure Get_Relative_Point
             (  Ellipse : Ellipse_Parameters;
                Angle   : Ellipse_Angle;
                X, Y    : out GDouble
             )  is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta : constant GDouble := GDouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant GDouble :=
                           abs cos (Pi/2.0 + Ellipse.Angle - Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error;
            end if;
            X := Ellipse.Minor_Radius * cos (Beta) / Cos_Gamma;
            Y := Ellipse.Minor_Radius * sin (Beta) / Cos_Gamma;
         end;
      else
         declare
            Cos_Angle   : constant GDouble := cos (GDouble (Angle));
            Sin_Angle   : constant GDouble := sin (GDouble (Angle));
            Cos_Ellipse : constant GDouble := cos (Ellipse.Angle);
            Sin_Ellipse : constant GDouble := sin (Ellipse.Angle);
         begin
            X :=
               (  Cos_Angle * Cos_Ellipse / Ellipse.Major_Curvature
               -  Sin_Angle * Sin_Ellipse * Ellipse.Minor_Radius
               );
            Y :=
               (  Cos_Angle * Sin_Ellipse / Ellipse.Major_Curvature
               +  Sin_Angle * Cos_Ellipse * Ellipse.Minor_Radius
               );
         end;
      end if;
   end Get_Relative_Point;

   function Get_X
            (  Ellipse : Ellipse_Parameters;
               Angle   : Ellipse_Angle
            )  return GDouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta : constant GDouble := GDouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant GDouble :=
                           abs cos (Pi/2.0 + Ellipse.Angle - Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error with "Infinite x-coordinate";
            end if;
            return
            (  Ellipse.Center.X
            +  Ellipse.Minor_Radius * cos (Beta) / Cos_Gamma
            );
         end;
      else
         declare
            Cos_Angle : constant GDouble := cos (GDouble (Angle));
            Sin_Angle : constant GDouble := sin (GDouble (Angle));
         begin
            return
            (  Ellipse.Center.X
            +  Cos_Angle * cos (Ellipse.Angle) / Ellipse.Major_Curvature
            -  Sin_Angle * sin (Ellipse.Angle) * Ellipse.Minor_Radius
            );
         end;
      end if;
   end Get_X;

   function Get_Y
            (  Ellipse : Ellipse_Parameters;
               Angle   : Ellipse_Angle
            )  return GDouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta : constant GDouble := GDouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant GDouble :=
                           abs cos (Pi/2.0 + Ellipse.Angle - Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error with "Infinite y-coordinate";
            end if;
            return
            (  Ellipse.Center.Y
            +  Ellipse.Minor_Radius * sin (Beta) / Cos_Gamma
            );
         end;
      else
         declare
            Cos_Angle : constant GDouble := cos (GDouble (Angle));
            Sin_Angle : constant GDouble := sin (GDouble (Angle));
         begin
            return
            (  Ellipse.Center.Y
            +  Cos_Angle * sin (Ellipse.Angle) / Ellipse.Major_Curvature
            +  Sin_Angle * cos (Ellipse.Angle) * Ellipse.Minor_Radius
            );
         end;
      end if;
   end Get_Y;

   function Is_Bounded (Ellipse : Ellipse_Parameters) return Boolean is
   begin
      return abs Ellipse.Major_Curvature >= Eps;
   end Is_Bounded;

   function Get_Path_Extents (Context : Cairo_Context)
      return Cairo_Box is
      X1, X2 : aliased GDouble;
      Y1, Y2 : aliased GDouble;
   begin
      Path_Extents
      (  Cr => Context,
         X1 => X1'Access,
         Y1 => Y1'Access,
         X2 => X2'Access,
         Y2 => Y2'Access
      );
      return (X1 => X1, Y1 => Y1, X2 => X2, Y2 => Y2);
   end Get_Path_Extents;

   function Save (Context : Cairo_Context) return Context_State is
   begin
      return Result : Context_State do
         Save (Context);
         Result.Context := Context;
      end return;
   end Save;

   function "*"
            (  Ellipse : Ellipse_Parameters;
               Angle   : GDouble
            )  return Ellipse_Angle is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         return Ellipse_Angle (Angle - Ellipse.Angle);
      else
         declare
            Cos_Ellipse : constant GDouble := cos (Ellipse.Angle);
            Sin_Ellipse : constant GDouble := - sin (Ellipse.Angle);
            Cos_Angle   : constant GDouble := cos (Angle);
            Sin_Angle   : constant GDouble := sin (Angle);
         begin
            return
               Ellipse_Angle
               (  arctan
                  (  Y =>
                        (  Sin_Ellipse * Cos_Angle
                        +  Cos_Ellipse * Sin_Angle
                        ),
                     X =>
                        (  Ellipse.Major_Curvature
                        *  Ellipse.Minor_Radius
                        *  (  Cos_Ellipse * Cos_Angle
                           -  Sin_Ellipse * Sin_Angle
               )  )     )  );
         end;
      end if;
   end "*";

   function "/"
            (  Angle   : Ellipse_Angle;
               Ellipse : Ellipse_Parameters
            )  return GDouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         return GDouble (Angle) - Ellipse.Angle;
      else
         declare
            X, Y : GDouble;
         begin
            Get_Relative_Point (Ellipse, Angle, X, Y);
            return arctan (X => X, Y => Y);
         end;
      end if;
   end "/";

   function "*"
            (  Angle   : GDouble;
               Ellipse : Ellipse_Parameters
            )  return Ellipse_Angle is
   begin
      return Ellipse * Angle;
   end "*";

   function "*"
            (  Ellipse : Ellipse_Parameters;
               Gain    : GDouble
            )  return Ellipse_Parameters is
   begin
      return
      (  Center          => (  X => Ellipse.Center.X * Gain,
                               Y => Ellipse.Center.Y * Gain
                            ),
         Major_Curvature => Ellipse.Major_Curvature / Gain,
         Minor_Radius    => Ellipse.Minor_Radius * Gain,
         Angle           => Ellipse.Angle
      );
   end "*";

   function "*"
            (  Gain    : GDouble;
               Ellipse : Ellipse_Parameters
            )  return Ellipse_Parameters is
   begin
      return
      (  Center          => (  X => Ellipse.Center.X * Gain,
                               Y => Ellipse.Center.Y * Gain
                            ),
         Major_Curvature => Ellipse.Major_Curvature / Gain,
         Minor_Radius    => Ellipse.Minor_Radius * Gain,
         Angle           => Ellipse.Angle
      );
   end "*";

   function "/"
            (  Ellipse : Ellipse_Parameters;
               Gain    : GDouble
            )  return Ellipse_Parameters is
   begin
      return
      (  Center          => (  X => Ellipse.Center.X / Gain,
                               Y => Ellipse.Center.Y / Gain
                            ),
         Major_Curvature => Ellipse.Major_Curvature * Gain,
         Minor_Radius    => Ellipse.Minor_Radius / Gain,
         Angle           => Ellipse.Angle
      );
   end "/";

   function "+"
            (  Ellipse : Ellipse_Parameters;
               Offset  : Cairo_Tuple
            )  return Ellipse_Parameters is
   begin
      return
      (  Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (  X => Ellipse.Center.X + Offset.X,
                               Y => Ellipse.Center.Y + Offset.Y
      )                     );
   end "+";

   function "+"
            (  Offset  : Cairo_Tuple;
               Ellipse : Ellipse_Parameters
            )  return Ellipse_Parameters is
   begin
      return
      (  Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (  X => Ellipse.Center.X + Offset.X,
                               Y => Ellipse.Center.Y + Offset.Y
      )                     );
   end "+";

   function "-"
            (  Ellipse : Ellipse_Parameters;
               Offset  : Cairo_Tuple
            )  return Ellipse_Parameters is
   begin
      return
      (  Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (  X => Ellipse.Center.X - Offset.X,
                               Y => Ellipse.Center.Y - Offset.Y
      )                     );
   end "-";

   function "-"
            (  Offset  : Cairo_Tuple;
               Ellipse : Ellipse_Parameters
            )  return Ellipse_Parameters is
   begin
      return
      (  Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (  X => Offset.X - Ellipse.Center.X,
                               Y => Offset.Y - Ellipse.Center.Y
      )                     );
   end "-";

end Cairo.Ellipses;
