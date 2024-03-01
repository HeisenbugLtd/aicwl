--                                                                    --
--  package Gtk.Main.Router         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2006       --
--                                                                    --
--                                Last revision :  13:14 14 Sep 2019  --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Streams;              use Ada.Streams;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Task_Identification;  use Ada.Task_Identification;
with Gdk.Color.IHLS;           use Gdk.Color.IHLS;
with GLib.Main;                use GLib.Main;
with GLib.Messages;            use GLib.Messages;
with GLib.Values;              use GLib.Values;
with GNAT.Sockets;             use GNAT.Sockets;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Clipboard;            use Gtk.Clipboard;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Image;                use Gtk.Image;
with Gtk.Image_Menu_Item;      use Gtk.Image_Menu_Item;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Missed;               use Gtk.Missed;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Font;               use Pango.Font;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Gtk.Handlers;

package body Gtk.Main.Router is
--
-- Gateway_State -- The  state  of  the  protected  object   controlling
--                  interaction between the main loop task  and  an  Ada
-- task  requesting  servicing.  The following diagram illustrates state
-- transitions upon a request.
--
--        main loop                   task
--           :                          |
--           :                          |__
--        sleeping                         | Request_Service (data)
--           :                             |
--  timer -->|                           __|
--           |__                        :
--              | Initiate_Service      :
--              | Service (data)     waiting
--              | Complete_Service -->  :
--            __|                       :__
--           |                            | Serviced
--           :                 <--------- |
--        sleeping                     .__|
--           :                         |
--
   Queue_Size    : constant        := 100;
   Max_Recursion : constant        := 100;
   GPS_Prompt    : constant String := "GPS>> ";
   GPS_Port      : Natural         := 50_000;
   Connected     : Boolean         := Standard.False;
   Recursion     : Natural         := 0;
   Window_X      : GInt            := 0;
   Window_Y      : GInt            := 0;
   Parent        : Gtk_Window      := null;
   Main          : Task_ID         := Null_Task_ID;
   Trace_Dialog  : Gtk_Dialog;
   Channel       : Socket_Type;
   Command       : Unbounded_String;

   function Where (Text : String) return String is
   begin
      return " in Gtk.Main.Router." & Text;
   end Where;

   function Get_Parent (Window : Gtk_Window := null)
      return Gtk_Window is
   begin
      if Window = null then
         return Parent;
      else
         return Window;
      end if;
   end Get_Parent;

   package Window_Callback is
      new Gtk.Handlers.Callback (Gtk_Window_Record);
   procedure On_Quit (Window : access Gtk_Window_Record'Class);

   type Request_State is
        (  Request_Unused,
           Request_Pending,
           Request_Serviced,
           Request_Failed,
           Request_Successful
        );
   type Request_Item;
   type Request_Item_Ptr is access all Request_Item;
   type Request_Item is limited record
      Previous    : Request_Item_Ptr := Request_Item'Unchecked_Access;
      Next        : Request_Item_Ptr := Request_Item'Unchecked_Access;
      State       : Request_State    := Request_Unused;
      Fault       : Exception_Occurrence;
      Synchronous : Boolean;
      Data        : Request_Data_Ptr;
   end record;

   function Get (Head : Request_Item) return Request_Item_Ptr;
   pragma Inline (Get);
   procedure Insert (Head : Request_Item; Item : Request_Item_Ptr);
   pragma Inline (Insert);
   function Length (Head : Request_Item) return Natural;
   pragma Inline (Length);

   function Get (Head : Request_Item) return Request_Item_Ptr is
      Self : constant Request_Item_Ptr := Head.Next.Previous;
   begin
      if Head.Next = Self then -- Empty
         return null;
      else
         declare
            Item : constant Request_Item_Ptr := Head.Next;
         begin
            Self.Next          := Item.Next;
            Item.Next.Previous := Self;
            Item.Next          := Item;
            Item.Previous      := Item;
            return Item;
         end;
      end if;
   end Get;

   procedure Insert (Head : Request_Item; Item : Request_Item_Ptr) is
      Self : constant Request_Item_Ptr := Head.Next.Previous;
   begin
      Item.Next.Previous := Item.Previous;
      Item.Previous.Next := Item.Next;
      Item.Previous      := Self.Previous;
      Item.Next          := Self;
      Item.Next.Previous := Item;
      Item.Previous.Next := Item;
   end Insert;

   function Length (Head : Request_Item) return Natural is
      This  : Request_Item_Ptr := Head.Next;
      Self  : constant Request_Item_Ptr := This.Previous;
      Count : Natural := 0;
   begin
      while This /= Self loop
         Count := Count + 1;
         This  := This.Next;
      end loop;
      return Count;
   end Length;

   protected Gateway is
      procedure Abort_Service
                (  Item  : in out Request_Item_Ptr;
                   Error : Exception_Occurrence
                );
      procedure Allocated_New;
      procedure Clean_Up         (Item : out Request_Item_Ptr);
      procedure Complete_Service (Item : in out Request_Item_Ptr);
      procedure Get_Free_Item    (Item : out Request_Item_Ptr);
      function Get_Max_Async return Positive;
      function Get_Request_Info return String;
      function Is_Quitted return Boolean;
      procedure Initiate_Service (Item : out Request_Item_Ptr);
      entry Request_Asynchronous (Item : Request_Item_Ptr);
      entry Request_Synchronous  (Item : in out Request_Item_Ptr);
      procedure Set_Max_Async (Max : Positive);
      procedure Quit;
   private
      entry Serviced (Boolean) (Item : in out Request_Item_Ptr);
      Quitted   : Boolean  := Standard.False;
      Current   : Boolean  := Standard.False;
      Async     : Natural  := 0;   -- Number of asynchronous requests
      Max_Async : Positive := 100; -- Maximum number of such requests
      Allocated : Natural  := 0;   -- Total allocated items
      Queued    : aliased Request_Item;
      Ready     : aliased Request_Item;
      Free      : aliased Request_Item;
   end Gateway;
--
-- Callback -- Called from the main loop on timer events
--
   function Callback return Boolean is
      Item : Request_Item_Ptr;
   begin
      loop
         Gateway.Initiate_Service (Item);
         exit when Item = null;
         begin
            if Recursion < Max_Recursion then
               Recursion := Recursion + 1;
               begin
                  Service (Item.Data.all);
                  Recursion := Recursion - 1;
               exception
                  when others =>
                     Recursion := Recursion - 1;
                     raise;
               end;
            end if;
            Gateway.Complete_Service (Item);
         exception
            when Error : others =>
               Gateway.Abort_Service (Item, Error);
         end;
      end loop;
      return Standard.True;
   end Callback;

   protected body Gateway is

      procedure Abort_Service
                (  Item  : in out Request_Item_Ptr;
                   Error : Exception_Occurrence
                )  is
      begin
         if Item.Synchronous then
            Item.State := Request_Failed;
            Save_Occurrence (Item.Fault, Error);
            Insert (Ready, Item);
            Current := not Current;
         else
            Item.State := Request_Unused;
            Insert (Free, Item);
            Async := Async - 1;
         end if;
      end Abort_Service;

      procedure Allocated_New is
      begin
         Allocated := Allocated + 1;
      end Allocated_New;

      procedure Clean_Up (Item : out Request_Item_Ptr) is
      begin
         Item := Get (Ready);
         if Item = null then
            Item := Get (Free);
         end if;
      end Clean_Up;

      procedure Complete_Service (Item : in out Request_Item_Ptr) is
      begin
         if Item.Synchronous then
            Item.State := Request_Successful;
            Insert (Ready, Item);
            Current := not Current;
         else
            Item.State := Request_Unused;
            Insert (Free, Item);
            Async := Async - 1;
         end if;
      end Complete_Service;

      procedure Get_Free_Item (Item : out Request_Item_Ptr) is
      begin
         Item := Get (Free);
      end Get_Free_Item;

      function Get_Max_Async return Positive is
      begin
         return Max_Async;
      end Get_Max_Async;

      function Get_Request_Info return String is
         function Is_Quitted return String is
         begin
            if Quitted then
               return " [quitted]";
            else
               return "";
            end if;
         end Is_Quitted;
      begin
         return
         (  "requests: pending" & Integer'Image (Length (Queued))
         &  ", ready"           & Integer'Image (Length (Ready ))
         &  ", free"            & Integer'Image (Length (Free  ))
         &  ", allocated"       & Integer'Image (Allocated)
         &  ", max async"       & Integer'Image (Max_Async)
         &  Is_Quitted
         );
      end Get_Request_Info;

      procedure Initiate_Service (Item : out Request_Item_Ptr) is
      begin
         Item := Get (Queued);
      end Initiate_Service;

      function Is_Quitted return Boolean is
      begin
         return Quitted;
      end Is_Quitted;

      procedure Quit is
      begin
         Quitted := Standard.True;
      end Quit;

      entry Request_Asynchronous
            (  Item : Request_Item_Ptr
            )  when Async < Max_Async or else Quitted is
      begin
         if Quitted then
            Item.State := Request_Unused;
            Insert (Free, Item);
            raise Quit_Error;
         end if;
         Item.State := Request_Pending;
         Insert (Queued, Item);
         Item.Synchronous := Standard.False;
         Async := Async + 1;
      end Request_Asynchronous;

      entry Request_Synchronous
            (  Item : in out Request_Item_Ptr
            )  when Standard.True is
      begin
         if Quitted then
            Item.State := Request_Unused;
            Insert (Free, Item);
            raise Quit_Error;
         end if;
         Item.State := Request_Pending;
         Insert (Queued, Item);
         Item.Synchronous := Standard.True;
         requeue Serviced (not Current);
      end Request_Synchronous;

      entry Serviced (for Toggle in Boolean)
            (  Item : in out Request_Item_Ptr
            )  when Quitted or else Current = Toggle is
      begin
         if Quitted then
            Item.State := Request_Unused;
            Insert (Free, Item);
            raise Quit_Error;
         else
            case Item.State is
               when Request_Unused =>
                  null;
               when Request_Pending | Request_Serviced =>
                  requeue Serviced (not Current);
               when Request_Failed =>
                  Insert (Free, Item);
                  Reraise_Occurrence (Item.Fault);
               when Request_Successful =>
                  Insert (Free, Item);
            end case;
         end if;
      end Serviced;

      procedure Set_Max_Async (Max : Positive) is
      begin
         Max_Async := Max;
      end Set_Max_Async;

   end Gateway;

   package body Generic_Message is

      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Message_Data,
                Message_Data_Ptr
             );

      package Sources is new Generic_Sources (Message_Data_Ptr);

      function Service (Message : Message_Data_Ptr)
         return Boolean is
         Ptr : Message_Data_Ptr := Message;
      begin
         Message.Handler (Message.Data);
         Free (Ptr);
         return Standard.False;
      exception
         when others =>
            Free (Ptr);
            return Standard.False;
      end Service;

      procedure Send
                (  Handler : Handler_Procedure;
                   Data    : User_Data;
                   Timeout : Duration := 0.5
                )  is
         Message : Message_Data_Ptr := new Message_Data;
         Item    : Request_Item_Ptr;
      begin
         Message.Handler := Handler;
         Message.Data    := Data;
         if Main = Current_Task then
            declare
               ID : G_Source_ID;
            begin
               ID := Sources.Idle_Add (Service'Access, Message);
            end;
         elsif Main = Null_Task_ID then
            raise Program_Error;
         else
            Gateway.Get_Free_Item (Item);
            if Item = null then
               Item := new Request_Item;
               Gateway.Allocated_New;
            end if;
            Item.Synchronous := Standard.False;
            Item.Data        := Message.all'Unchecked_Access;
            select
               Gateway.Request_Asynchronous (Item);
            or delay Timeout;
               raise Busy_Error with
                     "Current state " & Gateway.Get_Request_Info;
            end select;
         end if;
      exception
         when others =>
            Free (Message);
            raise;
      end Send;

      procedure Service (Data : in out Message_Data) is
         Message : Message_Data_Ptr := Data'Unchecked_Access;
      begin
         Message.Handler (Message.Data);
         Free (Message);
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  Exception_Information (Error)
               &  Where ("Generic_Message.Service")
            )  );
            Free (Message);
      end Service;

   end Generic_Message;
--
-- Connect -- To the GPS
--
-- The  procedure shows a dialog box while connecting to the GPS server.
-- The  dialog box shows connection error message when connection fails.
-- Waiting for connection can be canceled by closing the box.
--
   procedure Connect is
      Dialog  : Gtk_Dialog;
      Message : Unbounded_String;
      Label   : Gtk_Label;
   begin
      Gtk_New
      (  Dialog,
         (  "GPS at "
         &  Host_Name
         &  " port"
         &  Integer'Image (GPS_Port)
         ),
         Get_Parent,
         Destroy_With_Parent + Modal
      );
      Gtk_New (Label, "...connecting...");
      Set_Line_Wrap (Label, Standard.True);
      Pack_Start
      (  Get_Content_Area (Dialog),
         Label,
         Standard.True,
         Standard.True
      );
      declare
         --
         -- An independent timer for a connection  task  to  communicate
         -- with  the  dialog  box.  It  must be independent because the
         -- default timer may be blocked at this point.
         --
         package Timers is new Generic_Sources (Integer);
         --
         -- Connect_To_GPS -- The task connecting to GPS
         --
         task Connect_To_GPS;
         task body Connect_To_GPS is
            Address : Sock_Addr_Type;
         begin
            Connected := Standard.False;
            Create_Socket (Channel);
            Address.Addr := Addresses (Get_Host_By_Name ("localhost"));
            Address.Port := Port_Type (GPS_Port);
            begin
               Connect_Socket (Channel, Address);
               Connected := Standard.True;
            exception
               when Error : Socket_Error =>
                  Append
                  (  Message,
                     (  "Check if GPS is started with the server "
                     &  "option. For example"
                     &  Character'Val (10)
                     &  "  > gps --server="
                     &  Trim
                        (  Integer'Image (GPS_Port),
                           Ada.Strings.Both
                        )
                     &  Character'Val (10)
                     &  Exception_Message (Error)
                  )  );
               when Error : others =>
                  Append (Message, Exception_Information (Error));
            end;
         end Connect_To_GPS;
         --
         -- Connect_Callback -- Timer callback
         --
         -- When the task is terminated, the callback either  shows  the
         -- error message in the dialog box, or else closes it.
         --
         function Connect_Callback (Data : Integer) return Boolean is
         begin
            if Connect_To_GPS'Terminated then
               if Length (Message) = 0 then
                  Response (Dialog, Gtk_Response_OK);
               elsif Label /= null then
                  Label := null;
                  Set_Text (Label, To_String (Message));
               end if;
            end if;
            return Standard.True;
         end Connect_Callback;

         ID    : G_Source_Id;
         Close : Gtk_Button;
      begin
         Close := Add_Button_From_Stock
                  (  Dialog   => Dialog,
                     Response => Gtk_Response_Close,
                     Icon     => Stock_Close,
                     Label    => "_Close",
                     Tip      => "Stop connecting"
                  );
         Close.Set_Can_Default (Standard.True);
         Dialog.Show_All;
         ID := Timers.Timeout_Add
               (  Interval => 50,
                  Func     => Connect_Callback'Access,
                  Data     => 0
               );
         case Run (Dialog) is
            when Gtk_Response_OK => -- Closed on success from the task
               Remove (ID);
            when others =>          -- Closed on error or by user
               Remove (ID);            -- Stop polling
               Close_Socket (Channel); -- Will aboort task if active
               Connected := Standard.False;
         end case;
      end;
      Destroy (Dialog);
   end Connect;

   function Get_Max_Asynchronous return Positive is
   begin
      return Gateway.Get_Max_Async;
   end Get_Max_Asynchronous;

   function Get_Request_Info return String is
   begin
      return Gateway.Get_Request_Info;
   end Get_Request_Info;
--
-- GPS_Read -- Reading GPS connection socket until prompt it received
--
   procedure GPS_Read is
      Buffer : Stream_Element_Array (1..1);
      Byte   : Character;
      From   : Sock_Addr_Type;
      Last   : Stream_Element_Offset;
      Index  : Positive := GPS_Prompt'First;
   begin
      loop
         Receive_Socket (Channel, Buffer, Last, From);
         Byte := Character'Val (Stream_Element'Pos (Buffer (1)));
         if Byte = GPS_Prompt (Index) then
            exit when Index = GPS_Prompt'Last;
            Index := Index + 1;
         elsif Index > GPS_Prompt'First then
            Index := GPS_Prompt'First;
            if Byte = GPS_Prompt (Index) then
               Index := GPS_Prompt'First + 1;
            end if;
         end if;
      end loop;
   end GPS_Read;
--
-- GPS_Send -- Writing into GPS socket
--
--    Command - The GPS command to send
--
   procedure GPS_Send (Command : String) is
      Buffer : Stream_Element_Array (1..Command'Length);
      for Buffer'Address use Command (Command'First)'Address;
      pragma Import (Ada, Buffer);
      Last : Stream_Element_Offset;
   begin
      Send_Socket (Channel, Buffer, Last);
   end GPS_Send;

   procedure Init
             (  Window   : not null access Gtk_Window_Record'Class;
                Period   : Duration := 0.2;
                GPS_Port : Natural  := 50_000
             )  is
   begin
      Gtk.Main.Router.GPS_Port := GPS_Port;
      if Main = Null_Task_ID then
         Main := Current_Task;
         declare
            ID : G_Source_Id;
         begin
            ID := Timeout_Add
                  (  Guint (Float (Period) * 1000.0),
                     Callback'Access
                  );
         end;
         Parent := Window.all'Unchecked_Access;
         Window_Callback.Connect (Window, "destroy", On_Quit'Access);
      end if;
   end Init;

   function Is_Active return Boolean is
   begin
      return not Gateway.Is_Quitted;
   end Is_Active;

   procedure Quit is
   begin
      Gateway.Quit;
   end Quit;

   procedure Request (Data : in out Request_Data'Class) is
   begin
      if Main = Current_Task then
         if Gateway.Is_Quitted then
            raise Quit_Error;
         end if;
         if Recursion < Max_Recursion then
            Recursion := Recursion + 1;
            begin
               Service (Data);
               Recursion := Recursion - 1;
            exception
               when others =>
                  Recursion := Recursion - 1;
                  raise;
            end;
         end if;
      elsif Main = Null_Task_ID then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            "No main task active" & Where ("Request")
         );
         raise Program_Error;
      else
         declare
            Item : Request_Item_Ptr;
         begin
            Gateway.Get_Free_Item (Item);
            if Item = null then
               Item := new Request_Item;
               Gateway.Allocated_New;
            end if;
            Item.Data := Data'Unchecked_Access;
            Gateway.Request_Synchronous (Item);
         end;
      end if;
   end Request;

   package body Generic_Callback_Request is

      procedure Request
                (  Callback : Callback_Procedure;
                   Data     : not null access User_Data
                )  is
         Marshaler : Callback_Data (Data);
      begin
         Marshaler.Callback := Callback;
         Request (Marshaler);
      end Request;

      procedure Service (Data : in out Callback_Data) is
      begin
         Data.Callback (Data.Parameters);
      end Service;

   end Generic_Callback_Request;

   type Callback_Request_Data is new Request_Data with record
      Callback : Gtk_Callback;
   end record;
   procedure Service (Data : in out Callback_Request_Data);

   procedure Service (Data : in out Callback_Request_Data) is
   begin
      Data.Callback.all;
   end Service;

   procedure Request (Service : Gtk_Callback) is
      Marshaler : Callback_Request_Data;
   begin
      Marshaler.Callback := Service;
      Request (Marshaler);
   end Request;

   type UTF8_String_Ptr is access all UTF8_String;
   type Say_Data is record
      Message       : UTF8_String_Ptr;
      Title         : UTF8_String_Ptr;
      Mode          : UTF8_String_Ptr;
      Justification : Gtk_Justification;
      Parent        : Gtk_Widget;
   end record;
   package Say_Callback is new Generic_Callback_Request (Say_Data);

   procedure Say (Data : not null access Say_Data) is
      Main : Gtk_Window := Parent;
   begin
      if Data.Parent /= null then
         declare
            Top : Gtk_Widget;
         begin
            Top := Data.Parent.Get_Toplevel;
            if (  Top /= null
               and then
                  Top.all in Gtk_Window_Record'Class
               )
            then
               Main :=
                  Gtk_Window_Record'Class (Top.all)'Unchecked_Access;
            end if;
         end;
      end if;
      Message_Dialog
      (  Message       => Data.Message.all,
         Mode          => Data.Mode.all,
         Title         => Data.Title.all,
         Justification => Data.Justification,
         Parent        => Main
      );
   end Say;

   procedure Say
             (  Message       : UTF8_String;
                Title         : UTF8_String := "";
                Mode          : UTF8_String := Stock_Dialog_Info;
                Justification : Gtk_Justification := Justify_Left;
                Parent        : access Gtk_Widget_Record'Class := null
             )  is
      Message_Text : aliased UTF8_String := Message;
      Title_Text   : aliased UTF8_String := Title;
      Mode_Text    : aliased UTF8_String := Mode;
      Data         : aliased Say_Data;
   begin
      Data.Message := Message_Text'Unchecked_Access;
      Data.Title   := Title_Text'Unchecked_Access;
      Data.Mode    := Mode_Text'Unchecked_Access;
      Data.Justification := Justification;
      if Parent /= null then
         Data.Parent := Parent.all'Unchecked_Access;
      end if;
      Say_Callback.Request (Say'Access, Data'Access);
   end Say;

   procedure Set_Max_Asynchronous (Max : Positive) is
   begin
      Gateway.Set_Max_Async (Max);
   end Set_Max_Asynchronous;

   type Trace_Request (Length : Natural; Break : Boolean) is record
      Text : String (1..Length);
   end record;

   package Trace_Callback is
      new Generic_Callback_Request (Trace_Request);

   package Dialog_Handlers is
      new Gtk.Handlers.Callback (Gtk_Dialog_Record);

   package View_Handlers is
      new Gtk.Handlers.User_Callback (Gtk_Text_View_Record, Gtk_Dialog);

   package View_Event_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Text_View_Record,
             Boolean,
             Gtk_Dialog
          );

   package Gtk_Image_Menu_Item_Handlers is
      new Gtk.Handlers.Callback (Gtk_Image_Menu_Item_Record);

   Messages_List : Gtk_Text_Buffer;
   Step_Button   : Gtk_Button;
   Run_Button    : Gtk_Button;
   View          : Gtk_Text_View;
   Break_Button  : Gtk_Check_Button;

   type Tag_Index is mod 5;
   Tags        : array (Tag_Index) of Gtk_Text_Tag;
   Current_Tag : Tag_Index := 0;
--
-- On_Button_Press -- Event handler
--
   function On_Button_Press
            (  Dialog : access Gtk_Text_View_Record'Class;
               Event  : Gdk_Event;
               Parent : Gtk_Dialog
            )  return Boolean is
   begin
      if Get_Event_Type (Event) = Button_Press then
         Window_X := GInt (Event.Button.X);
         Window_Y := GInt (Event.Button.Y);
      end if;
      return Standard.False;
   end On_Button_Press;
--
-- On_Go_To_Location -- Event handler
--
   procedure On_Go_To_Location
             (  Item : access Gtk_Image_Menu_Item_Record'Class
             )  is
   begin
      if not Connected then
         Connect;
      end if;
      if Connected then
         GPS_Send (To_String (Command));
         GPS_Read;
      end if;
   end On_Go_To_Location;
--
-- On_Paste_Trace -- Event handler
--
   procedure On_Paste_Trace
             (  Item : access Gtk_Image_Menu_Item_Record'Class
             )  is
      Text    : constant String := Wait_For_Text (Get);
      Pointer : Integer := Text'First;
      procedure Skip is
      begin
         while Pointer < Text'Last loop
            exit when Text (Pointer) /= ' ';
            Pointer := Pointer + 1;
         end loop;
      end Skip;
      function Get_Item return Address is
         Value : Integer_Address := 0;
      begin
         if Pointer + 1 > Text'Last then
            raise Constraint_Error;
         end if;
         if Text (Pointer..Pointer + 1) /= "0x" then
            raise Constraint_Error;
         end if;
         Pointer := Pointer + 2;
         while Pointer < Text'Last loop
            case Text (Pointer) is
               when '0'..'9' =>
                  Value :=
                     (  Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('0')
                     );
               when 'a'..'f' =>
                  Value :=
                     (  Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('a')
                     +  10
                     );
               when 'A'..'F' =>
                  Value :=
                     (  Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('A')
                     +  10
                     );
               when others =>
                  exit;
            end case;
            Pointer := Pointer + 1;
         end loop;
         if Text (Pointer - 1) = 'x' then
            raise Constraint_Error;
         end if;
         return To_Address (Value);
      end Get_Item;
      Count : Natural := 0;
   begin
      declare
         Location : Address;
      begin
         while Pointer <= Text'Last loop
            Skip;
            Location := Get_Item;
            Count    := Count + 1;
         end loop;
      exception
         when others =>
            null;
      end;
      if Count > 0 then
         declare
            TB : Tracebacks_Array (1..Count);
         begin
            Pointer := Text'First;
            for Index in TB'Range loop
               Skip;
               TB (Index) := Get_Item;
            end loop;
            Trace
            (  "Symbolic stack traceback from the clipboard:" & LF
            &  Symbolic_Traceback (TB) &  LF
            );
         end;
      end if;
   end On_Paste_Trace;
--
-- On_Populate_Popup -- Event handler
--
   procedure On_Populate_Popup
             (  Dialog : access Gtk_Text_View_Record'Class;
                Params : GValues;
                Parent : Gtk_Dialog
             )  is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Dialog);
      Widget : Gtk_Widget;
      Menu   : Gtk_Menu;
      Item   : Gtk_Image_Menu_Item;
      Icon   : Gtk_Image;
      Start  : aliased Gtk_Text_Iter;
      Stop   : Gtk_Text_Iter;
      Can_Go : Boolean := Standard.False;
   begin
      Widget := Convert (Get_Address (Nth (Params, 1)));
      if Widget /= null and then Widget.all in Gtk_Menu_Record'Class
      then
         Menu := Gtk_Menu_Record'Class (Widget.all)'Unchecked_Access;
         Menu.Modify_Font
         (  Get_Font
            (  Get_Style_Context (Parent),
               Gtk_State_Flag_Normal
         )  );
         if Get.Wait_Is_Text_Available then -- Paste traceback item
            Gtk_New (Item, "Paste stack traceback");
            Gtk_New (Icon, Stock_Find, Icon_Size_Menu);
            Item.Set_Image (Icon);
            Item.Show_All;
            Gtk_Image_Menu_Item_Handlers.Connect
            (  Item,
               "activate",
               On_Paste_Trace'Access
            );
            Menu.Add (Item);
         end if;
         if Main /= Null_Task_ID then -- Go to the location item
            declare
               use type Gdk.Gdk_Window;
               Buffer_X : GInt;
               Buffer_Y : GInt;
               Moved    : Boolean;
               Res      : Boolean;
            begin
               Dialog.Window_To_Buffer_Coords
               (  Text_Window_Widget,
                  Window_X,
                  Window_Y,
                  Buffer_X,
                  Buffer_Y
               );
               Res := Dialog.Get_Iter_At_Location (Start'Access, Buffer_X, Buffer_Y);
               Copy (Start, Stop);
               Moved := Standard.True;
               while Moved and then not Starts_Line (Start) loop
                  Backward_Cursor_Position (Start, Moved);
               end loop;
               Moved := Standard.True;
               while Moved and then not Ends_Line (Stop) loop
                  Forward_Cursor_Position (Stop, Moved);
               end loop;
            end;
            declare
               Line    : String  := Buffer.Get_Text (Start, Stop);
               Pointer : Integer := Line'Last + 1;
            begin
               if (  Line'Length < 3
                  or else
                     Line (Line'First) /= '0'
                  or else
                     Line (Line'First + 1) /= 'x'
                  )
               then
                  goto Break;
               end if;
               loop
                  if Pointer = Line'First then
                     goto Break;
                  end if;
                  Pointer := Pointer - 1;
                  exit when Line (Pointer) not in '0'..'9';
               end loop;
               if (  Pointer - 4 < Line'First
                  or else
                     Line (Pointer - 4) /= '.'
                  or else
                     To_Lower (Line (Pointer - 3)) /= 'a'
                  or else
                     To_Lower (Line (Pointer - 2)) /= 'd'
                  or else
                     Line (Pointer) /= ':'
                  )
               then
                  goto Break;
               end if;
               Line (Pointer) := ' ';
               case To_Lower (Line (Pointer - 1)) is
                  when 's' | 'b' =>
                     Pointer := Pointer - 4;
                  when others =>
                     goto Break;
               end case;
               loop
                  if Pointer - 4 < Line'First then
                     goto Break;
                  end if;
                  exit when Line (Pointer - 4..Pointer - 1) = " at ";
                  Pointer := Pointer - 1;
               end loop;
               Can_Go := Standard.True;
               Command := To_Unbounded_String ("Editor.edit ");
               Append (Command, Line (Pointer..Line'Last));
               Append (Command, Character'Val (10));
            end;
<<Break>>   Gtk_New (Item, "Go to the source location");
            Item.Set_Sensitive (Can_Go);
            Gtk_New (Icon, Stock_Jump_To, Icon_Size_Menu);
            Item.Set_Image (Icon);
            Item.Show_All;
            Gtk_Image_Menu_Item_Handlers.Connect
            (  Item,
               "activate",
               On_Go_To_Location'Access
            );
            Menu.Add (Item);
         end if;
      end if;
   end On_Populate_Popup;

   procedure On_Quit (Window : access Gtk_Window_Record'Class) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Request_Item,
                Request_Item_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Request_Data'Class,
                Request_Data_Ptr
             );
      Item : Request_Item_Ptr;
   begin
      Parent := null;
      Gateway.Quit;
      loop
         Gateway.Initiate_Service (Item);
         exit when Item = null;
         if not Item.Synchronous then
            Free (Item.Data);
         end if;
         Free (Item);
      end loop;
      loop
         Gateway.Clean_Up (Item);
         exit when Item = null;
         Free (Item);
      end loop;
   end On_Quit;

   procedure On_Response
             (  Dialog   : access Gtk_Dialog_Record'Class;
                Response : Glib.Values.GValues
             )  is
   begin
      case Gtk_Response_Type (Get_Int (Nth (Response, 1))) is
         when Gtk_Response_None =>
            Trace_Dialog := null;
         when Gtk_Response_Cancel =>
            Set_Active (Break_Button, Standard.False);
         when Gtk_Response_OK =>
            Set_Active (Break_Button, Standard.True);
         when others =>
            Destroy (Trace_Dialog);
            Trace_Dialog := null;
      end case;
   end On_Response;

   procedure Trace (Data : not null access Trace_Request) is
      Start : Gtk_Text_Iter;
      Stop  : Gtk_Text_Iter;
   begin
      if Trace_Dialog = null then
         declare
            Scroll : Gtk_Scrolled_Window;
            Button : Gtk_Button;
            Font   : Pango_Font_Description :=
                        From_String ("monospace 10");
         begin
            Gtk_New
            (  Trace_Dialog,
               "Trace",
               Get_Parent,
               Destroy_With_Parent
            );

            Tags := (others => null);
            Gtk_New (Messages_List);
            Gtk_New (View, Messages_List);
            Messages_List.Unref;
            View.Modify_Font (Font);
            Free (Font);
               -- Get menu
            View_Handlers.Connect
            (  View,
               "populate-popup",
               On_Populate_Popup'Access,
               Trace_Dialog
            );
            View_Event_Handlers.Connect
            (  View,
               "button_press_event",
               View_Event_Handlers.To_Marshaller
               (  On_Button_Press'Access
               ),
               Trace_Dialog
            );
               -- Scrolled window
            Gtk_New (Scroll);
            Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
            Scroll.Add (View);
            Get_Content_Area (Trace_Dialog).Pack_Start (Scroll);
               -- Break button
            Gtk_New (Break_Button, "Break");
            Break_Button.Set_Active (Standard.True);
            Trace_Dialog.Get_Action_Area.Pack_Start
            (  Break_Button,
               Standard.False,
               Standard.False
            );
               -- Continue button
            Step_Button :=
               Add_Button_From_Stock
               (  Dialog   => Trace_Dialog,
                  Response => Gtk_Response_OK,
                  Icon     => Stock_Media_Next,
                  Label    => "_Next",
                  Tip      => "Run to the next message"
               );
            Step_Button.Set_Can_Default (Standard.True);
            Run_Button :=
               Add_Button_From_Stock
               (  Dialog   => Trace_Dialog,
                  Response => Gtk_Response_Cancel,
                  Icon     => Stock_Media_Record,
                  Label    => "_Continue",
                  Tip      => "Continue without breaking"
               );
            Button :=
               Add_Button_From_Stock
               (  Dialog   => Trace_Dialog,
                  Response => Gtk_Response_Close,
                  Icon     => Stock_Quit,
                  Label    => "_Close",
                  Tip      => "Close trace window and continue"
               );
            Dialog_Handlers.Connect
            (  Trace_Dialog,
               "response",
               On_Response'Access
            );
            Trace_Dialog.Set_Default_Size (200, 400);
            Trace_Dialog.Show_All;
            Trace_Dialog.Set_Transient_For (Get_Parent);
         end;
      end if;
      declare
--           Max_Buffer_Size : constant := 10_000;
         Offset : constant GInt := Messages_List.Get_Char_Count;
      begin
--           while Offset > Max_Buffer_Size loop
--              Messages_List.Get_Iter_At_Line (Start, 0);
--              Messages_List.Get_End_Iter (Stop);
--              Messages_List.Get_Iter_At_Line (Stop, 1);
--              Messages_List.Delete (Start, Stop);
--              Offset := Messages_List.Get_Char_Count;
--           end loop;
         Messages_List.Get_End_Iter (Start);
--         Messages_List.Insert (Start, Data.Text);
         Insert_Alt (Messages_List, Start, Data.Text);
         Messages_List.Get_Iter_At_Offset (Start, Offset);
      end;
      if View.Scroll_To_Iter
         (  Start,
            0.25,
            Standard.False,
            0.0,
            0.0
         )
      then
         null;
      end if;
         -- Colorize new text
      if Tags (Current_Tag) = null then
         Tags (Current_Tag) := Create_Tag (Messages_List);
         Gdk.Color.Set_Property
         (  Tags (Current_Tag),
            Background_Gdk_Property,
            To_RGB
            (  Val
               (  (  Hue        => 0,
                     Luminance  => 240 * 256,
                     Saturation => 29  * 256
                  ),
                  Natural (Current_Tag),
                  Tags'Length
         )  )  );
      end if;
      declare
         Offset : constant GInt := Messages_List.Get_Char_Count;
      begin
         Messages_List.Get_Iter_At_Offset (Stop, Offset);
      end;
      Messages_List.Apply_Tag (Tags (Current_Tag), Start, Stop);
      Current_Tag := Current_Tag + 1;
      if not (Data.Break or else Break_Button.Get_Active) then
         while Events_Pending loop -- Pump messages queue
             exit when not Main_Iteration_Do (Standard.False);
         end loop;
         return;
      end if;
         -- Setting buttons
      Step_Button.Set_Sensitive (Standard.True);
      Run_Button.Set_Sensitive (Standard.True);
      Trace_Dialog.Set_Default (Step_Button);
      Trace_Dialog.Set_Modal (Standard.True);
      case Trace_Dialog.Run is
         when Gtk_Response_Cancel | Gtk_Response_OK =>
            Trace_Dialog.Set_Modal (Standard.False);
            Step_Button.Set_Sensitive (Standard.False);
            Run_Button.Set_Sensitive (Standard.False);
         when others =>
            null;
      end case;
   end Trace;

   procedure Trace
             (  Message : UTF8_String;
                Break   : Boolean := Standard.False
             )  is
      Data : aliased Trace_Request (Message'Length + 1, Break);
   begin
      Data.Text := Message & LF;
      Trace_Callback.Request (Trace'Access, Data'Access);
   end Trace;

   procedure Trace
             (  Error : Exception_Occurrence;
                Break : Boolean := Standard.True
             )  is
   begin
      Trace (Exception_Information (Error), Break);
   end Trace;

end Gtk.Main.Router;
