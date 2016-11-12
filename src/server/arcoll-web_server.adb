-----------------------------------------------------------------------------
--                               A R C O L L
--       A d a   R e l a x   C o m p o n e n t   C o l l e c t i o n
--
--                 Copyright 2009-2014 M. Grella, M. Nicola
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-----------------------------------------------------------------------------

pragma License (Modified_GPL);

with AWS.Config.Set;
with AWS.Server;
with AWS.Response.Set;

package body ARColl.Web_Server is

    procedure Initialize
      (WS_Config     : in out AWS.Config.Object) is
    begin
        AWS.Config.Set.Server_Name(WS_Config, SERVER_NAME);
        AWS.Config.Set.Server_Port(WS_Config, Server_Port);
        AWS.Config.Set.Reuse_Address(WS_Config, True);
        AWS.Config.Set.Max_Connection(WS_Config, Max_Connection);
        AWS.Config.Set.Case_Sensitive_Parameters(WS_Config, False);
    end Initialize;

    procedure Start
      (WS_Config : in out AWS.Config.Object) is

        Web_Server : AWS.Server.HTTP;
    begin
        AWS.Server.Start
          (Web_Server => Web_Server,
           Callback   => Web_Server_Callback'Unrestricted_Access,
           Config     => WS_Config);

        AWS.Server.Wait (AWS.Server.Forever);
    end Start;

    procedure Validate_URI
      (URI : String) is
    begin

        if URI = "/parse" then

            null;

        elsif URI in "" | "/" then
            raise Server_Not_Found_Error
              with "Invalid URI: /";

        else
            raise Server_Not_Found_Error
              with "Unexpected URI: " & URI;
        end if;

    end Validate_URI;

    function Get_Body_Content
      (Request : AWS.Status.Data) return String_Access is

        use  AWS;

        Binary_Size   : constant ADA.Streams.Stream_Element_Offset := AWS.Status.Binary_Size (Request);
        Stream_Buffer : ADA.Streams.Stream_Element_Array (1 .. Binary_Size);
        Stream_Index  : ADA.Streams.Stream_Element_Offset := 1;
        Str_Index     : Integer := 1;
    begin

        return Body_Content : constant String_Access := new String (1 .. Integer (Binary_Size)) do

            while True loop
                AWS.Status.Read_Body (Request, Stream_Buffer, Stream_Index);

                for I in 1 .. Stream_Index loop
                    Body_Content (Str_Index) := Character'Val (Stream_Buffer (I));
                    Str_Index := Str_Index + 1;
                end loop;

                exit when Integer (Stream_Index) < 1;
            end loop;

        end return;

    end Get_Body_Content;

    function Build_Response
      (Body_Response : in Unbounded_String) return AWS.Response.Data is

        Ret_Stream  : Access_To_Stream := new Ada.Streams.Stream_Element_Array
          (Ada.Streams.Stream_Element_Offset (1)
           .. Ada.Streams.Stream_Element_Offset (Length (Body_Response)));
    begin

        return Ret_Data : AWS.Response.Data do

            for I in 1 .. Length (Body_Response) loop
                Ret_Stream (Ada.Streams.Stream_Element_Offset (I))
                  := Character'Pos (Element (Body_Response, I));
            end loop;

            Ret_Data := AWS.Response.Build
              ("application/json;charset=utf-8", Ret_Stream.all);

            AWS.Response.Set.Add_Header
              (Ret_Data, "Access-Control-Allow-Origin", "*");

            Free (Ret_Stream);

        end return;

    exception

        when others =>
            Free (Ret_Stream);
            raise;

    end Build_Response;

    function Web_Server_Callback
      (Request             : AWS.Status.Data)
       return AWS.Response.Data is

        Body_Response : Unbounded_String;
    begin

        Validate_URI(URI => AWS.Status.URI(Request));

        return Build_Response(Body_Response => Body_Response);

    end Web_Server_Callback;

end ARColl.Web_Server;
