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

with GNAT.String_Split;
with Ada.Strings.Fixed;

package body ARColl.Interfaces.Redis_Client is

    procedure Connect
      (Redis : in out Redis_Type;
       Host  : String;
       Port  : Positive) is
    begin
        if Redis.Is_Connected then
            raise Connection_Error with "already connected";
        end if;

        Redis.Host := To_Unbounded_String (Host);
        Redis.Port := GNAT.Sockets.Port_Type (Port);

        Redis.Address.Addr := GNAT.Sockets.Addresses
          (GNAT.Sockets.Get_Host_By_Name (Host), 1);

        Redis.Address.Port := Redis.Port;
        GNAT.Sockets.Create_Socket (Redis.Socket);

        GNAT.Sockets.Set_Socket_Option
          (Redis.Socket,
           GNAT.Sockets.Socket_Level,
           (GNAT.Sockets.Reuse_Address, True));

        GNAT.Sockets.Connect_Socket (Redis.Socket, Redis.Address);

        Redis.Channel := GNAT.Sockets.Stream (Redis.Socket);

        Redis.Is_Connected := True;

    exception
        when others =>
            begin
                Redis.Quit_And_Disconnect;
            exception when others => null;
            end;

            raise;
    end Connect;


    procedure Quit_And_Disconnect (Redis : in out Redis_Type) is
    begin
        if not Redis.Is_Connected then
            raise Connection_Error with "not connected";
        end if;

        declare
            Response : constant RESP_Element_Type'Class
              := Redis.Send_Command ("QUIT");
        begin
            if Response not in RESP_String_Type'Class then
                raise Command_Error
                  with "QUIT returned an unexpected RESP element";
            elsif RESP_String_Type (Response).Value /= "OK" then
                raise Command_Error
                  with "QUIT returned an unexpected response: " & To_String (RESP_String_Type (Response).Value);
            end if;

            GNAT.Sockets.Close_Socket (Redis.Socket);

            Redis := Null_Redis_Type;
        end;
    exception
        when others =>
            begin
                GNAT.Sockets.Close_Socket (Redis.Socket);
            exception
                when others => null;
            end;

            Redis := Null_Redis_Type;
            raise;
    end Quit_And_Disconnect;


    function INFO (Redis : Redis_Type) return Redis_Info_Type is
        Response : constant RESP_Element_Type'Class
          := Redis.Send_Command ("INFO");

        Ret : Redis_Info_Type;
    begin

        if Response not in RESP_String_Type'Class then
            raise Command_Error
              with "INFO returned an unexpected RESP element";
        end if;

        declare
            use Ada.Strings.Fixed;

            Lines : constant String := To_String (RESP_String_Type (Response).Value);
            Lines_Slices : GNAT.String_Split.Slice_Set;
        begin
            GNAT.String_Split.Create
              (S          => Lines_Slices,
               From       => Lines,
               Separators => ASCII.CR & ASCII.LF,
               Mode       => GNAT.String_Split.Multiple);

            for Line_Index in 1 .. GNAT.String_Split.Slice_Count (Lines_Slices) loop
                declare
                    Line : constant String
                      := GNAT.String_Split.Slice (Lines_Slices, Line_Index);

                    Colon_Index : Natural := 0;

                begin
                    if Line'Length > 0 and then Line (Line'First) /= '#' then

                        Colon_Index := Index (Line, ":");

                        if Colon_Index > Line'First then

                            if Index (Line, "=") > Line'First and then
                              Index (Line, ",") > Line'First then

                                -- Double level
                                declare
                                    Sub_Level : String_Maps.Map;
                                    Sub_Slices    : GNAT.String_Split.Slice_Set;
                                begin

                                    GNAT.String_Split.Create
                                      (S          => Sub_Slices,
                                       From       => Line (Colon_Index + 1 .. Line'Last),
                                       Separators => ",",
                                       Mode       => GNAT.String_Split.Single);

                                    for Sub_Index in 1 .. GNAT.String_Split.Slice_Count (Sub_Slices) loop
                                        declare
                                            Sub_Slice : constant String
                                              := GNAT.String_Split.Slice (Sub_Slices, Sub_Index);
                                            Equals_Index : constant Positive := Index (Sub_Slice, "=");
                                        begin
                                            Sub_Level.Insert
                                              (Sub_Slice (Sub_Slice'First .. Equals_Index - 1),
                                               Sub_Slice (Equals_Index + 1 .. Sub_Slice'Last));
                                        end;
                                    end loop;

                                    Ret.Double_Level.Insert
                                      (Line (Line'First .. Colon_Index - 1),
                                       Sub_Level);
                                end;

                            else
                                -- Single level
                                Ret.Single_Level.Insert
                                  (Line (Line'First .. Colon_Index - 1),
                                   Line (Colon_Index + 1 .. Line'Last));
                            end if;

                        else
                            -- Raw
                            Ret.Raw.Insert (Line);
                        end if;
                    end if;
                end;
            end loop;
        end;

        return Ret;
    end INFO;


    function HGETALL (Redis : Redis_Type; Key : String) return String_Maps.Map is
        Response : constant RESP_Element_Type'Class
          := Redis.Send_Command ("HGETALL " & Escape (Key));

        Position : RESP_Elements_Vectors.Cursor;

        Ret : String_Maps.Map;
    begin

        if Response not in RESP_Array_Type then
            raise Command_Error
              with "HGETALL returned an unexpected RESP element";
        end if;

        Position := RESP_Array_Type (Response).Elements.First;

        while RESP_Elements_Vectors.Has_Element (Position) loop

            -- Key --
            declare
                Key_Element : constant RESP_Element_Type'Class
                  := RESP_Elements_Vectors.Element (Position);
            begin
                if Key_Element not in RESP_String_Type'Class then
                    raise Command_Error
                      with "HGETALL array contains an unexpected RESP element (key)";
                end if;

                RESP_Elements_Vectors.Next (Position);

                if not RESP_Elements_Vectors.Has_Element (Position) then
                    raise Command_Error
                      with "HGETALL array encountered a premature array termination";
                end if;

                -- Value --
                declare
                    Value_Element : constant RESP_Element_Type'Class
                      := RESP_Elements_Vectors.Element (Position);
                begin
                    if Value_Element not in RESP_String_Type'Class then
                        raise Command_Error
                          with "HGETALL array contains an unexpected RESP element (value)";
                    end if;

                    Ret.Insert
                      (To_String (RESP_String_Type (Key_Element).Value),
                       To_String (RESP_String_Type (Value_Element).Value));
                end;

                RESP_Elements_Vectors.Next (Position);
            end;
        end loop;

        return Ret;
    end HGETALL;

    function HSET (Redis : Redis_Type; Key, Field : String; Value : String) return Natural is
        Response : constant RESP_Element_Type'Class
          := Redis.Send_Command ("HGETALL " & Escape (Key) & " " & Escape (Field) & " " & Escape (Value));

        Num : Integer_64;
    begin

        if Response not in RESP_Integer_Type then
            raise Command_Error
              with "HSET returned an unexpected RESP element";
        end if;

        Num := RESP_Integer_Type(Response).Value;

        case Num is
            when 0 => return 0;
            when 1 => return 1;
            when others =>
                raise Command_Error
                  with "HSET returned an unexpected integer value: " & Num'Img;
        end case;

    end HSET;

    procedure HSET (Redis : Redis_Type; Key, Field : String; Value : String) is
        Ret : Natural;
    begin
        Ret := HSET (Redis, Key, Field, Value);
        pragma Unreferenced (Ret);
    end HSET;


    function HINCRBY (Redis : Redis_Type; Key, Field : String; Increment : String) return Integer_64 is
        Response : constant RESP_Element_Type'Class
          := Redis.Send_Command ("HINCRBY " & Escape (Key) & " " & Escape (Field) & " " & Escape (Increment));
    begin

        if Response not in RESP_Integer_Type then
            raise Command_Error
              with "HINCRBY returned an unexpected RESP element";
        end if;

        return RESP_Integer_Type (Response).Value;
    end HINCRBY;


    function GET (Redis : Redis_Type; Key : String) return RESP_Element_Type'Class is
    begin
        return Redis.Send_Command ("GET " & Escape (Key));
    end GET;


    function INCRBY (Redis : Redis_Type; Key : String; Increment : String) return Integer_64 is
        Response : constant RESP_Element_Type'Class
          := Redis.Send_Command ("INCRBY " & Escape (Key) & " " & Escape (Increment));
    begin

        if Response not in RESP_Integer_Type then
            raise Command_Error
              with "INCRBY returned an unexpected RESP element";
        end if;

        return RESP_Integer_Type (Response).Value;
    end INCRBY;


    procedure INCRBY (Redis : Redis_Type; Key : String; Increment : String) is
        Ret : Integer_64;
    begin
        Ret := Redis.INCRBY (Key, Increment);
        pragma Unreferenced (Ret);
    end INCRBY;


    function Send_Command
      (Redis : Redis_Type; Command : String)
       return RESP_Element_Type'Class is
    begin

        String'Write (Redis.Channel, Command & ASCII.CR & ASCII.LF);

        declare
            Response : constant RESP_Element_Type'Class
              := Redis.Read;
        begin

            if Response in RESP_Error_Type then
                raise Redis_RESP_Error
                  with To_String (RESP_Error_Type (Response).Error_Message);
            end if;

            return Response;
        end;
    end Send_Command;


    function Read (Redis : Redis_Type) return RESP_Element_Type'Class is
        First_Byte : constant Character
          := Redis.Read_Character;
    begin

        case First_Byte is

            when '-' => -- Errors
                declare
                    RESP_Error : RESP_Error_Type;
                begin
                    RESP_Error.Error_Message := Redis.Read_Until_CRLF;
                    return RESP_Error;
                end;

            when '+' => -- Simple Strings
                declare
                    RESP_Simple_String : RESP_Simple_String_Type;
                begin
                    RESP_Simple_String.Value := Redis.Read_Until_CRLF;
                    return RESP_Simple_String;
                end;

            when ':' => -- Integers
                declare
                    Str_Value : constant Unbounded_String
                      := Redis.Read_Until_CRLF;

                    RESP_Integer : RESP_Integer_Type;
                begin
                    RESP_Integer.Value := Integer_64'Value (To_String (Str_Value));
                    return RESP_Integer;
                end;

            when '$' => -- Bulk Strings
                declare
                    Str_Num_Of_Bytes : constant Unbounded_String
                      := Redis.Read_Until_CRLF;

                    Num_Of_Bytes : constant Integer_64
                      := Integer_64'Value (To_String (Str_Num_Of_Bytes));
                begin
                    if Num_Of_Bytes < -1 then
                        raise Decoding_Error
                          with "Unexpected bulk string size:" & Num_Of_Bytes'Img;

                    elsif Num_Of_Bytes = -1 then
                        declare
                            RESP_Null_Bulk_String : RESP_Null_Bulk_String_Type;
                        begin
                            return RESP_Null_Bulk_String;
                        end;

                    else
                        declare
                            RESP_Bulk_String : RESP_Bulk_String_Type;
                            Remaining_Data   : Unbounded_String;
                        begin
                            RESP_Bulk_String.Value := Redis.Read_Bytes (Num_Of_Bytes);

                            Remaining_Data := Redis.Read_Until_CRLF; -- Final CRLF
                            if Length (Remaining_Data) > 0 then
                                raise Decoding_Error
                                  with "Unexpected bulk string termination: " & To_String (Remaining_Data);
                            end if;

                            return RESP_Bulk_String;
                        end;
                    end if;
                end;

            when '*' => -- Arrays
                declare
                    Str_Num_Of_Elements : constant Unbounded_String
                      := Redis.Read_Until_CRLF;

                    Num_Of_Elements : constant Integer_64
                      := Integer_64'Value (To_String (Str_Num_Of_Elements));
                begin
                    if Num_Of_Elements < -1 then
                        raise Decoding_Error
                          with "Unexpected array size:" & Num_Of_Elements'Img;

                    elsif Num_Of_Elements = -1 then
                        declare
                            RESP_Null_Array : RESP_Null_Array_Type;
                        begin
                            return RESP_Null_Array;
                        end;

                    else
                        declare
                            RESP_Array : RESP_Array_Type;
                        begin
                            for Element_Count in 1 .. Num_Of_Elements loop
                                RESP_Array.Elements.Append (Redis.Read);
                            end loop;

                            return RESP_Array;
                        end;
                    end if;
                end;

            when others =>
                raise Decoding_Error
                  with "Unexpected first byte """ & First_Byte & """";
        end case;

    end Read;

    function Read_Bytes
      (Redis : Redis_Type; Num_Of_Bytes : Natural_64)
       return Unbounded_String is
        Read_Bytes : Natural_64 := 0;
    begin
        return Ret : Unbounded_String do

            while Read_Bytes < Num_Of_Bytes loop
                Append (Ret, Character'Input (Redis.Channel));
                Read_Bytes := Read_Bytes + 1;
            end loop;

        end return;
    end Read_Bytes;


    function Read_Until_CRLF (Redis : Redis_Type) return Unbounded_String is
        C      : Character := ' ';
        Prev_C : Character := ' ';
    begin
        return Line : Unbounded_String do
            loop
                C := Character'Input (Redis.Channel);

                exit when Prev_C = ASCII.CR and then C = ASCII.LF;

                if C not in ASCII.CR | ASCII.LF then
                    Append (Line, C);
                end if;

                Prev_C := C;
            end loop;

        end return;
    end Read_Until_CRLF;


    function Read_Character (Redis : Redis_Type) return Character is
    begin
        return Character'Input (Redis.Channel);
    end Read_Character;

    -----
    -- Printing
    -----

    package body Printing is

        use Text_IO;
        use Ada.Strings.Fixed;

        procedure Print
          (Set    : String_Sets.Set;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output) is

            Prefix : constant String := (Indent * ' ');
        begin
            for Item of Set loop
                Put_Line (File, Prefix & Item);
            end loop;
        end Print;

        procedure Print
          (Map    : String_Maps.Map;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output) is

            Prefix : constant String := (Indent * ' ');
        begin
            for Position in Map.Iterate loop
                declare
                    Key   : String renames String_Maps.Key (Position);
                    Value : String renames String_Maps.Element (Position);
                begin
                    Put_Line (File, Prefix & Key & ": " & Value);
                end;
            end loop;
        end Print;

        procedure Print
          (Map    : Double_String_Maps.Map;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output) is

            Prefix : constant String := (Indent * ' ');
        begin
            for Position in Map.Iterate loop
                declare
                    Key   : String renames Double_String_Maps.Key (Position);
                    Value : String_Maps.Map renames Double_String_Maps.Element (Position);
                begin
                    Put_Line (File, Prefix & Key & ": {");
                    Print (Value, Indent + 4, File);
                    Put_Line (File, Prefix & "}");
                end;
            end loop;
        end Print;

        procedure Print
          (Info   : Redis_Info_Type;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output) is
        begin
            Print (Info.Raw, Indent, File);
            Print (Info.Single_Level, Indent, File);
            Print (Info.Double_Level, Indent, File);
        end Print;

    end Printing;

    function Escape
      (S : String)
       return String is
        use Ada.Strings.Fixed;
    begin
        if Index (S, " ") = 0 and then Index (S, """") = 0 and then Index (S, "\") = 0 and then Index (S, "'") = 0 then
            return S;

        else
            declare
                New_Str : Unbounded_String;
            begin

                Append (New_Str, """");

                for C of S loop
                    if C in '"' | '\' | ''' then
                        Append (New_Str, "\");
                    end if;
                    Append (New_Str, C);
                end loop;

                Append (New_Str, """");

                return To_String (New_Str);
            end;
        end if;
    end Escape;

end ARColl.Interfaces.Redis_Client;
