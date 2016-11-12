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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Text_IO;

package body ARColl.Readers.JSON is

    procedure Destroy
      (JSON_Element : in out JSON_Element_Class_Access) is
    begin
        if JSON_Element = null then
            return;
        end if;

        if JSON_Element.all in JSON_Array_Type then
            declare
                JSON_Array : constant access JSON_Array_Type
                  := JSON_Array_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                for I in JSON_Array.Vector.First_Index .. JSON_Array.Vector.Last_Index loop
                    declare
                        JSON_Item : JSON_Element_Class_Access
                          := JSON_Array.Vector.Element (I);
                    begin
                        Destroy (JSON_Item);
                    end;
                end loop;
                JSON_Array.Vector.Clear;
                JSON_Array.Vector.Reserve_Capacity (0);
            end;

        elsif JSON_Element.all in JSON_Object_Type then
            declare
                JSON_Object : constant access JSON_Object_Type
                  := JSON_Object_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                for Position in JSON_Object.Map.Iterate loop
                    declare
                        JSON_Item : JSON_Element_Class_Access
                          := Element_Hashed_Maps.Element (Position);
                    begin
                        Destroy (JSON_Item);
                    end;
                end loop;
                JSON_Object.Map.Clear;
                JSON_Object.Map.Reserve_Capacity (0);
            end;
        end if;

        Unchecked_Deallocation (JSON_Element);
    end Destroy;

    function Copy
      (JSON_Element : in JSON_Element_Class_Access)
       return JSON_Element_Class_Access is

        New_Element : JSON_Element_Class_Access;
    begin
        if JSON_Element = null then
            return null;
        end if;

        if JSON_Element.all in JSON_Null_Type then
            New_Element := new JSON_Null_Type;

        elsif JSON_Element.all in JSON_Boolean_Type then
            New_Element := New_Boolean (Value (JSON_Element));

        elsif JSON_Element.all in JSON_Number_Type then
            New_Element := New_Number (Long_Long_Float'(Value (JSON_Element)));

        elsif JSON_Element.all in JSON_Number_Type then
            New_Element := New_Number (Long_Long_Float'(Value (JSON_Element)));

        elsif JSON_Element.all in JSON_String_Type then
            New_Element := New_String (Unbounded_String'(Value (JSON_Element)));

        elsif JSON_Element.all in JSON_Array_Type then
            New_Element := new JSON_Array_Type;

            for Item of Vector (JSON_Element) loop
                Append (New_Element, Copy (Item));
            end loop;

        elsif JSON_Element.all in JSON_Object_Type then
            New_Element := new JSON_Object_Type;

            declare
                JMap : constant Element_Hashed_Maps.Map
                  := Map (JSON_Element);
            begin

                for Position in JMap.Iterate loop
                    Insert
                      (New_Element,
                       Element_Hashed_Maps.Key (Position),
                       Copy (Element_Hashed_Maps.Element (Position)));
                end loop;
            end;

        else
            raise JSON_Error with "unexpected/unimplemented json element type";
        end if;

        return New_Element;

    end Copy;

    procedure Set_Boolean
      (JSON_Element : in out JSON_Boolean_Type;
       Value        : in     Boolean) is
    begin
        JSON_Element.Value := Value;
    end Set_Boolean;

    function New_Boolean (Value : Boolean) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Boolean_Type;
            JSON_Boolean_Type (Ret.all).Value := Value;
        end return;
    end New_Boolean;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Boolean is
    begin
        if JSON_Element.all not in JSON_Boolean_Type then
            raise JSON_Error
              with "Attempt to get Boolean value from a non-boolean element";
        end if;

        return JSON_Boolean_Type (JSON_Element.all).Value;
    end Value;

    procedure Set_Number
      (JSON_Element : in out JSON_Number_Type;
       Value        : in     Long_Long_Float) is
    begin
        JSON_Element.Value := Value;
    end Set_Number;

    function New_Number (N : Float) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float(N);
        end return;
    end New_Number;

    function New_Number (N : Long_Float) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float(N);
        end return;
    end New_Number;

    function New_Number (N : Long_Long_Float) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := N;
        end return;
    end New_Number;

    function New_Number (N : Integer) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float (N);
        end return;
    end New_Number;

    function New_Number (N : Long_Integer) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float (N);
        end return;
    end New_Number;

    function New_Number (N : Long_Long_Integer) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float (N);
        end return;
    end New_Number;

    function New_Number (N : Ada.Containers.Count_Type) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_Number_Type;
            JSON_Number_Type (Ret.all).Value := Long_Long_Float (N);
        end return;
    end New_Number;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Integer is
    begin
        if JSON_Element.all not in JSON_Number_Type then
            raise JSON_Error
              with "Attempt to get Integer value from a non-number element";
        end if;

        return Integer (JSON_Number_Type (JSON_Element.all).Value);
    end Value;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Float is
    begin
        if JSON_Element.all not in JSON_Number_Type then
            raise JSON_Error
              with "Attempt to get Float value from a non-number element";
        end if;

        return Float (JSON_Number_Type (JSON_Element.all).Value);
    end Value;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Float is
    begin
        if JSON_Element.all not in JSON_Number_Type then
            raise JSON_Error
              with "Attempt to get Long_Float value from a non-number element";
        end if;

        return Long_Float (JSON_Number_Type (JSON_Element.all).Value);
    end Value;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Long_Float is
    begin
        if JSON_Element.all not in JSON_Number_Type then
            raise JSON_Error
              with "Attempt to get Long_Long_Float value from a non-number element";
        end if;

        return JSON_Number_Type (JSON_Element.all).Value;
    end Value;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Integer is
    begin
        if JSON_Element.all not in JSON_Number_Type then
            raise JSON_Error
              with "Attempt to get Long_Integer value from a non-number element";
        end if;

        return Long_Integer (JSON_Number_Type (JSON_Element.all).Value);
    end Value;

    procedure Set_String
      (JSON_Element : in out JSON_String_Type;
       Value        : in     String) is
    begin
        JSON_Element.Value := To_Unbounded_String (Value);
    end Set_String;

    procedure Set_String
      (JSON_Element : in out JSON_String_Type;
       Value        : in     Unbounded_String) is
    begin
        JSON_Element.Value := Value;
    end Set_String;

    function New_String (S : String) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_String_Type;
            JSON_String_Type (Ret.all).Value := To_Unbounded_String(S);
        end return;
    end New_String;

    function New_String (S : Unbounded_String) return JSON_Element_Class_Access is
    begin
        return Ret : JSON_Element_Class_Access do
            Ret := new JSON_String_Type;
            JSON_String_Type (Ret.all).Value := S;
        end return;
    end New_String;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Unbounded_String is
    begin
        if JSON_Element.all not in JSON_String_Type then
            raise JSON_Error
              with "Attempt to get String value from a non-string element";
        end if;

        return JSON_String_Type (JSON_Element.all).Value;
    end Value;

    function Length
      (JSON_Array : not null JSON_Element_Class_Access)
       return Ada.Containers.Count_Type is
    begin
        if JSON_Array.all not in JSON_Array_Type then
            raise JSON_Error
              with "Attempt to get length from a non-array element";
        end if;

        return JSON_Array_Type (JSON_Array.all).Vector.Length;
    end Length;

    procedure Append
      (JSON_Array : not null JSON_Element_Class_Access;
       Item       : not null JSON_Element_Class_Access) is
    begin
        if JSON_Array.all not in JSON_Array_Type then
            raise JSON_Error
              with "Attempt to append an item to a non-array element";
        end if;

        JSON_Array_Type (JSON_Array.all).Vector.Append (Item);
    end Append;

    function Vector
      (JSON_Element : not null JSON_Element_Class_Access)
       return Element_Vectors.Vector is
    begin
        if JSON_Element.all not in JSON_Array_Type then
            raise JSON_Error
              with "Attempt to get Vector value from a non-array element";
        end if;

        return JSON_Array_Type (JSON_Element.all).Vector;
    end Vector;

    function Has_Key
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String)
       return Boolean is
    begin
        if JSON_Object.all not in JSON_Object_Type then
            raise JSON_Error
              with "Attempt to check key existence from a non-object element";
        end if;

        return JSON_Object_Type (JSON_Object.all).Map.Contains (Key);
    end Has_Key;

    procedure Insert
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String;
       Item        : not null JSON_Element_Class_Access) is
    begin
        if JSON_Object.all not in JSON_Object_Type then
            raise JSON_Error
              with "Attempt to insert key/item to a non-object element";
        end if;

        JSON_Object_Type (JSON_Object.all).Map.Insert (Key, Item);
    end Insert;

    function Element
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String)
       return not null JSON_Element_Class_Access is
    begin
        if JSON_Object.all not in JSON_Object_Type then
            raise JSON_Error
              with "Attempt to get element from a non-object element";
        end if;

        return JSON_Object_Type (JSON_Object.all).Map.Element (Key);
    exception
        when others =>
            Text_IO.Put_Line
              (Text_IO.Standard_Error, ASCII.LF &
                 "ERROR JSON.Element for key """ & Key & """");
            raise;
    end Element;

    function Map
      (JSON_Element : JSON_Element_Class_Access)
       return Element_Hashed_Maps.Map is
    begin
        if JSON_Element.all not in JSON_Object_Type then
            raise JSON_Error
              with "Attempt to get Map value from a non-object element";
        end if;

        return JSON_Object_Type (JSON_Element.all).Map;
    end Map;

    function JSON_Decode
      (Input : in String)
       return JSON_Element_Class_Access is

        String_Buffer : String_Access_Type := new String (1 .. Input'Length);

        Input_Access : constant String_Access_Type := Input'Unrestricted_Access;

        Char_Index : Natural := Input'First;
    begin
        return JSON_Element : JSON_Element_Class_Access do
            JSON_Element := Decode (Input_Access, Char_Index, String_Buffer);

            Skip_Spaces (Input_Access, Char_Index);

            if Char_Index /= Input'Last + 1 then
                raise JSON_Error with
                  "unknown sequence from position" & Char_Index'Img;
            end if;

            Free (String_Buffer);
        exception
            when others =>
                Destroy (JSON_Element);
                Free (String_Buffer);
                raise;
        end return;
    end JSON_Decode;

    function Decode
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access is

    begin

        while Char_Index <= Input'Last loop
            case Input (Char_Index) is

                when ' ' | ASCII.HT | ASCII.LF | ASCII.CR =>
                    Char_Index := Char_Index + 1; -- Skip a space

                when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' =>
                    return Decode_Number (Input, Char_Index);

                when '"' =>
                    declare
                        JSON_String : JSON_Element_Class_Access
                          := new JSON_String_Type;
                    begin
                        Set_String
                          (JSON_String_Type (JSON_String.all),
                           Decode_String (Input, Char_Index, String_Buffer));

                        return JSON_String;
                    exception
                        when others =>
                            Destroy (JSON_String);
                            raise;
                    end;
                when 'n' =>
                    return Decode_Null (Input, Char_Index);

                when 't' =>
                    return Decode_True (Input, Char_Index);

                when 'f' =>
                    return Decode_False (Input, Char_Index);

                when '[' =>
                    return Decode_Array (Input, Char_Index, String_Buffer);

                when '{' =>
                    return Decode_Object (Input, Char_Index, String_Buffer);

                when others =>
                    raise JSON_Error with
                      "invalid character at position" & Char_Index'Img;
            end case;
        end loop;

        raise JSON_Error with
          "bad sequence at position" & Char_Index'Img;
    end Decode;

    procedure Skip_Spaces
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural) is
    begin
        while Char_Index <= Input'Last
          and then Input (Char_Index) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
            Char_Index := Char_Index + 1;
        end loop;
    end Skip_Spaces;

    function Decode_Null
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access is
    begin
        if Input'Last < Char_Index + 3
          or else Input (Char_Index .. Char_Index + 3) /= "null" then
            raise JSON_Error with
              "invalid character at position" & Char_Index'Img &
              " (possible mispelling of 'null')";
        end if;

        Char_Index := Char_Index + 4;

        return new JSON_Null_Type;
    end Decode_Null;

    function Decode_True
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access is
    begin
        if Input'Last < Char_Index + 3
          or else Input (Char_Index .. Char_Index + 3) /= "true" then
            raise JSON_Error with
              "invalid character at position" & Char_Index'Img &
              " (possible mispelling of 'true')";
        end if;

        Char_Index := Char_Index + 4;

        return JSON_Boolean : JSON_Element_Class_Access do
            JSON_Boolean := new JSON_Boolean_Type;
            Set_Boolean (JSON_Boolean_Type (JSON_Boolean.all), True);
        end return;
    end Decode_True;

    function Decode_False
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access is
    begin
        if Input'Last < Char_Index + 4
          or else Input (Char_Index .. Char_Index + 4) /= "false" then
            raise JSON_Error with
              "invalid character at position" & Char_Index'Img &
              " (possible mispelling of 'false')";
        end if;

        Char_Index := Char_Index + 5;

        return JSON_Boolean : JSON_Element_Class_Access do
            JSON_Boolean := new JSON_Boolean_Type;
            Set_Boolean (JSON_Boolean_Type (JSON_Boolean.all), False);
        end return;
    end Decode_False;

    function Decode_Number
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access is

        First_Index : constant Natural := Char_Index;
    begin
        while Char_Index <= Input'Last
          and then Input (Char_Index) in '-' | '+' | '.' | 'e' | 'E' |
          '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' loop
            Char_Index := Char_Index + 1;
        end loop;

        if Char_Index - 1 < First_Index then
            raise JSON_Error with
              "invalid sequence starting from position" & First_Index'Img &
              " (possible mispelling of a number)";
        end if;
        return JSON_Number : JSON_Element_Class_Access do
            JSON_Number := new JSON_Number_Type;
            Set_Number
              (JSON_Number_Type (JSON_Number.all),
               Long_Long_Float'Value (Input (First_Index .. Char_Index - 1)));
        exception
            when others =>
                Destroy (JSON_Number);
                raise;
        end return;
    end Decode_Number;

    function Decode_String
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return String is

        Last : Natural := String_Buffer'First - 1;
    begin

        -- Skip '"'
        Char_Index := Char_Index + 1;

        loop

            if Char_Index > Input'Last then
                raise JSON_Error with "invalid string at the end of line";
            end if;

            case Input (Char_Index) is

                when '\' =>
                    Char_Index := Char_Index + 1;
                    if Char_Index > Input'Last then
                        raise JSON_Error with "invalid string at the end of line";
                    end if;

                    case Input (Char_Index) is
                        when '"' | '\' | '/' =>
                            Last := Last + 1;
                            String_Buffer (Last) := Input (Char_Index);
                            Char_Index := Char_Index + 1;

                        when 'b' =>
                            Last := Last + 1;
                            String_Buffer (Last) := ASCII.BS;
                            Char_Index := Char_Index + 1;

                        when 'f' =>
                            Last := Last + 1;
                            String_Buffer (Last) := ASCII.FF;
                            Char_Index := Char_Index + 1;

                        when 'n' =>
                            Last := Last + 1;
                            String_Buffer (Last) := ASCII.LF;
                            Char_Index := Char_Index + 1;

                        when 'r' =>
                            Last := Last + 1;
                            String_Buffer (Last) := ASCII.CR;
                            Char_Index := Char_Index + 1;

                        when 't' =>
                            Last := Last + 1;
                            String_Buffer (Last) := ASCII.HT;
                            Char_Index := Char_Index + 1;

                        when 'u' =>
                            -- \u####
                            declare

                                function From_Hex (C : Character; Position : Natural) return Natural is
                                begin
                                    case C is
                                        when '0'       => return 0  * (16 ** Position);
                                        when '1'       => return 1  * (16 ** Position);
                                        when '2'       => return 2  * (16 ** Position);
                                        when '3'       => return 3  * (16 ** Position);
                                        when '4'       => return 4  * (16 ** Position);
                                        when '5'       => return 5  * (16 ** Position);
                                        when '6'       => return 6  * (16 ** Position);
                                        when '7'       => return 7  * (16 ** Position);
                                        when '8'       => return 8  * (16 ** Position);
                                        when '9'       => return 9  * (16 ** Position);
                                        when 'A' | 'a' => return 10 * (16 ** Position);
                                        when 'B' | 'b' => return 11 * (16 ** Position);
                                        when 'C' | 'c' => return 12 * (16 ** Position);
                                        when 'D' | 'd' => return 13 * (16 ** Position);
                                        when 'E' | 'e' => return 14 * (16 ** Position);
                                        when 'F' | 'f' => return 15 * (16 ** Position);
                                        when others => raise JSON_Error with "invalid hex numebr";
                                    end case;
                                end From_Hex;

                                Str_Code : constant String (1 .. 4)
                                  := Input (Char_Index + 1 .. Char_Index + 4);

                                Num_Code : constant Natural
                                  := From_Hex (Str_Code (4), 0) + From_Hex (Str_Code (3), 1) + From_Hex (Str_Code (2), 2) + From_Hex (Str_Code (1), 3);

                                WWC : constant Wide_Wide_String
                                  := Wide_Wide_Character'Val (Num_Code) & "";
                            begin

                                declare
                                    Str_Encoded : constant String
                                      := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (WWC);
                                begin

                                    for C of Str_Encoded loop
                                        Last := Last + 1;
                                        String_Buffer (Last) := C;
                                    end loop;

                                    Char_Index := Char_Index + 5;
                                end;

                            exception
                                when Ada.Strings.UTF_Encoding.Encoding_Error =>
                                    Text_IO.Put_Line
                                      (Text_IO.Standard_Error,
                                       "WARNING: cannot decode JSON sequence \u" & Str_Code);

                                    Last := Last + 1;
                                    String_Buffer (Last) := ' ';

                                    Char_Index := Char_Index + 5;
                            end;

                        when others =>
                            raise JSON_Error with "Invalid string escape sequence at position" & Char_Index'Img;
                    end case;

                when '"' =>
                    Char_Index := Char_Index + 1;
                    exit; -- break loop

                when others =>
                    Last := Last + 1;
                    String_Buffer (Last) := Input (Char_Index);
                    Char_Index := Char_Index + 1;
            end case;
        end loop;

        return String_Buffer (String_Buffer'First .. Last);

    end Decode_String;

    function Decode_Array
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access is

        JSON_Array : JSON_Element_Class_Access;
        JSON_Array_Acc : access JSON_Array_Type;
    begin
        JSON_Array := new JSON_Array_Type;
        JSON_Array_Acc := JSON_Array_Type (JSON_Array.all)'Unrestricted_Access;

        -- Skip '['
        Char_Index := Char_Index + 1;

        loop

            Skip_Spaces (Input, Char_Index);

            if Char_Index > Input'Last then
                raise JSON_Error with "incomplete array at the end of line";
            end if;

            if Input (Char_Index) /= ']' then
                JSON_Array_Acc.Vector.Append
                  (Decode (Input, Char_Index, String_Buffer));

                Skip_Spaces (Input, Char_Index);
            end if;

            if Char_Index > Input'Last then
                raise JSON_Error with "incomplete array at the end of line";
            end if;

            case Input (Char_Index) is
                when ']' =>
                    Char_Index := Char_Index + 1;
                    exit; -- break loop

                when ',' =>
                    Char_Index := Char_Index + 1;

                when others =>
                    raise JSON_Error with
                      "invalid character at position" & Char_Index'Img;
            end case;

        end loop;

        return JSON_Array;
    exception
        when others =>
            Destroy (JSON_Array);
            raise;

    end Decode_Array;

    function Decode_Object
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access is

        JSON_Object     : JSON_Element_Class_Access;
        JSON_Object_Acc : access JSON_Object_Type;
    begin
        JSON_Object := new JSON_Object_Type;
        JSON_Object_Acc := JSON_Object_Type (JSON_Object.all)'Unrestricted_Access;

        -- Skip '{'
        Char_Index := Char_Index + 1;

        loop
            Skip_Spaces (Input, Char_Index);

            if Char_Index > Input'Last then
                raise JSON_Error with "incomplete object at the end of line";
            end if;

            if Input (Char_Index) = '}' then
                Char_Index := Char_Index + 1;
                exit; -- break loop

            elsif Input (Char_Index) /= '"' then
                raise JSON_Error with
                  "invalid character at position" & Char_Index'Img &
                  " (key-string expected)";
            end if;

            declare
                Key : constant String
                  := Decode_String (Input, Char_Index, String_Buffer);
            begin
                Skip_Spaces (Input, Char_Index);

                if Char_Index > Input'Last then
                    raise JSON_Error with "incomplete object at the end of line";
                end if;

                if Input (Char_Index) /= ':' then
                    raise JSON_Error with
                      "invalid character at position" & Char_Index'Img &
                      " (':' expected)";
                end if;

                Char_Index := Char_Index + 1; -- Skip ':'

                Skip_Spaces (Input, Char_Index);

                if Char_Index > Input'Last then
                    raise JSON_Error with "incomplete object at the end of line";
                end if;

                JSON_Object_Acc.Map.Insert
                  (Key,
                   Decode (Input, Char_Index, String_Buffer));

                Skip_Spaces (Input, Char_Index);

                if Char_Index > Input'Last then
                    raise JSON_Error with "incomplete object at the end of line";
                end if;


                case Input (Char_Index) is
                    when '}' =>
                        Char_Index := Char_Index + 1;
                        exit; -- break loop

                    when ',' =>
                        Char_Index := Char_Index + 1;

                    when others =>
                        raise JSON_Error with
                          "invalid character at position" & Char_Index'Img;
                end case;

            end;

        end loop;

        return JSON_Object;
    exception
        when others =>
            Destroy (JSON_Object);
            raise;
    end Decode_Object;

    procedure String_Encode
      (Input  : in     String;
       Output : in out Unbounded_String) is
    begin
        declare
            Encoded_String : String_Access_Type
              := new String (1 .. (Input'Length * 2) + 2);
            -- "* 2" => max length in worst case; "+ 2" => first and last '"'
            P              : Natural := Encoded_String'First - 1;
        begin
            P := P + 1;
            Encoded_String (P) := '"';

            for C of Input loop
                case C is
                    when '"'      => P := P + 2; Encoded_String (P - 1 .. P) := "\""";
                    when '\'      => P := P + 2; Encoded_String (P - 1 .. P) := "\\";
                    when '/'      => P := P + 2; Encoded_String (P - 1 .. P) := "\/";
                    when ASCII.BS => P := P + 2; Encoded_String (P - 1 .. P) := "\b";
                    when ASCII.FF => P := P + 2; Encoded_String (P - 1 .. P) := "\f";
                    when ASCII.LF => P := P + 2; Encoded_String (P - 1 .. P) := "\n";
                    when ASCII.CR => P := P + 2; Encoded_String (P - 1 .. P) := "\r";
                    when ASCII.HT => P := P + 2; Encoded_String (P - 1 .. P) := "\t";
                    when others   => P := P + 1; Encoded_String (P) := C;
                end case;
            end loop;

            P := P + 1;
            Encoded_String (P) := '"';

            Append (Output, Encoded_String (Encoded_String'First .. P));

            Free (Encoded_String);
        exception
            when others =>
                Free (Encoded_String);
                raise;
        end;
    end String_Encode;

    procedure JSON_Encode
      (JSON_Element : in     JSON_Element_Class_Access;
       Output         : in out Unbounded_String) is
    begin

        if JSON_Element.all in JSON_Null_Type then
            Append (Output, "null");

        elsif JSON_Element.all in JSON_Boolean_Type then
            declare
                JSON_Boolean : constant access JSON_Boolean_Type
                  := JSON_Boolean_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                if JSON_Boolean.Value then
                    Append (Output, "true");
                else
                    Append (Output, "false");
                end if;
            end;

        elsif JSON_Element.all in JSON_String_Type then
            declare
                JSON_String : constant access JSON_String_Type
                  := JSON_String_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                String_Encode (To_String(JSON_String.Value), Output);
            end;

        elsif JSON_Element.all in JSON_Number_Type then
            declare
                JSON_Number : constant access JSON_Number_Type
                  := JSON_Number_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                if Long_Long_Float'Floor(JSON_Number.Value) = JSON_Number.Value
                  and then JSON_Number.Value < Long_Long_Float (Long_Long_Integer'Last)
                  and then JSON_Number.Value > Long_Long_Float (Long_Long_Integer'First) then -- Whole number
                    declare
                        Str_Num : constant String
                          := Long_Long_Integer (Long_Long_Float'Floor (JSON_Number.Value))'Img;
                    begin
                        if Str_Num (Str_Num'First) = ' ' then
                            Append (Output, Str_Num (Str_Num'First + 1 .. Str_Num'Last));
                        else
                            Append (Output, Str_Num);
                        end if;
                    end;
                else
                    -- FIXME: this is not the best way to represent number!
                    declare
                        Str_Num : constant String
                          := JSON_Number.Value'Img;
                    begin
                        if Str_Num (Str_Num'First) = ' ' then
                            Append (Output, Str_Num (Str_Num'First + 1 .. Str_Num'Last));
                        else
                            Append (Output, Str_Num);
                        end if;
                    end;
                end if;
            end;

        elsif JSON_Element.all in JSON_Array_Type then
            declare
                JSON_Array : constant access JSON_Array_Type
                  := JSON_Array_Type (JSON_Element.all)'Unrestricted_Access;
            begin
                Append (Output, "[");

                for I in JSON_Array.Vector.First_Index .. JSON_Array.Vector.Last_Index loop
                    if I > JSON_Array.Vector.First_Index then
                        Append (Output, ',');
                    end if;

                    JSON_Encode (JSON_Array.Vector.Element (I), Output);
                end loop;

                Append (Output, "]");
            end;

        elsif JSON_Element.all in JSON_Object_Type then
            declare
                JSON_Object : constant access JSON_Object_Type
                  := JSON_Object_Type (JSON_Element.all)'Unrestricted_Access;

                Is_First    : Boolean := True;
            begin
                Append (Output, "{");

                for Position in JSON_Object.Map.Iterate loop
                    declare
                        Key : constant String := Element_Hashed_Maps.Key (Position);
                        Value : constant JSON_Element_Class_Access := Element_Hashed_Maps.Element (Position);
                    begin
                        if Is_First then
                            Is_First := False;
                        else
                            Append (Output, ",");
                        end if;

                        String_Encode (Key, Output);
                        Append (Output, ":");
                        JSON_Encode (Value, Output);
                    end;
                end loop;

                Append (Output, "}");
            end;

        else
            raise JSON_Error with "Unknown element class";
        end if;

    end JSON_Encode;

    function JSON_Encode
      (JSON_Element : in JSON_Element_Class_Access)
       return Unbounded_String is
    begin
        return Output : Unbounded_String do
            JSON_Encode (JSON_Element, Output);
        end return;
    end JSON_Encode;

    procedure JSON_Encode
      (JSON_Element : in     JSON_Element_Class_Access;
       Output         : in out String) is
    begin
        Output := To_String (JSON_Encode (JSON_Element));
    end JSON_Encode;

    function  JSON_Encode
      (JSON_Element : in JSON_Element_Class_Access)
       return String is
    begin
        return To_String (JSON_Encode (JSON_Element));
    end JSON_Encode;

end ARColl.Readers.JSON;
