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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Text_IO;

private with GNAT.Sockets;

package ARColl.Interfaces.Redis_Client is
-- http://redis.io/topics/protocol
-- https://gist.github.com/kylelk/a55ed824cc5739f060509e3b5169ba05 (not working)

    -----
    -- Main Redis Type
    -----

    type Redis_Type is tagged private;

    -----
    -- Basic types
    -----

    pragma Suppress (Tampering_Check);
    package String_Sets is new
      Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => "=");
    -- Sets of String

    pragma Suppress (Tampering_Check);
    package String_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");
    -- String => String maps

    pragma Suppress (Tampering_Check);
    package Double_String_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String_Maps.Map,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=",
         "="             => String_Maps."=");
    -- String => (String => String) maps

    type Integer_64 is range -(2 ** 63) .. +(2 ** 63 - 1);

    subtype Natural_64 is Integer_64 range 0 .. Integer_64'Last;

    -----
    -- RESP Elements
    -----

    type RESP_Element_Type is interface;
    -- Basic RESP (REdis Serialization Protocol) element

    type RESP_String_Type is new RESP_Element_Type with
        record
            Value : Unbounded_String;
        end record;

    type RESP_Simple_String_Type is new RESP_String_Type with null record;

    type RESP_Bulk_String_Type is new RESP_String_Type with null record;

    type RESP_Integer_Type is new RESP_Element_Type with
        record
            Value : Integer_64;
        end record;

    pragma Suppress (Tampering_Check);
    package RESP_Elements_Vectors is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => RESP_Element_Type'Class);

    type RESP_Array_Type is new RESP_Element_Type with
        record
            Elements : RESP_Elements_Vectors.Vector;
        end record;

    type RESP_Null_Type is new RESP_Element_Type with null record;

    type RESP_Null_Bulk_String_Type is new RESP_Null_Type with null record;

    type RESP_Null_Array_Type is new RESP_Null_Type with null record;

    -----
    -- INFO
    -----

    -- Data returned by INFO command
    type Redis_Info_Type is
        record

            Raw          : String_Sets.Set;
            -- Non-splittable elements

            Single_Level : String_Maps.Map;
            -- Key => Value pairs

            Double_Level : Double_String_Maps.Map;
            -- Key => (Key => Value)
        end record;

    -----
    -- Connection
    -----

    procedure Connect
      (Redis : in out Redis_Type;
       Host  : String;
       Port  : Positive)
      with Inline;

    procedure Quit_And_Disconnect (Redis : in out Redis_Type)
      with Inline;

    -----
    -- Commands
    -----

    function INFO (Redis : Redis_Type) return Redis_Info_Type
      with Inline;
    -- The INFO command returns information and statistics about the server
    -- in a format that is simple to parse by computers and easy to read
    -- by humans.

    function HGETALL (Redis : Redis_Type; Key : String) return String_Maps.Map
      with Inline;
    -- Returns all fields and values of the hash stored at Key. In the
    -- returned value, every field name is followed by its value, so the
    -- length of the reply is twice the size of the hash.
    --
    -- Return value: Array reply:
    -- list of fields and their values stored in the hash, or an empty
    -- list when key does not exist.

    function HSET (Redis : Redis_Type; Key, Field : String; Value : String) return Natural
      with Inline;
    -- Sets Field in the hash stored at Key to Value. If Key does not exist,
    -- a new key holding a hash is created. If Field already exists in the
    -- hash, it is overwritten.
    --
    -- Return value: Integer reply, specifically:
    --     1 if field is a new field in the hash and value was set.
    --     0 if field already exists in the hash and the value was updated.

    procedure HSET (Redis : Redis_Type; Key, Field : String; Value : String)
      with Inline;
    -- Sets Field in the hash stored at Key to Value. If Key does not exist,
    -- a new key holding a hash is created. If Field already exists in the
    -- hash, it is overwritten.

    function HINCRBY (Redis : Redis_Type; Key, Field : String; Increment : String) return Integer_64
      with Inline;
    -- Increments the number stored at Field in the hash stored at Key by
    -- Increment. If Key does not exist, a new key holding a hash is created.
    -- If Field does not exist the value is set to 0 before the operation is
    -- performed.
    --
    -- The range of values supported by HINCRBY is limited to 64 bit signed
    -- integers.

    function GET (Redis : Redis_Type; Key : String) return RESP_Element_Type'Class
      with Inline;
    -- Get the value of key. If the key does not exist the special value nil
    -- is returned. An error is returned if the value stored at key is not a
    -- string, because GET only handles string values.
    --
    -- Return value: Bulk string reply: the value of key, or nil when key
    --               does not exist.

    function INCRBY (Redis : Redis_Type; Key : String; Increment : String) return Integer_64
      with Inline;
    -- Increments the number stored at Key by Increment. If the Key does not
    -- exist, it is set to 0 before performing the operation. An error is
    -- returned if the key contains a value of the wrong type or contains a
    -- string that can not be represented as integer. This operation is
    -- limited to 64 bit signed integers.
    -- See INCR for extra information on increment/decrement operations.
    --
    -- Return value: Integer reply: the value of key after the increment

    procedure INCRBY (Redis : Redis_Type; Key : String; Increment : String)
      with Inline;
    -- Increments the number stored at Key by Increment. If the Key does not
    -- exist, it is set to 0 before performing the operation. An error is
    -- returned if the key contains a value of the wrong type or contains a
    -- string that can not be represented as integer. This operation is
    -- limited to 64 bit signed integers.
    -- See INCR for extra information on increment/decrement operations.

    -----
    -- Exceptions
    -----

    Redis_RESP_Error,
    -- Raised when Redis server send a RESP Error

    Command_Error,
    -- Raised when a command encounter an unexpected value

    Decoding_Error,
    -- Raised when the decoding process encounter an unexpected value

    Connection_Error : exception;
    -- Errors related to sockets and connection

    -----
    -- Printing
    -----

    package Printing is

        procedure Print
          (Set    : String_Sets.Set;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output);

        procedure Print
          (Map    : String_Maps.Map;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output);

        procedure Print
          (Map    : Double_String_Maps.Map;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output);

        procedure Print
          (Info   : Redis_Info_Type;
           Indent : Natural := 0;
           File   : Text_IO.File_Type := Text_IO.Standard_Output);

    end Printing;

private

    -----
    -- Redis Type
    -----

    type Redis_Type is tagged
        record
            Host : Unbounded_String;
            Port : GNAT.Sockets.Port_Type := 0;

            Is_Connected : Boolean := False;

            Address      : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
            Socket       : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
            Channel      : GNAT.Sockets.Stream_Access := null;
        end record;

    -----
    -- Private RESP Elements
    -----

    type RESP_Error_Type is new RESP_Element_Type with
        record
            Error_Message : Unbounded_String;
        end record;

    -----
    -- Procedures
    -----

    function Send_Command
      (Redis : Redis_Type; Command : String)
       return RESP_Element_Type'Class
      with Inline;
    -- Sends a command and decode response

    function Read (Redis : Redis_Type) return RESP_Element_Type'Class
      with Inline;
    -- Main decoding function

    function Read_Bytes
      (Redis : Redis_Type; Num_Of_Bytes : Natural_64)
       return Unbounded_String
      with Inline;
    -- Reads a precise number of bytes.

    function Read_Until_CRLF (Redis : Redis_Type) return Unbounded_String
      with Inline;
    -- Reads a sequence of characters until CRLF is encountered.
    -- In any position, CR or LF characters are excluded from the result.

    function Read_Character (Redis : Redis_Type) return Character
      with Inline;
    -- Reads a single character from the stream.

    function Escape
      (S : String)
       return String
      with Inline;
    -- Escape " char, if necessary

    Null_Redis_Type : constant Redis_Type
      := (Host         => Null_Unbounded_String,
          Port         => 0,
          Is_Connected => False,
          Address      => GNAT.Sockets.No_Sock_Addr,
          Socket       => GNAT.Sockets.No_Socket,
          Channel      => null);

end ARColl.Interfaces.Redis_Client;
