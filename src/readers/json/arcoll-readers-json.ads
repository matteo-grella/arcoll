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

with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package ARColl.Readers.JSON is

    -----
    -- Element
    -----

    type JSON_Element_Type is abstract tagged null record;

    type JSON_Element_Class_Access is access JSON_Element_Type'Class;

    procedure Destroy
      (JSON_Element : in out JSON_Element_Class_Access)
      with Inline;

    function Copy
      (JSON_Element : in JSON_Element_Class_Access)
       return JSON_Element_Class_Access
      with Inline;

    -----
    -- Null
    -----

    type JSON_Null_Type is new JSON_Element_Type with null record;

    -----
    -- Boolean
    -----

    type JSON_Boolean_Type is new JSON_Element_Type with
        record
            Value : Boolean;
        end record;

    procedure Set_Boolean
      (JSON_Element : in out JSON_Boolean_Type;
       Value        : in     Boolean)
      with Inline;

    function New_Boolean (Value : Boolean) return JSON_Element_Class_Access;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Boolean
      with Inline;
    -- Returns the boolean value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Boolean_Type

    -----
    -- Number
    -----

    type JSON_Number_Type is new JSON_Element_Type with
        record
            Value : Long_Long_Float;
        end record;

    procedure Set_Number
      (JSON_Element : in out JSON_Number_Type;
       Value        : in     Long_Long_Float)
      with Inline;

    function New_Number (N : Float) return JSON_Element_Class_Access;
    function New_Number (N : Long_Float) return JSON_Element_Class_Access;
    function New_Number (N : Long_Long_Float) return JSON_Element_Class_Access;
    function New_Number (N : Integer) return JSON_Element_Class_Access;
    function New_Number (N : Long_Integer) return JSON_Element_Class_Access;
    function New_Number (N : Long_Long_Integer) return JSON_Element_Class_Access;
    function New_Number (N : Ada.Containers.Count_Type) return JSON_Element_Class_Access;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Integer
      with Inline;
    -- Returns the Integer value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Number_Type

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Float
      with Inline;
    -- Returns the Float value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Number_Type

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Float
      with Inline;
    -- Returns the Long_Float value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Number_Type

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Long_Float
      with Inline;
    -- Returns the Long_Long_Float value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Number_Type

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Long_Integer
      with Inline;
    -- Returns the Long_Integer value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Number_Type

    -----
    -- String
    -----

    type JSON_String_Type is new JSON_Element_Type with
        record
            Value : Unbounded_String;
        end record;

    procedure Set_String
      (JSON_Element : in out JSON_String_Type;
       Value        : in     String)
      with Inline;

    procedure Set_String
      (JSON_Element : in out JSON_String_Type;
       Value        : in     Unbounded_String)
      with Inline;

    function New_String (S : String) return JSON_Element_Class_Access;
    function New_String (S : Unbounded_String) return JSON_Element_Class_Access;

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return Unbounded_String
      with Inline;
    -- Returns the Unbounded_String value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_String_Type

    function Value
      (JSON_Element : JSON_Element_Class_Access)
       return String is
      (To_String (Value (JSON_Element)))
      with Inline;
    -- Returns the String value from JSON_Element.
    -- Raise error if JSON_Element is not JSON_String_Type

    -----
    -- Array
    -----

    pragma Suppress (Tampering_Check);
    package Element_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => JSON_Element_Class_Access);

    type JSON_Array_Type is new JSON_Element_Type with
        record
            Vector : Element_Vectors.Vector;
        end record;

    function Length
      (JSON_Array : not null JSON_Element_Class_Access)
       return Ada.Containers.Count_Type
      with Inline;
    -- Returns the array length.
    -- Raise error if JSON_Array is not JSON_Array_Type

    function Is_Empty
      (JSON_Array : not null JSON_Element_Class_Access)
       return Boolean is
      (Ada.Containers."=" (Length (JSON_Array),  0))
    with Inline;

    procedure Append
      (JSON_Array : not null JSON_Element_Class_Access;
       Item       : not null JSON_Element_Class_Access)
      with Inline;
    -- Append Item to JSON_Array.
    -- Raise error if JSON_Array is not JSON_Array_Type

    function Vector
      (JSON_Element : not null JSON_Element_Class_Access)
       return Element_Vectors.Vector
      with Inline;
    -- Returns the vector from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Array_Type

    -----
    -- Object
    -----

    pragma Suppress (Tampering_Check);
    package Element_Hashed_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => JSON_Element_Class_Access,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    type JSON_Object_Type is new JSON_Element_Type with
        record
            Map : Element_Hashed_Maps.Map;
        end record;

    function Has_Key
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String)
       return Boolean
      with Inline;
    -- Raise error if JSON_Object is not JSON_Object_Type

    procedure Insert
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String;
       Item        : not null JSON_Element_Class_Access)
      with Inline;
    -- Insert Item related to Key into JSON_Object.
    -- Raise error if JSON_Object is not JSON_Object_Type

    function Element
      (JSON_Object : not null JSON_Element_Class_Access;
       Key         : String)
       return not null JSON_Element_Class_Access
      with Inline;
    -- Get the element related to Key from JSON_Object.
    -- Raise error if JSON_Object is not JSON_Object_Type

    function Map
      (JSON_Element : JSON_Element_Class_Access)
       return Element_Hashed_Maps.Map
      with Inline;
    -- Returns the map from JSON_Element.
    -- Raise error if JSON_Element is not JSON_Object_Type

    -----
    -- Procedures
    -----

    function JSON_Decode
      (Input : in String)
       return JSON_Element_Class_Access
      with Inline;

    procedure JSON_Encode
      (JSON_Element   : in     JSON_Element_Class_Access;
       Output         : in out Unbounded_String)
      with Inline;

    function  JSON_Encode
      (JSON_Element : in JSON_Element_Class_Access)
       return Unbounded_String
      with Inline;

    procedure JSON_Encode
      (JSON_Element   : in     JSON_Element_Class_Access;
       Output         : in out String)
      with Inline;

    function JSON_Encode
      (JSON_Element : in JSON_Element_Class_Access)
       return String
      with Inline;

    procedure String_Encode
      (Input  : in     String;
       Output : in out Unbounded_String)
      with Inline;

    JSON_Error : exception;

private

    procedure Unchecked_Deallocation is new
      Ada.Unchecked_Deallocation
        (JSON_Element_Type'Class,
         JSON_Element_Class_Access);

    -----

    type String_Access_Type is access all String;

    procedure Free is new
      Ada.Unchecked_Deallocation (String, String_Access_Type);

    -----

    function Decode
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access
      with Inline;

    procedure Skip_Spaces
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
      with Inline;

    function Decode_Null
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access
      with Inline;

    function Decode_True
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access
      with Inline;

    function Decode_False
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access
      with Inline;

    function Decode_Number
      (Input      : in     String_Access_Type;
       Char_Index : in out Natural)
       return JSON_Element_Class_Access
      with Inline;

    function Decode_String
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return String;

    function Decode_Array
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access
      with Inline;

    function Decode_Object
      (Input         : in     String_Access_Type;
       Char_Index    : in out Natural;
       String_Buffer : in     String_Access_Type)
       return JSON_Element_Class_Access
      with Inline;

end ARColl.Readers.JSON;
