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

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

package ARColl is

    subtype Size_T is Ada.Containers.Count_Type;

    type Extended_Index_Type is range -1 .. 2 ** (64 - 1) - 1
      with Default_Value => 0;
    for Extended_Index_Type'Size use 64;

    subtype Index_Type is Extended_Index_Type range
      0 .. Extended_Index_Type'Last;

    type Length_Type is new Natural with Default_Value => 0;

    subtype Positive_Length_Type is Length_Type range 1 .. Length_Type'Last;

    type Index_Type_Array is array (Index_Type range <>) of Extended_Index_Type;
    -- Array (Index_Type) of Index_Type values

    function Hash (Index : Extended_Index_Type) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type
         ((abs Index) mod  Index_Type (Ada.Containers.Hash_Type'Last)))
    with Inline;

    function Img
      (Value : Integer) return String;
      
    function Img
      (Value : Extended_Index_Type) return String is
        (Img(Integer(Value))) with Inline;
    
    function To_Index_Array
      (Indexes : Index_Type_Array) return Index_Type_Array is
      (Indexes) with Inline;
    
    function To_Index_Array
      (Index : Extended_Index_Type) return Index_Type_Array is
      (To_Index_Array((Index_Type'First =>Index))) with Inline;
    
    package Index_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Index_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");

    type Boolean_Array is array (Index_Type range <>) of Boolean;

    type Extended_Index_Array_Type is array (Index_Type range <>) of Extended_Index_Type;
    -- Array (Index_Type) of Index_Type values

    package Index_Type_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Extended_Index_Type);
    
end ARColl;
