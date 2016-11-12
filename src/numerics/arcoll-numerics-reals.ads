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

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;

package ARColl.Numerics.Reals is
    
    type Real is new Float with Default_Value => 0.0;
   
    type Real_Access is access all Real;
    type Real_Access_Array is array (Index_Type range <>) of Real_Access;
    type Real_Access_Array_Access is access all Real_Access_Array;
    
    procedure Free is new Ada.Unchecked_Deallocation 
      (Real_Access_Array, Real_Access_Array_Access);
      
    type Real_Array is array (Index_Type range <>) of aliased Real;


    -- Real Array
    
    type Real_Array_Access is access all Real_Array;
    
    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        Real_Array_Access);
    for Real_Array_Access'Write use My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    Real_Array_Access);
    for Real_Array_Access'Read  use My_Read;
    
    procedure Free is new
      Ada.Unchecked_Deallocation (Real_Array, Real_Array_Access);
    
    type Real_Array_Access_Array is
      array (Index_Type range <>) of aliased Real_Array_Access;
    
    procedure Free (Array_Of_Real_Vectors : in Real_Array_Access_Array) with Inline;

    -- Real Matrix
    
    type Real_Matrix is
      array (Index_Type range <>, Index_Type range <>) of aliased Real;
    --pragma Convention (Fortran, Real_Matrix);

    type Real_Matrix_Access is access all Real_Matrix;
    
    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        Real_Matrix_Access);
    for Real_Matrix_Access'Write use My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    Real_Matrix_Access);
    for Real_Matrix_Access'Read  use My_Read;

    procedure Free is new
      Ada.Unchecked_Deallocation (Real_Matrix, Real_Matrix_Access);

    package Real_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Real);
    
    function Sum
      (V : Real_Vectors.Vector) return Real;
    
    function Avg
      (V : Real_Vectors.Vector) return Real is
      (if V.Is_Empty then 0.0 else Sum(V) / Real(V.Length)) with Inline;
    
end ARColl.Numerics.Reals;
