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

private with Ada.Unchecked_Deallocation;
private with Ada.Streams;

with Ada.Finalization;

generic

    type Index_Type is range <>;
    type Element_Type is private;

    with function "=" (Left, Right : Element_Type) return Boolean is <>;

    Conversion_Treshold : Positive;

package ARColl.Containers.Sparse_Vectors.Generic_Sparse_Vectors is

    type Sparse_Vector_Type is new Ada.Finalization.Controlled with private;

    Empty_Sparse_Vector : constant Sparse_Vector_Type;

    type Cursor is private;

    overriding function "="
      (Left, Right : Sparse_Vector_Type)
       return Boolean
      with Inline;

    function Find
      (Vector : Sparse_Vector_Type;
       Index  : Index_Type)
       return Cursor
      with Inline;

    function First
      (Vector : Sparse_Vector_Type)
       return Cursor
      with Inline;

    procedure Next
      (Vector   : Sparse_Vector_Type;
       Position : in out Cursor)
      with Inline;

    function Has_Element
      (Position : Cursor)
       return Boolean
      with Inline;

    function Index
      (Vector   : Sparse_Vector_Type;
       Position : Cursor)
       return Index_Type
      with Inline;

    function Element
      (Vector   : Sparse_Vector_Type;
       Position : Cursor)
       return Element_Type
      with Inline;

    procedure Update_Element
      (Vector   : in out Sparse_Vector_Type;
       Position : Cursor;
       Process  : not null access
         procedure (Index : Index_Type; Element : in out Element_Type))
      with Inline;

    procedure Insert
      (Vector  : in out Sparse_Vector_Type;
       Index   : Index_Type;
       Element : Element_Type)
      with Inline;

    function Length
      (Vector : Sparse_Vector_Type)
       return Natural
      with Inline;

    procedure Clear
      (Vector : in out Sparse_Vector_Type)
      with Inline;

    Sparse_Vectors_Exception : exception;


private

    subtype Extended_Index_Type is Index_Type'Base range
      Index_Type'First - 1 ..
        Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

    -----
    -- Array
    -----

    type Array_Type is array (Index_Type range <>) of Element_Type;
    -- TODO: ... ACCESS Element_Type -> more efficient?

    type Array_Access_Type is access Array_Type;

    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :     Array_Access_Type);
    for Array_Access_Type'Write use My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out Array_Access_Type);
    for Array_Access_Type'Read  use My_Read;

    procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
        (Array_Type,
         Array_Access_Type);

    -----
    -- Lists
    -----

    type Linked_List_Item_Type;

    type Linked_List_Item_Access_Type is access Linked_List_Item_Type;

    type Linked_List_Item_Type is
       record
           Index   : Index_Type;
           Element : Element_Type;
           Next    : Linked_List_Item_Access_Type := null;
       end record;

    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : Linked_List_Item_Access_Type);
    for Linked_List_Item_Access_Type'Write use My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out Linked_List_Item_Access_Type);
    for Linked_List_Item_Access_Type'Read  use My_Read;

    procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation
        (Linked_List_Item_Type,
         Linked_List_Item_Access_Type);

    -----
    -- Main Type
    -----

    type Sparse_Vector_Type is new Ada.Finalization.Controlled with
       record
           Is_Sparse       : Boolean := True;

           Internal_Length : Natural := 0;
           First_Index     : Extended_Index_Type := Index_Type'First;
           Last_Index      : Extended_Index_Type := Index_Type'First - 1;

           Linked_List     : Linked_List_Item_Access_Type := null;
           Arr             : Array_Access_Type := null;
       end record;

    overriding procedure Initialize
      (Object : in out Sparse_Vector_Type)
      with Inline;

    overriding procedure Adjust
      (Object : in out Sparse_Vector_Type)
      with Inline;

    overriding procedure Finalize
      (Object : in out Sparse_Vector_Type)
      with Inline;

    -----
    -- Cursor
    -----

    type Cursor is
       record
           Internal_Has_Element : Boolean := False;
           Index                : Extended_Index_Type := Index_Type'First - 1;
           Linked_List_Item     : Linked_List_Item_Access_Type := null;
       end Record;

    Empty_Sparse_Vector : constant Sparse_Vector_Type
      := (Ada.Finalization.Controlled with
          Is_Sparse       => True,
          Internal_Length => 0,
          First_Index     => Index_Type'First,
          Last_Index      => Index_Type'First - 1,
          Linked_List     => null,
          Arr             => null);

end  ARColl.Containers.Sparse_Vectors.Generic_Sparse_Vectors;
