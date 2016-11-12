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

with Ada.Containers;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;

generic

    type ID_Type is range <>;
    type Element_Type (<>) is private;
    with function Hash (Key : Element_Type) return Ada.Containers.Hash_Type;

package ARColl.Containers.Bimaps.Generic_ID_Bimaps_ME is

    subtype Extended_ID is ID_Type'Base
    range ID_Type'First - 1 ..
      ID_Type'Min (ID_Type'Base'Last - 1, ID_Type'Last) + 1;

    No_ID : constant Extended_ID := Extended_ID'First;

    type Bimap_Type is tagged private;

    function Empty_Bimap return Bimap_Type with Inline;

    procedure Clear
      (Bimap : in out Bimap_Type)
      with Inline;
    -- Clear the Bimap

    procedure Insert
      (Bimap    : in out Bimap_Type;
       Element  : in     Element_Type;
       Inserted :    out Boolean)
      with Inline;

    procedure Insert
      (Bimap    : in out Bimap_Type;
       Element  : in     Element_Type)
      with Inline;

    function Get_Element
      (Bimap : in Bimap_Type;
       ID    : in ID_Type) return Element_Type
      with Inline;
    -- Given an (ID_Type) ID, returns the corresponding Element

    function Get_ID
      (Bimap   : in Bimap_Type;
       Element : in Element_Type)
       return ID_Type;
    -- Given an Element, returns the corresponding (ID_Type) ID

    function Find_ID
      (Bimap   : in Bimap_Type;
       Element : in Element_Type)
       return Extended_ID;
    -- Given an Element, returns the corresponding (ID_Type) ID

    function Contains_ID
      (Bimap : in Bimap_Type;
       ID    : in ID_Type) return Boolean
      with Inline;

    function Contains_Element
      (Bimap   : in Bimap_Type;
       Element : in Element_Type) return Boolean
      with Inline;

    function First_ID (Bimap : Bimap_Type) return ID_Type with Inline;

    function Last_ID (Bimap : Bimap_Type) return Extended_ID with Inline;

    function Length (Bimap : in Bimap_Type) return Length_Type with Inline;

    function Is_Empty (Bimap : in Bimap_Type) return Boolean is
      (Length(Bimap) = 0)
      with Inline;

private

    package Element_To_ID_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => Element_Type,
         Element_Type    => ID_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");

    package Element_Vectors is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => ID_Type,
         Element_Type => Element_To_ID_Maps.Cursor,
         "="          => Element_To_ID_Maps."=");

    type Bimap_Type is tagged record
        ID_To_Element : Element_Vectors.Vector;
        -- Mapping from an ID (ID/Index_Type) to its Element

        Element_To_ID : Element_To_ID_Maps.Map;
        -- Mapping from am Element to its ID (Index_Type)
    end record;

end ARColl.Containers.Bimaps.Generic_ID_Bimaps_ME;
