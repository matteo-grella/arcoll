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

private with ARColl.Containers.Bimaps.Generic_ID_Bimaps;

generic

    type ID_Type is range <>;
    type Element_Type (<>) is private;
    with function Hash (Key : Element_Type) return Ada.Containers.Hash_Type;

package ARColl.Containers.Bimaps.Generic_Bounded_ID_Bimaps is

    type Bimap_Type (Max_Length : Length_Type) is tagged private;

    function Empty_Bimap return Bimap_Type with Inline; -- FIX: What about the Max_Length?

private

    package ID_Bimaps is new
      ARColl.Containers.Bimaps.Generic_ID_Bimaps
        (ID_Type      => ID_Type,
         Element_Type => Element_Type,
         Hash         => Hash);

    type Bimap_Type (Max_Length : Length_Type) is new ID_Bimaps.Bimap_Type with null record;

    overriding
    procedure Insert
      (Bimap    : in out Bimap_Type;
       Element  : in     Element_Type;
       Inserted :    out Boolean)
      with Inline;

end ARColl.Containers.Bimaps.Generic_Bounded_ID_Bimaps;
