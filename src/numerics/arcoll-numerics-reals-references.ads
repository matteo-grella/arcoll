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

with System;
with Ada.Containers.Indefinite_Hashed_Maps;

with ARColl.System_Related;

package ARColl.Numerics.Reals.References is
    
    type Real_Reference_Type is record
        Value            : Real        := 0.0;
        Reference_Access : Real_Access := null;
        Class            : Index_Type  := 0;
    end record;

    type Real_Reference_Array is array (Index_Type range <>) of Real_Reference_Type;
    type Real_Reference_Array_Access is access Real_Reference_Array;
    
    procedure Free is new
      Ada.Unchecked_Deallocation 
        (Real_Reference_Array, Real_Reference_Array_Access);
    
    package Real_Reference_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => System.Address,
         Element_Type    => Real_Reference_Type,
         Hash            => ARColl.System_Related.Hash,
         Equivalent_Keys => System."=");

    procedure Update_Real_References
      (V_In       : in     Real_Array;
       References : in out Real_Reference_Array) with
      Pre => V_In'Length = References'Length
      and then (for all I in References'Range
                => References (I).Value = 0.0);

    procedure Update_Real_References_Map
      (References             : in     Real_Reference_Array;
       Reference_Errors_Map   : in out Real_Reference_Maps.Map);

end ARColl.Numerics.Reals.References;
