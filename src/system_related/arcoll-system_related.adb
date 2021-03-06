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

with System.Storage_Elements;

package body ARColl.System_Related is

    function Hash
      (Key : System.Address) return Ada.Containers.Hash_Type is

        Integer_Address : constant System.Storage_Elements.Integer_Address
          := System.Storage_Elements.To_Integer (Key);

        Limited_Integer_Address : constant Integer
          := Integer ( Long_Integer (Integer_Address) mod Long_Integer (Integer'Last) );

    begin
        return Ada.Containers.Hash_Type ( Limited_Integer_Address );
    end Hash;

end ARColl.System_Related;
