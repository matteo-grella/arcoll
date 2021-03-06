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

with ARColl.Strings; use ARColl.Strings;

package ARColl.IO is

    function File_To_String
      (Filename : String) return String_Access;
    
    Dir_Separator : constant Character;
    pragma Import ( C, Dir_Separator, "__gnat_dir_separator");
    
    function Path_Join (A, B : String) return String is
      (if A'Length = 0 
       then B
       elsif A (A'Last) = Dir_Separator 
       then A & B
       else A & Dir_Separator & B) with Inline;
    
end ARColl.IO;
