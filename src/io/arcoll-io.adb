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

with Ada.Streams.Stream_IO;
with Ada.Directories;

package body ARColl.IO is

    function File_To_String
      (Filename : String) return String_Access is

        use Ada.Streams.Stream_IO;

        File_Size : constant Natural := Natural (Ada.Directories.Size (Filename));

        File         : File_Type;
        Input_Stream : Ada.Streams.Stream_IO.Stream_Access;

        File_Content : String_Access := new String (1 .. File_Size);
        Index        : Natural := File_Content'First;
    begin

        Ada.Streams.Stream_IO.Open (File, In_File, Filename);
        Input_Stream := Stream (File);

        while not End_Of_File (File) loop
            Character'Read (Input_Stream, File_Content (Index));
            Index := Index + 1;
        end loop;

        Close (File);

        return File_Content;
    exception
        when others =>
            begin
                Close (File);
            exception when others => null;
            end;
            Free (File_Content);
            raise;
    end File_To_String;

end ARColl.IO;
