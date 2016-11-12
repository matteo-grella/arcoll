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

package body ARColl.IO.Serialization is

    package Stream_IO renames Ada.Streams.Stream_IO;

    procedure Serialize
      (Element              : in Element_Type;
       Serialized_File_Name : in String) is

        File   : Stream_IO.File_Type;
        Stream : Stream_IO.Stream_Access;
    begin
        Stream_IO.Create (File, Stream_IO.Out_File, Serialized_File_Name);
        Stream := Stream_IO.Stream (File);
        Element_Type'Write (Stream, Element);
        Stream_IO.Close (File);
    end Serialize;

    procedure Deserialize
      (Element              : out Element_Type;
       Serialized_File_Name : in  String) is

        File   : Stream_IO.File_Type;
        Stream : Stream_IO.Stream_Access;
    begin
        Stream_IO.Open(File, Stream_IO.In_File, Serialized_File_Name);
        Stream := Stream_IO.Stream(File);
        Element_Type'Read(Stream, Element);
        Stream_IO.Close(File);
    end Deserialize;

end ARColl.IO.Serialization;
