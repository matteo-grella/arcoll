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

package body ARColl.Numerics.Reals is
    
    function Sum
      (V : Real_Vectors.Vector) return Real is
    begin
        return Ret : Real := 0.0 do
            for Element of V loop
                Ret := Ret + Element;
            end loop;
        end return;
    end Sum;
    
    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        Real_Array_Access) is
    begin
        Boolean'Output (Stream, Obj /= null);
        if Obj /= null then
            Index_Type'Write (Stream, Obj'First);
            Index_Type'Write (Stream, Obj'Last);
            Real_Array'Write (Stream, Obj.all);
        end if;
    end My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    Real_Array_Access) is
        First, Last : Index_Type;
    begin
        if Boolean'Input (Stream) then
            Index_Type'Read (Stream, First);
            Index_Type'Read (Stream, Last);
            Obj := new Real_Array (First .. Last);
            Real_Array'Read (Stream, Obj.all);
        else
            Obj := null;
        end if;
    end My_Read;

    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        Real_Matrix_Access) is
    begin
        Boolean'Output (Stream, Obj /= null);
        if Obj /= null then
            Index_Type'Write (Stream, Obj'First (1));
            Index_Type'Write (Stream, Obj'Last (1));
            Index_Type'Write (Stream, Obj'First (2));
            Index_Type'Write (Stream, Obj'Last (2));

            Real_Matrix'Write (Stream, Obj.all);
        end if;
    end My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    Real_Matrix_Access) is
        First_1, First_2, Last_1, Last_2 : Index_Type;
    begin
        if Boolean'Input (Stream) then
            Index_Type'Read (Stream, First_1);
            Index_Type'Read (Stream, Last_1);
            Index_Type'Read (Stream, First_2);
            Index_Type'Read (Stream, Last_2);

            Obj := new Real_Matrix (First_1 .. Last_1, First_2 .. Last_2);
            Real_Matrix'Read (Stream, Obj.all);
        else
            Obj := null;
        end if;
    end My_Read;
    
    procedure Free
      (Array_Of_Real_Vectors : in Real_Array_Access_Array) is
    begin
        for T in Array_Of_Real_Vectors'Range loop
            declare
                Vector : Real_Array_Access := Array_Of_Real_Vectors(T);
            begin
                if Vector /= null then
                    Free(Vector);
                end if;
            end;    
        end loop;
    end Free;
    
end ARColl.Numerics.Reals;
