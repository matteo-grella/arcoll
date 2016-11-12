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

with GNAT.String_Split;

package body ARColl.Strings.Tokenizer is

    procedure Split
      (Str       : in     String; 
       Separator : in     String := " ";
       Strs      : in out String_Vectors.Vector) is
        
        Subs : GNAT.String_Split.Slice_Set;
    begin
        GNAT.String_Split.Create(Subs, Str, Separator, GNAT.String_Split.Multiple);
        for I in 1 .. GNAT.String_Split.Slice_Count(Subs) loop
            Strs.Append(GNAT.String_Split.Slice(Subs, I));
        end loop;
    end Split;
    
    -------------------
    -- String Tokenizer
    -------------------

    procedure Fast_Tokenizer_Start
      (Fast_String_Tokenizer : out Fast_String_Tokenizer_Type;
       Str                   :  in String) is
    begin
        Fast_String_Tokenizer.Last_Index := Str'First - 1;
    end Fast_Tokenizer_Start;

    function Fast_Tokenizer_Has_Next
      (Fast_String_Tokenizer : in Fast_String_Tokenizer_Type; 
       Str                   : in String) return Boolean is
    begin
        return Fast_String_Tokenizer.Last_Index <= Str'Last;
    end Fast_Tokenizer_Has_Next;
    
    procedure Fast_Tokenizer_Next
      (Fast_String_Tokenizer : in out Fast_String_Tokenizer_Type; 
       Str                   : in     String;
       Separator             : in     String; 
       First                 :    out Positive;
       Last                  :    out Positive) is
        
        Pos : Natural;
    begin
        if Fast_String_Tokenizer.Last_Index > Str'Last then
            raise Fast_Tokenizer_Cant_Next;
        end if;

        First := Fast_String_Tokenizer.Last_Index + 1;

        Pos := Find_String(Str(First .. Str'Last), Separator);
        if Pos < First then
            Last := Str'Last;
            Fast_String_Tokenizer.Last_Index := Str'Last + 1;
        else
            Last := Pos - 1;
            Fast_String_Tokenizer.Last_Index := Pos + Separator'Length - 1;
        end if;
    end Fast_Tokenizer_Next;
    
end ARColl.Strings.Tokenizer;

