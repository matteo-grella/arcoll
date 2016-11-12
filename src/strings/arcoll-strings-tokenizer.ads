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

package ARColl.Strings.Tokenizer is
   
    ---------------
    procedure Split
    ---------------
      (Str       : in     String; 
       Separator : in     String := " ";
       Strs      : in out String_Vectors.Vector);
        
    -------------------
    -- Fast String Tokenizer
    -------------------
    
    type Fast_String_Tokenizer_Type is tagged private;
    
    ------------------------------
    procedure Fast_Tokenizer_Start
    ------------------------------
      (Fast_String_Tokenizer : out Fast_String_Tokenizer_Type;
       Str                   : in String) with Inline;
    
    --------------------------------
    function Fast_Tokenizer_Has_Next
    --------------------------------
      (Fast_String_Tokenizer : in Fast_String_Tokenizer_Type; 
       Str                   : in String) return Boolean with Inline;
    
    -----------------------------
    procedure Fast_Tokenizer_Next
    -----------------------------
      (Fast_String_Tokenizer : in out Fast_String_Tokenizer_Type; 
       Str                   : in     String;
       Separator             : in     String; 
       First                 :    out Positive;
       Last                  :    out Positive) with Inline;
    
    Fast_Tokenizer_Cant_Next : exception;

private
    
    type Fast_String_Tokenizer_Type is tagged record
        Last_Index : Natural;
    end record;

    pragma Inline(Fast_Tokenizer_Start);
    pragma Inline(Fast_Tokenizer_Has_Next);
    pragma Inline(Fast_Tokenizer_Next);

end ARColl.Strings.Tokenizer;
