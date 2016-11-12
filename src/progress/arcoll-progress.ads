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

package ARColl.Progress is

    type Progress_Indicator is tagged private;
    
    type Mode_Type is (ICON, BAR, PERCENTAGE);
    
    function Create_Indicator
      (Total : Natural;
       Mode  : Mode_Type)
       return Progress_Indicator
      with Inline;
    
    procedure Reset (Indicator : in out Progress_Indicator) with Inline;      
    
    procedure Tick
      (Indicator : in out Progress_Indicator;
       Amount    : in     Positive :=1) with Inline;
    
private
    
    Icon_Sequence : constant array (Natural range <>) of Character
      := ('-', '\', '|', '/', '-', '\', '|', '/');
    
    Bar_Length : constant Positive := 50;
    
    type Progress_Indicator is tagged
        record
            Total      : Natural   := 0;
            Current    : Natural   := 0;
            Perc       : Long_Long_Integer   := -1;
            Mode       : Mode_Type := BAR;
            Icon_Index : Natural   := Icon_Sequence'First;
        end record;

    procedure Print (Indicator : in out Progress_Indicator) with Inline;
    
end ARColl.Progress;
