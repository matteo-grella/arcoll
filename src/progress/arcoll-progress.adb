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

with Text_IO;
with Ada.Long_Long_Integer_Text_IO;

package body ARColl.Progress is
    
    function Create_Indicator
      (Total : Natural;
       Mode  : Mode_Type)
       return Progress_Indicator is

        Indicator : Progress_Indicator;
    begin
        
        Indicator.Total := Total;
        Indicator.Mode  := Mode;
        
        Indicator.Reset;
        
        return Indicator;
        
    end Create_Indicator;
    
    procedure Reset (Indicator : in out Progress_Indicator) is
    begin
        Indicator := Progress_Indicator'
          (Total      => Indicator.Total,
           Current    => 0,
           Perc       => -1,
           Mode       => Indicator.Mode,
           Icon_Index => Icon_Sequence'First);
    end Reset;
    
    procedure Tick
      (Indicator : in out Progress_Indicator;
       Amount    : in     Positive := 1) is
        
        Cur_Perc : Long_Long_Integer;
    begin
        Indicator.Current := Indicator.Current + Amount;
        
        Cur_Perc := Long_Long_Integer
          (Float'Floor (100.0 * Float(Indicator.Current) / Float (Indicator.Total)));
        
        if Cur_Perc > Indicator.Perc then
            Indicator.Perc := Cur_Perc;
            Print (Indicator);
        end if;
    end Tick;

    procedure Print (Indicator : in out Progress_Indicator) is
        use Text_IO;
        File : constant File_Type := Standard_Output;
    begin
        case Indicator.Mode is
                
            when ICON =>
                Put (File, ASCII.CR & Icon_Sequence (Indicator.Icon_Index));
                Indicator.Icon_Index := (Indicator.Icon_Index + 1) mod Icon_Sequence'Length;
                
            when BAR =>
                declare
                    Inner_Bar_Length : constant Natural
                      := Natural(Float'Floor (Float (Bar_Length) * Float(Indicator.Perc) / 100.0));
                begin
                    Put (File, ASCII.CR & '[');
                    
                    for I in 1 .. Bar_Length loop
                        if I < Inner_Bar_Length or else
                          (I = Inner_Bar_Length and then Indicator.Perc = 100) then
                            Put (File, "=");                            
                        elsif I = Inner_Bar_Length then
                            Put (File, ">");      
                        else
                            Put (File, ' ');
                        end if;
                    end loop;
                    
                    Put (File, "] ");
                    
                    Ada.Long_Long_Integer_Text_IO.Put (File, Indicator.Perc, Width => 0);
                    Put (File, "%");
                end;
                  
            when PERCENTAGE =>
                Put (File, ASCII.CR & '[');
                Ada.Long_Long_Integer_Text_IO.Put (File, Indicator.Perc, Width => 3);
                Put (File, "%]");

        end case;
        
        if Indicator.Perc = 100 then
            New_Line (File);
        end if;
        
    end Print;
    
end ARColl.Progress;
