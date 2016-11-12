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

package body ARColl.Numerics.Generic_Score_Pairs is

    function Partition
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer) return Integer is
        
        I    : Integer := Natural(P) - 1;
        Temp : Index_Score_Pair_Type;
    begin

        for J in P .. R - 1 loop
            if A (Index_Type(J)).Score >= A (Index_Type(r)).Score then
                I := I + 1;
                
                Temp := A (Index_Type(I));
                A (Index_Type(i)) := A (Index_Type(j));
                A (Index_Type(j)) := Temp;
            end if;
        end loop;

        I := I + 1;
        
        Temp := A (Index_Type(I));
        A (Index_Type(i)) := A (Index_Type(r));
        A (Index_Type(r)) := Temp;

        return I;

    end Partition;

    function Select_Index
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer;
       I : Integer) return Integer is
        
        Q : Integer;
        K : Integer;
    begin
        if P = R then
            return p;
        end if;
        
        Q := Partition (A, P, R);
        K := Q - P + 1;
        
        return (if    I = K then Q
                elsif I < K then Select_Index (A, P, Q - 1, I)
                else  Select_Index (A, Q + 1, R, I - K));
                    
    end Select_Index;

    procedure QuickSort
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer) is
    begin
        if P < R then
            declare
                Q : constant Integer := Partition (A, P, R);
            begin
                QuickSort (A, P, Q - 1);
                QuickSort (A, Q + 1, R);
            end;
        end if;
    end QuickSort;

    --Select i best elements

    procedure Select_Best
      (A     : in out Index_Score_Pair_Type_Array;
       First : Integer;
       Last  : Integer;
       Num   : Integer) is
        Index : constant Integer 
          := Select_Index (A, First, Last, Num);
    begin
        QuickSort (A, Integer(A'First), Index);
    end Select_best;
    
end ARColl.Numerics.Generic_Score_Pairs;
