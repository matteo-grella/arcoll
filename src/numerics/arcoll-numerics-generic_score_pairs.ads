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

generic
    type Float_Type is digits <>;

package ARColl.Numerics.Generic_Score_Pairs is
   
    type Index_Score_Pair_Type is record
        Index : Extended_Index_Type  := -1;
        Score : Float_Type := 0.0;
    end record;

    type Index_Score_Pair_Access_Type is access all Index_Score_Pair_Type;
    
    type Index_Score_Pair_Type_Array is array (Index_Type range <>) of aliased Index_Score_Pair_Type;

    type Index_Score_Pair_Access_Type_Array is array (Index_Type range <>) of Index_Score_Pair_Access_Type;
    
    type Index_Score_Pair_Matrix_Type is array (Index_Type range <>, Index_Type range <>) of aliased Index_Score_Pair_Type;

    procedure Select_best
      (A     : in out Index_Score_Pair_Type_Array;
       First : in     Integer;
       Last  : in     Integer;
       Num   : in     Integer);
    
private
    
    function Partition
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer) return Integer;
    
    function Select_Index
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer;
       I : Integer) return Integer;

    procedure QuickSort
      (A : in out Index_Score_Pair_Type_Array;
       P : Integer;
       R : Integer);
    
end ARColl.Numerics.Generic_Score_Pairs;