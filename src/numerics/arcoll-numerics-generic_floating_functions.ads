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

with Ada.Numerics.Generic_Elementary_Functions;
with ARColl.Numerics.C;

generic
    type Float_Type is digits <>;

    type Float_Type_Array is array (Index_Type range <>) of aliased Float_Type;
    
    type Float_Type_Matrix is array
      (Index_Type range <>, Index_Type range <>) of aliased Float_Type;
    
    type Float_Type_Matrix_Access is access all Float_Type_Matrix;
    
    type Float_Array_Access is access all Float_Type_Array;
    
    type Float_Array_Access_Array is array (Index_Type range <>) of aliased Float_Array_Access;
    
package ARColl.Numerics.Generic_Floating_Functions is
   
    -- pragma Assertion_Policy (DISABLE);

    
    pragma Unreferenced (Float_Array_Access_Array);
    
    ----
    -- Basic Math Operations
    ----
    
    package Float_Type_Elementary_Functions is new
      Ada.Numerics.Generic_Elementary_Functions
        (Float_Type);
    -- Real Elementary Functions
    
    procedure Addition
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array)
      with Inline,
      Pre => V1_Out'Length = V2'Length;
    -- V1 := V1 + V2
    
    procedure Addition
      (M1_Out : in out Float_Type_Matrix;
       M2     : in     Float_Type_Matrix)
      with Inline;
    -- M1 := M1 + M2

    procedure Subtraction
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array)
      with Inline,
      Pre => V1_Out'Length = V2'Length;
    -- V1 := V1 - V2
    
    procedure Subtraction
      (V_Out  : in out Float_Type_Array;
       V1     : in     Float_Type_Array;
       V2     : in     Float_Type_Array)
      with Inline,
      Pre => V1'Length = V2'Length;
    -- V_Out := V1 - V2
    
    procedure Addition
      (V_Out : in out Float_Type_Array;
       M     : in     Float_Type_Matrix;
       M_Row : in     Index_Type)
      with Inline;
    -- V := M + M_Row
      
    procedure VM_Product
      (V_Out       : in out  Float_Type_Array;
       V_In        : in      Float_Type_Array;
       M_In        : in      Float_Type_Matrix)
      with Inline;
    -- V_Out := V_In * M_In
    
    procedure MV_Product
      (V_Out       : out  Float_Type_Array;
       M_In        : in   Float_Type_Matrix;
       V_In        : in   Float_Type_Array)
      with Inline;
    -- V_Out := M_In * V_In

    procedure VV_Product
      (M_Out : out Float_Type_Matrix;
       V1    : in  Float_Type_Array;
       V2    : in  Float_Type_Array)
      with Inline;
    -- M_Out := V_1 * V2

    procedure VV_Add_Product
      (M_Out : in out Float_Type_Matrix;
       V1    : in     Float_Type_Array;
       V2    : in     Float_Type_Array)
      with inline;
    -- M_Out += V_1 * V2
    
    procedure Element_Division
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array)
      with Inline,
      Pre => V1_Out'Length = V2'Length;
    -- V1_Out := V1_Out / V2

    procedure Element_Product
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array)
      with Inline,
      Pre => V1_Out'Length = V2'Length;
    -- V1_Out := V1_Out * V2
    
    procedure Element_Product_Sub_One
      (V1 : in out Float_Type_Array;
       V2 : in     Float_Type_Array;
       V3 : in     Float_Type_Array);
    
    procedure Element_Product
      (V1 : in out Float_Type_Array;
       V2 : in     Float_Type_Array;
       V3 : in     Float_Type_Array);

    procedure Matrix_Division
      (M_Out : in out Float_Type_Matrix;
       N     : in  Float_Type)
      with inline;

    procedure Vector_Division
      (V1   : in out Float_Type_Array;
       N    : in  Float_Type)
      with inline;
    
    function Exp (X : Float_Type)  return Float_Type renames Float_Type_Elementary_Functions.Exp;
    function Sqrt (X : Float_Type) return Float_Type renames Float_Type_Elementary_Functions.Sqrt;
    function Log (X : Float_Type)  return Float_Type renames Float_Type_Elementary_Functions.Log;
    function Tanh (X : Float_Type)  return Float_Type renames Float_Type_Elementary_Functions.Tanh;

    -------
    -- Statistics
    -------
    
    function Percent (N, Tot : Natural) return Float_Type is
      (Float_Type (N) / Float_Type (Tot) * 100.0) with Inline;

    procedure Array_Random_Permutation
      (Arr       : out Index_Type_Array;
       Randomize :  in Boolean := True;
       Verbose   :  in Boolean := False);

    type Moving_Average_Type is tagged record
        Counter  : Natural    := 0;
        Mean     : Float_Type := 0.0;
        Variance : Float_Type := 0.0;
    end record;

    procedure Add
      (Moving_Average : in out Moving_Average_Type;
       Value          : in     Float_Type)
      with Inline;
    -- Add value to moving average.
    
    procedure Scaling
      (M     : in not null Float_Type_Matrix_Access;
       Debug : in          Boolean := False); 
    
    function Get_Max_Index
      (V              : in  Float_Type_Array)
       return Index_Type with Inline;
    
    -- FIXME: move to a more appropriate package
    function Get_Random_Weight (Random_Weights_Range : Float_Type := 0.01)  return Float_Type is
      ((2.0 * Float_Type (ARColl.Numerics.C.DRand48) * Random_Weights_Range) - Random_Weights_Range)
    with Inline;    
    
end ARColl.Numerics.Generic_Floating_Functions;
