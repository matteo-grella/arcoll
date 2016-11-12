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
with Ada.Float_Text_IO;

package body ARColl.Numerics.Generic_Floating_Functions is
   
    ----
    -- Basic Math Operations
    ----
        
    procedure Addition
      (V_Out : in  out Float_Type_Array;
       M     : in      Float_Type_Matrix;
       M_Row : in      Index_Type) is
    begin
        for I in V_Out'First .. V_Out'Last loop
            --pragma Loop_Optimize(Unroll);
            V_Out (I) := V_Out (I) + M (M_Row, I);
        end loop;
    end Addition;

    procedure Addition
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array) is
    begin
        for I in V1_Out'First .. V1_Out'Last loop
            --pragma Loop_Optimize (Unroll);
            if V2 (I) /= 0.0 then
                V1_Out (I) := V1_Out (I) + V2 (I);
            end if;
        end loop;
    end Addition;

    procedure Addition
      (M1_Out : in out Float_Type_Matrix;
       M2     : in     Float_Type_Matrix) is
    begin
        for I in M1_Out'First (1) .. M1_Out'Last (1) loop
            --pragma Loop_Optimize (Unroll);
            
            for J in M1_Out'First (2) ..  M1_Out'Last (2) loop
                --pragma Loop_Optimize (Unroll);
                
                if  M2 (I, J) /= 0.0 then
                    M1_Out (I, J) := M1_Out (I, J) + M2 (I, J);
                end if;
            end loop;
        end loop;
    end Addition;

    procedure Subtraction
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array) is
    begin
        for I in V1_Out'Range loop
            V1_Out (I) := V1_Out (I) - V2 (I);
        end loop;
    end Subtraction;
    
    procedure Subtraction
      (V_Out  : in out Float_Type_Array;
       V1     : in     Float_Type_Array;
       V2     : in     Float_Type_Array) is
    begin
        for I in V_Out'Range loop
            V_Out (I) := V1 (I) - V2 (I);
        end loop;
    end Subtraction;
    
    procedure VM_Product
      (V_Out       : in out Float_Type_Array;
       V_In        : in     Float_Type_Array;
       M_In        : in     Float_Type_Matrix) is
        
        M_In_J_I : Float_Type;
    begin

        V_Out := (others => 0.0);
        
        for J in V_In'First .. V_In'Last loop
            --pragma Loop_Optimize (Unroll);
            if V_In (J) /= 0.0 then
                declare
                    V_In_J : constant Float_Type := V_In (J);
                begin
                    for I in V_Out'First .. V_Out'Last loop
                        --pragma Loop_Optimize (Unroll);                        
                        M_In_J_I := M_In (J, I);

                        if M_In_J_I /= 0.0 then
                            V_Out (I) := V_Out (I) + V_In_J * M_In_J_I;
                        end if;
                    end loop;
                end;
            end if;
        end loop;

    end VM_Product;

    procedure MV_Product
      (V_Out       : out  Float_Type_Array;
       M_In        : in   Float_Type_Matrix;
       V_In        : in   Float_Type_Array) is
    begin
        V_Out := (others => 0.0);
        
        for J in V_In'Range loop
            if V_In (J) /= 0.0 then
                for I in V_Out'Range loop
                    V_Out (I) := V_Out (I) + (V_In (J) * M_In (I, J));
                end loop;
            end if;
        end loop;
    end MV_Product;

    procedure VV_Product
      (M_Out : out Float_Type_Matrix;
       V1    : in  Float_Type_Array;
       V2    : in  Float_Type_Array) is
    begin
        for I in V1'Range loop
            if V1(I) /= 0.0 then
                for J in V2'Range loop
                    if V2(J) /= 0.0 then
                        M_Out (I, J) := V1 (I) * V2 (J);
                    end if;
                end loop;
            end if;
        end loop;
    end VV_Product;

    procedure VV_Add_Product
      (M_Out : in out Float_Type_Matrix;
       V1    : in  Float_Type_Array;
       V2    : in  Float_Type_Array) is
    begin
        for I in V1'Range loop
            if V1(I) /= 0.0 then
                for J in V2'Range loop
                    M_Out (I, J) := M_Out (I, J) + (V1(I) * V2(J));
                end loop;
            end if;
        end loop;
    end VV_Add_Product;
    
    procedure Element_Division
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array) is
    begin
        for I in V1_Out'Range loop
            V1_Out (I) := V1_Out (I) / V2 (I);
        end loop;
    end Element_Division;
    
    procedure Element_Product
      (V1_Out : in out Float_Type_Array;
       V2     : in     Float_Type_Array) is
    begin
        for I in V1_Out'Range loop
            V1_Out (I) := V1_Out (I) * V2 (I);
        end loop;
    end Element_Product;
    
    procedure Element_Product_Sub_One
      (V1 : in out Float_Type_Array;
       V2 : in     Float_Type_Array;
       V3 : in     Float_Type_Array) is
    begin
        for I in V2'Range loop
            V1 (I) := V2 (I) * (1.0 - V3 (I));
        end loop;
    end Element_Product_Sub_One;
    
    procedure Element_Product
      (V1 : in out Float_Type_Array;
       V2 : in     Float_Type_Array;
       V3 : in     Float_Type_Array) is
    begin
        for I in V2'Range loop
            V1 (I) := V2 (I) * V3 (I);
        end loop;
    end Element_Product;
    
    procedure Matrix_Division
      (M_Out : in out Float_Type_Matrix;
       N     : in  Float_Type) is
    begin
        for I in M_Out'Range loop
            for J in M_Out'Range(2) loop
                M_Out (I, J) := M_Out (I, J) / N;
            end loop;
        end loop;
    end Matrix_Division;

    procedure Vector_Division
      (V1   : in out Float_Type_Array;
       N    : in  Float_Type) is
    begin
        for I in V1'Range loop
            V1 (I) := V1 (I) / N;
        end loop;
    end Vector_Division;
    
    -------
    -- Statistics
    -------
    
    function Get_Max_Index
      (V : in  Float_Type_Array)
       return Index_Type is

        Max_Index : Index_Type := Index_Type'First;
    begin
        for I in V'First + 1 .. V'Last loop
            if V (I) > V (Max_Index) then
                Max_Index := I;
            end if;
        end loop;

        return Max_Index;
    end Get_Max_Index;
    
    procedure Array_Random_Permutation
      (Arr       : out Index_Type_Array;
       Randomize :  in Boolean := True;
       Verbose   :  in Boolean := False) is
    -- Perform a random permutation of Arr's elements
    
        use ARColl.Numerics.C;
        use Text_IO;
    begin
        for I in Arr'Range loop
            Arr (I) := I;
        end loop;

        if Randomize then
            for I in Arr'First .. Arr'Last - 1 loop -- Skip last
                declare
                    Random_Index : constant Index_Type
                      := I + Index_Type
                        
                         (Integer (Rand) mod Integer (Arr'Last + 1 - I));

                    Tmp : constant Index_Type := Arr (I);
                begin
                    Arr (I) := Arr (Random_Index);
                    Arr (Random_Index) := Tmp;
                end;
            end loop;
        end if;
        
        if Verbose then
            for I in Arr'Range loop
                Put_Line(I'Img & "=>" & Arr(I)'Img);
            end loop;
        end if;

        -- Recover right position

        -- for I in 1 ..  Offset loop
        --     Array_Random_Permutation(Perm, False);
        -- end loop;
    end Array_Random_Permutation;
    
    procedure Add
      (Moving_Average : in out Moving_Average_Type;
       Value          : in     Float_Type) is
        
        C : constant Float_Type := 2.0; -- 1.0
    begin
        Moving_Average.Counter := Moving_Average.Counter + 1;

        Moving_Average.Mean := Moving_Average.Mean +
          ((C / Float_Type (Moving_Average.Counter)) *
           (Value - Moving_Average.Mean));

        Moving_Average.Variance := Moving_Average.Variance +
          ((C / Float_Type (Moving_Average.Counter)) *
           (((Value - Moving_Average.Mean) * (Value - Moving_Average.Mean))
              - Moving_Average.Variance));
    end Add;
    
    procedure Scaling
      (M     : in not null Float_Type_Matrix_Access;
       Debug : in          Boolean := False) is
        use Text_IO;
        use Ada.Float_Text_IO;
        
        R_Mean         : constant Float_Type := 0.0;
        R_Std          : constant Float_Type := 1.0;

        Count          : Natural    := 0;
        Mean           : Float_Type := 0.0;
        Std            : Float_Type := 0.0;
    begin
        for I in M'Range (1) loop
            for J in M'Range (2) loop
                Count := Count + 1;
                Mean  := Mean + M (I, J);
                Std   := Std  + (M (I, J) * M (I, J));
            end loop;
        end loop;

        Mean := Mean / Float_Type (Count);

        Std := Float_Type_Elementary_Functions.Sqrt
          ((Std / Float_Type (Count)) - (Mean * Mean));

        if Debug then
            Put_Line ("Scaling matrix:");
            Put ("    (mean = ");
            Put (Float (Mean), Fore => 1, Aft => 2, Exp  => 0);
            Put (", std = ");
            Put (Float (Std), Fore => 1, Aft => 2, Exp  => 0);
            Put (") - > (mean = ");
            Put (Float (R_Mean), Fore => 1, Aft => 2, Exp  => 0);
            Put (", std = ");
            Put (Float (R_Std), Fore => 1, Aft => 2, Exp  => 0);
            Put_Line (")");
        end if;
        
        for I in M'Range (1) loop
            for J in M'Range (2) loop
                M (I, J)
                  := (M (I, J) - Mean) * R_Std / Std + R_Mean;
            end loop;
        end loop;
    end Scaling;

end ARColl.Numerics.Generic_Floating_Functions;
