
with AUnit.Assertions; use AUnit.Assertions;

with ARColl.Numerics.Reals; use ARColl.Numerics.Reals;
with ARColl.Numerics.Reals.Functions; use ARColl.Numerics.Reals.Functions;

package body ARcoll.Numerics.Reals.Functions_Test is

    procedure Set_Up (T : in out TC) is
    begin
        null;
    end Set_Up;


    procedure Tear_Down (T : in out TC) is
    begin
        null;
    end Tear_Down;

    procedure Test_Array_Addition (T : in out TC) is
        pragma Unreferenced (T);

        V1 : Real_Array          := (1.0, 2.0, 3.0);
        V2 : constant Real_Array := (4.0, 5.0, 6.0);

        First_Index : constant Index_Type := V1'First;
    begin

        Addition (V1_Out => V1,
                  V2     => V2);

        Assert (V1 (First_Index)     = 5.0, "Value => " & True'Img);
        Assert (V1 (First_Index + 1) = 7.0, "Value => " & True'Img);
        Assert (V1 (First_Index + 2) = 9.0, "Value => " & True'Img);

    end Test_Array_Addition;

end ARcoll.Numerics.Reals.Functions_Test;
