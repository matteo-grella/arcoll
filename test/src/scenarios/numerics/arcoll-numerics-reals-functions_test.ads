
with AUnit;
with AUnit.Test_Fixtures;

package ARColl.Numerics.Reals.Functions_Test is

    type TC is new AUnit.Test_Fixtures.Test_Fixture with record
        null;
    end record;

    overriding procedure Set_Up (T : in out TC);
    overriding procedure Tear_Down (T : in out TC);

    procedure Test_Array_Addition (T : in out TC);

end ARColl.Numerics.Reals.Functions_Test;
