
with AUnit;
with AUnit.Test_Fixtures;

private with ARColl.Containers.Bimaps.Generic_Bounded_ID_Bimaps;
private with Ada.Strings.Fixed.Hash;

package ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test is

    type TC is new AUnit.Test_Fixtures.Test_Fixture with record
        null;
    end record;

    overriding procedure Set_Up (T : in out TC);
    overriding procedure Tear_Down (T : in out TC);

    procedure Test (T : in out TC);

private

    package String_Bounded_ID_Bimaps is new
      ARColl.Containers.Bimaps.Generic_Bounded_ID_Bimaps
        (ID_Type      => Positive,
         Element_Type => String,
         Hash         => Ada.Strings.Fixed.Hash);

end ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test;
