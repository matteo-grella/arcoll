
with AUnit;
with AUnit.Test_Fixtures;

private with ARColl.Containers.Bimaps.Generic_ID_Bimaps_ME;
private with Ada.Strings.Fixed.Hash;

package ARColl.Containers.Bimaps.ID_Bimaps_Strings_ME_Test is

    type TC is new AUnit.Test_Fixtures.Test_Fixture with record
        null;
    end record;

    overriding procedure Set_Up (T : in out TC);
    overriding procedure Tear_Down (T : in out TC);

    procedure Test_Insert_Inserted (T : in out TC);
    procedure Test_Insert_Exception (T : in out TC);

    procedure Test_Length_And_Clear (T : in out TC);

    procedure Test_Contains_ID_And_Element (T : in out TC);

    procedure Test_First_And_Last_ID (T : in out TC);

    procedure Test_Get_ID_And_Element (T : in out TC);

private

    package String_ID_Bimaps is new
      ARColl.Containers.Bimaps.Generic_ID_Bimaps_ME
        (ID_Type      => Natural,
         Element_Type => String,
         Hash         => Ada.Strings.Fixed.Hash);

end ARColl.Containers.Bimaps.ID_Bimaps_Strings_ME_Test;
