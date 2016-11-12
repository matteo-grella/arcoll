
with AUnit.Assertions; use AUnit.Assertions;

package body ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test is

    procedure Set_Up (T : in out TC) is
    begin
        null;
    end Set_Up;

    procedure Tear_Down (T : in out TC) is
    begin
        null;
    end Tear_Down;

    procedure Test (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_Bounded_ID_Bimaps.Bimap_Type (2);

        procedure Insert_Baz is
        begin
            Bimap.Insert ("Baz");
        end Insert_Baz;

    begin

        Bimap.Clear;

        Bimap.Insert ("Foo");
        Bimap.Insert ("Baz");

        Assert_Exception (Insert_Baz'Unrestricted_Access, "Exception excepted.");

    end Test;

end ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test;
