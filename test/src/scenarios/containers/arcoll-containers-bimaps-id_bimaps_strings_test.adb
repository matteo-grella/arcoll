
with AUnit.Assertions; use AUnit.Assertions;

with ARColl; use ARColl;

package body ARColl.Containers.Bimaps.ID_Bimaps_Strings_Test is

    procedure Set_Up (T : in out TC) is
    begin
        null;
    end Set_Up;


    procedure Tear_Down (T : in out TC) is
    begin
        null;
    end Tear_Down;

    procedure Test_Insert_Inserted (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;
        Inserted : Boolean;
    begin

        Inserted := False;
        Bimap.Insert (Element => "Foo", Inserted => Inserted);
        Assert (Inserted, "Value => " & Inserted'Img);

        Inserted := False;
        Bimap.Insert (Element => "Bar", Inserted => Inserted);
        Assert (Inserted, "Value => " & Inserted'Img);

        Inserted := True;
        Bimap.Insert (Element => "Foo", Inserted => Inserted);
        Assert (not Inserted, "Value => " & Inserted'Img);

        Inserted := True;
        Bimap.Insert (Element => "Bar", Inserted => Inserted);
        Assert (not Inserted, "Value => " & Inserted'Img);

    end Test_Insert_Inserted;

    procedure Test_Insert_Exception (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;

        procedure Insert_Foo is
        begin
            Bimap.Insert ("Foo");
        end Insert_Foo;

        procedure Insert_Bar is
        begin
            Bimap.Insert ("Bar");
        end Insert_Bar;
    begin

        Insert_Foo;
        Insert_Bar;

        Assert_Exception (Insert_Foo'Unrestricted_Access, "Exception expected");
        Assert_Exception (Insert_Bar'Unrestricted_Access, "Exception expected");

    end Test_Insert_Exception;

    procedure Test_Length_And_Clear (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;
    begin

        Assert (Bimap.Length = 0, "Value = " & Bimap.Length'Img);
        Assert (Bimap.Is_Empty, "Value = " & Bimap.Is_Empty'Img);

        Bimap.Insert ("Foo");
        Assert (Bimap.Length = 1, "Value = " & Bimap.Length'Img);
        Assert (not Bimap.Is_Empty, "Value = " & Bimap.Is_Empty'Img);

        Bimap.Insert ("Bar");
        Assert (Bimap.Length = 2, "Value = " & Bimap.Length'Img);
        Assert (not Bimap.Is_Empty, "Value = " & Bimap.Is_Empty'Img);

        Bimap.Clear;
        Assert (Bimap.Length = 0, "Value = " & Bimap.Length'Img);
        Assert (Bimap.Is_Empty, "Value = " & Bimap.Is_Empty'Img);

    end Test_Length_And_Clear;

    procedure Test_Contains_ID_And_Element (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;
    begin

        Assert (not Bimap.Contains_ID (0), "Wrong value.");
        Assert (not Bimap.Contains_Element ("Foo"), "Wrong value.");

        Bimap.Insert ("Foo");
        Assert (Bimap.Contains_ID (0), "Wrong value.");
        Assert (Bimap.Contains_Element ("Foo"), "Wrong value.");

    end Test_Contains_ID_And_Element;

    procedure Test_First_And_Last_ID (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;
    begin

        Assert (Bimap.First_ID = 0, "Value = " & Bimap.First_ID'Img);
        Assert (Bimap.Last_ID = -1, "Value = " & Bimap.Last_ID'Img);

        Bimap.Insert ("Foo");
        Assert (Bimap.First_ID = 0, "Value = " & Bimap.First_ID'Img);
        Assert (Bimap.Last_ID = 0, "Value = " & Bimap.Last_ID'Img);

        Bimap.Insert ("bar");
        Assert (Bimap.First_ID = 0, "Value = " & Bimap.First_ID'Img);
        Assert (Bimap.Last_ID = 1, "Value = " & Bimap.Last_ID'Img);

    end Test_First_And_Last_ID;

    procedure Test_Get_ID_And_Element (T : in out TC) is
        pragma Unreferenced (T);

        Bimap : String_ID_Bimaps.Bimap_Type;

        procedure Get_ID_Foo is
            ID : Natural;
        begin
            ID := Bimap.Get_ID ("Foo");
            pragma Unreferenced (ID);
        end Get_ID_Foo;

        procedure Get_ID_Bar is
            ID : Natural;
        begin
            ID := Bimap.Get_ID ("Bar");
            pragma Unreferenced (ID);
        end Get_ID_Bar;

        procedure Get_Element_0 is
        begin
            declare
                S : constant String := Bimap.Get_Element (0);
            begin
                pragma Unreferenced (S);
            end;
        end Get_Element_0;

        procedure Get_Element_1 is
        begin
            declare
                S : constant String := Bimap.Get_Element (1);
            begin
                pragma Unreferenced (S);
            end;
        end Get_Element_1;

    begin

        Assert_Exception (Get_ID_Foo'Unrestricted_Access, "Exception expected");
        Assert_Exception (Get_ID_Bar'Unrestricted_Access, "Exception expected");

        Assert_Exception (Get_Element_0'Unrestricted_Access, "Exception expected");
        Assert_Exception (Get_Element_1'Unrestricted_Access, "Exception expected");

        Bimap.Insert ("Foo");
        Assert (Bimap.Get_ID ("Foo") = 0, "Value := " & Bimap.Get_ID ("Foo")'Img);
        Assert (Bimap.Get_Element (0) = "Foo", "Value := " & Bimap.Get_Element (0));

        Assert_Exception (Get_ID_Bar'Unrestricted_Access, "Exception expected");
        Assert_Exception (Get_Element_1'Unrestricted_Access, "Exception expected");

        Bimap.Insert ("Bar");
        Assert (Bimap.Get_ID ("Bar") = 1, "Value := " & Bimap.Get_ID ("Bar")'Img);
        Assert (Bimap.Get_Element (1) = "Bar", "Value := " & Bimap.Get_Element (1));

    end Test_Get_ID_And_Element;

end ARColl.Containers.Bimaps.ID_Bimaps_Strings_Test;
