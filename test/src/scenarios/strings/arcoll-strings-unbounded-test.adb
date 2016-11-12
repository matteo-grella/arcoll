
with Ada.Streams.Stream_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body ARColl.Strings.Unbounded.Test is

    procedure Set_Up (T : in out TC) is
        Empty_1 : constant String := "";
        Empty_2 : constant String (-1 .. -2) := "";
        Empty_3 : constant String (0 .. -1) := "";
        Empty_4 : constant String (1 .. 0) := "";
        Empty_5 : constant String (100 .. 99) := "";
        Empty_6 : constant String (100 .. 50) := "";

        Foo_1   : constant String := "Foo";
        Foo_2   : constant String (1 .. 3) := "Foo";
        Foo_3   : constant String (100 .. 102) := "Foo";

        Baz_1   : constant String := "Baz";
        Baz_2   : constant String (1 .. 3) := "Baz";
        Baz_3   : constant String (100 .. 102) := "Baz";

        Bar_1   : constant String := "Bar";
        Bar_2   : constant String (1 .. 3) := "Bar";
        Bar_3   : constant String (100 .. 102) := "Bar";
    begin
        T.Empty_Strings.Append (Empty_1);
        T.Empty_Strings.Append (Empty_2);
        T.Empty_Strings.Append (Empty_3);
        T.Empty_Strings.Append (Empty_4);
        T.Empty_Strings.Append (Empty_5);
        T.Empty_Strings.Append (Empty_6);

        T.Foos.Append (Foo_1);
        T.Foos.Append (Foo_2);
        T.Foos.Append (Foo_3);

        T.Bazs.Append (Baz_1);
        T.Bazs.Append (Baz_2);
        T.Bazs.Append (Baz_3);

        T.Bars.Append (Bar_1);
        T.Bars.Append (Bar_2);
        T.Bars.Append (Bar_3);
    end Set_Up;


    procedure Tear_Down (T : in out TC) is
    begin
        T.Empty_Strings.Clear;
        T.Foos.Clear;
        T.Bazs.Clear;
        T.Bars.Clear;
    end Tear_Down;


    procedure Test_To_Unbounded_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            declare
                S : constant Unbounded_String := To_Unbounded_String (Empty);
            begin
                Assert (S = Empty, "Value = " & To_String (S));
                Assert (S = "", "Value = " & To_String (S));
            end;
        end loop;

        for Foo of T.Foos loop
            declare
                S : constant Unbounded_String := To_Unbounded_String (Foo);
            begin
                Assert (S = Foo, "Value = " & To_String (S));
                Assert (S = "Foo", "Value = " & To_String (S));
            end;
        end loop;
    end Test_To_Unbounded_String;


    procedure Test_To_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Assert (To_String (S) = Empty, "Value = " & To_String (S));
                            Assert (To_String (S) = "", "Value = " & To_String (S));

                            Append (S, "");
                            Assert (To_String (S) = Empty, "Value = " & To_String (S));
                            Assert (To_String (S) = "", "Value = " & To_String (S));

                            Append (S, Foo);
                            Assert (To_String (S) = Foo, "Value = " & To_String (S));
                            Assert (To_String (S) = "Foo", "Value = " & To_String (S));

                            Append (S, Baz);
                            Assert (To_String (S) = Foo & Baz, "Value = " & To_String (S));
                            Assert (To_String (S) = "FooBaz", "Value = " & To_String (S));

                            Append (S, Bar);
                            Assert (To_String (S) = Foo & Baz & Bar, "Value = " & To_String (S));
                            Assert (To_String (S) = "FooBazBar", "Value = " & To_String (S));
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;
    end Test_To_String;


    procedure Test_Append_Unbounded_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Append (S, To_Unbounded_String (Empty));
                            Assert (S = Empty, "Value = " & To_String (S));
                            Assert (S = "", "Value = " & To_String (S));

                            Append (S, To_Unbounded_String (Foo));
                            Assert (S = Foo, "Value = " & To_String (S));
                            Assert (S = "Foo", "Value = " & To_String (S));

                            Append (S, To_Unbounded_String (Baz));
                            Assert (S = Foo & Baz, "Value = " & To_String (S));
                            Assert (S = "FooBaz", "Value = " & To_String (S));

                            Append (S, To_Unbounded_String (Bar));
                            Assert (S = Foo & Baz & Bar, "Value = " & To_String (S));
                            Assert (S = "FooBazBar", "Value = " & To_String (S));
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;
    end Test_Append_Unbounded_String;


    procedure Test_Append_String_1 (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Append (S, Empty);
                            Assert (S = Empty, "Value = " & To_String (S));
                            Assert (S = "", "Value = " & To_String (S));

                            Append (S, Foo);
                            Assert (S = Foo, "Value = " & To_String (S));
                            Assert (S = "Foo", "Value = " & To_String (S));

                            Append (S, Baz);
                            Assert (S = Foo & Baz, "Value = " & To_String (S));
                            Assert (S = "FooBaz", "Value = " & To_String (S));

                            Append (S, Bar);
                            Assert (S = Foo & Baz & Bar, "Value = " & To_String (S));
                            Assert (S = "FooBazBar", "Value = " & To_String (S));
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;
    end Test_Append_String_1;


    procedure Test_Append_Character (T : in out TC) is
        pragma Unreferenced (T);
    begin
        declare
            S : Unbounded_String;
        begin
            Append (S, 'A');
            Assert (S = "A", "Value = " & To_String (S));

            Append (S, 'B');
            Assert (S = "AB", "Value = " & To_String (S));

            Append (S, 'C');
            Assert (S = "ABC", "Value = " & To_String (S));
        end;
    end Test_Append_Character;


    procedure Test_Length_With_Append_Unbounded_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

                            Append (S, Null_Unbounded_String);
                            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

                            Append (S, To_Unbounded_String (Empty));
                            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

                            Append (S, To_Unbounded_String (Foo));
                            Assert (Length (S) = 3, "Length = " & Length (S)'Img);

                            Append (S, To_Unbounded_String (Baz));
                            Assert (Length (S) = 6, "Length = " & Length (S)'Img);

                            Append (S, To_Unbounded_String (Bar));
                            Assert (Length (S) = 9, "Length = " & Length (S)'Img);
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;
    end Test_Length_With_Append_Unbounded_String;


    procedure Test_Length_With_Append_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

                            Append (S, Empty);
                            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

                            Append (S, Foo);
                            Assert (Length (S) = 3, "Length = " & Length (S)'Img);

                            Append (S, Baz);
                            Assert (Length (S) = 6, "Length = " & Length (S)'Img);

                            Append (S, Bar);
                            Assert (Length (S) = 9, "Length = " & Length (S)'Img);
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;
    end Test_Length_With_Append_String;


    procedure Test_Length_With_Append_Character (T : in out TC) is
        pragma Unreferenced (T);
    begin
        declare
            S : Unbounded_String;
        begin
            Assert (Length (S) = 0, "Length = " & Length (S)'Img);

            Append (S, 'A');
            Assert (Length (S) = 1, "Length = " & Length (S)'Img);

            Append (S, 'B');
            Assert (Length (S) = 2, "Length = " & Length (S)'Img);

            Append (S, 'C');
            Assert (Length (S) = 3, "Length = " & Length (S)'Img);
        end;
    end Test_Length_With_Append_Character;


    procedure Test_Allocated_Length_With_Append_Unbounded_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Assert (Get_Allocated_Length (S) = 0,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, Null_Unbounded_String);
                            Assert (Get_Allocated_Length (S) = 0,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, To_Unbounded_String (Empty));
                            Assert (Get_Allocated_Length (S) = 0,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, To_Unbounded_String (Foo));
                            Assert (Get_Allocated_Length (S) = 3,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, To_Unbounded_String (Baz));
                            Assert (Get_Allocated_Length (S) = 6,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, To_Unbounded_String (Bar));
                            Assert (Get_Allocated_Length (S) = 9,
                                    "Length = " & Get_Allocated_Length (S)'Img);
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;

        declare
            S : Unbounded_String;
        begin
            Append (S, To_Unbounded_String ("123456789|123456789|123456789|12"));
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, To_Unbounded_String ("X"));
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, To_Unbounded_String ("Y"));
            Assert (Get_Allocated_Length (S) = 35, "Length = " & Get_Allocated_Length (S)'Img);
        end;
    end Test_Allocated_Length_With_Append_Unbounded_String;



    procedure Test_Allocated_Length_With_Append_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                for Baz of T.Bazs loop
                    for Bar of T.Bars loop
                        declare
                            S : Unbounded_String;
                        begin
                            Assert (Get_Allocated_Length (S) = 0,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, Empty);
                            Assert (Get_Allocated_Length (S) = 0,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, Foo);
                            Assert (Get_Allocated_Length (S) = 3,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, Baz);
                            Assert (Get_Allocated_Length (S) = 6,
                                    "Length = " & Get_Allocated_Length (S)'Img);

                            Append (S, Bar);
                            Assert (Get_Allocated_Length (S) = 9,
                                    "Length = " & Get_Allocated_Length (S)'Img);
                        end;
                    end loop;
                end loop;
            end loop;
        end loop;

        declare
            S : Unbounded_String;
        begin
            Append (S, "123456789|123456789|123456789|12");
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, "X");
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, "Y");
            Assert (Get_Allocated_Length (S) = 35, "Length = " & Get_Allocated_Length (S)'Img);
        end;
    end Test_Allocated_Length_With_Append_String;


    procedure Test_Allocated_Length_With_Append_Character (T : in out TC) is
        pragma Unreferenced (T);
    begin
        declare
            S : Unbounded_String;
        begin
            Assert (Get_Allocated_Length (S) = 0, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, 'A');
            Assert (Get_Allocated_Length (S) = 1, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, 'B');
            Assert (Get_Allocated_Length (S) = 2, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, 'C');
            Assert (Get_Allocated_Length (S) = 3, "Length = " & Get_Allocated_Length (S)'Img);
        end;

        declare
            S : Unbounded_String;
        begin
            for I in 1 .. 32 loop
                Append (S, 'A');
            end loop;
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, 'B');
            Assert (Get_Allocated_Length (S) = 33, "Length = " & Get_Allocated_Length (S)'Img);

            Append (S, 'C');
            Assert (Get_Allocated_Length (S) = 35, "Length = " & Get_Allocated_Length (S)'Img);
        end;
    end Test_Allocated_Length_With_Append_Character;


    procedure Test_Equals_Unbounded_Unbounded (T : in out TC) is
    begin
        for Foo of T.Foos loop
            for Bar of T.Bars loop
                declare
                    A : Unbounded_String;
                    B : Unbounded_String;
                begin
                    Assert (A = A, "A=""" & To_String (A) & """");
                    Assert (B = B, "B=""" & To_String (B) & """");
                    Assert (A = B, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                    Assert (B = A, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");

                    A := To_Unbounded_String (Foo);
                    B := Null_Unbounded_String;
                    Assert (A /= B, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                    Assert (B /= A, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");

                    A := To_Unbounded_String (Foo);
                    B := To_Unbounded_String ("");
                    Assert (A /= B, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                    Assert (B /= A, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");

                    A := To_Unbounded_String (Foo);
                    B := To_Unbounded_String (Bar);
                    Assert (A /= B, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                    Assert (B /= A, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");

                    A := To_Unbounded_String (Foo);
                    B := To_Unbounded_String (Foo);
                    Assert (A = B, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                    Assert (B = A, "A=""" & To_String (A) & """ B=""" & To_String (B) & """");
                end;
            end loop;
        end loop;
    end Test_Equals_Unbounded_Unbounded;


    procedure Test_Equals_Unbounded_String (T : in out TC) is
    begin
        for Empty of T.Empty_Strings loop
            for Foo of T.Foos loop
                declare
                    A : Unbounded_String;
                begin
                    Assert (A = Empty, "A=""" & To_String (A) & """");
                    Assert (Empty = A, "A=""" & To_String (A) & """");

                    A := To_Unbounded_String (Foo);
                    Assert (A /= Empty, "A=""" & To_String (A) & """");
                    Assert (Empty /= A, "A=""" & To_String (A) & """");

                    Assert (A /= "Bar", "A=""" & To_String (A) & """");
                    Assert ("Bar" /= A, "A=""" & To_String (A) & """");

                    Assert (A = Foo, "A=""" & To_String (A) & """");
                    Assert (A = "Foo", "A=""" & To_String (A) & """");
                end;
            end loop;
        end loop;
    end Test_Equals_Unbounded_String;


    procedure Test_Amp_Unbounded_Unbounded (T : in out TC) is
        S : Unbounded_String;
    begin
        for Empty of T.Empty_Strings loop

            S := Null_Unbounded_String & Null_Unbounded_String;
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := Null_Unbounded_String & To_Unbounded_String (Empty);
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := To_Unbounded_String (Empty) & Null_Unbounded_String;
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := To_Unbounded_String (Empty) & To_Unbounded_String (Empty);
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := To_Unbounded_String ("A") & To_Unbounded_String (Empty);
            Assert (S = "A", "Value = " & To_String (S));

            S := To_Unbounded_String (Empty) & To_Unbounded_String ("B");
            Assert (S = "B", "Value = " & To_String (S));

            for Foo of T.Foos loop
                for Baz of T.Bazs loop

                    S := To_Unbounded_String (Foo) & To_Unbounded_String (Baz);
                    Assert (S = Foo & Baz, "Value = " & To_String (S));
                    Assert (S = "FooBaz", "Value = " & To_String (S));

                    for Bar of T.Bars loop

                        S := To_Unbounded_String (Foo) & To_Unbounded_String (Baz) &
                          To_Unbounded_String (Bar);
                        Assert (S = Foo & Baz & Bar, "Value = " & To_String (S));
                        Assert (S = "FooBazBar", "Value = " & To_String (S));

                    end loop;
                end loop;
            end loop;
        end loop;

        S := To_Unbounded_String ("A") & To_Unbounded_String ("B");
        Assert (S = "AB", "Value = " & To_String (S));
    end Test_Amp_Unbounded_Unbounded;


    procedure Test_Amp_Unbounded_String (T : in out TC) is
        S : Unbounded_String;
    begin

        for Empty of T.Empty_Strings loop
            S := Null_Unbounded_String & Empty;
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := To_Unbounded_String (Empty) & Empty;
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := To_Unbounded_String ("A") & Empty;
            Assert (S = "A", "Value = " & To_String (S));

            S := To_Unbounded_String (Empty) & "B";
            Assert (S = "B", "Value = " & To_String (S));


            for Foo of T.Foos loop
                for Baz of T.Bazs loop

                    S := To_Unbounded_String (Foo) & Baz;
                    Assert (S = Foo & Baz, "Value = " & To_String (S));
                    Assert (S = "FooBaz", "Value = " & To_String (S));

                end loop;
            end loop;
        end loop;

        S := To_Unbounded_String ("A") & "B";
        Assert (S = "AB", "Value = " & To_String (S));
    end Test_Amp_Unbounded_String;


    procedure Test_Amp_String_Unbounded (T : in out TC) is
        S : Unbounded_String;
    begin

        for Empty of T.Empty_Strings loop
            S := Empty & Null_Unbounded_String;
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := Empty & To_Unbounded_String (Empty);
            Assert (S = Empty, "Value = " & To_String (S));
            Assert (S = "", "Value = " & To_String (S));

            S := Empty & To_Unbounded_String ("A");
            Assert (S = "A", "Value = " & To_String (S));

            S := "B" & To_Unbounded_String (Empty);
            Assert (S = "B", "Value = " & To_String (S));

            for Foo of T.Foos loop
                for Baz of T.Bazs loop

                    S := Foo & To_Unbounded_String (Baz);
                    Assert (S = Foo & Baz, "Value = " & To_String (S));
                    Assert (S = "FooBaz", "Value = " & To_String (S));

                end loop;
            end loop;
        end loop;

        S := To_Unbounded_String ("A") & "B";
        Assert (S = "AB", "Value = " & To_String (S));
    end Test_Amp_String_Unbounded;

    procedure Test_Amp_Unbounded_Character (T : in out TC) is
        pragma Unreferenced (T);

        S : Unbounded_String;
    begin

        S := To_Unbounded_String ("") & 'B';
        Assert (S = "B", "Value = " & To_String (S));

        S := To_Unbounded_String ("A") & 'B';
        Assert (S = "AB", "Value = " & To_String (S));

        S := To_Unbounded_String ("Foo") & 'B';
        Assert (S = "FooB", "Value = " & To_String (S));

        S := To_Unbounded_String ("Foo") & 'B' & To_Unbounded_String ("Bar");
        Assert (S = "FooBBar", "Value = " & To_String (S));
    end Test_Amp_Unbounded_Character;


    procedure Test_Amp_Character_Unbounded (T : in out TC) is
        pragma Unreferenced (T);

        S : Unbounded_String;
    begin

        S := 'B' & To_Unbounded_String ("");
        Assert (S = "B", "Value = " & To_String (S));

        S := 'A' & To_Unbounded_String ("B");
        Assert (S = "AB", "Value = " & To_String (S));

        S := 'B' & To_Unbounded_String ("Foo");
        Assert (S = "BFoo", "Value = " & To_String (S));

        S := To_Unbounded_String ("Foo") & 'B' & To_Unbounded_String ("Bar");
        Assert (S = "FooBBar", "Value = " & To_String (S));
    end Test_Amp_Character_Unbounded;



    procedure Test_Lt (T : in out TC) is
        pragma Unreferenced (T);
    begin

        -- Unbounded VS Unbounded

        Assert
          ((To_Unbounded_String ("") < To_Unbounded_String ("")) = False,
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < To_Unbounded_String ("")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") < To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < To_Unbounded_String ("B")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") < To_Unbounded_String ("A")) = False,
           "Wrong result.");

        -- Unbounded VS String

        Assert
          ((To_Unbounded_String ("") < "") = False,
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < "") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") < "A"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < "A") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") < "B"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") < "A") = False,
           "Wrong result.");

        -- String VS Unbounded

        Assert
          (("" < To_Unbounded_String ("")) = False,
             "Wrong result.");
        Assert
          (("A" < To_Unbounded_String ("")) = False,
           "Wrong result.");
        Assert
          (("" < To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          (("A" < To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          (("A" < To_Unbounded_String ("B")),
           "Wrong result.");
        Assert
          (("B" < To_Unbounded_String ("A")) = False,
           "Wrong result.");

    end Test_Lt;

    procedure Test_Lte (T : in out TC) is
        pragma Unreferenced (T);
    begin

        -- Unbounded VS Unbounded

        Assert
          ((To_Unbounded_String ("") <= To_Unbounded_String ("")),
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= To_Unbounded_String ("")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") <= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= To_Unbounded_String ("B")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") <= To_Unbounded_String ("A")) = False,
           "Wrong result.");

        -- Unbounded VS String

        Assert
          ((To_Unbounded_String ("") <= ""),
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= "") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") <= "A"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= "A"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") <= "B"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") <= "A") = False,
           "Wrong result.");

        -- String VS Unbounded

        Assert
          (("" <= To_Unbounded_String ("")),
             "Wrong result.");
        Assert
          (("A" <= To_Unbounded_String ("")) = False,
           "Wrong result.");
        Assert
          (("" <= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          (("A" <= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          (("A" <= To_Unbounded_String ("B")),
           "Wrong result.");
        Assert
          (("B" <= To_Unbounded_String ("A")) = False,
           "Wrong result.");

    end Test_Lte;

    procedure Test_Gt (T : in out TC) is
        pragma Unreferenced (T);
    begin

        -- Unbounded VS Unbounded

        Assert
          ((To_Unbounded_String ("") > To_Unbounded_String ("")) = False,
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > To_Unbounded_String ("")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") > To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > To_Unbounded_String ("B")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") > To_Unbounded_String ("A")),
           "Wrong result.");

        -- Unbounded VS String

        Assert
          ((To_Unbounded_String ("") > "") = False,
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > ""),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") > "A") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > "A") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") > "B") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") > "A"),
           "Wrong result.");

        -- String VS Unbounded

        Assert
          (("" > To_Unbounded_String ("")) = False,
             "Wrong result.");
        Assert
          (("A" > To_Unbounded_String ("")),
           "Wrong result.");
        Assert
          (("" > To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          (("A" > To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          (("A" > To_Unbounded_String ("B")) = False,
           "Wrong result.");
        Assert
          (("B" > To_Unbounded_String ("A")),
           "Wrong result.");

    end Test_Gt;

    procedure Test_Gte (T : in out TC) is
        pragma Unreferenced (T);
    begin

        -- Unbounded VS Unbounded

        Assert
          ((To_Unbounded_String ("") >= To_Unbounded_String ("")),
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= To_Unbounded_String ("")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") >= To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= To_Unbounded_String ("B")) = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") >= To_Unbounded_String ("A")),
           "Wrong result.");

        -- Unbounded VS String

        Assert
          ((To_Unbounded_String ("") >= ""),
             "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= ""),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("") >= "A") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= "A"),
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("A") >= "B") = False,
           "Wrong result.");
        Assert
          ((To_Unbounded_String ("B") >= "A"),
           "Wrong result.");

        -- String VS Unbounded

        Assert
          (("" >= To_Unbounded_String ("")),
             "Wrong result.");
        Assert
          (("A" >= To_Unbounded_String ("")),
           "Wrong result.");
        Assert
          (("" >= To_Unbounded_String ("A")) = False,
           "Wrong result.");
        Assert
          (("A" >= To_Unbounded_String ("A")),
           "Wrong result.");
        Assert
          (("A" >= To_Unbounded_String ("B")) = False,
           "Wrong result.");
        Assert
          (("B" >= To_Unbounded_String ("A")),
           "Wrong result.");

    end Test_Gte;

    procedure Test_Element (T : in out TC) is
        pragma Unreferenced (T);

        S : Unbounded_String;
        C : Character;

        procedure Element_1 is
        begin
            C := Element (S, 1);
        end Element_1;

        procedure Element_2 is
        begin
            C := Element (S, 2);
        end Element_2;

    begin

        S := To_Unbounded_String ("");
        Assert_Exception (Element_1'Unrestricted_Access, "Exception expected.");
        Assert_Exception (Element_2'Unrestricted_Access, "Exception expected.");

        S := To_Unbounded_String ("A");
        C := Element (S, 1);
        Assert (C = 'A', "Value = " & C);
        Assert_Exception (Element_2'Unrestricted_Access, "Exception expected.");

        S := To_Unbounded_String ("BC");
        C := Element (S, 1);
        Assert (C = 'B', "Value = " & C);
        C := Element (S, 2);
        Assert (C = 'C', "Value = " & C);

        S := To_Unbounded_String ("XYZ");
        C := Element (S, 1);
        Assert (C = 'X', "Value = " & C);
        C := Element (S, 2);
        Assert (C = 'Y', "Value = " & C);
        C := Element (S, 3);
        Assert (C = 'Z', "Value = " & C);

    end Test_Element;


    procedure Test_Replace_Element (T : in out TC) is
        pragma Unreferenced (T);

        S : Unbounded_String;
        C : Character;
        pragma Unreferenced (C);

        procedure Replace_1 is
        begin
            Replace_Element (S, 1, 'X');
        end Replace_1;

        procedure Replace_2 is
        begin
            Replace_Element (S, 2, 'Y');
        end Replace_2;

    begin

        S := To_Unbounded_String ("");
        Assert_Exception (Replace_1'Unrestricted_Access, "Exception expected.");
        Assert_Exception (Replace_2'Unrestricted_Access, "Exception expected.");

        S := To_Unbounded_String ("A");
        Replace_Element (S, 1, 'Q');
        Assert (S = "Q", "Value = " & To_String (S));
        Assert_Exception (Replace_2'Unrestricted_Access, "Exception expected.");

        S := To_Unbounded_String ("AB");
        Replace_Element (S, 1, 'X');
        Assert (S = "XB", "Value = " & To_String (S));
        Replace_Element (S, 2, 'Y');
        Assert (S = "XY", "Value = " & To_String (S));

        S := To_Unbounded_String ("XYZ");
        Replace_Element (S, 3, 'C');
        Assert (S = "XYC", "Value = " & To_String (S));
        Replace_Element (S, 2, 'B');
        Assert (S = "XBC", "Value = " & To_String (S));
        Replace_Element (S, 1, 'A');
        Assert (S = "ABC", "Value = " & To_String (S));

    end Test_Replace_Element;



    procedure Test_Serialization (T : in out TC) is
        pragma Unreferenced (T);

        File_Name : constant String := "/tmp/_test_ustrings_";

        procedure Serialize (S : Unbounded_String) is
            use Ada.Streams;

            SFile : Stream_IO.File_Type;
            SAcc  : Stream_IO.Stream_Access;
        begin
            Stream_IO.Create (SFile, Stream_IO.Out_File, File_Name);
            SAcc := Stream_IO.Stream (SFile);

            Unbounded_String'Output (SAcc, S);

            Stream_IO.Close (SFile);
        end Serialize;

        function Load_Serialized return Unbounded_String is
            use Ada.Streams;

            SFile : Stream_IO.File_Type;
            SAcc  : Stream_IO.Stream_Access;
        begin
            return S : Unbounded_String do
                Stream_IO.Open (SFile, Stream_IO.In_File, File_Name);
                SAcc := Stream_IO.Stream (SFile);

                S := Unbounded_String'Input (SAcc);

                Stream_IO.Close (SFile);
            end return;
        end Load_Serialized;

        X : Unbounded_String;
        Y : Unbounded_String;
    begin

        X := To_Unbounded_String ("Foo Bar");
        Serialize (X);

        -----
        -- Stream_IO.Reset (SFile);
        -----

        X := To_Unbounded_String ("Lorem ipsum");
        Y := To_Unbounded_String ("dolor sit amet");

        Y := Load_Serialized;
        Assert (Y = "Foo Bar", "Value = " & To_String (Y));

    end Test_Serialization;

end ARColl.Strings.Unbounded.Test;
