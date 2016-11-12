
with AUnit;
with AUnit.Test_Fixtures;

package ARColl.Strings.Unbounded.Test is

    type TC is new AUnit.Test_Fixtures.Test_Fixture with record
        Empty_Strings : String_Vectors.Vector;
        Foos          : String_Vectors.Vector;
        Bazs          : String_Vectors.Vector;
        Bars          : String_Vectors.Vector;
    end record;

    overriding procedure Set_Up (T : in out TC);
    overriding procedure Tear_Down (T : in out TC);

    procedure Test_To_Unbounded_String (T : in out TC);
    procedure Test_To_String (T : in out TC);

    procedure Test_Append_Unbounded_String (T : in out TC);
    procedure Test_Append_String_1 (T : in out TC);
    procedure Test_Append_Character (T : in out TC);

    procedure Test_Length_With_Append_Unbounded_String (T : in out TC);
    procedure Test_Length_With_Append_String (T : in out TC);
    procedure Test_Length_With_Append_Character (T : in out TC);

    procedure Test_Allocated_Length_With_Append_Unbounded_String (T : in out TC);
    procedure Test_Allocated_Length_With_Append_String (T : in out TC);
    procedure Test_Allocated_Length_With_Append_Character (T : in out TC);

    procedure Test_Equals_Unbounded_Unbounded (T : in out TC);
    procedure Test_Equals_Unbounded_String (T : in out TC);

    procedure Test_Amp_Unbounded_Unbounded (T : in out TC);
    procedure Test_Amp_Unbounded_String (T : in out TC);
    procedure Test_Amp_String_Unbounded (T : in out TC);
    procedure Test_Amp_Unbounded_Character (T : in out TC);
    procedure Test_Amp_Character_Unbounded (T : in out TC);

    procedure Test_Lt (T : in out TC);
    procedure Test_Lte (T : in out TC);
    procedure Test_Gt (T : in out TC);
    procedure Test_Gte (T : in out TC);

    procedure Test_Element (T : in out TC);
    procedure Test_Replace_Element (T : in out TC);

    procedure Test_Serialization (T : in out TC);

end ARColl.Strings.Unbounded.Test;
