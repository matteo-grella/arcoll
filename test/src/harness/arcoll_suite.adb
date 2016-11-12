
with AUnit.Test_Caller;

with ARColl.Strings.Unbounded.Test;
with ARColl.Containers.Bimaps.ID_Bimaps_Strings_Test;
with ARColl.Containers.Bimaps.ID_Bimaps_Strings_ME_Test;
with ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test;
with ARColl.Numerics.Reals.Functions_Test;

package body ARColl_Suite is

    package Numerics_Reals_Functions_Suite_Caller is new AUnit.Test_Caller (ARColl.Numerics.Reals.Functions_Test.TC);
    function Numerics_Reals_Functions_Suite return Access_Test_Suite is
        package Caller renames Numerics_Reals_Functions_Suite_Caller;
        TS_Ptr : constant Access_Test_Suite := New_Suite;

        use ARColl.Numerics.Reals.Functions_Test;
    begin

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Numerics.Reals.Functions_Test.Test_Array_Addition",
              Test_Array_Addition'Access));

        return TS_Ptr;
    end Numerics_Reals_Functions_Suite;

    package Containers_Bounded_ID_Bimaps_Strings_Caller is new AUnit.Test_Caller (ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test.TC);
    function Containers_Bounded_ID_Bimaps_Strings_Suite return Access_Test_Suite is
        package Caller renames Containers_Bounded_ID_Bimaps_Strings_Caller;
        TS_Ptr : constant Access_Test_Suite := New_Suite;

        use ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test;
    begin

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : ARColl.Containers.Bimaps.Bounded_ID_Bimaps_Strings_Test.Test",
              Test'Access));


        return TS_Ptr;
    end Containers_Bounded_ID_Bimaps_Strings_Suite;

    package Containers_ID_Bimaps_Strings_ME_Caller is new AUnit.Test_Caller (ARColl.Containers.Bimaps.ID_Bimaps_Strings_ME_Test.TC);
    function Containers_ID_Bimaps_Strings_ME_Suite return Access_Test_Suite is
        package Caller renames Containers_ID_Bimaps_Strings_ME_Caller;
        TS_Ptr : constant Access_Test_Suite := New_Suite;

        use ARColl.Containers.Bimaps.ID_Bimaps_Strings_ME_Test;
    begin

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_Insert_Inserted",
              Test_Insert_Inserted'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_Insert_Exception",
              Test_Insert_Exception'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_Length_And_Clear",
              Test_Length_And_Clear'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_Contains_Contains_ID_And_Element",
              Test_Contains_ID_And_Element'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_First_And_Last_ID",
              Test_First_And_Last_ID'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_ME_Test.Test_Get_ID_And_Element",
              Test_Get_ID_And_Element'Access));

        return TS_Ptr;
    end Containers_ID_Bimaps_Strings_ME_Suite;

    package Containers_ID_Bimaps_Strings_Caller is new AUnit.Test_Caller (ARColl.Containers.Bimaps.ID_Bimaps_Strings_Test.TC);
    function Containers_ID_Bimaps_Strings_Suite return Access_Test_Suite is
        package Caller renames Containers_ID_Bimaps_Strings_Caller;
        TS_Ptr : constant Access_Test_Suite := New_Suite;

        use ARColl.Containers.Bimaps.ID_Bimaps_Strings_Test;
    begin

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_Insert_Inserted",
              Test_Insert_Inserted'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_Insert_Exception",
              Test_Insert_Exception'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_Length_And_Clear",
              Test_Length_And_Clear'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_Contains_Contains_ID_And_Element",
              Test_Contains_ID_And_Element'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_First_And_Last_ID",
              Test_First_And_Last_ID'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Containers.ID_Bimaps_Strings_Test.Test_Get_ID_And_Element",
              Test_Get_ID_And_Element'Access));

        return TS_Ptr;
    end Containers_ID_Bimaps_Strings_Suite;

    package Strings_Unbounded_Caller is new AUnit.Test_Caller (ARColl.Strings.Unbounded.Test.TC);
    function Strings_Unbounded_Suite return Access_Test_Suite is
        TS_Ptr : constant Access_Test_Suite := New_Suite;
        package Caller renames Strings_Unbounded_Caller;
    begin

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_To_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_To_Unbounded_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_To_String",
              ARColl.Strings.Unbounded.Test.Test_To_String'Access));

        -- Test Append

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Append_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_Append_Unbounded_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Append_String",
              ARColl.Strings.Unbounded.Test.Test_Append_String_1'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Append_Character",
              ARColl.Strings.Unbounded.Test.Test_Append_Character'Access));

        -- Test Length

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Length_With_Append_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_Length_With_Append_Unbounded_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Length_With_Append_String",
              ARColl.Strings.Unbounded.Test.Test_Length_With_Append_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Length_With_Append_Character",
              ARColl.Strings.Unbounded.Test.Test_Length_With_Append_Character'Access));

        -- Test Allocated Length

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Allocated_Length_With_Append_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_Allocated_Length_With_Append_Unbounded_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Allocated_Length_With_Append_String",
              ARColl.Strings.Unbounded.Test.Test_Allocated_Length_With_Append_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Allocated_Length_With_Append_Character",
              ARColl.Strings.Unbounded.Test.Test_Allocated_Length_With_Append_Character'Access));

        -- Test "=" (Equals)

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Equals_Unbounded_Unbounded",
              ARColl.Strings.Unbounded.Test.Test_Equals_Unbounded_Unbounded'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Equals_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_Equals_Unbounded_String'Access));

        -- Test "&" (Amp)

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Amp_Unbounded_Unbounded",
              ARColl.Strings.Unbounded.Test.Test_Amp_Unbounded_Unbounded'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Amp_Unbounded_String",
              ARColl.Strings.Unbounded.Test.Test_Amp_Unbounded_String'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Amp_String_Unbounded",
              ARColl.Strings.Unbounded.Test.Test_Amp_String_Unbounded'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Amp_Unbounded_Character",
              ARColl.Strings.Unbounded.Test.Test_Amp_Unbounded_Character'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Amp_Character_Unbounded",
              ARColl.Strings.Unbounded.Test.Test_Amp_Character_Unbounded'Access));

        -- < <= > >=

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Lt",
              ARColl.Strings.Unbounded.Test.Test_Lt'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Lte",
              ARColl.Strings.Unbounded.Test.Test_Lte'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Gt",
              ARColl.Strings.Unbounded.Test.Test_Gt'Access));

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Gte",
              ARColl.Strings.Unbounded.Test.Test_Gte'Access));

        -- Element

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Element",
              ARColl.Strings.Unbounded.Test.Test_Element'Access));

        -- Replace Element

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Replace_Element",
              ARColl.Strings.Unbounded.Test.Test_Replace_Element'Access));

        -- Serialization

        TS_Ptr.Add_Test
          (Caller.Create
             ("Test-case: Fixture : Strings.Unbounded.Test_Serialization",
              ARColl.Strings.Unbounded.Test.Test_Serialization'Access));


        return TS_Ptr;
    end Strings_Unbounded_Suite;

end ARColl_Suite;
