
with AUnit.Reporter.Text;
with AUnit.Run;

with ARColl_Suite; use ARColl_Suite;

procedure ARColl_Harness is

    procedure Run_Strings_Unbounded_Suite is
        procedure Runner is new AUnit.Run.Test_Runner (Strings_Unbounded_Suite);
        Reporter : AUnit.Reporter.Text.Text_Reporter;
    begin
        Runner (Reporter);
    end Run_Strings_Unbounded_Suite;

    procedure Run_Containers_ID_Bimaps_Strings_Suite is
        procedure Runner is new AUnit.Run.Test_Runner (Containers_ID_Bimaps_Strings_Suite);
        Reporter : AUnit.Reporter.Text.Text_Reporter;
    begin
        Runner (Reporter);
    end Run_Containers_ID_Bimaps_Strings_Suite;

    procedure Run_Containers_ID_Bimaps_Strings_ME_Suite is
        procedure Runner is new AUnit.Run.Test_Runner (Containers_ID_Bimaps_Strings_ME_Suite);
        Reporter : AUnit.Reporter.Text.Text_Reporter;
    begin
        Runner (Reporter);
    end Run_Containers_ID_Bimaps_Strings_ME_Suite;

    procedure Run_Containers_Bounded_ID_Bimaps_Strings_Suite is
        procedure Runner is new AUnit.Run.Test_Runner (Containers_Bounded_ID_Bimaps_Strings_Suite);
        Reporter : AUnit.Reporter.Text.Text_Reporter;
    begin
        Runner (Reporter);
    end Run_Containers_Bounded_ID_Bimaps_Strings_Suite;

    procedure Run_Numerics_Reals_Functions_Suite is
        procedure Runner is new AUnit.Run.Test_Runner (Numerics_Reals_Functions_Suite);
        Reporter : AUnit.Reporter.Text.Text_Reporter;
    begin
        Runner (Reporter);
    end Run_Numerics_Reals_Functions_Suite;

begin

    Run_Strings_Unbounded_Suite;
    Run_Containers_ID_Bimaps_Strings_Suite;
    Run_Containers_ID_Bimaps_Strings_ME_Suite;
    Run_Containers_Bounded_ID_Bimaps_Strings_Suite;
    Run_Numerics_Reals_Functions_Suite;

end ARColl_Harness;
