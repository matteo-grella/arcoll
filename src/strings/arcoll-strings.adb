-----------------------------------------------------------------------------
--                               A R C O L L
--       A d a   R e l a x   C o m p o n e n t   C o l l e c t i o n
--
--                 Copyright 2009-2014 M. Grella, M. Nicola
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-----------------------------------------------------------------------------

pragma License (Modified_GPL);

with GNAT.Regpat;
with Ada.Strings.Fixed.Hash;

package body ARColl.Strings is

    procedure Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        String_Access) is
    begin
        if Obj = null then
            Boolean'Write (Stream, False);
        else
            Boolean'Write (Stream, True);
            Natural'Write (Stream, Obj'First);
            Natural'Write (Stream, Obj'Last);
            String'Write (Stream, Obj.all);
        end if;

    end Write;

    procedure Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    String_Access) is

        Not_Null    : Boolean;
        First, Last : Natural;
    begin
        Boolean'Read (Stream, Not_Null);
        if Not_Null then
            Natural'Read (Stream, First);
            Natural'Read (Stream, Last);
            Obj := new String (First .. Last);
            String'Read (Stream, Obj.all);
        else
            Obj := null;
        end if;
    end Read;

    procedure Print_String_Set
      (Set   : String_Sets.Set;
       Title : String := "";
       File  : Text_IO.File_Type := Text_IO.Standard_Output) is
        use Text_IO;
    begin
        if Title /= "" then
            Put_Line (File, "-----");
            Put_Line (File, "-- " & Title);
            Put_Line (File, "-----");
        end if;

        for Item of Set loop
            Put_Line (File, Item);
        end loop;

        New_Line;

    end Print_String_Set;

    function Is_Null (Acc_Str : String_Access) return Boolean is
    begin
        return Acc_Str = null;
    end Is_Null;

    function String_Equal (String_A_Access : String_Access; String_B_Access : String_Access) return Boolean is
    begin
        return not Is_Null (String_A_Access) and then not Is_Null (String_B_Access) and then String_A_Access.all = String_B_Access.all;
    end String_Equal;

    function String_Equal (String_A_Access : String_Access; String_B_Fixed : String) return Boolean is
    begin
        return not Is_Null (String_A_Access) and then String_A_Access.all = String_B_Fixed;
    end String_Equal;

    function Search_Regexp (Pattern : String; Search_In : String) return Boolean is
        Re      : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Pattern);
        Matches : GNAT.Regpat.Match_Array (0 .. 1);
        use type GNAT.Regpat.Match_Location;
    begin
        GNAT.Regpat.Match (Re, Search_In, Matches);
        if Matches (0) = GNAT.Regpat.No_Match then
            return False;
        else
            return True;
        end if;
    end Search_Regexp;
    
    procedure Collect_Strings
      (Output_List : out String_Vectors.Vector;
       Input_List  : in  String_Vectors.Vector;
       Cutoff      : in  Positive := 1) is

        package String_Frequency_Maps is new
          Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => String,
             Element_Type    => Positive,
             Hash            => Ada.Strings.Fixed.Hash,
             Equivalent_Keys => "=");

        Frequency   : String_Frequency_Maps.Map;
        Sorted_Keys : String_Vectors.Vector;

        --------------------------
        function Reverse_Less_Than
        --------------------------
          (Left, Right : String) return Boolean with Inline is
            Left_Frequency  : constant Positive := Frequency.Element (Left);
            Right_Frequency : constant Positive := Frequency.Element (Right);
        begin
            return (if Left_Frequency = Right_Frequency
                    then Left < Right
                    else Left_Frequency > Right_Frequency);
        end Reverse_Less_Than;

        package Sorting is new
          String_Vectors.Generic_Sorting
            ("<" => Reverse_Less_Than);
    begin
        Frequency.Reserve_Capacity (Input_List.Length);
        Sorted_Keys.Reserve_Capacity (Input_List.Length);
        -- These capacities are too much, but make next loop faster

        -- Calculate frequencies and fill 'Sorted_Keys'

        for Str : String of Input_List loop
            if Frequency.Contains (Str) then
                Frequency.Replace (Str, Frequency.Element (Str) + 1);
            else
                Frequency.Insert (Str, 1);
                Sorted_Keys.Append (Str);
            end if;
        end loop;

        -- Sort 'Sorted_Keys'

        Sorting.Sort (Sorted_Keys);

        -- Build output

        Output_List.Clear;
        for Str of Sorted_Keys loop
            if Frequency.Element (Str) >= Cutoff then
                Output_List.Append (Str);
            end if;
        end loop;

    end Collect_Strings;
    
end ARColl.Strings;
