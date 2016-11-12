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

package body ARColl.Strings.Unbounded is

    procedure Initialize (Object : in out Unbounded_String) is
    begin
        Object := Null_Unbounded_String;
    end Initialize;


    procedure Adjust (Object : in out Unbounded_String) is
    begin
        if Object.Value /= null then
            Object.Value := new String'(Object.Value.all);
        end if;
    end Adjust;


    procedure Finalize (Object : in out Unbounded_String) is
    begin
        Free (Object.Value);
    end Finalize;


    function Length (Source : Unbounded_String) return Natural is
    begin
        if Source.Value = null then
            return 0;
        else
            return Source.Last;
        end if;
    end Length;


    function To_Unbounded_String
      (Source : String)
       return Unbounded_String is

        S : Unbounded_String;
    begin
        if Source'Length > 0 then
            Ensure_Allocation (S, Source'Length);
            S.Value (1 .. Source'Length) := Source;
            S.Last := Source'Length;
        end if;

        return S;
    end To_Unbounded_String;


    function To_String (Source : Unbounded_String) return String is
    begin
        if Source.Value = null then
            return "";
        else
            return Source.Value (Source.Value'First .. Source.Last);
        end if;
    end To_String;


    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : Unbounded_String) is
    begin
        if Length (New_Item) > 0 then
            Ensure_Allocation (Source, Length (Source) + New_Item.Last);

            Source.Value (Source.Last + 1 .. Source.Last + New_Item.Last)
              := New_Item.Value(New_Item.Value'First .. New_Item.Last);

            Source.Last := Source.Last + New_Item.Last;
        end if;
    end Append;


    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : String) is
    begin
        if New_Item'Length > 0 then
            Ensure_Allocation (Source, Length (Source) + New_Item'Length);

            Source.Value (Source.Last + 1 .. Source.Last + New_Item'Length) := New_Item;
            Source.Last := Source.Last + New_Item'Length;
        end if;
    end Append;


    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : Character) is
    begin
        Ensure_Allocation (Source, Length (Source) + 1);
        Source.Value (Source.Last + 1) := New_Item;
        Source.Last := Source.Last + 1;
    end Append;


    function "&"
      (Left  : Unbounded_String;
       Right : Unbounded_String)
       return Unbounded_String is
    begin
        if Left.Value /= null and then Right.Value /= null then
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := Left.Last + Right.Last;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1 .. Left.Last) := Left.Value (1 .. Left.Last);

                Ret.Value (Left.Last + 1 .. Ret.Last)
                  := Right.Value (1 .. Right.Last);

                return Ret;
            end;

        elsif Left.Value /= null and then Right.Value = null then
            return Left;

        elsif Left.Value = null and then Right.Value /= null then
            return Right;

        else
            return Null_Unbounded_String;
        end if;
    end "&";


    function "&"
      (Left  : Unbounded_String;
       Right : String)
       return Unbounded_String is
    begin
        if Left.Value /= null and then Right'Length > 0 then
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := Left.Last + Right'Length;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1 .. Left.Last) := Left.Value (1 .. Left.Last);
                Ret.Value (Left.Last + 1 .. Ret.Last) := Right;

                return Ret;
            end;

        elsif Left.Value /= null and then Right'Length = 0 then
            return Left;

        elsif Left.Value = null and then Right'Length > 0 then
            return To_Unbounded_String (Right);

        else
            return Null_Unbounded_String;
        end if;
    end "&";

    function "&"
      (Left  : String;
       Right : Unbounded_String)
       return Unbounded_String is
    begin
        if Left'Length > 0 and then Right.Value /= null then
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := Left'Length + Right.Last;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1 .. Left'Length) := Left;
                Ret.Value (Left'Length + 1 .. Ret.Last) := Right.Value (1 .. Right.Last);

                return Ret;
            end;

        elsif Left'Length > 0 and then Right.Value = null then
            return To_Unbounded_String (Left);

        elsif Left'Length = 0 and then Right.Value /= null then
            return Right;

        else
            return Null_Unbounded_String;
        end if;
    end "&";


    function "&"
      (Left  : Unbounded_String;
       Right : Character)
       return Unbounded_String is
    begin
        if Left.Value /= null then
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := Left.Last + 1;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1 .. Left.Last) := Left.Value (1 .. Left.Last);
                Ret.Value (Left.Last + 1) := Right;

                return Ret;
            end;

        else
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := 1;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1) := Right;

                return Ret;
            end;
        end if;
    end "&";


    function "&"
      (Left  : Character;
       Right : Unbounded_String)
       return Unbounded_String is
    begin
        if Right.Value /= null then
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := Right.Last + 1;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1) := Left;
                Ret.Value (2 .. Ret.Last) := Right.Value (1 .. Right.Last);

                return Ret;
            end;

        else
            declare
                Ret : Unbounded_String;
            begin
                Ret.Last := 1;
                Ensure_Allocation (Ret, Ret.Last);

                Ret.Value (1) := Left;

                return Ret;
            end;
        end if;
    end "&";


    function Element
      (Source : Unbounded_String;
       Index  : Positive)
       return Character is
    begin
        if Source.Value /= null and then Index <= Source.Last then
            return Source.Value (Index);
        else
            raise Ada.Strings.Index_Error;
        end if;
    end Element;

    procedure Replace_Element
      (Source : in out Unbounded_String;
       Index  : Positive;
       By     : Character) is
    begin
        if Source.Value /= null and then Index <= Source.Last then
            Source.Value (Index) := By;
        else
            raise Ada.Strings.Index_Error;
        end if;
    end Replace_Element;

    function "="
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean is
    begin
        if Left.Value = null and then Right.Value = null then
            return True;

        elsif (Left.Value = null and then Right.Value /= null)
          or else (Left.Value /= null and then Right.Value = null) then
            return False;

        else
            return (Left.Value (Left.Value'First .. Left.Last)
                    = Right.Value (Right.Value'First .. Right.Last));
        end if;
    end "=";


    function "="
      (Left  : Unbounded_String;
       Right : String) return Boolean is
    begin
        if Left.Value = null then
            return "" = Right;
        else
            return Left.Value (Left.Value'First .. Left.Last) = Right;
        end if;
    end "=";

    function "<"
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean is
    begin
        if Left.Value = null and then Right.Value = null then
            return False;
        elsif Left.Value /= null and then Right.Value = null then
            return Left.Value (1 .. Left.Last) < "";
        elsif Left.Value = null and then Right.Value /= null then
            return "" < Right.Value (1 .. Right.Last);
        else
            return Left.Value (1 .. Left.Last) < Right.Value (1 .. Right.Last);
        end if;
    end "<";

    function "<"
      (Left  : Unbounded_String;
       Right : String) return Boolean is
    begin
        if Left.Value = null then
            return "" < Right;
        else
            return Left.Value (1 .. Left.Last) < Right;
        end if;
    end "<";

    function "<"
      (Left  : String;
       Right : Unbounded_String) return Boolean is
    begin
        if Right.Value = null then
            return Left < "";
        else
            return Left < Right.Value (1 .. Right.Last);
        end if;
    end "<";

    function "<="
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean is
    begin
        if Left.Value = null and then Right.Value = null then
            return True;
        elsif Left.Value /= null and then Right.Value = null then
            return Left.Value (1 .. Left.Last) <= "";
        elsif Left.Value = null and then Right.Value /= null then
            return "" <= Right.Value (1 .. Right.Last);
        else
            return Left.Value (1 .. Left.Last) <= Right.Value (1 .. Right.Last);
        end if;
    end "<=";

    function "<="
      (Left  : Unbounded_String;
       Right : String) return Boolean is
    begin
        if Left.Value = null then
            return "" <= Right;
        else
            return Left.Value (1 .. Left.Last) <= Right;
        end if;
    end "<=";

    function "<="
      (Left  : String;
       Right : Unbounded_String) return Boolean is
    begin
        if Right.Value = null then
            return Left <= "";
        else
            return Left <= Right.Value (1 .. Right.Last);
        end if;
    end "<=";

    function ">"
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean is
    begin
        if Left.Value = null and then Right.Value = null then
            return False;
        elsif Left.Value /= null and then Right.Value = null then
            return Left.Value (1 .. Left.Last) > "";
        elsif Left.Value = null and then Right.Value /= null then
            return "" > Right.Value (1 .. Right.Last);
        else
            return Left.Value (1 .. Left.Last) > Right.Value (1 .. Right.Last);
        end if;
    end ">";

    function ">"
      (Left  : Unbounded_String;
       Right : String) return Boolean is
    begin
        if Left.Value = null then
            return "" > Right;
        else
            return Left.Value (1 .. Left.Last) > Right;
        end if;
    end ">";

    function ">"
      (Left  : String;
       Right : Unbounded_String) return Boolean is
    begin
        if Right.Value = null then
            return Left > "";
        else
            return Left > Right.Value (1 .. Right.Last);
        end if;
    end ">";

    function ">="
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean is
    begin
        if Left.Value = null and then Right.Value = null then
            return True;
        elsif Left.Value /= null and then Right.Value = null then
            return Left.Value (1 .. Left.Last) >= "";
        elsif Left.Value = null and then Right.Value /= null then
            return "" >= Right.Value (1 .. Right.Last);
        else
            return Left.Value (1 .. Left.Last) >= Right.Value (1 .. Right.Last);
        end if;
    end ">=";

    function ">="
      (Left  : Unbounded_String;
       Right : String) return Boolean is
    begin
        if Left.Value = null then
            return "" >= Right;
        else
            return Left.Value (1 .. Left.Last) >= Right;
        end if;
    end ">=";

    function ">="
      (Left  : String;
       Right : Unbounded_String) return Boolean is
    begin
        if Right.Value = null then
            return Left >= "";
        else
            return Left >= Right.Value (1 .. Right.Last);
        end if;
    end ">=";


    procedure Ensure_Allocation (Object : in out Unbounded_String; Last : Natural) is
    begin

        if Object.Value /= null and then Last > Object.Value'Last then
            declare
                New_Value : constant String_Access
                  := new String (1 .. Last + Last / Object.Growth_Factor);
            begin
                New_Value (Object.Value'Range) := Object.Value.all;
                Free (Object.Value);
                Object.Value := New_Value;
            end;

        elsif Object.Value = null and then Last > 0 then
            Object.Value := new String (1 .. Last + Last / Object.Growth_Factor);
        end if;

    end Ensure_Allocation;


    function Get_Allocated_Length (Source : Unbounded_String) return Natural is
    begin
        if Source.Value = null then
            return 0;
        else
            return Source.Value'Last;
        end if;
    end Get_Allocated_Length;

    procedure Replace_Slice
      (Source : in out Unbounded_String;
       Low    : Positive;
       High   : Natural;
       By     : String) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Replace_Slice
             (Source => To_String (Source),
              Low    => Low,
              High   => High,
              By     => By));
    end Replace_Slice;

    procedure Insert
      (Source   : in out Unbounded_String;
       Before   : Positive;
       New_Item : String) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Insert
             (Source   => To_String (Source),
              Before   => Before,
              New_Item => New_Item));
    end Insert;

    procedure Trim
      (Source : in out Unbounded_String;
       Side   : Ada.Strings.Trim_End) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Trim
             (Source => To_String (Source),
              Side   => Side));
    end Trim;


    procedure Trim
      (Source : in out Unbounded_String;
       Left   : Ada.Strings.Maps.Character_Set;
       Right  : Ada.Strings.Maps.Character_Set) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Trim
             (Source => To_String (Source),
              Left   => Left,
              Right  => Right));
    end Trim;

    procedure Head
      (Source : in out Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Head
             (Source => To_String (Source),
              Count  => Count,
              Pad    => Pad));
    end Head;

    procedure Tail
      (Source : in out Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space) is
    begin
        Source := To_Unbounded_String
          (Ada.Strings.Fixed.Tail
             (Source => To_String (Source),
              Count  => Count,
              Pad    => Pad));
    end Tail;


    function Join
      (String_Set : String_Sets.Set;
       Separator  : String)
       return Unbounded_String is
    begin
        return Ret : Unbounded_String do

            for Item of String_Set loop
                if Length (Ret) > 0 then
                    Append (Ret, Separator);
                end if;
                Append (Ret, Item);
            end loop;

        end return;
    end Join;


    procedure String_Replace (S : in out Unbounded_String; Pattern, Replacement : String) is
    -- example: if S is "Mary had a XX lamb", then String_Replace(S, "X", "little");
    --          will turn S into "Mary had a littlelittle lamb"
    --          and String_Replace(S, "Y", "small"); will not change S

        Index : Natural;
    begin
        loop
            Index := Unbounded.Index (Source => S, Pattern => Pattern);
            exit when Index = 0;
            Unbounded.Replace_Slice
              (Source => S, Low => Index, High => Index + Pattern'Length - 1,
               By     => Replacement);
        end loop;
    end String_Replace;

end ARColl.Strings.Unbounded;
