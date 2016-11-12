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

with Ada.Finalization;
with Ada.Strings;
with Ada.Strings.Maps;

package ARColl.Strings.Unbounded is

    type Unbounded_String is private;

    Null_Unbounded_String : constant Unbounded_String;

    function Length (Source : Unbounded_String) return Natural with Inline;

    --------------------------------------------------------
    -- Conversion, Concatenation, and Selection Functions --
    --------------------------------------------------------

    function To_Unbounded_String
      (Source : String)
       return Unbounded_String
      with Inline;

    -- TODO:
    --      function To_Unbounded_String
    --        (Length : Natural) return Unbounded_String;

    function To_String (Source : Unbounded_String) return String with Inline;

    -- TODO:
    --      procedure Set_Unbounded_String
    --        (Target : out Unbounded_String;
    --         Source : String);
    --      pragma Ada_05 (Set_Unbounded_String);

    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : Unbounded_String)
      with Inline;

    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : String)
      with Inline;

    procedure Append
      (Source   : in out Unbounded_String;
       New_Item : Character)
      with Inline;

    function "&"
      (Left  : Unbounded_String;
       Right : Unbounded_String)
       return Unbounded_String
      with Inline;

    function "&"
      (Left  : Unbounded_String;
       Right : String)
       return Unbounded_String
      with Inline;

    function "&"
      (Left  : String;
       Right : Unbounded_String)
       return Unbounded_String
    with Inline;

    function "&"
      (Left  : Unbounded_String;
       Right : Character)
       return Unbounded_String
      with Inline;

    function "&"
      (Left  : Character;
       Right : Unbounded_String)
       return Unbounded_String
    with Inline;

    function Element
      (Source : Unbounded_String;
       Index  : Positive)
       return Character
      with Inline;

    procedure Replace_Element
      (Source : in out Unbounded_String;
       Index  : Positive;
       By     : Character)
      with Inline;

    -- TODO:
    --      function Slice
    --        (Source : Unbounded_String;
    --         Low    : Positive;
    --         High   : Natural) return String;
    --
    --      function Unbounded_Slice
    --        (Source : Unbounded_String;
    --         Low    : Positive;
    --         High   : Natural) return Unbounded_String;
    --      pragma Ada_05 (Unbounded_Slice);
    --
    --      procedure Unbounded_Slice
    --        (Source : Unbounded_String;
    --         Target : out Unbounded_String;
    --         Low    : Positive;
    --         High   : Natural);
    --      pragma Ada_05 (Unbounded_Slice);

    function "="
      (Left  : Unbounded_String;
       Right : Unbounded_String)
       return Boolean
      with Inline;

    function "="
      (Left  : Unbounded_String;
       Right : String) return Boolean
      with Inline;

    function "="
      (Left  : String;
       Right : Unbounded_String)
       return Boolean is
      ("=" (Right, Left))
    with Inline;

    function "<"
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean with Inline;

    function "<"
      (Left  : Unbounded_String;
       Right : String) return Boolean with Inline;

    function "<"
      (Left  : String;
       Right : Unbounded_String) return Boolean with Inline;

    function "<="
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean with Inline;

    function "<="
      (Left  : Unbounded_String;
       Right : String) return Boolean with Inline;

    function "<="
      (Left  : String;
       Right : Unbounded_String) return Boolean with Inline;

    function ">"
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean with Inline;

    function ">"
      (Left  : Unbounded_String;
       Right : String) return Boolean with Inline;

    function ">"
      (Left  : String;
       Right : Unbounded_String) return Boolean with Inline;

    function ">="
      (Left  : Unbounded_String;
       Right : Unbounded_String) return Boolean with Inline;

    function ">="
      (Left  : Unbounded_String;
       Right : String) return Boolean with Inline;

    function ">="
      (Left  : String;
       Right : Unbounded_String) return Boolean with Inline;

    ------------------------
    -- Search Subprograms --
    ------------------------

    function Index
      (Source  : Unbounded_String;
       Pattern : String;
       Going   : Ada.Strings.Direction := Ada.Strings.Forward;
       Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => To_String (Source),
          Pattern => Pattern,
          Going   => Going,
          Mapping => Mapping))
    with Inline;

    function Index
      (Source  : Unbounded_String;
       Pattern : String;
       Going   : Ada.Strings.Direction := Ada.Strings.Forward;
       Mapping : Ada.Strings.Maps.Character_Mapping_Function)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => To_String (Source),
          Pattern => Pattern,
          Going   => Going,
          Mapping => Mapping))
    with Inline;

    function Index
      (Source : Unbounded_String;
       Set    : Ada.Strings.Maps.Character_Set;
       Test   : Ada.Strings.Membership := Ada.Strings.Inside;
       Going  : Ada.Strings.Direction  := Ada.Strings.Forward)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => To_String (Source),
          Set     => Set,
          Test    => Test,
          Going   => Going))
    with Inline;

    function Index
      (Source  : Unbounded_String;
       Pattern : String;
       From    : Positive;
       Going   : Ada.Strings.Direction := Ada.Strings.Forward;
       Mapping : Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => To_String (Source),
          Pattern => Pattern,
          From    => From,
          Going   => Going,
          Mapping => Mapping))
    with Inline;
    pragma Ada_05 (Index);

    function Index
      (Source  : Unbounded_String;
       Pattern : String;
       From    : Positive;
       Going   : Ada.Strings.Direction := Ada.Strings.Forward;
       Mapping : Ada.Strings.Maps.Character_Mapping_Function)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => To_String (Source),
          Pattern => Pattern,
          From    => From,
          Going   => Going,
          Mapping => Mapping))
    with Inline;
    pragma Ada_05 (Index);

    function Index
      (Source  : Unbounded_String;
       Set     : Ada.Strings.Maps.Character_Set;
       From    : Positive;
       Test    : Ada.Strings.Membership := Ada.Strings.Inside;
       Going   : Ada.Strings.Direction := Ada.Strings.Forward)
       return Natural is
      (Ada.Strings.Fixed.Index
         (Source => To_String (Source),
          Set    => Set,
          From   => From,
          Test   => Test,
          Going  => Going))
    with Inline;
    pragma Ada_05 (Index);

    -- TODO:
    --      function Index_Non_Blank
    --        (Source : Unbounded_String;
    --         Going  : Direction := Forward) return Natural;
    --
    --      function Index_Non_Blank
    --        (Source : Unbounded_String;
    --         From   : Positive;
    --         Going  : Direction := Forward) return Natural;
    --      pragma Ada_05 (Index_Non_Blank);
    --
    --      function Count
    --        (Source  : Unbounded_String;
    --         Pattern : String;
    --         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
    --
    --      function Count
    --        (Source  : Unbounded_String;
    --         Pattern : String;
    --         Mapping : Maps.Character_Mapping_Function) return Natural;
    --
    --      function Count
    --        (Source : Unbounded_String;
    --         Set    : Maps.Character_Set) return Natural;
    --
    --      procedure Find_Token
    --        (Source : Unbounded_String;
    --         Set    : Maps.Character_Set;
    --         From   : Positive;
    --         Test   : Membership;
    --         First  : out Positive;
    --         Last   : out Natural);
    --      pragma Ada_2012 (Find_Token);
    --
    --      procedure Find_Token
    --        (Source : Unbounded_String;
    --         Set    : Maps.Character_Set;
    --         Test   : Membership;
    --         First  : out Positive;
    --         Last   : out Natural);

    ------------------------------------
    -- String Translation Subprograms --
    ------------------------------------

    --      function Translate
    --        (Source  : Unbounded_String;
    --         Mapping : Maps.Character_Mapping) return Unbounded_String;
    --
    --      procedure Translate
    --        (Source  : in out Unbounded_String;
    --         Mapping : Maps.Character_Mapping);
    --
    --      function Translate
    --        (Source  : Unbounded_String;
    --         Mapping : Maps.Character_Mapping_Function) return Unbounded_String;
    --
    --      procedure Translate
    --        (Source  : in out Unbounded_String;
    --         Mapping : Maps.Character_Mapping_Function);

    ---------------------------------------
    -- String Transformation Subprograms --
    ---------------------------------------

    function Replace_Slice
      (Source : Unbounded_String;
       Low    : Positive;
       High   : Natural;
       By     : String) return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Replace_Slice
              (Source => To_String (Source),
               Low    => Low,
               High   => High,
               By     => By)))
    with Inline;

    procedure Replace_Slice
      (Source : in out Unbounded_String;
       Low    : Positive;
       High   : Natural;
       By     : String) with Inline;

    function Insert
      (Source   : Unbounded_String;
       Before   : Positive;
       New_Item : String)
       return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Insert
              (Source   => To_String (Source),
               Before   => Before,
               New_Item => New_Item)))
    with Inline;

    procedure Insert
      (Source   : in out Unbounded_String;
       Before   : Positive;
       New_Item : String)
      with Inline;

    -- TODO:
    --      function Overwrite
    --        (Source   : Unbounded_String;
    --         Position : Positive;
    --         New_Item : String) return Unbounded_String;
    --
    --      procedure Overwrite
    --        (Source   : in out Unbounded_String;
    --         Position : Positive;
    --         New_Item : String);
    --
    --      function Delete
    --        (Source  : Unbounded_String;
    --         From    : Positive;
    --         Through : Natural) return Unbounded_String;
    --
    --      procedure Delete
    --        (Source  : in out Unbounded_String;
    --         From    : Positive;
    --         Through : Natural);

    function Trim
      (Source : Unbounded_String;
       Side   : Ada.Strings.Trim_End) return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Trim
              (Source => To_String (Source),
               Side   => Side)))
    with Inline;

    procedure Trim
      (Source : in out Unbounded_String;
       Side   : Ada.Strings.Trim_End)
      with Inline;

    function Trim
      (Source : Unbounded_String;
       Left   : Ada.Strings.Maps.Character_Set;
       Right  : Ada.Strings.Maps.Character_Set)
       return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Trim
              (Source => To_String (Source),
               Left   => Left,
               Right  => Right)))
    with Inline;

    procedure Trim
      (Source : in out Unbounded_String;
       Left   : Ada.Strings.Maps.Character_Set;
       Right  : Ada.Strings.Maps.Character_Set)
      with Inline;

    function Head
      (Source : Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space)
       return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Head
              (Source => To_String (Source),
               Count  => Count,
               Pad    => Pad)))
    with Inline;

    procedure Head
      (Source : in out Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space)
      with Inline;

    function Tail
      (Source : Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space)
       return Unbounded_String is
      (To_Unbounded_String
         (Ada.Strings.Fixed.Tail
              (Source => To_String (Source),
               Count  => Count,
               Pad    => Pad)))
    with Inline;

    procedure Tail
      (Source : in out Unbounded_String;
       Count  : Natural;
       Pad    : Character := Ada.Strings.Space)
      with Inline;

    -- TODO:
    --      function "*"
    --        (Left  : Natural;
    --         Right : Character) return Unbounded_String;
    --
    --      function "*"
    --        (Left  : Natural;
    --         Right : String) return Unbounded_String;
    --
    --      function "*"
    --        (Left  : Natural;
    --         Right : Unbounded_String) return Unbounded_String;

    procedure Ensure_Allocation (Object : in out Unbounded_String; Last : Natural) with Inline;

    function Get_Allocated_Length (Source : Unbounded_String) return Natural with Inline;

    -----
    -- Utilities
    -----

    function Join
      (String_Set : String_Sets.Set;
       Separator  : String)
       return Unbounded_String
      with Inline;

    procedure String_Replace
      (S                    : in out Unbounded_String;
       Pattern, Replacement : String)
      with Inline;

    function Hash
      (Key : Unbounded_String) return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (To_String (Key)))
    with Inline;

private

    -- Strings first index is 1, so Last = 0 means 'null/empty string value'

    Default_Growth_Factor : constant Positive := 32;

    type Unbounded_String is new Ada.Finalization.Controlled with record
        Value         : String_Access := null;
        Last          : Integer := 0;
        Growth_Factor : Positive := Default_Growth_Factor;
    end record;

    overriding procedure Initialize (Object : in out Unbounded_String) with Inline;
    overriding procedure Adjust     (Object : in out Unbounded_String) with Inline;
    overriding procedure Finalize   (Object : in out Unbounded_String) with Inline;


    Null_Unbounded_String : constant Unbounded_String
      := (Ada.Finalization.Controlled with
          Value         => null,
          Last          => 0,
          Growth_Factor => Default_Growth_Factor);

end ARColl.Strings.Unbounded;
