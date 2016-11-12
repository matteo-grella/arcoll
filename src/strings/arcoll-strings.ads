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

with Text_IO;

with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings; use Ada.Strings;

package ARColl.Strings is
        
    type String_Access is access all String;
    
    procedure Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    :        String_Access);
    for String_Access'Write use Write;

    procedure Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out    String_Access);
    for String_Access'Read  use Read;

    function "=" (L : String; R : String_Access) return Boolean is
      (R /= null and then L = R.all)
    with Inline;

    function "=" (L : String_Access; R : String) return Boolean is
      (L /= null and then L.all = R)
    with Inline;

    function Length (S : String_Access) return Integer is
      (if S = null then -1 else S.all'Length)
    with Inline;

    procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access)
      with Inline;
    
    function Is_Null_Or_Empty (S : String_Access) return Boolean is
      (S = null or else S'Length = 0)
    with Inline;
    -- Returns True when S is null of empty string

    pragma Suppress (Tampering_Check);
    package String_Sets is new
      Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => "=");

    procedure Print_String_Set
      (Set   : String_Sets.Set;
       Title : String := "";
       File  : Text_IO.File_Type := Text_IO.Standard_Output);

    pragma Suppress (Tampering_Check);
    package String_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    pragma Suppress (Tampering_Check);
    package String_Access_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String_Access,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    pragma Suppress (Tampering_Check);
    package String_Access_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => String_Access);

    pragma Suppress (Tampering_Check);
    package String_Vectors is
      new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Index_Type,
         Element_Type => String);

    pragma Suppress (Tampering_Check);
    package Vectors_Of_String_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => String_Vectors.Vector,
         "="          => String_Vectors."=");

    pragma Suppress (Tampering_Check);
    package Ordered_Strings_Sets is new
      Ada.Containers.Indefinite_Ordered_Sets
        (String);
      
    function Is_Null (Acc_Str : String_Access) return Boolean with Inline;

    function String_Equal (String_A_Access : String_Access; String_B_Fixed : String) return Boolean with Inline;
    function String_Equal (String_A_Access : String_Access; String_B_Access : String_Access) return Boolean with Inline;

    function In_Str (Source : String; Pattern : String) return Boolean is
      (Ada.Strings.Fixed.Index (Source => Source, Pattern => Pattern) > 0)
    with Inline;

    function Search_Regexp
      (Pattern   : String;
       Search_In : String)
       return Boolean
      with Inline;

    package String_To_ID_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Index_Type,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");
    -- Maps a String Label to the corresponding Index
    
    function Find_String
      (Source  : String;
       Pattern : String) return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => Source,
          Pattern => Pattern)) with Inline;
       
    function Reverse_Find_String
      (Source  : String;
       Pattern : String) return Natural is
      (Ada.Strings.Fixed.Index
         (Source  => Source,
          Pattern => Pattern,
          Going   => Backward)) with Inline;
    
    function In_String
      (Source  : String;
       Pattern : String) return Boolean is
      (Find_String (Source, Pattern) > 0) with Inline;
    
    function String_To_Lower
      (Source : String) return String is
      (Ada.Strings.Fixed.Translate
         (Source, Ada.Strings.Maps.Constants.Lower_Case_Map)) with Inline;
    -- Change String to Lower Case (TODO: fix with utf8)

    procedure Collect_Strings
      (Output_List : out String_Vectors.Vector;
       Input_List  : in  String_Vectors.Vector;
       Cutoff      : in  Positive := 1) with
      Pre => Output_List.Is_Empty;
    
end ARColl.Strings;
