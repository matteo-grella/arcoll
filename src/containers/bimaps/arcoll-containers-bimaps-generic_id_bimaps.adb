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

package body ARColl.Containers.Bimaps.Generic_ID_Bimaps is

    procedure Clear
      (Bimap : in out Bimap_Type) is
    begin
        Bimap.ID_To_Element.Clear;
        Bimap.ID_To_Element.Reserve_Capacity (0);

        Bimap.Element_To_ID.Clear;
        Bimap.Element_To_ID.Reserve_Capacity (0);
    end Clear;

    procedure Insert
      (Bimap    : in out Bimap_Type;
       Element  : in     Element_Type;
       Inserted :    out Boolean) is

        use Element_To_ID_Maps;

        Position : constant Cursor
          := Bimap.Element_To_ID.Find (Element);
    begin
        if Has_Element (Position) then
            Inserted := False;
        else
            Bimap.ID_To_Element.Append (Element);

            Bimap.Element_To_ID.Insert
              (Key      => Element,
               New_Item => Bimap.ID_To_Element.Last_Index);

            Inserted := True;
        end if;
    end Insert;

    procedure Insert
      (Bimap    : in out Bimap_Type;
       Element  : in     Element_Type) is

        Inserted : Boolean;
    begin
        Insert
          (Bimap    => Bimap,
           Element  => Element,
           Inserted => Inserted);

        if not Inserted then
            raise Constraint_Error with
              "attempt to insert element already in bimap";
        end if;
    end Insert;

    function Get_Element
      (Bimap : in Bimap_Type;
       ID    : in ID_Type) return Element_Type is
    begin
        return Bimap.ID_To_Element.Element (ID);
    exception
        when Constraint_Error =>
            raise Constraint_Error with "ID not in bimap";
    end Get_Element;

    function Get_ID
      (Bimap   : in Bimap_Type;
       Element : in Element_Type) return ID_Type is
    begin
        return Bimap.Element_To_ID.Element (Element);
    exception
        when Constraint_Error =>
            raise Constraint_Error with "Element not in bimap";
    end Get_ID;

    function Find_ID
      (Bimap   : in Bimap_Type;
       Element : in Element_Type) return Extended_ID is

        use Element_To_ID_Maps;

         Position : constant Cursor
          := Bimap.Element_To_ID.Find (Element);
    begin
        if Has_Element (Position) then
            return Bimap.Element_To_ID(Position);
        else
            return -1;
        end if;
    end Find_ID;

    function Contains_ID
      (Bimap : in Bimap_Type;
       ID    : in ID_Type) return Boolean is
    begin
        return Bimap.ID_To_Element.Last_Index >= ID;
    end Contains_ID;

    function Contains_Element
      (Bimap   : in Bimap_Type;
       Element : in Element_Type) return Boolean is
    begin
        return Bimap.Element_To_ID.Contains (Element);
    end Contains_Element;

    function First_ID (Bimap : Bimap_Type) return ID_Type is
        pragma Unreferenced (Bimap);
    begin
        return ID_Type'First;
    end First_ID;

    function Last_ID (Bimap : Bimap_Type) return Extended_ID is
    begin
        return Bimap.ID_To_Element.Last_Index;
    end Last_ID;

    function Length (Bimap : in Bimap_Type) return Length_Type is
    begin
        return Length_Type (Bimap.ID_To_Element.Length);
    end;

    function Empty_Bimap return Bimap_Type is
        Ret : Bimap_Type;
    begin
        return Ret;
    end Empty_Bimap;

end ARColl.Containers.Bimaps.Generic_ID_Bimaps;
