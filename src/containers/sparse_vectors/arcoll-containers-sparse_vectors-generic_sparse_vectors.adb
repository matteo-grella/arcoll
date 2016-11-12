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

package body ARColl.Containers.Sparse_Vectors.Generic_Sparse_Vectors is

    overriding function "="
      (Left, Right : Sparse_Vector_Type)
       return Boolean is
    begin

        if Left.Length = 0 and Right.Length = 0 then
            return True;

        elsif not Left.Is_Sparse and not Right.Is_Sparse then
            return Left.Arr.all = Right.Arr.all;

        elsif Left.Is_Sparse and Right.Is_Sparse then

            if Left.Length /= Right.Length then
                return False;
            else

                declare
                    Left_Item : Linked_List_Item_Access_Type
                      := Left.Linked_List;

                    Right_Item : Linked_List_Item_Access_Type := null;
                    Found      : Boolean;
                begin
                    while Left_Item /= null loop

                        Found := False;
                        Right_Item := Right.Linked_List;
                        while Right_Item /= null loop

                            if Right_Item.Index = Left_Item.Index then

                                if Right_Item.Element = Left_Item.Element then
                                    Found := True;
                                    exit;
                                else
                                    return False;
                                end if;

                            end if;

                            Right_Item := Right_Item.Next;
                        end loop;

                        if not Found then
                            return False;
                        end if;

                        Left_Item := Left_Item.Next;
                    end loop;

                    return True;
                end;
            end if;

        else -- (L non-sparse & R sparse) || (L sparse & R non-sparse)
            return False;

        end if;

    end "=";

    overriding procedure Initialize
      (Object : in out Sparse_Vector_Type) is
    begin
        Object.Is_Sparse       := True;

        Object.Internal_Length := 0;
        Object.First_Index     := Index_Type'First;
        Object.Last_Index      := Index_Type'First - 1;

        Object.Linked_List     := null;
        Object.Arr             := null;
    end Initialize;

    overriding procedure Adjust
      (Object : in out Sparse_Vector_Type) is
    begin

        if Object.Linked_List /= null then
            declare
                New_Linked_List : Linked_List_Item_Access_Type := null;

                New_Item : Linked_List_Item_Access_Type := null;
                Item     : Linked_List_Item_Access_Type := null;
            begin
                New_Linked_List := new Linked_List_Item_Type;

                New_Item := New_Linked_List;
                Item := Object.Linked_List;

                loop

                    New_Item.Index := Item.Index;
                    New_Item.Element := Item.Element;

                    if Item.Next = null then
                        New_Item.Next := null;
                        exit;
                    else
                        New_Item.Next := new Linked_List_Item_Type;

                        Item := Item.Next;
                        New_Item := New_Item.Next;
                    end if;

                end loop;

                Object.Linked_List := New_Linked_List;
            end;
        end if;

        if Object.Arr /= null then
            Object.Arr := new Array_Type'(Object.Arr.all);
        end if;

    end Adjust;

    overriding procedure Finalize
      (Object : in out Sparse_Vector_Type) is
    begin
        Object.Clear;
    end Finalize;

    function Find
      (Vector : Sparse_Vector_Type;
       Index  : Index_Type)
       return Cursor is
    begin
        return Position : Cursor do

            Position.Internal_Has_Element := False;

            if Vector.Is_Sparse then

                declare
                    Item : Linked_List_Item_Access_Type
                      := Vector.Linked_List;
                begin
                    while Item /= null loop

                        if Item.Index = Index then
                            Position.Internal_Has_Element := True;
                            Position.Linked_List_Item := Item;
                            exit;
                        end if;

                        Item := Item.Next;
                    end loop;
                end;

            else
                if Index <= Vector.Last_Index then
                    Position.Internal_Has_Element := True;
                    Position.Index := Index;
                end if;
            end if;

        end return;
    end Find;

    function First
      (Vector : Sparse_Vector_Type)
       return Cursor is
    begin
        return Position : Cursor do

            Position.Internal_Has_Element := False;

            if Vector.Is_Sparse then
                if Vector.Linked_List /= null then
                    Position.Internal_Has_Element := True;
                    Position.Linked_List_Item := Vector.Linked_List;
                end if;

            else
                if Vector.Last_Index >= Vector.First_Index then
                    Position.Internal_Has_Element := True;
                    Position.Index := Vector.First_Index;
                end if;
            end if;

        end return;
    end First;

    procedure Next
      (Vector   : Sparse_Vector_Type;
       Position : in out Cursor) is
    begin

        if Vector.Is_Sparse then
            Position.Linked_List_Item := Position.Linked_List_Item.Next;

            if Position.Linked_List_Item = null then
                Position.Internal_Has_Element := False;
            end if;
        else
            Position.Index := Position.Index + 1;
            if Position.Index > Vector.Last_Index then
                Position.Internal_Has_Element := False;
            end if;
        end if;

    end Next;

    function Has_Element
      (Position : Cursor)
       return Boolean is
    begin
        return Position.Internal_Has_Element;
    end Has_Element;

    function Index
      (Vector   : Sparse_Vector_Type;
       Position : Cursor)
       return Index_Type is
    begin

        if Vector.Is_Sparse then
            return Position.Linked_List_Item.Index;
        else
            return Position.Index;
        end if;

    end Index;

    function Element
      (Vector   : Sparse_Vector_Type;
       Position : Cursor)
       return Element_Type is
    begin

        if Vector.Is_Sparse then
            return Position.Linked_List_Item.Element;
        else
            return Vector.Arr (Position.Index);
        end if;

    end Element;

    procedure Update_Element
      (Vector   : in out Sparse_Vector_Type;
       Position : Cursor;
       Process  : not null access
         procedure (Index : Index_Type; Element : in out Element_Type)) is
    begin

        if Vector.Is_Sparse then
            Process
              (Position.Linked_List_Item.Index,
               Position.Linked_List_Item.Element);
        else
            Process
              (Position.Index,
               Vector.Arr (Position.Index));
        end if;

    end Update_Element;

    procedure Insert
      (Vector  : in out Sparse_Vector_Type;
       Index   : Index_Type;
       Element : Element_Type) is
    begin

        if Vector.Is_Sparse then

            if Vector.Linked_List = null then

                -- First insert

                Vector.Linked_List := new Linked_List_Item_Type'
                  (Index   => Index,
                   Element => Element,
                   Next    => null);

                Vector.Internal_Length := 1;
                Vector.Last_Index := Index;

            else

                -- "Append" to last item

                declare
                    Item : Linked_List_Item_Access_Type
                      := Vector.Linked_List;
                begin

                    -- TODO: prepend (without checks) and avoid loop?
                    loop
                        if Item.Index = Index then
                            raise Sparse_Vectors_Exception with
                              "Index already exists.";
                        end if;

                        exit when Item.Next = null;

                        Item := Item.Next;
                    end loop;

                    Item.Next := new Linked_List_Item_Type'
                      (Index   => Index,
                       Element => Element,
                       Next    => null);

                    Vector.Internal_Length := Vector.Internal_Length + 1;
                    if Index > Vector.Last_Index then
                        Vector.Last_Index := Index;
                    end if;
                end;
            end if;

            -- Sparse to non-sparse conversion

            if Vector.Internal_Length > Conversion_Treshold then

                Vector.Arr
                  := new Array_Type (Vector.First_Index .. Vector.Last_Index);

                declare
                    Item : Linked_List_Item_Access_Type := Vector.Linked_List;
                    Next : Linked_List_Item_Access_Type := null;
                begin
                    while Item /= null loop

                        Vector.Arr (Item.Index) := Item.Element;

                        Next := Item.Next;
                        Unchecked_Deallocation (Item);
                        Item := Next;
                    end loop;
                end;

                Vector.Linked_List := null;
                Vector.Internal_Length := Vector.Arr'Length;

                Vector.Is_Sparse := False;
            end if;

        else -- non-sparse

            if Index > Vector.Last_Index then

                -- Array expansion
                declare
                    New_Arr : constant Array_Access_Type
                      := new Array_Type (Vector.First_Index .. Index);
                begin
                    New_Arr (Vector.Arr'Range) := Vector.Arr.all;
                    Unchecked_Deallocation (Vector.Arr);
                    Vector.Arr := New_Arr;

                    Vector.Internal_Length := Vector.Arr'Length;
                    Vector.Last_Index := Index;
                end;
            end if;

            Vector.Arr (Index) := Element;

        end if;

    end Insert;

    function Length
      (Vector : Sparse_Vector_Type)
       return Natural is
    begin
        return Vector.Internal_Length;
    end Length;

    procedure Clear
      (Vector : in out Sparse_Vector_Type) is
    begin
        Vector.Is_Sparse := True;
        Vector.Internal_Length := 0;

        Vector.First_Index := Index_Type'First;
        Vector.Last_Index := Index_Type'First - 1;

        -- TODO: duplicated in "Insert"
        declare
            Item : Linked_List_Item_Access_Type := Vector.Linked_List;
            Next : Linked_List_Item_Access_Type := null;
        begin
            while Item /= null loop
                Next := Item.Next;
                Unchecked_Deallocation (Item);
                Item := Next;
            end loop;
        end;
        Vector.Linked_List := null;

        Unchecked_Deallocation (Vector.Arr);
    end Clear;

    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : Array_Access_Type) is
    begin

        if Obj = null then
            Boolean'Output (Stream, False);
        else
            Boolean'Output (Stream, True);
            Array_Type'Output (Stream, Obj.all);
        end if;

    end My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out Array_Access_Type) is
    begin

        if Boolean'Input (Stream) then
            Obj := new Array_Type'(Array_Type'Input (Stream));
        else
            Obj := null;
        end if;

    end My_Read;

    procedure My_Write
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : Linked_List_Item_Access_Type) is
    begin

        if Obj = null then
            Boolean'Write (Stream, False);
        else
            Boolean'Write (Stream, True);
            Linked_List_Item_Type'Output (Stream, Obj.all);
        end if;

    end My_Write;

    procedure My_Read
      (Stream : access Ada.Streams.Root_Stream_Type'Class;
       Obj    : out Linked_List_Item_Access_Type) is
    begin

        if Boolean'Input (Stream) then
            Obj := new Linked_List_Item_Type'(Linked_List_Item_Type'Input (Stream));
        else
            Obj := null;
        end if;

    end My_Read;

end ARColl.Containers.Sparse_Vectors.Generic_Sparse_Vectors;

