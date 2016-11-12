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

package body ARColl.Numerics.Reals.References is
    
    procedure Update_Real_References  
      (V_In         : in     Real_Array;
       References   : in out Real_Reference_Array) is
    begin
        for I in V_In'Range loop            
            References (I).Value := V_In (I);            
        end loop;
    end Update_Real_References;
    
    procedure Update_Real_References_Map
      (References             : in     Real_Reference_Array;
       Reference_Errors_Map   : in out Real_Reference_Maps.Map) is
        
        use Real_Reference_Maps;
    begin
        
        for Reference of References loop
            if Reference.Reference_Access /= null then
                
                declare
                    Reference_Address : constant System.Address
                      := Reference.Reference_Access.all'Address;
                
                    Cursor            : constant Real_Reference_Maps.Cursor
                      := Reference_Errors_Map.Find (Reference_Address);
                begin
                
                    if Cursor = Real_Reference_Maps.No_Element then
                        Reference_Errors_Map.Insert 
                          (Key      => Reference_Address,
                           New_Item => 
                             (Value            => Reference.Value, 
                              Reference_Access => Reference.Reference_Access,
                              Class            => Reference.Class));
                    else
                        Reference_Errors_Map (Cursor).Value
                          := Reference_Errors_Map (Cursor).Value + Reference.Value;  
                    end if;
                
                end;
                
            end if;
        end loop;
        
    end Update_Real_References_Map;

end ARColl.Numerics.Reals.References;
