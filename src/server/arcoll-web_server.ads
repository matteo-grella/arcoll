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

with AWS.Config;
with AWS.Status;
with AWS.Response;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

with ARColl.Strings; use ARColl.Strings;
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

generic

    SERVER_NAME    : String;
    Server_Port    : Natural  := 0;
    Max_Connection : Positive := 1;


    -- TODO: charset
    -- TODO: application type

package ARColl.Web_Server is

    procedure Initialize
      (WS_Config     : in out AWS.Config.Object);

    procedure Start
      (WS_Config : in out AWS.Config.Object);

    function Web_Server_Callback
      (Request             : AWS.Status.Data)
       return AWS.Response.Data;

    ----
    -- Exception
    ----

    Server_Internal_Error,
    Server_Bad_Request_Error,
    Server_Not_Found_Error : exception;

private

    type Access_To_Stream is access all Ada.Streams.Stream_Element_Array;

    procedure Free is
      new Ada.Unchecked_Deallocation
        (Ada.Streams.Stream_Element_Array, Access_To_Stream);

    function Get_Body_Content
      (Request : AWS.Status.Data) return String_Access;

    function Build_Response
      (Body_Response : in Unbounded_String) return AWS.Response.Data;

end ARColl.Web_Server;

