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

with Interfaces; use Interfaces;
with Interfaces.C;

package ARColl.Numerics.C is
    pragma Linker_Options ("-lm");

    subtype Double is Interfaces.C.double;

    function Sin (X : Double) return Double;
    pragma Import (C, Sin, "sin");

    function Cos (X : Double) return Double;
    pragma Import (C, Cos, "cos");

    function Tan (X : Double) return Double;
    pragma Import (C, Tan, "tan");

    function Exp (X : Double) return Double;
    pragma Import (C, Exp, "exp");

    function Sqrt (X : Double) return Double;
    pragma Import (C, Sqrt, "sqrt");

    function Sqrtf (X : Float) return Float;
    pragma Import (C, Sqrtf, "sqrtf");

    function Log (X : Double) return Double;
    pragma Import (C, Log, "log");

    function Acos (X : Double) return Double;
    pragma Import (C, Acos, "acos");

    function Asin (X : Double) return Double;
    pragma Import (C, Asin, "asin");

    function Atan (X : Double) return Double;
    pragma Import (C, Atan, "atan");

    function Sinh (X : Double) return Double;
    pragma Import (C, Sinh, "sinh");

    function Cosh (X : Double) return Double;
    pragma Import (C, Cosh, "cosh");

    function Tanh (X : Double) return Double;
    pragma Import (C, Tanh, "tanh");

    function Pow (X, Y : Double) return Double;
    pragma Import (C, Pow, "pow");

    ---------
    -- Random
    ---------
    
    procedure SRand (I : Interfaces.C.int); 
    pragma Import (C, SRand, "srand");

    function RRand return Interfaces.C.int;
    pragma Import (C, RRand, "rand");
    
    function Rand return Interfaces.C.int with Inline;
    pragma Import (C, Rand, "rand");
    
    procedure SRand48 (I : Interfaces.C.int) with Inline;
    pragma Import (C, SRand48, "srand48");

    function DRand48 return Interfaces.C.double with Inline;
    pragma Import (C, DRand48, "drand48");
    
    ---
    
    procedure Initialize_SRand
      (Seed : in Integer := 1);
    
end ARColl.Numerics.C;
