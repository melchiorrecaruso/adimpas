{
  Description: ADimPas Math library.

  Copyright (C) 2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit ADimMath;

{$mode ObjFPC}{$H+}{$J-}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}
{$WARN 5024 OFF} // Suppress warning for unused routine parameter.
{$WARN 5033 OFF} // Suppress warning for unassigned function's return value.
{$MACRO ON}

interface

uses
  Classes, SysUtils;

type
  TComplex = record
  private
    fre: double;
    fimg: double;
  public
    class operator :=(const AValue: double): TComplex;
    class operator  +(const ALeft, ARight: TComplex): TComplex;
    class operator  +(const ALeft: double; const ARight: TComplex): TComplex;
    class operator  +(const ALeft: TComplex; const ARight: double): TComplex;
    class operator  -(const ALeft, ARight: TComplex): TComplex;
    class operator  -(const ALeft: double; const ARight: TComplex): TComplex;
    class operator  -(const ALeft: TComplex; const ARight: double): TComplex;
  end;

  TImaginaryUnit = record
    class operator *(const AValue: double; ASelf: TImaginaryUnit): TComplex;
  end;

  TVector = record
  private
    fm1: double;
    fm2: double;
    fm3: double;
  public
    function Norm: double;
    function SquareNorm: double;
    class operator  +(const ALeft, ARight: TVector): TVector;
    class operator  -(const ALeft, ARight: TVector): TVector;
    class operator  *(const ALeft, ARight: TVector): double;
    class operator **(const ALeft, ARight: TVector): TVector;
  end;

  TVersor1 = record class operator *(const AValue: double; const AVersor: TVersor1): TVector; end;
  TVersor2 = record class operator *(const AValue: double; const AVersor: TVersor2): TVector; end;
  TVersor3 = record class operator *(const AValue: double; const AVersor: TVersor3): TVector; end;

var
  img: TImaginaryUnit;

  e1: TVersor1;
  e2: TVersor2;
  e3: TVersor3;


implementation

// TComplex

class operator TComplex.:=(const AValue: double): TComplex;
begin
  result.fre  := AValue;
  result.fimg := 0;
end;

class operator TComplex.+(const ALeft, ARight: TComplex): TComplex;
begin
  result.fre  := ALeft.fre  + ARight.fre;
  result.fimg := ALeft.fimg + ARight.fimg;
end;

class operator TComplex.+(const ALeft: double; const ARight: TComplex): TComplex;
begin
  result.fre  := ARight.fre + ALeft;
  result.fimg := ARight.fimg;
end;

class operator TComplex.+(const ALeft: TComplex; const ARight: double): TComplex;
begin
  result.fre  := ALeft.fre + ARight;
  result.fimg := ALeft.fimg;
end;

class operator TComplex.-(const ALeft, ARight: TComplex): TComplex;
begin
  result.fre  := ALeft.fre  - ARight.fre;
  result.fimg := ALeft.fimg - ARight.fimg;
end;

class operator TComplex.-(const ALeft: double; const ARight: TComplex): TComplex;
begin
  result.fre  := -ARight.fre + ALeft;
  result.fimg := -ARight.fimg;
end;

class operator TComplex.-(const ALeft: TComplex; const ARight: double): TComplex;
begin
  result.fre  := ALeft.fre - ARight;
  result.fimg := ALeft.fimg;
end;

// TImaginaryUnit

class operator TImaginaryUnit.*(const AValue: double; ASelf: TImaginaryUnit): TComplex;
begin
  result.fre  := 0;
  result.fimg := AValue;
end;

// TVector

class operator TVector.+(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
end;

class operator TVector.-(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
end;

class operator TVector.*(const ALeft, ARight: TVector): double;
begin
  result := ALeft.fm1 * ARight.fm1 +
            ALeft.fm2 * ARight.fm2 +
            ALeft.fm3 * ARight.fm3;
end;

class operator TVector.**(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARighT.fm2;
  result.fm2 := ALeft.fm3 * ARight.fm1 - ALeft.fm1 * ARighT.fm3;
  result.fm3 := ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARighT.fm1;
end;

function TVector.SquareNorm: double;
begin
  result := fm1*fm1 + fm2*fm2 + fm3*fm3;
end;

function TVector.Norm: double;
begin
  result := sqrt(SquareNorm);
end;

// TVersor

class operator TVersor1.*(const AValue: double; const AVersor: TVersor1): TVector;
begin
  result.fm1 := AValue;
  result.fm2 := 0;
  result.fm3 := 0;
end;

class operator TVersor2.*(const AValue: double; const AVersor: TVersor2): TVector;
begin
  result.fm1 := 0;
  result.fm2 := AValue;
  result.fm3 := 0;
end;

class operator TVersor3.*(const AValue: double; const AVersor: TVersor3): TVector;
begin
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := AValue;
end;

end.

