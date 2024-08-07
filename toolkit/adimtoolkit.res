        ��  ��                  �      �� ��               <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
 <assemblyIdentity version="1.0.0.0" processorArchitecture="*" name="CompanyName.ProductName.AppName" type="win32"/>
 <description>Your application description.</description>
 <dependency>
  <dependentAssembly>
   <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*"/>
  </dependentAssembly>
 </dependency>
 <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
  <security>
   <requestedPrivileges>
    <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
   </requestedPrivileges>
  </security>
 </trustInfo>
 <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">
  <application>
   <!-- Windows Vista -->
   <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}" />
   <!-- Windows 7 -->
   <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}" />
   <!-- Windows 8 -->
   <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}" />
   <!-- Windows 8.1 -->
   <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}" />
   <!-- Windows 10 -->
   <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}" />
   </application>
  </compatibility>
 <asmv3:application xmlns:asmv3="urn:schemas-microsoft-com:asm.v3">
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">
   <dpiAware>True</dpiAware>
  </asmv3:windowsSettings>
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">
   
   <longPathAware>false</longPathAware>
   
  </asmv3:windowsSettings>
 </asmv3:application>
</assembly>   0   �� M A I N I C O N                              �   �  4   ��
 S E C T I O N - A 0                   {
  Description: ADimPas library.

  Copyright (C) 2023-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>
    
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
�	  4   ��
 S E C T I O N - A 1                   unit ADim;

{$H+}{$J-}
{$modeSwitch advancedRecords}
{$WARN 05024 OFF} // Suppress warning for unused routine parameter.
{$WARN 05033 OFF} // Suppress warning for unassigned function's return value.

interface

uses SysUtils, Types;

type 
  { Prefix }
  TPrefix = (pQuetta, pRonna, pYotta, pZetta, pExa, pPeta, pTera, pGiga, pMega, pKilo, pHecto, pDeca, 
    pNone, pDeci, pCenti, pMilli, pMicro, pNano, pPico, pFemto, pAtto, pZepto, pYocto, pRonto, pQuecto);
 
type 
  { Prefixes } 
  TPrefixes = array of TPrefix;    

  { TQuantity }
  generic TQuantity<U> = record
    type TSelf = specialize TQuantity<U>;    
  private
    FValue: double;
  public
    function Abs: TSelf;
    function Value: double;
    function Value(const APrefixes: TPrefixes): double;    
    function ToString: string;
    function ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString: string;
    function ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function IsSame(const AValue: TSelf): boolean;
    function IsZero: boolean;    
    class operator +  (const AValue: TSelf): TSelf;
    class operator -  (const AValue: TSelf): TSelf;
    class operator +  (const ALeft, ARight: TSelf): TSelf;
    class operator -  (const ALeft, ARight: TSelf): TSelf;
    class operator *  (const AValue: double; const ASelf: TSelf): TSelf;
    class operator *  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ALeft, ARight: TSelf): double;
    class operator mod(const ALeft, ARight: TSelf): TSelf;    
    class operator =  (const ALeft, ARight: TSelf): boolean;
    class operator <  (const ALeft, ARight: TSelf): boolean;
    class operator >  (const ALeft, ARight: TSelf): boolean;
    class operator <= (const ALeft, ARight: TSelf): boolean;
    class operator >= (const ALeft, ARight: TSelf): boolean;
  end;

  { TUnitId }
  generic TUnitId<U> = record
    type TSelf = specialize TUnitId<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}ASelf: TSelf): TBaseQuantity;
  end;
   �  4   ��
 S E C T I O N - A 4                   { Trigonometric functions } 

function Cos(const AQuantity: TRadians): double;
function Sin(const AQuantity: TRadians): double;
function Tan(const AQuantity: TRadians): double;
function Cotan(const AQuantity: TRadians): double;
function Secant(const AQuantity: TRadians): double;
function Cosecant(const AQuantity: TRadians): double;

function ArcCos(const AValue: double): TRadians;
function ArcSin(const AValue: double): TRadians;
function ArcTan(const AValue: double): TRadians;
function ArcTan2(const x, y: double): TRadians;

{ Override trigonometric functions }

function Cos(const AQuantity: double): double;
function Sin(const AQuantity: double): double;
function Tan(const AQuantity: double): double;
function Cotan(const AQuantity: double): double;
function Secant(const AQuantity: double): double;
function Cosecant(const AQuantity: double): double;

{ Math functions }

generic function Min<T>(const AValue1, AValue2: T): T;
generic function Max<T>(const AValue1, AValue2: T): T;

{ Constants }

const
  BohrRadius                     : TMeters                              = (FValue: 5.29177210903E-11);
  ElectronMass                   : TKilograms                           = (FValue: 9.1093837015E-31); 
  ElementaryCharge               : TCoulombs                            = (FValue: 1.602176634E-19);
  ComptonWaveLength              : TMeters                              = (FValue: 2.42631023867E-12);
  CoulombConstant                : TNewtonSquareMetersPerSquareCoulomb  = (FValue: 8.9875517923E9);
  NewtonianConstantOfGravitation : TNewtonSquareMetersPerSquareKilogram = (FValue: 6.67430E-11);
  PlanckConstant                 : TJouleSeconds                        = (FValue: 6.62607015E-34);
  StandardAccelerationOfGravity  : TMetersPerSquareSecond               = (FValue: 9.80665);
  ReducedPlanckConstant          : TJouleSeconds                        = (FValue: 6.62607015E-34/2/pi);  
  VacuumElectricPermittivity     : TFaradsPerMeter                      = (FValue: 8.8541878128E-12);
  VacuumLightSpeed               : TMetersPerSecond                     = (FValue: 299792458);  
  VacuumMagneticPermeability     : THenriesPerMeter                     = (FValue: 1.25663706212E-6);

 �&  4   ��
 S E C T I O N - B 1                   
const
  PrefixTable: array[pQuetta..pQuecto] of 
    record  Symbol, Name: string; Exponent: longint end = (
    (Symbol: 'Q';   Name: 'quetta';  Exponent: +30),
    (Symbol: 'R';   Name: 'ronna';   Exponent: +27),
    (Symbol: 'Y';   Name: 'yotta';   Exponent: +24),
    (Symbol: 'Z';   Name: 'zetta';   Exponent: +21),
    (Symbol: 'E';   Name: 'exa';     Exponent: +18),  
    (Symbol: 'P';   Name: 'peta';    Exponent: +15),
    (Symbol: 'T';   Name: 'tera';    Exponent: +12),
    (Symbol: 'G';   Name: 'giga';    Exponent: +09),
    (Symbol: 'M';   Name: 'mega';    Exponent: +06),
    (Symbol: 'k';   Name: 'kilo';    Exponent: +03),
    (Symbol: 'h';   Name: 'hecto';   Exponent: +02),
    (Symbol: 'da';  Name: 'deca';    Exponent: +01),
    (Symbol: '';    Name: '';        Exponent:  00),
    (Symbol: 'd';   Name: 'deci';    Exponent: -01),
    (Symbol: 'c';   Name: 'centi';   Exponent: -02),
    (Symbol: 'm';   Name: 'milli';   Exponent: -03),
    (Symbol: 'μ';   Name: 'micro';   Exponent: -06),
    (Symbol: 'n';   Name: 'nano';    Exponent: -09),
    (Symbol: 'p';   Name: 'pico';    Exponent: -12),
    (Symbol: 'f';   Name: 'femto';   Exponent: -15),
    (Symbol: 'a';   Name: 'atto';    Exponent: -18),
    (Symbol: 'z';   Name: 'zepto';   Exponent: -21),
    (Symbol: 'y';   Name: 'yocto';   Exponent: -24),
    (Symbol: 'r';   Name: 'ronto';   Exponent: -27),
    (Symbol: 'q';   Name: 'quecto';  Exponent: -30)
  );

function GetSymbol(const Symbol: string; const Prefixes: TPrefixes): string;
function GetSingularName(const SingularName: string; const Prefixes: TPrefixes): string;
function GetPluralName(const PluralName: string; const Prefixes: TPrefixes): string;

implementation

uses Math;

function GetSymbol(const Symbol: string; const Prefixes: TPrefixes): string;
var
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  case PrefixCount of
    0:  result := Symbol; 
    1:  result := Format(Symbol, [
          PrefixTable[Prefixes[0]].Symbol]);
    2:  result := Format(Symbol, [
          PrefixTable[Prefixes[0]].Symbol, 
          PrefixTable[Prefixes[1]].Symbol]);
    3:  result := Format(Symbol, [
          PrefixTable[Prefixes[0]].Symbol, 
          PrefixTable[Prefixes[1]].Symbol, 
          PrefixTable[Prefixes[2]].Symbol]);
  else raise Exception.Create('Wrong number of prefixes.');
  end;
end;

function GetSingularName(const SingularName: string; const Prefixes: TPrefixes): string;
var
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  case PrefixCount of
    0:  result := SingularName;
    1:  result := Format(SingularName, [
          PrefixTable[Prefixes[0]].Name]);
    2:  result := Format(SingularName, [
          PrefixTable[Prefixes[0]].Name, 
          PrefixTable[Prefixes[1]].Name]);
    3:  result := Format(SingularName, [
          PrefixTable[Prefixes[0]].Name, 
          PrefixTable[Prefixes[1]].Name, 
          PrefixTable[Prefixes[2]].Name]);
   else raise Exception.Create('Wrong number of prefixes.');
   end;
end;

function GetPluralName(const PluralName: string; const Prefixes: TPrefixes): string;
var
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  case PrefixCount of
    0:  result := PluralName;
    1:  result := Format(PluralName, [
          PrefixTable[Prefixes[0]].Name]);
    2:  result := Format(PluralName, [
          PrefixTable[Prefixes[0]].Name, 
          PrefixTable[Prefixes[1]].Name]);
    3:  result := Format(PluralName, [
          PrefixTable[Prefixes[0]].Name, 
          PrefixTable[Prefixes[1]].Name, 
          PrefixTable[Prefixes[2]].Name]); 
  else raise Exception.Create('Wrong number of prefixes.');
  end;
end;

{ TQuantity }

function TQuantity.Abs: TSelf;
begin
  result.FValue := System.Abs(FValue);
end;

function TQuantity.Value: double;
begin
  result := FValue;
end;

function TQuantity.Value(const APrefixes: TPrefixes): double;
var
  I: longint;
  Exponent: longint;
  PrefixCount: longint;
begin
  PrefixCount := Length(APrefixes);
  if PrefixCount = Length(U.Prefixes) then
  begin
    Exponent := 0;
    for I := 0 to PrefixCount -1 do
      Inc(Exponent, PrefixTable[U.Prefixes[I]].Exponent * U.PrefixExponents[I]);

    for I := 0 to PrefixCount -1 do
      Dec(Exponent, PrefixTable[APrefixes[I]].Exponent * U.PrefixExponents[I]);
    
    if Exponent <> 0 then  
      result := FValue * IntPower(10, Exponent)
    else  
      result := FValue;
  end else   
    if PrefixCount = 0 then 
      result := FValue
    else  
      raise Exception.Create('Wrong number of prefixes.');   
end;     

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue) + ' ' + GetSymbol(U.Symbol, U.Prefixes);
end;

function TQuantity.ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);

  if Length(APrefixes) = 0 then
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(U.Symbol, U.Prefixes)
  else
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(U.Symbol, APrefixes) 
end;

function TQuantity.ToString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var  
  FactoredTol: double; 
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);
  FactoredTol   := ATolerance.Value(APrefixes);

  if Length(APrefixes) = 0 then
  begin
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(FactoredTol,   ffGeneral, APrecision, ADigits) + ' '   + GetSymbol(U.Symbol, U.Prefixes)
  end else
  begin
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(FactoredTol,   ffGeneral, APrecision, ADigits) + ' '   + GetSymbol(U.Symbol, APrefixes);
  end;                                   
end;

function TQuantity.ToVerboseString: string;
begin
  if (FValue < -1) or (FValue > 1) then
    result := FloatToStr(FValue) + ' ' + GetPluralName(U.PluralName, U.Prefixes) 
  else
    result := FloatToStr(FValue) + ' ' + GetSingularName(U.SingularName, U.Prefixes);
end;

function TQuantity.ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);

  if (FactoredValue < -1) or (FactoredValue > 1) then
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetPluralName(U.PluralName, U.Prefixes)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetPluralName(U.PluralName, APrefixes); 
  end else
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSingularName(U.SingularName, U.Prefixes)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSingularName(U.SingularName, APrefixes)
  end; 
end;     

function TQuantity.ToVerboseString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var  
  FactoredTol: double; 
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);
  FactoredTol   := ATolerance.Value(APrefixes);

  if Length(APrefixes) = 0 then
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(FactoredTol,   ffGeneral, APrecision, ADigits) + ' '   + GetPluralName(U.PluralName, U.Prefixes)
  else
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(FactoredTol,   ffGeneral, APrecision, ADigits) + ' '   + GetPluralName(U.PluralName, APrefixes);
end;

function TQuantity.IsSame(const AValue: TSelf): boolean;
begin  
  result := SameValue(FValue, AValue.FValue);
end;

function TQuantity.IsZero: boolean;
begin
  result := System.Abs(FValue) <= 1E-12;
end; 

class operator TQuantity.+(const AValue: TSelf): TSelf;
begin
  result.FValue := AValue.FValue;
end;

class operator TQuantity.-(const AValue: TSelf): TSelf;
begin
  result.FValue := -AValue.FValue;
end;

class operator TQuantity.+(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue + ARight.FValue;
end;

class operator TQuantity.-(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue - ARight.FValue;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TSelf): TSelf;
begin
  result.FValue := AValue * ASelf.FValue;
end;

class operator TQuantity.*(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.FValue := ASelf.FValue * AValue;
end;

class operator TQuantity./(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.FValue := ASelf.FValue / AValue;
end;

class operator TQuantity./(const ALeft, ARight: TSelf): double;
begin
  result := ALeft.FValue / ARight.FValue;
end;

class operator TQuantity.mod(const ALeft, ARight: TSelf): TSelf;
begin
  result.FValue := ALeft.FValue mod ARight.FValue;
end;

class operator TQuantity.=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue = ARight.FValue;
end;

class operator TQuantity.<(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue < ARight.FValue;
end;

class operator TQuantity.>(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue > ARight.FValue;
end;

class operator TQuantity.<=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue <= ARight.FValue;
end;

class operator TQuantity.>=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.FValue >= ARight.FValue;
end;

{ TUnitId }

class function TUnitId.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.FValue := AQuantity.FValue;
end;

class operator TUnitId.*(const AValue: double; const ASelf: TSelf): TBaseQuantity;
begin
  result.FValue := AValue;
end;

{ External operators }

 �  4   ��
 S E C T I O N - B 4                   { Trigonometric functions } 

function Cos(const AQuantity: TRadians): double;
begin 
  result := System.Cos(AQuantity.FValue); 
end;

function Sin(const AQuantity: TRadians): double;
begin 
  result := System.Sin(AQuantity.FValue);
end;

function Tan(const AQuantity: TRadians): double;
begin 
  result := Math.Tan(AQuantity.FValue);
end;

function Cotan(const AQuantity: TRadians): double;
begin 
  result := Math.Cotan(AQuantity.FValue);
end;

function Secant(const AQuantity: TRadians): double;
begin 
  result := Math.Secant(AQuantity.FValue);
end;

function Cosecant(const AQuantity: TRadians): double;
begin
  result := Math.Cosecant(AQuantity.FValue);
end;

function ArcCos(const AValue: double): TRadians;
begin
  result.FValue := Math.ArcCos(AValue);
end;

function ArcSin(const AValue: double): TRadians;
begin
  result.FValue := Math.ArcSin(AValue);
end;

function ArcTan(const AValue: double): TRadians;
begin 
  result.FValue := System.ArcTan(AValue);
end;

function ArcTan2(const x, y: double): TRadians;
begin 
  result.FValue := Math.ArcTan2(x, y);
end;

{ Override trigonometric functions }

function Cos(const AQuantity: double): double;
begin 
  result := System.Cos(AQuantity); 
end;

function Sin(const AQuantity: double): double;
begin 
  result := System.Sin(AQuantity);
end;

function Tan(const AQuantity: double): double;
begin 
  result := Math.Tan(AQuantity);
end;

function Cotan(const AQuantity: double): double;
begin 
  result := Math.Cotan(AQuantity);
end;

function Secant(const AQuantity: double): double;
begin 
  result := Math.Secant(AQuantity);
end;

function Cosecant(const AQuantity: double): double;
begin 
  result := Math.Cosecant(AQuantity);
end;

{ Math functions }

generic function Min<T>(const AValue1, AValue2: T): T;
begin
  if AValue1 < AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

generic function Max<T>(const AValue1, AValue2: T): T;
begin
  if AValue1 > AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

 �      �� ��               (       @             d   d                                                                                                                                                                                                                                                                                           Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu���������������������������������������������������������������������������������޻��ը|�                                    Ӥu�����������������������������������������������������������������������������޼��֨{�ےm                                    Ӥu�������������������������������������������������������������������������޼��֨{�ےm                                        Ӥu���������������������������������������������������������������������޼��֨{�ےm                                            Ӥu�����������������������������������������������������������������޽��֨|�ےm                                                Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�֩}�ժ�                                                                                                                                                                                                                                                                                                                                                                                                                                    