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
Q  4   ��
 S E C T I O N - A 1                   unit ADim;

{$H+}{$J-}
{$modeswitch typehelpers} 
{$modeswitch advancedrecords}
{$WARN 5024 OFF} // Suppress warning for unused routine parameter.
{$WARN 5033 OFF} // Suppress warning for unassigned function's return value.
{$MACRO ON}

interface

uses SysUtils;

type
  { Prefix }
  TPrefix = (pQuetta, pRonna, pYotta, pZetta, pExa, pPeta, pTera, pGiga, pMega, pKilo, pHecto, pDeca, 
    pNone, pDeci, pCenti, pMilli, pMicro, pNano, pPico, pFemto, pAtto, pZepto, pYocto, pRonto, pQuecto);

  { Prefixes } 
  TPrefixes = array of TPrefix; 

  { Exponents } 
  TExponents = array of longint;  
   �  4   ��
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

generic function Min<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
generic function Max<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;

{ Useful routines }

function GetSymbol(const ASymbol: string; const Prefixes: TPrefixes): string;
function GetName(const AName: string; const Prefixes: TPrefixes): string;

{ Constants }

const
  AvogadroConstant               : TReciprocalMoles                     = (FValue: 6.02214076E+23);
  BohrMagneton                   : TSquareMeterAmperes                  = (FValue: 9.2740100657E-24);
  BohrRadius                     : TMeters                              = (FValue: 5.29177210903E-11);
  BoltzmannConstant              : TJoulesPerKelvin                     = (FValue: 1.380649E-23);
  ComptonWaveLength              : TMeters                              = (FValue: 2.42631023867E-12);
  CoulombConstant                : TNewtonSquareMetersPerSquareCoulomb  = (FValue: 8.9875517923E+9);
  DeuteronMass                   : TKilograms                           = (FValue: 3.3435837768E-27); 
  ElectricPermittivity           : TFaradsPerMeter                      = (FValue: 8.8541878128E-12); 
  ElectronMass                   : TKilograms                           = (FValue: 9.1093837015E-31); 
  ElectronCharge                 : TCoulombs                            = (FValue: 1.602176634E-19);
  ElementaryCharge               : TCoulombs                            = (FValue: 1.602176634E-19);
  MagneticPermeability           : THenriesPerMeter                     = (FValue: 1.25663706212E-6);
  MolarGasConstant               : TJoulesPerMolePerKelvin              = (FValue: 8.314462618);
  NeutronMass                    : TKilograms                           = (FValue: 1.67492750056E-27); 
  NewtonianConstantOfGravitation : TNewtonSquareMetersPerSquareKilogram = (FValue: 6.67430E-11);
  PlanckConstant                 : TJouleSeconds                        = (FValue: 6.62607015E-34);
  ProtonMass                     : TKilograms                           = (FValue: 1.67262192595E-27);
  RydbergConstant                : TReciprocalMeters                    = (FValue: 10973731.568157);
  SpeedOfLight                   : TMetersPerSecond                     = (FValue: 299792458);  
  SquaredSpeedOfLight            : TSquareMetersPerSquareSecond         = (FValue: 8.98755178736818E+16);  
  StandardAccelerationOfGravity  : TMetersPerSquareSecond               = (FValue: 9.80665);
  ReducedPlanckConstant          : TJouleSeconds                        = (FValue: 6.62607015E-34/2/pi);
  UnifiedAtomicMassUnit          : TKilograms                           = (FValue: 1.66053906892E-27);
  
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

     4   ��
 S E C T I O N - B 1                   
implementation

uses Math;

function GetSymbol(const ASymbol: string; const Prefixes: TPrefixes): string;
var
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  case PrefixCount of
    0:  result := ASymbol;
    1:  result := Format(ASymbol, [
          PrefixTable[Prefixes[0]].Symbol]);
    2:  result := Format(ASymbol, [
          PrefixTable[Prefixes[0]].Symbol,
          PrefixTable[Prefixes[1]].Symbol]);
    3:  result := Format(ASymbol, [
          PrefixTable[Prefixes[0]].Symbol,
          PrefixTable[Prefixes[1]].Symbol,
          PrefixTable[Prefixes[2]].Symbol]);
  else raise Exception.Create('Wrong number of prefixes.');
  end;
end;

function GetName(const AName: string; const Prefixes: TPrefixes): string;
var
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  case PrefixCount of
    0:  result := AName;
    1:  result := Format(AName, [
          PrefixTable[Prefixes[0]].Name]);
    2:  result := Format(AName, [
          PrefixTable[Prefixes[0]].Name,
          PrefixTable[Prefixes[1]].Name]);
    3:  result := Format(AName, [
          PrefixTable[Prefixes[0]].Name,
          PrefixTable[Prefixes[1]].Name,
          PrefixTable[Prefixes[2]].Name]);
   else raise Exception.Create('Wrong number of prefixes.');
   end;
end;
"  4   ��
 S E C T I O N - B 4                   { Trigonometric functions } 

function Cos(const AQuantity: TRadianQty): double;
begin 
  result := System.Cos(AQuantity.FValue); 
end;

function Sin(const AQuantity: TRadianQty): double;
begin 
  result := System.Sin(AQuantity.FValue);
end;

function Tan(const AQuantity: TRadianQty): double;
begin 
  result := Math.Tan(AQuantity.FValue);
end;

function Cotan(const AQuantity: TRadianQty): double;
begin 
  result := Math.Cotan(AQuantity.FValue);
end;

function Secant(const AQuantity: TRadianQty): double;
begin 
  result := Math.Secant(AQuantity.FValue);
end;

function Cosecant(const AQuantity: TRadianQty): double;
begin 
  result := Math.Cosecant(AQuantity.FValue);
end;

function ArcCos(const AValue: double): TRadianQty;
begin 
  result.FValue := Math.ArcCos(AValue);
end;

function ArcSin(const AValue: double): TRadianQty;
begin 
  result.FValue := Math.ArcSin(AValue);
end;

function ArcTan(const AValue: double): TRadianQty;
begin 
  result.FValue := System.ArcTan(AValue);
end;

function ArcTan2(const x, y: double): TRadianQty;
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

generic function Min<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
begin
  if AValue1 < AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

generic function Max<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
begin
  if AValue1 > AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

end.
  �      �� ��               (       @             d   d                                                                                                                                                                                                                                                                                           Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu���������������������������������������������������������������������������������޻��ը|�                                    Ӥu�����������������������������������������������������������������������������޼��֨{�ےm                                    Ӥu�������������������������������������������������������������������������޼��֨{�ےm                                        Ӥu���������������������������������������������������������������������޼��֨{�ےm                                            Ӥu�����������������������������������������������������������������޽��֨|�ےm                                                Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�֩}�ժ�                                                                                                                                                                                                                                                                                                                                                                                                                                    