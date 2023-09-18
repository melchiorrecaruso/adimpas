{
  Description: ADimPas library.

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

{
  ADimPas library built on 17/09/2023.

  Number of base units: 121
  Number of factored units: 63
  Number of operators: 959 (403 external, 556 internal)
}

unit ADim;

{$H+}
{$modeSwitch advancedrecords}
{$WARN 05024 OFF} // Suppress warning for unused routine parameter.
{$WARN 05033 OFF} // Suppress warning for unassigned function's return value.
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

{ Unit of second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSecondUnit}{$i common.inc}
type TSeconds = TSecondQty;

var s: TSecondUnit;

const ds: TSecondQty = (FValue: 1E-01);
const cs: TSecondQty = (FValue: 1E-02);
const ms: TSecondQty = (FValue: 1E-03);
const mis: TSecondQty = (FValue: 1E-06);
const ns: TSecondQty = (FValue: 1E-09);
const ps: TSecondQty = (FValue: 1E-12);

{ Unit of day }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TDayQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TDayUnit}{$i common.inc}
type TDays = TSecondQty;

const day: TSecondQty = (FValue: 86400);

{ Unit of hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=THourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=THourUnit}{$i common.inc}
type THours = TSecondQty;

const hr: TSecondQty = (FValue: 3600);

{ Unit of minute }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMinuteQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMinuteUnit}{$i common.inc}
type TMinutes = TSecondQty;

const minute: TSecondQty = (FValue: 60);

{ Unit of square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareSecondUnit}{$i common.inc}
type TSquareSeconds = TSquareSecondQty;

var s2: TSquareSecondUnit;

const ds2: TSquareSecondQty = (FValue: 1E-02);
const cs2: TSquareSecondQty = (FValue: 1E-04);
const ms2: TSquareSecondQty = (FValue: 1E-06);
const mis2: TSquareSecondQty = (FValue: 1E-12);
const ns2: TSquareSecondQty = (FValue: 1E-18);
const ps2: TSquareSecondQty = (FValue: 1E-24);

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty; inline;

{ Unit of square day }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareDayQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareDayUnit}{$i common.inc}
type TSquareDays = TSquareSecondQty;

const day2: TSquareSecondQty = (FValue: 7464960000);

{ Unit of square hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareHourUnit}{$i common.inc}
type TSquareHours = TSquareSecondQty;

const hr2: TSquareSecondQty = (FValue: 12960000);

{ Unit of square minute }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareMinuteQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMinuteUnit}{$i common.inc}
type TSquareMinutes = TSquareSecondQty;

const minute2: TSquareSecondQty = (FValue: 3600);

{ Unit of meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterUnit}{$i common.inc}
type TMeters = TMeterQty;

var m: TMeterUnit;

const km: TMeterQty = (FValue: 1E+03);
const dm: TMeterQty = (FValue: 1E-01);
const cm: TMeterQty = (FValue: 1E-02);
const mm: TMeterQty = (FValue: 1E-03);
const mim: TMeterQty = (FValue: 1E-06);
const nm: TMeterQty = (FValue: 1E-09);
const pm: TMeterQty = (FValue: 1E-12);

{ Unit of astronomical }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TAstronomicalQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TAstronomicalUnit}{$i common.inc}
type TAstronomical = TMeterQty;

const au: TMeterQty = (FValue: 149597870691);

{ Unit of inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TInchUnit}{$i common.inc}
type TInches = TMeterQty;

const inch: TMeterQty = (FValue: 0.0254);

{ Unit of foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TFootQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TFootUnit}{$i common.inc}
type TFeet = TMeterQty;

const ft: TMeterQty = (FValue: 0.3048);

{ Unit of yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TYardQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TYardUnit}{$i common.inc}
type TYards = TMeterQty;

const yd: TMeterQty = (FValue: 0.9144);

{ Unit of mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMileQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMileUnit}{$i common.inc}
type TMiles = TMeterQty;

const mi: TMeterQty = (FValue: 1609.344);

{ Unit of nautical mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNauticalMileQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNauticalMileUnit}{$i common.inc}
type TNauticalMiles = TMeterQty;

const nmi: TMeterQty = (FValue: 1852);

{ Unit of square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterUnit}{$i common.inc}
type TSquareMeters = TSquareMeterQty;

var m2: TSquareMeterUnit;

const km2: TSquareMeterQty = (FValue: 1E+06);
const dm2: TSquareMeterQty = (FValue: 1E-02);
const cm2: TSquareMeterQty = (FValue: 1E-04);
const mm2: TSquareMeterQty = (FValue: 1E-06);
const mim2: TSquareMeterQty = (FValue: 1E-12);
const nm2: TSquareMeterQty = (FValue: 1E-18);
const pm2: TSquareMeterQty = (FValue: 1E-24);

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterQty; const ARight: TMeterQty): TSquareMeterQty; inline;

{ Unit of square inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareInchUnit}{$i common.inc}
type TSquareInches = TSquareMeterQty;

const inch2: TSquareMeterQty = (FValue: 0.00064516);

{ Unit of square foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareFootQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareFootUnit}{$i common.inc}
type TSquareFeet = TSquareMeterQty;

const ft2: TSquareMeterQty = (FValue: 0.09290304);

{ Unit of square yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareYardQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareYardUnit}{$i common.inc}
type TSquareYards = TSquareMeterQty;

const yd2: TSquareMeterQty = (FValue: 0.83612736);

{ Unit of square mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareMileQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMileUnit}{$i common.inc}
type TSquareMiles = TSquareMeterQty;

const mi2: TSquareMeterQty = (FValue: 2589988.110336);

{ Unit of cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TSquareMeterQty; inline;
class operator /(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicMeterUnit}{$i common.inc}
type TCubicMeters = TCubicMeterQty;

var m3: TCubicMeterUnit;

const km3: TCubicMeterQty = (FValue: 1E+09);
const dm3: TCubicMeterQty = (FValue: 1E-03);
const cm3: TCubicMeterQty = (FValue: 1E-06);
const mm3: TCubicMeterQty = (FValue: 1E-09);
const mim3: TCubicMeterQty = (FValue: 1E-18);
const nm3: TCubicMeterQty = (FValue: 1E-27);
const pm3: TCubicMeterQty = (FValue: 1E-36);

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TCubicMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty; inline;

{ Unit of cubic inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicInchUnit}{$i common.inc}
type TCubicInches = TCubicMeterQty;

const inch3: TCubicMeterQty = (FValue: 0.000016387064);

{ Unit of cubic foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicFootQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicFootUnit}{$i common.inc}
type TCubicFeet = TCubicMeterQty;

const ft3: TCubicMeterQty = (FValue: 0.028316846592);

{ Unit of cubic yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicYardQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicYardUnit}{$i common.inc}
type TCubicYards = TCubicMeterQty;

const yd3: TCubicMeterQty = (FValue: 0.764554857984);

{ Unit of litre }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TLitreQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TLitreUnit}{$i common.inc}
type TLitres = TCubicMeterQty;

const L: TCubicMeterQty = (FValue: 1E-03);

const dL: TCubicMeterQty = (FValue: 1E-03 * 1E-01);
const cL: TCubicMeterQty = (FValue: 1E-03 * 1E-02);
const mL: TCubicMeterQty = (FValue: 1E-03 * 1E-03);

{ Unit of gallon }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TGallonQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TGallonUnit}{$i common.inc}
type TGallons = TCubicMeterQty;

const gal: TCubicMeterQty = (FValue: 0.0037854119678);

{ Unit of quartic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i common.inc}
class operator /(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSquareMeterQty; inline;
class operator /(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TCubicMeterQty; inline;
class operator /(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TQuarticMeterUnit}{$i common.inc}
type TQuarticMeters = TQuarticMeterQty;

var m4: TQuarticMeterUnit;

const km4: TQuarticMeterQty = (FValue: 1E+12);
const dm4: TQuarticMeterQty = (FValue: 1E-04);
const cm4: TQuarticMeterQty = (FValue: 1E-08);
const mm4: TQuarticMeterQty = (FValue: 1E-12);
const mim4: TQuarticMeterQty = (FValue: 1E-24);
const nm4: TQuarticMeterQty = (FValue: 1E-36);
const pm4: TQuarticMeterQty = (FValue: 1E-48);

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TQuarticMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TCubicMeterQty): TQuarticMeterQty; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty; inline;

{ Unit of quintic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i common.inc}
class operator /(const ALeft: TQuinticMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty; inline;
class operator /(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty; inline;
class operator /(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TQuarticMeterQty; inline;
class operator /(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TQuinticMeterUnit}{$i common.inc}
type TQuinticMeters = TQuinticMeterQty;

var m5: TQuinticMeterUnit;

const km5: TQuinticMeterQty = (FValue: 1E+15);
const dm5: TQuinticMeterQty = (FValue: 1E-05);
const cm5: TQuinticMeterQty = (FValue: 1E-10);
const mm5: TQuinticMeterQty = (FValue: 1E-15);
const mim5: TQuinticMeterQty = (FValue: 1E-30);
const nm5: TQuinticMeterQty = (FValue: 1E-45);
const pm5: TQuinticMeterQty = (FValue: 1E-60);

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TQuinticMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TQuarticMeterQty): TQuinticMeterQty; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TQuinticMeterQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TCubicMeterQty): TQuinticMeterQty; inline;

{ Unit of sextic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i common.inc}
class operator /(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty; inline;
class operator /(const ALeft: TSexticMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty; inline;
class operator /(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty; inline;
class operator /(const ALeft: TSexticMeterQty; const ARight: TMeterQty): TQuinticMeterQty; inline;
class operator /(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSexticMeterUnit}{$i common.inc}
type TSexticMeters = TSexticMeterQty;

var m6: TSexticMeterUnit;

const km6: TSexticMeterQty = (FValue: 1E+18);
const dm6: TSexticMeterQty = (FValue: 1E-06);
const cm6: TSexticMeterQty = (FValue: 1E-12);
const mm6: TSexticMeterQty = (FValue: 1E-18);
const mim6: TSexticMeterQty = (FValue: 1E-36);
const nm6: TSexticMeterQty = (FValue: 1E-54);
const pm6: TSexticMeterQty = (FValue: 1E-72);

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TSexticMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TQuinticMeterQty): TSexticMeterQty; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSexticMeterQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticMeterQty): TSexticMeterQty; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty; inline;

{ Unit of kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramUnit}{$i common.inc}
type TKilograms = TKilogramQty;

var kg: TKilogramUnit;

const hg: TKilogramQty = (FValue: 1E-01);
const dag: TKilogramQty = (FValue: 1E-02);
const g: TKilogramQty = (FValue: 1E-03);
const dg: TKilogramQty = (FValue: 1E-04);
const cg: TKilogramQty = (FValue: 1E-05);
const mg: TKilogramQty = (FValue: 1E-06);
const mig: TKilogramQty = (FValue: 1E-09);
const ng: TKilogramQty = (FValue: 1E-12);
const pg: TKilogramQty = (FValue: 1E-15);

{ Unit of tonne }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TTonneQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTonneUnit}{$i common.inc}
type TTonnes = TKilogramQty;

const tonne: TKilogramQty = (FValue: 1E+03);

const gigatonne: TKilogramQty = (FValue: 1E+03 * 1E+09);
const megatonne: TKilogramQty = (FValue: 1E+03 * 1E+06);
const kilotonne: TKilogramQty = (FValue: 1E+03 * 1E+03);

{ Unit of pound }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoundUnit}{$i common.inc}
type TPounds = TKilogramQty;

const lb: TKilogramQty = (FValue: 0.45359237);

{ Unit of ounce }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TOunceQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TOunceUnit}{$i common.inc}
type TOunces = TKilogramQty;

const oz: TKilogramQty = (FValue: 0.028349523125);

{ Unit of stone }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TStoneQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TStoneUnit}{$i common.inc}
type TStones = TKilogramQty;

const st: TKilogramQty = (FValue: 6.35029318);

{ Unit of ton }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TTonQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTonUnit}{$i common.inc}
type TTons = TKilogramQty;

const ton: TKilogramQty = (FValue: 907.18474);

{ Unit of square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i common.inc}
class operator /(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareKilogramUnit}{$i common.inc}
type TSquareKilograms = TSquareKilogramQty;

var kg2: TSquareKilogramUnit;

const hg2: TSquareKilogramQty = (FValue: 1E-02);
const dag2: TSquareKilogramQty = (FValue: 1E-04);
const g2: TSquareKilogramQty = (FValue: 1E-06);
const dg2: TSquareKilogramQty = (FValue: 1E-08);
const cg2: TSquareKilogramQty = (FValue: 1E-10);
const mg2: TSquareKilogramQty = (FValue: 1E-12);
const mig2: TSquareKilogramQty = (FValue: 1E-18);
const ng2: TSquareKilogramQty = (FValue: 1E-24);
const pg2: TSquareKilogramQty = (FValue: 1E-30);

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty; inline;

{ Unit of ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TAmpereUnit}{$i common.inc}
type TAmperes = TAmpereQty;

var A: TAmpereUnit;

const kA: TAmpereQty = (FValue: 1E+03);
const hA: TAmpereQty = (FValue: 1E+02);
const daA: TAmpereQty = (FValue: 1E+01);
const dA: TAmpereQty = (FValue: 1E-01);
const cA: TAmpereQty = (FValue: 1E-02);
const mA: TAmpereQty = (FValue: 1E-03);
const miA: TAmpereQty = (FValue: 1E-06);
const nA: TAmpereQty = (FValue: 1E-09);
const picoA: TAmpereQty = (FValue: 1E-12);

{ Unit of square ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i common.inc}
class operator /(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareAmpereUnit}{$i common.inc}
type TSquareAmperes = TSquareAmpereQty;

var A2: TSquareAmpereUnit;

const kA2: TSquareAmpereQty = (FValue: 1E+06);
const hA2: TSquareAmpereQty = (FValue: 1E+04);
const daA2: TSquareAmpereQty = (FValue: 1E+02);
const dA2: TSquareAmpereQty = (FValue: 1E-02);
const cA2: TSquareAmpereQty = (FValue: 1E-04);
const mA2: TSquareAmpereQty = (FValue: 1E-06);
const miA2: TSquareAmpereQty = (FValue: 1E-12);
const nA2: TSquareAmpereQty = (FValue: 1E-18);
const picoA2: TSquareAmpereQty = (FValue: 1E-24);

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty; inline;

{ Unit of kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKelvinUnit}{$i common.inc}
type TKelvins = TKelvinQty;

var K: TKelvinUnit;

{ Unit of degree celsius }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i common.inc}
type TDegreesCelsius = TKelvinQty;

var degC: TDegreeCelsiusUnit;

{ Unit of degree fahrenheit }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i common.inc}
type TDegreesFahrenheit = TKelvinQty;

var degF: TDegreeFahrenheitUnit;

{ Unit of square kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i common.inc}
class operator /(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareKelvinUnit}{$i common.inc}
type TSquareKelvins = TSquareKelvinQty;

var K2: TSquareKelvinUnit;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty; inline;

{ Unit of cubic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i common.inc}
class operator /(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty; inline;
class operator /(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicKelvinUnit}{$i common.inc}
type TCubicKelvins = TCubicKelvinQty;

var K3: TCubicKelvinUnit;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty; inline;

{ Unit of quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i common.inc}
class operator /(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty; inline;
class operator /(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty; inline;
class operator /(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i common.inc}
type TQuarticKelvins = TQuarticKelvinQty;

var K4: TQuarticKelvinUnit;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty; inline;

{ Unit of mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMoleUnit}{$i common.inc}
type TMoles = TMoleQty;

var mol: TMoleUnit;

const kmol: TMoleQty = (FValue: 1E+03);
const hmol: TMoleQty = (FValue: 1E+02);
const damol: TMoleQty = (FValue: 1E+01);

{ Unit of candela }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCandelaUnit}{$i common.inc}
type TCandelas = TCandelaQty;

var cd: TCandelaUnit;

{ Unit of radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TRadianUnit}{$i common.inc}
type TRadians = TRadianQty;

var rad: TRadianUnit;

{ Unit of degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TDegreeQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TDegreeUnit}{$i common.inc}
type TDegrees = TRadianQty;

const deg: TRadianQty = (FValue: Pi/180);

{ Unit of steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i common.inc}
class operator /(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSteradianUnit}{$i common.inc}
type TSteradians = TSteradianQty;

var sr: TSteradianUnit;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty; inline;

{ Unit of square degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareDegreeQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareDegreeUnit}{$i common.inc}
type TSquareDegrees = TSteradianQty;

const deg2: TSteradianQty = (FValue: Pi*Pi/32400);

{ Unit of hertz }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: THertzQty): TSecondQty; inline;
class operator *(const ALeft: THertzQty; const ARight: TSecondQty): double; inline;
class operator *(const ALeft: TSecondQty; const ARight: THertzQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=THertzUnit}{$i common.inc}
type THertz = THertzQty;

var Hz: THertzUnit;

const THz: THertzQty = (FValue: 1E+12);
const GHz: THertzQty = (FValue: 1E+09);
const MHz: THertzQty = (FValue: 1E+06);
const kHz: THertzQty = (FValue: 1E+03);

// main definition [ Hz ] = [ 1 ] / [ s ]
operator /(const ALeft: double; const ARight: TSecondQty): THertzQty; inline;
operator /(const ALeft: double; const ARight: TSecondUnit): THertzQty; inline;

{ Unit of square hertz }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i common.inc}
class operator /(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty; inline;
class operator /(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty; inline;
class operator *(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty; inline;
class operator /(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty; inline;
class operator *(const ALeft: TSquareHertzQty; const ARight: TSquareSecondQty): double; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareHertzQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareHertzUnit}{$i common.inc}
type TSquareHertz = TSquareHertzQty;

var Hz2: TSquareHertzUnit;

const THz2: TSquareHertzQty = (FValue: 1E+24);
const GHz2: TSquareHertzQty = (FValue: 1E+18);
const MHz2: TSquareHertzQty = (FValue: 1E+12);
const kHz2: TSquareHertzQty = (FValue: 1E+06);

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]
operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty; inline;
operator /(const ALeft: double; const ARight: TSquareSecondUnit): TSquareHertzQty; inline;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]
operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty; inline;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty; inline;

{ Unit of radian per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i common.inc}
class operator /(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty; inline;
class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty; inline;
class operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i common.inc}
type TRadiansPerSecond = TRadianPerSecondQty;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty; inline;
operator /(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty; inline;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]
operator *(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty; inline;
operator *(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty; inline;

operator :=(const AQuantity: TRadianPerSecondQty): THertzQty; inline;
operator :=(const AQuantity: THertzQty): TRadianPerSecondQty; inline;

{ Unit of radian per second squared }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$i common.inc}
class operator /(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareHertzQty): TRadianQty; inline;
class operator /(const ALeft: TRadianPerSecondSquaredQty; const ARight: TRadianQty): TSquareHertzQty; inline;
class operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondSquaredQty): TSquareSecondQty; inline;
class operator *(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareSecondQty): TRadianQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianQty; inline;
class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSecondSquaredQty): TSecondQty; inline;
class operator *(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSecondQty): TRadianPerSecondQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianPerSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TRadianPerSecondSquaredUnit}{$i common.inc}
type TRadiansPerSecondSquared = TSquareHertzQty;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSecondSquaredQty; inline;
operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSecondSquaredQty; inline;

// alternative definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSecondSquaredQty; inline;
operator /(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSecondSquaredQty; inline;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]
operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSecondSquaredQty; inline;
operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSecondSquaredQty; inline;

{ Unit of steradian per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty; inline;
class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty; inline;
class operator /(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty; inline;
class operator *(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i common.inc}
type TSteradiansPerSquareSecond = TSquareHertzQty;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty; inline;
operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondUnit): TSteradianPerSquareSecondQty; inline;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]
operator *(const ALeft: TSteradianQty; const ARight: TSquareHertzQty): TSteradianPerSquareSecondQty; inline;
operator *(const ALeft: TSquareHertzQty; const ARight: TSteradianQty): TSteradianPerSquareSecondQty; inline;

{ Unit of meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty; inline;
class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty; inline;
class operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i common.inc}
type TMetersPerSecond = TMeterPerSecondQty;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty; inline;
operator /(const ALeft: TMeterQty; const ARight: TSecondUnit): TMeterPerSecondQty; inline;

// alternative definition [ m/s ] = [ m ] * [ Hz ]
operator *(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty; inline;
operator *(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty; inline;
operator *(const ALeft: TMeterQty; const ARight: THertzUnit): TMeterPerSecondQty; inline;

{ Unit of meter per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMeterPerHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerHourUnit}{$i common.inc}
type TMetersPerHour = TMeterPerSecondQty;

{ Unit of mile per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMilePerHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMilePerHourUnit}{$i common.inc}
type TMilesPerHour = TMeterPerSecondQty;

{ Unit of nautical mile per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i common.inc}
type TNauticalMilesPerHour = TMeterPerSecondQty;

{ Unit of meter per second squared }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$i common.inc}
class operator /(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareHertzQty; inline;
class operator /(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareHertzQty): TMeterQty; inline;
class operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareSecondQty; inline;
class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareSecondQty): TMeterQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty; inline;
class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondSquaredQty): TSecondQty; inline;
class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSecondQty): TMeterPerSecondQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterPerSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerSecondSquaredUnit}{$i common.inc}
type TMetersPerSecondSquared = TMeterPerSecondSquaredQty;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSecondSquaredQty; inline;
operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSecondSquaredQty; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSecondSquaredQty; inline;
operator /(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSecondSquaredQty; inline;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]
operator *(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSecondSquaredQty; inline;

{ Unit of meter per second per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i common.inc}
type TMetersPerSecondPerSecond = TMeterPerSecondSquaredQty;

{ Unit of meter per hour per second }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i common.inc}
type TMetersPerHourPerSecond = TMeterPerSecondSquaredQty;

{ Unit of square meter per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty; inline;
class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty; inline;
class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty; inline;
class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i common.inc}
type TSquareMetersPerSquareSecond = TSquareMeterPerSquareSecondQty;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty; inline;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty; inline;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterPerSquareSecondQty; inline;

{ Unit of kilogram meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty; inline;
class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i common.inc}
type TKilogramMetersPerSecond = TKilogramMeterPerSecondQty;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty; inline;
operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondUnit): TKilogramMeterPerSecondQty; inline;

{ Unit of newton second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonSecondUnit}{$i common.inc}
type TNewtonSeconds = TKilogramMeterPerSecondQty;

{ Unit of kilogram square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty; inline;
class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i common.inc}
type TKilogramSquareMeters = TKilogramSquareMeterQty;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty; inline;
operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty; inline;

{ Unit of kilogram square meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty; inline;
class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty; inline;
class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty; inline;
class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty; inline;
class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i common.inc}
type TKilogramSquareMetersPerSecond = TKilogramSquareMeterPerSecondQty;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty; inline;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeterQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty; inline;
operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterQty): TKilogramSquareMeterPerSecondQty; inline;

// alternative definition [ kg*m2/s ] = [ kg*m/s ] * [ m ]
operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty; inline;

{ Unit of kilogram per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i common.inc}
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i common.inc}
type TKilogramsPerMeter = TKilogramPerMeterQty;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty; inline;
operator /(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramPerMeterQty; inline;

{ Unit of kilogram per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i common.inc}
type TKilogramsPerSquareMeter = TKilogramPerSquareMeterQty;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty; inline;
operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramPerSquareMeterQty; inline;

{ Unit of kilogram per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty; inline;
class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty; inline;
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty; inline;
class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty; inline;
class operator *(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i common.inc}
type TKilogramsPerCubicMeter = TKilogramPerCubicMeterQty;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty; inline;
operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterUnit): TKilogramPerCubicMeterQty; inline;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]
operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty; inline;

{ Unit of newton }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i common.inc}
class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty; inline;
class operator *(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondSquaredQty): TKilogramQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSecondSquaredQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonUnit}{$i common.inc}
type TNewtons = TNewtonQty;

var N: TNewtonUnit;

const GN: TNewtonQty = (FValue: 1E+09);
const MN: TNewtonQty = (FValue: 1E+06);
const kN: TNewtonQty = (FValue: 1E+03);
const hN: TNewtonQty = (FValue: 1E+02);
const daN: TNewtonQty = (FValue: 1E+01);

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredQty): TNewtonQty; inline;
operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramQty): TNewtonQty; inline;
operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredUnit): TNewtonQty; inline;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]
operator *(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty; inline;
operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty; inline;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]
operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty; inline;

{ Unit of pound force }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForceQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoundForceUnit}{$i common.inc}
type TPoundsForce = TNewtonQty;

const lbf: TNewtonQty = (FValue: 4.4482216152605);

{ Unit of square newton }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i common.inc}
class operator /(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareNewtonUnit}{$i common.inc}
type TSquareNewtons = TSquareNewtonQty;

var N2: TSquareNewtonUnit;

const GN2: TSquareNewtonQty = (FValue: 1E+18);
const MN2: TSquareNewtonQty = (FValue: 1E+12);
const kN2: TSquareNewtonQty = (FValue: 1E+06);
const hN2: TSquareNewtonQty = (FValue: 1E+04);
const daN2: TSquareNewtonQty = (FValue: 1E+02);

// main definition [ N2 ] = [ N ] * [ N ]
operator *(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty; inline;

{ Unit of pascal }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i common.inc}
class operator /(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty; inline;
class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty; inline;
class operator *(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPascalUnit}{$i common.inc}
type TPascals = TPascalQty;

var Pa: TPascalUnit;

const TPa: TPascalQty = (FValue: 1E+12);
const GPa: TPascalQty = (FValue: 1E+09);
const MPa: TPascalQty = (FValue: 1E+06);
const kPa: TPascalQty = (FValue: 1E+03);

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty; inline;
operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty; inline;

{ Unit of bar }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TBarQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TBarUnit}{$i common.inc}
type TBars = TPascalQty;

const bar: TPascalQty = (FValue: 1E+05);

const kbar: TPascalQty = (FValue: 1E+05 * 1E+03);
const mbar: TPascalQty = (FValue: 1E+05 * 1E-03);

{ Unit of pound per square inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i common.inc}
type TPoundsPerSquareInch = TPascalQty;

const psi: TPascalQty = (FValue: 6894.75729316836);

const kpsi: TPascalQty = (FValue: 6894.75729316836 * 1E+03);

{ Unit of joule per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i common.inc}
type TJoulesPerCubicMeter = TPascalQty;

{ Unit of joule }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty; inline;
class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty; inline;
class operator *(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty; inline;
class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty; inline;
class operator *(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJouleUnit}{$i common.inc}
type TJoules = TJouleQty;

var J: TJouleUnit;

const TJ: TJouleQty = (FValue: 1E+12);
const GJ: TJouleQty = (FValue: 1E+09);
const MJ: TJouleQty = (FValue: 1E+06);
const kJ: TJouleQty = (FValue: 1E+03);

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty; inline;
operator *(const ALeft: TNewtonQty; const ARight: TMeterUnit): TJouleQty; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty; inline;
operator *(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty; inline;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]
operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty; inline;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]
operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty; inline;
operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TKilogramUnit): TSquareMeterPerSquareSecondQty; inline;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]
operator *(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty; inline;
operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty; inline;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty; inline;

// alternative definition [ J ] = [ kg*m2/s ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty; inline;
operator *(const ALeft: TJouleQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty; inline;

// alternative definition [ J ] = [ kg*m2/s ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty; inline;
operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty; inline;

{ Unit of watt hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TWattHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattHourUnit}{$i common.inc}
type TWattHours = TJouleQty;

{ Unit of elettronvolt }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TElettronvoltQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TElettronvoltUnit}{$i common.inc}
type TElettronvolts = TJouleQty;

const eV: TJouleQty = (FValue: 1.60217742320523E-019);

const TeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+12);
const GeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+09);
const MeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+06);
const keV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+03);

{ Unit of newton meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonMeterUnit}{$i common.inc}
type TNewtonMeters = TJouleQty;

{ Unit of pound force inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForceInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoundForceInchUnit}{$i common.inc}
type TPoundForceInches = TJouleQty;

{ Unit of watt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty; inline;
class operator *(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattUnit}{$i common.inc}
type TWatts = TWattQty;

var W: TWattUnit;

const TW: TWattQty = (FValue: 1E+12);
const GW: TWattQty = (FValue: 1E+09);
const MW: TWattQty = (FValue: 1E+06);
const kW: TWattQty = (FValue: 1E+03);
const milliW: TWattQty = (FValue: 1E-03);

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty; inline;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJouleQty; const ARight: THertzQty): TWattQty; inline;
operator *(const ALeft: THertzQty; const ARight: TJouleQty): TWattQty; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty; inline;

{ Unit of coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i common.inc}
class operator /(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombUnit}{$i common.inc}
type TCoulombs = TCoulombQty;

var C: TCoulombUnit;

const kC: TCoulombQty = (FValue: 1E+03);
const hC: TCoulombQty = (FValue: 1E+02);
const daC: TCoulombQty = (FValue: 1E+01);
const dC: TCoulombQty = (FValue: 1E-01);
const cC: TCoulombQty = (FValue: 1E-02);
const mC: TCoulombQty = (FValue: 1E-03);
const miC: TCoulombQty = (FValue: 1E-06);
const nC: TCoulombQty = (FValue: 1E-09);
const pC: TCoulombQty = (FValue: 1E-12);

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty; inline;
operator *(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty; inline;
operator *(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty; inline;

{ Unit of ampere hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TAmpereHourQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TAmpereHourUnit}{$i common.inc}
type TAmpereHours = TCoulombQty;

{ Unit of square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i common.inc}
class operator /(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareCoulombUnit}{$i common.inc}
type TSquareCoulombs = TSquareCoulombQty;

var C2: TSquareCoulombUnit;

const kC2: TSquareCoulombQty = (FValue: 1E+06);
const hC2: TSquareCoulombQty = (FValue: 1E+04);
const daC2: TSquareCoulombQty = (FValue: 1E+02);
const dC2: TSquareCoulombQty = (FValue: 1E-02);
const cC2: TSquareCoulombQty = (FValue: 1E-04);
const mC2: TSquareCoulombQty = (FValue: 1E-06);
const miC2: TSquareCoulombQty = (FValue: 1E-12);
const nC2: TSquareCoulombQty = (FValue: 1E-18);
const pC2: TSquareCoulombQty = (FValue: 1E-24);

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty; inline;

{ Unit of volt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty; inline;
class operator *(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty; inline;
class operator *(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty; inline;
class operator *(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TVoltUnit}{$i common.inc}
type TVolts = TVoltQty;

var V: TVoltUnit;

const kV: TVoltQty = (FValue: 1E+03);
const mV: TVoltQty = (FValue: 1E-03);

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty; inline;
operator /(const ALeft: TWattQty; const ARight: TAmpereUnit): TVoltQty; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TCoulombUnit): TVoltQty; inline;

{ Unit of square volt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i common.inc}
class operator /(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareVoltUnit}{$i common.inc}
type TSquareVolts = TSquareVoltQty;

var V2: TSquareVoltUnit;

const kV2: TSquareVoltQty = (FValue: 1E+06);
const mV2: TSquareVoltQty = (FValue: 1E-06);

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty; inline;

{ Unit of farad }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i common.inc}
class operator /(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty; inline;
class operator *(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty; inline;
class operator *(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty; inline;
class operator *(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty; inline;
class operator *(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TFaradUnit}{$i common.inc}
type TFarads = TFaradQty;

var F: TFaradUnit;

const mF: TFaradQty = (FValue: 1E-03);
const miF: TFaradQty = (FValue: 1E-06);
const nF: TFaradQty = (FValue: 1E-09);
const pF: TFaradQty = (FValue: 1E-12);

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TVoltUnit): TFaradQty; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty; inline;

{ Unit of ohm }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i common.inc}
class operator /(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty; inline;
class operator *(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty; inline;
class operator *(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty; inline;
class operator *(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty; inline;
class operator *(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty; inline;
class operator /(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty; inline;
class operator *(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty; inline;
class operator *(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty; inline;
class operator /(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty; inline;
class operator *(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TOhmUnit}{$i common.inc}
type TOhms = TOhmQty;

var ohm: TOhmUnit;

const Gohm: TOhmQty = (FValue: 1E+09);
const megaohm: TOhmQty = (FValue: 1E+06);
const kohm: TOhmQty = (FValue: 1E+03);
const mohm: TOhmQty = (FValue: 1E-03);
const miohm: TOhmQty = (FValue: 1E-06);
const nohm: TOhmQty = (FValue: 1E-09);

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty; inline;
operator /(const ALeft: TVoltQty; const ARight: TAmpereUnit): TOhmQty; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty; inline;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty; inline;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty; inline;

{ Unit of siemens }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TSiemensQty): TOhmQty; inline;
class operator *(const ALeft: TSiemensQty; const ARight: TOhmQty): double; inline;
class operator *(const ALeft: TOhmQty; const ARight: TSiemensQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSiemensUnit}{$i common.inc}
type TSiemens = TSiemensQty;

var siemens: TSiemensUnit;

const millisiemens: TSiemensQty = (FValue: 1E-03);
const microsiemens: TSiemensQty = (FValue: 1E-06);
const nanosiemens: TSiemensQty = (FValue: 1E-09);

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty; inline;
operator /(const ALeft: double; const ARight: TOhmUnit): TSiemensQty; inline;

{ Unit of weber }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i common.inc}
class operator /(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty; inline;
class operator /(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWeberUnit}{$i common.inc}
type TWebers = TWeberQty;

var Wb: TWeberUnit;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty; inline;
operator *(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty; inline;
operator *(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty; inline;

{ Unit of tesla }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i common.inc}
class operator /(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty; inline;
class operator *(const ALeft: TTeslaQty; const ARight: TSquareMeterQty): TWeberQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TTeslaQty): TWeberQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTeslaUnit}{$i common.inc}
type TTeslas = TTeslaQty;

var T: TTeslaUnit;

const mT: TTeslaQty = (FValue: 1E-03);
const miT: TTeslaQty = (FValue: 1E-06);
const nT: TTeslaQty = (FValue: 1E-09);

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty; inline;
operator /(const ALeft: TWeberQty; const ARight: TSquareMeterUnit): TTeslaQty; inline;

{ Unit of henry }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i common.inc}
class operator /(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty; inline;
class operator *(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty; inline;
class operator *(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty; inline;
class operator /(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty; inline;
class operator /(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty; inline;
class operator /(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty; inline;
class operator *(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=THenryUnit}{$i common.inc}
type THenries = THenryQty;

var H: THenryUnit;

const mH: THenryQty = (FValue: 1E-03);
const miH: THenryQty = (FValue: 1E-06);
const nH: THenryQty = (FValue: 1E-09);

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty; inline;
operator /(const ALeft: TWeberQty; const ARight: TAmpereUnit): THenryQty; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty; inline;
operator *(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty; inline;

{ Unit of lumen }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i common.inc}
class operator /(const ALeft: TLumenQty; const ARight: TSteradianQty): TCandelaQty; inline;
class operator /(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TLumenUnit}{$i common.inc}
type TLumens = TLumenQty;

var lm: TLumenUnit;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty; inline;
operator *(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty; inline;
operator *(const ALeft: TCandelaQty; const ARight: TSteradianUnit): TLumenQty; inline;

{ Unit of lux }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i common.inc}
class operator /(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty; inline;
class operator *(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TLuxUnit}{$i common.inc}
type TLux = TLuxQty;

var lx: TLuxUnit;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumenQty; const ARight: TSquareMeterQty): TLuxQty; inline;
operator /(const ALeft: TLumenQty; const ARight: TSquareMeterUnit): TLuxQty; inline;

{ Unit of bequerel }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TBequerelUnit}{$i common.inc}
type TBequerels = THertzQty;

var Bq: THertzUnit;

const kBq: THertzQty = (FValue: 1E+03);
const mBq: THertzQty = (FValue: 1E-03);
const miBq: THertzQty = (FValue: 1E-06);
const nBq: THertzQty = (FValue: 1E-09);
const pBq: THertzQty = (FValue: 1E-12);

{ Unit of gray }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TGrayUnit}{$i common.inc}
type TGrays = TSquareMeterPerSquareSecondQty;

var Gy: TSquareMeterPerSquareSecondUnit;

const kGy: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
const mGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
const miGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
const nGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

{ Unit of sievert }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSievertUnit}{$i common.inc}
type TSieverts = TSquareMeterPerSquareSecondQty;

var Sv: TSquareMeterPerSquareSecondUnit;

const kSv: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
const mSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
const miSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
const nSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

{ Unit of katal }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i common.inc}
class operator /(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty; inline;
class operator *(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKatalUnit}{$i common.inc}
type TKatals = TKatalQty;

var kat: TKatalUnit;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty; inline;
operator /(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty; inline;

{ Unit of joule per radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty; inline;
class operator *(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty; inline;
class operator *(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i common.inc}
type TJoulesPerRadian = TJoulePerRadianQty;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty; inline;

{ Unit of joule per degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i common.inc}
type TJoulesPerDegree = TJoulePerRadianQty;

{ Unit of newton meter per radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i common.inc}
type TNewtonMetersPerRadian = TJoulePerRadianQty;

{ Unit of newton meter per degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i common.inc}
type TNewtonMetersPerDegree = TJoulePerRadianQty;

{ Unit of newton per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TKilogramPerCubicMeterQty; inline;
class operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSecondSquaredQty; inline;
class operator /(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty; inline;
class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty; inline;
class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty; inline;
class operator *(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i common.inc}
type TNewtonsPerCubicMeter = TNewtonPerCubicMeterQty;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TNewtonPerCubicMeterQty; inline;
operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty; inline;

{ Unit of newton per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i common.inc}
class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty; inline;
class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty; inline;
class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty; inline;
class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i common.inc}
type TNewtonsPerMeter = TNewtonPerMeterQty;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TMeterUnit): TNewtonPerMeterQty; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty; inline;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]
operator *(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty; inline;
operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty; inline;

{ Unit of pound force per inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i common.inc}
type TPoundsForcePerInch = TNewtonPerMeterQty;

{ Unit of cubic meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty; inline;
class operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i common.inc}
type TCubicMetersPerSecond = TCubicMeterPerSecondQty;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty; inline;
operator /(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty; inline;

{ Unit of kilogram per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty; inline;
class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty; inline;
class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty; inline;
class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty; inline;
class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty; inline;
class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty; inline;
class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty; inline;
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i common.inc}
type TKilogramsPerSecond = TKilogramPerSecondQty;

// main definition [ kg/s ] = [ kg ] / [ s ]
operator /(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty; inline;
operator /(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty; inline;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]
operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty; inline;

// alternative definition [ kg/s ] = [ kg/m ] * [ m/s ]
operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty; inline;

// alternative definition [ kg/s ] = [ W ] / [ m2/s2 ]
operator /(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty; inline;

{ Unit of poiseuille }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i common.inc}
class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty; inline;
class operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty; inline;
class operator /(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty; inline;
class operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty; inline;
class operator /(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPoiseuilleUnit}{$i common.inc}
type TPoiseuilles = TPoiseuilleQty;

var Pl: TPoiseuilleUnit;

const cPl: TPoiseuilleQty = (FValue: 1E-02);
const mPl: TPoiseuilleQty = (FValue: 1E-03);
const miPl: TPoiseuilleQty = (FValue: 1E-06);

// main definition [ Pl ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty; inline;
operator *(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty; inline;
operator *(const ALeft: TPascalQty; const ARight: TSecondUnit): TPoiseuilleQty; inline;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]
operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty; inline;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty; inline;
operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterUnit): TKilogramPerSecondQty; inline;

{ Unit of pascal second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TPascalSecondUnit}{$i common.inc}
type TPascalSeconds = TPoiseuilleQty;

{ Unit of square meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty; inline;
class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty; inline;
class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty; inline;
class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i common.inc}
type TSquareMetersPerSecond = TSquareMeterPerSecondQty;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty; inline;
operator /(const ALeft: TSquareMeterQty; const ARight: TSecondUnit): TSquareMeterPerSecondQty; inline;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]
operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty; inline;

{ Unit of kilogram per quartic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i common.inc}
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty; inline;
class operator *(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty; inline;
class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i common.inc}
type TKilogramsPerQuarticMeter = TKilogramPerQuarticMeterQty;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty; inline;
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterQty; inline;

{ Unit of quartic meter second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i common.inc}
class operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty; inline;
class operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i common.inc}
type TQuarticMeterSeconds = TQuarticMeterSecondQty;

// main definition [ m4*s ] = [ m4 ] * [ s ]
operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty; inline;
operator *(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty; inline;
operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondUnit): TQuarticMeterSecondQty; inline;

{ Unit of kilogram per quartic meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty; inline;
class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty; inline;
class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty; inline;
class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty; inline;
class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty; inline;
class operator *(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty; inline;
class operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty; inline;
class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty; inline;
class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty; inline;
class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i common.inc}
type TKilogramsPerQuarticMeterPerSecond = TKilogramPerQuarticMeterPerSecondQty;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty; inline;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterPerSecondQty; inline;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]
operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty; inline;
operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondUnit): TKilogramPerQuarticMeterPerSecondQty; inline;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty; inline;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]
operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty; inline;

{ Unit of cubic meter per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty; inline;
class operator *(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramPerCubicMeterQty): double; inline;
class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): double; inline;
class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty; inline;
class operator *(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty; inline;
class operator *(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i common.inc}
type TCubicMetersPerKilogram = TCubicMeterPerKilogramQty;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]
operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty; inline;
operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramUnit): TCubicMeterPerKilogramQty; inline;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]
operator /(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty; inline;

{ Unit of kilogram square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty; inline;
class operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i common.inc}
type TKilogramSquareSeconds = TKilogramSquareSecondQty;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]
operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty; inline;
operator *(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty; inline;
operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondUnit): TKilogramSquareSecondQty; inline;

{ Unit of cubic meter per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondSquaredQty; inline;
class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterQty; inline;
class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty; inline;
class operator *(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i common.inc}
type TCubicMetersPerSquareSecond = TCubicMeterPerSquareSecondQty;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]
operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty; inline;
operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondUnit): TCubicMeterPerSquareSecondQty; inline;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]
operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondSquaredQty): TCubicMeterPerSquareSecondQty; inline;

{ Unit of newton square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareMeterQty): TNewtonQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i common.inc}
type TNewtonSquareMeters = TNewtonSquareMeterQty;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TNewtonSquareMeterQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonQty): TNewtonSquareMeterQty; inline;
operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterQty; inline;

// alternative definition [ N*m2 ] = [ J ] * [ m ]
operator *(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty; inline;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]
operator *(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty; inline;
operator *(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty; inline;

{ Unit of newton per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i common.inc}
class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty; inline;
class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i common.inc}
type TNewtonsPerSquareKilogram = TNewtonPerSquareKilogramQty;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramUnit): TNewtonPerSquareKilogramQty; inline;

{ Unit of square kilogram per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i common.inc}
class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i common.inc}
type TSquareKilogramsPerMeter = TSquareKilogramPerMeterQty;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty; inline;
operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterUnit): TSquareKilogramPerMeterQty; inline;

{ Unit of square kilogram per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i common.inc}
type TSquareKilogramsPerSquareMeter = TSquareKilogramPerSquareMeterQty;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty; inline;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterUnit): TSquareKilogramPerSquareMeterQty; inline;

{ Unit of square meter per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i common.inc}
type TSquareMetersPerSquareKilogram = TSquareMeterPerSquareKilogramQty;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty; inline;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramUnit): TSquareMeterPerSquareKilogramQty; inline;

{ Unit of newton square meter per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i common.inc}
class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty; inline;
class operator *(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty; inline;
class operator /(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty; inline;
class operator *(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty; inline;
class operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareSecondQty): TCubicMeterPerKilogramQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerKilogramQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty; inline;
class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty; inline;
class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty; inline;
class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i common.inc}
type TNewtonSquareMetersPerSquareKilogram = TNewtonSquareMeterPerSquareKilogramQty;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty; inline;
operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty; inline;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramUnit): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty; inline;
operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]
operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]
operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]
operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty; inline;

{ Unit of reciprocal kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i common.inc}
type TReciprocalKelvins = TReciprocalKelvinQty;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty; inline;
operator /(const ALeft: double; const ARight: TKelvinUnit): TReciprocalKelvinQty; inline;

{ Unit of kilogram kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i common.inc}
class operator /(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty; inline;
class operator /(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i common.inc}
type TKilogramKelvins = TKilogramKelvinQty;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty; inline;
operator *(const ALeft: TKilogramQty; const ARight: TKelvinUnit): TKilogramKelvinQty; inline;

{ Unit of joule per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i common.inc}
type TJoulesPerKelvin = TJoulePerKelvinQty;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TKelvinUnit): TJoulePerKelvinQty; inline;

{ Unit of joule per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i common.inc}
type TJoulesPerKilogram = TSquareMeterPerSquareSecondQty;

{ Unit of joule per kilogram per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$i common.inc}
class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramQty; inline;
class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramQty): TJoulePerKelvinQty; inline;
class operator *(const ALeft: TKilogramQty; const ARight: TJoulePerKilogramPerKelvinQty): TJoulePerKelvinQty; inline;
class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TJoulePerKilogramPerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKelvinQty): TSquareMeterPerSquareSecondQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TSquareMeterPerSquareSecondQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramKelvinQty; inline;
class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramKelvinQty): TJouleQty; inline;
class operator *(const ALeft: TKilogramKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i common.inc}
type TJoulesPerKilogramPerKelvin = TJoulePerKilogramPerKelvinQty;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty; inline;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinUnit): TJoulePerKilogramPerKelvinQty; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty; inline;
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramUnit): TJoulePerKilogramPerKelvinQty; inline;

{ Unit of meter kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i common.inc}
class operator /(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty; inline;
class operator /(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterKelvinUnit}{$i common.inc}
type TMeterKelvins = TMeterKelvinQty;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TKelvinUnit): TMeterKelvinQty; inline;

{ Unit of kelvin per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i common.inc}
class operator /(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i common.inc}
type TKelvinsPerMeter = TKelvinPerMeterQty;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty; inline;
operator /(const ALeft: TKelvinQty; const ARight: TMeterUnit): TKelvinPerMeterQty; inline;

{ Unit of watt per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerMeterUnit}{$i common.inc}
type TWattsPerMeter = TWattPerMeterQty;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty; inline;
operator /(const ALeft: TWattQty; const ARight: TMeterUnit): TWattPerMeterQty; inline;

{ Unit of watt per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i common.inc}
type TWattsPerSquareMeter = TWattPerSquareMeterQty;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty; inline;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterQty; inline;

{ Unit of watt per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i common.inc}
type TWattsPerKelvin = TWattPerKelvinQty;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty; inline;
operator /(const ALeft: TWattQty; const ARight: TKelvinUnit): TWattPerKelvinQty; inline;

{ Unit of watt per meter per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i common.inc}
class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty; inline;
class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty; inline;
class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty; inline;
class operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty; inline;
class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty; inline;
class operator /(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty; inline;
class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty; inline;
class operator *(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i common.inc}
type TWattsPerMeterPerKelvin = TWattPerMeterPerKelvinQty;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty; inline;
operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinUnit): TWattPerMeterPerKelvinQty; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty; inline;
operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterUnit): TWattPerMeterPerKelvinQty; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty; inline;

{ Unit of square meter kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty; inline;
class operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TSquareMeterQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i common.inc}
type TSquareMeterKelvins = TSquareMeterKelvinQty;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinUnit): TSquareMeterKelvinQty; inline;

{ Unit of watt per square meter per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$i common.inc}
class operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterQty): TWattPerKelvinQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerKelvinQty; inline;
class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerSquareMeterQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerSquareMeterQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterKelvinQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterKelvinQty): TWattQty; inline;
class operator *(const ALeft: TSquareMeterKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i common.inc}
type TWattsPerSquareMeterPerKelvin = TWattPerSquareMeterPerKelvinQty;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWattQty; const ARight: TSquareMeterKelvinQty): TWattPerSquareMeterPerKelvinQty; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinQty): TWattPerSquareMeterPerKelvinQty; inline;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinUnit): TWattPerSquareMeterPerKelvinQty; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerKelvinQty; inline;
operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerKelvinQty; inline;

{ Unit of square meter quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TQuarticKelvinQty): TSquareMeterQty; inline;
class operator /(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TSquareMeterQty): TQuarticKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i common.inc}
type TSquareMeterQuarticKelvins = TSquareMeterQuarticKelvinQty;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinQty): TSquareMeterQuarticKelvinQty; inline;
operator *(const ALeft: TQuarticKelvinQty; const ARight: TSquareMeterQty): TSquareMeterQuarticKelvinQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinUnit): TSquareMeterQuarticKelvinQty; inline;

{ Unit of watt per quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty; inline;
class operator *(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty; inline;
class operator *(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i common.inc}
type TWattsPerQuarticKelvin = TWattPerQuarticKelvinQty;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinQty): TWattPerQuarticKelvinQty; inline;
operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinUnit): TWattPerQuarticKelvinQty; inline;

{ Unit of watt per square meter per quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$i common.inc}
class operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerQuarticKelvinQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerQuarticKelvinQty; inline;
class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TQuarticKelvinQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterQty; inline;
class operator *(const ALeft: TQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerSquareMeterQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQuarticKelvinQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQuarticKelvinQty): TWattQty; inline;
class operator *(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i common.inc}
type TWattsPerSquareMeterPerQuarticKelvin = TWattPerSquareMeterPerQuarticKelvinQty;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWattQty; const ARight: TSquareMeterQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty; inline;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinUnit): TWattPerSquareMeterPerQuarticKelvinQty; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerQuarticKelvinQty; inline;
operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerQuarticKelvinQty; inline;

{ Unit of joule per mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: TJoulePerMoleQty): TMoleQty; inline;
class operator *(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty; inline;
class operator *(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i common.inc}
type TJoulesPerMole = TJoulePerMoleQty;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty; inline;
operator /(const ALeft: TJouleQty; const ARight: TMoleUnit): TJoulePerMoleQty; inline;

{ Unit of mole kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i common.inc}
class operator /(const ALeft: TMoleKelvinQty; const ARight: TKelvinQty): TMoleQty; inline;
class operator /(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMoleKelvinUnit}{$i common.inc}
type TMoleKelvins = TMoleKelvinQty;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty; inline;
operator *(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty; inline;
operator *(const ALeft: TMoleQty; const ARight: TKelvinUnit): TMoleKelvinQty; inline;

{ Unit of joule per mole per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$i common.inc}
class operator /(const ALeft: TJoulePerMoleQty; const ARight: TJoulePerMolePerKelvinQty): TKelvinQty; inline;
class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TKelvinQty): TJoulePerMoleQty; inline;
class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerMoleQty; inline;
class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TMoleQty; inline;
class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleQty): TJoulePerKelvinQty; inline;
class operator *(const ALeft: TMoleQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerKelvinQty; inline;
class operator /(const ALeft: TJouleQty; const ARight: TJoulePerMolePerKelvinQty): TMoleKelvinQty; inline;
class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleKelvinQty): TJouleQty; inline;
class operator *(const ALeft: TMoleKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i common.inc}
type TJoulesPerMolePerKelvin = TJoulePerMolePerKelvinQty;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJouleQty; const ARight: TMoleKelvinQty): TJoulePerMolePerKelvinQty; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleQty): TJoulePerMolePerKelvinQty; inline;
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleUnit): TJoulePerMolePerKelvinQty; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinQty): TJoulePerMolePerKelvinQty; inline;
operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinUnit): TJoulePerMolePerKelvinQty; inline;

{ Unit of ohm meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i common.inc}
class operator /(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty; inline;
class operator /(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TOhmMeterUnit}{$i common.inc}
type TOhmMeters = TOhmMeterQty;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty; inline;
operator *(const ALeft: TOhmQty; const ARight: TMeterUnit): TOhmMeterQty; inline;

{ Unit of volt per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i common.inc}
class operator /(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty; inline;
class operator /(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty; inline;
class operator /(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty; inline;
class operator *(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty; inline;
class operator *(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty; inline;
class operator /(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i common.inc}
type TVoltsPerMeter = TVoltPerMeterQty;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty; inline;
operator /(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltPerMeterQty; inline;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TCoulombUnit): TVoltPerMeterQty; inline;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty; inline;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty; inline;

{ Unit of newton per coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i common.inc}
type TNewtonsPerCoulomb = TVoltPerMeterQty;

{ Unit of coulomb per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i common.inc}
class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i common.inc}
type TCoulombsPerMeter = TCoulombPerMeterQty;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TMeterUnit): TCoulombPerMeterQty; inline;

{ Unit of square coulomb per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i common.inc}
class operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty; inline;
class operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty; inline;
class operator /(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i common.inc}
type TSquareCoulombsPerMeter = TSquareCoulombPerMeterQty;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty; inline;
operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterUnit): TSquareCoulombPerMeterQty; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty; inline;
operator *(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty; inline;

{ Unit of coulomb per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty; inline;
class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i common.inc}
type TCoulombsPerSquareMeter = TCoulombPerSquareMeterQty;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterUnit): TCoulombPerSquareMeterQty; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty; inline;

{ Unit of square meter per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty; inline;
class operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i common.inc}
type TSquareMetersPerSquareCoulomb = TSquareMeterPerSquareCoulombQty;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty; inline;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombUnit): TSquareMeterPerSquareCoulombQty; inline;

{ Unit of newton per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i common.inc}
class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty; inline;
class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty; inline;
class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i common.inc}
type TNewtonsPerSquareCoulomb = TNewtonPerSquareCoulombQty;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombUnit): TNewtonPerSquareCoulombQty; inline;

{ Unit of newton square meter per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i common.inc}
class operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty; inline;
class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty; inline;
class operator /(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty; inline;
class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty; inline;
class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty; inline;
class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i common.inc}
type TNewtonSquareMetersPerSquareCoulomb = TNewtonSquareMeterPerSquareCoulombQty;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty; inline;
operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty; inline;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombUnit): TNewtonSquareMeterPerSquareCoulombQty; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty; inline;
operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareCoulombQty; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty; inline;

{ Unit of volt meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i common.inc}
class operator /(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty; inline;
class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty; inline;
class operator /(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty; inline;
class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TVoltMeterUnit}{$i common.inc}
type TVoltMeters = TVoltMeterQty;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty; inline;
operator *(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltMeterQty; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty; inline;

{ Unit of newton square meter per coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i common.inc}
type TNewtonSquareMetersPerCoulomb = TVoltMeterQty;

{ Unit of volt meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i common.inc}
class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i common.inc}
type TVoltMetersPerSecond = TVoltMeterPerSecondQty;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty; inline;
operator /(const ALeft: TVoltMeterQty; const ARight: TSecondUnit): TVoltMeterPerSecondQty; inline;

{ Unit of farad per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty; inline;
class operator *(const ALeft: TFaradPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): double; inline;
class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TFaradPerMeterQty): double; inline;
class operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TFaradPerMeterQty): TVoltPerMeterQty; inline;
class operator *(const ALeft: TFaradPerMeterQty; const ARight: TVoltPerMeterQty): TCoulombPerSquareMeterQty; inline;
class operator *(const ALeft: TVoltPerMeterQty; const ARight: TFaradPerMeterQty): TCoulombPerSquareMeterQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TFaradPerMeterQty): TVoltMeterQty; inline;
class operator *(const ALeft: TFaradPerMeterQty; const ARight: TVoltMeterQty): TCoulombQty; inline;
class operator *(const ALeft: TVoltMeterQty; const ARight: TFaradPerMeterQty): TCoulombQty; inline;
class operator /(const ALeft: TFaradQty; const ARight: TFaradPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TFaradPerMeterQty; const ARight: TMeterQty): TFaradQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TFaradPerMeterQty): TFaradQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i common.inc}
type TFaradsPerMeter = TFaradPerMeterQty;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty; inline;
operator /(const ALeft: TFaradQty; const ARight: TMeterUnit): TFaradPerMeterQty; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty; inline;

{ Unit of ampere per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i common.inc}
class operator /(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i common.inc}
type TAmperesPerMeter = TAmperePerMeterQty;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty; inline;
operator /(const ALeft: TAmpereQty; const ARight: TMeterUnit): TAmperePerMeterQty; inline;

{ Unit of meter per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i common.inc}
class operator /(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty; inline;
class operator *(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i common.inc}
type TMetersPerAmpere = TMeterPerAmpereQty;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty; inline;
operator /(const ALeft: TMeterQty; const ARight: TAmpereUnit): TMeterPerAmpereQty; inline;

{ Unit of tesla meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i common.inc}
class operator /(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty; inline;
class operator *(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty; inline;
class operator /(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty; inline;
class operator /(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTeslaMeterUnit}{$i common.inc}
type TTeslaMeters = TTeslaMeterQty;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty; inline;
operator *(const ALeft: TTeslaQty; const ARight: TMeterUnit): TTeslaMeterQty; inline;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty; inline;
operator /(const ALeft: TNewtonQty; const ARight: TAmpereUnit): TTeslaMeterQty; inline;

{ Unit of newton per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i common.inc}
type TNewtonsPerAmpere = TTeslaMeterQty;

{ Unit of tesla per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i common.inc}
class operator /(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty; inline;
class operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i common.inc}
type TTeslasPerAmpere = TTeslaPerAmpereQty;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty; inline;
operator /(const ALeft: TTeslaQty; const ARight: TAmpereUnit): TTeslaPerAmpereQty; inline;

{ Unit of henry per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i common.inc}
class operator /(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty; inline;
class operator *(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty; inline;
class operator *(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty; inline;
class operator /(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty; inline;
class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty; inline;
class operator *(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty; inline;
class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty; inline;
class operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty; inline;
class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty; inline;
class operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty; inline;
class operator /(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty; inline;
class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty; inline;
class operator *(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty; inline;
class operator /(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=THenryPerMeterUnit}{$i common.inc}
type THenriesPerMeter = THenryPerMeterQty;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty; inline;
operator /(const ALeft: THenryQty; const ARight: TMeterUnit): THenryPerMeterQty; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty; inline;
operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereUnit): THenryPerMeterQty; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty; inline;
operator *(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty; inline;
operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterUnit): THenryPerMeterQty; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty; inline;
operator *(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty; inline;

{ Unit of tesla meter per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i common.inc}
type TTeslaMetersPerAmpere = THenryPerMeterQty;

{ Unit of newton per square ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i common.inc}
type TNewtonsPerSquareAmpere = THenryPerMeterQty;

{ Unit of radian per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i common.inc}
class operator /(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty; inline;
class operator *(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i common.inc}
type TRadiansPerMeter = TRadianPerMeterQty;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty; inline;
operator /(const ALeft: TRadianQty; const ARight: TMeterUnit): TRadianPerMeterQty; inline;

{ Unit of square kilogram per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i common.inc}
class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty; inline;
class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty; inline;
class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty; inline;
class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty; inline;
class operator *(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty; inline;
class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i common.inc}
type TSquareKilogramsPerSquareSecond = TSquareKilogramPerSquareSecondQty;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty; inline;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty; inline;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]
operator *(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty; inline;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]
operator *(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty; inline;
operator *(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty; inline;

{ Unit of reciprocal meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty; inline;
class operator *(const ALeft: TReciprocalMeterQty; const ARight: TMeterQty): double; inline;
class operator *(const ALeft: TMeterQty; const ARight: TReciprocalMeterQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i common.inc}
type TReciprocalMeters = TReciprocalMeterQty;

// main definition [ 1/m ] = 1 / [ m ]
operator /(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty; inline;
operator /(const ALeft: double; const ARight: TMeterUnit): TReciprocalMeterQty; inline;

{ Unit of square second per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty; inline;
class operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty; inline;
class operator /(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty; inline;
class operator *(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): double; inline;
class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): double; inline;
class operator /(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i common.inc}
type TSquareSecondsPerSquareMeter = TSquareSecondPerSquareMeterQty;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]
operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty; inline;
operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterUnit): TSquareSecondPerSquareMeterQty; inline;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]
operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty; inline;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]
operator *(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty; inline;
operator *(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty; inline;

{ Unit of square joule }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i common.inc}
class operator /(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareJouleUnit}{$i common.inc}
type TSquareJoules = TSquareJouleQty;

var J2: TSquareJouleUnit;

const TJ2: TSquareJouleQty = (FValue: 1E+24);
const GJ2: TSquareJouleQty = (FValue: 1E+18);
const MJ2: TSquareJouleQty = (FValue: 1E+12);
const kJ2: TSquareJouleQty = (FValue: 1E+06);

// main definition [ J2 ] = [ J ] * [ J ]
operator *(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty; inline;

{ Unit of joule second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TJouleSecondUnit}{$i common.inc}
type TJouleSeconds = TKilogramSquareMeterPerSecondQty;

{ Unit of elettronvolt second }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i common.inc}
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i common.inc}
type TElettronvoltSeconds = TKilogramSquareMeterPerSecondQty;

{ Unit of lumen per watt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i common.inc}
class operator /(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty; inline;
class operator *(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty; inline;
class operator *(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TLumenPerWattUnit}{$i common.inc}
type TLumensPerWatt = TLumenPerWattQty;

// main definition [ lm/W ] = [ lm ] / [ W ]
operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty; inline;
operator /(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty; inline;

{ Unit of reciprocal mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i common.inc}
class operator /(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty; inline;
class operator *(const ALeft: TReciprocalMoleQty; const ARight: TMoleQty): double; inline;
class operator *(const ALeft: TMoleQty; const ARight: TReciprocalMoleQty): double; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i common.inc}
type TReciprocalMoles = TReciprocalMoleQty;

// main definition [ 1/mol ] = 1 / [ mol ]
operator /(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty; inline;
operator /(const ALeft: double; const ARight: TMoleUnit): TReciprocalMoleQty; inline;

{ Unit of ampere per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty; inline;
class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty; inline;
class operator /(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i common.inc}
type TAmperesPerSquareMeter = TAmperePerSquareMeterQty;

// main definition [ A/m2 ] = [ A ] / [ m2 ]
operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty; inline;
operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterUnit): TAmperePerSquareMeterQty; inline;

// alternative definition [ A/m2 ] = [ A/m ] / [ m ]
operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty; inline;

{ Unit of mole per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty; inline;
class operator *(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty; inline;
class operator *(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i common.inc}
type TMolesPerCubicMeter = TMolePerCubicMeterQty;

// main definition [ mol/m3 ] = [ mol ] / [ m3 ]
operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty; inline;
operator /(const ALeft: TMoleQty; const ARight: TCubicMeterUnit): TMolePerCubicMeterQty; inline;

{ Unit of candela per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i common.inc}
class operator /(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i common.inc}
type TCandelasPerSquareMeter = TCandelaPerSquareMeterQty;

// main definition [ cd/m2 ] = [ cd ] / [ m2 ]
operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty; inline;
operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterUnit): TCandelaPerSquareMeterQty; inline;

{ Unit of coulomb per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty; inline;
class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty; inline;
class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty; inline;
class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty; inline;
class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty; inline;
class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty; inline;
class operator *(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i common.inc}
type TCoulombsPerCubicMeter = TCoulombPerCubicMeterQty;

// main definition [ C/m3 ] = [ C ] / [ m3 ]
operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterUnit): TCoulombPerCubicMeterQty; inline;

// alternative definition [ C/m3 ] = [ C/m ] / [ m2 ]
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty; inline;

// alternative definition [ C/m3 ] = [ C/m2 ] / [ m ]
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty; inline;

{ Unit of coulomb per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i common.inc}
class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty; inline;
class operator *(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty; inline;
class operator *(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i common.inc}
type TCoulombsPerKilogram = TCoulombPerKilogramQty;

// main definition [ C/kg ] = [ C ] / [ kg ]
operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TKilogramUnit): TCoulombPerKilogramQty; inline;

{ Unit of gray per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty; inline;
class operator *(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty; inline;
class operator *(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i common.inc}
type TGraysPerSecond = TGrayPerSecondQty;

// main definition [ Gy/s ] = [ Gy ] / [ s ]
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty; inline;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondUnit): TGrayPerSecondQty; inline;

{ Unit of watt per steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i common.inc}
class operator /(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty; inline;
class operator *(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty; inline;
class operator *(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i common.inc}
type TWattsPerSteradian = TWattPerSteradianQty;

// main definition [ W/sr ] = [ W ] / [ sr ]
operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty; inline;
operator /(const ALeft: TWattQty; const ARight: TSteradianUnit): TWattPerSteradianQty; inline;

{ Unit of square meter steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i common.inc}
class operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSteradianQty): TSquareMeterQty; inline;
class operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSquareMeterQty): TSteradianQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i common.inc}
type TSquareMeterSteradians = TSquareMeterSteradianQty;

// main definition [ m2 * sr ] = [ m2 ] * [ sr ]
operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianQty): TSquareMeterSteradianQty; inline;
operator *(const ALeft: TSteradianQty; const ARight: TSquareMeterQty): TSquareMeterSteradianQty; inline;
operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianUnit): TSquareMeterSteradianQty; inline;

{ Unit of watt per square meter per steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$i common.inc}
class operator /(const ALeft: TWattPerSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSteradianQty; inline;
class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSteradianQty; inline;
class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSteradianQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSteradianQty): TWattPerSquareMeterQty; inline;
class operator *(const ALeft: TSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSquareMeterQty; inline;
class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterSteradianQty; inline;
class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterSteradianQty): TWattQty; inline;
class operator *(const ALeft: TSquareMeterSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i common.inc}
type TWattsPerSquareMeterPerSteradian = TWattPerSquareMeterPerSteradianQty;

// main definition [ W/m2/sr ] = [ W ] / [ m2 * sr ]
operator /(const ALeft: TWattQty; const ARight: TSquareMeterSteradianQty): TWattPerSquareMeterPerSteradianQty; inline;

// alternative definition [ W/m2/sr ] = [ W/m2 ] / [ sr ]
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty; inline;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianUnit): TWattPerSquareMeterPerSteradianQty; inline;

// alternative definition [ W/m2/sr ] = [ W/sr ] / [ m2 ]
operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty; inline;
operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerSteradianQty; inline;

{ Unit of katal per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i common.inc}
class operator /(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty; inline;
class operator *(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty; inline;
class operator *(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i common.inc}
type TKatalsPerCubicMeter = TKatalPerCubicMeterQty;

// main definition [ kat/m3 ] = [ kat ] / [ m3 ]
operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty; inline;
operator /(const ALeft: TKatalQty; const ARight: TCubicMeterUnit): TKatalPerCubicMeterQty; inline;

{ Unit of coulomb per mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i common.inc}
class operator /(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty; inline;
class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty; inline;
class operator *(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty; inline;
class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty; inline;
class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty; inline;
class operator *(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty; inline;
{$DEFINE INTF_UNIT}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i common.inc}
type TCoulombsPerMole = TCoulombPerMoleQty;

// main definition [ C/mol ] = [ C ] / [ mol ]
operator /(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty; inline;
operator /(const ALeft: TCoulombQty; const ARight: TMoleUnit): TCoulombPerMoleQty; inline;

// alternative definition [ C/mol ] = [ J/mol ] / [ V ]
operator /(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty; inline;

{ Helpers }

type
  TSecondHelper = record helper for TSecondQty
    function ToMinute: TMinuteQty;
    function ToHour: THourQty;
    function ToDay: TDayQty;
  end;

type
  TSquareSecondHelper = record helper for TSquareSecondQty
    function ToSquareMinute: TSquareMinuteQty;
    function ToSquareHour: TSquareHourQty;
    function ToSquareDay: TSquareDayQty;
  end;

type
  TMeterHelper = record helper for TMeterQty
    function ToNauticalMile: TNauticalMileQty;
    function ToMile: TMileQty;
    function ToYard: TYardQty;
    function ToFoot: TFootQty;
    function ToInch: TInchQty;
    function ToAstronomical: TAstronomicalQty;
  end;

type
  TSquareMeterHelper = record helper for TSquareMeterQty
    function ToSquareMile: TSquareMileQty;
    function ToSquareYard: TSquareYardQty;
    function ToSquareFoot: TSquareFootQty;
    function ToSquareInch: TSquareInchQty;
  end;

type
  TCubicMeterHelper = record helper for TCubicMeterQty
    function ToGallon: TGallonQty;
    function ToLitre: TLitreQty;
    function ToCubicYard: TCubicYardQty;
    function ToCubicFoot: TCubicFootQty;
    function ToCubicInch: TCubicInchQty;
  end;

type
  TKilogramHelper = record helper for TKilogramQty
    function ToTon: TTonQty;
    function ToStone: TStoneQty;
    function ToOunce: TOunceQty;
    function ToPound: TPoundQty;
    function ToTonne: TTonneQty;
  end;

type
  TDegreeCelsiusHelper = record helper for TDegreeCelsiusQty
    function ToKelvin: TKelvinQty;
  end;

type
  TKelvinHelper = record helper for TKelvinQty
    function ToDegreeFahrenheit: TDegreeFahrenheitQty;
    function ToDegreeCelsius: TDegreeCelsiusQty;
  end;

type
  TDegreeFahrenheitHelper = record helper for TDegreeFahrenheitQty
    function ToKelvin: TKelvinQty;
  end;

type
  TRadianHelper = record helper for TRadianQty
    function ToDegree: TDegreeQty;
  end;

type
  TSteradianHelper = record helper for TSteradianQty
    function ToSquareDegree: TSquareDegreeQty;
  end;

type
  TSquareHertzHelper = record helper for TSquareHertzQty
    function ToSteradianPerSquareSecond: TSteradianPerSquareSecondQty;
    function ToRadianPerSecondSquared: TRadianPerSecondSquaredQty;
  end;

type
  TMeterPerSecondHelper = record helper for TMeterPerSecondQty
    function ToNauticalMilePerHour: TNauticalMilePerHourQty;
    function ToMilePerHour: TMilePerHourQty;
    function ToMeterPerHour: TMeterPerHourQty;
  end;

type
  TMeterPerSecondSquaredHelper = record helper for TMeterPerSecondSquaredQty
    function ToMeterPerHourPerSecond: TMeterPerHourPerSecondQty;
    function ToMeterPerSecondPerSecond: TMeterPerSecondPerSecondQty;
  end;

type
  TKilogramMeterPerSecondHelper = record helper for TKilogramMeterPerSecondQty
    function ToNewtonSecond: TNewtonSecondQty;
  end;

type
  TNewtonHelper = record helper for TNewtonQty
    function ToPoundForce: TPoundForceQty;
  end;

type
  TPascalHelper = record helper for TPascalQty
    function ToJoulePerCubicMeter: TJoulePerCubicMeterQty;
    function ToPoundPerSquareInch: TPoundPerSquareInchQty;
    function ToBar: TBarQty;
  end;

type
  TJouleHelper = record helper for TJouleQty
    function ToPoundForceInch: TPoundForceInchQty;
    function ToNewtonMeter: TNewtonMeterQty;
    function ToElettronvolt: TElettronvoltQty;
    function ToWattHour: TWattHourQty;
  end;

type
  TCoulombHelper = record helper for TCoulombQty
    function ToAmpereHour: TAmpereHourQty;
  end;

type
  THertzHelper = record helper for THertzQty
    function ToBequerel: TBequerelQty;
  end;

type
  TSquareMeterPerSquareSecondHelper = record helper for TSquareMeterPerSquareSecondQty
    function ToJoulePerKilogram: TJoulePerKilogramQty;
    function ToSievert: TSievertQty;
    function ToGray: TGrayQty;
  end;

type
  TJoulePerRadianHelper = record helper for TJoulePerRadianQty
    function ToNewtonMeterPerDegree: TNewtonMeterPerDegreeQty;
    function ToNewtonMeterPerRadian: TNewtonMeterPerRadianQty;
    function ToJoulePerDegree: TJoulePerDegreeQty;
  end;

type
  TNewtonPerMeterHelper = record helper for TNewtonPerMeterQty
    function ToPoundForcePerInch: TPoundForcePerInchQty;
  end;

type
  TPoiseuilleHelper = record helper for TPoiseuilleQty
    function ToPascalSecond: TPascalSecondQty;
  end;

type
  TVoltPerMeterHelper = record helper for TVoltPerMeterQty
    function ToNewtonPerCoulomb: TNewtonPerCoulombQty;
  end;

type
  TVoltMeterHelper = record helper for TVoltMeterQty
    function ToNewtonSquareMeterPerCoulomb: TNewtonSquareMeterPerCoulombQty;
  end;

type
  TTeslaMeterHelper = record helper for TTeslaMeterQty
    function ToNewtonPerAmpere: TNewtonPerAmpereQty;
  end;

type
  THenryPerMeterHelper = record helper for THenryPerMeterQty
    function ToNewtonPerSquareAmpere: TNewtonPerSquareAmpereQty;
    function ToTeslaMeterPerAmpere: TTeslaMeterPerAmpereQty;
  end;

type
  TKilogramSquareMeterPerSecondHelper = record helper for TKilogramSquareMeterPerSecondQty
    function ToElettronvoltSecond: TElettronvoltSecondQty;
    function ToJouleSecond: TJouleSecondQty;
  end;

{ Power units }

function SquarePower(AQuantity: TSecondQty): TSquareSecondQty;
function SquareRoot(AQuantity: TSquareSecondQty): TSecondQty;
function SquarePower(AQuantity: TMeterQty): TSquareMeterQty;
function SquareRoot(AQuantity: TSquareMeterQty): TMeterQty;
function CubicPower(AQuantity: TMeterQty): TCubicMeterQty;
function CubicRoot(AQuantity: TCubicMeterQty): TMeterQty;
function SquarePower(AQuantity: TSquareMeterQty): TQuarticMeterQty;
function SquareRoot(AQuantity: TQuarticMeterQty): TSquareMeterQty;
function QuarticPower(AQuantity: TMeterQty): TQuarticMeterQty;
function QuarticRoot(AQuantity: TQuarticMeterQty): TMeterQty;
function QuinticPower(AQuantity: TMeterQty): TQuinticMeterQty;
function QuinticRoot(AQuantity: TQuinticMeterQty): TMeterQty;
function SquarePower(AQuantity: TCubicMeterQty): TSexticMeterQty;
function SquareRoot(AQuantity: TSexticMeterQty): TCubicMeterQty;
function CubicPower(AQuantity: TSquareMeterQty): TSexticMeterQty;
function CubicRoot(AQuantity: TSexticMeterQty): TSquareMeterQty;
function SexticPower(AQuantity: TMeterQty): TSexticMeterQty;
function SexticRoot(AQuantity: TSexticMeterQty): TMeterQty;
function SquarePower(AQuantity: TAmpereQty): TSquareAmpereQty;
function SquareRoot(AQuantity: TSquareAmpereQty): TAmpereQty;
function SquarePower(AQuantity: TKelvinQty): TSquareKelvinQty;
function SquareRoot(AQuantity: TSquareKelvinQty): TKelvinQty;
function CubicPower(AQuantity: TKelvinQty): TCubicKelvinQty;
function CubicRoot(AQuantity: TCubicKelvinQty): TKelvinQty;
function SquarePower(AQuantity: TSquareKelvinQty): TQuarticKelvinQty;
function SquareRoot(AQuantity: TQuarticKelvinQty): TSquareKelvinQty;
function QuarticPower(AQuantity: TKelvinQty): TQuarticKelvinQty;
function QuarticRoot(AQuantity: TQuarticKelvinQty): TKelvinQty;
function SquarePower(AQuantity: TRadianQty): TSteradianQty;
function SquareRoot(AQuantity: TSteradianQty): TRadianQty;
function SquarePower(AQuantity: THertzQty): TSquareHertzQty;
function SquareRoot(AQuantity: TSquareHertzQty): THertzQty;
function SquarePower(AQuantity: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
function SquareRoot(AQuantity: TSquareMeterPerSquareSecondQty): TMeterPerSecondQty;
function SquarePower(AQuantity: TNewtonQty): TSquareNewtonQty;
function SquareRoot(AQuantity: TSquareNewtonQty): TNewtonQty;
function SquarePower(AQuantity: TCoulombQty): TSquareCoulombQty;
function SquareRoot(AQuantity: TSquareCoulombQty): TCoulombQty;
function SquarePower(AQuantity: TVoltQty): TSquareVoltQty;
function SquareRoot(AQuantity: TSquareVoltQty): TVoltQty;
function SquarePower(AQuantity: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
function SquareRoot(AQuantity: TSquareKilogramPerSquareSecondQty): TKilogramPerSecondQty;
function SquarePower(AQuantity: TJouleQty): TSquareJouleQty;
function SquareRoot(AQuantity: TSquareJouleQty): TJouleQty;

{ Trigonometric functions }

function Cos(const AQuantity: TRadianQty): double;
function Sin(const AQuantity: TRadianQty): double;
function Tan(const AQuantity: TRadianQty): double;
function Cotan(const AQuantity: TRadianQty): double;
function Secant(const AQuantity: TRadianQty): double;
function Cosecant(const AQuantity: TRadianQty): double;

function ArcCos(const AValue: double): TRadianQty;
function ArcSin(const AValue: double): TRadianQty;
function ArcTan(const AValue: double): TRadianQty;
function ArcTan2(const x, y: double): TRadianQty;

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

function GetSymbol(const ASymbol: string; const Prefixes: TPrefixes): string;
function GetName(const AName: string; const Prefixes: TPrefixes): string;

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

{ External operators }

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSecondUnit}{$i common.inc}

class function TSecondQty.Symbol: string;
begin result := '%ss' end;

class function TSecondQty.Name: string;
begin result := '%ssecond?' end;

class function TSecondQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSecondQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TDayQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TDayUnit}{$i common.inc}

class function TDayQty.Symbol: string;
begin result := 'd' end;
class function TDayQty.Name: string;
begin result := 'day?' end;
class function TDayQty.Prefixes: TPrefixes;
begin result := []; end;

class function TDayQty.Exponents: TExponents;
begin result := []; end;

class function TDayQty.ToBaseFactor: double;
begin result := 86400; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=THourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=THourUnit}{$i common.inc}

class function THourQty.Symbol: string;
begin result := 'h' end;
class function THourQty.Name: string;
begin result := 'hour?' end;
class function THourQty.Prefixes: TPrefixes;
begin result := []; end;

class function THourQty.Exponents: TExponents;
begin result := []; end;

class function THourQty.ToBaseFactor: double;
begin result := 3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMinuteQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMinuteUnit}{$i common.inc}

class function TMinuteQty.Symbol: string;
begin result := 'min' end;
class function TMinuteQty.Name: string;
begin result := 'minute?' end;
class function TMinuteQty.Prefixes: TPrefixes;
begin result := []; end;

class function TMinuteQty.Exponents: TExponents;
begin result := []; end;

class function TMinuteQty.ToBaseFactor: double;
begin result := 60; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareSecondUnit}{$i common.inc}

class function TSquareSecondQty.Symbol: string;
begin result := '%ss2' end;

class function TSquareSecondQty.Name: string;
begin result := 'square %ssecond?' end;

class function TSquareSecondQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareSecondQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ s2 ] = [ s ] * [ s ]

operator *(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondQty./(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareDayQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareDayUnit}{$i common.inc}

class function TSquareDayQty.Symbol: string;
begin result := 'd2' end;
class function TSquareDayQty.Name: string;
begin result := 'square day?' end;
class function TSquareDayQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareDayQty.Exponents: TExponents;
begin result := []; end;

class function TSquareDayQty.ToBaseFactor: double;
begin result := 7464960000; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareHourUnit}{$i common.inc}

class function TSquareHourQty.Symbol: string;
begin result := 'h2' end;
class function TSquareHourQty.Name: string;
begin result := 'square hour?' end;
class function TSquareHourQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareHourQty.Exponents: TExponents;
begin result := []; end;

class function TSquareHourQty.ToBaseFactor: double;
begin result := 12960000; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareMinuteQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMinuteUnit}{$i common.inc}

class function TSquareMinuteQty.Symbol: string;
begin result := 'min2' end;
class function TSquareMinuteQty.Name: string;
begin result := 'square minute?' end;
class function TSquareMinuteQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareMinuteQty.Exponents: TExponents;
begin result := []; end;

class function TSquareMinuteQty.ToBaseFactor: double;
begin result := 3600; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterUnit}{$i common.inc}

class function TMeterQty.Symbol: string;
begin result := '%sm' end;

class function TMeterQty.Name: string;
begin result := '%smeter?' end;

class function TMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TMeterQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TAstronomicalQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TAstronomicalUnit}{$i common.inc}

class function TAstronomicalQty.Symbol: string;
begin result := 'au' end;
class function TAstronomicalQty.Name: string;
begin result := 'astronomical unit?' end;
class function TAstronomicalQty.Prefixes: TPrefixes;
begin result := []; end;

class function TAstronomicalQty.Exponents: TExponents;
begin result := []; end;

class function TAstronomicalQty.ToBaseFactor: double;
begin result := 149597870691; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TInchUnit}{$i common.inc}

class function TInchQty.Symbol: string;
begin result := 'in' end;
class function TInchQty.Name: string;
begin result := 'inch!' end;
class function TInchQty.Prefixes: TPrefixes;
begin result := []; end;

class function TInchQty.Exponents: TExponents;
begin result := []; end;

class function TInchQty.ToBaseFactor: double;
begin result := 0.0254; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TFootQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TFootUnit}{$i common.inc}

class function TFootQty.Symbol: string;
begin result := 'ft' end;
class function TFootQty.Name: string;
begin result := 'foot!' end;
class function TFootQty.Prefixes: TPrefixes;
begin result := []; end;

class function TFootQty.Exponents: TExponents;
begin result := []; end;

class function TFootQty.ToBaseFactor: double;
begin result := 0.3048; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TYardQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TYardUnit}{$i common.inc}

class function TYardQty.Symbol: string;
begin result := 'yd' end;
class function TYardQty.Name: string;
begin result := 'yard?' end;
class function TYardQty.Prefixes: TPrefixes;
begin result := []; end;

class function TYardQty.Exponents: TExponents;
begin result := []; end;

class function TYardQty.ToBaseFactor: double;
begin result := 0.9144; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMileQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMileUnit}{$i common.inc}

class function TMileQty.Symbol: string;
begin result := 'mi' end;
class function TMileQty.Name: string;
begin result := 'mile?' end;
class function TMileQty.Prefixes: TPrefixes;
begin result := []; end;

class function TMileQty.Exponents: TExponents;
begin result := []; end;

class function TMileQty.ToBaseFactor: double;
begin result := 1609.344; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNauticalMileQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNauticalMileUnit}{$i common.inc}

class function TNauticalMileQty.Symbol: string;
begin result := 'nmi' end;
class function TNauticalMileQty.Name: string;
begin result := 'nautical mile?' end;
class function TNauticalMileQty.Prefixes: TPrefixes;
begin result := []; end;

class function TNauticalMileQty.Exponents: TExponents;
begin result := []; end;

class function TNauticalMileQty.ToBaseFactor: double;
begin result := 1852; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterUnit}{$i common.inc}

class function TSquareMeterQty.Symbol: string;
begin result := '%sm2' end;

class function TSquareMeterQty.Name: string;
begin result := 'square %smeter?' end;

class function TSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareMeterQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ m2 ] = [ m ] * [ m ]

operator *(const ALeft: TMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterQty./(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareInchUnit}{$i common.inc}

class function TSquareInchQty.Symbol: string;
begin result := 'in2' end;
class function TSquareInchQty.Name: string;
begin result := 'square inch!' end;
class function TSquareInchQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareInchQty.Exponents: TExponents;
begin result := []; end;

class function TSquareInchQty.ToBaseFactor: double;
begin result := 0.00064516; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareFootQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareFootUnit}{$i common.inc}

class function TSquareFootQty.Symbol: string;
begin result := 'ft2' end;
class function TSquareFootQty.Name: string;
begin result := 'square foot!' end;
class function TSquareFootQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareFootQty.Exponents: TExponents;
begin result := []; end;

class function TSquareFootQty.ToBaseFactor: double;
begin result := 0.09290304; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareYardQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareYardUnit}{$i common.inc}

class function TSquareYardQty.Symbol: string;
begin result := 'yd2' end;
class function TSquareYardQty.Name: string;
begin result := 'square yard?' end;
class function TSquareYardQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareYardQty.Exponents: TExponents;
begin result := []; end;

class function TSquareYardQty.ToBaseFactor: double;
begin result := 0.83612736; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareMileQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMileUnit}{$i common.inc}

class function TSquareMileQty.Symbol: string;
begin result := 'mi2' end;
class function TSquareMileQty.Name: string;
begin result := 'square mile?' end;
class function TSquareMileQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareMileQty.Exponents: TExponents;
begin result := []; end;

class function TSquareMileQty.ToBaseFactor: double;
begin result := 2589988.110336; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicMeterUnit}{$i common.inc}

class function TCubicMeterQty.Symbol: string;
begin result := '%sm3' end;

class function TCubicMeterQty.Name: string;
begin result := 'cubic %smeter?' end;

class function TCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TCubicMeterQty.Exponents: TExponents;
begin result := [3]; end;

// main definition [ m3 ] = [ m2 ] * [ m ]

operator *(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterQty./(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterQty./(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicInchUnit}{$i common.inc}

class function TCubicInchQty.Symbol: string;
begin result := 'in3' end;
class function TCubicInchQty.Name: string;
begin result := 'cubic inch!' end;
class function TCubicInchQty.Prefixes: TPrefixes;
begin result := []; end;

class function TCubicInchQty.Exponents: TExponents;
begin result := []; end;

class function TCubicInchQty.ToBaseFactor: double;
begin result := 0.000016387064; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicFootQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicFootUnit}{$i common.inc}

class function TCubicFootQty.Symbol: string;
begin result := 'ft3' end;
class function TCubicFootQty.Name: string;
begin result := 'cubic foot!' end;
class function TCubicFootQty.Prefixes: TPrefixes;
begin result := []; end;

class function TCubicFootQty.Exponents: TExponents;
begin result := []; end;

class function TCubicFootQty.ToBaseFactor: double;
begin result := 0.028316846592; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicYardQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicYardUnit}{$i common.inc}

class function TCubicYardQty.Symbol: string;
begin result := 'yd3' end;
class function TCubicYardQty.Name: string;
begin result := 'cubic yard?' end;
class function TCubicYardQty.Prefixes: TPrefixes;
begin result := []; end;

class function TCubicYardQty.Exponents: TExponents;
begin result := []; end;

class function TCubicYardQty.ToBaseFactor: double;
begin result := 0.764554857984; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TLitreQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TLitreUnit}{$i common.inc}

class function TLitreQty.Symbol: string;
begin result := '%sL' end;
class function TLitreQty.Name: string;
begin result := '%slitre?' end;
class function TLitreQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TLitreQty.Exponents: TExponents;
begin result := [1]; end;

class function TLitreQty.ToBaseFactor: double;
begin result := 1E-03; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TGallonQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TGallonUnit}{$i common.inc}

class function TGallonQty.Symbol: string;
begin result := 'gal' end;
class function TGallonQty.Name: string;
begin result := 'gallon?' end;
class function TGallonQty.Prefixes: TPrefixes;
begin result := []; end;

class function TGallonQty.Exponents: TExponents;
begin result := []; end;

class function TGallonQty.ToBaseFactor: double;
begin result := 0.0037854119678; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TQuarticMeterUnit}{$i common.inc}

class function TQuarticMeterQty.Symbol: string;
begin result := '%sm4' end;

class function TQuarticMeterQty.Name: string;
begin result := 'quartic %smeter?' end;

class function TQuarticMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TQuarticMeterQty.Exponents: TExponents;
begin result := [4]; end;

// main definition [ m4 ] = [ m3 ] * [ m ]

operator *(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TCubicMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuarticMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TQuarticMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]

operator *(const ALeft: TSquareMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuarticMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TQuinticMeterUnit}{$i common.inc}

class function TQuinticMeterQty.Symbol: string;
begin result := '%sm5' end;

class function TQuinticMeterQty.Name: string;
begin result := 'quintic %smeter?' end;

class function TQuinticMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TQuinticMeterQty.Exponents: TExponents;
begin result := [5]; end;

// main definition [ m5 ] = [ m4 ] * [ m ]

operator *(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TQuarticMeterQty): TQuinticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuinticMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TQuinticMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]

operator *(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TQuinticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TCubicMeterQty): TQuinticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuinticMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TQuinticMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSexticMeterUnit}{$i common.inc}

class function TSexticMeterQty.Symbol: string;
begin result := '%sm6' end;

class function TSexticMeterQty.Name: string;
begin result := 'sextic %smeter?' end;

class function TSexticMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSexticMeterQty.Exponents: TExponents;
begin result := [6]; end;

// main definition [ m6 ] = [ m5 ] * [ m ]

operator *(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TSexticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TQuinticMeterQty): TSexticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSexticMeterQty./(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSexticMeterQty./(const ALeft: TSexticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]

operator *(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSexticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticMeterQty): TSexticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSexticMeterQty./(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSexticMeterQty./(const ALeft: TSexticMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]

operator *(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSexticMeterQty./(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramUnit}{$i common.inc}

class function TKilogramQty.Symbol: string;
begin result := '%sg' end;

class function TKilogramQty.Name: string;
begin result := '%sgram?' end;

class function TKilogramQty.Prefixes: TPrefixes;
begin result := [pKilo]; end;

class function TKilogramQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TTonneQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTonneUnit}{$i common.inc}

class function TTonneQty.Symbol: string;
begin result := '%st' end;
class function TTonneQty.Name: string;
begin result := '%stonne?' end;
class function TTonneQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TTonneQty.Exponents: TExponents;
begin result := [1]; end;

class function TTonneQty.ToBaseFactor: double;
begin result := 1E+03; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoundUnit}{$i common.inc}

class function TPoundQty.Symbol: string;
begin result := 'lb' end;
class function TPoundQty.Name: string;
begin result := 'pound?' end;
class function TPoundQty.Prefixes: TPrefixes;
begin result := []; end;

class function TPoundQty.Exponents: TExponents;
begin result := []; end;

class function TPoundQty.ToBaseFactor: double;
begin result := 0.45359237; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TOunceQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TOunceUnit}{$i common.inc}

class function TOunceQty.Symbol: string;
begin result := 'oz' end;
class function TOunceQty.Name: string;
begin result := 'ounce?' end;
class function TOunceQty.Prefixes: TPrefixes;
begin result := []; end;

class function TOunceQty.Exponents: TExponents;
begin result := []; end;

class function TOunceQty.ToBaseFactor: double;
begin result := 0.028349523125; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TStoneQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TStoneUnit}{$i common.inc}

class function TStoneQty.Symbol: string;
begin result := 'st' end;
class function TStoneQty.Name: string;
begin result := 'stone?' end;
class function TStoneQty.Prefixes: TPrefixes;
begin result := []; end;

class function TStoneQty.Exponents: TExponents;
begin result := []; end;

class function TStoneQty.ToBaseFactor: double;
begin result := 6.35029318; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TTonQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTonUnit}{$i common.inc}

class function TTonQty.Symbol: string;
begin result := 'ton' end;
class function TTonQty.Name: string;
begin result := 'ton?' end;
class function TTonQty.Prefixes: TPrefixes;
begin result := []; end;

class function TTonQty.Exponents: TExponents;
begin result := []; end;

class function TTonQty.ToBaseFactor: double;
begin result := 907.18474; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareKilogramUnit}{$i common.inc}

class function TSquareKilogramQty.Symbol: string;
begin result := '%sg2' end;

class function TSquareKilogramQty.Name: string;
begin result := 'square %sgram?' end;

class function TSquareKilogramQty.Prefixes: TPrefixes;
begin result := [pKilo]; end;

class function TSquareKilogramQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ kg2 ] = [ kg ] * [ kg ]

operator *(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramQty./(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TAmpereUnit}{$i common.inc}

class function TAmpereQty.Symbol: string;
begin result := '%sA' end;

class function TAmpereQty.Name: string;
begin result := '%sampere?' end;

class function TAmpereQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TAmpereQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareAmpereUnit}{$i common.inc}

class function TSquareAmpereQty.Symbol: string;
begin result := '%sA2' end;

class function TSquareAmpereQty.Name: string;
begin result := 'square %sampere?' end;

class function TSquareAmpereQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareAmpereQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ A2 ] = [ A ] * [ A ]

operator *(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareAmpereQty./(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKelvinUnit}{$i common.inc}

class function TKelvinQty.Symbol: string;
begin result := '%sK' end;

class function TKelvinQty.Name: string;
begin result := '%skelvin?' end;

class function TKelvinQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TKelvinQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i common.inc}

class function TDegreeCelsiusQty.Symbol: string;
begin result := 'ºC' end;
class function TDegreeCelsiusQty.Name: string;
begin result := 'degree? Celsius' end;
class function TDegreeCelsiusQty.Prefixes: TPrefixes;
begin result := []; end;

class function TDegreeCelsiusQty.Exponents: TExponents;
begin result := []; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i common.inc}

class function TDegreeFahrenheitQty.Symbol: string;
begin result := 'ºF' end;
class function TDegreeFahrenheitQty.Name: string;
begin result := 'degree? Fahrenheit' end;
class function TDegreeFahrenheitQty.Prefixes: TPrefixes;
begin result := []; end;

class function TDegreeFahrenheitQty.Exponents: TExponents;
begin result := []; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareKelvinUnit}{$i common.inc}

class function TSquareKelvinQty.Symbol: string;
begin result := '%sK2' end;

class function TSquareKelvinQty.Name: string;
begin result := 'square %skelvin?' end;

class function TSquareKelvinQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareKelvinQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ K2 ] = [ K ] * [ K ]

operator *(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKelvinQty./(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicKelvinUnit}{$i common.inc}

class function TCubicKelvinQty.Symbol: string;
begin result := '%sK3' end;

class function TCubicKelvinQty.Name: string;
begin result := 'cubic %skelvin?' end;

class function TCubicKelvinQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TCubicKelvinQty.Exponents: TExponents;
begin result := [3]; end;

// main definition [ K3 ] = [ K2 ] * [ K ]

operator *(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicKelvinQty./(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicKelvinQty./(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i common.inc}

class function TQuarticKelvinQty.Symbol: string;
begin result := '%sK4' end;

class function TQuarticKelvinQty.Name: string;
begin result := 'quartic %skelvin?' end;

class function TQuarticKelvinQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TQuarticKelvinQty.Exponents: TExponents;
begin result := [4]; end;

// main definition [ K4 ] = [ K3 ] * [ K ]

operator *(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuarticKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TQuarticKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]

operator *(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuarticKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMoleUnit}{$i common.inc}

class function TMoleQty.Symbol: string;
begin result := '%smol' end;

class function TMoleQty.Name: string;
begin result := '%smole?' end;

class function TMoleQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TMoleQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCandelaUnit}{$i common.inc}

class function TCandelaQty.Symbol: string;
begin result := '%scd' end;

class function TCandelaQty.Name: string;
begin result := '%scandela?' end;

class function TCandelaQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TCandelaQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TRadianUnit}{$i common.inc}

class function TRadianQty.Symbol: string;
begin result := 'rad' end;

class function TRadianQty.Name: string;
begin result := 'radian?' end;

class function TRadianQty.Prefixes: TPrefixes;
begin result := []; end;

class function TRadianQty.Exponents: TExponents;
begin result := []; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TDegreeQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TDegreeUnit}{$i common.inc}

class function TDegreeQty.Symbol: string;
begin result := 'deg' end;
class function TDegreeQty.Name: string;
begin result := 'degree?' end;
class function TDegreeQty.Prefixes: TPrefixes;
begin result := []; end;

class function TDegreeQty.Exponents: TExponents;
begin result := []; end;

class function TDegreeQty.ToBaseFactor: double;
begin result := Pi/180; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSteradianUnit}{$i common.inc}

class function TSteradianQty.Symbol: string;
begin result := 'sr' end;

class function TSteradianQty.Name: string;
begin result := 'steradian?' end;

class function TSteradianQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSteradianQty.Exponents: TExponents;
begin result := []; end;

// main definition [ sr ] = [ rad ] * [ rad ]

operator *(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSteradianQty./(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareDegreeQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareDegreeUnit}{$i common.inc}

class function TSquareDegreeQty.Symbol: string;
begin result := 'deg2' end;
class function TSquareDegreeQty.Name: string;
begin result := 'square degree?' end;
class function TSquareDegreeQty.Prefixes: TPrefixes;
begin result := []; end;

class function TSquareDegreeQty.Exponents: TExponents;
begin result := []; end;

class function TSquareDegreeQty.ToBaseFactor: double;
begin result := Pi*Pi/32400; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=THertzUnit}{$i common.inc}

class function THertzQty.Symbol: string;
begin result := '%sHz' end;

class function THertzQty.Name: string;
begin result := '%shertz' end;

class function THertzQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function THertzQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ Hz ] = [ 1 ] / [ s ]

operator /(const ALeft: double; const ARight: TSecondQty): THertzQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator THertzQty.*(const ALeft: TSecondQty; const ARight: THertzQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator THertzQty.*(const ALeft: THertzQty; const ARight: TSecondQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator THertzQty./(const ALeft: double; const ARight: THertzQty): TSecondQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSecondUnit): THertzQty;
begin result.FValue := ALeft; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareHertzUnit}{$i common.inc}

class function TSquareHertzQty.Symbol: string;
begin result := '%sHz2' end;

class function TSquareHertzQty.Name: string;
begin result := 'square %shertz' end;

class function TSquareHertzQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareHertzQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]

operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TSquareHertzQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareHertzQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TSquareSecondQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSquareHertzQty./(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSquareSecondUnit): TSquareHertzQty;
begin result.FValue := ALeft; end;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]

operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareHertzQty.*(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareHertzQty./(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]

operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareHertzQty./(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i common.inc}

class function TRadianPerSecondQty.Symbol: string;
begin result := 'rad/%ss' end;

class function TRadianPerSecondQty.Name: string;
begin result := 'radian? per %ssecond' end;

class function TRadianPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TRadianPerSecondQty.Exponents: TExponents;
begin result := [-1]; end;

// main definition [ rad/s ] = [ rad ] / [ s ]

operator /(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerSecondQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondQty.*(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondQty./(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]

operator *(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerSecondQty./(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator :=(const AQuantity: TRadianPerSecondQty): THertzQty; inline;
begin result.FValue := AQuantity.FValue; end;

operator :=(const AQuantity: THertzQty): TRadianPerSecondQty; inline;
begin result.FValue := AQuantity.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TRadianPerSecondSquaredUnit}{$i common.inc}

class function TRadianPerSecondSquaredQty.Symbol: string;
begin result := 'rad/%ss2' end;

class function TRadianPerSecondSquaredQty.Name: string;
begin result := 'radian? per %ssecond squared' end;

class function TRadianPerSecondSquaredQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TRadianPerSecondSquaredQty.Exponents: TExponents;
begin result := [-2]; end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]

operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSecondSquaredQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s2 ] = [ rad ] / [ s2 ]

operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TSquareSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareSecondQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianQty; const ARight: TRadianPerSecondSquaredQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]

operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSecondSquaredQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondSquaredQty; const ARight: TRadianQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareHertzQty): TRadianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i common.inc}

class function TSteradianPerSquareSecondQty.Symbol: string;
begin result := 'rad2/%ss2' end;

class function TSteradianPerSquareSecondQty.Name: string;
begin result := 'square radian? per square %ssecond' end;

class function TSteradianPerSquareSecondQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSteradianPerSquareSecondQty.Exponents: TExponents;
begin result := [-2]; end;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]

operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondUnit): TSteradianPerSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]

operator *(const ALeft: TSteradianQty; const ARight: TSquareHertzQty): TSteradianPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertzQty; const ARight: TSteradianQty): TSteradianPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i common.inc}

class function TMeterPerSecondQty.Symbol: string;
begin result := '%sm/%ss' end;

class function TMeterPerSecondQty.Name: string;
begin result := '%smeter? per %ssecond' end;

class function TMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMeterPerSecondQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ m/s ] = [ m ] / [ s ]

operator /(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondQty./(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeterQty; const ARight: TSecondUnit): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s ] = [ m ] * [ Hz ]

operator *(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerSecondQty./(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: THertzUnit): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMeterPerHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerHourUnit}{$i common.inc}

class function TMeterPerHourQty.Symbol: string;
begin result := '%sm/h' end;
class function TMeterPerHourQty.Name: string;
begin result := '%smeter? per hour' end;
class function TMeterPerHourQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TMeterPerHourQty.Exponents: TExponents;
begin result := [1]; end;

class function TMeterPerHourQty.ToBaseFactor: double;
begin result := 1/3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMilePerHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMilePerHourUnit}{$i common.inc}

class function TMilePerHourQty.Symbol: string;
begin result := 'mi/h' end;
class function TMilePerHourQty.Name: string;
begin result := 'mile? per hour' end;
class function TMilePerHourQty.Prefixes: TPrefixes;
begin result := []; end;

class function TMilePerHourQty.Exponents: TExponents;
begin result := []; end;

class function TMilePerHourQty.ToBaseFactor: double;
begin result := 0.44704; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i common.inc}

class function TNauticalMilePerHourQty.Symbol: string;
begin result := 'nmi/h' end;
class function TNauticalMilePerHourQty.Name: string;
begin result := 'nautical mile? per hour' end;
class function TNauticalMilePerHourQty.Prefixes: TPrefixes;
begin result := []; end;

class function TNauticalMilePerHourQty.Exponents: TExponents;
begin result := []; end;

class function TNauticalMilePerHourQty.ToBaseFactor: double;
begin result := 463/900; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerSecondSquaredUnit}{$i common.inc}

class function TMeterPerSecondSquaredQty.Symbol: string;
begin result := '%sm/%ss2' end;

class function TMeterPerSecondSquaredQty.Name: string;
begin result := '%smeter? per %ssecond squared' end;

class function TMeterPerSecondSquaredQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMeterPerSecondSquaredQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ m/s2 ] = [ m/s ] / [ s ]

operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondSquaredQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]

operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareSecondQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]

operator *(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareHertzQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i common.inc}

class function TMeterPerSecondPerSecondQty.Symbol: string;
begin result := '%sm/%ss/%ss' end;

class function TMeterPerSecondPerSecondQty.Name: string;
begin result := '%smeter? per %ssecond per %ssecond' end;

class function TMeterPerSecondPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TMeterPerSecondPerSecondQty.Exponents: TExponents;
begin result := [1, -1, -1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i common.inc}

class function TMeterPerHourPerSecondQty.Symbol: string;
begin result := '%sm/h/%ss' end;
class function TMeterPerHourPerSecondQty.Name: string;
begin result := '%smeter? per hour per %ssecond' end;
class function TMeterPerHourPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMeterPerHourPerSecondQty.Exponents: TExponents;
begin result := [1, -1]; end;

class function TMeterPerHourPerSecondQty.ToBaseFactor: double;
begin result := 1/3600; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i common.inc}

class function TSquareMeterPerSquareSecondQty.Symbol: string;
begin result := '%sm2/%ss2' end;

class function TSquareMeterPerSquareSecondQty.Name: string;
begin result := 'square %smeter? per square %ssecond' end;

class function TSquareMeterPerSquareSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareMeterPerSquareSecondQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]

operator *(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]

operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i common.inc}

class function TKilogramMeterPerSecondQty.Symbol: string;
begin result := '%sg.%sm/%ss' end;

class function TKilogramMeterPerSecondQty.Name: string;
begin result := '%sgram %smeter? per %ssecond' end;

class function TKilogramMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone, pNone]; end;

class function TKilogramMeterPerSecondQty.Exponents: TExponents;
begin result := [1, 1, -1]; end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]

operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondUnit): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonSecondUnit}{$i common.inc}

class function TNewtonSecondQty.Symbol: string;
begin result := '%sN.%ss' end;

class function TNewtonSecondQty.Name: string;
begin result := '%snewton %ssecond?' end;

class function TNewtonSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonSecondQty.Exponents: TExponents;
begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i common.inc}

class function TKilogramSquareMeterQty.Symbol: string;
begin result := '%sg.%sm2' end;

class function TKilogramSquareMeterQty.Name: string;
begin result := '%sgram square %smeter?' end;

class function TKilogramSquareMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramSquareMeterQty.Exponents: TExponents;
begin result := [1, 2]; end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]

operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareMeterQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramSquareMeterQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i common.inc}

class function TKilogramSquareMeterPerSecondQty.Symbol: string;
begin result := '%sg.%sm2/%ss' end;

class function TKilogramSquareMeterPerSecondQty.Name: string;
begin result := '%sgram square %smeter? per %ssecond' end;

class function TKilogramSquareMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone, pNone]; end;

class function TKilogramSquareMeterPerSecondQty.Exponents: TExponents;
begin result := [1, 2, -1]; end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]

operator *(const ALeft: TKilogramSquareMeterQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg*m2/s ] = [ kg*m/s ] * [ m ]

operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i common.inc}

class function TKilogramPerMeterQty.Symbol: string;
begin result := '%sg/%sm' end;

class function TKilogramPerMeterQty.Name: string;
begin result := '%sgram? per %smeter' end;

class function TKilogramPerMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ kg/m ] = [ kg ] / [ m ]

operator /(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerMeterQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i common.inc}

class function TKilogramPerSquareMeterQty.Symbol: string;
begin result := '%sg/%sm2' end;

class function TKilogramPerSquareMeterQty.Name: string;
begin result := '%sgram? per square %smeter' end;

class function TKilogramPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramPerSquareMeterQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]

operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSquareMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i common.inc}

class function TKilogramPerCubicMeterQty.Symbol: string;
begin result := '%sg/%sm3' end;

class function TKilogramPerCubicMeterQty.Name: string;
begin result := '%sgram? per cubic %smeter' end;

class function TKilogramPerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramPerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]

operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterUnit): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]

operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonUnit}{$i common.inc}

class function TNewtonQty.Symbol: string;
begin result := '%sN' end;

class function TNewtonQty.Name: string;
begin result := '%snewton?' end;

class function TNewtonQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TNewtonQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ N ] = [ kg ] * [ m/s2 ]

operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonQty./(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonQty./(const ALeft: TNewtonQty; const ARight: TMeterPerSecondSquaredQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredUnit): TNewtonQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]

operator *(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonQty./(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonQty./(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]

operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonQty.*(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonQty.*(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForceQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoundForceUnit}{$i common.inc}

class function TPoundForceQty.Symbol: string;
begin result := 'lbf' end;
class function TPoundForceQty.Name: string;
begin result := 'pound?-force' end;
class function TPoundForceQty.Prefixes: TPrefixes;
begin result := []; end;

class function TPoundForceQty.Exponents: TExponents;
begin result := []; end;

class function TPoundForceQty.ToBaseFactor: double;
begin result := 4.4482216152605; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareNewtonUnit}{$i common.inc}

class function TSquareNewtonQty.Symbol: string;
begin result := '%sN2' end;

class function TSquareNewtonQty.Name: string;
begin result := 'square %snewton?' end;

class function TSquareNewtonQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareNewtonQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ N2 ] = [ N ] * [ N ]

operator *(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareNewtonQty./(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPascalUnit}{$i common.inc}

class function TPascalQty.Symbol: string;
begin result := '%sPa' end;

class function TPascalQty.Name: string;
begin result := '%spascal?' end;

class function TPascalQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TPascalQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ Pa ] = [ N ] / [ m2 ]

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TPascalQty.*(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPascalQty./(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]

operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPascalQty./(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TPascalQty./(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TBarQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TBarUnit}{$i common.inc}

class function TBarQty.Symbol: string;
begin result := '%sbar' end;
class function TBarQty.Name: string;
begin result := '%sbar?' end;
class function TBarQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TBarQty.Exponents: TExponents;
begin result := [1]; end;

class function TBarQty.ToBaseFactor: double;
begin result := 1E+05; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i common.inc}

class function TPoundPerSquareInchQty.Symbol: string;
begin result := '%spsi' end;
class function TPoundPerSquareInchQty.Name: string;
begin result := '%spound? per square inch' end;
class function TPoundPerSquareInchQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TPoundPerSquareInchQty.Exponents: TExponents;
begin result := [1]; end;

class function TPoundPerSquareInchQty.ToBaseFactor: double;
begin result := 6894.75729316836; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i common.inc}

class function TJoulePerCubicMeterQty.Symbol: string;
begin result := '%sJ/%sm3' end;

class function TJoulePerCubicMeterQty.Name: string;
begin result := '%sjoule? per cubic %smeter' end;

class function TJoulePerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TJoulePerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJouleUnit}{$i common.inc}

class function TJouleQty.Symbol: string;
begin result := '%sJ' end;

class function TJouleQty.Name: string;
begin result := '%sjoule?' end;

class function TJouleQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TJouleQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ J ] = [ N ] * [ m ]

operator *(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonQty; const ARight: TMeterUnit): TJouleQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]

operator *(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]

operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]

operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramUnit): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]

operator *(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty.*(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty.*(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg*m2/s ] / [ s ]

operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty.*(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty.*(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TJouleQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J ] = [ kg*m2/s ] * [ Hz ]

operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJouleQty./(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TWattHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattHourUnit}{$i common.inc}

class function TWattHourQty.Symbol: string;
begin result := '%sW.h' end;
class function TWattHourQty.Name: string;
begin result := '%swatt hour?' end;
class function TWattHourQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TWattHourQty.Exponents: TExponents;
begin result := [1]; end;

class function TWattHourQty.ToBaseFactor: double;
begin result := 3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TElettronvoltQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TElettronvoltUnit}{$i common.inc}

class function TElettronvoltQty.Symbol: string;
begin result := '%seV' end;
class function TElettronvoltQty.Name: string;
begin result := '%selettronvolt?' end;
class function TElettronvoltQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TElettronvoltQty.Exponents: TExponents;
begin result := [1]; end;

class function TElettronvoltQty.ToBaseFactor: double;
begin result := 1.60217742320523E-019; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonMeterUnit}{$i common.inc}

class function TNewtonMeterQty.Symbol: string;
begin result := '%sN.%sm' end;

class function TNewtonMeterQty.Name: string;
begin result := '%snewton %smeter?' end;

class function TNewtonMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonMeterQty.Exponents: TExponents;
begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForceInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoundForceInchUnit}{$i common.inc}

class function TPoundForceInchQty.Symbol: string;
begin result := 'lbf.in' end;
class function TPoundForceInchQty.Name: string;
begin result := 'pound-force inch!' end;
class function TPoundForceInchQty.Prefixes: TPrefixes;
begin result := []; end;

class function TPoundForceInchQty.Exponents: TExponents;
begin result := []; end;

class function TPoundForceInchQty.ToBaseFactor: double;
begin result := 0.112984829027617; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattUnit}{$i common.inc}

class function TWattQty.Symbol: string;
begin result := '%sW' end;

class function TWattQty.Name: string;
begin result := '%swatt?' end;

class function TWattQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TWattQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ W ] = [ J ] / [ s ]

operator /(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattQty.*(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattQty.*(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattQty./(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W ] = [ J ] * [ Hz ]

operator *(const ALeft: TJouleQty; const ARight: THertzQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertzQty; const ARight: TJouleQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W ] = [ N ] * [ m/s ]

operator *(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombUnit}{$i common.inc}

class function TCoulombQty.Symbol: string;
begin result := '%sC' end;

class function TCoulombQty.Name: string;
begin result := '%scoulomb?' end;

class function TCoulombQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TCoulombQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ C ] = [ s ] * [ A ]

operator *(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombQty./(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombQty./(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TAmpereHourQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TAmpereHourUnit}{$i common.inc}

class function TAmpereHourQty.Symbol: string;
begin result := '%sA.h' end;
class function TAmpereHourQty.Name: string;
begin result := '%sampere hour?' end;
class function TAmpereHourQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TAmpereHourQty.Exponents: TExponents;
begin result := [1]; end;

class function TAmpereHourQty.ToBaseFactor: double;
begin result := 3600; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareCoulombUnit}{$i common.inc}

class function TSquareCoulombQty.Symbol: string;
begin result := '%sC2' end;

class function TSquareCoulombQty.Name: string;
begin result := 'square %scoulomb?' end;

class function TSquareCoulombQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareCoulombQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ C2 ] = [ C ] * [ C ]

operator *(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareCoulombQty./(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TVoltUnit}{$i common.inc}

class function TVoltQty.Symbol: string;
begin result := '%sV' end;

class function TVoltQty.Name: string;
begin result := '%svolt?' end;

class function TVoltQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TVoltQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ V ] = [ W ] / [ A ]

operator /(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltQty.*(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltQty./(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TAmpereUnit): TVoltQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V ] = [ J ] / [ C ]

operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltQty.*(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltQty./(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TCoulombUnit): TVoltQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareVoltUnit}{$i common.inc}

class function TSquareVoltQty.Symbol: string;
begin result := '%sV2' end;

class function TSquareVoltQty.Name: string;
begin result := 'square %svolt?' end;

class function TSquareVoltQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareVoltQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ V2 ] = [ V ] * [ V ]

operator *(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareVoltQty./(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TFaradUnit}{$i common.inc}

class function TFaradQty.Symbol: string;
begin result := '%sF' end;

class function TFaradQty.Name: string;
begin result := '%sfarad?' end;

class function TFaradQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TFaradQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ F ] = [ C ] / [ V ]

operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TFaradQty.*(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradQty./(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TVoltUnit): TFaradQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ F ] = [ C2 ] / [ J ]

operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TFaradQty.*(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradQty./(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TOhmUnit}{$i common.inc}

class function TOhmQty.Symbol: string;
begin result := '%sΩ' end;

class function TOhmQty.Name: string;
begin result := '%sohm?' end;

class function TOhmQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TOhmQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ Ω ] = [ V ] / [ A ]

operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty./(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltQty; const ARight: TAmpereUnit): TOhmQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Ω ] = [ s ] / [ F ]

operator /(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty./(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Ω ] = [ W ] / [ A2 ]

operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty./(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Ω ] = [ V2 ] / [ W ]

operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmQty./(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSiemensUnit}{$i common.inc}

class function TSiemensQty.Symbol: string;
begin result := '%sS' end;

class function TSiemensQty.Name: string;
begin result := '%ssiemens' end;

class function TSiemensQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSiemensQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ S ] = 1 / [ Ω ]

operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TSiemensQty.*(const ALeft: TOhmQty; const ARight: TSiemensQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSiemensQty.*(const ALeft: TSiemensQty; const ARight: TOhmQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSiemensQty./(const ALeft: double; const ARight: TSiemensQty): TOhmQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TOhmUnit): TSiemensQty;
begin result.FValue := ALeft; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWeberUnit}{$i common.inc}

class function TWeberQty.Symbol: string;
begin result := '%sWb' end;

class function TWeberQty.Name: string;
begin result := '%sweber?' end;

class function TWeberQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TWeberQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ Wb ] = [ V ] * [ s ]

operator *(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWeberQty./(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWeberQty./(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTeslaUnit}{$i common.inc}

class function TTeslaQty.Symbol: string;
begin result := '%sT' end;

class function TTeslaQty.Name: string;
begin result := '%stesla?' end;

class function TTeslaQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TTeslaQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ T ] = [ Wb ] / [ m2 ]

operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TTeslaQty.*(const ALeft: TSquareMeterQty; const ARight: TTeslaQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaQty.*(const ALeft: TTeslaQty; const ARight: TSquareMeterQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaQty./(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWeberQty; const ARight: TSquareMeterUnit): TTeslaQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=THenryUnit}{$i common.inc}

class function THenryQty.Symbol: string;
begin result := '%sH' end;

class function THenryQty.Name: string;
begin result := '%shenry!' end;

class function THenryQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function THenryQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ H ] = [ Wb ] / [ A ]

operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryQty.*(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryQty./(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWeberQty; const ARight: TAmpereUnit): THenryQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H ] = [ Ω ] * [ s ]

operator *(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryQty./(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryQty./(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]

operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryQty.*(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryQty./(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TLumenUnit}{$i common.inc}

class function TLumenQty.Symbol: string;
begin result := '%slm' end;

class function TLumenQty.Name: string;
begin result := '%slumen?' end;

class function TLumenQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TLumenQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ lm ] = [ cd ] * [ sr ]

operator *(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TLumenQty./(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TLumenQty./(const ALeft: TLumenQty; const ARight: TSteradianQty): TCandelaQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCandelaQty; const ARight: TSteradianUnit): TLumenQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TLuxUnit}{$i common.inc}

class function TLuxQty.Symbol: string;
begin result := '%slx' end;

class function TLuxQty.Name: string;
begin result := '%slux' end;

class function TLuxQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TLuxQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ lx ] = [ lm ] / [ m2 ]

operator /(const ALeft: TLumenQty; const ARight: TSquareMeterQty): TLuxQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TLuxQty.*(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TLuxQty.*(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TLuxQty./(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TLumenQty; const ARight: TSquareMeterUnit): TLuxQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TBequerelUnit}{$i common.inc}

class function TBequerelQty.Symbol: string;
begin result := '%sBq' end;

class function TBequerelQty.Name: string;
begin result := '%sbequerel?' end;

class function TBequerelQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TBequerelQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TGrayUnit}{$i common.inc}

class function TGrayQty.Symbol: string;
begin result := '%sGy' end;

class function TGrayQty.Name: string;
begin result := '%sgray?' end;

class function TGrayQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TGrayQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSievertUnit}{$i common.inc}

class function TSievertQty.Symbol: string;
begin result := '%sSv' end;

class function TSievertQty.Name: string;
begin result := '%ssievert?' end;

class function TSievertQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSievertQty.Exponents: TExponents;
begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKatalUnit}{$i common.inc}

class function TKatalQty.Symbol: string;
begin result := '%skat' end;

class function TKatalQty.Name: string;
begin result := '%skatal?' end;

class function TKatalQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TKatalQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ kat ] = [ mol ] / [ s ]

operator /(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKatalQty.*(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKatalQty.*(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKatalQty./(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i common.inc}

class function TJoulePerRadianQty.Symbol: string;
begin result := '%sJ/rad' end;

class function TJoulePerRadianQty.Name: string;
begin result := '%sjoule? per radian' end;

class function TJoulePerRadianQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TJoulePerRadianQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ J/rad ] = [ J ] / [ rad ]

operator /(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerRadianQty.*(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerRadianQty.*(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerRadianQty./(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i common.inc}

class function TJoulePerDegreeQty.Symbol: string;
begin result := '%sJ/deg' end;
class function TJoulePerDegreeQty.Name: string;
begin result := '%sjoule? per degree' end;
class function TJoulePerDegreeQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TJoulePerDegreeQty.Exponents: TExponents;
begin result := [1]; end;

class function TJoulePerDegreeQty.ToBaseFactor: double;
begin result := 180/Pi; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i common.inc}

class function TNewtonMeterPerRadianQty.Symbol: string;
begin result := '%sN.%sm/rad' end;

class function TNewtonMeterPerRadianQty.Name: string;
begin result := '%snewton %smeter? per radian' end;

class function TNewtonMeterPerRadianQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonMeterPerRadianQty.Exponents: TExponents;
begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i common.inc}

class function TNewtonMeterPerDegreeQty.Symbol: string;
begin result := '%sN.%sm/deg' end;
class function TNewtonMeterPerDegreeQty.Name: string;
begin result := '%snewton %smeter? per degree' end;
class function TNewtonMeterPerDegreeQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonMeterPerDegreeQty.Exponents: TExponents;
begin result := [1, 1]; end;

class function TNewtonMeterPerDegreeQty.ToBaseFactor: double;
begin result := 180/Pi; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i common.inc}

class function TNewtonPerCubicMeterQty.Symbol: string;
begin result := '%sN/%sm3' end;

class function TNewtonPerCubicMeterQty.Name: string;
begin result := '%snewton? per cubic %smeter' end;

class function TNewtonPerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]

operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]

operator /(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]

operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TNewtonPerCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i common.inc}

class function TNewtonPerMeterQty.Symbol: string;
begin result := '%sN/%sm' end;

class function TNewtonPerMeterQty.Name: string;
begin result := '%snewton? per %smeter' end;

class function TNewtonPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ N/m ] = [ N ] / [ m ]

operator /(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TMeterUnit): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]

operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]

operator *(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]

operator *(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i common.inc}

class function TPoundForcePerInchQty.Symbol: string;
begin result := 'lbf/in' end;
class function TPoundForcePerInchQty.Name: string;
begin result := 'pound?-force per inch' end;
class function TPoundForcePerInchQty.Prefixes: TPrefixes;
begin result := []; end;

class function TPoundForcePerInchQty.Exponents: TExponents;
begin result := []; end;

class function TPoundForcePerInchQty.ToBaseFactor: double;
begin result := 175.126835246476; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i common.inc}

class function TCubicMeterPerSecondQty.Symbol: string;
begin result := '%sm3/%ss' end;

class function TCubicMeterPerSecondQty.Name: string;
begin result := 'cubic %smeter? per %ssecond' end;

class function TCubicMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCubicMeterPerSecondQty.Exponents: TExponents;
begin result := [3, -1]; end;

// main definition [ m3/s ] = [ m3 ] / [ s ]

operator /(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]

operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i common.inc}

class function TKilogramPerSecondQty.Symbol: string;
begin result := '%sg/%ss' end;

class function TKilogramPerSecondQty.Name: string;
begin result := '%sgram? per %ssecond' end;

class function TKilogramPerSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramPerSecondQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ kg/s ] = [ kg ] / [ s ]

operator /(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]

operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty./(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/s ] = [ kg/m ] * [ m/s ]

operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/s ] = [ W ] / [ m2/s2 ]

operator /(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerSecondQty./(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPoiseuilleUnit}{$i common.inc}

class function TPoiseuilleQty.Symbol: string;
begin result := '%sPl' end;

class function TPoiseuilleQty.Name: string;
begin result := '%spoiseuille?' end;

class function TPoiseuilleQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TPoiseuilleQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ Pl ] = [ Pa ] * [ s ]

operator *(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPoiseuilleQty./(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TPoiseuilleQty./(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TPascalQty; const ARight: TSecondUnit): TPoiseuilleQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]

operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPoiseuilleQty./(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TPoiseuilleQty./(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TPoiseuilleQty.*(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPoiseuilleQty.*(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TPoiseuilleQty./(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterUnit): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TPascalSecondUnit}{$i common.inc}

class function TPascalSecondQty.Symbol: string;
begin result := '%sPa.%ss' end;

class function TPascalSecondQty.Name: string;
begin result := '%spascal %ssecond?' end;

class function TPascalSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TPascalSecondQty.Exponents: TExponents;
begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i common.inc}

class function TSquareMeterPerSecondQty.Symbol: string;
begin result := '%sm2/%ss' end;

class function TSquareMeterPerSecondQty.Name: string;
begin result := 'square %smeter? per %ssecond' end;

class function TSquareMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareMeterPerSecondQty.Exponents: TExponents;
begin result := [2, -1]; end;

// main definition [ m2/s ] = [ m2 ] / [ s ]

operator /(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSecondUnit): TSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]

operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSecondQty./(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i common.inc}

class function TKilogramPerQuarticMeterQty.Symbol: string;
begin result := '%sg/%sm4' end;

class function TKilogramPerQuarticMeterQty.Name: string;
begin result := '%sgram? per quartic %smeter' end;

class function TKilogramPerQuarticMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramPerQuarticMeterQty.Exponents: TExponents;
begin result := [1, -4]; end;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i common.inc}

class function TQuarticMeterSecondQty.Symbol: string;
begin result := '%sm4.%ss' end;

class function TQuarticMeterSecondQty.Name: string;
begin result := 'quartic %smeter %ssecond?' end;

class function TQuarticMeterSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TQuarticMeterSecondQty.Exponents: TExponents;
begin result := [4, 1]; end;

// main definition [ m4*s ] = [ m4 ] * [ s ]

operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TQuarticMeterSecondQty./(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TQuarticMeterSecondQty./(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondUnit): TQuarticMeterSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i common.inc}

class function TKilogramPerQuarticMeterPerSecondQty.Symbol: string;
begin result := '%sg/%sm4/%ss' end;

class function TKilogramPerQuarticMeterPerSecondQty.Name: string;
begin result := '%sgram? per quartic %smeter per %ssecond' end;

class function TKilogramPerQuarticMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone, pNone]; end;

class function TKilogramPerQuarticMeterPerSecondQty.Exponents: TExponents;
begin result := [1, -4, -1]; end;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]

operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondUnit): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]

operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i common.inc}

class function TCubicMeterPerKilogramQty.Symbol: string;
begin result := '%sm3/%sg' end;

class function TCubicMeterPerKilogramQty.Name: string;
begin result := 'cubic %smeter? per %sgram' end;

class function TCubicMeterPerKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo]; end;

class function TCubicMeterPerKilogramQty.Exponents: TExponents;
begin result := [3, -1]; end;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]

operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramUnit): TCubicMeterPerKilogramQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]

operator /(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramPerCubicMeterQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerKilogramQty./(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty;
begin result.FValue := ALeft / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i common.inc}

class function TKilogramSquareSecondQty.Symbol: string;
begin result := '%sg.%ss2' end;

class function TKilogramSquareSecondQty.Name: string;
begin result := '%sgram square %ssecond?' end;

class function TKilogramSquareSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramSquareSecondQty.Exponents: TExponents;
begin result := [1, 2]; end;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]

operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramSquareSecondQty./(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramSquareSecondQty./(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondUnit): TKilogramSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i common.inc}

class function TCubicMeterPerSquareSecondQty.Symbol: string;
begin result := '%sm3/%ss2' end;

class function TCubicMeterPerSquareSecondQty.Name: string;
begin result := 'cubic %smeter? per square %ssecond' end;

class function TCubicMeterPerSquareSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCubicMeterPerSquareSecondQty.Exponents: TExponents;
begin result := [3, -2]; end;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondUnit): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]

operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondSquaredQty): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondSquaredQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i common.inc}

class function TNewtonSquareMeterQty.Symbol: string;
begin result := '%sN.%sm2' end;

class function TNewtonSquareMeterQty.Name: string;
begin result := '%snewton square %smeter?' end;

class function TNewtonSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonSquareMeterQty.Exponents: TExponents;
begin result := [1, 2]; end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]

operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2 ] = [ J ] * [ m ]

operator *(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]

operator *(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i common.inc}

class function TNewtonPerSquareKilogramQty.Symbol: string;
begin result := '%sN/%sg2' end;

class function TNewtonPerSquareKilogramQty.Name: string;
begin result := '%snewton? per square %sgram' end;

class function TNewtonPerSquareKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo]; end;

class function TNewtonPerSquareKilogramQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]

operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerSquareKilogramQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramUnit): TNewtonPerSquareKilogramQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i common.inc}

class function TSquareKilogramPerMeterQty.Symbol: string;
begin result := '%sg2/%sm' end;

class function TSquareKilogramPerMeterQty.Name: string;
begin result := 'square %sgram? per %smeter' end;

class function TSquareKilogramPerMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TSquareKilogramPerMeterQty.Exponents: TExponents;
begin result := [2, -1]; end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]

operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterUnit): TSquareKilogramPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i common.inc}

class function TSquareKilogramPerSquareMeterQty.Symbol: string;
begin result := '%sg2/%sm2' end;

class function TSquareKilogramPerSquareMeterQty.Name: string;
begin result := 'square %sgram? per square %smeter' end;

class function TSquareKilogramPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TSquareKilogramPerSquareMeterQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterUnit): TSquareKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i common.inc}

class function TSquareMeterPerSquareKilogramQty.Symbol: string;
begin result := '%sm2/%sg2' end;

class function TSquareMeterPerSquareKilogramQty.Name: string;
begin result := 'square %smeter? per square %sgram' end;

class function TSquareMeterPerSquareKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo]; end;

class function TSquareMeterPerSquareKilogramQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareKilogramQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramUnit): TSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i common.inc}

class function TNewtonSquareMeterPerSquareKilogramQty.Symbol: string;
begin result := '%sN.%sm2/%sg2' end;

class function TNewtonSquareMeterPerSquareKilogramQty.Name: string;
begin result := '%snewton square %smeter? per square %sgram' end;

class function TNewtonSquareMeterPerSquareKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pKilo]; end;

class function TNewtonSquareMeterPerSquareKilogramQty.Exponents: TExponents;
begin result := [1, 2, -2]; end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]

operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]

operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramUnit): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]

operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]

operator /(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]

operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareSecondQty): TCubicMeterPerKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]

operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]

operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i common.inc}

class function TReciprocalKelvinQty.Symbol: string;
begin result := '1/%sK' end;

class function TReciprocalKelvinQty.Name: string;
begin result := 'reciprocal %skelvin' end;

class function TReciprocalKelvinQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TReciprocalKelvinQty.Exponents: TExponents;
begin result := [-1]; end;

// main definition [ 1/K ] = 1 / [ K ]

operator /(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TReciprocalKelvinQty.*(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalKelvinQty.*(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalKelvinQty./(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TKelvinUnit): TReciprocalKelvinQty;
begin result.FValue := ALeft; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i common.inc}

class function TKilogramKelvinQty.Symbol: string;
begin result := '%sg.%sK' end;

class function TKilogramKelvinQty.Name: string;
begin result := '%sgram %skelvin?' end;

class function TKilogramKelvinQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TKilogramKelvinQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ kg*K] = [ kg ] * [ K ]

operator *(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKilogramKelvinQty./(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKilogramKelvinQty./(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramQty; const ARight: TKelvinUnit): TKilogramKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i common.inc}

class function TJoulePerKelvinQty.Symbol: string;
begin result := '%sJ/%sK' end;

class function TJoulePerKelvinQty.Name: string;
begin result := '%sjoule? per %skelvin' end;

class function TJoulePerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TJoulePerKelvinQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ J/K ] = [ J ] / [ K ]

operator /(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKelvinQty.*(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TKelvinUnit): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i common.inc}

class function TJoulePerKilogramQty.Symbol: string;
begin result := '%sJ/%sg' end;

class function TJoulePerKilogramQty.Name: string;
begin result := '%sjoule? per %sgram' end;

class function TJoulePerKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo]; end;

class function TJoulePerKilogramQty.Exponents: TExponents;
begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i common.inc}

class function TJoulePerKilogramPerKelvinQty.Symbol: string;
begin result := '%sJ/%sg/%sK' end;

class function TJoulePerKilogramPerKelvinQty.Name: string;
begin result := '%sjoule? per %sgram per %skelvin' end;

class function TJoulePerKilogramPerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo, pNone]; end;

class function TJoulePerKilogramPerKelvinQty.Exponents: TExponents;
begin result := [1, -1, -1]; end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]

operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKelvinQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TJoulePerKilogramPerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinUnit): TJoulePerKilogramPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramQty; const ARight: TJoulePerKilogramPerKelvinQty): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramQty): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramUnit): TJoulePerKilogramPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterKelvinUnit}{$i common.inc}

class function TMeterKelvinQty.Symbol: string;
begin result := '%sm.%sK' end;

class function TMeterKelvinQty.Name: string;
begin result := '%smeter %skelvin?' end;

class function TMeterKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMeterKelvinQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ m*K ] = [ m ] * [ K ]

operator *(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterKelvinQty./(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterKelvinQty./(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TKelvinUnit): TMeterKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i common.inc}

class function TKelvinPerMeterQty.Symbol: string;
begin result := '%sK/%sm' end;

class function TKelvinPerMeterQty.Name: string;
begin result := '%skelvin? per %smeter' end;

class function TKelvinPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TKelvinPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ K/m ] = [ K ] / [ m ]

operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKelvinPerMeterQty.*(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKelvinPerMeterQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKelvinPerMeterQty./(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKelvinQty; const ARight: TMeterUnit): TKelvinPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerMeterUnit}{$i common.inc}

class function TWattPerMeterQty.Symbol: string;
begin result := '%sW/%sm' end;

class function TWattPerMeterQty.Name: string;
begin result := '%swatt? per %smeter' end;

class function TWattPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TWattPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ W/m ] = [ W ] / [ m ]

operator /(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerMeterQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterQty.*(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterQty./(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TMeterUnit): TWattPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i common.inc}

class function TWattPerSquareMeterQty.Symbol: string;
begin result := '%sW/%sm2' end;

class function TWattPerSquareMeterQty.Name: string;
begin result := '%swatt? per square %smeter' end;

class function TWattPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TWattPerSquareMeterQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterQty.*(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i common.inc}

class function TWattPerKelvinQty.Symbol: string;
begin result := '%sW/%sK' end;

class function TWattPerKelvinQty.Name: string;
begin result := '%swatt? per %skelvin' end;

class function TWattPerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TWattPerKelvinQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ W/K ] = [ W ] / [ K ]

operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerKelvinQty.*(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TKelvinUnit): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i common.inc}

class function TWattPerMeterPerKelvinQty.Symbol: string;
begin result := '%sW/%sm/%sK' end;

class function TWattPerMeterPerKelvinQty.Name: string;
begin result := '%swatt? per %smeter per %skelvin' end;

class function TWattPerMeterPerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TWattPerMeterPerKelvinQty.Exponents: TExponents;
begin result := [1, -1, -1]; end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]

operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]

operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinUnit): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]

operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterUnit): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i common.inc}

class function TSquareMeterKelvinQty.Symbol: string;
begin result := '%sm2.%sK' end;

class function TSquareMeterKelvinQty.Name: string;
begin result := 'square %smeter %skelvin?' end;

class function TSquareMeterKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareMeterKelvinQty.Exponents: TExponents;
begin result := [2, 1]; end;

// main definition [ m2*K ] = [ m2 ] * [ K ]

operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterKelvinQty./(const ALeft: TSquareMeterKelvinQty; const ARight: TSquareMeterQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterKelvinQty./(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinUnit): TSquareMeterKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i common.inc}

class function TWattPerSquareMeterPerKelvinQty.Symbol: string;
begin result := '%sW/%sm2/%sK' end;

class function TWattPerSquareMeterPerKelvinQty.Name: string;
begin result := '%swatt? per square %smeter per %skelvin' end;

class function TWattPerSquareMeterPerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TWattPerSquareMeterPerKelvinQty.Exponents: TExponents;
begin result := [1, -2, -1]; end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]

operator /(const ALeft: TWattQty; const ARight: TSquareMeterKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinUnit): TWattPerSquareMeterPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]

operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterQty): TWattPerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i common.inc}

class function TSquareMeterQuarticKelvinQty.Symbol: string;
begin result := '%sm2.%sK4' end;

class function TSquareMeterQuarticKelvinQty.Name: string;
begin result := 'square %smeter quartic %skelvin?' end;

class function TSquareMeterQuarticKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareMeterQuarticKelvinQty.Exponents: TExponents;
begin result := [2, 4]; end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]

operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TQuarticKelvinQty; const ARight: TSquareMeterQty): TSquareMeterQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterQuarticKelvinQty./(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TSquareMeterQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterQuarticKelvinQty./(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TQuarticKelvinQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinUnit): TSquareMeterQuarticKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i common.inc}

class function TWattPerQuarticKelvinQty.Symbol: string;
begin result := '%sW/%sK4' end;

class function TWattPerQuarticKelvinQty.Name: string;
begin result := '%swatt? per quartic %skelvin' end;

class function TWattPerQuarticKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TWattPerQuarticKelvinQty.Exponents: TExponents;
begin result := [1, -4]; end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]

operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinQty): TWattPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinUnit): TWattPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i common.inc}

class function TWattPerSquareMeterPerQuarticKelvinQty.Symbol: string;
begin result := '%sW/%sm2/%sK4' end;

class function TWattPerSquareMeterPerQuarticKelvinQty.Name: string;
begin result := '%swatt? per square %smeter per quartic %skelvin' end;

class function TWattPerSquareMeterPerQuarticKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TWattPerSquareMeterPerQuarticKelvinQty.Exponents: TExponents;
begin result := [1, -2, -4]; end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQuarticKelvinQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinUnit): TWattPerSquareMeterPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]

operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerQuarticKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i common.inc}

class function TJoulePerMoleQty.Symbol: string;
begin result := '%sJ/%smol' end;

class function TJoulePerMoleQty.Name: string;
begin result := '%sjoule? per %smole' end;

class function TJoulePerMoleQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TJoulePerMoleQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ J/mol ] = [ J ] / [ mol ]

operator /(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerMoleQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMoleQty.*(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMoleQty./(const ALeft: TJouleQty; const ARight: TJoulePerMoleQty): TMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleQty; const ARight: TMoleUnit): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMoleKelvinUnit}{$i common.inc}

class function TMoleKelvinQty.Symbol: string;
begin result := '%smol.%sK' end;

class function TMoleKelvinQty.Name: string;
begin result := '%smole %skelvin?' end;

class function TMoleKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMoleKelvinQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ mol*K ] = [ mol ] * [ K ]

operator *(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMoleKelvinQty./(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMoleKelvinQty./(const ALeft: TMoleKelvinQty; const ARight: TKelvinQty): TMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoleQty; const ARight: TKelvinUnit): TMoleKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i common.inc}

class function TJoulePerMolePerKelvinQty.Symbol: string;
begin result := '%sJ/%smol/%sK' end;

class function TJoulePerMolePerKelvinQty.Name: string;
begin result := '%sjoule? per %smole per %skelvin' end;

class function TJoulePerMolePerKelvinQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TJoulePerMolePerKelvinQty.Exponents: TExponents;
begin result := [1, -1, -1]; end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]

operator /(const ALeft: TJouleQty; const ARight: TMoleKelvinQty): TJoulePerMolePerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleKelvinQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerMolePerKelvinQty): TMoleKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleQty): TJoulePerMolePerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleQty): TJoulePerKelvinQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleUnit): TJoulePerMolePerKelvinQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]

operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinQty): TJoulePerMolePerKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TKelvinQty): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerMoleQty; const ARight: TJoulePerMolePerKelvinQty): TKelvinQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinUnit): TJoulePerMolePerKelvinQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TOhmMeterUnit}{$i common.inc}

class function TOhmMeterQty.Symbol: string;
begin result := '%sΩ.%sm' end;

class function TOhmMeterQty.Name: string;
begin result := '%sohm %smeter?' end;

class function TOhmMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TOhmMeterQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]

operator *(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TOhmMeterQty./(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TOhmMeterQty./(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TOhmQty; const ARight: TMeterUnit): TOhmMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i common.inc}

class function TVoltPerMeterQty.Symbol: string;
begin result := '%sV/%sm' end;

class function TVoltPerMeterQty.Name: string;
begin result := '%svolt? per %smeter' end;

class function TVoltPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TVoltPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ V/m ] = [ V ] / [ m ]

operator /(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltPerMeterQty.*(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltPerMeterQty./(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V/m ] = [ N ] / [ C ]

operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltPerMeterQty.*(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltPerMeterQty./(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TCoulombUnit): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V/m ] = [ T ] * [ m/s ]

operator *(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltPerMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltPerMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i common.inc}

class function TNewtonPerCoulombQty.Symbol: string;
begin result := '%sN/%sC' end;

class function TNewtonPerCoulombQty.Name: string;
begin result := '%snewton? per %scoulomb' end;

class function TNewtonPerCoulombQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerCoulombQty.Exponents: TExponents;
begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i common.inc}

class function TCoulombPerMeterQty.Symbol: string;
begin result := '%sC/%sm' end;

class function TCoulombPerMeterQty.Name: string;
begin result := '%scoulomb? per %smeter' end;

class function TCoulombPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCoulombPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ C/m ] = [ C ] / [ m ]

operator /(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMeterQty.*(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TMeterUnit): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i common.inc}

class function TSquareCoulombPerMeterQty.Symbol: string;
begin result := '%sC2/%sm' end;

class function TSquareCoulombPerMeterQty.Name: string;
begin result := 'square %scoulomb? per %smeter' end;

class function TSquareCoulombPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareCoulombPerMeterQty.Exponents: TExponents;
begin result := [2, -1]; end;

// main definition [ C2/m ] = [ C2 ] / [ m ]

operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterUnit): TSquareCoulombPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]

operator *(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i common.inc}

class function TCoulombPerSquareMeterQty.Symbol: string;
begin result := '%sC/%sm2' end;

class function TCoulombPerSquareMeterQty.Name: string;
begin result := '%scoulomb? per square %smeter' end;

class function TCoulombPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCoulombPerSquareMeterQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]

operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterUnit): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i common.inc}

class function TSquareMeterPerSquareCoulombQty.Symbol: string;
begin result := '%sm2/%sC2' end;

class function TSquareMeterPerSquareCoulombQty.Name: string;
begin result := 'square %smeter? per square %scoulomb' end;

class function TSquareMeterPerSquareCoulombQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareMeterPerSquareCoulombQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterPerSquareCoulombQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombUnit): TSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i common.inc}

class function TNewtonPerSquareCoulombQty.Symbol: string;
begin result := '%sN/%sC2' end;

class function TNewtonPerSquareCoulombQty.Name: string;
begin result := '%snewton? per square %scoulomb' end;

class function TNewtonPerSquareCoulombQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerSquareCoulombQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]

operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonPerSquareCoulombQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombUnit): TNewtonPerSquareCoulombQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i common.inc}

class function TNewtonSquareMeterPerSquareCoulombQty.Symbol: string;
begin result := '%sN.%sm2/%sC2' end;

class function TNewtonSquareMeterPerSquareCoulombQty.Name: string;
begin result := '%snewton square %smeter? per square %scoulomb' end;

class function TNewtonSquareMeterPerSquareCoulombQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TNewtonSquareMeterPerSquareCoulombQty.Exponents: TExponents;
begin result := [1, 2, -2]; end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]

operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombUnit): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]

operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]

operator /(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]

operator /(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TVoltMeterUnit}{$i common.inc}

class function TVoltMeterQty.Symbol: string;
begin result := '%sV.%sm' end;

class function TVoltMeterQty.Name: string;
begin result := '%svolt %smeter?' end;

class function TVoltMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TVoltMeterQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ V*m ] = [ V ] * [ m ]

operator *(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltMeterQty./(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltMeterQty./(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]

operator *(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltMeterQty./(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltMeterQty./(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i common.inc}

class function TNewtonSquareMeterPerCoulombQty.Symbol: string;
begin result := '%sN.%sm2/%sC' end;

class function TNewtonSquareMeterPerCoulombQty.Name: string;
begin result := '%snewton square %smeter? per %scoulomb' end;

class function TNewtonSquareMeterPerCoulombQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TNewtonSquareMeterPerCoulombQty.Exponents: TExponents;
begin result := [1, 2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i common.inc}

class function TVoltMeterPerSecondQty.Symbol: string;
begin result := '%sV.%sm/%ss' end;

class function TVoltMeterPerSecondQty.Name: string;
begin result := '%svolt %smeter? per %ssecond' end;

class function TVoltMeterPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TVoltMeterPerSecondQty.Exponents: TExponents;
begin result := [1, 1, -1]; end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]

operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TVoltMeterPerSecondQty./(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltMeterQty; const ARight: TSecondUnit): TVoltMeterPerSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i common.inc}

class function TFaradPerMeterQty.Symbol: string;
begin result := '%sF/%sm' end;

class function TFaradPerMeterQty.Name: string;
begin result := '%sfarad? per %smeter' end;

class function TFaradPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TFaradPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ F/m ] = [ F ] / [ m ]

operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TMeterQty; const ARight: TFaradPerMeterQty): TFaradQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TMeterQty): TFaradQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty./(const ALeft: TFaradQty; const ARight: TFaradPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TFaradQty; const ARight: TMeterUnit): TFaradPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]

operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltMeterQty; const ARight: TFaradPerMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombQty; const ARight: TFaradPerMeterQty): TVoltMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TFaradPerMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltPerMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TFaradPerMeterQty): TVoltPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]

operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TFaradPerMeterQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TFaradPerMeterQty./(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin result.FValue := ALeft / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i common.inc}

class function TAmperePerMeterQty.Symbol: string;
begin result := '%sA/%sm' end;

class function TAmperePerMeterQty.Name: string;
begin result := '%sampere? per %smeter' end;

class function TAmperePerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TAmperePerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ A/m ] = [ A ] / [ m ]

operator /(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TAmperePerMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerMeterQty./(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TAmpereQty; const ARight: TMeterUnit): TAmperePerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i common.inc}

class function TMeterPerAmpereQty.Symbol: string;
begin result := '%sm/%sA' end;

class function TMeterPerAmpereQty.Name: string;
begin result := '%smeter? per %sampere' end;

class function TMeterPerAmpereQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMeterPerAmpereQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ m/A ] = [ m ] / [ A ]

operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMeterPerAmpereQty.*(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerAmpereQty.*(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMeterPerAmpereQty./(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeterQty; const ARight: TAmpereUnit): TMeterPerAmpereQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTeslaMeterUnit}{$i common.inc}

class function TTeslaMeterQty.Symbol: string;
begin result := '%sT.%sm' end;

class function TTeslaMeterQty.Name: string;
begin result := '%stesla %smeter?' end;

class function TTeslaMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TTeslaMeterQty.Exponents: TExponents;
begin result := [1, 1]; end;

// main definition [ T*m ] = [ T ] * [ m ]

operator *(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaMeterQty./(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TTeslaMeterQty./(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TTeslaQty; const ARight: TMeterUnit): TTeslaMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]

operator /(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TTeslaMeterQty.*(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaMeterQty.*(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaMeterQty./(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonQty; const ARight: TAmpereUnit): TTeslaMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i common.inc}

class function TNewtonPerAmpereQty.Symbol: string;
begin result := '%sN/%sA' end;

class function TNewtonPerAmpereQty.Name: string;
begin result := '%snewton? per %sampere' end;

class function TNewtonPerAmpereQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerAmpereQty.Exponents: TExponents;
begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i common.inc}

class function TTeslaPerAmpereQty.Symbol: string;
begin result := '%sT/%sA' end;

class function TTeslaPerAmpereQty.Name: string;
begin result := '%stesla? per %sampere' end;

class function TTeslaPerAmpereQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TTeslaPerAmpereQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ T/A ] = [ T ] / [ A ]

operator /(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TTeslaPerAmpereQty.*(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaPerAmpereQty.*(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TTeslaPerAmpereQty./(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TTeslaQty; const ARight: TAmpereUnit): TTeslaPerAmpereQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=THenryPerMeterUnit}{$i common.inc}

class function THenryPerMeterQty.Symbol: string;
begin result := '%sH/%sm' end;

class function THenryPerMeterQty.Name: string;
begin result := '%shenry! per %smeter' end;

class function THenryPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function THenryPerMeterQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ H/m ] = [ H ] / [ m ]

operator /(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: THenryQty; const ARight: TMeterUnit): THenryPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]

operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereUnit): THenryPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]

operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterUnit): THenryPerMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]

operator *(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]

operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]

operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator THenryPerMeterQty./(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i common.inc}

class function TTeslaMeterPerAmpereQty.Symbol: string;
begin result := '%sT.%sm/%sA' end;

class function TTeslaMeterPerAmpereQty.Name: string;
begin result := '%stesla %smeter? per %sampere' end;

class function TTeslaMeterPerAmpereQty.Prefixes: TPrefixes;
begin result := [pNone, pNone, pNone]; end;

class function TTeslaMeterPerAmpereQty.Exponents: TExponents;
begin result := [1, 1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i common.inc}

class function TNewtonPerSquareAmpereQty.Symbol: string;
begin result := '%sN/%sA2' end;

class function TNewtonPerSquareAmpereQty.Name: string;
begin result := '%snewton? per square %sampere' end;

class function TNewtonPerSquareAmpereQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TNewtonPerSquareAmpereQty.Exponents: TExponents;
begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i common.inc}

class function TRadianPerMeterQty.Symbol: string;
begin result := 'rad/%sm' end;

class function TRadianPerMeterQty.Name: string;
begin result := 'radian? per %smeter' end;

class function TRadianPerMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TRadianPerMeterQty.Exponents: TExponents;
begin result := [-1]; end;

// main definition [ rad/m ] = [ rad ] / [ m ]

operator /(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TRadianPerMeterQty.*(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerMeterQty.*(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TRadianPerMeterQty./(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadianQty; const ARight: TMeterUnit): TRadianPerMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i common.inc}

class function TSquareKilogramPerSquareSecondQty.Symbol: string;
begin result := '%sg2/%ss2' end;

class function TSquareKilogramPerSquareSecondQty.Name: string;
begin result := 'square %sgram? per square %ssecond' end;

class function TSquareKilogramPerSquareSecondQty.Prefixes: TPrefixes;
begin result := [pKilo, pNone]; end;

class function TSquareKilogramPerSquareSecondQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]

operator *(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]

operator *(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i common.inc}

class function TReciprocalMeterQty.Symbol: string;
begin result := '1/%sm' end;

class function TReciprocalMeterQty.Name: string;
begin result := 'reciprocal %smeter?' end;

class function TReciprocalMeterQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TReciprocalMeterQty.Exponents: TExponents;
begin result := [-1]; end;

// main definition [ 1/m ] = 1 / [ m ]

operator /(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TReciprocalMeterQty.*(const ALeft: TMeterQty; const ARight: TReciprocalMeterQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalMeterQty.*(const ALeft: TReciprocalMeterQty; const ARight: TMeterQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalMeterQty./(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TMeterUnit): TReciprocalMeterQty;
begin result.FValue := ALeft; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i common.inc}

class function TSquareSecondPerSquareMeterQty.Symbol: string;
begin result := '%ss2/%sm2' end;

class function TSquareSecondPerSquareMeterQty.Name: string;
begin result := 'square %ssecond? per square %smeter' end;

class function TSquareSecondPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TSquareSecondPerSquareMeterQty.Exponents: TExponents;
begin result := [2, -2]; end;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]

operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterUnit): TSquareSecondPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]

operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft / ARight.FValue; end;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]

operator *(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareJouleUnit}{$i common.inc}

class function TSquareJouleQty.Symbol: string;
begin result := '%sJ2' end;

class function TSquareJouleQty.Name: string;
begin result := 'square %sjoule?' end;

class function TSquareJouleQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareJouleQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ J2 ] = [ J ] * [ J ]

operator *(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareJouleQty./(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TJouleSecondUnit}{$i common.inc}

class function TJouleSecondQty.Symbol: string;
begin result := '%sJ.%ss' end;

class function TJouleSecondQty.Name: string;
begin result := '%sjoule %ssecond?' end;

class function TJouleSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TJouleSecondQty.Exponents: TExponents;
begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i common.inc}

class function TElettronvoltSecondQty.Symbol: string;
begin result := '%seV.%ss' end;
class function TElettronvoltSecondQty.Name: string;
begin result := '%selettronvolt %ssecond?' end;
class function TElettronvoltSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TElettronvoltSecondQty.Exponents: TExponents;
begin result := [1, 1]; end;

class function TElettronvoltSecondQty.ToBaseFactor: double;
begin result := 1.60217742320523E-019; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TLumenPerWattUnit}{$i common.inc}

class function TLumenPerWattQty.Symbol: string;
begin result := '%slm/%sW' end;

class function TLumenPerWattQty.Name: string;
begin result := '%slumen? per %swatt' end;

class function TLumenPerWattQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TLumenPerWattQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ lm/W ] = [ lm ] / [ W ]

operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TLumenPerWattQty.*(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TLumenPerWattQty.*(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TLumenPerWattQty./(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i common.inc}

class function TReciprocalMoleQty.Symbol: string;
begin result := '1/%smol' end;

class function TReciprocalMoleQty.Name: string;
begin result := 'reciprocal %smole?' end;

class function TReciprocalMoleQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TReciprocalMoleQty.Exponents: TExponents;
begin result := [-1]; end;

// main definition [ 1/mol ] = 1 / [ mol ]

operator /(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty;
begin result.FValue := ALeft / ARight.FValue; end;

class operator TReciprocalMoleQty.*(const ALeft: TMoleQty; const ARight: TReciprocalMoleQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalMoleQty.*(const ALeft: TReciprocalMoleQty; const ARight: TMoleQty): double;
begin result := ALeft.FValue * ARight.FValue; end;

class operator TReciprocalMoleQty./(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TMoleUnit): TReciprocalMoleQty;
begin result.FValue := ALeft; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i common.inc}

class function TAmperePerSquareMeterQty.Symbol: string;
begin result := '%sA/%sm2' end;

class function TAmperePerSquareMeterQty.Name: string;
begin result := '%sampere? per square %smeter' end;

class function TAmperePerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TAmperePerSquareMeterQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ A/m2 ] = [ A ] / [ m2 ]

operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerSquareMeterQty./(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterUnit): TAmperePerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ A/m2 ] = [ A/m ] / [ m ]

operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TAmperePerSquareMeterQty./(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i common.inc}

class function TMolePerCubicMeterQty.Symbol: string;
begin result := '%smol/%sm3' end;

class function TMolePerCubicMeterQty.Name: string;
begin result := '%smole? per cubic %smeter' end;

class function TMolePerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TMolePerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

// main definition [ mol/m3 ] = [ mol ] / [ m3 ]

operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TMolePerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMolePerCubicMeterQty.*(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TMolePerCubicMeterQty./(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMoleQty; const ARight: TCubicMeterUnit): TMolePerCubicMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i common.inc}

class function TCandelaPerSquareMeterQty.Symbol: string;
begin result := '%scd/%sm2' end;

class function TCandelaPerSquareMeterQty.Name: string;
begin result := '%scandela? per square %smeter' end;

class function TCandelaPerSquareMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCandelaPerSquareMeterQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ cd/m2 ] = [ cd ] / [ m2 ]

operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCandelaPerSquareMeterQty./(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterUnit): TCandelaPerSquareMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i common.inc}

class function TCoulombPerCubicMeterQty.Symbol: string;
begin result := '%sC/%sm3' end;

class function TCoulombPerCubicMeterQty.Name: string;
begin result := '%scoulomb? per cubic %smeter' end;

class function TCoulombPerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCoulombPerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

// main definition [ C/m3 ] = [ C ] / [ m3 ]

operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterUnit): TCoulombPerCubicMeterQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C/m3 ] = [ C/m ] / [ m2 ]

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ C/m3 ] = [ C/m2 ] / [ m ]

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i common.inc}

class function TCoulombPerKilogramQty.Symbol: string;
begin result := '%sC/%sg' end;

class function TCoulombPerKilogramQty.Name: string;
begin result := '%scoulomb? per %sgram' end;

class function TCoulombPerKilogramQty.Prefixes: TPrefixes;
begin result := [pNone, pKilo]; end;

class function TCoulombPerKilogramQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ C/kg ] = [ C ] / [ kg ]

operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerKilogramQty.*(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerKilogramQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TKilogramUnit): TCoulombPerKilogramQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i common.inc}

class function TGrayPerSecondQty.Symbol: string;
begin result := '%sGy/%ss' end;

class function TGrayPerSecondQty.Name: string;
begin result := '%sgray? per %ssecond' end;

class function TGrayPerSecondQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TGrayPerSecondQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ Gy/s ] = [ Gy ] / [ s ]

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TGrayPerSecondQty.*(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TGrayPerSecondQty.*(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TGrayPerSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondUnit): TGrayPerSecondQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i common.inc}

class function TWattPerSteradianQty.Symbol: string;
begin result := '%sW/sr' end;

class function TWattPerSteradianQty.Name: string;
begin result := '%swatt? per steradian' end;

class function TWattPerSteradianQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TWattPerSteradianQty.Exponents: TExponents;
begin result := [1]; end;

// main definition [ W/sr ] = [ W ] / [ sr ]

operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSteradianQty.*(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattQty; const ARight: TSteradianUnit): TWattPerSteradianQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i common.inc}

class function TSquareMeterSteradianQty.Symbol: string;
begin result := '%sm2.sr' end;

class function TSquareMeterSteradianQty.Name: string;
begin result := 'square %smeter steradian?' end;

class function TSquareMeterSteradianQty.Prefixes: TPrefixes;
begin result := [pNone]; end;

class function TSquareMeterSteradianQty.Exponents: TExponents;
begin result := [2]; end;

// main definition [ m2 * sr ] = [ m2 ] * [ sr ]

operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianQty): TSquareMeterSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSteradianQty; const ARight: TSquareMeterQty): TSquareMeterSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TSquareMeterSteradianQty./(const ALeft: TSquareMeterSteradianQty; const ARight: TSquareMeterQty): TSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TSquareMeterSteradianQty./(const ALeft: TSquareMeterSteradianQty; const ARight: TSteradianQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianUnit): TSquareMeterSteradianQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i common.inc}

class function TWattPerSquareMeterPerSteradianQty.Symbol: string;
begin result := '%sW/%sm2/sr' end;

class function TWattPerSquareMeterPerSteradianQty.Name: string;
begin result := '%swatt? per square %smeter per steradian' end;

class function TWattPerSquareMeterPerSteradianQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TWattPerSquareMeterPerSteradianQty.Exponents: TExponents;
begin result := [1, -2]; end;

// main definition [ W/m2/sr ] = [ W ] / [ m2 * sr ]

operator /(const ALeft: TWattQty; const ARight: TSquareMeterSteradianQty): TWattPerSquareMeterPerSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterSteradianQty): TWattQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/sr ] = [ W/m2 ] / [ sr ]

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSteradianQty): TWattPerSquareMeterQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianUnit): TWattPerSquareMeterPerSteradianQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/sr ] = [ W/sr ] / [ m2 ]

operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSteradianQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerSteradianQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i common.inc}

class function TKatalPerCubicMeterQty.Symbol: string;
begin result := '%skat/%sm3' end;

class function TKatalPerCubicMeterQty.Name: string;
begin result := '%skatal? per cubic %smeter' end;

class function TKatalPerCubicMeterQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TKatalPerCubicMeterQty.Exponents: TExponents;
begin result := [1, -3]; end;

// main definition [ kat/m3 ] = [ kat ] / [ m3 ]

operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TKatalPerCubicMeterQty./(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKatalQty; const ARight: TCubicMeterUnit): TKatalPerCubicMeterQty;
begin result.FValue := ALeft.FValue; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i common.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i common.inc}

class function TCoulombPerMoleQty.Symbol: string;
begin result := '%sC/%smol' end;

class function TCoulombPerMoleQty.Name: string;
begin result := '%scoulomb? per %smole' end;

class function TCoulombPerMoleQty.Prefixes: TPrefixes;
begin result := [pNone, pNone]; end;

class function TCoulombPerMoleQty.Exponents: TExponents;
begin result := [1, -1]; end;

// main definition [ C/mol ] = [ C ] / [ mol ]

operator /(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerMoleQty.*(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMoleQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMoleQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombQty; const ARight: TMoleUnit): TCoulombPerMoleQty;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C/mol ] = [ J/mol ] / [ V ]

operator /(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

class operator TCoulombPerMoleQty.*(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMoleQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

class operator TCoulombPerMoleQty./(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

{ Helpers }

function TSecondHelper.ToDay: TDayQty;
begin result.FValue := FValue / TDayQty.ToBaseFactor; end;

function TSecondHelper.ToHour: THourQty;
begin result.FValue := FValue / THourQty.ToBaseFactor; end;

function TSecondHelper.ToMinute: TMinuteQty;
begin result.FValue := FValue / TMinuteQty.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareDay: TSquareDayQty;
begin result.FValue := FValue / TSquareDayQty.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareHour: TSquareHourQty;
begin result.FValue := FValue / TSquareHourQty.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareMinute: TSquareMinuteQty;
begin result.FValue := FValue / TSquareMinuteQty.ToBaseFactor; end;

function TMeterHelper.ToAstronomical: TAstronomicalQty;
begin result.FValue := FValue / TAstronomicalQty.ToBaseFactor; end;

function TMeterHelper.ToInch: TInchQty;
begin result.FValue := FValue / TInchQty.ToBaseFactor; end;

function TMeterHelper.ToFoot: TFootQty;
begin result.FValue := FValue / TFootQty.ToBaseFactor; end;

function TMeterHelper.ToYard: TYardQty;
begin result.FValue := FValue / TYardQty.ToBaseFactor; end;

function TMeterHelper.ToMile: TMileQty;
begin result.FValue := FValue / TMileQty.ToBaseFactor; end;

function TMeterHelper.ToNauticalMile: TNauticalMileQty;
begin result.FValue := FValue / TNauticalMileQty.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareInch: TSquareInchQty;
begin result.FValue := FValue / TSquareInchQty.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareFoot: TSquareFootQty;
begin result.FValue := FValue / TSquareFootQty.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareYard: TSquareYardQty;
begin result.FValue := FValue / TSquareYardQty.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareMile: TSquareMileQty;
begin result.FValue := FValue / TSquareMileQty.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicInch: TCubicInchQty;
begin result.FValue := FValue / TCubicInchQty.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicFoot: TCubicFootQty;
begin result.FValue := FValue / TCubicFootQty.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicYard: TCubicYardQty;
begin result.FValue := FValue / TCubicYardQty.ToBaseFactor; end;

function TCubicMeterHelper.ToLitre: TLitreQty;
begin result.FValue := FValue / TLitreQty.ToBaseFactor; end;

function TCubicMeterHelper.ToGallon: TGallonQty;
begin result.FValue := FValue / TGallonQty.ToBaseFactor; end;

function TKilogramHelper.ToTonne: TTonneQty;
begin result.FValue := FValue / TTonneQty.ToBaseFactor; end;

function TKilogramHelper.ToPound: TPoundQty;
begin result.FValue := FValue / TPoundQty.ToBaseFactor; end;

function TKilogramHelper.ToOunce: TOunceQty;
begin result.FValue := FValue / TOunceQty.ToBaseFactor; end;

function TKilogramHelper.ToStone: TStoneQty;
begin result.FValue := FValue / TStoneQty.ToBaseFactor; end;

function TKilogramHelper.ToTon: TTonQty;
begin result.FValue := FValue / TTonQty.ToBaseFactor; end;

function TDegreeCelsiusHelper.ToKelvin: TKelvinQty;
begin result.FValue := FValue + 273.15; end;

function TKelvinHelper.ToDegreeCelsius: TDegreeCelsiusQty;
begin result.FValue := FValue - 273.15; end;

function TDegreeFahrenheitHelper.ToKelvin: TKelvinQty;
begin result.FValue := 5/9 * (FValue - 32) + 273.15; end;

function TKelvinHelper.ToDegreeFahrenheit: TDegreeFahrenheitQty;
begin result.FValue := 9/5 * FValue - 459.67; end;

function TRadianHelper.ToDegree: TDegreeQty;
begin result.FValue := FValue / TDegreeQty.ToBaseFactor; end;

function TSteradianHelper.ToSquareDegree: TSquareDegreeQty;
begin result.FValue := FValue / TSquareDegreeQty.ToBaseFactor; end;

function TSquareHertzHelper.ToRadianPerSecondSquared: TRadianPerSecondSquaredQty;
begin result.FValue := FValue; end;

function TSquareHertzHelper.ToSteradianPerSquareSecond: TSteradianPerSquareSecondQty;
begin result.FValue := FValue; end;

function TMeterPerSecondHelper.ToMeterPerHour: TMeterPerHourQty;
begin result.FValue := FValue / TMeterPerHourQty.ToBaseFactor; end;

function TMeterPerSecondHelper.ToMilePerHour: TMilePerHourQty;
begin result.FValue := FValue / TMilePerHourQty.ToBaseFactor; end;

function TMeterPerSecondHelper.ToNauticalMilePerHour: TNauticalMilePerHourQty;
begin result.FValue := FValue / TNauticalMilePerHourQty.ToBaseFactor; end;

function TMeterPerSecondSquaredHelper.ToMeterPerSecondPerSecond: TMeterPerSecondPerSecondQty;
begin result.FValue := FValue; end;

function TMeterPerSecondSquaredHelper.ToMeterPerHourPerSecond: TMeterPerHourPerSecondQty;
begin result.FValue := FValue / TMeterPerHourPerSecondQty.ToBaseFactor; end;

function TKilogramMeterPerSecondHelper.ToNewtonSecond: TNewtonSecondQty;
begin result.FValue := FValue; end;

function TNewtonHelper.ToPoundForce: TPoundForceQty;
begin result.FValue := FValue / TPoundForceQty.ToBaseFactor; end;

function TPascalHelper.ToBar: TBarQty;
begin result.FValue := FValue / TBarQty.ToBaseFactor; end;

function TPascalHelper.ToPoundPerSquareInch: TPoundPerSquareInchQty;
begin result.FValue := FValue / TPoundPerSquareInchQty.ToBaseFactor; end;

function TPascalHelper.ToJoulePerCubicMeter: TJoulePerCubicMeterQty;
begin result.FValue := FValue; end;

function TJouleHelper.ToWattHour: TWattHourQty;
begin result.FValue := FValue / TWattHourQty.ToBaseFactor; end;

function TJouleHelper.ToElettronvolt: TElettronvoltQty;
begin result.FValue := FValue / TElettronvoltQty.ToBaseFactor; end;

function TJouleHelper.ToNewtonMeter: TNewtonMeterQty;
begin result.FValue := FValue; end;

function TJouleHelper.ToPoundForceInch: TPoundForceInchQty;
begin result.FValue := FValue / TPoundForceInchQty.ToBaseFactor; end;

function TCoulombHelper.ToAmpereHour: TAmpereHourQty;
begin result.FValue := FValue / TAmpereHourQty.ToBaseFactor; end;

function THertzHelper.ToBequerel: TBequerelQty;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToGray: TGrayQty;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToSievert: TSievertQty;
begin result.FValue := FValue; end;

function TJoulePerRadianHelper.ToJoulePerDegree: TJoulePerDegreeQty;
begin result.FValue := FValue / TJoulePerDegreeQty.ToBaseFactor; end;

function TJoulePerRadianHelper.ToNewtonMeterPerRadian: TNewtonMeterPerRadianQty;
begin result.FValue := FValue; end;

function TJoulePerRadianHelper.ToNewtonMeterPerDegree: TNewtonMeterPerDegreeQty;
begin result.FValue := FValue / TNewtonMeterPerDegreeQty.ToBaseFactor; end;

function TNewtonPerMeterHelper.ToPoundForcePerInch: TPoundForcePerInchQty;
begin result.FValue := FValue / TPoundForcePerInchQty.ToBaseFactor; end;

function TPoiseuilleHelper.ToPascalSecond: TPascalSecondQty;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToJoulePerKilogram: TJoulePerKilogramQty;
begin result.FValue := FValue; end;

function TVoltPerMeterHelper.ToNewtonPerCoulomb: TNewtonPerCoulombQty;
begin result.FValue := FValue; end;

function TVoltMeterHelper.ToNewtonSquareMeterPerCoulomb: TNewtonSquareMeterPerCoulombQty;
begin result.FValue := FValue; end;

function TTeslaMeterHelper.ToNewtonPerAmpere: TNewtonPerAmpereQty;
begin result.FValue := FValue; end;

function THenryPerMeterHelper.ToTeslaMeterPerAmpere: TTeslaMeterPerAmpereQty;
begin result.FValue := FValue; end;

function THenryPerMeterHelper.ToNewtonPerSquareAmpere: TNewtonPerSquareAmpereQty;
begin result.FValue := FValue; end;

function TKilogramSquareMeterPerSecondHelper.ToJouleSecond: TJouleSecondQty;
begin result.FValue := FValue; end;

function TKilogramSquareMeterPerSecondHelper.ToElettronvoltSecond: TElettronvoltSecondQty;
begin result.FValue := FValue / TElettronvoltSecondQty.ToBaseFactor; end;

{ Power quantities }

function SquarePower(AQuantity: TSecondQty): TSquareSecondQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareSecondQty): TSecondQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TMeterQty): TSquareMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareMeterQty): TMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TMeterQty): TCubicMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TCubicMeterQty): TMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SquarePower(AQuantity: TSquareMeterQty): TQuarticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TQuarticMeterQty): TSquareMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function QuarticPower(AQuantity: TMeterQty): TQuarticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 4); end;

function QuarticRoot(AQuantity: TQuarticMeterQty): TMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/4); end;

function QuinticPower(AQuantity: TMeterQty): TQuinticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 5); end;

function QuinticRoot(AQuantity: TQuinticMeterQty): TMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/5); end;

function SquarePower(AQuantity: TCubicMeterQty): TSexticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSexticMeterQty): TCubicMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TSquareMeterQty): TSexticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TSexticMeterQty): TSquareMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SexticPower(AQuantity: TMeterQty): TSexticMeterQty;
begin result.FValue := IntPower(AQuantity.FValue, 6); end;

function SexticRoot(AQuantity: TSexticMeterQty): TMeterQty;
begin result.FValue := Power(AQuantity.FValue, 1/6); end;

function SquarePower(AQuantity: TAmpereQty): TSquareAmpereQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareAmpereQty): TAmpereQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TKelvinQty): TSquareKelvinQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareKelvinQty): TKelvinQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TKelvinQty): TCubicKelvinQty;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TCubicKelvinQty): TKelvinQty;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SquarePower(AQuantity: TSquareKelvinQty): TQuarticKelvinQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TQuarticKelvinQty): TSquareKelvinQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function QuarticPower(AQuantity: TKelvinQty): TQuarticKelvinQty;
begin result.FValue := IntPower(AQuantity.FValue, 4); end;

function QuarticRoot(AQuantity: TQuarticKelvinQty): TKelvinQty;
begin result.FValue := Power(AQuantity.FValue, 1/4); end;

function SquarePower(AQuantity: TRadianQty): TSteradianQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSteradianQty): TRadianQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: THertzQty): TSquareHertzQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareHertzQty): THertzQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareMeterPerSquareSecondQty): TMeterPerSecondQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TNewtonQty): TSquareNewtonQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareNewtonQty): TNewtonQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TCoulombQty): TSquareCoulombQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareCoulombQty): TCoulombQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TVoltQty): TSquareVoltQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareVoltQty): TVoltQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareKilogramPerSquareSecondQty): TKilogramPerSecondQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TJouleQty): TSquareJouleQty;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareJouleQty): TJouleQty;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

{ Trigonometric functions }

function Cos(const AQuantity: TRadianQty): double;
begin result := System.Cos(AQuantity.FValue); end;

function Sin(const AQuantity: TRadianQty): double;
begin result := System.Sin(AQuantity.FValue); end;

function Tan(const AQuantity: TRadianQty): double;
begin result := Math.Tan(AQuantity.FValue); end;

function Cotan(const AQuantity: TRadianQty): double;
begin result := Math.Cotan(AQuantity.FValue); end;

function Secant(const AQuantity: TRadianQty): double;
begin result := Math.Secant(AQuantity.FValue); end;

function Cosecant(const AQuantity: TRadianQty): double;
begin result := Math.Cosecant(AQuantity.FValue); end;

function ArcCos(const AValue: double): TRadianQty;
begin result.FValue := Math.ArcCos(AValue); end;

function ArcSin(const AValue: double): TRadianQty;
begin result.FValue := Math.ArcSin(AValue); end;

function ArcTan(const AValue: double): TRadianQty;
begin result.FValue := System.ArcTan(AValue); end;

function ArcTan2(const x, y: double): TRadianQty;
begin result.FValue := Math.ArcTan2(x, y); end;

end.
