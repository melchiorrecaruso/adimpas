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
  ADimPas library built on 31/08/2023.

  Number of base units: 122
  Number of factored units: 62
  Number of operators: 956
}

unit ADim;

{$H+}
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
    function Value(const Prefixes: TPrefixes): double;
    function ToString: string;
    function ToVerboseString: string;
    function ToString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
    function ToVerboseString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
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

type
  { Unit of second }
  TSecondUnit = record
    const Symbol       = '%ss';
    const SingularName = '%ssecond';
    const PluralName   = '%sseconds';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TSeconds = specialize TQuantity<TSecondUnit>;
  TSecondUnitId = specialize TUnitId<TSecondUnit>;

var s: TSecondUnitId;

const ds: specialize TQuantity<TSecondUnit> = (FValue: 1E-01);
const cs: specialize TQuantity<TSecondUnit> = (FValue: 1E-02);
const ms: specialize TQuantity<TSecondUnit> = (FValue: 1E-03);
const mis: specialize TQuantity<TSecondUnit> = (FValue: 1E-06);
const ns: specialize TQuantity<TSecondUnit> = (FValue: 1E-09);
const ps: specialize TQuantity<TSecondUnit> = (FValue: 1E-12);

type
  { Unit of day }
  TDayUnit = record
    const Symbol       = 'd';
    const SingularName = 'day';
    const PluralName   = 'days';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 86400;
  end;
  TDays = specialize TQuantity<TSecondUnit>;
  TDayUnitId = specialize TUnitId<TDayUnit>;

const day: specialize TQuantity<TSecondUnit> = (FValue: TDayUnit.ToBaseFactor);

type
  { Unit of hour }
  THourUnit = record
    const Symbol       = 'h';
    const SingularName = 'hour';
    const PluralName   = 'hours';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 3600;
  end;
  THours = specialize TQuantity<TSecondUnit>;
  THourUnitId = specialize TUnitId<THourUnit>;

const hr: specialize TQuantity<TSecondUnit> = (FValue: THourUnit.ToBaseFactor);

type
  { Unit of minute }
  TMinuteUnit = record
    const Symbol       = 'min';
    const SingularName = 'minute';
    const PluralName   = 'minutes';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 60;
  end;
  TMinutes = specialize TQuantity<TSecondUnit>;
  TMinuteUnitId = specialize TUnitId<TMinuteUnit>;

const minute: specialize TQuantity<TSecondUnit> = (FValue: TMinuteUnit.ToBaseFactor);

type
  { Unit of square second }
  TSquareSecondUnit = record
    const Symbol       = '%ss2';
    const SingularName = 'square %ssecond';
    const PluralName   = 'square %sseconds';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;
  TSquareSecondUnitId = specialize TUnitId<TSquareSecondUnit>;

var s2: TSquareSecondUnitId;

const ds2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-02);
const cs2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-04);
const ms2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-06);
const mis2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-12);
const ns2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-18);
const ps2: specialize TQuantity<TSquareSecondUnit> = (FValue: 1E-24);

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

type
  { Unit of square day }
  TSquareDayUnit = record
    const Symbol       = 'd2';
    const SingularName = 'square day';
    const PluralName   = 'square days';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 7464960000;
  end;
  TSquareDays = specialize TQuantity<TSquareSecondUnit>;
  TSquareDayUnitId = specialize TUnitId<TSquareDayUnit>;

const day2: specialize TQuantity<TSquareSecondUnit> = (FValue: TSquareDayUnit.ToBaseFactor);

type
  { Unit of square hour }
  TSquareHourUnit = record
    const Symbol       = 'h2';
    const SingularName = 'square hour';
    const PluralName   = 'square hours';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 12960000;
  end;
  TSquareHours = specialize TQuantity<TSquareSecondUnit>;
  TSquareHourUnitId = specialize TUnitId<TSquareHourUnit>;

const hr2: specialize TQuantity<TSquareSecondUnit> = (FValue: TSquareHourUnit.ToBaseFactor);

type
  { Unit of square minute }
  TSquareMinuteUnit = record
    const Symbol       = 'min2';
    const SingularName = 'square minute';
    const PluralName   = 'square minutes';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 3600;
  end;
  TSquareMinutes = specialize TQuantity<TSquareSecondUnit>;
  TSquareMinuteUnitId = specialize TUnitId<TSquareMinuteUnit>;

const minute2: specialize TQuantity<TSquareSecondUnit> = (FValue: TSquareMinuteUnit.ToBaseFactor);

type
  { Unit of meter }
  TMeterUnit = record
    const Symbol       = '%sm';
    const SingularName = '%smeter';
    const PluralName   = '%smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TMeters = specialize TQuantity<TMeterUnit>;
  TMeterUnitId = specialize TUnitId<TMeterUnit>;

var m: TMeterUnitId;

const km: specialize TQuantity<TMeterUnit> = (FValue: 1E+03);
const dm: specialize TQuantity<TMeterUnit> = (FValue: 1E-01);
const cm: specialize TQuantity<TMeterUnit> = (FValue: 1E-02);
const mm: specialize TQuantity<TMeterUnit> = (FValue: 1E-03);
const mim: specialize TQuantity<TMeterUnit> = (FValue: 1E-06);
const nm: specialize TQuantity<TMeterUnit> = (FValue: 1E-09);
const pm: specialize TQuantity<TMeterUnit> = (FValue: 1E-12);

type
  { Unit of astronomical }
  TAstronomicalUnit = record
    const Symbol       = 'au';
    const SingularName = 'astronomical unit';
    const PluralName   = 'astronomical units';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 149597870691;
  end;
  TAstronomical = specialize TQuantity<TMeterUnit>;
  TAstronomicalUnitId = specialize TUnitId<TAstronomicalUnit>;

const au: specialize TQuantity<TMeterUnit> = (FValue: TAstronomicalUnit.ToBaseFactor);

type
  { Unit of inch }
  TInchUnit = record
    const Symbol       = 'in';
    const SingularName = 'inch';
    const PluralName   = 'inches';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.0254;
  end;
  TInches = specialize TQuantity<TMeterUnit>;
  TInchUnitId = specialize TUnitId<TInchUnit>;

const inch: specialize TQuantity<TMeterUnit> = (FValue: TInchUnit.ToBaseFactor);

type
  { Unit of foot }
  TFootUnit = record
    const Symbol       = 'ft';
    const SingularName = 'foot';
    const PluralName   = 'feet';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.3048;
  end;
  TFeet = specialize TQuantity<TMeterUnit>;
  TFootUnitId = specialize TUnitId<TFootUnit>;

const ft: specialize TQuantity<TMeterUnit> = (FValue: TFootUnit.ToBaseFactor);

type
  { Unit of yard }
  TYardUnit = record
    const Symbol       = 'yd';
    const SingularName = 'yard';
    const PluralName   = 'yards';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.9144;
  end;
  TYards = specialize TQuantity<TMeterUnit>;
  TYardUnitId = specialize TUnitId<TYardUnit>;

const yd: specialize TQuantity<TMeterUnit> = (FValue: TYardUnit.ToBaseFactor);

type
  { Unit of mile }
  TMileUnit = record
    const Symbol       = 'mi';
    const SingularName = 'mile';
    const PluralName   = 'miles';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 1609.344;
  end;
  TMiles = specialize TQuantity<TMeterUnit>;
  TMileUnitId = specialize TUnitId<TMileUnit>;

const mi: specialize TQuantity<TMeterUnit> = (FValue: TMileUnit.ToBaseFactor);

type
  { Unit of nautical mile }
  TNauticalMileUnit = record
    const Symbol       = 'nmi';
    const SingularName = 'nautical mile';
    const PluralName   = 'nautical miles';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 1852;
  end;
  TNauticalMiles = specialize TQuantity<TMeterUnit>;
  TNauticalMileUnitId = specialize TUnitId<TNauticalMileUnit>;

const nmi: specialize TQuantity<TMeterUnit> = (FValue: TNauticalMileUnit.ToBaseFactor);

type
  { Unit of square meter }
  TSquareMeterUnit = record
    const Symbol       = '%sm2';
    const SingularName = 'square %smeter';
    const PluralName   = 'square %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;
  TSquareMeterUnitId = specialize TUnitId<TSquareMeterUnit>;

var m2: TSquareMeterUnitId;

const km2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E+06);
const dm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-02);
const cm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-04);
const mm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-06);
const mim2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-12);
const nm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-18);
const pm2: specialize TQuantity<TSquareMeterUnit> = (FValue: 1E-24);

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

type
  { Unit of square inch }
  TSquareInchUnit = record
    const Symbol       = 'in2';
    const SingularName = 'square inch';
    const PluralName   = 'square inches';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.00064516;
  end;
  TSquareInches = specialize TQuantity<TSquareMeterUnit>;
  TSquareInchUnitId = specialize TUnitId<TSquareInchUnit>;

const inch2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareInchUnit.ToBaseFactor);

type
  { Unit of square foot }
  TSquareFootUnit = record
    const Symbol       = 'ft2';
    const SingularName = 'square foot';
    const PluralName   = 'square feet';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.09290304;
  end;
  TSquareFeet = specialize TQuantity<TSquareMeterUnit>;
  TSquareFootUnitId = specialize TUnitId<TSquareFootUnit>;

const ft2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareFootUnit.ToBaseFactor);

type
  { Unit of square yard }
  TSquareYardUnit = record
    const Symbol       = 'yd2';
    const SingularName = 'square yard';
    const PluralName   = 'square yards';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.83612736;
  end;
  TSquareYards = specialize TQuantity<TSquareMeterUnit>;
  TSquareYardUnitId = specialize TUnitId<TSquareYardUnit>;

const yd2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareYardUnit.ToBaseFactor);

type
  { Unit of square mile }
  TSquareMileUnit = record
    const Symbol       = 'mi2';
    const SingularName = 'square mile';
    const PluralName   = 'square miles';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 2589988.110336;
  end;
  TSquareMiles = specialize TQuantity<TSquareMeterUnit>;
  TSquareMileUnitId = specialize TUnitId<TSquareMileUnit>;

const mi2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareMileUnit.ToBaseFactor);

type
  { Unit of cubic meter }
  TCubicMeterUnit = record
    const Symbol       = '%sm3';
    const SingularName = 'cubic %smeter';
    const PluralName   = 'cubic %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (3);
  end;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;
  TCubicMeterUnitId = specialize TUnitId<TCubicMeterUnit>;

var m3: TCubicMeterUnitId;

const km3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E+09);
const dm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03);
const cm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-06);
const mm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-09);
const mim3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-18);
const nm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-27);
const pm3: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-36);

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

type
  { Unit of cubic inch }
  TCubicInchUnit = record
    const Symbol       = 'in3';
    const SingularName = 'cubic inch';
    const PluralName   = 'cubic inches';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.000016387064;
  end;
  TCubicInches = specialize TQuantity<TCubicMeterUnit>;
  TCubicInchUnitId = specialize TUnitId<TCubicInchUnit>;

const inch3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicInchUnit.ToBaseFactor);

type
  { Unit of cubic foot }
  TCubicFootUnit = record
    const Symbol       = 'ft3';
    const SingularName = 'cubic foot';
    const PluralName   = 'cubic feet';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.028316846592;
  end;
  TCubicFeet = specialize TQuantity<TCubicMeterUnit>;
  TCubicFootUnitId = specialize TUnitId<TCubicFootUnit>;

const ft3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicFootUnit.ToBaseFactor);

type
  { Unit of cubic yard }
  TCubicYardUnit = record
    const Symbol       = 'yd3';
    const SingularName = 'cubic yard';
    const PluralName   = 'cubic yards';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.764554857984;
  end;
  TCubicYards = specialize TQuantity<TCubicMeterUnit>;
  TCubicYardUnitId = specialize TUnitId<TCubicYardUnit>;

const yd3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicYardUnit.ToBaseFactor);

type
  { Unit of litre }
  TLitreUnit = record
    const Symbol       = '%sL';
    const SingularName = '%slitre';
    const PluralName   = '%slitres';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 1E-03;
  end;
  TLitres = specialize TQuantity<TCubicMeterUnit>;
  TLitreUnitId = specialize TUnitId<TLitreUnit>;

const L: specialize TQuantity<TCubicMeterUnit> = (FValue: TLitreUnit.ToBaseFactor);

const dL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-01);
const cL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-02);
const mL: specialize TQuantity<TCubicMeterUnit> = (FValue: 1E-03 * 1E-03);

type
  { Unit of gallon }
  TGallonUnit = record
    const Symbol       = 'gal';
    const SingularName = 'gallon';
    const PluralName   = 'gallons';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.0037854119678;
  end;
  TGallons = specialize TQuantity<TCubicMeterUnit>;
  TGallonUnitId = specialize TUnitId<TGallonUnit>;

const gal: specialize TQuantity<TCubicMeterUnit> = (FValue: TGallonUnit.ToBaseFactor);

type
  { Unit of quartic meter }
  TQuarticMeterUnit = record
    const Symbol       = '%sm4';
    const SingularName = 'quartic %smeter';
    const PluralName   = 'quartic %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (4);
  end;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;
  TQuarticMeterUnitId = specialize TUnitId<TQuarticMeterUnit>;

var m4: TQuarticMeterUnitId;

const km4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E+12);
const dm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-04);
const cm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-08);
const mm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-12);
const mim4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-24);
const nm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-36);
const pm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: 1E-48);

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters; inline;

type
  { Unit of quintic meter }
  TQuinticMeterUnit = record
    const Symbol       = '%sm5';
    const SingularName = 'quintic %smeter';
    const PluralName   = 'quintic %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (5);
  end;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;
  TQuinticMeterUnitId = specialize TUnitId<TQuinticMeterUnit>;

var m5: TQuinticMeterUnitId;

const km5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E+15);
const dm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-05);
const cm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-10);
const mm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-15);
const mim5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-30);
const nm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-45);
const pm5: specialize TQuantity<TQuinticMeterUnit> = (FValue: 1E-60);

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters; inline;

type
  { Unit of sextic meter }
  TSexticMeterUnit = record
    const Symbol       = '%sm6';
    const SingularName = 'sextic %smeter';
    const PluralName   = 'sextic %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (6);
  end;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;
  TSexticMeterUnitId = specialize TUnitId<TSexticMeterUnit>;

var m6: TSexticMeterUnitId;

const km6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E+18);
const dm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-06);
const cm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-12);
const mm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-18);
const mim6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-36);
const nm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-54);
const pm6: specialize TQuantity<TSexticMeterUnit> = (FValue: 1E-72);

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters; inline;

type
  { Unit of kilogram }
  TKilogramUnit = record
    const Symbol       = '%sg';
    const SingularName = '%sgram';
    const PluralName   = '%sgrams';
    const Prefixes : TPrefixes = (pKilo);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TKilograms = specialize TQuantity<TKilogramUnit>;
  TKilogramUnitId = specialize TUnitId<TKilogramUnit>;

var kg: TKilogramUnitId;

const hg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-01);
const dag: specialize TQuantity<TKilogramUnit> = (FValue: 1E-02);
const g: specialize TQuantity<TKilogramUnit> = (FValue: 1E-03);
const dg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-04);
const cg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-05);
const mg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-06);
const mig: specialize TQuantity<TKilogramUnit> = (FValue: 1E-09);
const ng: specialize TQuantity<TKilogramUnit> = (FValue: 1E-12);
const pg: specialize TQuantity<TKilogramUnit> = (FValue: 1E-15);

type
  { Unit of tonne }
  TTonneUnit = record
    const Symbol       = '%st';
    const SingularName = '%stonne';
    const PluralName   = '%stonnes';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 1E+03;
  end;
  TTonnes = specialize TQuantity<TKilogramUnit>;
  TTonneUnitId = specialize TUnitId<TTonneUnit>;

const tonne: specialize TQuantity<TKilogramUnit> = (FValue: TTonneUnit.ToBaseFactor);

const gigatonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+09);
const megatonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+06);
const kilotonne: specialize TQuantity<TKilogramUnit> = (FValue: 1E+03 * 1E+03);

type
  { Unit of pound }
  TPoundUnit = record
    const Symbol       = 'lb';
    const SingularName = 'pound';
    const PluralName   = 'pounds';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.45359237;
  end;
  TPounds = specialize TQuantity<TKilogramUnit>;
  TPoundUnitId = specialize TUnitId<TPoundUnit>;

const lb: specialize TQuantity<TKilogramUnit> = (FValue: TPoundUnit.ToBaseFactor);

type
  { Unit of ounce }
  TOunceUnit = record
    const Symbol       = 'oz';
    const SingularName = 'ounce';
    const PluralName   = 'ounces';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.028349523125;
  end;
  TOunces = specialize TQuantity<TKilogramUnit>;
  TOunceUnitId = specialize TUnitId<TOunceUnit>;

const oz: specialize TQuantity<TKilogramUnit> = (FValue: TOunceUnit.ToBaseFactor);

type
  { Unit of stone }
  TStoneUnit = record
    const Symbol       = 'st';
    const SingularName = 'stone';
    const PluralName   = 'stones';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 6.35029318;
  end;
  TStones = specialize TQuantity<TKilogramUnit>;
  TStoneUnitId = specialize TUnitId<TStoneUnit>;

const st: specialize TQuantity<TKilogramUnit> = (FValue: TStoneUnit.ToBaseFactor);

type
  { Unit of ton }
  TTonUnit = record
    const Symbol       = 'ton';
    const SingularName = 'ton';
    const PluralName   = 'tons';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 907.18474;
  end;
  TTons = specialize TQuantity<TKilogramUnit>;
  TTonUnitId = specialize TUnitId<TTonUnit>;

const ton: specialize TQuantity<TKilogramUnit> = (FValue: TTonUnit.ToBaseFactor);

type
  { Unit of square kilogram }
  TSquareKilogramUnit = record
    const Symbol       = '%sg2';
    const SingularName = 'square %sgram';
    const PluralName   = 'square %sgrams';
    const Prefixes : TPrefixes = (pKilo);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;
  TSquareKilogramUnitId = specialize TUnitId<TSquareKilogramUnit>;

var kg2: TSquareKilogramUnitId;

const hg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-02);
const dag2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-04);
const g2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-06);
const dg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-08);
const cg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-10);
const mg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-12);
const mig2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-18);
const ng2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-24);
const pg2: specialize TQuantity<TSquareKilogramUnit> = (FValue: 1E-30);

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

type
  { Unit of ampere }
  TAmpereUnit = record
    const Symbol       = '%sA';
    const SingularName = '%sampere';
    const PluralName   = '%samperes';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TAmperes = specialize TQuantity<TAmpereUnit>;
  TAmpereUnitId = specialize TUnitId<TAmpereUnit>;

var A: TAmpereUnitId;

const kA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+03);
const hA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+02);
const daA: specialize TQuantity<TAmpereUnit> = (FValue: 1E+01);
const dA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-01);
const cA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-02);
const mA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-03);
const miA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-06);
const nA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-09);
const picoA: specialize TQuantity<TAmpereUnit> = (FValue: 1E-12);

type
  { Unit of square ampere }
  TSquareAmpereUnit = record
    const Symbol       = '%sA2';
    const SingularName = 'square %sampere';
    const PluralName   = 'square %samperes';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;
  TSquareAmpereUnitId = specialize TUnitId<TSquareAmpereUnit>;

var A2: TSquareAmpereUnitId;

const kA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+06);
const hA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+04);
const daA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E+02);
const dA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-02);
const cA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-04);
const mA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-06);
const miA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-12);
const nA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-18);
const picoA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: 1E-24);

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

type
  { Unit of kelvin }
  TKelvinUnit = record
    const Symbol       = '%sK';
    const SingularName = '%skelvin';
    const PluralName   = '%skelvins';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TKelvins = specialize TQuantity<TKelvinUnit>;
  TKelvinUnitId = specialize TUnitId<TKelvinUnit>;

var K: TKelvinUnitId;

type
  { Unit of degree celsius }
  TDegreeCelsiusUnit = record
    const Symbol       = 'ºC';
    const SingularName = 'degree Celsius';
    const PluralName   = 'degrees Celsius';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
  end;
  TDegreesCelsius = specialize TQuantity<TDegreeCelsiusUnit>;
  TDegreeCelsiusUnitId = specialize TUnitId<TDegreeCelsiusUnit>;

var degC: TDegreeCelsiusUnitId;

type
  { Unit of degree fahrenheit }
  TDegreeFahrenheitUnit = record
    const Symbol       = 'ºF';
    const SingularName = 'degree Fahrenheit';
    const PluralName   = 'degrees Fahrenheit';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
  end;
  TDegreesFahrenheit = specialize TQuantity<TDegreeFahrenheitUnit>;
  TDegreeFahrenheitUnitId = specialize TUnitId<TDegreeFahrenheitUnit>;

var degF: TDegreeFahrenheitUnitId;

type
  { Unit of square kelvin }
  TSquareKelvinUnit = record
    const Symbol       = '%sK2';
    const SingularName = 'square %skelvin';
    const PluralName   = 'square %skelvins';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;
  TSquareKelvinUnitId = specialize TUnitId<TSquareKelvinUnit>;

var K2: TSquareKelvinUnitId;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

type
  { Unit of cubic kelvin }
  TCubicKelvinUnit = record
    const Symbol       = '%sK3';
    const SingularName = 'cubic %skelvin';
    const PluralName   = 'cubic %skelvins';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (3);
  end;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;
  TCubicKelvinUnitId = specialize TUnitId<TCubicKelvinUnit>;

var K3: TCubicKelvinUnitId;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

type
  { Unit of quartic kelvin }
  TQuarticKelvinUnit = record
    const Symbol       = '%sK4';
    const SingularName = 'quartic %skelvin';
    const PluralName   = 'quartic %skelvins';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (4);
  end;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;
  TQuarticKelvinUnitId = specialize TUnitId<TQuarticKelvinUnit>;

var K4: TQuarticKelvinUnitId;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

type
  { Unit of mole }
  TMoleUnit = record
    const Symbol       = '%smol';
    const SingularName = '%smole';
    const PluralName   = '%smoles';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TMoles = specialize TQuantity<TMoleUnit>;
  TMoleUnitId = specialize TUnitId<TMoleUnit>;

var mol: TMoleUnitId;

const kmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+03);
const hmol: specialize TQuantity<TMoleUnit> = (FValue: 1E+02);
const damol: specialize TQuantity<TMoleUnit> = (FValue: 1E+01);

type
  { Unit of candela }
  TCandelaUnit = record
    const Symbol       = '%scd';
    const SingularName = '%scandela';
    const PluralName   = '%scandelas';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TCandelas = specialize TQuantity<TCandelaUnit>;
  TCandelaUnitId = specialize TUnitId<TCandelaUnit>;

var cd: TCandelaUnitId;

type
  { Unit of radian }
  TRadianUnit = record
    const Symbol       = 'rad';
    const SingularName = 'radian';
    const PluralName   = 'radians';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
  end;
  TRadians = specialize TQuantity<TRadianUnit>;
  TRadianUnitId = specialize TUnitId<TRadianUnit>;

var rad: TRadianUnitId;

type
  { Unit of degree }
  TDegreeUnit = record
    const Symbol       = 'deg';
    const SingularName = 'degree';
    const PluralName   = 'degrees';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = Pi/180;
  end;
  TDegrees = specialize TQuantity<TRadianUnit>;
  TDegreeUnitId = specialize TUnitId<TDegreeUnit>;

const deg: specialize TQuantity<TRadianUnit> = (FValue: TDegreeUnit.ToBaseFactor);

type
  { Unit of steradian }
  TSteradianUnit = record
    const Symbol       = 'sr';
    const SingularName = 'steradian';
    const PluralName   = 'steradians';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
  end;
  TSteradians = specialize TQuantity<TSteradianUnit>;
  TSteradianUnitId = specialize TUnitId<TSteradianUnit>;

var sr: TSteradianUnitId;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

type
  { Unit of square degree }
  TSquareDegreeUnit = record
    const Symbol       = 'deg2';
    const SingularName = 'square degree';
    const PluralName   = 'square degrees';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = Pi*Pi/32400;
  end;
  TSquareDegrees = specialize TQuantity<TSteradianUnit>;
  TSquareDegreeUnitId = specialize TUnitId<TSquareDegreeUnit>;

const deg2: specialize TQuantity<TSteradianUnit> = (FValue: TSquareDegreeUnit.ToBaseFactor);

type
  { Unit of hertz }
  THertzUnit = record
    const Symbol       = '%sHz';
    const SingularName = '%shertz';
    const PluralName   = '%shertz';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  THertz = specialize TQuantity<THertzUnit>;
  THertzUnitId = specialize TUnitId<THertzUnit>;

var Hz: THertzUnitId;

const THz: specialize TQuantity<THertzUnit> = (FValue: 1E+12);
const GHz: specialize TQuantity<THertzUnit> = (FValue: 1E+09);
const MHz: specialize TQuantity<THertzUnit> = (FValue: 1E+06);
const kHz: specialize TQuantity<THertzUnit> = (FValue: 1E+03);

// main definition [ Hz ] = [ 1 ] / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator /(const ALeft: double; const ARight: TSecondUnitId): THertz; inline;

type
  { Unit of square hertz }
  TSquareHertzUnit = record
    const Symbol       = '%sHz2';
    const SingularName = 'square %shertz';
    const PluralName   = 'square %shertz';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;
  TSquareHertzUnitId = specialize TUnitId<TSquareHertzUnit>;

var Hz2: TSquareHertzUnitId;

const THz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+24);
const GHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+18);
const MHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+12);
const kHz2: specialize TQuantity<TSquareHertzUnit> = (FValue: 1E+06);

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]
operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double; inline;
operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds; inline;
operator /(const ALeft: double; const ARight: TSquareSecondUnitId): TSquareHertz; inline;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]
operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds; inline;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

type
  { Unit of radian per second }
  TRadianPerSecondUnit = record
    const Symbol       = 'rad/%ss';
    const SingularName = 'radian per %ssecond';
    const PluralName   = 'radians per %ssecond';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-1);
  end;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;
  TRadianPerSecondUnitId = specialize TUnitId<TRadianPerSecondUnit>;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator /(const ALeft: TRadians; const ARight: TSecondUnitId): TRadiansPerSecond; inline;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]
operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians; inline;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;

type
  { Unit of radian per second squared }
  TRadianPerSecondSquaredUnit = record
    const Symbol       = 'rad/%ss2';
    const SingularName = 'radian per %ssecond squared';
    const PluralName   = 'radians per %ssecond squared';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-2);
  end;
  TRadiansPerSecondSquared = specialize TQuantity<TSquareHertzUnit>;
  TRadianPerSecondSquaredUnitId = specialize TUnitId<TSquareHertzUnit>;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSecondSquared; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecondSquared): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecondSquared; const ARight: TSeconds): TRadiansPerSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecondSquared): TSeconds; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TSecondUnitId): TRadiansPerSecondSquared; inline;

// alternative definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSecondSquared; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSecondSquared): TRadians; inline;
operator *(const ALeft: TRadiansPerSecondSquared; const ARight: TSquareSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecondSquared): TSquareSeconds; inline;
operator /(const ALeft: TRadians; const ARight: TSquareSecondUnitId): TRadiansPerSecondSquared; inline;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]
operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSecondSquared; inline;
operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSecondSquared; inline;
operator /(const ALeft: TRadiansPerSecondSquared; const ARight: TRadians): TSquareHertz; inline;
operator /(const ALeft: TRadiansPerSecondSquared; const ARight: TSquareHertz): TRadians; inline;

type
  { Unit of steradian per square second }
  TSteradianPerSquareSecondUnit = record
    const Symbol       = 'rad2/%ss2';
    const SingularName = 'square radian per square %ssecond';
    const PluralName   = 'square radians per square %ssecond';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-2);
  end;
  TSteradiansPerSquareSecond = specialize TQuantity<TSquareHertzUnit>;
  TSteradianPerSquareSecondUnitId = specialize TUnitId<TSquareHertzUnit>;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSteradians; const ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond; inline;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]
operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians; inline;

type
  { Unit of meter per second }
  TMeterPerSecondUnit = record
    const Symbol       = '%sm/%ss';
    const SingularName = '%smeter per %ssecond';
    const PluralName   = '%smeters per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerSecondUnitId = specialize TUnitId<TMeterPerSecondUnit>;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TMeters; const ARight: TSecondUnitId): TMetersPerSecond; inline;

// alternative definition [ m/s ] = [ m ] * [ Hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: THertzUnitId): TMetersPerSecond; inline;

type
  { Unit of meter per hour }
  TMeterPerHourUnit = record
    const Symbol       = '%sm/h';
    const SingularName = '%smeter per hour';
    const PluralName   = '%smeters per hour';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 1/3600;
  end;
  TMetersPerHour = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerHourUnitId = specialize TUnitId<TMeterPerHourUnit>;

type
  { Unit of mile per hour }
  TMilePerHourUnit = record
    const Symbol       = 'mi/h';
    const SingularName = 'mile per hour';
    const PluralName   = 'miles per hour';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.44704;
  end;
  TMilesPerHour = specialize TQuantity<TMeterPerSecondUnit>;
  TMilePerHourUnitId = specialize TUnitId<TMilePerHourUnit>;

type
  { Unit of nautical mile per hour }
  TNauticalMilePerHourUnit = record
    const Symbol       = 'nmi/h';
    const SingularName = 'nautical mile per hour';
    const PluralName   = 'nautical miles per hour';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 463/900;
  end;
  TNauticalMilesPerHour = specialize TQuantity<TMeterPerSecondUnit>;
  TNauticalMilePerHourUnitId = specialize TUnitId<TNauticalMilePerHourUnit>;

type
  { Unit of meter per second squared }
  TMeterPerSecondSquaredUnit = record
    const Symbol       = '%sm/%ss2';
    const SingularName = '%smeter per %ssecond squared';
    const PluralName   = '%smeters per %ssecond squared';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TMetersPerSecondSquared = specialize TQuantity<TMeterPerSecondSquaredUnit>;
  TMeterPerSecondSquaredUnitId = specialize TUnitId<TMeterPerSecondSquaredUnit>;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSecondSquared; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecondSquared): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecondSquared): TSeconds; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TSecondUnitId): TMetersPerSecondSquared; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSecondSquared; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSecondSquared): TMeters; inline;
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSquareSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecondSquared): TSquareSeconds; inline;
operator /(const ALeft: TMeters; const ARight: TSquareSecondUnitId): TMetersPerSecondSquared; inline;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]
operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSecondSquared; inline;
operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSecondSquared; inline;
operator /(const ALeft: TMetersPerSecondSquared; const ARight: TSquareHertz): TMeters; inline;
operator /(const ALeft: TMetersPerSecondSquared; const ARight: TMeters): TSquareHertz; inline;

type
  { Unit of meter per second per second }
  TMeterPerSecondPerSecondUnit = record
    const Symbol       = '%sm/%ss/%ss';
    const SingularName = '%smeter per %ssecond per %ssecond';
    const PluralName   = '%smeters per %ssecond per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1, -1);
  end;
  TMetersPerSecondPerSecond = specialize TQuantity<TMeterPerSecondSquaredUnit>;
  TMeterPerSecondPerSecondUnitId = specialize TUnitId<TMeterPerSecondSquaredUnit>;

type
  { Unit of meter per hour per second }
  TMeterPerHourPerSecondUnit = record
    const Symbol       = '%sm/h/%ss';
    const SingularName = '%smeter per hour per %ssecond';
    const PluralName   = '%smeters per hour per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
    const ToBaseFactor = 1/3600;
  end;
  TMetersPerHourPerSecond = specialize TQuantity<TMeterPerSecondSquaredUnit>;
  TMeterPerHourPerSecondUnitId = specialize TUnitId<TMeterPerHourPerSecondUnit>;

type
  { Unit of square meter per square second }
  TSquareMeterPerSquareSecondUnit = record
    const Symbol       = '%sm2/%ss2';
    const SingularName = 'square %smeter per square %ssecond';
    const PluralName   = 'square %smeters per square %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSquareMeterPerSquareSecondUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSecondSquared): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecondSquared): TMeters; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSecondSquared; inline;

type
  { Unit of kilogram meter per second }
  TKilogramMeterPerSecondUnit = record
    const Symbol       = '%sg·%sm/%ss';
    const SingularName = '%sgram %smeter per %ssecond';
    const PluralName   = '%sgram %smeters per %ssecond';
    const Prefixes : TPrefixes = (pKilo, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1, -1);
  end;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TKilogramMeterPerSecondUnitId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;
operator *(const ALeft: TKilograms; const ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond; inline;

type
  { Unit of newton second }
  TNewtonSecondUnit = record
    const Symbol       = '%sN·%ss';
    const SingularName = '%snewton %ssecond';
    const PluralName   = '%snewton %sseconds';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TNewtonSeconds = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TNewtonSecondUnitId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

type
  { Unit of kilogram square meter }
  TKilogramSquareMeterUnit = record
    const Symbol       = '%sg·%sm2';
    const SingularName = '%sgram square %smeter';
    const PluralName   = '%sgram square %smeters';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2);
  end;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;
  TKilogramSquareMeterUnitId = specialize TUnitId<TKilogramSquareMeterUnit>;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareMeterUnitId): TKilogramSquareMeters; inline;

type
  { Unit of kilogram square meter per second }
  TKilogramSquareMeterPerSecondUnit = record
    const Symbol       = '%sg·%sm2/%ss';
    const SingularName = '%sgram square %smeter per %ssecond';
    const PluralName   = '%sgram square %smeters per %ssecond';
    const Prefixes : TPrefixes = (pKilo, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2, -1);
  end;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMeterPerSecondUnitId = specialize TUnitId<TKilogramSquareMeterPerSecondUnit>;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSecondUnitId): TKilogramSquareMetersPerSecond; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters; inline;

type
  { Unit of kilogram per meter }
  TKilogramPerMeterUnit = record
    const Symbol       = '%sg/%sm';
    const SingularName = '%sgram per %smeter';
    const PluralName   = '%sgrams per %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;
  TKilogramPerMeterUnitId = specialize TUnitId<TKilogramPerMeterUnit>;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TKilograms; const ARight: TMeterUnitId): TKilogramsPerMeter; inline;

type
  { Unit of kilogram per square meter }
  TKilogramPerSquareMeterUnit = record
    const Symbol       = '%sg/%sm2';
    const SingularName = '%sgram per square %smeter';
    const PluralName   = '%sgrams per square %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;
  TKilogramPerSquareMeterUnitId = specialize TUnitId<TKilogramPerSquareMeterUnit>;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TKilograms; const ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter; inline;

type
  { Unit of kilogram per cubic meter }
  TKilogramPerCubicMeterUnit = record
    const Symbol       = '%sg/%sm3';
    const SingularName = '%sgram per cubic %smeter';
    const PluralName   = '%sgrams per cubic %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;
  TKilogramPerCubicMeterUnitId = specialize TUnitId<TKilogramPerCubicMeterUnit>;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TKilograms; const ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter; inline;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters; inline;

type
  { Unit of newton }
  TNewtonUnit = record
    const Symbol       = '%sN';
    const SingularName = '%snewton';
    const PluralName   = '%snewtons';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TNewtons = specialize TQuantity<TNewtonUnit>;
  TNewtonUnitId = specialize TUnitId<TNewtonUnit>;

var N: TNewtonUnitId;

const GN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+09);
const MN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+06);
const kN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+03);
const hN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+02);
const daN: specialize TQuantity<TNewtonUnit> = (FValue: 1E+01);

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecondSquared): TNewtons; inline;
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSecondSquared; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSecondSquared): TKilograms; inline;
operator *(const ALeft: TKilograms; const ARight: TMeterPerSecondSquaredUnitId): TNewtons; inline;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerMeter; const ARight: TSquareMetersPerSquareSecond): TNewtons; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerMeter): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilogramsPerMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerMeter; inline;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond; inline;
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds; inline;

type
  { Unit of pound force }
  TPoundForceUnit = record
    const Symbol       = 'lbf';
    const SingularName = 'pound-force';
    const PluralName   = 'pounds-force';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 4.4482216152605;
  end;
  TPoundsForce = specialize TQuantity<TNewtonUnit>;
  TPoundForceUnitId = specialize TUnitId<TPoundForceUnit>;

const lbf: specialize TQuantity<TNewtonUnit> = (FValue: TPoundForceUnit.ToBaseFactor);

type
  { Unit of square newton }
  TSquareNewtonUnit = record
    const Symbol       = '%sN2';
    const SingularName = 'square %snewton';
    const PluralName   = 'square %snewtons';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareNewtons = specialize TQuantity<TSquareNewtonUnit>;
  TSquareNewtonUnitId = specialize TUnitId<TSquareNewtonUnit>;

var N2: TSquareNewtonUnitId;

const GN2: specialize TQuantity<TSquareNewtonUnit> = (FValue: 1E+18);
const MN2: specialize TQuantity<TSquareNewtonUnit> = (FValue: 1E+12);
const kN2: specialize TQuantity<TSquareNewtonUnit> = (FValue: 1E+06);
const hN2: specialize TQuantity<TSquareNewtonUnit> = (FValue: 1E+04);
const daN2: specialize TQuantity<TSquareNewtonUnit> = (FValue: 1E+02);

// main definition [ N2 ] = [ N ] * [ N ]
operator *(const ALeft: TNewtons; const ARight: TNewtons): TSquareNewtons; inline;
operator /(const ALeft: TSquareNewtons; const ARight: TNewtons): TNewtons; inline;

type
  { Unit of pascal }
  TPascalUnit = record
    const Symbol       = '%sPa';
    const SingularName = '%spascal';
    const PluralName   = '%spascals';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TPascals = specialize TQuantity<TPascalUnit>;
  TPascalUnitId = specialize TUnitId<TPascalUnit>;

var Pa: TPascalUnitId;

const TPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+12);
const GPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+09);
const MPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+06);
const kPa: specialize TQuantity<TPascalUnit> = (FValue: 1E+03);

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareMeterUnitId): TPascals; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of bar }
  TBarUnit = record
    const Symbol       = '%sbar';
    const SingularName = '%sbar';
    const PluralName   = '%sbars';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 1E+05;
  end;
  TBars = specialize TQuantity<TPascalUnit>;
  TBarUnitId = specialize TUnitId<TBarUnit>;

const bar: specialize TQuantity<TPascalUnit> = (FValue: TBarUnit.ToBaseFactor);

const kbar: specialize TQuantity<TPascalUnit> = (FValue: 1E+05 * 1E+03);
const mbar: specialize TQuantity<TPascalUnit> = (FValue: 1E+05 * 1E-03);

type
  { Unit of pound per square inch }
  TPoundPerSquareInchUnit = record
    const Symbol       = '%spsi';
    const SingularName = '%spound per square inch';
    const PluralName   = '%spounds per square inch';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 6894.75729316836;
  end;
  TPoundsPerSquareInch = specialize TQuantity<TPascalUnit>;
  TPoundPerSquareInchUnitId = specialize TUnitId<TPoundPerSquareInchUnit>;

const psi: specialize TQuantity<TPascalUnit> = (FValue: TPoundPerSquareInchUnit.ToBaseFactor);

const kpsi: specialize TQuantity<TPascalUnit> = (FValue: 6894.75729316836 * 1E+03);

type
  { Unit of joule per cubic meter }
  TJoulePerCubicMeterUnit = record
    const Symbol       = '%sJ/%sm3';
    const SingularName = '%sjoule per cubic %smeter';
    const PluralName   = '%sjoules per cubic %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TJoulesPerCubicMeter = specialize TQuantity<TPascalUnit>;
  TJoulePerCubicMeterUnitId = specialize TUnitId<TPascalUnit>;

type
  { Unit of joule }
  TJouleUnit = record
    const Symbol       = '%sJ';
    const SingularName = '%sjoule';
    const PluralName   = '%sjoules';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TJoules = specialize TQuantity<TJouleUnit>;
  TJouleUnitId = specialize TUnitId<TJouleUnit>;

var J: TJouleUnitId;

const TJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+12);
const GJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+09);
const MJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+06);
const kJ: specialize TQuantity<TJouleUnit> = (FValue: 1E+03);

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const ARight: TMeterUnitId): TJoules; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TJoules; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramMetersPerSecond): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramMetersPerSecond): TMetersPerSecond; inline;
operator /(const ALeft: TJoules; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramUnitId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSquareHertz): TJoules; inline;
operator *(const ALeft: TSquareHertz; const ARight: TKilogramSquareMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSquareHertz; inline;
operator /(const ALeft: TJoules; const ARight: TSquareHertz): TKilogramSquareMeters; inline;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareSeconds): TJoules; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TJoules): TKilogramSquareMeters; inline;
operator *(const ALeft: TJoules; const ARight: TSquareSeconds): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TJoules): TSquareSeconds; inline;

type
  { Unit of watt hour }
  TWattHourUnit = record
    const Symbol       = '%sW·h';
    const SingularName = '%swatt hour';
    const PluralName   = '%swatt hours';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 3600;
  end;
  TWattHours = specialize TQuantity<TJouleUnit>;
  TWattHourUnitId = specialize TUnitId<TWattHourUnit>;

type
  { Unit of elettronvolt }
  TElettronvoltUnit = record
    const Symbol       = '%seV';
    const SingularName = '%selettronvolt';
    const PluralName   = '%selettronvolts';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 1.60217742320523E-019;
  end;
  TElettronvolts = specialize TQuantity<TJouleUnit>;
  TElettronvoltUnitId = specialize TUnitId<TElettronvoltUnit>;

const eV: specialize TQuantity<TJouleUnit> = (FValue: TElettronvoltUnit.ToBaseFactor);

const TeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+12);
const GeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+09);
const MeV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+06);
const keV: specialize TQuantity<TJouleUnit> = (FValue: 1.60217742320523E-019 * 1E+03);

type
  { Unit of newton meter }
  TNewtonMeterUnit = record
    const Symbol       = '%sN·%sm';
    const SingularName = '%snewton %smeter';
    const PluralName   = '%snewton %smeters';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TNewtonMeters = specialize TQuantity<TJouleUnit>;
  TNewtonMeterUnitId = specialize TUnitId<TJouleUnit>;

type
  { Unit of pound force inch }
  TPoundForceInchUnit = record
    const Symbol       = 'lbf·in';
    const SingularName = 'pound-force inch';
    const PluralName   = 'pound-force inches';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 0.112984829027617;
  end;
  TPoundForceInches = specialize TQuantity<TJouleUnit>;
  TPoundForceInchUnitId = specialize TUnitId<TPoundForceInchUnit>;

type
  { Unit of watt }
  TWattUnit = record
    const Symbol       = '%sW';
    const SingularName = '%swatt';
    const PluralName   = '%swatts';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TWatts = specialize TQuantity<TWattUnit>;
  TWattUnitId = specialize TUnitId<TWattUnit>;

var W: TWattUnitId;

const TW: specialize TQuantity<TWattUnit> = (FValue: 1E+12);
const GW: specialize TQuantity<TWattUnit> = (FValue: 1E+09);
const MW: specialize TQuantity<TWattUnit> = (FValue: 1E+06);
const kW: specialize TQuantity<TWattUnit> = (FValue: 1E+03);
const milliW: specialize TQuantity<TWattUnit> = (FValue: 1E-03);

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator /(const ALeft: TJoules; const ARight: TSecondUnitId): TWatts; inline;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJoules; const ARight: THertz): TWatts; inline;
operator *(const ALeft: THertz; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): THertz; inline;
operator /(const ALeft: TWatts; const ARight: THertz): TJoules; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond; inline;
operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons; inline;

type
  { Unit of coulomb }
  TCoulombUnit = record
    const Symbol       = '%sC';
    const SingularName = '%scoulomb';
    const PluralName   = '%scoulombs';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TCoulombs = specialize TQuantity<TCoulombUnit>;
  TCoulombUnitId = specialize TUnitId<TCoulombUnit>;

var C: TCoulombUnitId;

const kC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+03);
const hC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+02);
const daC: specialize TQuantity<TCoulombUnit> = (FValue: 1E+01);
const dC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-01);
const cC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-02);
const mC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-03);
const miC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-06);
const nC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-09);
const pC: specialize TQuantity<TCoulombUnit> = (FValue: 1E-12);

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TAmpereUnitId): TCoulombs; inline;

type
  { Unit of ampere hour }
  TAmpereHourUnit = record
    const Symbol       = '%sA·h';
    const SingularName = '%sampere hour';
    const PluralName   = '%sampere hours';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 3600;
  end;
  TAmpereHours = specialize TQuantity<TCoulombUnit>;
  TAmpereHourUnitId = specialize TUnitId<TAmpereHourUnit>;

type
  { Unit of square coulomb }
  TSquareCoulombUnit = record
    const Symbol       = '%sC2';
    const SingularName = 'square %scoulomb';
    const PluralName   = 'square %scoulombs';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;
  TSquareCoulombUnitId = specialize TUnitId<TSquareCoulombUnit>;

var C2: TSquareCoulombUnitId;

const kC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+06);
const hC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+04);
const daC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E+02);
const dC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-02);
const cC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-04);
const mC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-06);
const miC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-12);
const nC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-18);
const pC2: specialize TQuantity<TSquareCoulombUnit> = (FValue: 1E-24);

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

type
  { Unit of volt }
  TVoltUnit = record
    const Symbol       = '%sV';
    const SingularName = '%svolt';
    const PluralName   = '%svolts';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TVolts = specialize TQuantity<TVoltUnit>;
  TVoltUnitId = specialize TUnitId<TVoltUnit>;

var V: TVoltUnitId;

const kV: specialize TQuantity<TVoltUnit> = (FValue: 1E+03);
const mV: specialize TQuantity<TVoltUnit> = (FValue: 1E-03);

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator /(const ALeft: TWatts; const ARight: TAmpereUnitId): TVolts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TJoules; const ARight: TCoulombUnitId): TVolts; inline;

type
  { Unit of square volt }
  TSquareVoltUnit = record
    const Symbol       = '%sV2';
    const SingularName = 'square %svolt';
    const PluralName   = 'square %svolts';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;
  TSquareVoltUnitId = specialize TUnitId<TSquareVoltUnit>;

var V2: TSquareVoltUnitId;

const kV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E+06);
const mV2: specialize TQuantity<TSquareVoltUnit> = (FValue: 1E-06);

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

type
  { Unit of farad }
  TFaradUnit = record
    const Symbol       = '%sF';
    const SingularName = '%sfarad';
    const PluralName   = '%sfarads';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TFarads = specialize TQuantity<TFaradUnit>;
  TFaradUnitId = specialize TUnitId<TFaradUnit>;

var F: TFaradUnitId;

const mF: specialize TQuantity<TFaradUnit> = (FValue: 1E-03);
const miF: specialize TQuantity<TFaradUnit> = (FValue: 1E-06);
const nF: specialize TQuantity<TFaradUnit> = (FValue: 1E-09);
const pF: specialize TQuantity<TFaradUnit> = (FValue: 1E-12);

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator /(const ALeft: TCoulombs; const ARight: TVoltUnitId): TFarads; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;

type
  { Unit of ohm }
  TOhmUnit = record
    const Symbol       = '%sΩ';
    const SingularName = '%sohm';
    const PluralName   = '%sohms';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TOhms = specialize TQuantity<TOhmUnit>;
  TOhmUnitId = specialize TUnitId<TOhmUnit>;

var ohm: TOhmUnitId;

const Gohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+09);
const megaohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+06);
const kohm: specialize TQuantity<TOhmUnit> = (FValue: 1E+03);
const mohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-03);
const miohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-06);
const nohm: specialize TQuantity<TOhmUnit> = (FValue: 1E-09);

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator /(const ALeft: TVolts; const ARight: TAmpereUnitId): TOhms; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms; inline;
operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds; inline;
operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds; inline;
operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads; inline;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms; inline;
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts; inline;
operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes; inline;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;

type
  { Unit of siemens }
  TSiemensUnit = record
    const Symbol       = '%sS';
    const SingularName = '%ssiemens';
    const PluralName   = '%ssiemens';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TSiemens = specialize TQuantity<TSiemensUnit>;
  TSiemensUnitId = specialize TUnitId<TSiemensUnit>;

var siemens: TSiemensUnitId;

const millisiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-03);
const microsiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-06);
const nanosiemens: specialize TQuantity<TSiemensUnit> = (FValue: 1E-09);

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator /(const ALeft: double; const ARight: TOhmUnitId): TSiemens; inline;

type
  { Unit of weber }
  TWeberUnit = record
    const Symbol       = '%sWb';
    const SingularName = '%sweber';
    const PluralName   = '%swebers';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TWebers = specialize TQuantity<TWeberUnit>;
  TWeberUnitId = specialize TUnitId<TWeberUnit>;

var Wb: TWeberUnitId;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;
operator *(const ALeft: TVolts; const ARight: TSecondUnitId): TWebers; inline;

type
  { Unit of tesla }
  TTeslaUnit = record
    const Symbol       = '%sT';
    const SingularName = '%stesla';
    const PluralName   = '%steslas';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TTeslas = specialize TQuantity<TTeslaUnit>;
  TTeslaUnitId = specialize TUnitId<TTeslaUnit>;

var T: TTeslaUnitId;

const mT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-03);
const miT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-06);
const nT: specialize TQuantity<TTeslaUnit> = (FValue: 1E-09);

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator /(const ALeft: TWebers; const ARight: TSquareMeterUnitId): TTeslas; inline;

type
  { Unit of henry }
  THenryUnit = record
    const Symbol       = '%sH';
    const SingularName = '%shenry';
    const PluralName   = '%shenries';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  THenries = specialize TQuantity<THenryUnit>;
  THenryUnitId = specialize TUnitId<THenryUnit>;

var H: THenryUnitId;

const mH: specialize TQuantity<THenryUnit> = (FValue: 1E-03);
const miH: specialize TQuantity<THenryUnit> = (FValue: 1E-06);
const nH: specialize TQuantity<THenryUnit> = (FValue: 1E-09);

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenries; inline;
operator *(const ALeft: TAmperes; const ARight: THenries): TWebers; inline;
operator *(const ALeft: THenries; const ARight: TAmperes): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: THenries): TAmperes; inline;
operator /(const ALeft: TWebers; const ARight: TAmpereUnitId): THenries; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenries; inline;
operator *(const ALeft: TSeconds; const ARight: TOhms): THenries; inline;
operator /(const ALeft: THenries; const ARight: TOhms): TSeconds; inline;
operator /(const ALeft: THenries; const ARight: TSeconds): TOhms; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenries; inline;
operator *(const ALeft: THertz; const ARight: THenries): TOhms; inline;
operator *(const ALeft: THenries; const ARight: THertz): TOhms; inline;
operator /(const ALeft: TOhms; const ARight: THenries): THertz; inline;

type
  { Unit of lumen }
  TLumenUnit = record
    const Symbol       = '%slm';
    const SingularName = '%slumen';
    const PluralName   = '%slumens';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TLumens = specialize TQuantity<TLumenUnit>;
  TLumenUnitId = specialize TUnitId<TLumenUnit>;

var lm: TLumenUnitId;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;
operator *(const ALeft: TCandelas; const ARight: TSteradianUnitId): TLumens; inline;

type
  { Unit of lux }
  TLuxUnit = record
    const Symbol       = '%slx';
    const SingularName = '%slux';
    const PluralName   = '%slux';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TLux = specialize TQuantity<TLuxUnit>;
  TLuxUnitId = specialize TUnitId<TLuxUnit>;

var lx: TLuxUnitId;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator /(const ALeft: TLumens; const ARight: TSquareMeterUnitId): TLux; inline;

type
  { Unit of bequerel }
  TBequerelUnit = record
    const Symbol       = '%sBq';
    const SingularName = '%sbequerel';
    const PluralName   = '%sbequerels';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TBequerels = specialize TQuantity<THertzUnit>;
  TBequerelUnitId = specialize TUnitId<THertzUnit>;

var Bq: TBequerelUnitId;

const kBq: specialize TQuantity<THertzUnit> = (FValue: 1E+03);
const mBq: specialize TQuantity<THertzUnit> = (FValue: 1E-03);
const miBq: specialize TQuantity<THertzUnit> = (FValue: 1E-06);
const nBq: specialize TQuantity<THertzUnit> = (FValue: 1E-09);
const pBq: specialize TQuantity<THertzUnit> = (FValue: 1E-12);

type
  { Unit of gray }
  TGrayUnit = record
    const Symbol       = '%sGy';
    const SingularName = '%sgray';
    const PluralName   = '%sgrays';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TGrays = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TGrayUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

var Gy: TGrayUnitId;

const kGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+03);
const mGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-03);
const miGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-06);
const nGy: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-09);

type
  { Unit of sievert }
  TSievertUnit = record
    const Symbol       = '%sSv';
    const SingularName = '%ssievert';
    const PluralName   = '%ssieverts';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TSieverts = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSievertUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

var Sv: TSievertUnitId;

const kSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E+03);
const mSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-03);
const miSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-06);
const nSv: specialize TQuantity<TSquareMeterPerSquareSecondUnit> = (FValue: 1E-09);

type
  { Unit of katal }
  TKatalUnit = record
    const Symbol       = '%skat';
    const SingularName = '%skatal';
    const PluralName   = '%skatals';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TKatals = specialize TQuantity<TKatalUnit>;
  TKatalUnitId = specialize TUnitId<TKatalUnit>;

var kat: TKatalUnitId;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator /(const ALeft: TMoles; const ARight: TSecondUnitId): TKatals; inline;

type
  { Unit of joule per radian }
  TJoulePerRadianUnit = record
    const Symbol       = '%sJ/rad';
    const SingularName = '%sjoule per radian';
    const PluralName   = '%sjoules per radian';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerRadianUnitId = specialize TUnitId<TJoulePerRadianUnit>;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator /(const ALeft: TJoules; const ARight: TRadianUnitId): TJoulesPerRadian; inline;

type
  { Unit of joule per degree }
  TJoulePerDegreeUnit = record
    const Symbol       = '%sJ/deg';
    const SingularName = '%sjoule per degree';
    const PluralName   = '%sjoules per degree';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
    const ToBaseFactor = 180/Pi;
  end;
  TJoulesPerDegree = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerDegreeUnitId = specialize TUnitId<TJoulePerDegreeUnit>;

type
  { Unit of newton meter per radian }
  TNewtonMeterPerRadianUnit = record
    const Symbol       = '%sN·%sm/rad';
    const SingularName = '%snewton %smeter per radian';
    const PluralName   = '%snewton %smeters per radian';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TNewtonMetersPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TNewtonMeterPerRadianUnitId = specialize TUnitId<TJoulePerRadianUnit>;

type
  { Unit of newton meter per degree }
  TNewtonMeterPerDegreeUnit = record
    const Symbol       = '%sN·%sm/deg';
    const SingularName = '%snewton %smeter per degree';
    const PluralName   = '%snewton %smeters per degree';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
    const ToBaseFactor = 180/Pi;
  end;
  TNewtonMetersPerDegree = specialize TQuantity<TJoulePerRadianUnit>;
  TNewtonMeterPerDegreeUnitId = specialize TUnitId<TNewtonMeterPerDegreeUnit>;

type
  { Unit of newton per cubic meter }
  TNewtonPerCubicMeterUnit = record
    const Symbol       = '%sN/%sm3';
    const SingularName = '%snewton per cubic %smeter';
    const PluralName   = '%snewtons per cubic %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;
  TNewtonPerCubicMeterUnitId = specialize TUnitId<TNewtonPerCubicMeterUnit>;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSecondSquared): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSecondSquared; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSecondSquared): TKilogramsPerCubicMeter; inline;

type
  { Unit of newton per meter }
  TNewtonPerMeterUnit = record
    const Symbol       = '%sN/%sm';
    const SingularName = '%snewton per %smeter';
    const PluralName   = '%snewtons per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;
  TNewtonPerMeterUnitId = specialize TUnitId<TNewtonPerMeterUnit>;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TMeterUnitId): TNewtonsPerMeter; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals; inline;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareHertz): TNewtonsPerMeter; inline;
operator *(const ALeft: TSquareHertz; const ARight: TKilograms): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareHertz; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareHertz): TKilograms; inline;

type
  { Unit of pound force per inch }
  TPoundForcePerInchUnit = record
    const Symbol       = 'lbf/in';
    const SingularName = 'pound-force per inch';
    const PluralName   = 'pounds-force per inch';
    const Prefixes : TPrefixes = ();
    const PrefixExponents : TIntegerDynArray = ();
    const ToBaseFactor = 175.126835246476;
  end;
  TPoundsForcePerInch = specialize TQuantity<TNewtonPerMeterUnit>;
  TPoundForcePerInchUnitId = specialize TUnitId<TPoundForcePerInchUnit>;

type
  { Unit of cubic meter per second }
  TCubicMeterPerSecondUnit = record
    const Symbol       = '%sm3/%ss';
    const SingularName = 'cubic %smeter per %ssecond';
    const PluralName   = 'cubic %smeters per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (3, -1);
  end;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;
  TCubicMeterPerSecondUnitId = specialize TUnitId<TCubicMeterPerSecondUnit>;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSecondUnitId): TCubicMetersPerSecond; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

type
  { Unit of kilogram per second }
  TKilogramPerSecondUnit = record
    const Symbol       = '%sg/%ss';
    const SingularName = '%sgram per %ssecond';
    const PluralName   = '%sgrams per %ssecond';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TKilogramsPerSecond = specialize TQuantity<TKilogramPerSecondUnit>;
  TKilogramPerSecondUnitId = specialize TUnitId<TKilogramPerSecondUnit>;

// main definition [ kg/s ] = [ kg ] / [ s ]
operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds; inline;
operator /(const ALeft: TKilograms; const ARight: TSecondUnitId): TKilogramsPerSecond; inline;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]
operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond; inline;

// alternative definition [ kg/s ] = [ kg/m ] * [ m/s ]
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMetersPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerMeter): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerMeter): TMetersPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TKilogramsPerMeter; inline;

// alternative definition [ kg/s ] = [ W ] / [ m2/s2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerSecond): TWatts; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TSquareMetersPerSquareSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TKilogramsPerSecond): TSquareMetersPerSquareSecond; inline;

type
  { Unit of poiseuille }
  TPoiseuilleUnit = record
    const Symbol       = '%sPl';
    const SingularName = '%spoiseuille';
    const PluralName   = '%spoiseuilles';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TPoiseuilles = specialize TQuantity<TPoiseuilleUnit>;
  TPoiseuilleUnitId = specialize TUnitId<TPoiseuilleUnit>;

var Pl: TPoiseuilleUnitId;

const cPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-02);
const mPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-03);
const miPl: specialize TQuantity<TPoiseuilleUnit> = (FValue: 1E-06);

// main definition [ Pl ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPoiseuilles; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TPascals): TSeconds; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TSeconds): TPascals; inline;
operator *(const ALeft: TPascals; const ARight: TSecondUnitId): TPoiseuilles; inline;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPoiseuilles; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter; inline;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPoiseuilles; inline;
operator *(const ALeft: TMeters; const ARight: TPoiseuilles): TKilogramsPerSecond; inline;
operator *(const ALeft: TPoiseuilles; const ARight: TMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TPoiseuilles): TMeters; inline;
operator *(const ALeft: TPoiseuilles; const ARight: TMeterUnitId): TKilogramsPerSecond; inline;

type
  { Unit of pascal second }
  TPascalSecondUnit = record
    const Symbol       = '%sPa·%ss';
    const SingularName = '%spascal %ssecond';
    const PluralName   = '%spascal %sseconds';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TPascalSeconds = specialize TQuantity<TPoiseuilleUnit>;
  TPascalSecondUnitId = specialize TUnitId<TPoiseuilleUnit>;

type
  { Unit of square meter per second }
  TSquareMeterPerSecondUnit = record
    const Symbol       = '%sm2/%ss';
    const SingularName = 'square %smeter per %ssecond';
    const PluralName   = 'square %smeters per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -1);
  end;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;
  TSquareMeterPerSecondUnitId = specialize TUnitId<TSquareMeterPerSecondUnit>;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSecondUnitId): TSquareMetersPerSecond; inline;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]
operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPoiseuilles; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPoiseuilles; inline;
operator /(const ALeft: TPoiseuilles; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of kilogram per quartic meter }
  TKilogramPerQuarticMeterUnit = record
    const Symbol       = '%sg/%sm4';
    const SingularName = '%sgram per quartic %smeter';
    const PluralName   = '%sgrams per quartic %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -4);
  end;
  TKilogramsPerQuarticMeter = specialize TQuantity<TKilogramPerQuarticMeterUnit>;
  TKilogramPerQuarticMeterUnitId = specialize TUnitId<TKilogramPerQuarticMeterUnit>;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters; inline;
operator /(const ALeft: TKilograms; const ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter; inline;

type
  { Unit of quartic meter second }
  TQuarticMeterSecondUnit = record
    const Symbol       = '%sm4·%ss';
    const SingularName = 'quartic %smeter %ssecond';
    const PluralName   = 'quartic %smeter %sseconds';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (4, 1);
  end;
  TQuarticMeterSeconds = specialize TQuantity<TQuarticMeterSecondUnit>;
  TQuarticMeterSecondUnitId = specialize TUnitId<TQuarticMeterSecondUnit>;

// main definition [ m4*s ] = [ m4 ] * [ s ]
operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TSecondUnitId): TQuarticMeterSeconds; inline;

type
  { Unit of kilogram per quartic meter per second }
  TKilogramPerQuarticMeterPerSecondUnit = record
    const Symbol       = '%sg/%sm4/%ss';
    const SingularName = '%sgram per quartic %smeter per %ssecond';
    const PluralName   = '%sgrams per quartic %smeter per %ssecond';
    const Prefixes : TPrefixes = (pKilo, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -4, -1);
  end;
  TKilogramsPerQuarticMeterPerSecond = specialize TQuantity<TKilogramPerQuarticMeterPerSecondUnit>;
  TKilogramPerQuarticMeterPerSecondUnitId = specialize TUnitId<TKilogramPerQuarticMeterPerSecondUnit>;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeterSeconds): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TQuarticMeterSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilograms; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeterSeconds): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeterSeconds; inline;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]
operator /(const ALeft: TPascals; const ARight: TCubicMetersPerSecond): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TPascals; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TCubicMetersPerSecond): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerQuarticMeterPerSecond): TCubicMetersPerSecond; inline;

type
  { Unit of cubic meter per kilogram }
  TCubicMeterPerKilogramUnit = record
    const Symbol       = '%sm3/%sg';
    const SingularName = 'cubic %smeter per %sgram';
    const PluralName   = 'cubic %smeters per %sgram';
    const Prefixes : TPrefixes = (pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (3, -1);
  end;
  TCubicMetersPerKilogram = specialize TQuantity<TCubicMeterPerKilogramUnit>;
  TCubicMeterPerKilogramUnitId = specialize TUnitId<TCubicMeterPerKilogramUnit>;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]
operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms; inline;
operator /(const ALeft: TCubicMeters; const ARight: TKilogramUnitId): TCubicMetersPerKilogram; inline;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]
operator /(const ALeft: double; const ARight: TKilogramsPerCubicMeter): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMetersPerKilogram): double; inline;
operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilogramsPerCubicMeter): double; inline;
operator /(const ALeft: double; const ARight: TCubicMetersPerKilogram): TKilogramsPerCubicMeter; inline;

type
  { Unit of kilogram square second }
  TKilogramSquareSecondUnit = record
    const Symbol       = '%sg·%ss2';
    const SingularName = '%sgram square %ssecond';
    const PluralName   = '%sgram square %sseconds';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2);
  end;
  TKilogramSquareSeconds = specialize TQuantity<TKilogramSquareSecondUnit>;
  TKilogramSquareSecondUnitId = specialize TUnitId<TKilogramSquareSecondUnit>;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareSecondUnitId): TKilogramSquareSeconds; inline;

type
  { Unit of cubic meter per square second }
  TCubicMeterPerSquareSecondUnit = record
    const Symbol       = '%sm3/%ss2';
    const SingularName = 'cubic %smeter per square %ssecond';
    const PluralName   = 'cubic %smeters per square %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (3, -2);
  end;
  TCubicMetersPerSquareSecond = specialize TQuantity<TCubicMeterPerSquareSecondUnit>;
  TCubicMeterPerSquareSecondUnitId = specialize TUnitId<TCubicMeterPerSquareSecondUnit>;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond; inline;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]
operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSquareMeters): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecondSquared): TCubicMetersPerSquareSecond; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSecondSquared): TSquareMeters; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSecondSquared; inline;

type
  { Unit of newton square meter }
  TNewtonSquareMeterUnit = record
    const Symbol       = '%sN·%sm2';
    const SingularName = '%snewton square %smeter';
    const PluralName   = '%snewton square %smeters';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2);
  end;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;
  TNewtonSquareMeterUnitId = specialize TUnitId<TNewtonSquareMeterUnit>;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareMeterUnitId): TNewtonSquareMeters; inline;

// alternative definition [ N*m2 ] = [ J ] * [ m ]
operator *(const ALeft: TJoules; const ARight: TMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TMeters; const ARight: TJoules): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TJoules): TMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TMeters): TJoules; inline;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]
operator *(const ALeft: TPascals; const ARight: TQuarticMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TPascals): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TPascals): TQuarticMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TQuarticMeters): TPascals; inline;

type
  { Unit of newton per square kilogram }
  TNewtonPerSquareKilogramUnit = record
    const Symbol       = '%sN/%sg2';
    const SingularName = '%snewton per square %sgram';
    const PluralName   = '%snewtons per square %sgram';
    const Prefixes : TPrefixes = (pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;
  TNewtonPerSquareKilogramUnitId = specialize TUnitId<TNewtonPerSquareKilogramUnit>;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram; inline;

type
  { Unit of square kilogram per meter }
  TSquareKilogramPerMeterUnit = record
    const Symbol       = '%sg2/%sm';
    const SingularName = 'square %sgram per %smeter';
    const PluralName   = 'square %sgrams per %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -1);
  end;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;
  TSquareKilogramPerMeterUnitId = specialize TUnitId<TSquareKilogramPerMeterUnit>;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TMeterUnitId): TSquareKilogramsPerMeter; inline;

type
  { Unit of square kilogram per square meter }
  TSquareKilogramPerSquareMeterUnit = record
    const Symbol       = '%sg2/%sm2';
    const SingularName = 'square %sgram per square %smeter';
    const PluralName   = 'square %sgrams per square %smeter';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramPerSquareMeterUnitId = specialize TUnitId<TSquareKilogramPerSquareMeterUnit>;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter; inline;

type
  { Unit of square meter per square kilogram }
  TSquareMeterPerSquareKilogramUnit = record
    const Symbol       = '%sm2/%sg2';
    const SingularName = 'square %smeter per square %sgram';
    const PluralName   = 'square %smeters per square %sgram';
    const Prefixes : TPrefixes = (pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TSquareMeterPerSquareKilogramUnitId = specialize TUnitId<TSquareMeterPerSquareKilogramUnit>;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram; inline;

type
  { Unit of newton square meter per square kilogram }
  TNewtonSquareMeterPerSquareKilogramUnit = record
    const Symbol       = '%sN·%sm2/%sg2';
    const SingularName = '%snewton square %smeter per square %sgram';
    const PluralName   = '%snewton square %smeters per square %sgram';
    const Prefixes : TPrefixes = (pNone, pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (1, 2, -2);
  end;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMeterPerSquareKilogramUnitId = specialize TUnitId<TNewtonSquareMeterPerSquareKilogramUnit>;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]
operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TSquareSeconds): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareSeconds): TCubicMetersPerKilogram; inline;
operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareSeconds; inline;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TKilogramSquareSeconds): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TKilogramSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilogramSquareSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilogramSquareSeconds; inline;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilograms): TCubicMetersPerSquareSecond; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilograms; inline;

type
  { Unit of reciprocal kelvin }
  TReciprocalKelvinUnit = record
    const Symbol       = '1/%sK';
    const SingularName = 'reciprocal %skelvin';
    const PluralName   = 'reciprocal %skelvin';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-1);
  end;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;
  TReciprocalKelvinUnitId = specialize TUnitId<TReciprocalKelvinUnit>;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator /(const ALeft: double; const ARight: TKelvinUnitId): TReciprocalKelvins; inline;

type
  { Unit of kilogram kelvin }
  TKilogramKelvinUnit = record
    const Symbol       = '%sg·%sK';
    const SingularName = '%sgram %skelvin';
    const PluralName   = '%sgram %skelvins';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;
  TKilogramKelvinUnitId = specialize TUnitId<TKilogramKelvinUnit>;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;
operator *(const ALeft: TKilograms; const ARight: TKelvinUnitId): TKilogramKelvins; inline;

type
  { Unit of joule per kelvin }
  TJoulePerKelvinUnit = record
    const Symbol       = '%sJ/%sK';
    const SingularName = '%sjoule per %skelvin';
    const PluralName   = '%sjoules per %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;
  TJoulePerKelvinUnitId = specialize TUnitId<TJoulePerKelvinUnit>;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator /(const ALeft: TJoules; const ARight: TKelvinUnitId): TJoulesPerKelvin; inline;

type
  { Unit of joule per kilogram }
  TJoulePerKilogramUnit = record
    const Symbol       = '%sJ/%sg';
    const SingularName = '%sjoule per %sgram';
    const PluralName   = '%sjoules per %sgram';
    const Prefixes : TPrefixes = (pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TJoulesPerKilogram = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TJoulePerKilogramUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

type
  { Unit of joule per kilogram per kelvin }
  TJoulePerKilogramPerKelvinUnit = record
    const Symbol       = '%sJ/%sg/%sK';
    const SingularName = '%sjoule per %sgram per %skelvin';
    const PluralName   = '%sjoules per %sgram per %skelvin';
    const Prefixes : TPrefixes = (pNone, pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1, -1);
  end;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;
  TJoulePerKilogramPerKelvinUnitId = specialize TUnitId<TJoulePerKilogramPerKelvinUnit>;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin; inline;

type
  { Unit of meter kelvin }
  TMeterKelvinUnit = record
    const Symbol       = '%sm·%sK';
    const SingularName = '%smeter %skelvin';
    const PluralName   = '%smeter %skelvins';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;
  TMeterKelvinUnitId = specialize TUnitId<TMeterKelvinUnit>;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinUnitId): TMeterKelvins; inline;

type
  { Unit of kelvin per meter }
  TKelvinPerMeterUnit = record
    const Symbol       = '%sK/%sm';
    const SingularName = '%skelvin per %smeter';
    const PluralName   = '%skelvins per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;
  TKelvinPerMeterUnitId = specialize TUnitId<TKelvinPerMeterUnit>;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator /(const ALeft: TKelvins; const ARight: TMeterUnitId): TKelvinsPerMeter; inline;

type
  { Unit of watt per meter }
  TWattPerMeterUnit = record
    const Symbol       = '%sW/%sm';
    const SingularName = '%swatt per %smeter';
    const PluralName   = '%swatts per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;
  TWattPerMeterUnitId = specialize TUnitId<TWattPerMeterUnit>;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator /(const ALeft: TWatts; const ARight: TMeterUnitId): TWattsPerMeter; inline;

type
  { Unit of watt per square meter }
  TWattPerSquareMeterUnit = record
    const Symbol       = '%sW/%sm2';
    const SingularName = '%swatt per square %smeter';
    const PluralName   = '%swatts per square %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;
  TWattPerSquareMeterUnitId = specialize TUnitId<TWattPerSquareMeterUnit>;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TWatts; const ARight: TSquareMeterUnitId): TWattsPerSquareMeter; inline;

type
  { Unit of watt per kelvin }
  TWattPerKelvinUnit = record
    const Symbol       = '%sW/%sK';
    const SingularName = '%swatt per %skelvin';
    const PluralName   = '%swatts per %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;
  TWattPerKelvinUnitId = specialize TUnitId<TWattPerKelvinUnit>;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator /(const ALeft: TWatts; const ARight: TKelvinUnitId): TWattsPerKelvin; inline;

type
  { Unit of watt per meter per kelvin }
  TWattPerMeterPerKelvinUnit = record
    const Symbol       = '%sW/%sm/%sK';
    const SingularName = '%swatt per %smeter per %skelvin';
    const PluralName   = '%swatts per %smeter per %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1, -1);
  end;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;
  TWattPerMeterPerKelvinUnitId = specialize TUnitId<TWattPerMeterPerKelvinUnit>;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvinUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeterUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter; inline;

type
  { Unit of square meter kelvin }
  TSquareMeterKelvinUnit = record
    const Symbol       = '%sm2·%sK';
    const SingularName = 'square %smeter %skelvin';
    const PluralName   = 'square %smeter %skelvins';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, 1);
  end;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;
  TSquareMeterKelvinUnitId = specialize TUnitId<TSquareMeterKelvinUnit>;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKelvinUnitId): TSquareMeterKelvins; inline;

type
  { Unit of watt per square meter per kelvin }
  TWattPerSquareMeterPerKelvinUnit = record
    const Symbol       = '%sW/%sm2/%sK';
    const SingularName = '%swatt per square %smeter per %skelvin';
    const PluralName   = '%swatts per square %smeter per %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2, -1);
  end;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;
  TWattPerSquareMeterPerKelvinUnitId = specialize TUnitId<TWattPerSquareMeterPerKelvinUnit>;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin; inline;

type
  { Unit of square meter quartic kelvin }
  TSquareMeterQuarticKelvinUnit = record
    const Symbol       = '%sm2·%sK4';
    const SingularName = 'square %smeter quartic %skelvin';
    const PluralName   = 'square %smeter quartic %skelvins';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, 4);
  end;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvinUnitId = specialize TUnitId<TSquareMeterQuarticKelvinUnit>;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins; inline;

type
  { Unit of watt per quartic kelvin }
  TWattPerQuarticKelvinUnit = record
    const Symbol       = '%sW/%sK4';
    const SingularName = '%swatt per quartic %skelvin';
    const PluralName   = '%swatts per quartic %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -4);
  end;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;
  TWattPerQuarticKelvinUnitId = specialize TUnitId<TWattPerQuarticKelvinUnit>;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator /(const ALeft: TWatts; const ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin; inline;

type
  { Unit of watt per square meter per quartic kelvin }
  TWattPerSquareMeterPerQuarticKelvinUnit = record
    const Symbol       = '%sW/%sm2/%sK4';
    const SingularName = '%swatt per square %smeter per quartic %skelvin';
    const PluralName   = '%swatts per square %smeter per quartic %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2, -4);
  end;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattPerSquareMeterPerQuarticKelvinUnitId = specialize TUnitId<TWattPerSquareMeterPerQuarticKelvinUnit>;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

type
  { Unit of joule per mole }
  TJoulePerMoleUnit = record
    const Symbol       = '%sJ/%smol';
    const SingularName = '%sjoule per %smole';
    const PluralName   = '%sjoules per %smole';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;
  TJoulePerMoleUnitId = specialize TUnitId<TJoulePerMoleUnit>;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator /(const ALeft: TJoules; const ARight: TMoleUnitId): TJoulesPerMole; inline;

type
  { Unit of mole kelvin }
  TMoleKelvinUnit = record
    const Symbol       = '%smol·%sK';
    const SingularName = '%smole %skelvin';
    const PluralName   = '%smole %skelvins';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;
  TMoleKelvinUnitId = specialize TUnitId<TMoleKelvinUnit>;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;
operator *(const ALeft: TMoles; const ARight: TKelvinUnitId): TMoleKelvins; inline;

type
  { Unit of joule per mole per kelvin }
  TJoulePerMolePerKelvinUnit = record
    const Symbol       = '%sJ/%smol/%sK';
    const SingularName = '%sjoule per %smole per %skelvin';
    const PluralName   = '%sjoules per %smole per %skelvin';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1, -1);
  end;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;
  TJoulePerMolePerKelvinUnitId = specialize TUnitId<TJoulePerMolePerKelvinUnit>;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoleUnitId): TJoulesPerMolePerKelvin; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvinUnitId): TJoulesPerMolePerKelvin; inline;

type
  { Unit of ohm meter }
  TOhmMeterUnit = record
    const Symbol       = '%sΩ·%sm';
    const SingularName = '%sohm %smeter';
    const PluralName   = '%sohm %smeters';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;
  TOhmMeterUnitId = specialize TUnitId<TOhmMeterUnit>;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TMeterUnitId): TOhmMeters; inline;

type
  { Unit of volt per meter }
  TVoltPerMeterUnit = record
    const Symbol       = '%sV/%sm';
    const SingularName = '%svolt per %smeter';
    const PluralName   = '%svolts per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;
  TVoltPerMeterUnitId = specialize TUnitId<TVoltPerMeterUnit>;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator /(const ALeft: TVolts; const ARight: TMeterUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator /(const ALeft: TNewtons; const ARight: TCoulombUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

type
  { Unit of newton per coulomb }
  TNewtonPerCoulombUnit = record
    const Symbol       = '%sN/%sC';
    const SingularName = '%snewton per %scoulomb';
    const PluralName   = '%snewtons per %scoulomb';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TNewtonsPerCoulomb = specialize TQuantity<TVoltPerMeterUnit>;
  TNewtonPerCoulombUnitId = specialize TUnitId<TVoltPerMeterUnit>;

type
  { Unit of coulomb per meter }
  TCoulombPerMeterUnit = record
    const Symbol       = '%sC/%sm';
    const SingularName = '%scoulomb per %smeter';
    const PluralName   = '%scoulombs per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;
  TCoulombPerMeterUnitId = specialize TUnitId<TCoulombPerMeterUnit>;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TCoulombs; const ARight: TMeterUnitId): TCoulombsPerMeter; inline;

type
  { Unit of square coulomb per meter }
  TSquareCoulombPerMeterUnit = record
    const Symbol       = '%sC2/%sm';
    const SingularName = 'square %scoulomb per %smeter';
    const PluralName   = 'square %scoulombs per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -1);
  end;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;
  TSquareCoulombPerMeterUnitId = specialize TUnitId<TSquareCoulombPerMeterUnit>;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TMeterUnitId): TSquareCoulombsPerMeter; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

type
  { Unit of coulomb per square meter }
  TCoulombPerSquareMeterUnit = record
    const Symbol       = '%sC/%sm2';
    const SingularName = '%scoulomb per square %smeter';
    const PluralName   = '%scoulombs per square %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;
  TCoulombPerSquareMeterUnitId = specialize TUnitId<TCoulombPerSquareMeterUnit>;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TCoulombs; const ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;

type
  { Unit of square meter per square coulomb }
  TSquareMeterPerSquareCoulombUnit = record
    const Symbol       = '%sm2/%sC2';
    const SingularName = 'square %smeter per square %scoulomb';
    const PluralName   = 'square %smeters per square %scoulomb';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;
  TSquareMeterPerSquareCoulombUnitId = specialize TUnitId<TSquareMeterPerSquareCoulombUnit>;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb; inline;

type
  { Unit of newton per square coulomb }
  TNewtonPerSquareCoulombUnit = record
    const Symbol       = '%sN/%sC2';
    const SingularName = '%snewton per square %scoulomb';
    const PluralName   = '%snewtons per square %scoulomb';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;
  TNewtonPerSquareCoulombUnitId = specialize TUnitId<TNewtonPerSquareCoulombUnit>;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb; inline;

type
  { Unit of newton square meter per square coulomb }
  TNewtonSquareMeterPerSquareCoulombUnit = record
    const Symbol       = '%sN·%sm2/%sC2';
    const SingularName = '%snewton square %smeter per square %scoulomb';
    const PluralName   = '%snewton square %smeters per square %scoulomb';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2, -2);
  end;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMeterPerSquareCoulombUnitId = specialize TUnitId<TNewtonSquareMeterPerSquareCoulombUnit>;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter; inline;

type
  { Unit of volt meter }
  TVoltMeterUnit = record
    const Symbol       = '%sV·%sm';
    const SingularName = '%svolt %smeter';
    const PluralName   = '%svolt %smeters';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;
  TVoltMeterUnitId = specialize TUnitId<TVoltMeterUnit>;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TVolts; const ARight: TMeterUnitId): TVoltMeters; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

type
  { Unit of newton square meter per coulomb }
  TNewtonSquareMeterPerCoulombUnit = record
    const Symbol       = '%sN·%sm2/%sC';
    const SingularName = '%snewton square %smeter per %scoulomb';
    const PluralName   = '%snewton square %smeters per %scoulomb';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 2, -1);
  end;
  TNewtonSquareMetersPerCoulomb = specialize TQuantity<TVoltMeterUnit>;
  TNewtonSquareMeterPerCoulombUnitId = specialize TUnitId<TVoltMeterUnit>;

type
  { Unit of volt meter per second }
  TVoltMeterPerSecondUnit = record
    const Symbol       = '%sV·%sm/%ss';
    const SingularName = '%svolt %smeter per %ssecond';
    const PluralName   = '%svolt %smeters per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1, -1);
  end;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;
  TVoltMeterPerSecondUnitId = specialize TUnitId<TVoltMeterPerSecondUnit>;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSecondUnitId): TVoltMetersPerSecond; inline;

type
  { Unit of farad per meter }
  TFaradPerMeterUnit = record
    const Symbol       = '%sF/%sm';
    const SingularName = '%sfarad per %smeter';
    const PluralName   = '%sfarads per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;
  TFaradPerMeterUnitId = specialize TUnitId<TFaradPerMeterUnit>;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator /(const ALeft: TFarads; const ARight: TMeterUnitId): TFaradsPerMeter; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter; inline;
operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double; inline;
operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;

type
  { Unit of ampere per meter }
  TAmperePerMeterUnit = record
    const Symbol       = '%sA/%sm';
    const SingularName = '%sampere per %smeter';
    const PluralName   = '%samperes per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;
  TAmperePerMeterUnitId = specialize TUnitId<TAmperePerMeterUnit>;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator /(const ALeft: TAmperes; const ARight: TMeterUnitId): TAmperesPerMeter; inline;

type
  { Unit of meter per ampere }
  TMeterPerAmpereUnit = record
    const Symbol       = '%sm/%sA';
    const SingularName = '%smeter per %sampere';
    const PluralName   = '%smeters per %sampere';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;
  TMeterPerAmpereUnitId = specialize TUnitId<TMeterPerAmpereUnit>;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator /(const ALeft: TMeters; const ARight: TAmpereUnitId): TMetersPerAmpere; inline;

type
  { Unit of tesla meter }
  TTeslaMeterUnit = record
    const Symbol       = '%sT·%sm';
    const SingularName = '%stesla %smeter';
    const PluralName   = '%stesla %smeters';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;
  TTeslaMeterUnitId = specialize TUnitId<TTeslaMeterUnit>;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;
operator *(const ALeft: TTeslas; const ARight: TMeterUnitId): TTeslaMeters; inline;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator /(const ALeft: TNewtons; const ARight: TAmpereUnitId): TTeslaMeters; inline;

type
  { Unit of newton per ampere }
  TNewtonPerAmpereUnit = record
    const Symbol       = '%sN/%sA';
    const SingularName = '%snewton per %sampere';
    const PluralName   = '%snewtons per %sampere';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TNewtonsPerAmpere = specialize TQuantity<TTeslaMeterUnit>;
  TNewtonPerAmpereUnitId = specialize TUnitId<TTeslaMeterUnit>;

type
  { Unit of tesla per ampere }
  TTeslaPerAmpereUnit = record
    const Symbol       = '%sT/%sA';
    const SingularName = '%stesla per %sampere';
    const PluralName   = '%steslas per %sampere';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;
  TTeslaPerAmpereUnitId = specialize TUnitId<TTeslaPerAmpereUnit>;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator /(const ALeft: TTeslas; const ARight: TAmpereUnitId): TTeslasPerAmpere; inline;

type
  { Unit of henry per meter }
  THenryPerMeterUnit = record
    const Symbol       = '%sH/%sm';
    const SingularName = '%shenry per %smeter';
    const PluralName   = '%shenries per %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  THenriesPerMeter = specialize TQuantity<THenryPerMeterUnit>;
  THenryPerMeterUnitId = specialize TUnitId<THenryPerMeterUnit>;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenries; const ARight: TMeters): THenriesPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: THenriesPerMeter): THenries; inline;
operator *(const ALeft: THenriesPerMeter; const ARight: TMeters): THenries; inline;
operator /(const ALeft: THenries; const ARight: THenriesPerMeter): TMeters; inline;
operator /(const ALeft: THenries; const ARight: TMeterUnitId): THenriesPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenriesPerMeter; inline;
operator *(const ALeft: TAmperes; const ARight: THenriesPerMeter): TTeslaMeters; inline;
operator *(const ALeft: THenriesPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenriesPerMeter): TAmperes; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TAmpereUnitId): THenriesPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenriesPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenriesPerMeter; inline;
operator /(const ALeft: THenriesPerMeter; const ARight: TTeslasPerAmpere): TMeters; inline;
operator /(const ALeft: THenriesPerMeter; const ARight: TMeters): TTeslasPerAmpere; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeterUnitId): THenriesPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenriesPerMeter; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenriesPerMeter; inline;
operator /(const ALeft: THenriesPerMeter; const ARight: TTeslas): TMetersPerAmpere; inline;
operator /(const ALeft: THenriesPerMeter; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenriesPerMeter; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: THenriesPerMeter): TTeslas; inline;
operator *(const ALeft: THenriesPerMeter; const ARight: TAmperesPerMeter): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: THenriesPerMeter): TAmperesPerMeter; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenriesPerMeter; inline;
operator *(const ALeft: TSquareAmperes; const ARight: THenriesPerMeter): TNewtons; inline;
operator *(const ALeft: THenriesPerMeter; const ARight: TSquareAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: THenriesPerMeter): TSquareAmperes; inline;

type
  { Unit of tesla meter per ampere }
  TTeslaMeterPerAmpereUnit = record
    const Symbol       = '%sT·%sm/%sA';
    const SingularName = '%stesla %smeter per %sampere';
    const PluralName   = '%stesla %smeters per %sampere';
    const Prefixes : TPrefixes = (pNone, pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1, -1);
  end;
  TTeslaMetersPerAmpere = specialize TQuantity<THenryPerMeterUnit>;
  TTeslaMeterPerAmpereUnitId = specialize TUnitId<THenryPerMeterUnit>;

type
  { Unit of newton per square ampere }
  TNewtonPerSquareAmpereUnit = record
    const Symbol       = '%sN/%sA2';
    const SingularName = '%snewton per square %sampere';
    const PluralName   = '%snewtons per square %sampere';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TNewtonsPerSquareAmpere = specialize TQuantity<THenryPerMeterUnit>;
  TNewtonPerSquareAmpereUnitId = specialize TUnitId<THenryPerMeterUnit>;

type
  { Unit of radian per meter }
  TRadianPerMeterUnit = record
    const Symbol       = 'rad/%sm';
    const SingularName = 'radian per %smeter';
    const PluralName   = 'radians per %smeter';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-1);
  end;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;
  TRadianPerMeterUnitId = specialize TUnitId<TRadianPerMeterUnit>;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator /(const ALeft: TRadians; const ARight: TMeterUnitId): TRadiansPerMeter; inline;

type
  { Unit of square kilogram per square second }
  TSquareKilogramPerSquareSecondUnit = record
    const Symbol       = '%sg2/%ss2';
    const SingularName = 'square %sgram per square %ssecond';
    const PluralName   = 'square %sgrams per square %ssecond';
    const Prefixes : TPrefixes = (pKilo, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareKilogramsPerSquareSecond = specialize TQuantity<TSquareKilogramPerSquareSecondUnit>;
  TSquareKilogramPerSquareSecondUnitId = specialize TUnitId<TSquareKilogramPerSquareSecondUnit>;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]
operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]
operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms; inline;

type
  { Unit of reciprocal meter }
  TReciprocalMeterUnit = record
    const Symbol       = '1/%sm';
    const SingularName = 'reciprocal %smeter';
    const PluralName   = 'reciprocal %smeters';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-1);
  end;
  TReciprocalMeters = specialize TQuantity<TReciprocalMeterUnit>;
  TReciprocalMeterUnitId = specialize TUnitId<TReciprocalMeterUnit>;

// main definition [ 1/m ] = 1 / [ m ]
operator /(const ALeft: double; const ARight: TMeters): TReciprocalMeters; inline;
operator *(const ALeft: TMeters; const ARight: TReciprocalMeters): double; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TMeters): double; inline;
operator /(const ALeft: double; const ARight: TReciprocalMeters): TMeters; inline;
operator /(const ALeft: double; const ARight: TMeterUnitId): TReciprocalMeters; inline;

type
  { Unit of square second per square meter }
  TSquareSecondPerSquareMeterUnit = record
    const Symbol       = '%ss2/%sm2';
    const SingularName = 'square %ssecond per square %smeter';
    const PluralName   = 'square %sseconds per square %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (2, -2);
  end;
  TSquareSecondsPerSquareMeter = specialize TQuantity<TSquareSecondPerSquareMeterUnit>;
  TSquareSecondPerSquareMeterUnitId = specialize TUnitId<TSquareSecondPerSquareMeterUnit>;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]
operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter; inline;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]
operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double; inline;
operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond; inline;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]
operator *(const ALeft: TFaradsPerMeter; const ARight: THenriesPerMeter): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: THenriesPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenriesPerMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenriesPerMeter): TFaradsPerMeter; inline;

type
  { Unit of square joule }
  TSquareJouleUnit = record
    const Symbol       = '%sJ2';
    const SingularName = 'square %sjoule';
    const PluralName   = 'square %sjoules';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareJoules = specialize TQuantity<TSquareJouleUnit>;
  TSquareJouleUnitId = specialize TUnitId<TSquareJouleUnit>;

var J2: TSquareJouleUnitId;

const TJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+24);
const GJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+18);
const MJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+12);
const kJ2: specialize TQuantity<TSquareJouleUnit> = (FValue: 1E+06);

// main definition [ J2 ] = [ J ] * [ J ]
operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules; inline;
operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules; inline;

type
  { Unit of joule second }
  TJouleSecondUnit = record
    const Symbol       = '%sJ·%ss';
    const SingularName = '%sjoule %ssecond';
    const PluralName   = '%sjoule %sseconds';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
  end;
  TJouleSeconds = specialize TQuantity<TJouleSecondUnit>;
  TJouleSecondUnitId = specialize TUnitId<TJouleSecondUnit>;

// main definition [ J*s ] = [ J ] * [ s ]
operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TJoules; const ARight: TSecondUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ J ] / [ Hz ]
operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds; inline;
operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules; inline;
operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz; inline;
operator /(const ALeft: TJoules; const ARight: THertzUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond; inline;

type
  { Unit of elettronvolt second }
  TElettronvoltSecondUnit = record
    const Symbol       = '%seV·%ss';
    const SingularName = '%selettronvolt %ssecond';
    const PluralName   = '%selettronvolt %sseconds';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, 1);
    const ToBaseFactor = 1.60217742320523E-019;
  end;
  TElettronvoltSeconds = specialize TQuantity<TJouleSecondUnit>;
  TElettronvoltSecondUnitId = specialize TUnitId<TElettronvoltSecondUnit>;

type
  { Unit of lumen per watt }
  TLumenPerWattUnit = record
    const Symbol       = '%slm/%sW';
    const SingularName = '%slumen per %swatt';
    const PluralName   = '%slumens per %swatt';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TLumensPerWatt = specialize TQuantity<TLumenPerWattUnit>;
  TLumenPerWattUnitId = specialize TUnitId<TLumenPerWattUnit>;

// main definition [ lm/W ] = [ lm ] / [ W ]
operator /(const ALeft: TLumens; const ARight: TWatts): TLumensPerWatt; inline;
operator *(const ALeft: TWatts; const ARight: TLumensPerWatt): TLumens; inline;
operator *(const ALeft: TLumensPerWatt; const ARight: TWatts): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TLumensPerWatt): TWatts; inline;
operator /(const ALeft: TLumens; const ARight: TWattUnitId): TLumensPerWatt; inline;

type
  { Unit of reciprocal mole }
  TReciprocalMoleUnit = record
    const Symbol       = '1/%smol';
    const SingularName = 'reciprocal %smole';
    const PluralName   = 'reciprocal %smoles';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (-1);
  end;
  TReciprocalMoles = specialize TQuantity<TReciprocalMoleUnit>;
  TReciprocalMoleUnitId = specialize TUnitId<TReciprocalMoleUnit>;

// main definition [ 1/mol ] = 1 / [ mol ]
operator /(const ALeft: double; const ARight: TMoles): TReciprocalMoles; inline;
operator *(const ALeft: TMoles; const ARight: TReciprocalMoles): double; inline;
operator *(const ALeft: TReciprocalMoles; const ARight: TMoles): double; inline;
operator /(const ALeft: double; const ARight: TReciprocalMoles): TMoles; inline;
operator /(const ALeft: double; const ARight: TMoleUnitId): TReciprocalMoles; inline;

type
  { Unit of ampere per square meter }
  TAmperePerSquareMeterUnit = record
    const Symbol       = '%sA/%sm2';
    const SingularName = '%sampere per square %smeter';
    const PluralName   = '%samperes per square %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TAmperesPerSquareMeter = specialize TQuantity<TAmperePerSquareMeterUnit>;
  TAmperePerSquareMeterUnitId = specialize TUnitId<TAmperePerSquareMeterUnit>;

// main definition [ A/m2 ] = [ A ] / [ m2 ]
operator /(const ALeft: TAmperes; const ARight: TSquareMeters): TAmperesPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TAmperesPerSquareMeter): TAmperes; inline;
operator *(const ALeft: TAmperesPerSquareMeter; const ARight: TSquareMeters): TAmperes; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TAmperes; const ARight: TSquareMeterUnitId): TAmperesPerSquareMeter; inline;

// alternative definition [ A/m2 ] = [ A/m ] / [ m ]
operator /(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperesPerSquareMeter; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerSquareMeter): TAmperesPerMeter; inline;
operator *(const ALeft: TAmperesPerSquareMeter; const ARight: TMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperesPerMeter; const ARight: TAmperesPerSquareMeter): TMeters; inline;

type
  { Unit of mole per cubic meter }
  TMolePerCubicMeterUnit = record
    const Symbol       = '%smol/%sm3';
    const SingularName = '%smole per cubic %smeter';
    const PluralName   = '%smoles per cubic %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TMolesPerCubicMeter = specialize TQuantity<TMolePerCubicMeterUnit>;
  TMolePerCubicMeterUnitId = specialize TUnitId<TMolePerCubicMeterUnit>;

// main definition [ mol/m3 ] = [ mol ] / [ m3 ]
operator /(const ALeft: TMoles; const ARight: TCubicMeters): TMolesPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TMolesPerCubicMeter): TMoles; inline;
operator *(const ALeft: TMolesPerCubicMeter; const ARight: TCubicMeters): TMoles; inline;
operator /(const ALeft: TMoles; const ARight: TMolesPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TMoles; const ARight: TCubicMeterUnitId): TMolesPerCubicMeter; inline;

type
  { Unit of candela per square meter }
  TCandelaPerSquareMeterUnit = record
    const Symbol       = '%scd/%sm2';
    const SingularName = '%scandela per square %smeter';
    const PluralName   = '%scandelas per square %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TCandelasPerSquareMeter = specialize TQuantity<TCandelaPerSquareMeterUnit>;
  TCandelaPerSquareMeterUnitId = specialize TUnitId<TCandelaPerSquareMeterUnit>;

// main definition [ cd/m2 ] = [ cd ] / [ m2 ]
operator /(const ALeft: TCandelas; const ARight: TSquareMeters): TCandelasPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCandelasPerSquareMeter): TCandelas; inline;
operator *(const ALeft: TCandelasPerSquareMeter; const ARight: TSquareMeters): TCandelas; inline;
operator /(const ALeft: TCandelas; const ARight: TCandelasPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TCandelas; const ARight: TSquareMeterUnitId): TCandelasPerSquareMeter; inline;

type
  { Unit of coulomb per cubic meter }
  TCoulombPerCubicMeterUnit = record
    const Symbol       = '%sC/%sm3';
    const SingularName = '%scoulomb per cubic %smeter';
    const PluralName   = '%scoulombs per cubic %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TCoulombsPerCubicMeter = specialize TQuantity<TCoulombPerCubicMeterUnit>;
  TCoulombPerCubicMeterUnitId = specialize TUnitId<TCoulombPerCubicMeterUnit>;

// main definition [ C/m3 ] = [ C ] / [ m3 ]
operator /(const ALeft: TCoulombs; const ARight: TCubicMeters): TCoulombsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TCoulombsPerCubicMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TCubicMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TCoulombs; const ARight: TCubicMeterUnitId): TCoulombsPerCubicMeter; inline;

// alternative definition [ C/m3 ] = [ C/m ] / [ m2 ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TSquareMeters): TCoulombsPerCubicMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerCubicMeter): TCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TSquareMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerCubicMeter): TSquareMeters; inline;

// alternative definition [ C/m3 ] = [ C/m2 ] / [ m ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerCubicMeter): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TCoulombsPerCubicMeter): TMeters; inline;

type
  { Unit of coulomb per kilogram }
  TCoulombPerKilogramUnit = record
    const Symbol       = '%sC/%sg';
    const SingularName = '%scoulomb per %sgram';
    const PluralName   = '%scoulombs per %sgram';
    const Prefixes : TPrefixes = (pNone, pKilo);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TCoulombsPerKilogram = specialize TQuantity<TCoulombPerKilogramUnit>;
  TCoulombPerKilogramUnitId = specialize TUnitId<TCoulombPerKilogramUnit>;

// main definition [ C/kg ] = [ C ] / [ kg ]
operator /(const ALeft: TCoulombs; const ARight: TKilograms): TCoulombsPerKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TCoulombsPerKilogram): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerKilogram; const ARight: TKilograms): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerKilogram): TKilograms; inline;
operator /(const ALeft: TCoulombs; const ARight: TKilogramUnitId): TCoulombsPerKilogram; inline;

type
  { Unit of gray per second }
  TGrayPerSecondUnit = record
    const Symbol       = '%sGy/%ss';
    const SingularName = '%sgray per %ssecond';
    const PluralName   = '%sgrays per %ssecond';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TGraysPerSecond = specialize TQuantity<TGrayPerSecondUnit>;
  TGrayPerSecondUnitId = specialize TUnitId<TGrayPerSecondUnit>;

// main definition [ Gy/s ] = [ Gy ] / [ s ]
operator /(const ALeft: TGrays; const ARight: TSeconds): TGraysPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TGraysPerSecond): TGrays; inline;
operator *(const ALeft: TGraysPerSecond; const ARight: TSeconds): TGrays; inline;
operator /(const ALeft: TGrays; const ARight: TGraysPerSecond): TSeconds; inline;
operator /(const ALeft: TGrays; const ARight: TSecondUnitId): TGraysPerSecond; inline;

type
  { Unit of watt per steradian }
  TWattPerSteradianUnit = record
    const Symbol       = '%sW/sr';
    const SingularName = '%swatt per steradian';
    const PluralName   = '%swatts per steradian';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (1);
  end;
  TWattsPerSteradian = specialize TQuantity<TWattPerSteradianUnit>;
  TWattPerSteradianUnitId = specialize TUnitId<TWattPerSteradianUnit>;

// main definition [ W/sr ] = [ W ] / [ sr ]
operator /(const ALeft: TWatts; const ARight: TSteradians): TWattsPerSteradian; inline;
operator *(const ALeft: TSteradians; const ARight: TWattsPerSteradian): TWatts; inline;
operator *(const ALeft: TWattsPerSteradian; const ARight: TSteradians): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSteradian): TSteradians; inline;
operator /(const ALeft: TWatts; const ARight: TSteradianUnitId): TWattsPerSteradian; inline;

type
  { Unit of square meter steradian }
  TSquareMeterSteradianUnit = record
    const Symbol       = '%sm2·sr';
    const SingularName = 'square %smeter steradian';
    const PluralName   = 'square %smeter steradians';
    const Prefixes : TPrefixes = (pNone);
    const PrefixExponents : TIntegerDynArray = (2);
  end;
  TSquareMeterSteradians = specialize TQuantity<TSquareMeterSteradianUnit>;
  TSquareMeterSteradianUnitId = specialize TUnitId<TSquareMeterSteradianUnit>;

// main definition [ m2 * sr ] = [ m2 ] * [ sr ]
operator *(const ALeft: TSquareMeters; const ARight: TSteradians): TSquareMeterSteradians; inline;
operator *(const ALeft: TSteradians; const ARight: TSquareMeters): TSquareMeterSteradians; inline;
operator /(const ALeft: TSquareMeterSteradians; const ARight: TSquareMeters): TSteradians; inline;
operator /(const ALeft: TSquareMeterSteradians; const ARight: TSteradians): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSteradianUnitId): TSquareMeterSteradians; inline;

type
  { Unit of watt per square meter per steradian }
  TWattPerSquareMeterPerSteradianUnit = record
    const Symbol       = '%sW/%sm2/sr';
    const SingularName = '%swatt per square %smeter per steradian';
    const PluralName   = '%swatts per square %smeter per steradian';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -2);
  end;
  TWattsPerSquareMeterPerSteradian = specialize TQuantity<TWattPerSquareMeterPerSteradianUnit>;
  TWattPerSquareMeterPerSteradianUnitId = specialize TUnitId<TWattPerSquareMeterPerSteradianUnit>;

// main definition [ W/m2/sr ] = [ W ] / [ m2 * sr ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterSteradians): TWattsPerSquareMeterPerSteradian; inline;
operator *(const ALeft: TSquareMeterSteradians; const ARight: TWattsPerSquareMeterPerSteradian): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSquareMeterSteradians): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerSteradian): TSquareMeterSteradians; inline;

// alternative definition [ W/m2/sr ] = [ W/m2 ] / [ sr ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TSteradians): TWattsPerSquareMeterPerSteradian; inline;
operator *(const ALeft: TSteradians; const ARight: TWattsPerSquareMeterPerSteradian): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSteradians): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerSteradian): TSteradians; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TSteradianUnitId): TWattsPerSquareMeterPerSteradian; inline;

// alternative definition [ W/m2/sr ] = [ W/sr ] / [ m2 ]
operator /(const ALeft: TWattsPerSteradian; const ARight: TSquareMeters): TWattsPerSquareMeterPerSteradian; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerSteradian): TWattsPerSteradian; inline;
operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSquareMeters): TWattsPerSteradian; inline;
operator /(const ALeft: TWattsPerSteradian; const ARight: TWattsPerSquareMeterPerSteradian): TSquareMeters; inline;
operator /(const ALeft: TWattsPerSteradian; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerSteradian; inline;

type
  { Unit of katal per cubic meter }
  TKatalPerCubicMeterUnit = record
    const Symbol       = '%skat/%sm3';
    const SingularName = '%skatal per cubic %smeter';
    const PluralName   = '%skatals per cubic %smeter';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -3);
  end;
  TKatalsPerCubicMeter = specialize TQuantity<TKatalPerCubicMeterUnit>;
  TKatalPerCubicMeterUnitId = specialize TUnitId<TKatalPerCubicMeterUnit>;

// main definition [ kat/m3 ] = [ kat ] / [ m3 ]
operator /(const ALeft: TKatals; const ARight: TCubicMeters): TKatalsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKatalsPerCubicMeter): TKatals; inline;
operator *(const ALeft: TKatalsPerCubicMeter; const ARight: TCubicMeters): TKatals; inline;
operator /(const ALeft: TKatals; const ARight: TKatalsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TKatals; const ARight: TCubicMeterUnitId): TKatalsPerCubicMeter; inline;

type
  { Unit of coulomb per mole }
  TCoulombPerMoleUnit = record
    const Symbol       = '%sC/%smol';
    const SingularName = '%scoulomb per %smole';
    const PluralName   = '%scoulombs per %smole';
    const Prefixes : TPrefixes = (pNone, pNone);
    const PrefixExponents : TIntegerDynArray = (1, -1);
  end;
  TCoulombsPerMole = specialize TQuantity<TCoulombPerMoleUnit>;
  TCoulombPerMoleUnitId = specialize TUnitId<TCoulombPerMoleUnit>;

// main definition [ C/mol ] = [ C ] / [ mol ]
operator /(const ALeft: TCoulombs; const ARight: TMoles): TCoulombsPerMole; inline;
operator *(const ALeft: TMoles; const ARight: TCoulombsPerMole): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerMole; const ARight: TMoles): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMole): TMoles; inline;
operator /(const ALeft: TCoulombs; const ARight: TMoleUnitId): TCoulombsPerMole; inline;

{ Helpers }

type
  TSecondHelper = record helper for TSeconds
    function ToMinute: specialize TQuantity<TMinuteUnit>;
    function ToHour: specialize TQuantity<THourUnit>;
    function ToDay: specialize TQuantity<TDayUnit>;
  end;

type
  TSquareSecondHelper = record helper for TSquareSeconds
    function ToSquareMinute: specialize TQuantity<TSquareMinuteUnit>;
    function ToSquareHour: specialize TQuantity<TSquareHourUnit>;
    function ToSquareDay: specialize TQuantity<TSquareDayUnit>;
  end;

type
  TMeterHelper = record helper for TMeters
    function ToNauticalMile: specialize TQuantity<TNauticalMileUnit>;
    function ToMile: specialize TQuantity<TMileUnit>;
    function ToYard: specialize TQuantity<TYardUnit>;
    function ToFoot: specialize TQuantity<TFootUnit>;
    function ToInch: specialize TQuantity<TInchUnit>;
    function ToAstronomical: specialize TQuantity<TAstronomicalUnit>;
  end;

type
  TSquareMeterHelper = record helper for TSquareMeters
    function ToSquareMile: specialize TQuantity<TSquareMileUnit>;
    function ToSquareYard: specialize TQuantity<TSquareYardUnit>;
    function ToSquareFoot: specialize TQuantity<TSquareFootUnit>;
    function ToSquareInch: specialize TQuantity<TSquareInchUnit>;
  end;

type
  TCubicMeterHelper = record helper for TCubicMeters
    function ToGallon: specialize TQuantity<TGallonUnit>;
    function ToLitre: specialize TQuantity<TLitreUnit>;
    function ToCubicYard: specialize TQuantity<TCubicYardUnit>;
    function ToCubicFoot: specialize TQuantity<TCubicFootUnit>;
    function ToCubicInch: specialize TQuantity<TCubicInchUnit>;
  end;

type
  TKilogramHelper = record helper for TKilograms
    function ToTon: specialize TQuantity<TTonUnit>;
    function ToStone: specialize TQuantity<TStoneUnit>;
    function ToOunce: specialize TQuantity<TOunceUnit>;
    function ToPound: specialize TQuantity<TPoundUnit>;
    function ToTonne: specialize TQuantity<TTonneUnit>;
  end;

type
  TDegreeCelsiusHelper = record helper for TDegreesCelsius
    function ToKelvin: specialize TQuantity<TKelvinUnit>;
  end;

type
  TKelvinHelper = record helper for TKelvins
    function ToDegreeFahrenheit: specialize TQuantity<TDegreeFahrenheitUnit>;
    function ToDegreeCelsius: specialize TQuantity<TDegreeCelsiusUnit>;
  end;

type
  TDegreeFahrenheitHelper = record helper for TDegreesFahrenheit
    function ToKelvin: specialize TQuantity<TKelvinUnit>;
  end;

type
  TRadianHelper = record helper for TRadians
    function ToDegree: specialize TQuantity<TDegreeUnit>;
  end;

type
  TSteradianHelper = record helper for TSteradians
    function ToSquareDegree: specialize TQuantity<TSquareDegreeUnit>;
  end;

type
  TSquareHertzHelper = record helper for TSquareHertz
    function ToSteradianPerSquareSecond: specialize TQuantity<TSteradianPerSquareSecondUnit>;
    function ToRadianPerSecondSquared: specialize TQuantity<TRadianPerSecondSquaredUnit>;
  end;

type
  TMeterPerSecondHelper = record helper for TMetersPerSecond
    function ToNauticalMilePerHour: specialize TQuantity<TNauticalMilePerHourUnit>;
    function ToMilePerHour: specialize TQuantity<TMilePerHourUnit>;
    function ToMeterPerHour: specialize TQuantity<TMeterPerHourUnit>;
  end;

type
  TMeterPerSecondSquaredHelper = record helper for TMetersPerSecondSquared
    function ToMeterPerHourPerSecond: specialize TQuantity<TMeterPerHourPerSecondUnit>;
    function ToMeterPerSecondPerSecond: specialize TQuantity<TMeterPerSecondPerSecondUnit>;
  end;

type
  TKilogramMeterPerSecondHelper = record helper for TKilogramMetersPerSecond
    function ToNewtonSecond: specialize TQuantity<TNewtonSecondUnit>;
  end;

type
  TNewtonHelper = record helper for TNewtons
    function ToPoundForce: specialize TQuantity<TPoundForceUnit>;
  end;

type
  TPascalHelper = record helper for TPascals
    function ToJoulePerCubicMeter: specialize TQuantity<TJoulePerCubicMeterUnit>;
    function ToPoundPerSquareInch: specialize TQuantity<TPoundPerSquareInchUnit>;
    function ToBar: specialize TQuantity<TBarUnit>;
  end;

type
  TJouleHelper = record helper for TJoules
    function ToPoundForceInch: specialize TQuantity<TPoundForceInchUnit>;
    function ToNewtonMeter: specialize TQuantity<TNewtonMeterUnit>;
    function ToElettronvolt: specialize TQuantity<TElettronvoltUnit>;
    function ToWattHour: specialize TQuantity<TWattHourUnit>;
  end;

type
  TCoulombHelper = record helper for TCoulombs
    function ToAmpereHour: specialize TQuantity<TAmpereHourUnit>;
  end;

type
  THertzHelper = record helper for THertz
    function ToBequerel: specialize TQuantity<TBequerelUnit>;
  end;

type
  TSquareMeterPerSquareSecondHelper = record helper for TSquareMetersPerSquareSecond
    function ToJoulePerKilogram: specialize TQuantity<TJoulePerKilogramUnit>;
    function ToSievert: specialize TQuantity<TSievertUnit>;
    function ToGray: specialize TQuantity<TGrayUnit>;
  end;

type
  TJoulePerRadianHelper = record helper for TJoulesPerRadian
    function ToNewtonMeterPerDegree: specialize TQuantity<TNewtonMeterPerDegreeUnit>;
    function ToNewtonMeterPerRadian: specialize TQuantity<TNewtonMeterPerRadianUnit>;
    function ToJoulePerDegree: specialize TQuantity<TJoulePerDegreeUnit>;
  end;

type
  TNewtonPerMeterHelper = record helper for TNewtonsPerMeter
    function ToPoundForcePerInch: specialize TQuantity<TPoundForcePerInchUnit>;
  end;

type
  TPoiseuilleHelper = record helper for TPoiseuilles
    function ToPascalSecond: specialize TQuantity<TPascalSecondUnit>;
  end;

type
  TVoltPerMeterHelper = record helper for TVoltsPerMeter
    function ToNewtonPerCoulomb: specialize TQuantity<TNewtonPerCoulombUnit>;
  end;

type
  TVoltMeterHelper = record helper for TVoltMeters
    function ToNewtonSquareMeterPerCoulomb: specialize TQuantity<TNewtonSquareMeterPerCoulombUnit>;
  end;

type
  TTeslaMeterHelper = record helper for TTeslaMeters
    function ToNewtonPerAmpere: specialize TQuantity<TNewtonPerAmpereUnit>;
  end;

type
  THenryPerMeterHelper = record helper for THenriesPerMeter
    function ToNewtonPerSquareAmpere: specialize TQuantity<TNewtonPerSquareAmpereUnit>;
    function ToTeslaMeterPerAmpere: specialize TQuantity<TTeslaMeterPerAmpereUnit>;
  end;

type
  TJouleSecondHelper = record helper for TJouleSeconds
    function ToElettronvoltSecond: specialize TQuantity<TElettronvoltSecondUnit>;
  end;

{ Power units }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
function SquarePower(AQuantity: TMeters): TSquareMeters;
function SquareRoot(AQuantity: TSquareMeters): TMeters;
function CubicPower(AQuantity: TMeters): TCubicMeters;
function CubicRoot(AQuantity: TCubicMeters): TMeters;
function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
function QuarticPower(AQuantity: TMeters): TQuarticMeters;
function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
function QuinticPower(AQuantity: TMeters): TQuinticMeters;
function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
function SexticPower(AQuantity: TMeters): TSexticMeters;
function SexticRoot(AQuantity: TSexticMeters): TMeters;
function SquarePower(AQuantity: TAmperes): TSquareAmperes;
function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
function SquarePower(AQuantity: TKelvins): TSquareKelvins;
function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
function CubicPower(AQuantity: TKelvins): TCubicKelvins;
function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
function SquarePower(AQuantity: TRadians): TSteradians;
function SquareRoot(AQuantity: TSteradians): TRadians;
function SquarePower(AQuantity: THertz): TSquareHertz;
function SquareRoot(AQuantity: TSquareHertz): THertz;
function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
function SquarePower(AQuantity: TNewtons): TSquareNewtons;
function SquareRoot(AQuantity: TSquareNewtons): TNewtons;
function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
function SquarePower(AQuantity: TVolts): TSquareVolts;
function SquareRoot(AQuantity: TSquareVolts): TVolts;
function SquarePower(AQuantity: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
function SquareRoot(AQuantity: TSquareKilogramsPerSquareSecond): TKilogramsPerSecond;
function SquarePower(AQuantity: TJoules): TSquareJoules;
function SquareRoot(AQuantity: TSquareJoules): TJoules;

{ Trigonometric functions }

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

function TQuantity.Value(const Prefixes: TPrefixes): double;
var
  I: longint;
  Exponent: longint;
  PrefixCount: longint;
begin
  PrefixCount := Length(Prefixes);
  if PrefixCount = Length(U.Prefixes) then
  begin
    Exponent := 0;
    for I := 0 to PrefixCount -1 do
      Inc(Exponent, PrefixTable[U.Prefixes[I]].Exponent * U.PrefixExponents[I]);

    for I := 0 to PrefixCount -1 do
      Dec(Exponent, PrefixTable[Prefixes[I]].Exponent * U.PrefixExponents[I]);

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

function TQuantity.ToVerboseString: string;
begin
  if (FValue < -1) or (FValue > 1) then
    result := FloatToStr(FValue) + ' ' + GetPluralName(U.PluralName, U.Prefixes)
  else
    result := FloatToStr(FValue) + ' ' + GetSingularName(U.SingularName, U.Prefixes);
end;

function TQuantity.ToString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(Prefixes);

  if Length(Prefixes) = 0 then
    result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetSymbol(U.Symbol, U.Prefixes)
  else
    result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetSymbol(U.Symbol, Prefixes)
end;

function TQuantity.ToVerboseString(Precision, Digits: longint; const Prefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(Prefixes);

  if (FactoredValue < -1) or (FactoredValue > 1) then
  begin
    if Length(Prefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetPluralName(U.PluralName, U.Prefixes)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetPluralName(U.PluralName, Prefixes);
  end else
  begin
    if Length(Prefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetSingularName(U.SingularName, U.Prefixes)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, Precision, Digits) + ' ' + GetSingularName(U.SingularName, Prefixes)
  end;
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

// main definition [ s2 ] = [ s ] * [ s ]

operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m2 ] = [ m ] * [ m ]

operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m3 ] = [ m2 ] * [ m ]

operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m4 ] = [ m3 ] * [ m ]

operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]

operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m5 ] = [ m4 ] * [ m ]

operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]

operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m6 ] = [ m5 ] * [ m ]

operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]

operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]

operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ kg2 ] = [ kg ] * [ kg ]

operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ A2 ] = [ A ] * [ A ]

operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ K2 ] = [ K ] * [ K ]

operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ K3 ] = [ K2 ] * [ K ]

operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ K4 ] = [ K3 ] * [ K ]

operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]

operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ sr ] = [ rad ] * [ rad ]

operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ Hz ] = [ 1 ] / [ s ]

operator /(const ALeft: double; const ARight: TSeconds): THertz;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: THertz): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TSeconds): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: THertz): TSeconds;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSecondUnitId): THertz;
begin result.FValue := ALeft; end;

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]

operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSquareSecondUnitId): TSquareHertz;
begin result.FValue := ALeft; end;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]

operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]

operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ rad/s ] = [ rad ] / [ s ]

operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TSecondUnitId): TRadiansPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]

operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
begin result.FValue := AQuantity.FValue; end;

operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;
begin result.FValue := AQuantity.FValue; end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]

operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecondSquared): TRadiansPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TRadiansPerSecondSquared; const ARight: TSeconds): TRadiansPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecondSquared): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TSecondUnitId): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s2 ] = [ rad ] / [ s2 ]

operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSecondSquared): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TRadiansPerSecondSquared; const ARight: TSquareSeconds): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSecondSquared): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TSquareSecondUnitId): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]

operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSecondSquared;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecondSquared; const ARight: TRadians): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadiansPerSecondSquared; const ARight: TSquareHertz): TRadians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]

operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSteradians; const ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]

operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m/s ] = [ m ] / [ s ]

operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TSecondUnitId): TMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s ] = [ m ] * [ Hz ]

operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: THertzUnitId): TMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// main definition [ m/s2 ] = [ m/s ] / [ s ]

operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSecondSquared): TMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSeconds): TMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecondSquared): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMetersPerSecond; const ARight: TSecondUnitId): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]

operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSecondSquared): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSquareSeconds): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSecondSquared): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TSquareSecondUnitId): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]

operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMetersPerSecondSquared; const ARight: TSquareHertz): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMetersPerSecondSquared; const ARight: TMeters): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]

operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TMeters): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TMetersPerSecondSquared): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecondSquared): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]

operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TSquareMeterUnitId): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSecondUnitId): TKilogramSquareMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]

operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ kg/m ] = [ kg ] / [ m ]

operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TMeterUnitId): TKilogramsPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]

operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]

operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]

operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ N ] = [ kg ] * [ m/s2 ]

operator *(const ALeft: TKilograms; const ARight: TMetersPerSecondSquared): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TKilograms): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TMetersPerSecondSquared): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TMeterPerSecondSquaredUnitId): TNewtons;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]

operator *(const ALeft: TKilogramsPerMeter; const ARight: TSquareMetersPerSquareSecond): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerMeter): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ N2 ] = [ N ] * [ N ]

operator *(const ALeft: TNewtons; const ARight: TNewtons): TSquareNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareNewtons; const ARight: TNewtons): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ Pa ] = [ N ] / [ m2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TSquareMeterUnitId): TPascals;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ J ] = [ N ] * [ m ]

operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtons; const ARight: TMeterUnitId): TJoules;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]

operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]

operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramMetersPerSecond): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TKilogramMetersPerSecond): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TKilogramUnitId): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]

operator *(const ALeft: TKilogramSquareMeters; const ARight: TSquareHertz): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TKilogramSquareMeters): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TSquareHertz): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareSeconds): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TJoules): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoules; const ARight: TSquareSeconds): TKilogramSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TJoules): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ W ] = [ J ] / [ s ]

operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TSecondUnitId): TWatts;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W ] = [ J ] * [ Hz ]

operator *(const ALeft: TJoules; const ARight: THertz): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TJoules): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TJoules): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: THertz): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W ] = [ N ] * [ m/s ]

operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ C ] = [ s ] * [ A ]

operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TAmpereUnitId): TCoulombs;
begin result.FValue := ALeft.FValue; end;

// main definition [ C2 ] = [ C ] * [ C ]

operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ V ] = [ W ] / [ A ]

operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TAmpereUnitId): TVolts;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V ] = [ J ] / [ C ]

operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TCoulombUnitId): TVolts;
begin result.FValue := ALeft.FValue; end;

// main definition [ V2 ] = [ V ] * [ V ]

operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ F ] = [ C ] / [ V ]

operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TVoltUnitId): TFarads;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ F ] = [ C2 ] / [ J ]

operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ Ω ] = [ V ] / [ A ]

operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVolts; const ARight: TAmpereUnitId): TOhms;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Ω ] = [ s ] / [ F ]

operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Ω ] = [ W ] / [ A2 ]

operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Ω ] = [ V2 ] / [ W ]

operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ S ] = 1 / [ Ω ]

operator /(const ALeft: double; const ARight: TOhms): TSiemens;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TSiemens): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSiemens; const ARight: TOhms): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSiemens): TOhms;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TOhmUnitId): TSiemens;
begin result.FValue := ALeft; end;

// main definition [ Wb ] = [ V ] * [ s ]

operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVolts; const ARight: TSecondUnitId): TWebers;
begin result.FValue := ALeft.FValue; end;

// main definition [ T ] = [ Wb ] / [ m2 ]

operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: TSquareMeterUnitId): TTeslas;
begin result.FValue := ALeft.FValue; end;

// main definition [ H ] = [ Wb ] / [ A ]

operator /(const ALeft: TWebers; const ARight: TAmperes): THenries;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: THenries): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenries; const ARight: TAmperes): TWebers;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: THenries): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWebers; const ARight: TAmpereUnitId): THenries;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H ] = [ Ω ] * [ s ]

operator *(const ALeft: TOhms; const ARight: TSeconds): THenries;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TOhms): THenries;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: THenries; const ARight: TOhms): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: THenries; const ARight: TSeconds): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]

operator /(const ALeft: TOhms; const ARight: THertz): THenries;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: THenries): TOhms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenries; const ARight: THertz): TOhms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TOhms; const ARight: THenries): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ lm ] = [ cd ] * [ sr ]

operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCandelas; const ARight: TSteradianUnitId): TLumens;
begin result.FValue := ALeft.FValue; end;

// main definition [ lx ] = [ lm ] / [ m2 ]

operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TSquareMeterUnitId): TLux;
begin result.FValue := ALeft.FValue; end;

// main definition [ kat ] = [ mol ] / [ s ]

operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMoles; const ARight: TSecondUnitId): TKatals;
begin result.FValue := ALeft.FValue; end;

// main definition [ J/rad ] = [ J ] / [ rad ]

operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TRadianUnitId): TJoulesPerRadian;
begin result.FValue := ALeft.FValue; end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]

operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]

operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSecondSquared): TNewtonsPerCubicMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSecondSquared): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ N/m ] = [ N ] / [ m ]

operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TMeterUnitId): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]

operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]

operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareHertz): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareHertz; const ARight: TKilograms): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareHertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareHertz): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m3/s ] = [ m3 ] / [ s ]

operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TSecondUnitId): TCubicMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]

operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ kg/s ] = [ kg ] / [ s ]

operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TSecondUnitId): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]

operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/s ] = [ kg/m ] * [ m/s ]

operator *(const ALeft: TKilogramsPerMeter; const ARight: TMetersPerSecond): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerMeter): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerMeter): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TKilogramsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/s ] = [ W ] / [ m2/s2 ]

operator /(const ALeft: TWatts; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerSecond): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TSquareMetersPerSquareSecond): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TKilogramsPerSecond): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ Pl ] = [ Pa ] * [ s ]

operator *(const ALeft: TPascals; const ARight: TSeconds): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TPascals): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPoiseuilles; const ARight: TPascals): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TPoiseuilles; const ARight: TSeconds): TPascals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TPascals; const ARight: TSecondUnitId): TPoiseuilles;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ Pl ] = [ kg/m2 ] * [ m/s ]

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TPoiseuilles; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ Pl ] = [ kg/s ] / [ m ]

operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPoiseuilles;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TPoiseuilles): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TPoiseuilles; const ARight: TMeters): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TPoiseuilles): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TPoiseuilles; const ARight: TMeterUnitId): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue; end;

// main definition [ m2/s ] = [ m2 ] / [ s ]

operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSecondUnitId): TSquareMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m2/s ] = [ Pl ] / [ kg/m3 ]

operator /(const ALeft: TPoiseuilles; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPoiseuilles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPoiseuilles; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]

operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ m4*s ] = [ m4 ] * [ s ]

operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticMeters; const ARight: TSecondUnitId): TQuarticMeterSeconds;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]

operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]

operator /(const ALeft: TKilograms; const ARight: TQuarticMeterSeconds): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticMeterSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeterSeconds): TKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeterSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]

operator /(const ALeft: TPascals; const ARight: TCubicMetersPerSecond): TKilogramsPerQuarticMeterPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TCubicMetersPerSecond): TPascals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerQuarticMeterPerSecond): TCubicMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]

operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TKilogramUnitId): TCubicMetersPerKilogram;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/kg ] = 1 / [ kg/m3 ]

operator /(const ALeft: double; const ARight: TKilogramsPerCubicMeter): TCubicMetersPerKilogram;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMetersPerKilogram): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilogramsPerCubicMeter): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TCubicMetersPerKilogram): TKilogramsPerCubicMeter;
begin result.FValue := ALeft / ARight.FValue; end;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]

operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TSquareSecondUnitId): TKilogramSquareSeconds;
begin result.FValue := ALeft.FValue; end;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]

operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]

operator *(const ALeft: TMetersPerSecondSquared; const ARight: TSquareMeters): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecondSquared): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSecondSquared): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSecondSquared;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtons; const ARight: TSquareMeterUnitId): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2 ] = [ J ] * [ m ]

operator *(const ALeft: TJoules; const ARight: TMeters): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TJoules): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TJoules): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TMeters): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]

operator *(const ALeft: TPascals; const ARight: TQuarticMeters): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TQuarticMeters; const ARight: TPascals): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TPascals): TQuarticMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TQuarticMeters): TPascals;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]

operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TMeterUnitId): TSquareKilogramsPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]

operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue; end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]

operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]

operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareSeconds): TCubicMetersPerKilogram;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]

operator /(const ALeft: TCubicMeters; const ARight: TKilogramSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilogramSquareSeconds): TCubicMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilogramSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TKilograms): TNewtonSquareMetersPerSquareKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilograms): TCubicMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ 1/K ] = 1 / [ K ]

operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TKelvinUnitId): TReciprocalKelvins;
begin result.FValue := ALeft; end;

// main definition [ kg*K] = [ kg ] * [ K ]

operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TKelvinUnitId): TKilogramKelvins;
begin result.FValue := ALeft.FValue; end;

// main definition [ J/K ] = [ J ] / [ K ]

operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TKelvinUnitId): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]

operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]

operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ m*K ] = [ m ] * [ K ]

operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TKelvinUnitId): TMeterKelvins;
begin result.FValue := ALeft.FValue; end;

// main definition [ K/m ] = [ K ] / [ m ]

operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKelvins; const ARight: TMeterUnitId): TKelvinsPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m ] = [ W ] / [ m ]

operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TMeterUnitId): TWattsPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]

operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TSquareMeterUnitId): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/K ] = [ W ] / [ K ]

operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TKelvinUnitId): TWattsPerKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]

operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]

operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerMeter; const ARight: TKelvinUnitId): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]

operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TMeterUnitId): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m2*K ] = [ m2 ] * [ K ]

operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TKelvinUnitId): TSquareMeterKelvins;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]

operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]

operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]

operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]

operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]

operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ J/mol ] = [ J ] / [ mol ]

operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TMoleUnitId): TJoulesPerMole;
begin result.FValue := ALeft.FValue; end;

// main definition [ mol*K ] = [ mol ] * [ K ]

operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoles; const ARight: TKelvinUnitId): TMoleKelvins;
begin result.FValue := ALeft.FValue; end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]

operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]

operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoleUnitId): TJoulesPerMolePerKelvin;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]

operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoulesPerMole; const ARight: TKelvinUnitId): TJoulesPerMolePerKelvin;
begin result.FValue := ALeft.FValue; end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]

operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TOhms; const ARight: TMeterUnitId): TOhmMeters;
begin result.FValue := ALeft.FValue; end;

// main definition [ V/m ] = [ V ] / [ m ]

operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVolts; const ARight: TMeterUnitId): TVoltsPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V/m ] = [ N ] / [ C ]

operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TCoulombUnitId): TVoltsPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V/m ] = [ T ] * [ m/s ]

operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ C/m ] = [ C ] / [ m ]

operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TMeterUnitId): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ C2/m ] = [ C2 ] / [ m ]

operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareCoulombs; const ARight: TMeterUnitId): TSquareCoulombsPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]

operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]

operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]

operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]

operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue; end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb;
begin result.FValue := ALeft.FValue; end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]

operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]

operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]

operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ V*m ] = [ V ] * [ m ]

operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVolts; const ARight: TMeterUnitId): TVoltMeters;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]

operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]

operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TVoltMeters; const ARight: TSecondUnitId): TVoltMetersPerSecond;
begin result.FValue := ALeft.FValue; end;

// main definition [ F/m ] = [ F ] / [ m ]

operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TFarads; const ARight: TMeterUnitId): TFaradsPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]

operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]

operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin result.FValue := ALeft / ARight.FValue; end;

// main definition [ A/m ] = [ A ] / [ m ]

operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TAmperes; const ARight: TMeterUnitId): TAmperesPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ m/A ] = [ m ] / [ A ]

operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMeters; const ARight: TAmpereUnitId): TMetersPerAmpere;
begin result.FValue := ALeft.FValue; end;

// main definition [ T*m ] = [ T ] * [ m ]

operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TTeslas; const ARight: TMeterUnitId): TTeslaMeters;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]

operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: TAmpereUnitId): TTeslaMeters;
begin result.FValue := ALeft.FValue; end;

// main definition [ T/A ] = [ T ] / [ A ]

operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TTeslas; const ARight: TAmpereUnitId): TTeslasPerAmpere;
begin result.FValue := ALeft.FValue; end;

// main definition [ H/m ] = [ H ] / [ m ]

operator /(const ALeft: THenries; const ARight: TMeters): THenriesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: THenriesPerMeter): THenries;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenriesPerMeter; const ARight: TMeters): THenries;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: THenries; const ARight: THenriesPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: THenries; const ARight: TMeterUnitId): THenriesPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]

operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenriesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperes; const ARight: THenriesPerMeter): TTeslaMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenriesPerMeter; const ARight: TAmperes): TTeslaMeters;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TTeslaMeters; const ARight: THenriesPerMeter): TAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TTeslaMeters; const ARight: TAmpereUnitId): THenriesPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]

operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenriesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenriesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: THenriesPerMeter; const ARight: TTeslasPerAmpere): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: THenriesPerMeter; const ARight: TMeters): TTeslasPerAmpere;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeterUnitId): THenriesPerMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]

operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenriesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenriesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: THenriesPerMeter; const ARight: TTeslas): TMetersPerAmpere;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: THenriesPerMeter; const ARight: TMetersPerAmpere): TTeslas;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]

operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenriesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TAmperesPerMeter; const ARight: THenriesPerMeter): TTeslas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenriesPerMeter; const ARight: TAmperesPerMeter): TTeslas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TTeslas; const ARight: THenriesPerMeter): TAmperesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]

operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenriesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareAmperes; const ARight: THenriesPerMeter): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenriesPerMeter; const ARight: TSquareAmperes): TNewtons;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TNewtons; const ARight: THenriesPerMeter): TSquareAmperes;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ rad/m ] = [ rad ] / [ m ]

operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TRadians; const ARight: TMeterUnitId): TRadiansPerMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]

operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]

operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]

operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ 1/m ] = 1 / [ m ]

operator /(const ALeft: double; const ARight: TMeters): TReciprocalMeters;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TReciprocalMeters): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TReciprocalMeters; const ARight: TMeters): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TReciprocalMeters): TMeters;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TMeterUnitId): TReciprocalMeters;
begin result.FValue := ALeft; end;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]

operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]

operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond;
begin result.FValue := ALeft / ARight.FValue; end;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]

operator *(const ALeft: TFaradsPerMeter; const ARight: THenriesPerMeter): TSquareSecondsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: THenriesPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenriesPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenriesPerMeter): TFaradsPerMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ J2 ] = [ J ] * [ J ]

operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ J*s ] = [ J ] * [ s ]

operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TJoules; const ARight: TSecondUnitId): TJouleSeconds;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J*s ] = [ J ] / [ Hz ]

operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJoules; const ARight: THertzUnitId): TJouleSeconds;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]

operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ lm/W ] = [ lm ] / [ W ]

operator /(const ALeft: TLumens; const ARight: TWatts): TLumensPerWatt;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TWatts; const ARight: TLumensPerWatt): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TLumensPerWatt; const ARight: TWatts): TLumens;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TLumensPerWatt): TWatts;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TLumens; const ARight: TWattUnitId): TLumensPerWatt;
begin result.FValue := ALeft.FValue; end;

// main definition [ 1/mol ] = 1 / [ mol ]

operator /(const ALeft: double; const ARight: TMoles): TReciprocalMoles;
begin result.FValue := ALeft / ARight.FValue; end;

operator *(const ALeft: TMoles; const ARight: TReciprocalMoles): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TReciprocalMoles; const ARight: TMoles): double;
begin result := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: double; const ARight: TReciprocalMoles): TMoles;
begin result.FValue := ALeft / ARight.FValue; end;

operator /(const ALeft: double; const ARight: TMoleUnitId): TReciprocalMoles;
begin result.FValue := ALeft; end;

// main definition [ A/m2 ] = [ A ] / [ m2 ]

operator /(const ALeft: TAmperes; const ARight: TSquareMeters): TAmperesPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TAmperesPerSquareMeter): TAmperes;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TAmperesPerSquareMeter; const ARight: TSquareMeters): TAmperes;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TAmperes; const ARight: TAmperesPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TAmperes; const ARight: TSquareMeterUnitId): TAmperesPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ A/m2 ] = [ A/m ] / [ m ]

operator /(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperesPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TAmperesPerSquareMeter): TAmperesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TAmperesPerSquareMeter; const ARight: TMeters): TAmperesPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TAmperesPerMeter; const ARight: TAmperesPerSquareMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ mol/m3 ] = [ mol ] / [ m3 ]

operator /(const ALeft: TMoles; const ARight: TCubicMeters): TMolesPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TMolesPerCubicMeter): TMoles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TMolesPerCubicMeter; const ARight: TCubicMeters): TMoles;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TMoles; const ARight: TMolesPerCubicMeter): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TMoles; const ARight: TCubicMeterUnitId): TMolesPerCubicMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ cd/m2 ] = [ cd ] / [ m2 ]

operator /(const ALeft: TCandelas; const ARight: TSquareMeters): TCandelasPerSquareMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TCandelasPerSquareMeter): TCandelas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCandelasPerSquareMeter; const ARight: TSquareMeters): TCandelas;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCandelas; const ARight: TCandelasPerSquareMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCandelas; const ARight: TSquareMeterUnitId): TCandelasPerSquareMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ C/m3 ] = [ C ] / [ m3 ]

operator /(const ALeft: TCoulombs; const ARight: TCubicMeters): TCoulombsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TCoulombsPerCubicMeter): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TCubicMeters): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerCubicMeter): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCubicMeterUnitId): TCoulombsPerCubicMeter;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ C/m3 ] = [ C/m ] / [ m2 ]

operator /(const ALeft: TCoulombsPerMeter; const ARight: TSquareMeters): TCoulombsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerCubicMeter): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TSquareMeters): TCoulombsPerMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerCubicMeter): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ C/m3 ] = [ C/m2 ] / [ m ]

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerCubicMeter): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerCubicMeter; const ARight: TMeters): TCoulombsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TCoulombsPerCubicMeter): TMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// main definition [ C/kg ] = [ C ] / [ kg ]

operator /(const ALeft: TCoulombs; const ARight: TKilograms): TCoulombsPerKilogram;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TKilograms; const ARight: TCoulombsPerKilogram): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerKilogram; const ARight: TKilograms): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerKilogram): TKilograms;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TKilogramUnitId): TCoulombsPerKilogram;
begin result.FValue := ALeft.FValue; end;

// main definition [ Gy/s ] = [ Gy ] / [ s ]

operator /(const ALeft: TGrays; const ARight: TSeconds): TGraysPerSecond;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSeconds; const ARight: TGraysPerSecond): TGrays;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TGraysPerSecond; const ARight: TSeconds): TGrays;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TGrays; const ARight: TGraysPerSecond): TSeconds;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TGrays; const ARight: TSecondUnitId): TGraysPerSecond;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/sr ] = [ W ] / [ sr ]

operator /(const ALeft: TWatts; const ARight: TSteradians): TWattsPerSteradian;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSteradians; const ARight: TWattsPerSteradian): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSteradian; const ARight: TSteradians): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSteradian): TSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TSteradianUnitId): TWattsPerSteradian;
begin result.FValue := ALeft.FValue; end;

// main definition [ m2 * sr ] = [ m2 ] * [ sr ]

operator *(const ALeft: TSquareMeters; const ARight: TSteradians): TSquareMeterSteradians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TSteradians; const ARight: TSquareMeters): TSquareMeterSteradians;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TSquareMeterSteradians; const ARight: TSquareMeters): TSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TSquareMeterSteradians; const ARight: TSteradians): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TSteradianUnitId): TSquareMeterSteradians;
begin result.FValue := ALeft.FValue; end;

// main definition [ W/m2/sr ] = [ W ] / [ m2 * sr ]

operator /(const ALeft: TWatts; const ARight: TSquareMeterSteradians): TWattsPerSquareMeterPerSteradian;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeterSteradians; const ARight: TWattsPerSquareMeterPerSteradian): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSquareMeterSteradians): TWatts;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerSteradian): TSquareMeterSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

// alternative definition [ W/m2/sr ] = [ W/m2 ] / [ sr ]

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TSteradians): TWattsPerSquareMeterPerSteradian;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSteradians; const ARight: TWattsPerSquareMeterPerSteradian): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSteradians): TWattsPerSquareMeter;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerSteradian): TSteradians;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TSteradianUnitId): TWattsPerSquareMeterPerSteradian;
begin result.FValue := ALeft.FValue; end;

// alternative definition [ W/m2/sr ] = [ W/sr ] / [ m2 ]

operator /(const ALeft: TWattsPerSteradian; const ARight: TSquareMeters): TWattsPerSquareMeterPerSteradian;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerSteradian): TWattsPerSteradian;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TWattsPerSquareMeterPerSteradian; const ARight: TSquareMeters): TWattsPerSteradian;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TWattsPerSteradian; const ARight: TWattsPerSquareMeterPerSteradian): TSquareMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TWattsPerSteradian; const ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerSteradian;
begin result.FValue := ALeft.FValue; end;

// main definition [ kat/m3 ] = [ kat ] / [ m3 ]

operator /(const ALeft: TKatals; const ARight: TCubicMeters): TKatalsPerCubicMeter;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TCubicMeters; const ARight: TKatalsPerCubicMeter): TKatals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TKatalsPerCubicMeter; const ARight: TCubicMeters): TKatals;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TKatals; const ARight: TKatalsPerCubicMeter): TCubicMeters;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TKatals; const ARight: TCubicMeterUnitId): TKatalsPerCubicMeter;
begin result.FValue := ALeft.FValue; end;

// main definition [ C/mol ] = [ C ] / [ mol ]

operator /(const ALeft: TCoulombs; const ARight: TMoles): TCoulombsPerMole;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator *(const ALeft: TMoles; const ARight: TCoulombsPerMole): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator *(const ALeft: TCoulombsPerMole; const ARight: TMoles): TCoulombs;
begin result.FValue := ALeft.FValue * ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMole): TMoles;
begin result.FValue := ALeft.FValue / ARight.FValue; end;

operator /(const ALeft: TCoulombs; const ARight: TMoleUnitId): TCoulombsPerMole;
begin result.FValue := ALeft.FValue; end;

{ Helpers }

function TSecondHelper.ToDay: specialize TQuantity<TDayUnit>;
begin result.FValue := FValue / TDayUnit.ToBaseFactor; end;

function TSecondHelper.ToHour: specialize TQuantity<THourUnit>;
begin result.FValue := FValue / THourUnit.ToBaseFactor; end;

function TSecondHelper.ToMinute: specialize TQuantity<TMinuteUnit>;
begin result.FValue := FValue / TMinuteUnit.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareDay: specialize TQuantity<TSquareDayUnit>;
begin result.FValue := FValue / TSquareDayUnit.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareHour: specialize TQuantity<TSquareHourUnit>;
begin result.FValue := FValue / TSquareHourUnit.ToBaseFactor; end;

function TSquareSecondHelper.ToSquareMinute: specialize TQuantity<TSquareMinuteUnit>;
begin result.FValue := FValue / TSquareMinuteUnit.ToBaseFactor; end;

function TMeterHelper.ToAstronomical: specialize TQuantity<TAstronomicalUnit>;
begin result.FValue := FValue / TAstronomicalUnit.ToBaseFactor; end;

function TMeterHelper.ToInch: specialize TQuantity<TInchUnit>;
begin result.FValue := FValue / TInchUnit.ToBaseFactor; end;

function TMeterHelper.ToFoot: specialize TQuantity<TFootUnit>;
begin result.FValue := FValue / TFootUnit.ToBaseFactor; end;

function TMeterHelper.ToYard: specialize TQuantity<TYardUnit>;
begin result.FValue := FValue / TYardUnit.ToBaseFactor; end;

function TMeterHelper.ToMile: specialize TQuantity<TMileUnit>;
begin result.FValue := FValue / TMileUnit.ToBaseFactor; end;

function TMeterHelper.ToNauticalMile: specialize TQuantity<TNauticalMileUnit>;
begin result.FValue := FValue / TNauticalMileUnit.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareInch: specialize TQuantity<TSquareInchUnit>;
begin result.FValue := FValue / TSquareInchUnit.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareFoot: specialize TQuantity<TSquareFootUnit>;
begin result.FValue := FValue / TSquareFootUnit.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareYard: specialize TQuantity<TSquareYardUnit>;
begin result.FValue := FValue / TSquareYardUnit.ToBaseFactor; end;

function TSquareMeterHelper.ToSquareMile: specialize TQuantity<TSquareMileUnit>;
begin result.FValue := FValue / TSquareMileUnit.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicInch: specialize TQuantity<TCubicInchUnit>;
begin result.FValue := FValue / TCubicInchUnit.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicFoot: specialize TQuantity<TCubicFootUnit>;
begin result.FValue := FValue / TCubicFootUnit.ToBaseFactor; end;

function TCubicMeterHelper.ToCubicYard: specialize TQuantity<TCubicYardUnit>;
begin result.FValue := FValue / TCubicYardUnit.ToBaseFactor; end;

function TCubicMeterHelper.ToLitre: specialize TQuantity<TLitreUnit>;
begin result.FValue := FValue / TLitreUnit.ToBaseFactor; end;

function TCubicMeterHelper.ToGallon: specialize TQuantity<TGallonUnit>;
begin result.FValue := FValue / TGallonUnit.ToBaseFactor; end;

function TKilogramHelper.ToTonne: specialize TQuantity<TTonneUnit>;
begin result.FValue := FValue / TTonneUnit.ToBaseFactor; end;

function TKilogramHelper.ToPound: specialize TQuantity<TPoundUnit>;
begin result.FValue := FValue / TPoundUnit.ToBaseFactor; end;

function TKilogramHelper.ToOunce: specialize TQuantity<TOunceUnit>;
begin result.FValue := FValue / TOunceUnit.ToBaseFactor; end;

function TKilogramHelper.ToStone: specialize TQuantity<TStoneUnit>;
begin result.FValue := FValue / TStoneUnit.ToBaseFactor; end;

function TKilogramHelper.ToTon: specialize TQuantity<TTonUnit>;
begin result.FValue := FValue / TTonUnit.ToBaseFactor; end;

function TDegreeCelsiusHelper.ToKelvin: specialize TQuantity<TKelvinUnit>;
begin result.FValue := FValue + 273.15; end;

function TKelvinHelper.ToDegreeCelsius: specialize TQuantity<TDegreeCelsiusUnit>;
begin result.FValue := FValue - 273.15; end;

function TDegreeFahrenheitHelper.ToKelvin: specialize TQuantity<TKelvinUnit>;
begin result.FValue := 5/9 * (FValue - 32) + 273.15; end;

function TKelvinHelper.ToDegreeFahrenheit: specialize TQuantity<TDegreeFahrenheitUnit>;
begin result.FValue := 9/5 * FValue - 459.67; end;

function TRadianHelper.ToDegree: specialize TQuantity<TDegreeUnit>;
begin result.FValue := FValue / TDegreeUnit.ToBaseFactor; end;

function TSteradianHelper.ToSquareDegree: specialize TQuantity<TSquareDegreeUnit>;
begin result.FValue := FValue / TSquareDegreeUnit.ToBaseFactor; end;

function TSquareHertzHelper.ToRadianPerSecondSquared: specialize TQuantity<TRadianPerSecondSquaredUnit>;
begin result.FValue := FValue; end;

function TSquareHertzHelper.ToSteradianPerSquareSecond: specialize TQuantity<TSteradianPerSquareSecondUnit>;
begin result.FValue := FValue; end;

function TMeterPerSecondHelper.ToMeterPerHour: specialize TQuantity<TMeterPerHourUnit>;
begin result.FValue := FValue / TMeterPerHourUnit.ToBaseFactor; end;

function TMeterPerSecondHelper.ToMilePerHour: specialize TQuantity<TMilePerHourUnit>;
begin result.FValue := FValue / TMilePerHourUnit.ToBaseFactor; end;

function TMeterPerSecondHelper.ToNauticalMilePerHour: specialize TQuantity<TNauticalMilePerHourUnit>;
begin result.FValue := FValue / TNauticalMilePerHourUnit.ToBaseFactor; end;

function TMeterPerSecondSquaredHelper.ToMeterPerSecondPerSecond: specialize TQuantity<TMeterPerSecondPerSecondUnit>;
begin result.FValue := FValue; end;

function TMeterPerSecondSquaredHelper.ToMeterPerHourPerSecond: specialize TQuantity<TMeterPerHourPerSecondUnit>;
begin result.FValue := FValue / TMeterPerHourPerSecondUnit.ToBaseFactor; end;

function TKilogramMeterPerSecondHelper.ToNewtonSecond: specialize TQuantity<TNewtonSecondUnit>;
begin result.FValue := FValue; end;

function TNewtonHelper.ToPoundForce: specialize TQuantity<TPoundForceUnit>;
begin result.FValue := FValue / TPoundForceUnit.ToBaseFactor; end;

function TPascalHelper.ToBar: specialize TQuantity<TBarUnit>;
begin result.FValue := FValue / TBarUnit.ToBaseFactor; end;

function TPascalHelper.ToPoundPerSquareInch: specialize TQuantity<TPoundPerSquareInchUnit>;
begin result.FValue := FValue / TPoundPerSquareInchUnit.ToBaseFactor; end;

function TPascalHelper.ToJoulePerCubicMeter: specialize TQuantity<TJoulePerCubicMeterUnit>;
begin result.FValue := FValue; end;

function TJouleHelper.ToWattHour: specialize TQuantity<TWattHourUnit>;
begin result.FValue := FValue / TWattHourUnit.ToBaseFactor; end;

function TJouleHelper.ToElettronvolt: specialize TQuantity<TElettronvoltUnit>;
begin result.FValue := FValue / TElettronvoltUnit.ToBaseFactor; end;

function TJouleHelper.ToNewtonMeter: specialize TQuantity<TNewtonMeterUnit>;
begin result.FValue := FValue; end;

function TJouleHelper.ToPoundForceInch: specialize TQuantity<TPoundForceInchUnit>;
begin result.FValue := FValue / TPoundForceInchUnit.ToBaseFactor; end;

function TCoulombHelper.ToAmpereHour: specialize TQuantity<TAmpereHourUnit>;
begin result.FValue := FValue / TAmpereHourUnit.ToBaseFactor; end;

function THertzHelper.ToBequerel: specialize TQuantity<TBequerelUnit>;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToGray: specialize TQuantity<TGrayUnit>;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToSievert: specialize TQuantity<TSievertUnit>;
begin result.FValue := FValue; end;

function TJoulePerRadianHelper.ToJoulePerDegree: specialize TQuantity<TJoulePerDegreeUnit>;
begin result.FValue := FValue / TJoulePerDegreeUnit.ToBaseFactor; end;

function TJoulePerRadianHelper.ToNewtonMeterPerRadian: specialize TQuantity<TNewtonMeterPerRadianUnit>;
begin result.FValue := FValue; end;

function TJoulePerRadianHelper.ToNewtonMeterPerDegree: specialize TQuantity<TNewtonMeterPerDegreeUnit>;
begin result.FValue := FValue / TNewtonMeterPerDegreeUnit.ToBaseFactor; end;

function TNewtonPerMeterHelper.ToPoundForcePerInch: specialize TQuantity<TPoundForcePerInchUnit>;
begin result.FValue := FValue / TPoundForcePerInchUnit.ToBaseFactor; end;

function TPoiseuilleHelper.ToPascalSecond: specialize TQuantity<TPascalSecondUnit>;
begin result.FValue := FValue; end;

function TSquareMeterPerSquareSecondHelper.ToJoulePerKilogram: specialize TQuantity<TJoulePerKilogramUnit>;
begin result.FValue := FValue; end;

function TVoltPerMeterHelper.ToNewtonPerCoulomb: specialize TQuantity<TNewtonPerCoulombUnit>;
begin result.FValue := FValue; end;

function TVoltMeterHelper.ToNewtonSquareMeterPerCoulomb: specialize TQuantity<TNewtonSquareMeterPerCoulombUnit>;
begin result.FValue := FValue; end;

function TTeslaMeterHelper.ToNewtonPerAmpere: specialize TQuantity<TNewtonPerAmpereUnit>;
begin result.FValue := FValue; end;

function THenryPerMeterHelper.ToTeslaMeterPerAmpere: specialize TQuantity<TTeslaMeterPerAmpereUnit>;
begin result.FValue := FValue; end;

function THenryPerMeterHelper.ToNewtonPerSquareAmpere: specialize TQuantity<TNewtonPerSquareAmpereUnit>;
begin result.FValue := FValue; end;

function TJouleSecondHelper.ToElettronvoltSecond: specialize TQuantity<TElettronvoltSecondUnit>;
begin result.FValue := FValue / TElettronvoltSecondUnit.ToBaseFactor; end;

{ Power quantities }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TMeters): TSquareMeters;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareMeters): TMeters;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TMeters): TCubicMeters;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TCubicMeters): TMeters;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function QuarticPower(AQuantity: TMeters): TQuarticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 4); end;

function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
begin result.FValue := Power(AQuantity.FValue, 1/4); end;

function QuinticPower(AQuantity: TMeters): TQuinticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 5); end;

function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
begin result.FValue := Power(AQuantity.FValue, 1/5); end;

function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SexticPower(AQuantity: TMeters): TSexticMeters;
begin result.FValue := IntPower(AQuantity.FValue, 6); end;

function SexticRoot(AQuantity: TSexticMeters): TMeters;
begin result.FValue := Power(AQuantity.FValue, 1/6); end;

function SquarePower(AQuantity: TAmperes): TSquareAmperes;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TKelvins): TSquareKelvins;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
begin result.FValue := IntPower(AQuantity.FValue, 3); end;

function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
begin result.FValue := Power(AQuantity.FValue, 1/3); end;

function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
begin result.FValue := IntPower(AQuantity.FValue, 4); end;

function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
begin result.FValue := Power(AQuantity.FValue, 1/4); end;

function SquarePower(AQuantity: TRadians): TSteradians;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSteradians): TRadians;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: THertz): TSquareHertz;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareHertz): THertz;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TNewtons): TSquareNewtons;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareNewtons): TNewtons;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TVolts): TSquareVolts;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareVolts): TVolts;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareKilogramsPerSquareSecond): TKilogramsPerSecond;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

function SquarePower(AQuantity: TJoules): TSquareJoules;
begin result.FValue := IntPower(AQuantity.FValue, 2); end;

function SquareRoot(AQuantity: TSquareJoules): TJoules;
begin result.FValue := Power(AQuantity.FValue, 1/2); end;

{ Trigonometric functions }

function Cos(const AQuantity: TRadians): double;
begin result := System.Cos(AQuantity.FValue); end;

function Sin(const AQuantity: TRadians): double;
begin result := System.Sin(AQuantity.FValue); end;

function Tan(const AQuantity: TRadians): double;
begin result := Math.Tan(AQuantity.FValue); end;

function Cotan(const AQuantity: TRadians): double;
begin result := Math.Cotan(AQuantity.FValue); end;

function Secant(const AQuantity: TRadians): double;
begin result := Math.Secant(AQuantity.FValue); end;

function Cosecant(const AQuantity: TRadians): double;
begin result := Math.Cosecant(AQuantity.FValue); end;

function ArcCos(const AValue: double): TRadians;
begin result.FValue := Math.ArcCos(AValue); end;

function ArcSin(const AValue: double): TRadians;
begin result.FValue := Math.ArcSin(AValue); end;

function ArcTan(const AValue: double): TRadians;
begin result.FValue := System.ArcTan(AValue); end;

function ArcTan2(const x, y: double): TRadians;
begin result.FValue := Math.ArcTan2(x, y); end;

end.
