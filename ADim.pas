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

unit ADim;

{$H+}{$modeSwitch advancedRecords}
{$WARN NO_RETVAL OFF}

interface

uses SysUtils;

type
  { TQuantity }
  generic TQuantity<U> = record
    type TSelf = specialize TQuantity<U>;
  private
    FValue: double;
  public
    function Abs: TSelf;
    function Value: double;
    function ToString: string;
    function ToString(Precision, Digits: longint): string;
    function ToVerboseString: string;
    function ToVerboseString(Precision, Digits: longint): string;
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

  { TFactoredUnitId }
  generic TFactoredUnitId<BaseU, U> = record
    type TSelf = specialize TFactoredUnitId<BaseU, U>;
    type TBaseQuantity = specialize TQuantity<BaseU>;
    type TFactoredQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TFactoredQuantity; inline; static;
  end;

type
  { Unit of Second }
  TSecondUnit = record
    const Symbol = 's';
    const Name   = 'second';
  end;
  TSeconds = specialize TQuantity<TSecondUnit>;
  TSecondUnitId = specialize TUnitId<TSecondUnit>;

var
  s: TSecondUnitId;

type
  { Unit of Year }
  TYearUnit = record
    const Symbol = 'year';
    const Name   = 'year';
    const Factor = 31556952;
  end;
  TYearUnitId = specialize TFactoredUnitId<TSecondUnit, TYearUnit>;

const
  year: specialize TQuantity<TSecondUnit> = (FValue: TYearUnit.Factor);

type
  { Unit of Day }
  TDayUnit = record
    const Symbol = 'day';
    const Name   = 'day';
    const Factor = 86400;
  end;
  TDayUnitId = specialize TFactoredUnitId<TSecondUnit, TDayUnit>;

const
  day: specialize TQuantity<TSecondUnit> = (FValue: TDayUnit.Factor);

type
  { Unit of Hour }
  THourUnit = record
    const Symbol = 'h';
    const Name   = 'hour';
    const Factor = 3600;
  end;
  THourUnitId = specialize TFactoredUnitId<TSecondUnit, THourUnit>;

const
  hour: specialize TQuantity<TSecondUnit> = (FValue: THourUnit.Factor);

type
  { Unit of Minute }
  TMinuteUnit = record
    const Symbol = 'min';
    const Name   = 'minute';
    const Factor = 60;
  end;
  TMinuteUnitId = specialize TFactoredUnitId<TSecondUnit, TMinuteUnit>;

const
  minute: specialize TQuantity<TSecondUnit> = (FValue: TMinuteUnit.Factor);

type
  { Unit of Decisecond }
  TDecisecondUnit = record
    const Symbol = 'ds';
    const Name   = 'decisecond';
    const Factor = 1E-01;
  end;
  TDecisecondUnitId = specialize TFactoredUnitId<TSecondUnit, TDecisecondUnit>;

const
  ds: specialize TQuantity<TSecondUnit> = (FValue: TDecisecondUnit.Factor);

type
  { Unit of Centisecond }
  TCentisecondUnit = record
    const Symbol = 'cs';
    const Name   = 'centisecond';
    const Factor = 1E-02;
  end;
  TCentisecondUnitId = specialize TFactoredUnitId<TSecondUnit, TCentisecondUnit>;

const
  cs: specialize TQuantity<TSecondUnit> = (FValue: TCentisecondUnit.Factor);

type
  { Unit of Millisecond }
  TMillisecondUnit = record
    const Symbol = 'ms';
    const Name   = 'millisecond';
    const Factor = 1E-03;
  end;
  TMillisecondUnitId = specialize TFactoredUnitId<TSecondUnit, TMillisecondUnit>;

const
  ms: specialize TQuantity<TSecondUnit> = (FValue: TMillisecondUnit.Factor);

type
  { Unit of Microsecond }
  TMicrosecondUnit = record
    const Symbol = 'us';
    const Name   = 'microsecond';
    const Factor = 1E-06;
  end;
  TMicrosecondUnitId = specialize TFactoredUnitId<TSecondUnit, TMicrosecondUnit>;

const
  us: specialize TQuantity<TSecondUnit> = (FValue: TMicrosecondUnit.Factor);

type
  { Unit of Nanosecond }
  TNanosecondUnit = record
    const Symbol = 'ns';
    const Name   = 'nanosecond';
    const Factor = 1E-09;
  end;
  TNanosecondUnitId = specialize TFactoredUnitId<TSecondUnit, TNanosecondUnit>;

const
  ns: specialize TQuantity<TSecondUnit> = (FValue: TNanosecondUnit.Factor);

type
  { Unit of Picosecond }
  TPicosecondUnit = record
    const Symbol = 'ps';
    const Name   = 'picosecond';
    const Factor = 1E-12;
  end;
  TPicosecondUnitId = specialize TFactoredUnitId<TSecondUnit, TPicosecondUnit>;

const
  ps: specialize TQuantity<TSecondUnit> = (FValue: TPicosecondUnit.Factor);

type
  { Unit of SquareSecond }
  TSquareSecondUnit = record
    const Symbol = 's2';
    const Name   = 'square second';
  end;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;
  TSquareSecondUnitId = specialize TUnitId<TSquareSecondUnit>;

var
  s2: TSquareSecondUnitId;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

type
  { Unit of Meter }
  TMeterUnit = record
    const Symbol = 'm';
    const Name   = 'meter';
  end;
  TMeters = specialize TQuantity<TMeterUnit>;
  TMeterUnitId = specialize TUnitId<TMeterUnit>;

var
  m: TMeterUnitId;

type
  { Unit of Astronomical }
  TAstronomicalUnit = record
    const Symbol = 'au';
    const Name   = 'astronomical unit';
    const Factor = 149597870691;
  end;
  TAstronomicalUnitId = specialize TFactoredUnitId<TMeterUnit, TAstronomicalUnit>;

const
  au: specialize TQuantity<TMeterUnit> = (FValue: TAstronomicalUnit.Factor);

type
  { Unit of Kilometer }
  TKilometerUnit = record
    const Symbol = 'km';
    const Name   = 'kilometer';
    const Factor = 1E+03;
  end;
  TKilometerUnitId = specialize TFactoredUnitId<TMeterUnit, TKilometerUnit>;

const
  km: specialize TQuantity<TMeterUnit> = (FValue: TKilometerUnit.Factor);

type
  { Unit of Hectometer }
  THectometerUnit = record
    const Symbol = 'hm';
    const Name   = 'hectometer';
    const Factor = 1E+02;
  end;
  THectometerUnitId = specialize TFactoredUnitId<TMeterUnit, THectometerUnit>;

const
  hm: specialize TQuantity<TMeterUnit> = (FValue: THectometerUnit.Factor);

type
  { Unit of Decameter }
  TDecameterUnit = record
    const Symbol = 'dam';
    const Name   = 'decameter';
    const Factor = 1E+01;
  end;
  TDecameterUnitId = specialize TFactoredUnitId<TMeterUnit, TDecameterUnit>;

const
  dam: specialize TQuantity<TMeterUnit> = (FValue: TDecameterUnit.Factor);

type
  { Unit of Decimeter }
  TDecimeterUnit = record
    const Symbol = 'dm';
    const Name   = 'decimeter';
    const Factor = 1E-01;
  end;
  TDecimeterUnitId = specialize TFactoredUnitId<TMeterUnit, TDecimeterUnit>;

const
  dm: specialize TQuantity<TMeterUnit> = (FValue: TDecimeterUnit.Factor);

type
  { Unit of Centimeter }
  TCentimeterUnit = record
    const Symbol = 'cm';
    const Name   = 'centimeter';
    const Factor = 1E-02;
  end;
  TCentimeterUnitId = specialize TFactoredUnitId<TMeterUnit, TCentimeterUnit>;

const
  cm: specialize TQuantity<TMeterUnit> = (FValue: TCentimeterUnit.Factor);

type
  { Unit of Millimeter }
  TMillimeterUnit = record
    const Symbol = 'mm';
    const Name   = 'millimeter';
    const Factor = 1E-03;
  end;
  TMillimeterUnitId = specialize TFactoredUnitId<TMeterUnit, TMillimeterUnit>;

const
  mm: specialize TQuantity<TMeterUnit> = (FValue: TMillimeterUnit.Factor);

type
  { Unit of Micrometer }
  TMicrometerUnit = record
    const Symbol = 'um';
    const Name   = 'micrometer';
    const Factor = 1E-06;
  end;
  TMicrometerUnitId = specialize TFactoredUnitId<TMeterUnit, TMicrometerUnit>;

const
  um: specialize TQuantity<TMeterUnit> = (FValue: TMicrometerUnit.Factor);

type
  { Unit of Nanometer }
  TNanometerUnit = record
    const Symbol = 'nm';
    const Name   = 'nanometer';
    const Factor = 1E-09;
  end;
  TNanometerUnitId = specialize TFactoredUnitId<TMeterUnit, TNanometerUnit>;

const
  nm: specialize TQuantity<TMeterUnit> = (FValue: TNanometerUnit.Factor);

type
  { Unit of Picometer }
  TPicometerUnit = record
    const Symbol = 'pm';
    const Name   = 'picometer';
    const Factor = 1E-12;
  end;
  TPicometerUnitId = specialize TFactoredUnitId<TMeterUnit, TPicometerUnit>;

const
  pm: specialize TQuantity<TMeterUnit> = (FValue: TPicometerUnit.Factor);

type
  { Unit of SquareMeter }
  TSquareMeterUnit = record
    const Symbol = 'm2';
    const Name   = 'square meter';
  end;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;
  TSquareMeterUnitId = specialize TUnitId<TSquareMeterUnit>;

var
  m2: TSquareMeterUnitId;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

type
  { Unit of SquareKilometer }
  TSquareKilometerUnit = record
    const Symbol = 'km2';
    const Name   = 'square kilometer';
    const Factor = 1E+06;
  end;
  TSquareKilometerUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareKilometerUnit>;

const
  km2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareKilometerUnit.Factor);

type
  { Unit of SquareHectometer }
  TSquareHectometerUnit = record
    const Symbol = 'hm2';
    const Name   = 'square hectometer';
    const Factor = 1E+04;
  end;
  TSquareHectometerUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareHectometerUnit>;

const
  hm2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareHectometerUnit.Factor);

type
  { Unit of SquareDecameter }
  TSquareDecameterUnit = record
    const Symbol = 'dam2';
    const Name   = 'square decameter';
    const Factor = 1E+02;
  end;
  TSquareDecameterUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareDecameterUnit>;

const
  dam2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareDecameterUnit.Factor);

type
  { Unit of SquareDecimeter }
  TSquareDecimeterUnit = record
    const Symbol = 'dm2';
    const Name   = 'square decimeter';
    const Factor = 1E-02;
  end;
  TSquareDecimeterUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareDecimeterUnit>;

const
  dm2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareDecimeterUnit.Factor);

type
  { Unit of SquareCentimeter }
  TSquareCentimeterUnit = record
    const Symbol = 'cm2';
    const Name   = 'square centimeter';
    const Factor = 1E-04;
  end;
  TSquareCentimeterUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareCentimeterUnit>;

const
  cm2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareCentimeterUnit.Factor);

type
  { Unit of SquareMillimeter }
  TSquareMillimeterUnit = record
    const Symbol = 'mm2';
    const Name   = 'square millimeter';
    const Factor = 1E-06;
  end;
  TSquareMillimeterUnitId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareMillimeterUnit>;

const
  mm2: specialize TQuantity<TSquareMeterUnit> = (FValue: TSquareMillimeterUnit.Factor);

type
  { Unit of CubicMeter }
  TCubicMeterUnit = record
    const Symbol = 'm3';
    const Name   = 'cubic meter';
  end;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;
  TCubicMeterUnitId = specialize TUnitId<TCubicMeterUnit>;

var
  m3: TCubicMeterUnitId;

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

type
  { Unit of CubicKilometer }
  TCubicKilometerUnit = record
    const Symbol = 'km3';
    const Name   = 'cubic kilometer';
    const Factor = 1E+09;
  end;
  TCubicKilometerUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicKilometerUnit>;

const
  km3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicKilometerUnit.Factor);

type
  { Unit of CubicHectometer }
  TCubicHectometerUnit = record
    const Symbol = 'hm3';
    const Name   = 'cubic hectometer';
    const Factor = 1E+06;
  end;
  TCubicHectometerUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicHectometerUnit>;

const
  hm3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicHectometerUnit.Factor);

type
  { Unit of CubicDecameter }
  TCubicDecameterUnit = record
    const Symbol = 'dam3';
    const Name   = 'cubic decameter';
    const Factor = 1E+03;
  end;
  TCubicDecameterUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicDecameterUnit>;

const
  dam3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicDecameterUnit.Factor);

type
  { Unit of CubicDecimeter }
  TCubicDecimeterUnit = record
    const Symbol = 'dm3';
    const Name   = 'cubic decimeter';
    const Factor = 1E-03;
  end;
  TCubicDecimeterUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicDecimeterUnit>;

const
  dm3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicDecimeterUnit.Factor);

type
  { Unit of CubicCentimeter }
  TCubicCentimeterUnit = record
    const Symbol = 'cm3';
    const Name   = 'cubic centimeter';
    const Factor = 1E-06;
  end;
  TCubicCentimeterUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicCentimeterUnit>;

const
  cm3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicCentimeterUnit.Factor);

type
  { Unit of CubicMillimeter }
  TCubicMillimeterUnit = record
    const Symbol = 'mm3';
    const Name   = 'cubic millimeter';
    const Factor = 1E-09;
  end;
  TCubicMillimeterUnitId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicMillimeterUnit>;

const
  mm3: specialize TQuantity<TCubicMeterUnit> = (FValue: TCubicMillimeterUnit.Factor);

type
  { Unit of QuarticMeter }
  TQuarticMeterUnit = record
    const Symbol = 'm4';
    const Name   = 'quartic meter';
  end;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;
  TQuarticMeterUnitId = specialize TUnitId<TQuarticMeterUnit>;

var
  m4: TQuarticMeterUnitId;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters; inline;

type
  { Unit of QuarticKilometer }
  TQuarticKilometerUnit = record
    const Symbol = 'km4';
    const Name   = 'quartic kilometer';
    const Factor = 1E+12;
  end;
  TQuarticKilometerUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticKilometerUnit>;

const
  km4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticKilometerUnit.Factor);

type
  { Unit of QuarticHectometer }
  TQuarticHectometerUnit = record
    const Symbol = 'hm4';
    const Name   = 'quartic hectometer';
    const Factor = 1E+08;
  end;
  TQuarticHectometerUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticHectometerUnit>;

const
  hm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticHectometerUnit.Factor);

type
  { Unit of QuarticDecameter }
  TQuarticDecameterUnit = record
    const Symbol = 'dam4';
    const Name   = 'quartic decameter';
    const Factor = 1E+04;
  end;
  TQuarticDecameterUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticDecameterUnit>;

const
  dam4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticDecameterUnit.Factor);

type
  { Unit of QuarticDecimeter }
  TQuarticDecimeterUnit = record
    const Symbol = 'dm4';
    const Name   = 'quartic decimeter';
    const Factor = 1E-04;
  end;
  TQuarticDecimeterUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticDecimeterUnit>;

const
  dm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticDecimeterUnit.Factor);

type
  { Unit of QuarticCentimeter }
  TQuarticCentimeterUnit = record
    const Symbol = 'cm4';
    const Name   = 'quartic centimeter';
    const Factor = 1E-08;
  end;
  TQuarticCentimeterUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticCentimeterUnit>;

const
  cm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticCentimeterUnit.Factor);

type
  { Unit of QuarticMillimeter }
  TQuarticMillimeterUnit = record
    const Symbol = 'mm4';
    const Name   = 'quartic millimeter';
    const Factor = 1E-12;
  end;
  TQuarticMillimeterUnitId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticMillimeterUnit>;

const
  mm4: specialize TQuantity<TQuarticMeterUnit> = (FValue: TQuarticMillimeterUnit.Factor);

type
  { Unit of QuinticMeter }
  TQuinticMeterUnit = record
    const Symbol = 'm5';
    const Name   = 'quintic meter';
  end;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;
  TQuinticMeterUnitId = specialize TUnitId<TQuinticMeterUnit>;

var
  m5: TQuinticMeterUnitId;

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
  { Unit of SexticMeter }
  TSexticMeterUnit = record
    const Symbol = 'm6';
    const Name   = 'sextic meter';
  end;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;
  TSexticMeterUnitId = specialize TUnitId<TSexticMeterUnit>;

var
  m6: TSexticMeterUnitId;

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
  { Unit of Kilogram }
  TKilogramUnit = record
    const Symbol = 'kg';
    const Name   = 'kilogram';
  end;
  TKilograms = specialize TQuantity<TKilogramUnit>;
  TKilogramUnitId = specialize TUnitId<TKilogramUnit>;

var
  kg: TKilogramUnitId;

type
  { Unit of Hectogram }
  THectogramUnit = record
    const Symbol = 'hg';
    const Name   = 'hectogram';
    const Factor = 1E-01;
  end;
  THectogramUnitId = specialize TFactoredUnitId<TKilogramUnit, THectogramUnit>;

const
  hg: specialize TQuantity<TKilogramUnit> = (FValue: THectogramUnit.Factor);

type
  { Unit of Decagram }
  TDecagramUnit = record
    const Symbol = 'dag';
    const Name   = 'decagram';
    const Factor = 1E-02;
  end;
  TDecagramUnitId = specialize TFactoredUnitId<TKilogramUnit, TDecagramUnit>;

const
  dag: specialize TQuantity<TKilogramUnit> = (FValue: TDecagramUnit.Factor);

type
  { Unit of Gram }
  TGramUnit = record
    const Symbol = 'g';
    const Name   = 'gram';
    const Factor = 1E-03;
  end;
  TGramUnitId = specialize TFactoredUnitId<TKilogramUnit, TGramUnit>;

const
  g: specialize TQuantity<TKilogramUnit> = (FValue: TGramUnit.Factor);

type
  { Unit of Decigram }
  TDecigramUnit = record
    const Symbol = 'dg';
    const Name   = 'decigram';
    const Factor = 1E-04;
  end;
  TDecigramUnitId = specialize TFactoredUnitId<TKilogramUnit, TDecigramUnit>;

const
  dg: specialize TQuantity<TKilogramUnit> = (FValue: TDecigramUnit.Factor);

type
  { Unit of Centigram }
  TCentigramUnit = record
    const Symbol = 'cg';
    const Name   = 'centigram';
    const Factor = 1E-05;
  end;
  TCentigramUnitId = specialize TFactoredUnitId<TKilogramUnit, TCentigramUnit>;

const
  cg: specialize TQuantity<TKilogramUnit> = (FValue: TCentigramUnit.Factor);

type
  { Unit of Milligram }
  TMilligramUnit = record
    const Symbol = 'mg';
    const Name   = 'milligram';
    const Factor = 1E-06;
  end;
  TMilligramUnitId = specialize TFactoredUnitId<TKilogramUnit, TMilligramUnit>;

const
  mg: specialize TQuantity<TKilogramUnit> = (FValue: TMilligramUnit.Factor);

type
  { Unit of Microgram }
  TMicrogramUnit = record
    const Symbol = 'ug';
    const Name   = 'microgram';
    const Factor = 1E-09;
  end;
  TMicrogramUnitId = specialize TFactoredUnitId<TKilogramUnit, TMicrogramUnit>;

const
  ug: specialize TQuantity<TKilogramUnit> = (FValue: TMicrogramUnit.Factor);

type
  { Unit of Nanogram }
  TNanogramUnit = record
    const Symbol = 'ng';
    const Name   = 'nanogram';
    const Factor = 1E-12;
  end;
  TNanogramUnitId = specialize TFactoredUnitId<TKilogramUnit, TNanogramUnit>;

const
  ng: specialize TQuantity<TKilogramUnit> = (FValue: TNanogramUnit.Factor);

type
  { Unit of Picogram }
  TPicogramUnit = record
    const Symbol = 'pg';
    const Name   = 'picogram';
    const Factor = 1E-15;
  end;
  TPicogramUnitId = specialize TFactoredUnitId<TKilogramUnit, TPicogramUnit>;

const
  pg: specialize TQuantity<TKilogramUnit> = (FValue: TPicogramUnit.Factor);

type
  { Unit of SquareKilogram }
  TSquareKilogramUnit = record
    const Symbol = 'kg2';
    const Name   = 'square kilogram';
  end;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;
  TSquareKilogramUnitId = specialize TUnitId<TSquareKilogramUnit>;

var
  kg2: TSquareKilogramUnitId;

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

type
  { Unit of Ampere }
  TAmpereUnit = record
    const Symbol = 'A';
    const Name   = 'ampere';
  end;
  TAmperes = specialize TQuantity<TAmpereUnit>;
  TAmpereUnitId = specialize TUnitId<TAmpereUnit>;

var
  A: TAmpereUnitId;

type
  { Unit of Kiloampere }
  TKiloampereUnit = record
    const Symbol = 'kA';
    const Name   = 'kiloampere';
    const Factor = 1E+03;
  end;
  TKiloampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TKiloampereUnit>;

const
  kA: specialize TQuantity<TAmpereUnit> = (FValue: TKiloampereUnit.Factor);

type
  { Unit of Hectoampere }
  THectoampereUnit = record
    const Symbol = 'hA';
    const Name   = 'hectoampere';
    const Factor = 1E+02;
  end;
  THectoampereUnitId = specialize TFactoredUnitId<TAmpereUnit, THectoampereUnit>;

const
  hA: specialize TQuantity<TAmpereUnit> = (FValue: THectoampereUnit.Factor);

type
  { Unit of Decampere }
  TDecampereUnit = record
    const Symbol = 'daA';
    const Name   = 'decampere';
    const Factor = 1E+01;
  end;
  TDecampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TDecampereUnit>;

const
  daA: specialize TQuantity<TAmpereUnit> = (FValue: TDecampereUnit.Factor);

type
  { Unit of Deciampere }
  TDeciampereUnit = record
    const Symbol = 'dA';
    const Name   = 'deciampere';
    const Factor = 1E-01;
  end;
  TDeciampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TDeciampereUnit>;

const
  dA: specialize TQuantity<TAmpereUnit> = (FValue: TDeciampereUnit.Factor);

type
  { Unit of Centiampere }
  TCentiampereUnit = record
    const Symbol = 'cA';
    const Name   = 'centiampere';
    const Factor = 1E-02;
  end;
  TCentiampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TCentiampereUnit>;

const
  cA: specialize TQuantity<TAmpereUnit> = (FValue: TCentiampereUnit.Factor);

type
  { Unit of Milliampere }
  TMilliampereUnit = record
    const Symbol = 'mA';
    const Name   = 'milliampere';
    const Factor = 1E-03;
  end;
  TMilliampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TMilliampereUnit>;

const
  mA: specialize TQuantity<TAmpereUnit> = (FValue: TMilliampereUnit.Factor);

type
  { Unit of Microampere }
  TMicroampereUnit = record
    const Symbol = 'uA';
    const Name   = 'microampere';
    const Factor = 1E-06;
  end;
  TMicroampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TMicroampereUnit>;

const
  uA: specialize TQuantity<TAmpereUnit> = (FValue: TMicroampereUnit.Factor);

type
  { Unit of Nanoampere }
  TNanoampereUnit = record
    const Symbol = 'nA';
    const Name   = 'nanoampere';
    const Factor = 1E-09;
  end;
  TNanoampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TNanoampereUnit>;

const
  nA: specialize TQuantity<TAmpereUnit> = (FValue: TNanoampereUnit.Factor);

type
  { Unit of Picoampere }
  TPicoampereUnit = record
    const Symbol = 'pA';
    const Name   = 'picoampere';
    const Factor = 1E-12;
  end;
  TPicoampereUnitId = specialize TFactoredUnitId<TAmpereUnit, TPicoampereUnit>;

const
  picoampere: specialize TQuantity<TAmpereUnit> = (FValue: TPicoampereUnit.Factor);

type
  { Unit of SquareAmpere }
  TSquareAmpereUnit = record
    const Symbol = 'A2';
    const Name   = 'square ampere';
  end;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;
  TSquareAmpereUnitId = specialize TUnitId<TSquareAmpereUnit>;

var
  A2: TSquareAmpereUnitId;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

type
  { Unit of SquareMilliampere }
  TSquareMilliampereUnit = record
    const Symbol = 'mA2';
    const Name   = 'square milliampere';
    const Factor = 1E-06;
  end;
  TSquareMilliampereUnitId = specialize TFactoredUnitId<TSquareAmpereUnit, TSquareMilliampereUnit>;

const
  mA2: specialize TQuantity<TSquareAmpereUnit> = (FValue: TSquareMilliampereUnit.Factor);

type
  { Unit of Kelvin }
  TKelvinUnit = record
    const Symbol = 'K';
    const Name   = 'kelvin';
  end;
  TKelvins = specialize TQuantity<TKelvinUnit>;
  TKelvinUnitId = specialize TUnitId<TKelvinUnit>;

var
  K: TKelvinUnitId;

type
  { Unit of SquareKelvin }
  TSquareKelvinUnit = record
    const Symbol = 'K2';
    const Name   = 'square kelvin';
  end;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;
  TSquareKelvinUnitId = specialize TUnitId<TSquareKelvinUnit>;

var
  K2: TSquareKelvinUnitId;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

type
  { Unit of CubicKelvin }
  TCubicKelvinUnit = record
    const Symbol = 'K3';
    const Name   = 'cubic kelvin';
  end;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;
  TCubicKelvinUnitId = specialize TUnitId<TCubicKelvinUnit>;

var
  K3: TCubicKelvinUnitId;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

type
  { Unit of QuarticKelvin }
  TQuarticKelvinUnit = record
    const Symbol = 'K4';
    const Name   = 'quartic kelvin';
  end;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;
  TQuarticKelvinUnitId = specialize TUnitId<TQuarticKelvinUnit>;

var
  K4: TQuarticKelvinUnitId;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

type
  { Unit of Mole }
  TMoleUnit = record
    const Symbol = 'mol';
    const Name   = 'mole';
  end;
  TMoles = specialize TQuantity<TMoleUnit>;
  TMoleUnitId = specialize TUnitId<TMoleUnit>;

var
  mol: TMoleUnitId;

type
  { Unit of Candela }
  TCandelaUnit = record
    const Symbol = 'cd';
    const Name   = 'candela';
  end;
  TCandelas = specialize TQuantity<TCandelaUnit>;
  TCandelaUnitId = specialize TUnitId<TCandelaUnit>;

var
  cd: TCandelaUnitId;

type
  { Unit of Radian }
  TRadianUnit = record
    const Symbol = 'rad';
    const Name   = 'radian';
  end;
  TRadians = specialize TQuantity<TRadianUnit>;
  TRadianUnitId = specialize TUnitId<TRadianUnit>;

var
  rad: TRadianUnitId;

type
  { Unit of Degree }
  TDegreeUnit = record
    const Symbol = 'deg';
    const Name   = 'degree';
    const Factor = Pi/180;
  end;
  TDegreeUnitId = specialize TFactoredUnitId<TRadianUnit, TDegreeUnit>;

const
  deg: specialize TQuantity<TRadianUnit> = (FValue: TDegreeUnit.Factor);

type
  { Unit of Steradian }
  TSteradianUnit = record
    const Symbol = 'sr';
    const Name   = 'steradian';
  end;
  TSteradians = specialize TQuantity<TSteradianUnit>;
  TSteradianUnitId = specialize TUnitId<TSteradianUnit>;

var
  sr: TSteradianUnitId;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

type
  { Unit of Hertz }
  THertzUnit = record
    const Symbol = 'Hz';
    const Name   = 'hertz';
  end;
  THertz = specialize TQuantity<THertzUnit>;
  THertzUnitId = specialize TUnitId<THertzUnit>;

var
  Hz: THertzUnitId;

// main definition [ Hz ] = [ 1 ] / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator /(const ALeft: double; const {%H-}ARight: TSecondUnitId): THertz; inline;

type
  { Unit of Gigahertz }
  TGigahertzUnit = record
    const Symbol = 'GHz';
    const Name   = 'gigahertz';
    const Factor = 1E+09;
  end;
  TGigahertzUnitId = specialize TFactoredUnitId<THertzUnit, TGigahertzUnit>;

const
  GHz: specialize TQuantity<THertzUnit> = (FValue: TGigahertzUnit.Factor);

type
  { Unit of Megahertz }
  TMegahertzUnit = record
    const Symbol = 'MHz';
    const Name   = 'megahertz';
    const Factor = 1E+06;
  end;
  TMegahertzUnitId = specialize TFactoredUnitId<THertzUnit, TMegahertzUnit>;

const
  MHz: specialize TQuantity<THertzUnit> = (FValue: TMegahertzUnit.Factor);

type
  { Unit of Kilohertz }
  TKilohertzUnit = record
    const Symbol = 'kHz';
    const Name   = 'kilohertz';
    const Factor = 1E+03;
  end;
  TKilohertzUnitId = specialize TFactoredUnitId<THertzUnit, TKilohertzUnit>;

const
  kHz: specialize TQuantity<THertzUnit> = (FValue: TKilohertzUnit.Factor);

type
  { Unit of SquareHertz }
  TSquareHertzUnit = record
    const Symbol = 'Hz2';
    const Name   = 'square hertz';
  end;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;
  TSquareHertzUnitId = specialize TUnitId<TSquareHertzUnit>;

var
  Hz2: TSquareHertzUnitId;

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]
operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double; inline;
operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds; inline;
operator /(const ALeft: double; const {%H-}ARight: TSquareSecondUnitId): TSquareHertz; inline;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]
operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds; inline;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

type
  { Unit of RadianPerSecond }
  TRadianPerSecondUnit = record
    const Symbol = 'rad/s';
    const Name   = 'radian per second';
  end;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;
  TRadianPerSecondUnitId = specialize TUnitId<TRadianPerSecondUnit>;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TSecondUnitId): TRadiansPerSecond; inline;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]
operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians; inline;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;

type
  { Unit of RadianPerSquareSecond }
  TRadianPerSquareSecondUnit = record
    const Symbol = 'rad/s2';
    const Name   = 'radian per square second';
  end;
  TRadiansPerSquareSecond = specialize TQuantity<TSquareHertzUnit>;
  TRadianPerSquareSecondUnitId = specialize TUnitId<TRadianPerSquareSecondUnit>;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondUnitId): TRadiansPerSquareSecond; inline;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]
operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSquareSecond; inline;
operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TRadians): TSquareHertz; inline;
operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareHertz): TRadians; inline;

type
  { Unit of SteradianPerSquareSecond }
  TSteradianPerSquareSecondUnit = record
    const Symbol = 'rad2/s2';
    const Name   = 'square rad per square second';
  end;
  TSteradiansPerSquareSecond = specialize TQuantity<TSquareHertzUnit>;
  TSteradianPerSquareSecondUnitId = specialize TUnitId<TSteradianPerSquareSecondUnit>;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond; inline;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]
operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond; inline;
operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians; inline;

type
  { Unit of MeterPerSecond }
  TMeterPerSecondUnit = record
    const Symbol = 'm/s';
    const Name   = 'meter per second';
  end;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerSecondUnitId = specialize TUnitId<TMeterPerSecondUnit>;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TSecondUnitId): TMetersPerSecond; inline;

// alternative definition [ m/s ] = [ m ] * [ Hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters; inline;
operator *(const ALeft: TMeters; const {%H-}ARight: THertzUnitId): TMetersPerSecond; inline;

type
  { Unit of KilometerPerHour }
  TKilometerPerHourUnit = record
    const Symbol = 'km/h';
    const Name   = 'kilometer per hour';
    const Factor = 5/18;
  end;
  TKilometerPerHourUnitId = specialize TFactoredUnitId<TMeterPerSecondUnit, TKilometerPerHourUnit>;

type
  { Unit of DecimeterPerSecond }
  TDecimeterPerSecondUnit = record
    const Symbol = 'dm/s';
    const Name   = 'decimeter per second';
    const Factor = 1E-01;
  end;
  TDecimeterPerSecondUnitId = specialize TFactoredUnitId<TMeterPerSecondUnit, TDecimeterPerSecondUnit>;

type
  { Unit of CentimeterPerSecond }
  TCentimeterPerSecondUnit = record
    const Symbol = 'cm/s';
    const Name   = 'centimeter per second';
    const Factor = 1E-02;
  end;
  TCentimeterPerSecondUnitId = specialize TFactoredUnitId<TMeterPerSecondUnit, TCentimeterPerSecondUnit>;

type
  { Unit of MillimeterPerSecond }
  TMillimeterPerSecondUnit = record
    const Symbol = 'mm/s';
    const Name   = 'millimeter per second';
    const Factor = 1E-03;
  end;
  TMillimeterPerSecondUnitId = specialize TFactoredUnitId<TMeterPerSecondUnit, TMillimeterPerSecondUnit>;

type
  { Unit of MeterPerSquareSecond }
  TMeterPerSquareSecondUnit = record
    const Symbol = 'm/s2';
    const Name   = 'meter per square second';
  end;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerSquareSecondUnitId = specialize TUnitId<TMeterPerSquareSecondUnit>;

// main definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondUnitId): TMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds; inline;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]
operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareHertz): TMeters; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareHertz; inline;

type
  { Unit of KilometerPerHourPerSecond }
  TKilometerPerHourPerSecondUnit = record
    const Symbol = 'km/h/s';
    const Name   = 'kilometer-hour per second';
    const Factor = 5/18;
  end;
  TKilometerPerHourPerSecondUnitId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TKilometerPerHourPerSecondUnit>;

type
  { Unit of DecimeterPerSquareSecond }
  TDecimeterPerSquareSecondUnit = record
    const Symbol = 'dm/s2';
    const Name   = 'decimeter per square second';
    const Factor = 1E-01;
  end;
  TDecimeterPerSquareSecondUnitId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TDecimeterPerSquareSecondUnit>;

type
  { Unit of CentimeterPerSquareSecond }
  TCentimeterPerSquareSecondUnit = record
    const Symbol = 'cm/s2';
    const Name   = 'centimeter per square second';
    const Factor = 1E-02;
  end;
  TCentimeterPerSquareSecondUnitId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TCentimeterPerSquareSecondUnit>;

type
  { Unit of MillimeterPerSquareSecond }
  TMillimeterPerSquareSecondUnit = record
    const Symbol = 'mm/s2';
    const Name   = 'millimeter per square second';
    const Factor = 1E-03;
  end;
  TMillimeterPerSquareSecondUnitId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TMillimeterPerSquareSecondUnit>;

type
  { Unit of SquareMeterPerSquareSecond }
  TSquareMeterPerSquareSecondUnit = record
    const Symbol = 'm2/s2';
    const Name   = 'square meter per square second';
  end;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSquareMeterPerSquareSecondUnitId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;

type
  { Unit of KilogramMeterPerSecond }
  TKilogramMeterPerSecondUnit = record
    const Symbol = 'kg.m/s';
    const Name   = 'kilogram meter per second';
  end;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TKilogramMeterPerSecondUnitId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond; inline;

type
  { Unit of NewtonSecond }
  TNewtonSecondUnit = record
    const Symbol = 'N.s';
    const Name   = 'newton second';
  end;
  TNewtonSeconds = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TNewtonSecondUnitId = specialize TUnitId<TNewtonSecondUnit>;

type
  { Unit of KilogramSquareMeter }
  TKilogramSquareMeterUnit = record
    const Symbol = 'kg.m2';
    const Name   = 'kilogram square meter';
  end;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;
  TKilogramSquareMeterUnitId = specialize TUnitId<TKilogramSquareMeterUnit>;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramSquareMeters; inline;

type
  { Unit of KilogramSquareMeterPerSecond }
  TKilogramSquareMeterPerSecondUnit = record
    const Symbol = 'kg.m2 / s';
    const Name   = 'kilogram square meter per second';
  end;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMeterPerSecondUnitId = specialize TUnitId<TKilogramSquareMeterPerSecondUnit>;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondUnitId): TKilogramSquareMetersPerSecond; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond; inline;
operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters; inline;

type
  { Unit of KilogramPerMeter }
  TKilogramPerMeterUnit = record
    const Symbol = 'kg/m';
    const Name   = 'kilogram per meter';
  end;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;
  TKilogramPerMeterUnitId = specialize TUnitId<TKilogramPerMeterUnit>;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterUnitId): TKilogramsPerMeter; inline;

type
  { Unit of KilogramPerSquareMeter }
  TKilogramPerSquareMeterUnit = record
    const Symbol = 'kg/m2';
    const Name   = 'kilogram per square meter';
  end;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;
  TKilogramPerSquareMeterUnitId = specialize TUnitId<TKilogramPerSquareMeterUnit>;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter; inline;

type
  { Unit of KilogramPerCubicMeter }
  TKilogramPerCubicMeterUnit = record
    const Symbol = 'kg/m3';
    const Name   = 'kilogram per cubic meter';
  end;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;
  TKilogramPerCubicMeterUnitId = specialize TUnitId<TKilogramPerCubicMeterUnit>;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter; inline;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters; inline;

type
  { Unit of KilogramPerCubicMillimeter }
  TKilogramPerCubicMillimeterUnit = record
    const Symbol = 'kg/mm3';
    const Name   = 'kilogram per cubic millimeter';
    const Factor = 1E+09;
  end;
  TKilogramPerCubicMillimeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicMillimeterUnit>;

type
  { Unit of KilogramPerCubicCentimeter }
  TKilogramPerCubicCentimeterUnit = record
    const Symbol = 'kg/cm3';
    const Name   = 'kilogram per cubic centimeter';
    const Factor = 1E+06;
  end;
  TKilogramPerCubicCentimeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicCentimeterUnit>;

type
  { Unit of KilogramPerCubicDecimeter }
  TKilogramPerCubicDecimeterUnit = record
    const Symbol = 'kg/dm3';
    const Name   = 'kilogram per cubic decimeter';
    const Factor = 1E+03;
  end;
  TKilogramPerCubicDecimeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicDecimeterUnit>;

type
  { Unit of HectogramPerCubicMeter }
  THectogramPerCubicMeterUnit = record
    const Symbol = 'hg/m3';
    const Name   = 'hectogram per cubic meter';
    const Factor = 1E-01;
  end;
  THectogramPerCubicMeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, THectogramPerCubicMeterUnit>;

type
  { Unit of DecagramPerCubicMeter }
  TDecagramPerCubicMeterUnit = record
    const Symbol = 'dag/m3';
    const Name   = 'decagram per cubic meter';
    const Factor = 1E-02;
  end;
  TDecagramPerCubicMeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TDecagramPerCubicMeterUnit>;

type
  { Unit of GramPerCubicMeter }
  TGramPerCubicMeterUnit = record
    const Symbol = 'g/m3';
    const Name   = 'gram per cubic meter';
    const Factor = 1E-03;
  end;
  TGramPerCubicMeterUnitId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TGramPerCubicMeterUnit>;

type
  { Unit of Newton }
  TNewtonUnit = record
    const Symbol = 'N';
    const Name   = 'newton';
  end;
  TNewtons = specialize TQuantity<TNewtonUnit>;
  TNewtonUnitId = specialize TUnitId<TNewtonUnit>;

var
  N: TNewtonUnitId;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondUnitId): TNewtons; inline;

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
  { Unit of Giganewton }
  TGiganewtonUnit = record
    const Symbol = 'GN';
    const Name   = 'giganewton';
    const Factor = 1E+09;
  end;
  TGiganewtonUnitId = specialize TFactoredUnitId<TNewtonUnit, TGiganewtonUnit>;

const
  GN: specialize TQuantity<TNewtonUnit> = (FValue: TGiganewtonUnit.Factor);

type
  { Unit of Meganewton }
  TMeganewtonUnit = record
    const Symbol = 'MN';
    const Name   = 'meganewton';
    const Factor = 1E+06;
  end;
  TMeganewtonUnitId = specialize TFactoredUnitId<TNewtonUnit, TMeganewtonUnit>;

const
  MN: specialize TQuantity<TNewtonUnit> = (FValue: TMeganewtonUnit.Factor);

type
  { Unit of Kilonewton }
  TKilonewtonUnit = record
    const Symbol = 'kN';
    const Name   = 'kilonewton';
    const Factor = 1E+03;
  end;
  TKilonewtonUnitId = specialize TFactoredUnitId<TNewtonUnit, TKilonewtonUnit>;

const
  kN: specialize TQuantity<TNewtonUnit> = (FValue: TKilonewtonUnit.Factor);

type
  { Unit of Pascal }
  TPascalUnit = record
    const Symbol = 'Pa';
    const Name   = 'pascal';
  end;
  TPascals = specialize TQuantity<TPascalUnit>;
  TPascalUnitId = specialize TUnitId<TPascalUnit>;

var
  Pa: TPascalUnitId;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TPascals; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of Gigapascal }
  TGigapascalUnit = record
    const Symbol = 'GPa';
    const Name   = 'gigapascal';
    const Factor = 1E+09;
  end;
  TGigapascalUnitId = specialize TFactoredUnitId<TPascalUnit, TGigapascalUnit>;

const
  GPa: specialize TQuantity<TPascalUnit> = (FValue: TGigapascalUnit.Factor);

type
  { Unit of Megapascal }
  TMegapascalUnit = record
    const Symbol = 'MPa';
    const Name   = 'megapascal';
    const Factor = 1E+06;
  end;
  TMegapascalUnitId = specialize TFactoredUnitId<TPascalUnit, TMegapascalUnit>;

const
  MPa: specialize TQuantity<TPascalUnit> = (FValue: TMegapascalUnit.Factor);

type
  { Unit of Kilopascal }
  TKilopascalUnit = record
    const Symbol = 'kPa';
    const Name   = 'kilopascal';
    const Factor = 1E+03;
  end;
  TKilopascalUnitId = specialize TFactoredUnitId<TPascalUnit, TKilopascalUnit>;

const
  kPa: specialize TQuantity<TPascalUnit> = (FValue: TKilopascalUnit.Factor);

type
  { Unit of Joule }
  TJouleUnit = record
    const Symbol = 'J';
    const Name   = 'joule';
  end;
  TJoules = specialize TQuantity<TJouleUnit>;
  TJouleUnitId = specialize TUnitId<TJouleUnit>;

var
  J: TJouleUnitId;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TJoules; inline;

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
operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramUnitId): TSquareMetersPerSquareSecond; inline;

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
  { Unit of Terajoule }
  TTerajouleUnit = record
    const Symbol = 'TJ';
    const Name   = 'terajoule';
    const Factor = 1E+12;
  end;
  TTerajouleUnitId = specialize TFactoredUnitId<TJouleUnit, TTerajouleUnit>;

const
  TJ: specialize TQuantity<TJouleUnit> = (FValue: TTerajouleUnit.Factor);

type
  { Unit of Gigajoule }
  TGigajouleUnit = record
    const Symbol = 'GJ';
    const Name   = 'gigajoule';
    const Factor = 1E+09;
  end;
  TGigajouleUnitId = specialize TFactoredUnitId<TJouleUnit, TGigajouleUnit>;

const
  GJ: specialize TQuantity<TJouleUnit> = (FValue: TGigajouleUnit.Factor);

type
  { Unit of Megajoule }
  TMegajouleUnit = record
    const Symbol = 'MJ';
    const Name   = 'magajoule';
    const Factor = 1E+06;
  end;
  TMegajouleUnitId = specialize TFactoredUnitId<TJouleUnit, TMegajouleUnit>;

const
  MJ: specialize TQuantity<TJouleUnit> = (FValue: TMegajouleUnit.Factor);

type
  { Unit of Kilojoule }
  TKilojouleUnit = record
    const Symbol = 'kJ';
    const Name   = 'kilojoule';
    const Factor = 1E+03;
  end;
  TKilojouleUnitId = specialize TFactoredUnitId<TJouleUnit, TKilojouleUnit>;

const
  kJ: specialize TQuantity<TJouleUnit> = (FValue: TKilojouleUnit.Factor);

type
  { Unit of Teraelettronvolt }
  TTeraelettronvoltUnit = record
    const Symbol = 'TeV';
    const Name   = 'teraelettronvolt ';
    const Factor = 1.60217742320523E-007;
  end;
  TTeraelettronvoltUnitId = specialize TFactoredUnitId<TJouleUnit, TTeraelettronvoltUnit>;

const
  TeV: specialize TQuantity<TJouleUnit> = (FValue: TTeraelettronvoltUnit.Factor);

type
  { Unit of Gigaelettronvolt }
  TGigaelettronvoltUnit = record
    const Symbol = 'GeV';
    const Name   = 'gigaelettronvolt ';
    const Factor = 1.60217742320523E-010;
  end;
  TGigaelettronvoltUnitId = specialize TFactoredUnitId<TJouleUnit, TGigaelettronvoltUnit>;

const
  GeV: specialize TQuantity<TJouleUnit> = (FValue: TGigaelettronvoltUnit.Factor);

type
  { Unit of Megaelettronvolt }
  TMegaelettronvoltUnit = record
    const Symbol = 'MeV';
    const Name   = 'megaelettronvolt ';
    const Factor = 1.60217742320523E-013;
  end;
  TMegaelettronvoltUnitId = specialize TFactoredUnitId<TJouleUnit, TMegaelettronvoltUnit>;

const
  MeV: specialize TQuantity<TJouleUnit> = (FValue: TMegaelettronvoltUnit.Factor);

type
  { Unit of Kiloelettronvolt }
  TKiloelettronvoltUnit = record
    const Symbol = 'keV';
    const Name   = 'kiloelettronvolt ';
    const Factor = 1.60217742320523E-016;
  end;
  TKiloelettronvoltUnitId = specialize TFactoredUnitId<TJouleUnit, TKiloelettronvoltUnit>;

const
  keV: specialize TQuantity<TJouleUnit> = (FValue: TKiloelettronvoltUnit.Factor);

type
  { Unit of Elettronvolt }
  TElettronvoltUnit = record
    const Symbol = 'eV';
    const Name   = 'elettronvolt ';
    const Factor = 1.60217742320523E-019;
  end;
  TElettronvoltUnitId = specialize TFactoredUnitId<TJouleUnit, TElettronvoltUnit>;

const
  eV: specialize TQuantity<TJouleUnit> = (FValue: TElettronvoltUnit.Factor);

type
  { Unit of Watt }
  TWattUnit = record
    const Symbol = 'W';
    const Name   = 'watt';
  end;
  TWatts = specialize TQuantity<TWattUnit>;
  TWattUnitId = specialize TUnitId<TWattUnit>;

var
  W: TWattUnitId;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TWatts; inline;

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
  { Unit of Gigawatt }
  TGigawattUnit = record
    const Symbol = 'GW';
    const Name   = 'gigawatt';
    const Factor = 1E+09;
  end;
  TGigawattUnitId = specialize TFactoredUnitId<TWattUnit, TGigawattUnit>;

const
  GW: specialize TQuantity<TWattUnit> = (FValue: TGigawattUnit.Factor);

type
  { Unit of Megawatt }
  TMegawattUnit = record
    const Symbol = 'MW';
    const Name   = 'megawatt';
    const Factor = 1E+06;
  end;
  TMegawattUnitId = specialize TFactoredUnitId<TWattUnit, TMegawattUnit>;

const
  megawatt: specialize TQuantity<TWattUnit> = (FValue: TMegawattUnit.Factor);

type
  { Unit of Kilowatt }
  TKilowattUnit = record
    const Symbol = 'kW';
    const Name   = 'kilowatt';
    const Factor = 1E+03;
  end;
  TKilowattUnitId = specialize TFactoredUnitId<TWattUnit, TKilowattUnit>;

const
  kW: specialize TQuantity<TWattUnit> = (FValue: TKilowattUnit.Factor);

type
  { Unit of Milliwatt }
  TMilliwattUnit = record
    const Symbol = 'mW';
    const Name   = 'milliwatt';
    const Factor = 1E-03;
  end;
  TMilliwattUnitId = specialize TFactoredUnitId<TWattUnit, TMilliwattUnit>;

const
  mW: specialize TQuantity<TWattUnit> = (FValue: TMilliwattUnit.Factor);

type
  { Unit of Coulomb }
  TCoulombUnit = record
    const Symbol = 'C';
    const Name   = 'coulomb';
  end;
  TCoulombs = specialize TQuantity<TCoulombUnit>;
  TCoulombUnitId = specialize TUnitId<TCoulombUnit>;

var
  C: TCoulombUnitId;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;
operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereUnitId): TCoulombs; inline;

type
  { Unit of Gigacoulomb }
  TGigacoulombUnit = record
    const Symbol = 'GC';
    const Name   = 'gigacoulomb';
    const Factor = 1E+09;
  end;
  TGigacoulombUnitId = specialize TFactoredUnitId<TCoulombUnit, TGigacoulombUnit>;

const
  GC: specialize TQuantity<TCoulombUnit> = (FValue: TGigacoulombUnit.Factor);

type
  { Unit of Megacoulomb }
  TMegacoulombUnit = record
    const Symbol = 'MC';
    const Name   = 'megacoulomb';
    const Factor = 1E+06;
  end;
  TMegacoulombUnitId = specialize TFactoredUnitId<TCoulombUnit, TMegacoulombUnit>;

const
  megacoulomb: specialize TQuantity<TCoulombUnit> = (FValue: TMegacoulombUnit.Factor);

type
  { Unit of Kilocoulomb }
  TKilocoulombUnit = record
    const Symbol = 'kC';
    const Name   = 'kilocoulomb';
    const Factor = 1E+03;
  end;
  TKilocoulombUnitId = specialize TFactoredUnitId<TCoulombUnit, TKilocoulombUnit>;

const
  kC: specialize TQuantity<TCoulombUnit> = (FValue: TKilocoulombUnit.Factor);

type
  { Unit of Millicoulomb }
  TMillicoulombUnit = record
    const Symbol = 'mC';
    const Name   = 'millicoulomb';
    const Factor = 1E-03;
  end;
  TMillicoulombUnitId = specialize TFactoredUnitId<TCoulombUnit, TMillicoulombUnit>;

const
  mC: specialize TQuantity<TCoulombUnit> = (FValue: TMillicoulombUnit.Factor);

type
  { Unit of SquareCoulomb }
  TSquareCoulombUnit = record
    const Symbol = 'C2';
    const Name   = 'square coulomb';
  end;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;
  TSquareCoulombUnitId = specialize TUnitId<TSquareCoulombUnit>;

var
  C2: TSquareCoulombUnitId;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

type
  { Unit of Volt }
  TVoltUnit = record
    const Symbol = 'V';
    const Name   = 'volt';
  end;
  TVolts = specialize TQuantity<TVoltUnit>;
  TVoltUnitId = specialize TUnitId<TVoltUnit>;

var
  V: TVoltUnitId;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereUnitId): TVolts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombUnitId): TVolts; inline;

type
  { Unit of Gigavolt }
  TGigavoltUnit = record
    const Symbol = 'GV';
    const Name   = 'gigavolt';
    const Factor = 1E+09;
  end;
  TGigavoltUnitId = specialize TFactoredUnitId<TVoltUnit, TGigavoltUnit>;

const
  GV: specialize TQuantity<TVoltUnit> = (FValue: TGigavoltUnit.Factor);

type
  { Unit of Megavolt }
  TMegavoltUnit = record
    const Symbol = 'MV';
    const Name   = 'megavolt';
    const Factor = 1E+06;
  end;
  TMegavoltUnitId = specialize TFactoredUnitId<TVoltUnit, TMegavoltUnit>;

const
  megavolt: specialize TQuantity<TVoltUnit> = (FValue: TMegavoltUnit.Factor);

type
  { Unit of Kilovolt }
  TKilovoltUnit = record
    const Symbol = 'kV';
    const Name   = 'kilovolt';
    const Factor = 1E+03;
  end;
  TKilovoltUnitId = specialize TFactoredUnitId<TVoltUnit, TKilovoltUnit>;

const
  kV: specialize TQuantity<TVoltUnit> = (FValue: TKilovoltUnit.Factor);

type
  { Unit of Millivolt }
  TMillivoltUnit = record
    const Symbol = 'mV';
    const Name   = 'millivolt';
    const Factor = 1E-03;
  end;
  TMillivoltUnitId = specialize TFactoredUnitId<TVoltUnit, TMillivoltUnit>;

const
  mV: specialize TQuantity<TVoltUnit> = (FValue: TMillivoltUnit.Factor);

type
  { Unit of SquareVolt }
  TSquareVoltUnit = record
    const Symbol = 'V2';
    const Name   = 'square volt';
  end;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;
  TSquareVoltUnitId = specialize TUnitId<TSquareVoltUnit>;

var
  V2: TSquareVoltUnitId;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

type
  { Unit of Farad }
  TFaradUnit = record
    const Symbol = 'F';
    const Name   = 'farad';
  end;
  TFarads = specialize TQuantity<TFaradUnit>;
  TFaradUnitId = specialize TUnitId<TFaradUnit>;

var
  F: TFaradUnitId;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltUnitId): TFarads; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;

type
  { Unit of Gigafarad }
  TGigafaradUnit = record
    const Symbol = 'GF';
    const Name   = 'gigafarad';
    const Factor = 1E+09;
  end;
  TGigafaradUnitId = specialize TFactoredUnitId<TFaradUnit, TGigafaradUnit>;

const
  GF: specialize TQuantity<TFaradUnit> = (FValue: TGigafaradUnit.Factor);

type
  { Unit of Megafarad }
  TMegafaradUnit = record
    const Symbol = 'MF';
    const Name   = 'megafarad';
    const Factor = 1E+06;
  end;
  TMegafaradUnitId = specialize TFactoredUnitId<TFaradUnit, TMegafaradUnit>;

const
  megafarad: specialize TQuantity<TFaradUnit> = (FValue: TMegafaradUnit.Factor);

type
  { Unit of Kilofarad }
  TKilofaradUnit = record
    const Symbol = 'kF';
    const Name   = 'kilofarad';
    const Factor = 1E+03;
  end;
  TKilofaradUnitId = specialize TFactoredUnitId<TFaradUnit, TKilofaradUnit>;

const
  kF: specialize TQuantity<TFaradUnit> = (FValue: TKilofaradUnit.Factor);

type
  { Unit of Millifarad }
  TMillifaradUnit = record
    const Symbol = 'mF';
    const Name   = 'millifarad';
    const Factor = 1E-03;
  end;
  TMillifaradUnitId = specialize TFactoredUnitId<TFaradUnit, TMillifaradUnit>;

const
  mF: specialize TQuantity<TFaradUnit> = (FValue: TMillifaradUnit.Factor);

type
  { Unit of Microfarad }
  TMicrofaradUnit = record
    const Symbol = 'uF';
    const Name   = 'microfarad';
    const Factor = 1E-06;
  end;
  TMicrofaradUnitId = specialize TFactoredUnitId<TFaradUnit, TMicrofaradUnit>;

const
  uF: specialize TQuantity<TFaradUnit> = (FValue: TMicrofaradUnit.Factor);

type
  { Unit of Nanofarad }
  TNanofaradUnit = record
    const Symbol = 'nF';
    const Name   = 'nanofarad';
    const Factor = 1E-09;
  end;
  TNanofaradUnitId = specialize TFactoredUnitId<TFaradUnit, TNanofaradUnit>;

const
  nF: specialize TQuantity<TFaradUnit> = (FValue: TNanofaradUnit.Factor);

type
  { Unit of Picofarad }
  TPicofaradUnit = record
    const Symbol = 'pF';
    const Name   = 'picofarad';
    const Factor = 1E-12;
  end;
  TPicofaradUnitId = specialize TFactoredUnitId<TFaradUnit, TPicofaradUnit>;

const
  pF: specialize TQuantity<TFaradUnit> = (FValue: TPicofaradUnit.Factor);

type
  { Unit of Ohm }
  TOhmUnit = record
    const Symbol = 'Ω';
    const Name   = 'ohm';
  end;
  TOhms = specialize TQuantity<TOhmUnit>;
  TOhmUnitId = specialize TUnitId<TOhmUnit>;

var
  ohm: TOhmUnitId;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereUnitId): TOhms; inline;

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
  { Unit of Gigaohm }
  TGigaohmUnit = record
    const Symbol = 'GΩ';
    const Name   = 'gigaohm';
    const Factor = 1E+09;
  end;
  TGigaohmUnitId = specialize TFactoredUnitId<TOhmUnit, TGigaohmUnit>;

const
  gigaohm: specialize TQuantity<TOhmUnit> = (FValue: TGigaohmUnit.Factor);

type
  { Unit of Megaohm }
  TMegaohmUnit = record
    const Symbol = 'MΩ';
    const Name   = 'megaohm';
    const Factor = 1E+06;
  end;
  TMegaohmUnitId = specialize TFactoredUnitId<TOhmUnit, TMegaohmUnit>;

const
  megaohm: specialize TQuantity<TOhmUnit> = (FValue: TMegaohmUnit.Factor);

type
  { Unit of Kiloohm }
  TKiloohmUnit = record
    const Symbol = 'kΩ';
    const Name   = 'kiloohm';
    const Factor = 1E+03;
  end;
  TKiloohmUnitId = specialize TFactoredUnitId<TOhmUnit, TKiloohmUnit>;

const
  kiloohm: specialize TQuantity<TOhmUnit> = (FValue: TKiloohmUnit.Factor);

type
  { Unit of Milliohm }
  TMilliohmUnit = record
    const Symbol = 'mΩ';
    const Name   = 'milliohm';
    const Factor = 1E-03;
  end;
  TMilliohmUnitId = specialize TFactoredUnitId<TOhmUnit, TMilliohmUnit>;

const
  milliohm: specialize TQuantity<TOhmUnit> = (FValue: TMilliohmUnit.Factor);

type
  { Unit of Microohm }
  TMicroohmUnit = record
    const Symbol = 'uΩ';
    const Name   = 'microohm';
    const Factor = 1E-06;
  end;
  TMicroohmUnitId = specialize TFactoredUnitId<TOhmUnit, TMicroohmUnit>;

const
  microohm: specialize TQuantity<TOhmUnit> = (FValue: TMicroohmUnit.Factor);

type
  { Unit of Nanoohm }
  TNanoohmUnit = record
    const Symbol = 'nΩ';
    const Name   = 'nanoohm';
    const Factor = 1E-09;
  end;
  TNanoohmUnitId = specialize TFactoredUnitId<TOhmUnit, TNanoohmUnit>;

const
  nanoohm: specialize TQuantity<TOhmUnit> = (FValue: TNanoohmUnit.Factor);

type
  { Unit of Picoohm }
  TPicoohmUnit = record
    const Symbol = 'pΩ';
    const Name   = 'picoohm';
    const Factor = 1E-12;
  end;
  TPicoohmUnitId = specialize TFactoredUnitId<TOhmUnit, TPicoohmUnit>;

const
  picoohm: specialize TQuantity<TOhmUnit> = (FValue: TPicoohmUnit.Factor);

type
  { Unit of Siemens }
  TSiemensUnit = record
    const Symbol = 'S';
    const Name   = 'siemens';
  end;
  TSiemens = specialize TQuantity<TSiemensUnit>;
  TSiemensUnitId = specialize TUnitId<TSiemensUnit>;

var
  siemens: TSiemensUnitId;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator /(const ALeft: double; const {%H-}ARight: TOhmUnitId): TSiemens; inline;

type
  { Unit of Weber }
  TWeberUnit = record
    const Symbol = 'Wb';
    const Name   = 'weber';
  end;
  TWebers = specialize TQuantity<TWeberUnit>;
  TWeberUnitId = specialize TUnitId<TWeberUnit>;

var
  Wb: TWeberUnitId;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;
operator *(const ALeft: TVolts; const {%H-}ARight: TSecondUnitId): TWebers; inline;

type
  { Unit of Tesla }
  TTeslaUnit = record
    const Symbol = 'T';
    const Name   = 'tesla';
  end;
  TTeslas = specialize TQuantity<TTeslaUnit>;
  TTeslaUnitId = specialize TUnitId<TTeslaUnit>;

var
  T: TTeslaUnitId;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterUnitId): TTeslas; inline;

type
  { Unit of Henry }
  THenryUnit = record
    const Symbol = 'H';
    const Name   = 'henry';
  end;
  THenrys = specialize TQuantity<THenryUnit>;
  THenryUnitId = specialize TUnitId<THenryUnit>;

var
  H: THenryUnitId;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys; inline;
operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers; inline;
operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes; inline;
operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereUnitId): THenrys; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys; inline;
operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds; inline;
operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenrys; inline;
operator *(const ALeft: THertz; const ARight: THenrys): TOhms; inline;
operator *(const ALeft: THenrys; const ARight: THertz): TOhms; inline;
operator /(const ALeft: TOhms; const ARight: THenrys): THertz; inline;

type
  { Unit of Lumen }
  TLumenUnit = record
    const Symbol = 'lm';
    const Name   = 'lumen';
  end;
  TLumens = specialize TQuantity<TLumenUnit>;
  TLumenUnitId = specialize TUnitId<TLumenUnit>;

var
  lm: TLumenUnitId;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;
operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianUnitId): TLumens; inline;

type
  { Unit of Lux }
  TLuxUnit = record
    const Symbol = 'lx';
    const Name   = 'lux';
  end;
  TLux = specialize TQuantity<TLuxUnit>;
  TLuxUnitId = specialize TUnitId<TLuxUnit>;

var
  lx: TLuxUnitId;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterUnitId): TLux; inline;

type
  { Unit of Bequerel }
  TBequerelUnit = record
    const Symbol = 'Bq';
    const Name   = 'bequerel';
  end;
  TBequerels = specialize TQuantity<THertzUnit>;
  TBequerelUnitId = specialize TUnitId<TBequerelUnit>;

var
  Bq: THertzUnitId;

type
  { Unit of Gray }
  TGrayUnit = record
    const Symbol = 'Gy';
    const Name   = 'gray';
  end;
  TGrays = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TGrayUnitId = specialize TUnitId<TGrayUnit>;

var
  Gy: TSquareMeterPerSquareSecondUnitId;

type
  { Unit of Milligray }
  TMilligrayUnit = record
    const Symbol = 'mGy';
    const Name   = 'milli gray';
    const Factor = 1E-03;
  end;
  TMilligrayUnitId = specialize TFactoredUnitId<TGrayUnit, TMilligrayUnit>;

const
  mGy: specialize TQuantity<TGrayUnit> = (FValue: TMilligrayUnit.Factor);

type
  { Unit of Sievert }
  TSievertUnit = record
    const Symbol = 'Sv';
    const Name   = 'sievert';
  end;
  TSieverts = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSievertUnitId = specialize TUnitId<TSievertUnit>;

var
  Sv: TSquareMeterPerSquareSecondUnitId;

type
  { Unit of MilliSievert }
  TMilliSievertUnit = record
    const Symbol = 'mSv';
    const Name   = 'millisievert';
    const Factor = 1E-03;
  end;
  TMilliSievertUnitId = specialize TFactoredUnitId<TSievertUnit, TMilliSievertUnit>;

const
  mSv: specialize TQuantity<TSievertUnit> = (FValue: TMilliSievertUnit.Factor);

type
  { Unit of Katal }
  TKatalUnit = record
    const Symbol = 'kat';
    const Name   = 'katal';
  end;
  TKatals = specialize TQuantity<TKatalUnit>;
  TKatalUnitId = specialize TUnitId<TKatalUnit>;

var
  kat: TKatalUnitId;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator /(const ALeft: TMoles; const {%H-}ARight: TSecondUnitId): TKatals; inline;

type
  { Unit of NewtonMeter }
  TNewtonMeterUnit = record
    const Symbol = 'N.m';
    const Name   = 'newton meter';
  end;
  TNewtonMeters = specialize TQuantity<TJouleUnit>;
  TNewtonMeterUnitId = specialize TUnitId<TNewtonMeterUnit>;

type
  { Unit of JoulePerRadian }
  TJoulePerRadianUnit = record
    const Symbol = 'J/rad';
    const Name   = 'joule per radian';
  end;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerRadianUnitId = specialize TUnitId<TJoulePerRadianUnit>;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TRadianUnitId): TJoulesPerRadian; inline;

type
  { Unit of JoulePerDegree }
  TJoulePerDegreeUnit = record
    const Symbol = 'J/deg';
    const Name   = 'joule per degree';
    const Factor = 180/Pi;
  end;
  TJoulePerDegreeUnitId = specialize TFactoredUnitId<TJoulePerRadianUnit, TJoulePerDegreeUnit>;

type
  { Unit of NewtonMeterPerRadian }
  TNewtonMeterPerRadianUnit = record
    const Symbol = 'N.m/rad';
    const Name   = 'newton meter per radian';
  end;
  TNewtonMetersPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TNewtonMeterPerRadianUnitId = specialize TUnitId<TNewtonMeterPerRadianUnit>;

type
  { Unit of NewtonMeterPerDegree }
  TNewtonMeterPerDegreeUnit = record
    const Symbol = 'N.m/deg';
    const Name   = 'newton meter per degree';
    const Factor = 180/Pi;
  end;
  TNewtonMeterPerDegreeUnitId = specialize TFactoredUnitId<TJoulePerRadianUnit, TNewtonMeterPerDegreeUnit>;

type
  { Unit of NewtonPerCubicMeter }
  TNewtonPerCubicMeterUnit = record
    const Symbol = 'N/m3';
    const Name   = 'newton per cubic meter';
  end;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;
  TNewtonPerCubicMeterUnitId = specialize TUnitId<TNewtonPerCubicMeterUnit>;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of NewtonPerMeter }
  TNewtonPerMeterUnit = record
    const Symbol = 'N/m';
    const Name   = 'newton per meter';
  end;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;
  TNewtonPerMeterUnitId = specialize TUnitId<TNewtonPerMeterUnit>;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TNewtonsPerMeter; inline;

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
  { Unit of NewtonPerMillimeter }
  TNewtonPerMillimeterUnit = record
    const Symbol = 'N/mm';
    const Name   = 'newton per millimeter';
    const Factor = 1E+03;
  end;
  TNewtonPerMillimeterUnitId = specialize TFactoredUnitId<TNewtonPerMeterUnit, TNewtonPerMillimeterUnit>;

type
  { Unit of CubicMeterPerSecond }
  TCubicMeterPerSecondUnit = record
    const Symbol = 'm3/s';
    const Name   = 'cubic meter per second';
  end;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;
  TCubicMeterPerSecondUnitId = specialize TUnitId<TCubicMeterPerSecondUnit>;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondUnitId): TCubicMetersPerSecond; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

type
  { Unit of KilogramPerSecond }
  TKilogramPerSecondUnit = record
    const Symbol = 'kg/s';
    const Name   = 'kilogram per second';
  end;
  TKilogramsPerSecond = specialize TQuantity<TKilogramPerSecondUnit>;
  TKilogramPerSecondUnitId = specialize TUnitId<TKilogramPerSecondUnit>;

// main definition [ kg/s ] = [ kg ] / [ s ]
operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TSecondUnitId): TKilogramsPerSecond; inline;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]
operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons; inline;
operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond; inline;

type
  { Unit of PascalSecond }
  TPascalSecondUnit = record
    const Symbol = 'Pa.s';
    const Name   = 'pascal second';
  end;
  TPascalSeconds = specialize TQuantity<TPascalSecondUnit>;
  TPascalSecondUnitId = specialize TUnitId<TPascalSecondUnit>;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals; inline;
operator *(const ALeft: TPascals; const {%H-}ARight: TSecondUnitId): TPascalSeconds; inline;

// alternative definition [ Pa*s ] = [ kg/s ] / [ m ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPascalSeconds; inline;
operator *(const ALeft: TMeters; const ARight: TPascalSeconds): TKilogramsPerSecond; inline;
operator *(const ALeft: TPascalSeconds; const ARight: TMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TPascalSeconds): TMeters; inline;
operator *(const ALeft: TPascalSeconds; const {%H-}ARight: TMeterUnitId): TKilogramsPerSecond; inline;

// alternative definition [ Pa*s ] = [ kg/m2 ] * [ m/s ]
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPascalSeconds; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter; inline;

type
  { Unit of Poise }
  TPoiseUnit = record
    const Symbol = 'P';
    const Name   = 'poise';
    const Factor = 1E-01;
  end;
  TPoiseUnitId = specialize TFactoredUnitId<TPascalSecondUnit, TPoiseUnit>;

const
  P: specialize TQuantity<TPascalSecondUnit> = (FValue: TPoiseUnit.Factor);

type
  { Unit of SquareMeterPerSecond }
  TSquareMeterPerSecondUnit = record
    const Symbol = 'm2/s';
    const Name   = 'square meter per second';
  end;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;
  TSquareMeterPerSecondUnitId = specialize TUnitId<TSquareMeterPerSecondUnit>;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondUnitId): TSquareMetersPerSecond; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of KilogramPerQuarticMeter }
  TKilogramPerQuarticMeterUnit = record
    const Symbol = 'kg/m4';
    const Name   = 'kilogram per quartic meter';
  end;
  TKilogramsPerQuarticMeter = specialize TQuantity<TKilogramPerQuarticMeterUnit>;
  TKilogramPerQuarticMeterUnitId = specialize TUnitId<TKilogramPerQuarticMeterUnit>;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms; inline;
operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters; inline;
operator /(const ALeft: TKilograms; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter; inline;

type
  { Unit of QuarticMeterSecond }
  TQuarticMeterSecondUnit = record
    const Symbol = 'm4.s';
    const Name   = 'quartic meter second';
  end;
  TQuarticMeterSeconds = specialize TQuantity<TQuarticMeterSecondUnit>;
  TQuarticMeterSecondUnitId = specialize TUnitId<TQuarticMeterSecondUnit>;

// main definition [ m4*s ] = [ m4 ] * [ s ]
operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds; inline;
operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters; inline;
operator *(const ALeft: TQuarticMeters; const {%H-}ARight: TSecondUnitId): TQuarticMeterSeconds; inline;

type
  { Unit of KilogramPerQuarticMeterPerSecond }
  TKilogramPerQuarticMeterPerSecondUnit = record
    const Symbol = 'kg/m4/s';
    const Name   = 'kilogram per quartic meter per second';
  end;
  TKilogramsPerQuarticMeterPerSecond = specialize TQuantity<TKilogramPerQuarticMeterPerSecondUnit>;
  TKilogramPerQuarticMeterPerSecondUnitId = specialize TUnitId<TKilogramPerQuarticMeterPerSecondUnit>;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond; inline;
operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters; inline;
operator /(const ALeft: TKilogramsPerSecond; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter; inline;
operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds; inline;
operator /(const ALeft: TKilogramsPerQuarticMeter; const {%H-}ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond; inline;

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
  { Unit of CubicMeterPerKilogram }
  TCubicMeterPerKilogramUnit = record
    const Symbol = 'm3/kg';
    const Name   = 'cubic meter per kilogram';
  end;
  TCubicMetersPerKilogram = specialize TQuantity<TCubicMeterPerKilogramUnit>;
  TCubicMeterPerKilogramUnitId = specialize TUnitId<TCubicMeterPerKilogramUnit>;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]
operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram; inline;
operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TKilogramUnitId): TCubicMetersPerKilogram; inline;

type
  { Unit of KilogramSquareSecond }
  TKilogramSquareSecondUnit = record
    const Symbol = 'kg.s2';
    const Name   = 'kilogram square second';
  end;
  TKilogramSquareSeconds = specialize TQuantity<TKilogramSquareSecondUnit>;
  TKilogramSquareSecondUnitId = specialize TUnitId<TKilogramSquareSecondUnit>;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds; inline;
operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareSecondUnitId): TKilogramSquareSeconds; inline;

type
  { Unit of CubicMeterPerSquareSecond }
  TCubicMeterPerSquareSecondUnit = record
    const Symbol = 'm3/s2';
    const Name   = 'cubic meter per square second';
  end;
  TCubicMetersPerSquareSecond = specialize TQuantity<TCubicMeterPerSquareSecondUnit>;
  TCubicMeterPerSquareSecondUnitId = specialize TUnitId<TCubicMeterPerSquareSecondUnit>;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters; inline;
operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond; inline;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareMeters): TCubicMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSquareSecond): TCubicMetersPerSquareSecond; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TSquareMeters; inline;
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSquareSecond; inline;

type
  { Unit of NewtonSquareMeter }
  TNewtonSquareMeterUnit = record
    const Symbol = 'N.m2';
    const Name   = 'newton square meter';
  end;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;
  TNewtonSquareMeterUnitId = specialize TUnitId<TNewtonSquareMeterUnit>;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMeters; inline;

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
  { Unit of NewtonPerSquareKilogram }
  TNewtonPerSquareKilogramUnit = record
    const Symbol = 'N/kg2';
    const Name   = 'newton per square kilogram';
  end;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;
  TNewtonPerSquareKilogramUnitId = specialize TUnitId<TNewtonPerSquareKilogramUnit>;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram; inline;

type
  { Unit of SquareKilogramPerMeter }
  TSquareKilogramPerMeterUnit = record
    const Symbol = 'kg2/m';
    const Name   = 'square kilogram per meter';
  end;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;
  TSquareKilogramPerMeterUnitId = specialize TUnitId<TSquareKilogramPerMeterUnit>;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterUnitId): TSquareKilogramsPerMeter; inline;

type
  { Unit of SquareKilogramPerSquareMeter }
  TSquareKilogramPerSquareMeterUnit = record
    const Symbol = 'kg2/m2';
    const Name   = 'square kilogram per square meter';
  end;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramPerSquareMeterUnitId = specialize TUnitId<TSquareKilogramPerSquareMeterUnit>;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter; inline;

type
  { Unit of SquareMeterPerSquareKilogram }
  TSquareMeterPerSquareKilogramUnit = record
    const Symbol = 'm2/kg2';
    const Name   = 'square meter per square kilogram';
  end;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TSquareMeterPerSquareKilogramUnitId = specialize TUnitId<TSquareMeterPerSquareKilogramUnit>;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram; inline;

type
  { Unit of NewtonSquareMeterPerSquareKilogram }
  TNewtonSquareMeterPerSquareKilogramUnit = record
    const Symbol = 'N.m2/kg2';
    const Name   = 'newton square meter per square kilogram';
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
operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram; inline;

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
  { Unit of ReciprocalKelvin }
  TReciprocalKelvinUnit = record
    const Symbol = '1/K';
    const Name   = 'reciprocal kelvin';
  end;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;
  TReciprocalKelvinUnitId = specialize TUnitId<TReciprocalKelvinUnit>;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator /(const ALeft: double; const {%H-}ARight: TKelvinUnitId): TReciprocalKelvins; inline;

type
  { Unit of KilogramKelvin }
  TKilogramKelvinUnit = record
    const Symbol = 'kg.K';
    const Name   = 'kilogram kelvin';
  end;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;
  TKilogramKelvinUnitId = specialize TUnitId<TKilogramKelvinUnit>;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;
operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinUnitId): TKilogramKelvins; inline;

type
  { Unit of JoulePerKelvin }
  TJoulePerKelvinUnit = record
    const Symbol = 'J/K';
    const Name   = 'joule per kelvin';
  end;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;
  TJoulePerKelvinUnitId = specialize TUnitId<TJoulePerKelvinUnit>;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinUnitId): TJoulesPerKelvin; inline;

type
  { Unit of JoulePerKilogram }
  TJoulePerKilogramUnit = record
    const Symbol = 'J/kg';
    const Name   = 'joule per kilogram';
  end;
  TJoulesPerKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TJoulePerKilogramUnitId = specialize TUnitId<TJoulePerKilogramUnit>;

type
  { Unit of JoulePerKilogramPerKelvin }
  TJoulePerKilogramPerKelvinUnit = record
    const Symbol = 'J/kg/K';
    const Name   = 'joule per kilogram per kelvin';
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
operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin; inline;

type
  { Unit of MeterKelvin }
  TMeterKelvinUnit = record
    const Symbol = 'm.K';
    const Name   = 'meter kelvin';
  end;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;
  TMeterKelvinUnitId = specialize TUnitId<TMeterKelvinUnit>;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;
operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinUnitId): TMeterKelvins; inline;

type
  { Unit of KelvinPerMeter }
  TKelvinPerMeterUnit = record
    const Symbol = 'K/m';
    const Name   = 'kelvin per meter';
  end;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;
  TKelvinPerMeterUnitId = specialize TUnitId<TKelvinPerMeterUnit>;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterUnitId): TKelvinsPerMeter; inline;

type
  { Unit of WattPerMeter }
  TWattPerMeterUnit = record
    const Symbol = 'W/m';
    const Name   = 'watt per meter';
  end;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;
  TWattPerMeterUnitId = specialize TUnitId<TWattPerMeterUnit>;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TMeterUnitId): TWattsPerMeter; inline;

type
  { Unit of WattPerSquareMeter }
  TWattPerSquareMeterUnit = record
    const Symbol = 'W/m2';
    const Name   = 'watt per square meter';
  end;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;
  TWattPerSquareMeterUnitId = specialize TUnitId<TWattPerSquareMeterUnit>;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeter; inline;

type
  { Unit of WattPerKelvin }
  TWattPerKelvinUnit = record
    const Symbol = 'W/K';
    const Name   = 'watt per kelvin';
  end;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;
  TWattPerKelvinUnitId = specialize TUnitId<TWattPerKelvinUnit>;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinUnitId): TWattsPerKelvin; inline;

type
  { Unit of WattPerMeterPerKelvin }
  TWattPerMeterPerKelvinUnit = record
    const Symbol = 'W/m/K';
    const Name   = 'watt per meter per kelvin';
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
operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TMeterUnitId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter; inline;

type
  { Unit of SquareMeterKelvin }
  TSquareMeterKelvinUnit = record
    const Symbol = 'm2.K';
    const Name   = 'square meter kelvin';
  end;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;
  TSquareMeterKelvinUnitId = specialize TUnitId<TSquareMeterKelvinUnit>;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinUnitId): TSquareMeterKelvins; inline;

type
  { Unit of WattPerSquareMeterPerKelvin }
  TWattPerSquareMeterPerKelvinUnit = record
    const Symbol = 'W/m2/K';
    const Name   = 'watt per square meter per kelvin';
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
operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin; inline;

type
  { Unit of SquareMeterQuarticKelvin }
  TSquareMeterQuarticKelvinUnit = record
    const Symbol = 'm2.K4';
    const Name   = 'square meter quartic kelvin';
  end;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvinUnitId = specialize TUnitId<TSquareMeterQuarticKelvinUnit>;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins; inline;

type
  { Unit of WattPerQuarticKelvin }
  TWattPerQuarticKelvinUnit = record
    const Symbol = 'W/K4';
    const Name   = 'watt per quartic kelvin';
  end;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;
  TWattPerQuarticKelvinUnitId = specialize TUnitId<TWattPerQuarticKelvinUnit>;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin; inline;

type
  { Unit of WattPerSquareMeterPerQuarticKelvin }
  TWattPerSquareMeterPerQuarticKelvinUnit = record
    const Symbol = 'W/m2/K4';
    const Name   = 'watt per square meter per quartic kelvin';
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
operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin; inline;

type
  { Unit of JoulePerMole }
  TJoulePerMoleUnit = record
    const Symbol = 'J/mol';
    const Name   = 'joule per mole';
  end;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;
  TJoulePerMoleUnitId = specialize TUnitId<TJoulePerMoleUnit>;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: TMoleUnitId): TJoulesPerMole; inline;

type
  { Unit of MoleKelvin }
  TMoleKelvinUnit = record
    const Symbol = 'mol.K';
    const Name   = 'mole kelvin';
  end;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;
  TMoleKelvinUnitId = specialize TUnitId<TMoleKelvinUnit>;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;
operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinUnitId): TMoleKelvins; inline;

type
  { Unit of JoulePerMolePerKelvin }
  TJoulePerMolePerKelvinUnit = record
    const Symbol = 'J/mol/K';
    const Name   = 'joule per mole per kelvin';
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
operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TMoleUnitId): TJoulesPerMolePerKelvin; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins; inline;
operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinUnitId): TJoulesPerMolePerKelvin; inline;

type
  { Unit of OhmMeter }
  TOhmMeterUnit = record
    const Symbol = 'Ω.m';
    const Name   = 'ohm meter';
  end;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;
  TOhmMeterUnitId = specialize TUnitId<TOhmMeterUnit>;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;
operator *(const ALeft: TOhms; const {%H-}ARight: TMeterUnitId): TOhmMeters; inline;

type
  { Unit of VoltPerMeter }
  TVoltPerMeterUnit = record
    const Symbol = 'V/m';
    const Name   = 'volt per meter';
  end;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;
  TVoltPerMeterUnitId = specialize TUnitId<TVoltPerMeterUnit>;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator /(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombUnitId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

type
  { Unit of CoulombPerMeter }
  TCoulombPerMeterUnit = record
    const Symbol = 'C/m';
    const Name   = 'coulomb per meter';
  end;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;
  TCoulombPerMeterUnitId = specialize TUnitId<TCoulombPerMeterUnit>;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterUnitId): TCoulombsPerMeter; inline;

type
  { Unit of SquareCoulombPerMeter }
  TSquareCoulombPerMeterUnit = record
    const Symbol = 'C2/m';
    const Name   = 'square coulomb per meter';
  end;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;
  TSquareCoulombPerMeterUnitId = specialize TUnitId<TSquareCoulombPerMeterUnit>;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterUnitId): TSquareCoulombsPerMeter; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

type
  { Unit of CoulombPerSquareMeter }
  TCoulombPerSquareMeterUnit = record
    const Symbol = 'C/m2';
    const Name   = 'coulomb per square meter';
  end;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;
  TCoulombPerSquareMeterUnitId = specialize TUnitId<TCoulombPerSquareMeterUnit>;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;

type
  { Unit of SquareMeterPerSquareCoulomb }
  TSquareMeterPerSquareCoulombUnit = record
    const Symbol = 'm2/C2';
    const Name   = 'square meter per square coulomb';
  end;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;
  TSquareMeterPerSquareCoulombUnitId = specialize TUnitId<TSquareMeterPerSquareCoulombUnit>;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb; inline;

type
  { Unit of NewtonPerSquareCoulomb }
  TNewtonPerSquareCoulombUnit = record
    const Symbol = 'N/C2';
    const Name   = 'newton per square coulomb';
  end;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;
  TNewtonPerSquareCoulombUnitId = specialize TUnitId<TNewtonPerSquareCoulombUnit>;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb; inline;

type
  { Unit of NewtonSquareMeterPerSquareCoulomb }
  TNewtonSquareMeterPerSquareCoulombUnit = record
    const Symbol = 'N.m2/C2';
    const Name   = 'newton square meter per square coulomb';
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
operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb; inline;

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
  { Unit of VoltMeter }
  TVoltMeterUnit = record
    const Symbol = 'V.m';
    const Name   = 'volt meter';
  end;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;
  TVoltMeterUnitId = specialize TUnitId<TVoltMeterUnit>;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltMeters; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

type
  { Unit of VoltMeterPerSecond }
  TVoltMeterPerSecondUnit = record
    const Symbol = 'V.m/s';
    const Name   = 'volt meter per second';
  end;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;
  TVoltMeterPerSecondUnitId = specialize TUnitId<TVoltMeterPerSecondUnit>;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondUnitId): TVoltMetersPerSecond; inline;

type
  { Unit of FaradPerMeter }
  TFaradPerMeterUnit = record
    const Symbol = 'F/m';
    const Name   = 'farad per meter';
  end;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;
  TFaradPerMeterUnitId = specialize TUnitId<TFaradPerMeterUnit>;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator /(const ALeft: TFarads; const {%H-}ARight: TMeterUnitId): TFaradsPerMeter; inline;

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
  { Unit of AmperePerMeter }
  TAmperePerMeterUnit = record
    const Symbol = 'A/m';
    const Name   = 'ampere per meter';
  end;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;
  TAmperePerMeterUnitId = specialize TUnitId<TAmperePerMeterUnit>;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterUnitId): TAmperesPerMeter; inline;

type
  { Unit of MeterPerAmpere }
  TMeterPerAmpereUnit = record
    const Symbol = 'm/A';
    const Name   = 'meter per ampere';
  end;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;
  TMeterPerAmpereUnitId = specialize TUnitId<TMeterPerAmpereUnit>;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereUnitId): TMetersPerAmpere; inline;

type
  { Unit of TeslaMeter }
  TTeslaMeterUnit = record
    const Symbol = 'T.m';
    const Name   = 'tesla meter';
  end;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;
  TTeslaMeterUnitId = specialize TUnitId<TTeslaMeterUnit>;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;
operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterUnitId): TTeslaMeters; inline;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereUnitId): TTeslaMeters; inline;

type
  { Unit of TeslaPerAmpere }
  TTeslaPerAmpereUnit = record
    const Symbol = 'T/A';
    const Name   = 'tesla per ampere';
  end;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;
  TTeslaPerAmpereUnitId = specialize TUnitId<TTeslaPerAmpereUnit>;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereUnitId): TTeslasPerAmpere; inline;

type
  { Unit of HenryPerMeter }
  THenryPerMeterUnit = record
    const Symbol = 'H/m';
    const Name   = 'henry per meter';
  end;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;
  THenryPerMeterUnitId = specialize TUnitId<THenryPerMeterUnit>;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters; inline;
operator /(const ALeft: THenrys; const {%H-}ARight: TMeterUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter; inline;
operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes; inline;
operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere; inline;
operator *(const ALeft: TTeslasPerAmpere; const {%H-}ARight: TMeterUnitId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas; inline;
operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter; inline;
operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes; inline;

type
  { Unit of RadianPerMeter }
  TRadianPerMeterUnit = record
    const Symbol = 'rad/m';
    const Name   = 'radian per meter';
  end;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;
  TRadianPerMeterUnitId = specialize TUnitId<TRadianPerMeterUnit>;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator /(const ALeft: TRadians; const {%H-}ARight: TMeterUnitId): TRadiansPerMeter; inline;

type
  { Unit of SquareKilogramPerSquareSecond }
  TSquareKilogramPerSquareSecondUnit = record
    const Symbol = 'kg2/s2';
    const Name   = 'square kilogram per square second';
  end;
  TSquareKilogramsPerSquareSecond = specialize TQuantity<TSquareKilogramPerSquareSecondUnit>;
  TSquareKilogramPerSquareSecondUnitId = specialize TUnitId<TSquareKilogramPerSquareSecondUnit>;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms; inline;
operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds; inline;
operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]
operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond; inline;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]
operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter; inline;
operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms; inline;

type
  { Unit of SquareSecondPerSquareMeter }
  TSquareSecondPerSquareMeterUnit = record
    const Symbol = 's2/m2';
    const Name   = 'square second per square meter';
  end;
  TSquareSecondsPerSquareMeter = specialize TQuantity<TSquareSecondPerSquareMeterUnit>;
  TSquareSecondPerSquareMeterUnitId = specialize TUnitId<TSquareSecondPerSquareMeterUnit>;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]
operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters; inline;
operator /(const ALeft: TSquareSeconds; const {%H-}ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter; inline;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]
operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double; inline;
operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double; inline;
operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond; inline;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]
operator *(const ALeft: TFaradsPerMeter; const ARight: THenrysPerMeter): TSquareSecondsPerSquareMeter; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenrysPerMeter; inline;
operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenrysPerMeter): TFaradsPerMeter; inline;

type
  { Unit of SquareJoule }
  TSquareJouleUnit = record
    const Symbol = 'J2';
    const Name   = 'square joule';
  end;
  TSquareJoules = specialize TQuantity<TSquareJouleUnit>;
  TSquareJouleUnitId = specialize TUnitId<TSquareJouleUnit>;

var
  J2: TSquareJouleUnitId;

// main definition [ J2 ] = [ J ] * [ J ]
operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules; inline;
operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules; inline;

type
  { Unit of JouleSecond }
  TJouleSecondUnit = record
    const Symbol = 'J.s';
    const Name   = 'joule second';
  end;
  TJouleSeconds = specialize TQuantity<TJouleSecondUnit>;
  TJouleSecondUnitId = specialize TUnitId<TJouleSecondUnit>;

// main definition [ J*s ] = [ J ] * [ s ]
operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ J ] / [ Hz ]
operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds; inline;
operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules; inline;
operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz; inline;
operator /(const ALeft: TJoules; const {%H-}ARight: THertzUnitId): TJouleSeconds; inline;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters; inline;
operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond; inline;

type
  { Unit of TeraelettronvoltSecond }
  TTeraelettronvoltSecondUnit = record
    const Symbol = 'TeV.s';
    const Name   = 'teraelettronvolt second';
    const Factor = 1.60217742320523E-007;
  end;
  TTeraelettronvoltSecondUnitId = specialize TFactoredUnitId<TJouleSecondUnit, TTeraelettronvoltSecondUnit>;

{ Helpers }

type
  TRadianPerSquareSecondHelper = record helper for TRadianPerSquareSecondUnitId
    class function From(const AQuantity: TSquareHertz): TRadianPerSquareSecondUnitId.TBaseQuantity; static;
  end;

type
  TSteradianPerSquareSecondHelper = record helper for TSteradianPerSquareSecondUnitId
    class function From(const AQuantity: TSquareHertz): TSteradianPerSquareSecondUnitId.TBaseQuantity; static;
  end;

type
  TNewtonSecondHelper = record helper for TNewtonSecondUnitId
    class function From(const AQuantity: TKilogramMetersPerSecond): TNewtonSecondUnitId.TBaseQuantity; static;
  end;

type
  TBequerelHelper = record helper for TBequerelUnitId
    class function From(const AQuantity: THertz): TBequerelUnitId.TBaseQuantity; static;
  end;

type
  TGrayHelper = record helper for TGrayUnitId
    class function From(const AQuantity: TSquareMetersPerSquareSecond): TGrayUnitId.TBaseQuantity; static;
  end;

type
  TSievertHelper = record helper for TSievertUnitId
    class function From(const AQuantity: TSquareMetersPerSquareSecond): TSievertUnitId.TBaseQuantity; static;
  end;

type
  TNewtonMeterHelper = record helper for TNewtonMeterUnitId
    class function From(const AQuantity: TJoules): TNewtonMeterUnitId.TBaseQuantity; static;
  end;

type
  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianUnitId
    class function From(const AQuantity: TJoulesPerRadian): TNewtonMeterPerRadianUnitId.TBaseQuantity; static;
  end;

type
  TNewtonMeterPerDegreeHelper = record helper for TNewtonMeterPerDegreeUnitId
    class function From(const AQuantity: TJoulesPerRadian): TNewtonMeterPerDegreeUnitId.TFactoredQuantity; static;
  end;

type
  TJoulePerKilogramHelper = record helper for TJoulePerKilogramUnitId
    class function From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulePerKilogramUnitId.TBaseQuantity; static;
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

implementation

uses Math;

{ TQuantity }

function TQuantity.Abs: TSelf;
begin
  result.FValue := System.Abs(FValue);
end;

function TQuantity.Value: double;
begin
  result := FValue;
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue) + ' ' + U.Symbol;
end;

function TQuantity.ToString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(FValue, ffGeneral, Precision, Digits)  + ' ' + U.Symbol;
end;

function TQuantity.ToVerboseString: string;
begin
  result := FloatToStr(FValue) + ' ' + U.Name;
end;

function TQuantity.ToVerboseString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(FValue, ffGeneral, Precision, Digits)  + ' ' + U.Name;
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

{ TFactoredUnitId }

class function TFactoredUnitId.From(const AQuantity: TBaseQuantity): TFactoredQuantity;
begin
  result.FValue := AQuantity.FValue / U.Factor;
end;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ Hz ] = [ 1 ] / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: THertz): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TSeconds): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: THertz): TSeconds;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TSecondUnitId): THertz;
begin
  result.FValue := ALeft;
end;

// main definition [ Hz2 ] = [ 1 ] / [ s2 ]
operator /(const ALeft: double; const ARight: TSquareSeconds): TSquareHertz;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareHertz): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSquareSeconds): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareHertz): TSquareSeconds;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TSquareSecondUnitId): TSquareHertz;
begin
  result.FValue := ALeft;
end;

// alternative definition [ Hz2 ] = [ Hz ] / [ s ]
operator /(const ALeft: THertz; const ARight: TSeconds): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TSquareHertz): THertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSeconds): THertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THertz; const ARight: TSquareHertz): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSecondUnitId): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ rad/s ] = [ rad ] * [ Hz ]
operator *(const ALeft: TRadians; const ARight: THertz): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TRadians): TRadiansPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TRadians): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSecond; const ARight: THertz): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator :=(const AQuantity: TRadiansPerSecond): THertz; inline;
begin
  result.FValue := AQuantity.FValue;
end;

operator :=(const AQuantity: THertz): TRadiansPerSecond; inline;
begin
  result.FValue := AQuantity.FValue;
end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondUnitId): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ rad/s2 ] = [ rad ] * [ Hz2 ]
operator *(const ALeft: TRadians; const ARight: TSquareHertz): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TRadians): TRadiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TRadians): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareHertz): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondUnitId): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ sr/s2 ] = [ sr ] * [ Hz2 ]
operator *(const ALeft: TSteradians; const ARight: TSquareHertz): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TSteradians): TSteradiansPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSteradians): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareHertz): TSteradians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSecondUnitId): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m/s ] = [ m ] * [ Hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const {%H-}ARight: THertzUnitId): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondUnitId): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m/s2 ] = [ Hz2 ] * [ m ]
operator *(const ALeft: TSquareHertz; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareHertz): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareHertz): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondUnitId): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondUnitId): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondUnitId): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: THertz): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: THertz): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterUnitId): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterUnitId): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterUnitId): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m3 ] = [ kg/m2 ] / [ m ]
operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TMeters): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerCubicMeter): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMeters): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSquareMeter; const ARight: TKilogramsPerCubicMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondUnitId): TNewtons;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N ] = [ kg/m ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerMeter; const ARight: TSquareMetersPerSquareSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N ] = [ kg*m/s ] / [ s ]
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TPascals;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TJoules;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg*m/s ] * [ m/s ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramMetersPerSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilogramMetersPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg ] * [ m2/s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramUnitId): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J ] = [ kg*m2 ] * [ Hz2 ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSquareHertz): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TKilogramSquareMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TSquareHertz): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J ] = [ kg*m2 ] / [ s2 ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareSeconds): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TJoules): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoules; const ARight: TSquareSeconds): TKilogramSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TJoules): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TWatts;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJoules; const ARight: THertz): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TJoules): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TJoules): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: THertz): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereUnitId): TCoulombs;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereUnitId): TVolts;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombUnitId): TVolts;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltUnitId): TFarads;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereUnitId): TOhms;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TOhms; const ARight: TSiemens): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSiemens; const ARight: TOhms): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSiemens): TOhms;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TOhmUnitId): TSiemens;
begin
  result.FValue := ALeft;
end;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TSecondUnitId): TWebers;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterUnitId): TTeslas;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereUnitId): THenrys;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenrys;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: THenrys): TOhms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrys; const ARight: THertz): TOhms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhms; const ARight: THenrys): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianUnitId): TLumens;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterUnitId): TLux;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoles; const {%H-}ARight: TSecondUnitId): TKatals;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TRadianUnitId): TJoulesPerRadian;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterUnitId): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterUnitId): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N/m ] = [ kg ] * [ Hz2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareHertz): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertz; const ARight: TKilograms): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareHertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareHertz): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondUnitId): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ kg/s ] = [ kg ] / [ s ]
operator /(const ALeft: TKilograms; const ARight: TSeconds): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSecondUnitId): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/s ] = [ N ] / [ m/s ]
operator /(const ALeft: TNewtons; const ARight: TMetersPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerSecond; const ARight: TMetersPerSecond): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TKilogramsPerSecond): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TPascals; const {%H-}ARight: TSecondUnitId): TPascalSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Pa*s ] = [ kg/s ] / [ m ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TMeters): TPascalSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TPascalSeconds): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TPascalSeconds; const ARight: TMeters): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TPascalSeconds): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TPascalSeconds; const {%H-}ARight: TMeterUnitId): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ Pa*s ] = [ kg/m2 ] * [ m/s ]
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TMetersPerSecond): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilogramsPerSquareMeter): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerSquareMeter): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TMetersPerSecond): TKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondUnitId): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ kg/m4 ] = [ kg ] / [ m4 ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeters): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeter): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeter; const ARight: TQuarticMeters): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeter): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m4*s ] = [ m4 ] * [ s ]
operator *(const ALeft: TQuarticMeters; const ARight: TSeconds): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TQuarticMeters): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TQuarticMeters): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSeconds; const ARight: TSeconds): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const {%H-}ARight: TSecondUnitId): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg/m4/s ] = [ kg/s ] / [ m4 ]
operator /(const ALeft: TKilogramsPerSecond; const ARight: TQuarticMeters): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeters): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerSecond; const {%H-}ARight: TQuarticMeterUnitId): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m4/s ] = [ kg/m4 ] / [ s ]
operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TSeconds): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TSeconds): TKilogramsPerQuarticMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const ARight: TKilogramsPerQuarticMeterPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramsPerQuarticMeter; const {%H-}ARight: TSecondUnitId): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg/m4/s ] = [ kg ] / [ m4*s ]
operator /(const ALeft: TKilograms; const ARight: TQuarticMeterSeconds): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticMeterSeconds; const ARight: TKilogramsPerQuarticMeterPerSecond): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TQuarticMeterSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerQuarticMeterPerSecond): TQuarticMeterSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ kg/m4/s ] = [ Pa ] / [ m3/s ]
operator /(const ALeft: TPascals; const ARight: TCubicMetersPerSecond): TKilogramsPerQuarticMeterPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TKilogramsPerQuarticMeterPerSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramsPerQuarticMeterPerSecond; const ARight: TCubicMetersPerSecond): TPascals;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerQuarticMeterPerSecond): TCubicMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m3/kg ] = [ m3 ] / [ kg ]
operator /(const ALeft: TCubicMeters; const ARight: TKilograms): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TCubicMetersPerKilogram): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerKilogram; const ARight: TKilograms): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerKilogram): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TKilogramUnitId): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg*s2 ] = [ kg ] * [ s2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareSeconds): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TKilograms): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TKilograms): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSeconds; const ARight: TSquareSeconds): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareSecondUnitId): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// main definitio [ m3/s2 ] = [ m3 ] / [ s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TSquareSeconds): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TCubicMetersPerSquareSecond): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSquareSecondUnitId): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ m3/s2 ] = [ m/s2 ] * [ m2 ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareMeters): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSquareSecond): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TSquareMeters): TMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2 ] = [ J ] * [ m ]
operator *(const ALeft: TJoules; const ARight: TMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TJoules): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TJoules): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TMeters): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2 ] = [ Pa ] * [ m4 ]
operator *(const ALeft: TPascals; const ARight: TQuarticMeters): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TPascals): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TPascals): TQuarticMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TQuarticMeters): TPascals;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramUnitId): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterUnitId): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterUnitId): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramUnitId): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3/kg ] / [ s2 ]
operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareSeconds): TCubicMetersPerKilogram;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerKilogram; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3 ] / [ kg*s2 ]
operator /(const ALeft: TCubicMeters; const ARight: TKilogramSquareSeconds): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramSquareSeconds; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilogramSquareSeconds): TCubicMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilogramSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/kg2 ] = [ m3/s2 ] / [ kg ]
operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TKilograms): TNewtonSquareMetersPerSquareKilogram;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TKilograms): TCubicMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMetersPerSquareSecond; const ARight: TNewtonSquareMetersPerSquareKilogram): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const {%H-}ARight: TKelvinUnitId): TReciprocalKelvins;
begin
  result.FValue := ALeft;
end;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinUnitId): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinUnitId): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinUnitId): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TKilogramUnitId): TJoulesPerKilogramPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinUnitId): TMeterKelvins;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterUnitId): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TMeterUnitId): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinUnitId): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TMeterUnitId): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinUnitId): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinUnitId): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinUnitId): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattsPerQuarticKelvin; const {%H-}ARight: TSquareMeterUnitId): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TMoleUnitId): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinUnitId): TMoleKelvins;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerKelvin; const {%H-}ARight: TMoleUnitId): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinUnitId): TJoulesPerMolePerKelvin;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TOhms; const {%H-}ARight: TMeterUnitId): TOhmMeters;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombUnitId): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterUnitId): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterUnitId): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterUnitId): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombUnitId): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const {%H-}ARight: TSquareMeterUnitId): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TMeterUnitId): TVoltMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondUnitId): TVoltMetersPerSecond;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TFarads; const {%H-}ARight: TMeterUnitId): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.FValue := ALeft / ARight.FValue;
end;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterUnitId): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereUnitId): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterUnitId): TTeslaMeters;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereUnitId): TTeslaMeters;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereUnitId): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrys; const {%H-}ARight: TMeterUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslasPerAmpere; const {%H-}ARight: TMeterUnitId): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TMeterUnitId): TRadiansPerMeter;
begin
  result.FValue := ALeft.FValue;
end;

// main definition [ kg2/s2 ] = [ kg2 ] / [ s2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareSeconds): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareKilogramsPerSquareSecond): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TSquareSeconds): TSquareKilograms;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareSecond): TSquareSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareSecondUnitId): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ kg2/s2 ] = [ kg/s ] * [ kg/s ]
operator *(const ALeft: TKilogramsPerSecond; const ARight: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilogramsPerSecond): TKilogramsPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// alternative definition [ kg2/s2 ] = [ kg ] * [ N/m ]
operator *(const ALeft: TKilograms; const ARight: TNewtonsPerMeter): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TKilograms): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TKilograms): TNewtonsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramsPerSquareSecond; const ARight: TNewtonsPerMeter): TKilograms;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ s2/m2 ] = [ s2 ] / [ m2 ]
operator /(const ALeft: TSquareSeconds; const ARight: TSquareMeters): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareSecondsPerSquareMeter): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMeters): TSquareSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSquareSecondsPerSquareMeter): TSquareMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSeconds; const {%H-}ARight: TSquareMeterUnitId): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ s2/m2 ] = [ 1 ] / [ m2/s2 ]
operator /(const ALeft: double; const ARight: TSquareMetersPerSquareSecond): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSecondsPerSquareMeter): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TSquareMetersPerSquareSecond): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareSecondsPerSquareMeter): TSquareMetersPerSquareSecond;
begin
  result.FValue := ALeft / ARight.FValue;
end;

// alternative definition [ s2/m2 ] = [ F/m ] * [ H/m ]
operator *(const ALeft: TFaradsPerMeter; const ARight: THenrysPerMeter): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TFaradsPerMeter): TSquareSecondsPerSquareMeter;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: TFaradsPerMeter): THenrysPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSecondsPerSquareMeter; const ARight: THenrysPerMeter): TFaradsPerMeter;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ J2 ] = [ J ] * [ J ]
operator *(const ALeft: TJoules; const ARight: TJoules): TSquareJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareJoules; const ARight: TJoules): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

// main definition [ J*s ] = [ J ] * [ s ]
operator *(const ALeft: TJoules; const ARight: TSeconds): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSeconds; const ARight: TJoules): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TJoules): TSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TSeconds): TJoules;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TJoules; const {%H-}ARight: TSecondUnitId): TJouleSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J*s ] = [ J ] / [ Hz ]
operator /(const ALeft: TJoules; const ARight: THertz): TJouleSeconds;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertz; const ARight: TJouleSeconds): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJouleSeconds; const ARight: THertz): TJoules;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoules; const ARight: TJouleSeconds): THertz;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: THertzUnitId): TJouleSeconds;
begin
  result.FValue := ALeft.FValue;
end;

// alternative definition [ J*s ] = [ kg*m/s ] * [ m ]
operator *(const ALeft: TKilogramMetersPerSecond; const ARight: TMeters): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramMetersPerSecond): TJouleSeconds;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TKilogramMetersPerSecond): TMeters;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleSeconds; const ARight: TMeters): TKilogramMetersPerSecond;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ Helpers }

class function TRadianPerSquareSecondHelper.From(const AQuantity: TSquareHertz): TRadianPerSquareSecondUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TSteradianPerSquareSecondHelper.From(const AQuantity: TSquareHertz): TSteradianPerSquareSecondUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TNewtonSecondHelper.From(const AQuantity: TKilogramMetersPerSecond): TNewtonSecondUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TBequerelHelper.From(const AQuantity: THertz): TBequerelUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TGrayHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TGrayUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TSievertHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TSievertUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TNewtonMeterHelper.From(const AQuantity: TJoules): TNewtonMeterUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TNewtonMeterPerRadianHelper.From(const AQuantity: TJoulesPerRadian): TNewtonMeterPerRadianUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TNewtonMeterPerDegreeHelper.From(const AQuantity: TJoulesPerRadian): TNewtonMeterPerDegreeUnitId.TFactoredQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

class function TJoulePerKilogramHelper.From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulePerKilogramUnitId.TBaseQuantity; static;
begin
  result.FValue := AQuantity.FValue;
end;

{ Power quantities }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TMeters): TCubicMeters;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TMeters): TQuarticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function QuinticPower(AQuantity: TMeters): TQuinticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 5);
end;

function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/5);
end;

function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SexticPower(AQuantity: TMeters): TSexticMeters;
begin
  result.FValue := Power(AQuantity.FValue, 6);
end;

function SexticRoot(AQuantity: TSexticMeters): TMeters;
begin
  result.FValue := Power(AQuantity.FValue, 1/6);
end;

function SquarePower(AQuantity: TAmperes): TSquareAmperes;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKelvins): TSquareKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function SquarePower(AQuantity: TRadians): TSteradians;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSteradians): TRadians;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: THertz): TSquareHertz;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareHertz): THertz;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TVolts): TSquareVolts;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareVolts): TVolts;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKilogramsPerSecond): TSquareKilogramsPerSquareSecond;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKilogramsPerSquareSecond): TKilogramsPerSecond;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TJoules): TSquareJoules;
begin
  result.FValue := Power(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareJoules): TJoules;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

{ Trigonometric functions }

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

end.
