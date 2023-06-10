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
    Value: double;
  public
    function Abs: TSelf;
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
  TSecondId = specialize TUnitId<TSecondUnit>;

var
  s: TSecondId;

type
  { Unit of Day }
  TDayUnit = record
    const Symbol = 'day';
    const Name   = 'day';
    const Factor = 86400;
  end;
  TDayId = specialize TFactoredUnitId<TSecondUnit, TDayUnit>;

const
  day: specialize TQuantity<TSecondUnit> = (Value: TDayUnit.Factor);

type
  { Unit of Hour }
  THourUnit = record
    const Symbol = 'h';
    const Name   = 'hour';
    const Factor = 3600;
  end;
  THourId = specialize TFactoredUnitId<TSecondUnit, THourUnit>;

const
  hour: specialize TQuantity<TSecondUnit> = (Value: THourUnit.Factor);

type
  { Unit of Minute }
  TMinuteUnit = record
    const Symbol = 'min';
    const Name   = 'minute';
    const Factor = 60;
  end;
  TMinuteId = specialize TFactoredUnitId<TSecondUnit, TMinuteUnit>;

const
  minute: specialize TQuantity<TSecondUnit> = (Value: TMinuteUnit.Factor);

type
  { Unit of Decisecond }
  TDecisecondUnit = record
    const Symbol = 'ds';
    const Name   = 'decisecond';
    const Factor = 1E-01;
  end;
  TDecisecondId = specialize TFactoredUnitId<TSecondUnit, TDecisecondUnit>;

const
  ds: specialize TQuantity<TSecondUnit> = (Value: TDecisecondUnit.Factor);

type
  { Unit of Centisecond }
  TCentisecondUnit = record
    const Symbol = 'cs';
    const Name   = 'centisecond';
    const Factor = 1E-02;
  end;
  TCentisecondId = specialize TFactoredUnitId<TSecondUnit, TCentisecondUnit>;

const
  cs: specialize TQuantity<TSecondUnit> = (Value: TCentisecondUnit.Factor);

type
  { Unit of Millisecond }
  TMillisecondUnit = record
    const Symbol = 'ms';
    const Name   = 'millisecond';
    const Factor = 1E-03;
  end;
  TMillisecondId = specialize TFactoredUnitId<TSecondUnit, TMillisecondUnit>;

const
  ms: specialize TQuantity<TSecondUnit> = (Value: TMillisecondUnit.Factor);

type
  { Unit of Microsecond }
  TMicrosecondUnit = record
    const Symbol = 'us';
    const Name   = 'microsecond';
    const Factor = 1E-06;
  end;
  TMicrosecondId = specialize TFactoredUnitId<TSecondUnit, TMicrosecondUnit>;

const
  us: specialize TQuantity<TSecondUnit> = (Value: TMicrosecondUnit.Factor);

type
  { Unit of Nanosecond }
  TNanosecondUnit = record
    const Symbol = 'ns';
    const Name   = 'nanosecond';
    const Factor = 1E-09;
  end;
  TNanosecondId = specialize TFactoredUnitId<TSecondUnit, TNanosecondUnit>;

const
  ns: specialize TQuantity<TSecondUnit> = (Value: TNanosecondUnit.Factor);

type
  { Unit of Picosecond }
  TPicosecondUnit = record
    const Symbol = 'ps';
    const Name   = 'picosecond';
    const Factor = 1E-12;
  end;
  TPicosecondId = specialize TFactoredUnitId<TSecondUnit, TPicosecondUnit>;

const
  ps: specialize TQuantity<TSecondUnit> = (Value: TPicosecondUnit.Factor);

type
  { Unit of SquareSecond }
  TSquareSecondUnit = record
    const Symbol = 's2';
    const Name   = 'square second';
  end;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;
  TSquareSecondId = specialize TUnitId<TSquareSecondUnit>;

var
  s2: TSquareSecondId;

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
  TMeterId = specialize TUnitId<TMeterUnit>;

var
  m: TMeterId;

type
  { Unit of Kilometer }
  TKilometerUnit = record
    const Symbol = 'km';
    const Name   = 'kilometer';
    const Factor = 1E+03;
  end;
  TKilometerId = specialize TFactoredUnitId<TMeterUnit, TKilometerUnit>;

const
  km: specialize TQuantity<TMeterUnit> = (Value: TKilometerUnit.Factor);

type
  { Unit of Hectometer }
  THectometerUnit = record
    const Symbol = 'hm';
    const Name   = 'hectometer';
    const Factor = 1E+02;
  end;
  THectometerId = specialize TFactoredUnitId<TMeterUnit, THectometerUnit>;

const
  hm: specialize TQuantity<TMeterUnit> = (Value: THectometerUnit.Factor);

type
  { Unit of Decameter }
  TDecameterUnit = record
    const Symbol = 'dam';
    const Name   = 'decameter';
    const Factor = 1E+01;
  end;
  TDecameterId = specialize TFactoredUnitId<TMeterUnit, TDecameterUnit>;

const
  dam: specialize TQuantity<TMeterUnit> = (Value: TDecameterUnit.Factor);

type
  { Unit of Decimeter }
  TDecimeterUnit = record
    const Symbol = 'dm';
    const Name   = 'decimeter';
    const Factor = 1E-01;
  end;
  TDecimeterId = specialize TFactoredUnitId<TMeterUnit, TDecimeterUnit>;

const
  dm: specialize TQuantity<TMeterUnit> = (Value: TDecimeterUnit.Factor);

type
  { Unit of Centimeter }
  TCentimeterUnit = record
    const Symbol = 'cm';
    const Name   = 'centimeter';
    const Factor = 1E-02;
  end;
  TCentimeterId = specialize TFactoredUnitId<TMeterUnit, TCentimeterUnit>;

const
  cm: specialize TQuantity<TMeterUnit> = (Value: TCentimeterUnit.Factor);

type
  { Unit of Millimeter }
  TMillimeterUnit = record
    const Symbol = 'mm';
    const Name   = 'millimeter';
    const Factor = 1E-03;
  end;
  TMillimeterId = specialize TFactoredUnitId<TMeterUnit, TMillimeterUnit>;

const
  mm: specialize TQuantity<TMeterUnit> = (Value: TMillimeterUnit.Factor);

type
  { Unit of Micrometer }
  TMicrometerUnit = record
    const Symbol = 'um';
    const Name   = 'micrometer';
    const Factor = 1E-06;
  end;
  TMicrometerId = specialize TFactoredUnitId<TMeterUnit, TMicrometerUnit>;

const
  um: specialize TQuantity<TMeterUnit> = (Value: TMicrometerUnit.Factor);

type
  { Unit of Nanometer }
  TNanometerUnit = record
    const Symbol = 'nm';
    const Name   = 'nanometer';
    const Factor = 1E-09;
  end;
  TNanometerId = specialize TFactoredUnitId<TMeterUnit, TNanometerUnit>;

const
  nm: specialize TQuantity<TMeterUnit> = (Value: TNanometerUnit.Factor);

type
  { Unit of Picometer }
  TPicometerUnit = record
    const Symbol = 'pm';
    const Name   = 'picometer';
    const Factor = 1E-12;
  end;
  TPicometerId = specialize TFactoredUnitId<TMeterUnit, TPicometerUnit>;

const
  pm: specialize TQuantity<TMeterUnit> = (Value: TPicometerUnit.Factor);

type
  { Unit of SquareMeter }
  TSquareMeterUnit = record
    const Symbol = 'm2';
    const Name   = 'square meter';
  end;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;
  TSquareMeterId = specialize TUnitId<TSquareMeterUnit>;

var
  m2: TSquareMeterId;

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
  TSquareKilometerId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareKilometerUnit>;

const
  km2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareKilometerUnit.Factor);

type
  { Unit of SquareHectometer }
  TSquareHectometerUnit = record
    const Symbol = 'hm2';
    const Name   = 'square hectometer';
    const Factor = 1E+04;
  end;
  TSquareHectometerId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareHectometerUnit>;

const
  hm2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareHectometerUnit.Factor);

type
  { Unit of SquareDecameter }
  TSquareDecameterUnit = record
    const Symbol = 'dam2';
    const Name   = 'square decameter';
    const Factor = 1E+02;
  end;
  TSquareDecameterId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareDecameterUnit>;

const
  dam2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareDecameterUnit.Factor);

type
  { Unit of SquareDecimeter }
  TSquareDecimeterUnit = record
    const Symbol = 'dm2';
    const Name   = 'square decimeter';
    const Factor = 1E-02;
  end;
  TSquareDecimeterId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareDecimeterUnit>;

const
  dm2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareDecimeterUnit.Factor);

type
  { Unit of SquareCentimeter }
  TSquareCentimeterUnit = record
    const Symbol = 'cm2';
    const Name   = 'square centimeter';
    const Factor = 1E-04;
  end;
  TSquareCentimeterId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareCentimeterUnit>;

const
  cm2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareCentimeterUnit.Factor);

type
  { Unit of SquareMillimeter }
  TSquareMillimeterUnit = record
    const Symbol = 'mm2';
    const Name   = 'square millimeter';
    const Factor = 1E-06;
  end;
  TSquareMillimeterId = specialize TFactoredUnitId<TSquareMeterUnit, TSquareMillimeterUnit>;

const
  mm2: specialize TQuantity<TSquareMeterUnit> = (Value: TSquareMillimeterUnit.Factor);

type
  { Unit of CubicMeter }
  TCubicMeterUnit = record
    const Symbol = 'm3';
    const Name   = 'cubic meter';
  end;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;
  TCubicMeterId = specialize TUnitId<TCubicMeterUnit>;

var
  m3: TCubicMeterId;

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

type
  { Unit of CubicKilometer }
  TCubicKilometerUnit = record
    const Symbol = 'km3';
    const Name   = 'cubic kilometer';
    const Factor = 1E+09;
  end;
  TCubicKilometerId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicKilometerUnit>;

const
  km3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicKilometerUnit.Factor);

type
  { Unit of CubicHectometer }
  TCubicHectometerUnit = record
    const Symbol = 'hm3';
    const Name   = 'cubic hectometer';
    const Factor = 1E+06;
  end;
  TCubicHectometerId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicHectometerUnit>;

const
  hm3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicHectometerUnit.Factor);

type
  { Unit of CubicDecameter }
  TCubicDecameterUnit = record
    const Symbol = 'dam3';
    const Name   = 'cubic decameter';
    const Factor = 1E+03;
  end;
  TCubicDecameterId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicDecameterUnit>;

const
  dam3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicDecameterUnit.Factor);

type
  { Unit of CubicDecimeter }
  TCubicDecimeterUnit = record
    const Symbol = 'dm3';
    const Name   = 'cubic decimeter';
    const Factor = 1E-03;
  end;
  TCubicDecimeterId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicDecimeterUnit>;

const
  dm3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicDecimeterUnit.Factor);

type
  { Unit of CubicCentimeter }
  TCubicCentimeterUnit = record
    const Symbol = 'cm3';
    const Name   = 'cubic centimeter';
    const Factor = 1E-06;
  end;
  TCubicCentimeterId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicCentimeterUnit>;

const
  cm3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicCentimeterUnit.Factor);

type
  { Unit of CubicMillimeter }
  TCubicMillimeterUnit = record
    const Symbol = 'mm3';
    const Name   = 'cubic millimeter';
    const Factor = 1E-09;
  end;
  TCubicMillimeterId = specialize TFactoredUnitId<TCubicMeterUnit, TCubicMillimeterUnit>;

const
  mm3: specialize TQuantity<TCubicMeterUnit> = (Value: TCubicMillimeterUnit.Factor);

type
  { Unit of QuarticMeter }
  TQuarticMeterUnit = record
    const Symbol = 'm4';
    const Name   = 'quartic meter';
  end;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;
  TQuarticMeterId = specialize TUnitId<TQuarticMeterUnit>;

var
  m4: TQuarticMeterId;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
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
  TQuarticKilometerId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticKilometerUnit>;

const
  km4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticKilometerUnit.Factor);

type
  { Unit of QuarticHectometer }
  TQuarticHectometerUnit = record
    const Symbol = 'hm4';
    const Name   = 'quartic hectometer';
    const Factor = 1E+08;
  end;
  TQuarticHectometerId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticHectometerUnit>;

const
  hm4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticHectometerUnit.Factor);

type
  { Unit of QuarticDecameter }
  TQuarticDecameterUnit = record
    const Symbol = 'dam4';
    const Name   = 'quartic decameter';
    const Factor = 1E+04;
  end;
  TQuarticDecameterId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticDecameterUnit>;

const
  dam4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticDecameterUnit.Factor);

type
  { Unit of QuarticDecimeter }
  TQuarticDecimeterUnit = record
    const Symbol = 'dm4';
    const Name   = 'quartic decimeter';
    const Factor = 1E-04;
  end;
  TQuarticDecimeterId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticDecimeterUnit>;

const
  dm4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticDecimeterUnit.Factor);

type
  { Unit of QuarticCentimeter }
  TQuarticCentimeterUnit = record
    const Symbol = 'cm4';
    const Name   = 'quartic centimeter';
    const Factor = 1E-08;
  end;
  TQuarticCentimeterId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticCentimeterUnit>;

const
  cm4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticCentimeterUnit.Factor);

type
  { Unit of QuarticMillimeter }
  TQuarticMillimeterUnit = record
    const Symbol = 'mm4';
    const Name   = 'quartic millimeter';
    const Factor = 1E-12;
  end;
  TQuarticMillimeterId = specialize TFactoredUnitId<TQuarticMeterUnit, TQuarticMillimeterUnit>;

const
  mm4: specialize TQuantity<TQuarticMeterUnit> = (Value: TQuarticMillimeterUnit.Factor);

type
  { Unit of QuinticMeter }
  TQuinticMeterUnit = record
    const Symbol = 'm5';
    const Name   = 'quintic meter';
  end;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;
  TQuinticMeterId = specialize TUnitId<TQuinticMeterUnit>;

var
  m5: TQuinticMeterId;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters; inline;

type
  { Unit of SexticMeter }
  TSexticMeterUnit = record
    const Symbol = 'm6';
    const Name   = 'sextic meter';
  end;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;
  TSexticMeterId = specialize TUnitId<TSexticMeterUnit>;

var
  m6: TSexticMeterId;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters; inline;
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
  TKilogramId = specialize TUnitId<TKilogramUnit>;

var
  kg: TKilogramId;

type
  { Unit of Hectogram }
  THectogramUnit = record
    const Symbol = 'hg';
    const Name   = 'hectogram';
    const Factor = 1E-01;
  end;
  THectogramId = specialize TFactoredUnitId<TKilogramUnit, THectogramUnit>;

const
  hg: specialize TQuantity<TKilogramUnit> = (Value: THectogramUnit.Factor);

type
  { Unit of Decagram }
  TDecagramUnit = record
    const Symbol = 'dag';
    const Name   = 'decagram';
    const Factor = 1E-02;
  end;
  TDecagramId = specialize TFactoredUnitId<TKilogramUnit, TDecagramUnit>;

const
  dag: specialize TQuantity<TKilogramUnit> = (Value: TDecagramUnit.Factor);

type
  { Unit of Gram }
  TGramUnit = record
    const Symbol = 'g';
    const Name   = 'gram';
    const Factor = 1E-03;
  end;
  TGramId = specialize TFactoredUnitId<TKilogramUnit, TGramUnit>;

const
  g: specialize TQuantity<TKilogramUnit> = (Value: TGramUnit.Factor);

type
  { Unit of Decigram }
  TDecigramUnit = record
    const Symbol = 'dg';
    const Name   = 'decigram';
    const Factor = 1E-04;
  end;
  TDecigramId = specialize TFactoredUnitId<TKilogramUnit, TDecigramUnit>;

const
  dg: specialize TQuantity<TKilogramUnit> = (Value: TDecigramUnit.Factor);

type
  { Unit of Centigram }
  TCentigramUnit = record
    const Symbol = 'cg';
    const Name   = 'centigram';
    const Factor = 1E-05;
  end;
  TCentigramId = specialize TFactoredUnitId<TKilogramUnit, TCentigramUnit>;

const
  cg: specialize TQuantity<TKilogramUnit> = (Value: TCentigramUnit.Factor);

type
  { Unit of Milligram }
  TMilligramUnit = record
    const Symbol = 'mg';
    const Name   = 'milligram';
    const Factor = 1E-06;
  end;
  TMilligramId = specialize TFactoredUnitId<TKilogramUnit, TMilligramUnit>;

const
  mg: specialize TQuantity<TKilogramUnit> = (Value: TMilligramUnit.Factor);

type
  { Unit of Microgram }
  TMicrogramUnit = record
    const Symbol = 'ug';
    const Name   = 'microgram';
    const Factor = 1E-09;
  end;
  TMicrogramId = specialize TFactoredUnitId<TKilogramUnit, TMicrogramUnit>;

const
  ug: specialize TQuantity<TKilogramUnit> = (Value: TMicrogramUnit.Factor);

type
  { Unit of Nanogram }
  TNanogramUnit = record
    const Symbol = 'ng';
    const Name   = 'nanogram';
    const Factor = 1E-12;
  end;
  TNanogramId = specialize TFactoredUnitId<TKilogramUnit, TNanogramUnit>;

const
  ng: specialize TQuantity<TKilogramUnit> = (Value: TNanogramUnit.Factor);

type
  { Unit of Picogram }
  TPicogramUnit = record
    const Symbol = 'pg';
    const Name   = 'picogram';
    const Factor = 1E-15;
  end;
  TPicogramId = specialize TFactoredUnitId<TKilogramUnit, TPicogramUnit>;

const
  pg: specialize TQuantity<TKilogramUnit> = (Value: TPicogramUnit.Factor);

type
  { Unit of SquareKilogram }
  TSquareKilogramUnit = record
    const Symbol = 'kg2';
    const Name   = 'square kilogram';
  end;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;
  TSquareKilogramId = specialize TUnitId<TSquareKilogramUnit>;

var
  kg2: TSquareKilogramId;

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
  TAmpereId = specialize TUnitId<TAmpereUnit>;

var
  A: TAmpereId;

type
  { Unit of Kiloampere }
  TKiloampereUnit = record
    const Symbol = 'kA';
    const Name   = 'kiloampere';
    const Factor = 1E+03;
  end;
  TKiloampereId = specialize TFactoredUnitId<TAmpereUnit, TKiloampereUnit>;

const
  kA: specialize TQuantity<TAmpereUnit> = (Value: TKiloampereUnit.Factor);

type
  { Unit of Hectoampere }
  THectoampereUnit = record
    const Symbol = 'hA';
    const Name   = 'hectoampere';
    const Factor = 1E+02;
  end;
  THectoampereId = specialize TFactoredUnitId<TAmpereUnit, THectoampereUnit>;

const
  hA: specialize TQuantity<TAmpereUnit> = (Value: THectoampereUnit.Factor);

type
  { Unit of Decampere }
  TDecampereUnit = record
    const Symbol = 'daA';
    const Name   = 'decampere';
    const Factor = 1E+01;
  end;
  TDecampereId = specialize TFactoredUnitId<TAmpereUnit, TDecampereUnit>;

const
  daA: specialize TQuantity<TAmpereUnit> = (Value: TDecampereUnit.Factor);

type
  { Unit of Deciampere }
  TDeciampereUnit = record
    const Symbol = 'dA';
    const Name   = 'deciampere';
    const Factor = 1E-01;
  end;
  TDeciampereId = specialize TFactoredUnitId<TAmpereUnit, TDeciampereUnit>;

const
  dA: specialize TQuantity<TAmpereUnit> = (Value: TDeciampereUnit.Factor);

type
  { Unit of Centiampere }
  TCentiampereUnit = record
    const Symbol = 'cA';
    const Name   = 'centiampere';
    const Factor = 1E-02;
  end;
  TCentiampereId = specialize TFactoredUnitId<TAmpereUnit, TCentiampereUnit>;

const
  cA: specialize TQuantity<TAmpereUnit> = (Value: TCentiampereUnit.Factor);

type
  { Unit of Milliampere }
  TMilliampereUnit = record
    const Symbol = 'mA';
    const Name   = 'milliampere';
    const Factor = 1E-03;
  end;
  TMilliampereId = specialize TFactoredUnitId<TAmpereUnit, TMilliampereUnit>;

const
  mA: specialize TQuantity<TAmpereUnit> = (Value: TMilliampereUnit.Factor);

type
  { Unit of Microampere }
  TMicroampereUnit = record
    const Symbol = 'uA';
    const Name   = 'microampere';
    const Factor = 1E-06;
  end;
  TMicroampereId = specialize TFactoredUnitId<TAmpereUnit, TMicroampereUnit>;

const
  uA: specialize TQuantity<TAmpereUnit> = (Value: TMicroampereUnit.Factor);

type
  { Unit of Nanoampere }
  TNanoampereUnit = record
    const Symbol = 'nA';
    const Name   = 'nanoampere';
    const Factor = 1E-09;
  end;
  TNanoampereId = specialize TFactoredUnitId<TAmpereUnit, TNanoampereUnit>;

const
  nA: specialize TQuantity<TAmpereUnit> = (Value: TNanoampereUnit.Factor);

type
  { Unit of Picoampere }
  TPicoampereUnit = record
    const Symbol = 'pA';
    const Name   = 'picoampere';
    const Factor = 1E-12;
  end;
  TPicoampereId = specialize TFactoredUnitId<TAmpereUnit, TPicoampereUnit>;

const
  picoampere: specialize TQuantity<TAmpereUnit> = (Value: TPicoampereUnit.Factor);

type
  { Unit of SquareAmpere }
  TSquareAmpereUnit = record
    const Symbol = 'A2';
    const Name   = 'square ampere';
  end;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;
  TSquareAmpereId = specialize TUnitId<TSquareAmpereUnit>;

var
  A2: TSquareAmpereId;

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
  TSquareMilliampereId = specialize TFactoredUnitId<TSquareAmpereUnit, TSquareMilliampereUnit>;

const
  mA2: specialize TQuantity<TSquareAmpereUnit> = (Value: TSquareMilliampereUnit.Factor);

type
  { Unit of Kelvin }
  TKelvinUnit = record
    const Symbol = 'K';
    const Name   = 'kelvin';
  end;
  TKelvins = specialize TQuantity<TKelvinUnit>;
  TKelvinId = specialize TUnitId<TKelvinUnit>;

var
  K: TKelvinId;

type
  { Unit of SquareKelvin }
  TSquareKelvinUnit = record
    const Symbol = 'K2';
    const Name   = 'square kelvin';
  end;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;
  TSquareKelvinId = specialize TUnitId<TSquareKelvinUnit>;

var
  K2: TSquareKelvinId;

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
  TCubicKelvinId = specialize TUnitId<TCubicKelvinUnit>;

var
  K3: TCubicKelvinId;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

type
  { Unit of QuarticKelvin }
  TQuarticKelvinUnit = record
    const Symbol = 'K4';
    const Name   = 'quartic kelvin';
  end;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;
  TQuarticKelvinId = specialize TUnitId<TQuarticKelvinUnit>;

var
  K4: TQuarticKelvinId;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

//
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

type
  { Unit of Mole }
  TMoleUnit = record
    const Symbol = 'mol';
    const Name   = 'mole';
  end;
  TMoles = specialize TQuantity<TMoleUnit>;
  TMoleId = specialize TUnitId<TMoleUnit>;

var
  mol: TMoleId;

type
  { Unit of Candela }
  TCandelaUnit = record
    const Symbol = 'cd';
    const Name   = 'candela';
  end;
  TCandelas = specialize TQuantity<TCandelaUnit>;
  TCandelaId = specialize TUnitId<TCandelaUnit>;

var
  cd: TCandelaId;

type
  { Unit of Radian }
  TRadianUnit = record
    const Symbol = 'rad';
    const Name   = 'radian';
  end;
  TRadians = specialize TQuantity<TRadianUnit>;
  TRadianId = specialize TUnitId<TRadianUnit>;

var
  rad: TRadianId;

type
  { Unit of Degree }
  TDegreeUnit = record
    const Symbol = 'deg';
    const Name   = 'degree';
    const Factor = Pi/180;
  end;
  TDegreeId = specialize TFactoredUnitId<TRadianUnit, TDegreeUnit>;

const
  deg: specialize TQuantity<TRadianUnit> = (Value: TDegreeUnit.Factor);

type
  { Unit of Steradian }
  TSteradianUnit = record
    const Symbol = 'sr';
    const Name   = 'steradian';
  end;
  TSteradians = specialize TQuantity<TSteradianUnit>;
  TSteradianId = specialize TUnitId<TSteradianUnit>;

var
  sr: TSteradianId;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

type
  { Unit of MeterPerSecond }
  TMeterPerSecondUnit = record
    const Symbol = 'm/s';
    const Name   = 'meter per second';
  end;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerSecondId = specialize TUnitId<TMeterPerSecondUnit>;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;

operator /(const ALeft: TMeters; const {%H-}ARight: TSecondId): TMetersPerSecond; inline;

type
  { Unit of KilometerPerHour }
  TKilometerPerHourUnit = record
    const Symbol = 'km/h';
    const Name   = 'kilometer per hour';
    const Factor = 5/18;
  end;
  TKilometerPerHourId = specialize TFactoredUnitId<TMeterPerSecondUnit, TKilometerPerHourUnit>;

type
  { Unit of DecimeterPerSecond }
  TDecimeterPerSecondUnit = record
    const Symbol = 'dm/s';
    const Name   = 'decimeter per second';
    const Factor = 1E-01;
  end;
  TDecimeterPerSecondId = specialize TFactoredUnitId<TMeterPerSecondUnit, TDecimeterPerSecondUnit>;

type
  { Unit of CentimeterPerSecond }
  TCentimeterPerSecondUnit = record
    const Symbol = 'cm/s';
    const Name   = 'centimeter per second';
    const Factor = 1E-02;
  end;
  TCentimeterPerSecondId = specialize TFactoredUnitId<TMeterPerSecondUnit, TCentimeterPerSecondUnit>;

type
  { Unit of MillimeterPerSecond }
  TMillimeterPerSecondUnit = record
    const Symbol = 'mm/s';
    const Name   = 'millimeter per second';
    const Factor = 1E-03;
  end;
  TMillimeterPerSecondId = specialize TFactoredUnitId<TMeterPerSecondUnit, TMillimeterPerSecondUnit>;

type
  { Unit of MeterPerSquareSecond }
  TMeterPerSquareSecondUnit = record
    const Symbol = 'm/s2';
    const Name   = 'meter per square second';
  end;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerSquareSecondId = specialize TUnitId<TMeterPerSquareSecondUnit>;

// main definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters; inline;

operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondId): TMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond; inline;

type
  { Unit of KilometerPerHourPerSecond }
  TKilometerPerHourPerSecondUnit = record
    const Symbol = 'km/h/s';
    const Name   = 'kilometer-hour per second';
    const Factor = 5/18;
  end;
  TKilometerPerHourPerSecondId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TKilometerPerHourPerSecondUnit>;

type
  { Unit of DecimeterPerSquareSecond }
  TDecimeterPerSquareSecondUnit = record
    const Symbol = 'dm/s2';
    const Name   = 'decimeter per square second';
    const Factor = 1E-01;
  end;
  TDecimeterPerSquareSecondId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TDecimeterPerSquareSecondUnit>;

type
  { Unit of CentimeterPerSquareSecond }
  TCentimeterPerSquareSecondUnit = record
    const Symbol = 'cm/s2';
    const Name   = 'centimeter per square second';
    const Factor = 1E-02;
  end;
  TCentimeterPerSquareSecondId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TCentimeterPerSquareSecondUnit>;

type
  { Unit of MillimeterPerSquareSecond }
  TMillimeterPerSquareSecondUnit = record
    const Symbol = 'mm/s2';
    const Name   = 'millimeter per square second';
    const Factor = 1E-03;
  end;
  TMillimeterPerSquareSecondId = specialize TFactoredUnitId<TMeterPerSquareSecondUnit, TMillimeterPerSquareSecondUnit>;

type
  { Unit of RadianPerSecond }
  TRadianPerSecondUnit = record
    const Symbol = 'rad/s';
    const Name   = 'radian per second';
  end;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;
  TRadianPerSecondId = specialize TUnitId<TRadianPerSecondUnit>;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;

operator /(const ALeft: TRadians; const {%H-}ARight: TSecondId): TRadiansPerSecond; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): TRadiansPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TRadiansPerSecond): TMeters; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerSecond): TMetersPerSecond; inline;

type
  { Unit of RadianPerSquareSecond }
  TRadianPerSquareSecondUnit = record
    const Symbol = 'rad/s2';
    const Name   = 'radian per square second';
  end;
  TRadiansPerSquareSecond = specialize TQuantity<TRadianPerSquareSecondUnit>;
  TRadianPerSquareSecondId = specialize TUnitId<TRadianPerSquareSecondUnit>;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians; inline;

operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondId): TRadiansPerSquareSecond; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSquareSecond): TRadiansPerSecond; inline;

type
  { Unit of Hertz }
  THertzUnit = record
    const Symbol = 'Hz';
    const Name   = 'hertz';
  end;
  THertz = specialize TQuantity<THertzUnit>;
  THertzId = specialize TUnitId<THertzUnit>;

var
  Hz: THertzId;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;

operator /(const ALeft: double; const {%H-}ARight: TSecondId): THertz; inline;

type
  { Unit of Gigahertz }
  TGigahertzUnit = record
    const Symbol = 'GHz';
    const Name   = 'gigahertz';
    const Factor = 1E+09;
  end;
  TGigahertzId = specialize TFactoredUnitId<THertzUnit, TGigahertzUnit>;

const
  GHz: specialize TQuantity<THertzUnit> = (Value: TGigahertzUnit.Factor);

type
  { Unit of Megahertz }
  TMegahertzUnit = record
    const Symbol = 'MHz';
    const Name   = 'megahertz';
    const Factor = 1E+06;
  end;
  TMegahertzId = specialize TFactoredUnitId<THertzUnit, TMegahertzUnit>;

const
  MHz: specialize TQuantity<THertzUnit> = (Value: TMegahertzUnit.Factor);

type
  { Unit of Kilohertz }
  TKilohertzUnit = record
    const Symbol = 'kHz';
    const Name   = 'kilohertz';
    const Factor = 1E+03;
  end;
  TKilohertzId = specialize TFactoredUnitId<THertzUnit, TKilohertzUnit>;

const
  kHz: specialize TQuantity<THertzUnit> = (Value: TKilohertzUnit.Factor);

type
  { Unit of SquareHertz }
  TSquareHertzUnit = record
    const Symbol = 'Hz2';
    const Name   = 'square hertz';
  end;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;
  TSquareHertzId = specialize TUnitId<TSquareHertzUnit>;

var
  Hz2: TSquareHertzId;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

type
  { Unit of Newton }
  TNewtonUnit = record
    const Symbol = 'N';
    const Name   = 'newton';
  end;
  TNewtons = specialize TQuantity<TNewtonUnit>;
  TNewtonId = specialize TUnitId<TNewtonUnit>;

var
  N: TNewtonId;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondId): TNewtons; inline;

type
  { Unit of Giganewton }
  TGiganewtonUnit = record
    const Symbol = 'GN';
    const Name   = 'giganewton';
    const Factor = 1E+09;
  end;
  TGiganewtonId = specialize TFactoredUnitId<TNewtonUnit, TGiganewtonUnit>;

const
  GN: specialize TQuantity<TNewtonUnit> = (Value: TGiganewtonUnit.Factor);

type
  { Unit of Meganewton }
  TMeganewtonUnit = record
    const Symbol = 'MN';
    const Name   = 'meganewton';
    const Factor = 1E+06;
  end;
  TMeganewtonId = specialize TFactoredUnitId<TNewtonUnit, TMeganewtonUnit>;

const
  MN: specialize TQuantity<TNewtonUnit> = (Value: TMeganewtonUnit.Factor);

type
  { Unit of Kilonewton }
  TKilonewtonUnit = record
    const Symbol = 'kN';
    const Name   = 'kilonewton';
    const Factor = 1E+03;
  end;
  TKilonewtonId = specialize TFactoredUnitId<TNewtonUnit, TKilonewtonUnit>;

const
  kN: specialize TQuantity<TNewtonUnit> = (Value: TKilonewtonUnit.Factor);

type
  { Unit of Pascal }
  TPascalUnit = record
    const Symbol = 'Pa';
    const Name   = 'pascal';
  end;
  TPascals = specialize TQuantity<TPascalUnit>;
  TPascalId = specialize TUnitId<TPascalUnit>;

var
  Pa: TPascalId;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterId): TPascals; inline;

type
  { Unit of Gigapascal }
  TGigapascalUnit = record
    const Symbol = 'GPa';
    const Name   = 'gigapascal';
    const Factor = 1E+09;
  end;
  TGigapascalId = specialize TFactoredUnitId<TPascalUnit, TGigapascalUnit>;

const
  GPa: specialize TQuantity<TPascalUnit> = (Value: TGigapascalUnit.Factor);

type
  { Unit of Megapascal }
  TMegapascalUnit = record
    const Symbol = 'MPa';
    const Name   = 'megapascal';
    const Factor = 1E+06;
  end;
  TMegapascalId = specialize TFactoredUnitId<TPascalUnit, TMegapascalUnit>;

const
  MPa: specialize TQuantity<TPascalUnit> = (Value: TMegapascalUnit.Factor);

type
  { Unit of Kilopascal }
  TKilopascalUnit = record
    const Symbol = 'kPa';
    const Name   = 'kilopascal';
    const Factor = 1E+03;
  end;
  TKilopascalId = specialize TFactoredUnitId<TPascalUnit, TKilopascalUnit>;

const
  kPa: specialize TQuantity<TPascalUnit> = (Value: TKilopascalUnit.Factor);

type
  { Unit of Joule }
  TJouleUnit = record
    const Symbol = 'J';
    const Name   = 'joule';
  end;
  TJoules = specialize TQuantity<TJouleUnit>;
  TJouleId = specialize TUnitId<TJouleUnit>;

var
  J: TJouleId;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;

operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterId): TJoules; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

type
  { Unit of Gigajoule }
  TGigajouleUnit = record
    const Symbol = 'GJ';
    const Name   = 'gigajoule';
    const Factor = 1E+09;
  end;
  TGigajouleId = specialize TFactoredUnitId<TJouleUnit, TGigajouleUnit>;

const
  GJ: specialize TQuantity<TJouleUnit> = (Value: TGigajouleUnit.Factor);

type
  { Unit of Megajoule }
  TMegajouleUnit = record
    const Symbol = 'MJ';
    const Name   = 'magajoule';
    const Factor = 1E+06;
  end;
  TMegajouleId = specialize TFactoredUnitId<TJouleUnit, TMegajouleUnit>;

const
  MJ: specialize TQuantity<TJouleUnit> = (Value: TMegajouleUnit.Factor);

type
  { Unit of Kilojoule }
  TKilojouleUnit = record
    const Symbol = 'kJ';
    const Name   = 'kilojoule';
    const Factor = 1E+03;
  end;
  TKilojouleId = specialize TFactoredUnitId<TJouleUnit, TKilojouleUnit>;

const
  kJ: specialize TQuantity<TJouleUnit> = (Value: TKilojouleUnit.Factor);

type
  { Unit of Watt }
  TWattUnit = record
    const Symbol = 'W';
    const Name   = 'watt';
  end;
  TWatts = specialize TQuantity<TWattUnit>;
  TWattId = specialize TUnitId<TWattUnit>;

var
  W: TWattId;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TSecondId): TWatts; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJoules; const ARight: TRadiansPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TRadiansPerSecond): TJoules; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons; inline;

type
  { Unit of Gigawatt }
  TGigawattUnit = record
    const Symbol = 'GW';
    const Name   = 'gigawatt';
    const Factor = 1E+09;
  end;
  TGigawattId = specialize TFactoredUnitId<TWattUnit, TGigawattUnit>;

const
  GW: specialize TQuantity<TWattUnit> = (Value: TGigawattUnit.Factor);

type
  { Unit of Megawatt }
  TMegawattUnit = record
    const Symbol = 'MW';
    const Name   = 'megawatt';
    const Factor = 1E+06;
  end;
  TMegawattId = specialize TFactoredUnitId<TWattUnit, TMegawattUnit>;

const
  megawatt: specialize TQuantity<TWattUnit> = (Value: TMegawattUnit.Factor);

type
  { Unit of Kilowatt }
  TKilowattUnit = record
    const Symbol = 'kW';
    const Name   = 'kilowatt';
    const Factor = 1E+03;
  end;
  TKilowattId = specialize TFactoredUnitId<TWattUnit, TKilowattUnit>;

const
  kW: specialize TQuantity<TWattUnit> = (Value: TKilowattUnit.Factor);

type
  { Unit of Milliwatt }
  TMilliwattUnit = record
    const Symbol = 'mW';
    const Name   = 'milliwatt';
    const Factor = 1E-03;
  end;
  TMilliwattId = specialize TFactoredUnitId<TWattUnit, TMilliwattUnit>;

const
  mW: specialize TQuantity<TWattUnit> = (Value: TMilliwattUnit.Factor);

type
  { Unit of Coulomb }
  TCoulombUnit = record
    const Symbol = 'C';
    const Name   = 'coulomb';
  end;
  TCoulombs = specialize TQuantity<TCoulombUnit>;
  TCoulombId = specialize TUnitId<TCoulombUnit>;

var
  C: TCoulombId;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;

operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereId): TCoulombs; inline;

type
  { Unit of Gigacoulomb }
  TGigacoulombUnit = record
    const Symbol = 'GC';
    const Name   = 'gigacoulomb';
    const Factor = 1E+09;
  end;
  TGigacoulombId = specialize TFactoredUnitId<TCoulombUnit, TGigacoulombUnit>;

const
  GC: specialize TQuantity<TCoulombUnit> = (Value: TGigacoulombUnit.Factor);

type
  { Unit of Megacoulomb }
  TMegacoulombUnit = record
    const Symbol = 'MC';
    const Name   = 'megacoulomb';
    const Factor = 1E+06;
  end;
  TMegacoulombId = specialize TFactoredUnitId<TCoulombUnit, TMegacoulombUnit>;

const
  megacoulomb: specialize TQuantity<TCoulombUnit> = (Value: TMegacoulombUnit.Factor);

type
  { Unit of Kilocoulomb }
  TKilocoulombUnit = record
    const Symbol = 'kC';
    const Name   = 'kilocoulomb';
    const Factor = 1E+03;
  end;
  TKilocoulombId = specialize TFactoredUnitId<TCoulombUnit, TKilocoulombUnit>;

const
  kC: specialize TQuantity<TCoulombUnit> = (Value: TKilocoulombUnit.Factor);

type
  { Unit of Millicoulomb }
  TMillicoulombUnit = record
    const Symbol = 'mC';
    const Name   = 'millicoulomb';
    const Factor = 1E-03;
  end;
  TMillicoulombId = specialize TFactoredUnitId<TCoulombUnit, TMillicoulombUnit>;

const
  mC: specialize TQuantity<TCoulombUnit> = (Value: TMillicoulombUnit.Factor);

type
  { Unit of SquareCoulomb }
  TSquareCoulombUnit = record
    const Symbol = 'C2';
    const Name   = 'square coulomb';
  end;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;
  TSquareCoulombId = specialize TUnitId<TSquareCoulombUnit>;

var
  C2: TSquareCoulombId;

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
  TVoltId = specialize TUnitId<TVoltUnit>;

var
  V: TVoltId;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;

operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereId): TVolts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombId): TVolts; inline;

type
  { Unit of Gigavolt }
  TGigavoltUnit = record
    const Symbol = 'GV';
    const Name   = 'gigavolt';
    const Factor = 1E+09;
  end;
  TGigavoltId = specialize TFactoredUnitId<TVoltUnit, TGigavoltUnit>;

const
  GV: specialize TQuantity<TVoltUnit> = (Value: TGigavoltUnit.Factor);

type
  { Unit of Megavolt }
  TMegavoltUnit = record
    const Symbol = 'MV';
    const Name   = 'megavolt';
    const Factor = 1E+06;
  end;
  TMegavoltId = specialize TFactoredUnitId<TVoltUnit, TMegavoltUnit>;

const
  megavolt: specialize TQuantity<TVoltUnit> = (Value: TMegavoltUnit.Factor);

type
  { Unit of Kilovolt }
  TKilovoltUnit = record
    const Symbol = 'kV';
    const Name   = 'kilovolt';
    const Factor = 1E+03;
  end;
  TKilovoltId = specialize TFactoredUnitId<TVoltUnit, TKilovoltUnit>;

const
  kV: specialize TQuantity<TVoltUnit> = (Value: TKilovoltUnit.Factor);

type
  { Unit of Millivolt }
  TMillivoltUnit = record
    const Symbol = 'mV';
    const Name   = 'millivolt';
    const Factor = 1E-03;
  end;
  TMillivoltId = specialize TFactoredUnitId<TVoltUnit, TMillivoltUnit>;

const
  mV: specialize TQuantity<TVoltUnit> = (Value: TMillivoltUnit.Factor);

type
  { Unit of SquareVolt }
  TSquareVoltUnit = record
    const Symbol = 'V2';
    const Name   = 'square volt';
  end;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;
  TSquareVoltId = specialize TUnitId<TSquareVoltUnit>;

var
  V2: TSquareVoltId;

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
  TFaradId = specialize TUnitId<TFaradUnit>;

var
  F: TFaradId;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltId): TFarads; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;

type
  { Unit of Gigafarad }
  TGigafaradUnit = record
    const Symbol = 'GF';
    const Name   = 'gigafarad';
    const Factor = 1E+09;
  end;
  TGigafaradId = specialize TFactoredUnitId<TFaradUnit, TGigafaradUnit>;

const
  GF: specialize TQuantity<TFaradUnit> = (Value: TGigafaradUnit.Factor);

type
  { Unit of Megafarad }
  TMegafaradUnit = record
    const Symbol = 'MF';
    const Name   = 'megafarad';
    const Factor = 1E+06;
  end;
  TMegafaradId = specialize TFactoredUnitId<TFaradUnit, TMegafaradUnit>;

const
  megafarad: specialize TQuantity<TFaradUnit> = (Value: TMegafaradUnit.Factor);

type
  { Unit of Kilofarad }
  TKilofaradUnit = record
    const Symbol = 'kF';
    const Name   = 'kilofarad';
    const Factor = 1E+03;
  end;
  TKilofaradId = specialize TFactoredUnitId<TFaradUnit, TKilofaradUnit>;

const
  kF: specialize TQuantity<TFaradUnit> = (Value: TKilofaradUnit.Factor);

type
  { Unit of Millifarad }
  TMillifaradUnit = record
    const Symbol = 'mF';
    const Name   = 'millifarad';
    const Factor = 1E-03;
  end;
  TMillifaradId = specialize TFactoredUnitId<TFaradUnit, TMillifaradUnit>;

const
  mF: specialize TQuantity<TFaradUnit> = (Value: TMillifaradUnit.Factor);

type
  { Unit of Microfarad }
  TMicrofaradUnit = record
    const Symbol = 'uF';
    const Name   = 'microfarad';
    const Factor = 1E-06;
  end;
  TMicrofaradId = specialize TFactoredUnitId<TFaradUnit, TMicrofaradUnit>;

const
  uF: specialize TQuantity<TFaradUnit> = (Value: TMicrofaradUnit.Factor);

type
  { Unit of Nanofarad }
  TNanofaradUnit = record
    const Symbol = 'nF';
    const Name   = 'nanofarad';
    const Factor = 1E-09;
  end;
  TNanofaradId = specialize TFactoredUnitId<TFaradUnit, TNanofaradUnit>;

const
  nF: specialize TQuantity<TFaradUnit> = (Value: TNanofaradUnit.Factor);

type
  { Unit of Picofarad }
  TPicofaradUnit = record
    const Symbol = 'pF';
    const Name   = 'picofarad';
    const Factor = 1E-12;
  end;
  TPicofaradId = specialize TFactoredUnitId<TFaradUnit, TPicofaradUnit>;

const
  pF: specialize TQuantity<TFaradUnit> = (Value: TPicofaradUnit.Factor);

type
  { Unit of Ohm }
  TOhmUnit = record
    const Symbol = 'Ω';
    const Name   = 'ohm';
  end;
  TOhms = specialize TQuantity<TOhmUnit>;
  TOhmId = specialize TUnitId<TOhmUnit>;

var
  ohm: TOhmId;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;

operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereId): TOhms; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms; inline;
operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads; inline;
operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds; inline;
operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds; inline;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms; inline;
operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes; inline;
operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts; inline;
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts; inline;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;

type
  { Unit of Gigaohm }
  TGigaohmUnit = record
    const Symbol = 'GΩ';
    const Name   = 'gigaohm';
    const Factor = 1E+09;
  end;
  TGigaohmId = specialize TFactoredUnitId<TOhmUnit, TGigaohmUnit>;

const
  gigaohm: specialize TQuantity<TOhmUnit> = (Value: TGigaohmUnit.Factor);

type
  { Unit of Megaohm }
  TMegaohmUnit = record
    const Symbol = 'MΩ';
    const Name   = 'megaohm';
    const Factor = 1E+06;
  end;
  TMegaohmId = specialize TFactoredUnitId<TOhmUnit, TMegaohmUnit>;

const
  megaohm: specialize TQuantity<TOhmUnit> = (Value: TMegaohmUnit.Factor);

type
  { Unit of Kiloohm }
  TKiloohmUnit = record
    const Symbol = 'kΩ';
    const Name   = 'kiloohm';
    const Factor = 1E+03;
  end;
  TKiloohmId = specialize TFactoredUnitId<TOhmUnit, TKiloohmUnit>;

const
  kiloohm: specialize TQuantity<TOhmUnit> = (Value: TKiloohmUnit.Factor);

type
  { Unit of Milliohm }
  TMilliohmUnit = record
    const Symbol = 'mΩ';
    const Name   = 'milliohm';
    const Factor = 1E-03;
  end;
  TMilliohmId = specialize TFactoredUnitId<TOhmUnit, TMilliohmUnit>;

const
  milliohm: specialize TQuantity<TOhmUnit> = (Value: TMilliohmUnit.Factor);

type
  { Unit of Microohm }
  TMicroohmUnit = record
    const Symbol = 'uΩ';
    const Name   = 'microohm';
    const Factor = 1E-06;
  end;
  TMicroohmId = specialize TFactoredUnitId<TOhmUnit, TMicroohmUnit>;

const
  microohm: specialize TQuantity<TOhmUnit> = (Value: TMicroohmUnit.Factor);

type
  { Unit of Nanoohm }
  TNanoohmUnit = record
    const Symbol = 'nΩ';
    const Name   = 'nanoohm';
    const Factor = 1E-09;
  end;
  TNanoohmId = specialize TFactoredUnitId<TOhmUnit, TNanoohmUnit>;

const
  nanoohm: specialize TQuantity<TOhmUnit> = (Value: TNanoohmUnit.Factor);

type
  { Unit of Picoohm }
  TPicoohmUnit = record
    const Symbol = 'pΩ';
    const Name   = 'picoohm';
    const Factor = 1E-12;
  end;
  TPicoohmId = specialize TFactoredUnitId<TOhmUnit, TPicoohmUnit>;

const
  picoohm: specialize TQuantity<TOhmUnit> = (Value: TPicoohmUnit.Factor);

type
  { Unit of Siemens }
  TSiemensUnit = record
    const Symbol = 'S';
    const Name   = 'siemens';
  end;
  TSiemens = specialize TQuantity<TSiemensUnit>;
  TSiemensId = specialize TUnitId<TSiemensUnit>;

var
  siemens: TSiemensId;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;

operator /(const ALeft: double; const {%H-}ARight: TOhmId): TSiemens; inline;

type
  { Unit of Weber }
  TWeberUnit = record
    const Symbol = 'Wb';
    const Name   = 'weber';
  end;
  TWebers = specialize TQuantity<TWeberUnit>;
  TWeberId = specialize TUnitId<TWeberUnit>;

var
  Wb: TWeberId;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;

operator *(const ALeft: TVolts; const {%H-}ARight: TSecondId): TWebers; inline;

type
  { Unit of Tesla }
  TTeslaUnit = record
    const Symbol = 'T';
    const Name   = 'tesla';
  end;
  TTeslas = specialize TQuantity<TTeslaUnit>;
  TTeslaId = specialize TUnitId<TTeslaUnit>;

var
  T: TTeslaId;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;

operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterId): TTeslas; inline;

type
  { Unit of Henry }
  THenryUnit = record
    const Symbol = 'H';
    const Name   = 'henry';
  end;
  THenrys = specialize TQuantity<THenryUnit>;
  THenryId = specialize TUnitId<THenryUnit>;

var
  H: THenryId;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys; inline;
operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes; inline;
operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers; inline;
operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers; inline;

operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereId): THenrys; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenrys; inline;
operator /(const ALeft: TOhms; const ARight: THenrys): THertz; inline;
operator *(const ALeft: THenrys; const ARight: THertz): TOhms; inline;
operator *(const ALeft: THertz; const ARight: THenrys): TOhms; inline;

type
  { Unit of Lumen }
  TLumenUnit = record
    const Symbol = 'lm';
    const Name   = 'lumen';
  end;
  TLumens = specialize TQuantity<TLumenUnit>;
  TLumenId = specialize TUnitId<TLumenUnit>;

var
  lm: TLumenId;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;

operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianId): TLumens; inline;

type
  { Unit of Lux }
  TLuxUnit = record
    const Symbol = 'lx';
    const Name   = 'lux';
  end;
  TLux = specialize TQuantity<TLuxUnit>;
  TLuxId = specialize TUnitId<TLuxUnit>;

var
  lx: TLuxId;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;

operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterId): TLux; inline;

type
  { Unit of Bequerel }
  TBequerelUnit = record
    const Symbol = 'Bq';
    const Name   = 'bequerel';
  end;
  TBequerels = specialize TQuantity<TBequerelUnit>;
  TBequerelId = specialize TUnitId<TBequerelUnit>;

var
  Bq: TBequerelId;

type
  { Unit of Gray }
  TGrayUnit = record
    const Symbol = 'Gy';
    const Name   = 'gray';
  end;
  TGrays = specialize TQuantity<TGrayUnit>;
  TGrayId = specialize TUnitId<TGrayUnit>;

var
  Gy: TGrayId;

type
  { Unit of Milligray }
  TMilligrayUnit = record
    const Symbol = 'mGy';
    const Name   = 'milli gray';
    const Factor = 1E-03;
  end;
  TMilligrayId = specialize TFactoredUnitId<TGrayUnit, TMilligrayUnit>;

const
  mGy: specialize TQuantity<TGrayUnit> = (Value: TMilligrayUnit.Factor);

type
  { Unit of Sievert }
  TSievertUnit = record
    const Symbol = 'Sv';
    const Name   = 'sievert';
  end;
  TSieverts = specialize TQuantity<TSievertUnit>;
  TSievertId = specialize TUnitId<TSievertUnit>;

var
  Sv: TSievertId;

type
  { Unit of MilliSievert }
  TMilliSievertUnit = record
    const Symbol = 'mSv';
    const Name   = 'millisievert';
    const Factor = 1E-03;
  end;
  TMilliSievertId = specialize TFactoredUnitId<TSievertUnit, TMilliSievertUnit>;

const
  mSv: specialize TQuantity<TSievertUnit> = (Value: TMilliSievertUnit.Factor);

type
  { Unit of Katal }
  TKatalUnit = record
    const Symbol = 'kat';
    const Name   = 'katal';
  end;
  TKatals = specialize TQuantity<TKatalUnit>;
  TKatalId = specialize TUnitId<TKatalUnit>;

var
  kat: TKatalId;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;

operator /(const ALeft: TMoles; const {%H-}ARight: TSecondId): TKatals; inline;

type
  { Unit of NewtonMeter }
  TNewtonMeterUnit = record
    const Symbol = 'N.m';
    const Name   = 'newton meter';
  end;
  TNewtonMeters = specialize TQuantity<TNewtonMeterUnit>;
  TNewtonMeterId = specialize TUnitId<TNewtonMeterUnit>;

type
  { Unit of JoulePerRadian }
  TJoulePerRadianUnit = record
    const Symbol = 'J/rad';
    const Name   = 'joule per radian';
  end;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerRadianId = specialize TUnitId<TJoulePerRadianUnit>;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TRadianId): TJoulesPerRadian; inline;

type
  { Unit of JoulePerDegree }
  TJoulePerDegreeUnit = record
    const Symbol = 'J/deg';
    const Name   = 'joule per degree';
    const Factor = 180/Pi;
  end;
  TJoulePerDegreeId = specialize TFactoredUnitId<TJoulePerRadianUnit, TJoulePerDegreeUnit>;

type
  { Unit of NewtonMeterPerRadian }
  TNewtonMeterPerRadianUnit = record
    const Symbol = 'N.m/rad';
    const Name   = 'newton meter per radian';
  end;
  TNewtonMetersPerRadian = specialize TQuantity<TNewtonMeterPerRadianUnit>;
  TNewtonMeterPerRadianId = specialize TUnitId<TNewtonMeterPerRadianUnit>;

type
  { Unit of NewtonMeterPerDegree }
  TNewtonMeterPerDegreeUnit = record
    const Symbol = 'N.m/deg';
    const Name   = 'newton meter per degree';
    const Factor = 180/Pi;
  end;
  TNewtonMeterPerDegreeId = specialize TFactoredUnitId<TJoulePerRadianUnit, TNewtonMeterPerDegreeUnit>;

type
  { Unit of KilogramPerMeter }
  TKilogramPerMeterUnit = record
    const Symbol = 'kg/m';
    const Name   = 'kilogram per meter';
  end;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;
  TKilogramPerMeterId = specialize TUnitId<TKilogramPerMeterUnit>;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;

operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterId): TKilogramsPerMeter; inline;

type
  { Unit of KilogramPerSquareMeter }
  TKilogramPerSquareMeterUnit = record
    const Symbol = 'kg/m2';
    const Name   = 'kilogram per square meter';
  end;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;
  TKilogramPerSquareMeterId = specialize TUnitId<TKilogramPerSquareMeterUnit>;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterId): TKilogramsPerSquareMeter; inline;

type
  { Unit of KilogramPerCubicMeter }
  TKilogramPerCubicMeterUnit = record
    const Symbol = 'kg/m3';
    const Name   = 'kilogram per cubic meter';
  end;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;
  TKilogramPerCubicMeterId = specialize TUnitId<TKilogramPerCubicMeterUnit>;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;

operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterId): TKilogramsPerCubicMeter; inline;

type
  { Unit of KilogramPerCubicMillimeter }
  TKilogramPerCubicMillimeterUnit = record
    const Symbol = 'kg/mm3';
    const Name   = 'kilogram per cubic millimeter';
    const Factor = 1E+09;
  end;
  TKilogramPerCubicMillimeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicMillimeterUnit>;

type
  { Unit of KilogramPerCubicCentimeter }
  TKilogramPerCubicCentimeterUnit = record
    const Symbol = 'kg/cm3';
    const Name   = 'kilogram per cubic centimeter';
    const Factor = 1E+06;
  end;
  TKilogramPerCubicCentimeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicCentimeterUnit>;

type
  { Unit of KilogramPerCubicDecimeter }
  TKilogramPerCubicDecimeterUnit = record
    const Symbol = 'kg/dm3';
    const Name   = 'kilogram per cubic decimeter';
    const Factor = 1E+03;
  end;
  TKilogramPerCubicDecimeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TKilogramPerCubicDecimeterUnit>;

type
  { Unit of HectogramPerCubicMeter }
  THectogramPerCubicMeterUnit = record
    const Symbol = 'hg/m3';
    const Name   = 'hectogram per cubic meter';
    const Factor = 1E-01;
  end;
  THectogramPerCubicMeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, THectogramPerCubicMeterUnit>;

type
  { Unit of DecagramPerCubicMeter }
  TDecagramPerCubicMeterUnit = record
    const Symbol = 'dag/m3';
    const Name   = 'decagram per cubic meter';
    const Factor = 1E-02;
  end;
  TDecagramPerCubicMeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TDecagramPerCubicMeterUnit>;

type
  { Unit of GramPerCubicMeter }
  TGramPerCubicMeterUnit = record
    const Symbol = 'g/m3';
    const Name   = 'gram per cubic meter';
    const Factor = 1E-03;
  end;
  TGramPerCubicMeterId = specialize TFactoredUnitId<TKilogramPerCubicMeterUnit, TGramPerCubicMeterUnit>;

type
  { Unit of NewtonPerCubicMeter }
  TNewtonPerCubicMeterUnit = record
    const Symbol = 'N/m3';
    const Name   = 'newton per cubic meter';
  end;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;
  TNewtonPerCubicMeterId = specialize TUnitId<TNewtonPerCubicMeterUnit>;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterId): TNewtonsPerCubicMeter; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

type
  { Unit of NewtonPerMeter }
  TNewtonPerMeterUnit = record
    const Symbol = 'N/m';
    const Name   = 'newton per meter';
  end;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;
  TNewtonPerMeterId = specialize TUnitId<TNewtonPerMeterUnit>;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterId): TNewtonsPerMeter; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals; inline;

type
  { Unit of NewtonPerMillimeter }
  TNewtonPerMillimeterUnit = record
    const Symbol = 'N/mm';
    const Name   = 'newton per millimeter';
    const Factor = 1E+03;
  end;
  TNewtonPerMillimeterId = specialize TFactoredUnitId<TNewtonPerMeterUnit, TNewtonPerMillimeterUnit>;

type
  { Unit of KilogramMeterPerSecond }
  TKilogramMeterPerSecondUnit = record
    const Symbol = 'kg.m/s';
    const Name   = 'kilogram meter per second';
  end;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TKilogramMeterPerSecondId = specialize TUnitId<TKilogramMeterPerSecondUnit>;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondId): TKilogramMetersPerSecond; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons; inline;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSecondId): TKilogramMetersPerSecond; inline;

type
  { Unit of NewtonSecond }
  TNewtonSecondUnit = record
    const Symbol = 'N.s';
    const Name   = 'newton second';
  end;
  TNewtonSeconds = specialize TQuantity<TNewtonSecondUnit>;
  TNewtonSecondId = specialize TUnitId<TNewtonSecondUnit>;

type
  { Unit of KilogramSquareMeter }
  TKilogramSquareMeterUnit = record
    const Symbol = 'kg.m2';
    const Name   = 'kilogram square meter';
  end;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;
  TKilogramSquareMeterId = specialize TUnitId<TKilogramSquareMeterUnit>;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterId): TKilogramSquareMeters; inline;

type
  { Unit of KilogramSquareMeterPerSecond }
  TKilogramSquareMeterPerSecondUnit = record
    const Symbol = 'kg.m2 / s';
    const Name   = 'kilogram square meter per second';
  end;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMeterPerSecondId = specialize TUnitId<TKilogramSquareMeterPerSecondUnit>;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;

operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondId): TKilogramSquareMetersPerSecond; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TRadiansPerSecond): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TRadiansPerSecond): TKilogramSquareMeters; inline;

type
  { Unit of SquareMeterPerSquareSecond }
  TSquareMeterPerSquareSecondUnit = record
    const Symbol = 'm2/s2';
    const Name   = 'square meter per square second';
  end;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSquareMeterPerSquareSecondId = specialize TUnitId<TSquareMeterPerSquareSecondUnit>;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramId): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;

type
  { Unit of SteradianPerSquareSecond }
  TSteradianPerSquareSecondUnit = record
    const Symbol = 'rad2/s2';
    const Name   = 'square rad per square second';
  end;
  TSteradiansPerSquareSecond = specialize TQuantity<TSteradianPerSquareSecondUnit>;
  TSteradianPerSquareSecondId = specialize TUnitId<TSteradianPerSquareSecondUnit>;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;

operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondId): TSteradiansPerSquareSecond; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond; inline;

// alternative definition [ rad2/s2 ] = [ m/s2 ] / [ m ]
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSteradiansPerSquareSecond): TMeters; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TSteradiansPerSquareSecond): TMetersPerSquareSecond; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSteradiansPerSquareSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TKilogramSquareMeters): TJoules; inline;
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSteradiansPerSquareSecond): TJoules; inline;

type
  { Unit of CubicMeterPerSecond }
  TCubicMeterPerSecondUnit = record
    const Symbol = 'm3/s';
    const Name   = 'cubic meter per second';
  end;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;
  TCubicMeterPerSecondId = specialize TUnitId<TCubicMeterPerSecondUnit>;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondId): TCubicMetersPerSecond; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

type
  { Unit of PascalSecond }
  TPascalSecondUnit = record
    const Symbol = 'Pa.s';
    const Name   = 'pascal second';
  end;
  TPascalSeconds = specialize TQuantity<TPascalSecondUnit>;
  TPascalSecondId = specialize TUnitId<TPascalSecondUnit>;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals; inline;

operator *(const ALeft: TPascals; const {%H-}ARight: TSecondId): TPascalSeconds; inline;

type
  { Unit of SquareMeterPerSecond }
  TSquareMeterPerSecondUnit = record
    const Symbol = 'm2/s';
    const Name   = 'square meter per second';
  end;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;
  TSquareMeterPerSecondId = specialize TUnitId<TSquareMeterPerSecondUnit>;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondId): TSquareMetersPerSecond; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds; inline;

type
  { Unit of NewtonSquareMeter }
  TNewtonSquareMeterUnit = record
    const Symbol = 'N.m2';
    const Name   = 'newton square meter';
  end;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;
  TNewtonSquareMeterId = specialize TUnitId<TNewtonSquareMeterUnit>;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeters; inline;

type
  { Unit of NewtonPerSquareKilogram }
  TNewtonPerSquareKilogramUnit = record
    const Symbol = 'N/kg2';
    const Name   = 'newton per square kilogram';
  end;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;
  TNewtonPerSquareKilogramId = specialize TUnitId<TNewtonPerSquareKilogramUnit>;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramId): TNewtonsPerSquareKilogram; inline;

type
  { Unit of SquareKilogramPerMeter }
  TSquareKilogramPerMeterUnit = record
    const Symbol = 'kg2/m';
    const Name   = 'square kilogram per meter';
  end;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;
  TSquareKilogramPerMeterId = specialize TUnitId<TSquareKilogramPerMeterUnit>;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterId): TSquareKilogramsPerMeter; inline;

type
  { Unit of SquareKilogramPerSquareMeter }
  TSquareKilogramPerSquareMeterUnit = record
    const Symbol = 'kg2/m2';
    const Name   = 'square kilogram per square meter';
  end;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramPerSquareMeterId = specialize TUnitId<TSquareKilogramPerSquareMeterUnit>;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterId): TSquareKilogramsPerSquareMeter; inline;

type
  { Unit of SquareMeterPerSquareKilogram }
  TSquareMeterPerSquareKilogramUnit = record
    const Symbol = 'm2/kg2';
    const Name   = 'square meter per square kilogram';
  end;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TSquareMeterPerSquareKilogramId = specialize TUnitId<TSquareMeterPerSquareKilogramUnit>;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramId): TSquareMetersPerSquareKilogram; inline;

type
  { Unit of NewtonSquareMeterPerSquareKilogram }
  TNewtonSquareMeterPerSquareKilogramUnit = record
    const Symbol = 'N.m2/kg2';
    const Name   = 'newton square meter per square kilogram';
  end;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMeterPerSquareKilogramId = specialize TUnitId<TNewtonSquareMeterPerSquareKilogramUnit>;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons; inline;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters; inline;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMetersPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules; inline;

type
  { Unit of ReciprocalKelvin }
  TReciprocalKelvinUnit = record
    const Symbol = '1/K';
    const Name   = 'reciprocal kelvin';
  end;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;
  TReciprocalKelvinId = specialize TUnitId<TReciprocalKelvinUnit>;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;

operator /(const ALeft: double; const {%H-}ARight: TKelvinId): TReciprocalKelvins; inline;

type
  { Unit of KilogramKelvin }
  TKilogramKelvinUnit = record
    const Symbol = 'kg.K';
    const Name   = 'kilogram kelvin';
  end;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;
  TKilogramKelvinId = specialize TUnitId<TKilogramKelvinUnit>;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;

operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinId): TKilogramKelvins; inline;

type
  { Unit of JoulePerKelvin }
  TJoulePerKelvinUnit = record
    const Symbol = 'J/K';
    const Name   = 'joule per kelvin';
  end;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;
  TJoulePerKelvinId = specialize TUnitId<TJoulePerKelvinUnit>;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinId): TJoulesPerKelvin; inline;

type
  { Unit of JoulePerKilogram }
  TJoulePerKilogramUnit = record
    const Symbol = 'J/kg';
    const Name   = 'joule per kilogram';
  end;
  TJoulesPerKilogram = specialize TQuantity<TJoulePerKilogramUnit>;
  TJoulePerKilogramId = specialize TUnitId<TJoulePerKilogramUnit>;

type
  { Unit of JoulePerKilogramPerKelvin }
  TJoulePerKilogramPerKelvinUnit = record
    const Symbol = 'J/kg/K';
    const Name   = 'joule per kilogram per kelvin';
  end;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;
  TJoulePerKilogramPerKelvinId = specialize TUnitId<TJoulePerKilogramPerKelvinUnit>;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules; inline;
operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond; inline;

operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinId): TJoulesPerKilogramPerKelvin; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;

type
  { Unit of MeterKelvin }
  TMeterKelvinUnit = record
    const Symbol = 'm.K';
    const Name   = 'meter kelvin';
  end;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;
  TMeterKelvinId = specialize TUnitId<TMeterKelvinUnit>;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;

operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinId): TMeterKelvins; inline;

type
  { Unit of KelvinPerMeter }
  TKelvinPerMeterUnit = record
    const Symbol = 'K/m';
    const Name   = 'kelvin per meter';
  end;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;
  TKelvinPerMeterId = specialize TUnitId<TKelvinPerMeterUnit>;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;

operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterId): TKelvinsPerMeter; inline;

type
  { Unit of WattPerMeter }
  TWattPerMeterUnit = record
    const Symbol = 'W/m';
    const Name   = 'watt per meter';
  end;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;
  TWattPerMeterId = specialize TUnitId<TWattPerMeterUnit>;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;

operator /(const ALeft: TWatts; const {%H-}ARight: TMeterId): TWattsPerMeter; inline;

type
  { Unit of WattPerSquareMeter }
  TWattPerSquareMeterUnit = record
    const Symbol = 'W/m2';
    const Name   = 'watt per square meter';
  end;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;
  TWattPerSquareMeterId = specialize TUnitId<TWattPerSquareMeterUnit>;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;

operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterId): TWattsPerSquareMeter; inline;

type
  { Unit of WattPerKelvin }
  TWattPerKelvinUnit = record
    const Symbol = 'W/K';
    const Name   = 'watt per kelvin';
  end;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;
  TWattPerKelvinId = specialize TUnitId<TWattPerKelvinUnit>;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;

operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinId): TWattsPerKelvin; inline;

type
  { Unit of WattPerMeterPerKelvin }
  TWattPerMeterPerKelvinUnit = record
    const Symbol = 'W/m/K';
    const Name   = 'watt per meter per kelvin';
  end;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;
  TWattPerMeterPerKelvinId = specialize TUnitId<TWattPerMeterPerKelvinUnit>;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts; inline;
operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter; inline;

operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinId): TWattsPerMeterPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter; inline;

type
  { Unit of SquareMeterKelvin }
  TSquareMeterKelvinUnit = record
    const Symbol = 'm2.K';
    const Name   = 'square meter kelvin';
  end;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;
  TSquareMeterKelvinId = specialize TUnitId<TSquareMeterKelvinUnit>;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinId): TSquareMeterKelvins; inline;

type
  { Unit of WattPerSquareMeterPerKelvin }
  TWattPerSquareMeterPerKelvinUnit = record
    const Symbol = 'W/m2/K';
    const Name   = 'watt per square meter per kelvin';
  end;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;
  TWattPerSquareMeterPerKelvinId = specialize TUnitId<TWattPerSquareMeterPerKelvinUnit>;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts; inline;
operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter; inline;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinId): TWattsPerSquareMeterPerKelvin; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;

type
  { Unit of SquareMeterQuarticKelvin }
  TSquareMeterQuarticKelvinUnit = record
    const Symbol = 'm2.K4';
    const Name   = 'square meter quartic kelvin';
  end;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvinId = specialize TUnitId<TSquareMeterQuarticKelvinUnit>;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinId): TSquareMeterQuarticKelvins; inline;

type
  { Unit of WattPerQuarticKelvin }
  TWattPerQuarticKelvinUnit = record
    const Symbol = 'W/K4';
    const Name   = 'watt per quartic kelvin';
  end;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;
  TWattPerQuarticKelvinId = specialize TUnitId<TWattPerQuarticKelvinUnit>;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;

operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinId): TWattsPerQuarticKelvin; inline;

type
  { Unit of WattPerSquareMeterPerQuarticKelvin }
  TWattPerSquareMeterPerQuarticKelvinUnit = record
    const Symbol = 'W/m2/K4';
    const Name   = 'watt per square meter per quartic kelvin';
  end;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattPerSquareMeterPerQuarticKelvinId = specialize TUnitId<TWattPerSquareMeterPerQuarticKelvinUnit>;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts; inline;
operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter; inline;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinId): TWattsPerSquareMeterPerQuarticKelvin; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;

type
  { Unit of JoulePerMole }
  TJoulePerMoleUnit = record
    const Symbol = 'J/mol';
    const Name   = 'joule per mole';
  end;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;
  TJoulePerMoleId = specialize TUnitId<TJoulePerMoleUnit>;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;

operator /(const ALeft: TJoules; const {%H-}ARight: TMoleId): TJoulesPerMole; inline;

type
  { Unit of MoleKelvin }
  TMoleKelvinUnit = record
    const Symbol = 'mol.K';
    const Name   = 'mole kelvin';
  end;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;
  TMoleKelvinId = specialize TUnitId<TMoleKelvinUnit>;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;

operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinId): TMoleKelvins; inline;

type
  { Unit of JoulePerMolePerKelvin }
  TJoulePerMolePerKelvinUnit = record
    const Symbol = 'J/mol/K';
    const Name   = 'joule per mole per kelvin';
  end;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;
  TJoulePerMolePerKelvinId = specialize TUnitId<TJoulePerMolePerKelvinUnit>;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules; inline;
operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole; inline;

operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinId): TJoulesPerMolePerKelvin; inline;

type
  { Unit of OhmMeter }
  TOhmMeterUnit = record
    const Symbol = 'Ω.m';
    const Name   = 'ohm meter';
  end;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;
  TOhmMeterId = specialize TUnitId<TOhmMeterUnit>;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;

operator *(const ALeft: TOhms; const {%H-}ARight: TMeterId): TOhmMeters; inline;

type
  { Unit of VoltPerMeter }
  TVoltPerMeterUnit = record
    const Symbol = 'V/m';
    const Name   = 'volt per meter';
  end;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;
  TVoltPerMeterId = specialize TUnitId<TVoltPerMeterUnit>;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;

operator /(const ALeft: TVolts; const {%H-}ARight: TMeterId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombId): TVoltsPerMeter; inline;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

type
  { Unit of CoulombPerMeter }
  TCoulombPerMeterUnit = record
    const Symbol = 'C/m';
    const Name   = 'coulomb per meter';
  end;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;
  TCoulombPerMeterId = specialize TUnitId<TCoulombPerMeterUnit>;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterId): TCoulombsPerMeter; inline;

type
  { Unit of SquareCoulombPerMeter }
  TSquareCoulombPerMeterUnit = record
    const Symbol = 'C2/m';
    const Name   = 'square coulomb per meter';
  end;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;
  TSquareCoulombPerMeterId = specialize TUnitId<TSquareCoulombPerMeterUnit>;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;

operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterId): TSquareCoulombsPerMeter; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

type
  { Unit of CoulombPerSquareMeter }
  TCoulombPerSquareMeterUnit = record
    const Symbol = 'C/m2';
    const Name   = 'coulomb per square meter';
  end;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;
  TCoulombPerSquareMeterId = specialize TUnitId<TCoulombPerSquareMeterUnit>;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterId): TCoulombsPerSquareMeter; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;

type
  { Unit of SquareMeterPerSquareCoulomb }
  TSquareMeterPerSquareCoulombUnit = record
    const Symbol = 'm2/C2';
    const Name   = 'square meter per square coulomb';
  end;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;
  TSquareMeterPerSquareCoulombId = specialize TUnitId<TSquareMeterPerSquareCoulombUnit>;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombId): TSquareMetersPerSquareCoulomb; inline;

type
  { Unit of NewtonPerSquareCoulomb }
  TNewtonPerSquareCoulombUnit = record
    const Symbol = 'N/C2';
    const Name   = 'newton per square coulomb';
  end;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;
  TNewtonPerSquareCoulombId = specialize TUnitId<TNewtonPerSquareCoulombUnit>;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombId): TNewtonsPerSquareCoulomb; inline;

type
  { Unit of NewtonSquareMeterPerSquareCoulomb }
  TNewtonSquareMeterPerSquareCoulombUnit = record
    const Symbol = 'N.m2/C2';
    const Name   = 'newton square meter per square coulomb';
  end;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMeterPerSquareCoulombId = specialize TUnitId<TNewtonSquareMeterPerSquareCoulombUnit>;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters; inline;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMetersPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules; inline;

type
  { Unit of VoltMeter }
  TVoltMeterUnit = record
    const Symbol = 'V.m';
    const Name   = 'volt meter';
  end;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;
  TVoltMeterId = specialize TUnitId<TVoltMeterUnit>;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;

operator *(const ALeft: TVolts; const {%H-}ARight: TMeterId): TVoltMeters; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

type
  { Unit of VoltMeterPerSecond }
  TVoltMeterPerSecondUnit = record
    const Symbol = 'V.m/s';
    const Name   = 'volt meter per second';
  end;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;
  TVoltMeterPerSecondId = specialize TUnitId<TVoltMeterPerSecondUnit>;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;

operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondId): TVoltMetersPerSecond; inline;

type
  { Unit of FaradPerMeter }
  TFaradPerMeterUnit = record
    const Symbol = 'F/m';
    const Name   = 'farad per meter';
  end;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;
  TFaradPerMeterId = specialize TUnitId<TFaradPerMeterUnit>;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;

operator /(const ALeft: TFarads; const {%H-}ARight: TMeterId): TFaradsPerMeter; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs; inline;
operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter; inline;
operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double; inline;

type
  { Unit of AmperePerMeter }
  TAmperePerMeterUnit = record
    const Symbol = 'A/m';
    const Name   = 'ampere per meter';
  end;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;
  TAmperePerMeterId = specialize TUnitId<TAmperePerMeterUnit>;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;

operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterId): TAmperesPerMeter; inline;

type
  { Unit of MeterPerAmpere }
  TMeterPerAmpereUnit = record
    const Symbol = 'm/A';
    const Name   = 'meter per ampere';
  end;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;
  TMeterPerAmpereId = specialize TUnitId<TMeterPerAmpereUnit>;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;

operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereId): TMetersPerAmpere; inline;

type
  { Unit of TeslaMeter }
  TTeslaMeterUnit = record
    const Symbol = 'T.m';
    const Name   = 'tesla meter';
  end;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;
  TTeslaMeterId = specialize TUnitId<TTeslaMeterUnit>;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;

operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterId): TTeslaMeters; inline;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereId): TTeslaMeters; inline;

type
  { Unit of TeslaPerAmpere }
  TTeslaPerAmpereUnit = record
    const Symbol = 'T/A';
    const Name   = 'tesla per ampere';
  end;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;
  TTeslaPerAmpereId = specialize TUnitId<TTeslaPerAmpereUnit>;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;

operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereId): TTeslasPerAmpere; inline;

type
  { Unit of HenryPerMeter }
  THenryPerMeterUnit = record
    const Symbol = 'H/m';
    const Name   = 'henry per meter';
  end;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;
  THenryPerMeterId = specialize TUnitId<THenryPerMeterUnit>;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter; inline;
operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys; inline;
operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys; inline;

operator /(const ALeft: THenrys; const {%H-}ARight: TMeterId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters; inline;

operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereId): THenrysPerMeter; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter; inline;
operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons; inline;
operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons; inline;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareAmpereId): THenrysPerMeter; inline;

type
  { Unit of RadianPerMeter }
  TRadianPerMeterUnit = record
    const Symbol = 'rad/m';
    const Name   = 'radian per meter';
  end;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;
  TRadianPerMeterId = specialize TUnitId<TRadianPerMeterUnit>;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;

operator /(const ALeft: TRadians; const {%H-}ARight: TMeterId): TRadiansPerMeter; inline;

{ Helpers }

type
  TBequerelHelper = record helper for TBequerelId
    function From(const AQuantity: THertz): TBequerels;
  end;

type
  TGrayHelper = record helper for TGrayId
    function From(const AQuantity: TSquareMetersPerSquareSecond): TGrays;
  end;

type
  TSievertHelper = record helper for TSievertId
    function From(const AQuantity: TSquareMetersPerSquareSecond): TSieverts;
  end;

type
  TNewtonMeterHelper = record helper for TNewtonMeterId
    function From(const AQuantity: TJoules): TNewtonMeters;
  end;

type
  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianId
    function From(const AQuantity: TJoulesPerRadian): TNewtonMetersPerRadian;
  end;

type
  TNewtonSecondHelper = record helper for TNewtonSecondId
    function From(const AQuantity: TKilogramMetersPerSecond): TNewtonSeconds;
  end;

type
  TJoulePerKilogramHelper = record helper for TJoulePerKilogramId
    function From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulesPerKilogram;
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
function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
function SquarePower(AQuantity: TVolts): TSquareVolts;
function SquareRoot(AQuantity: TSquareVolts): TVolts;
function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
function SquarePower(AQuantity: TRadiansPerSecond): TSteradiansPerSquareSecond;
function SquareRoot(AQuantity: TSteradiansPerSquareSecond): TRadiansPerSecond;

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
  result.Value := System.Abs(Value);
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(Value) + ' ' + U.Symbol;
end;

function TQuantity.ToString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(Value, ffGeneral, Precision, Digits)  + ' ' + U.Symbol;
end;

function TQuantity.ToVerboseString: string;
begin
  result := FloatToStr(Value) + ' ' + U.Name;
end;

function TQuantity.ToVerboseString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(Value, ffGeneral, Precision, Digits)  + ' ' + U.Name;
end;

class operator TQuantity.+(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value + ARight.Value;
end;

class operator TQuantity.-(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value - ARight.Value;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TSelf): TSelf;
begin
  result.Value := AValue * ASelf.Value;
end;

class operator TQuantity.*(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.Value := ASelf.Value * AValue;
end;

class operator TQuantity./(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.Value := ASelf.Value / AValue;
end;

class operator TQuantity./(const ALeft, ARight: TSelf): double;
begin
  result := ALeft.Value / ARight.Value;
end;

class operator TQuantity.mod(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value mod ARight.Value;
end;

class operator TQuantity.=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value = ARight.Value;
end;

class operator TQuantity.<(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value < ARight.Value;
end;

class operator TQuantity.>(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value > ARight.Value;
end;

class operator TQuantity.<=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value <= ARight.Value;
end;

class operator TQuantity.>=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value >= ARight.Value;
end;

{ TUnitId }

class function TUnitId.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.Value := AQuantity.Value;
end;

class operator TUnitId.*(const AValue: double; const ASelf: TSelf): TBaseQuantity;
begin
  result.Value := AValue;
end;

{ TFactoredUnitId }

class function TFactoredUnitId.From(const AQuantity: TBaseQuantity): TFactoredQuantity;
begin
  result.Value := AQuantity.Value / U.Factor;
end;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m3 ] = [ m2 ] * [ m ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ kg2 ] = [ kg ] * [ kg ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

//
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSecondId): TMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// main definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TSquareSecondId): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSecondId): TRadiansPerSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TRadiansPerSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TMeters): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TRadiansPerSecond): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TSquareSecondId): TRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value;
end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSquareSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSeconds): TRadiansPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TRadiansPerSquareSecond): TRadiansPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: THertz): TSeconds;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: TSeconds): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: THertz): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: double; const {%H-}ARight: TSecondId): THertz;
begin
  result.Value := ALeft;
end;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSquareSecondId): TNewtons;
begin
  result.Value := ALeft.Value;
end;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterId): TPascals;
begin
  result.Value := ALeft.Value;
end;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TMeterId): TJoules;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TSecondId): TWatts;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJoules; const ARight: TRadiansPerSecond): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TJoules): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TJoules): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TRadiansPerSecond): TJoules;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const {%H-}ARight: TAmpereId): TCoulombs;
begin
  result.Value := ALeft.Value;
end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TAmpereId): TVolts;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TCoulombId): TVolts;
begin
  result.Value := ALeft.Value;
end;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TVoltId): TFarads;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TAmpereId): TOhms;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ Ω ] = [ W ] / [ A2 ]
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ Ω ] = [ V2 ] / [ W ]
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TSiemens): TOhms;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TSiemens; const ARight: TOhms): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TSiemens): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: double; const {%H-}ARight: TOhmId): TSiemens;
begin
  result.Value := ALeft;
end;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TSecondId): TWebers;
begin
  result.Value := ALeft.Value;
end;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TSquareMeterId): TTeslas;
begin
  result.Value := ALeft.Value;
end;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWebers; const {%H-}ARight: TAmpereId): THenrys;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhms; const ARight: THertz): THenrys;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TOhms; const ARight: THenrys): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrys; const ARight: THertz): TOhms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: THenrys): TOhms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCandelas; const {%H-}ARight: TSteradianId): TLumens;
begin
  result.Value := ALeft.Value;
end;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TLumens; const {%H-}ARight: TSquareMeterId): TLux;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMoles; const {%H-}ARight: TSecondId): TKatals;
begin
  result.Value := ALeft.Value;
end;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TRadianId): TJoulesPerRadian;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TMeterId): TKilogramsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterId): TKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilograms; const {%H-}ARight: TCubicMeterId): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCubicMeterId): TNewtonsPerCubicMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TMeterId): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TMeterPerSecondId): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSecondId): TKilogramMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TSquareMeterId): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMeters; const {%H-}ARight: TSecondId): TKilogramSquareMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TRadiansPerSecond): TKilogramSquareMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TRadiansPerSecond): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareSecondId): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m2/s2 ] = [ m/s2 ] * [ m ]
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKilogramId): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ sr/s2 ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradians; const {%H-}ARight: TSquareSecondId): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ rad2/s2 ] = [ m/s2 ] / [ m ]
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSteradiansPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSteradiansPerSquareSecond): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TSteradiansPerSquareSecond): TKilogramSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TKilogramSquareMeters): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramSquareMeters; const ARight: TSteradiansPerSquareSecond): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicMeters; const {%H-}ARight: TSecondId): TCubicMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPascals; const {%H-}ARight: TSecondId): TPascalSeconds;
begin
  result.Value := ALeft.Value;
end;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSecondId): TSquareMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtons; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value;
end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareKilogramId): TNewtonsPerSquareKilogram;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TMeterId): TSquareKilogramsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const {%H-}ARight: TSquareMeterId): TSquareKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareKilogramId): TSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value;
end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: double; const {%H-}ARight: TKelvinId): TReciprocalKelvins;
begin
  result.Value := ALeft;
end;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilograms; const {%H-}ARight: TKelvinId): TKilogramKelvins;
begin
  result.Value := ALeft.Value;
end;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TKelvinId): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value;
end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const {%H-}ARight: TKelvinId): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const {%H-}ARight: TKelvinId): TMeterKelvins;
begin
  result.Value := ALeft.Value;
end;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKelvins; const {%H-}ARight: TMeterId): TKelvinsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TMeterId): TWattsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TSquareMeterId): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TKelvinId): TWattsPerKelvin;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWattsPerMeter; const {%H-}ARight: TKelvinId): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TKelvinId): TSquareMeterKelvins;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TKelvinId): TWattsPerSquareMeterPerKelvin;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const {%H-}ARight: TQuarticKelvinId): TSquareMeterQuarticKelvins;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const {%H-}ARight: TQuarticKelvinId): TWattsPerQuarticKelvin;
begin
  result.Value := ALeft.Value;
end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWattsPerSquareMeter; const {%H-}ARight: TQuarticKelvinId): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const {%H-}ARight: TMoleId): TJoulesPerMole;
begin
  result.Value := ALeft.Value;
end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMoles; const {%H-}ARight: TKelvinId): TMoleKelvins;
begin
  result.Value := ALeft.Value;
end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoulesPerMole; const {%H-}ARight: TKelvinId): TJoulesPerMolePerKelvin;
begin
  result.Value := ALeft.Value;
end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const {%H-}ARight: TMeterId): TOhmMeters;
begin
  result.Value := ALeft.Value;
end;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVolts; const {%H-}ARight: TMeterId): TVoltsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ V/m ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TCoulombId): TVoltsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ V/m ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TMeterId): TCoulombsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const {%H-}ARight: TMeterId): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombs; const {%H-}ARight: TSquareMeterId): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const {%H-}ARight: TSquareCoulombId): TSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value;
end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareCoulombId): TNewtonsPerSquareCoulomb;
begin
  result.Value := ALeft.Value;
end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMeters; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVolts; const {%H-}ARight: TMeterId): TVoltMeters;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltMeters; const {%H-}ARight: TSecondId): TVoltMetersPerSecond;
begin
  result.Value := ALeft.Value;
end;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TFarads; const {%H-}ARight: TMeterId): TFaradsPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double;
begin
  result := ALeft.Value * ARight.Value;
end;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAmperes; const {%H-}ARight: TMeterId): TAmperesPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMeters; const {%H-}ARight: TAmpereId): TMetersPerAmpere;
begin
  result.Value := ALeft.Value;
end;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslas; const {%H-}ARight: TMeterId): TTeslaMeters;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TAmpereId): TTeslaMeters;
begin
  result.Value := ALeft.Value;
end;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslas; const {%H-}ARight: TAmpereId): TTeslasPerAmpere;
begin
  result.Value := ALeft.Value;
end;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrys; const {%H-}ARight: TMeterId): THenrysPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslaMeters; const {%H-}ARight: TAmpereId): THenrysPerMeter;
begin
  result.Value := ALeft.Value;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtons; const {%H-}ARight: TSquareAmpereId): THenrysPerMeter;
begin
  result.Value := ALeft.Value;
end;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TRadians; const {%H-}ARight: TMeterId): TRadiansPerMeter;
begin
  result.Value := ALeft.Value;
end;

{ Helpers }

function TBequerelHelper.From(const AQuantity: THertz): TBequerels;
begin
  result.Value := AQuantity.Value;
end;

function TGrayHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TGrays;
begin
  result.Value := AQuantity.Value;
end;

function TSievertHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TSieverts;
begin
  result.Value := AQuantity.Value;
end;

function TNewtonMeterHelper.From(const AQuantity: TJoules): TNewtonMeters;
begin
  result.Value := AQuantity.Value;
end;

function TNewtonMeterPerRadianHelper.From(const AQuantity: TJoulesPerRadian): TNewtonMetersPerRadian;
begin
  result.Value := AQuantity.Value;
end;

function TNewtonSecondHelper.From(const AQuantity: TKilogramMetersPerSecond): TNewtonSeconds;
begin
  result.Value := AQuantity.Value;
end;

function TJoulePerKilogramHelper.From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulesPerKilogram;
begin
  result.Value := AQuantity.Value;
end;

{ Power quantities }

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TMeters): TSquareMeters;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareMeters): TMeters;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function CubicPower(AQuantity: TMeters): TCubicMeters;
begin
  result.Value := Power(AQuantity.Value, 3);
end;

function CubicRoot(AQuantity: TCubicMeters): TMeters;
begin
  result.Value := Power(AQuantity.Value, 1/3);
end;

function SquarePower(AQuantity: TSquareMeters): TQuarticMeters;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TQuarticMeters): TSquareMeters;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function QuarticPower(AQuantity: TMeters): TQuarticMeters;
begin
  result.Value := Power(AQuantity.Value, 4);
end;

function QuarticRoot(AQuantity: TQuarticMeters): TMeters;
begin
  result.Value := Power(AQuantity.Value, 1/4);
end;

function QuinticPower(AQuantity: TMeters): TQuinticMeters;
begin
  result.Value := Power(AQuantity.Value, 5);
end;

function QuinticRoot(AQuantity: TQuinticMeters): TMeters;
begin
  result.Value := Power(AQuantity.Value, 1/5);
end;

function SquarePower(AQuantity: TCubicMeters): TSexticMeters;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSexticMeters): TCubicMeters;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function CubicPower(AQuantity: TSquareMeters): TSexticMeters;
begin
  result.Value := Power(AQuantity.Value, 3);
end;

function CubicRoot(AQuantity: TSexticMeters): TSquareMeters;
begin
  result.Value := Power(AQuantity.Value, 1/3);
end;

function SexticPower(AQuantity: TMeters): TSexticMeters;
begin
  result.Value := Power(AQuantity.Value, 6);
end;

function SexticRoot(AQuantity: TSexticMeters): TMeters;
begin
  result.Value := Power(AQuantity.Value, 1/6);
end;

function SquarePower(AQuantity: TAmperes): TSquareAmperes;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareAmperes): TAmperes;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TKelvins): TSquareKelvins;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
begin
  result.Value := Power(AQuantity.Value, 3);
end;

function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/3);
end;

function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
begin
  result.Value := Power(AQuantity.Value, 4);
end;

function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/4);
end;

function SquarePower(AQuantity: TRadians): TSteradians;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSteradians): TRadians;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: THertz): TSquareHertz;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareHertz): THertz;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TVolts): TSquareVolts;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareVolts): TVolts;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TRadiansPerSecond): TSteradiansPerSquareSecond;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSteradiansPerSquareSecond): TRadiansPerSecond;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

{ Trigonometric functions }

function Cos(const AQuantity: TRadians): double;
begin
  result := System.Cos(AQuantity.Value);
end;

function Sin(const AQuantity: TRadians): double;
begin
  result := System.Sin(AQuantity.Value);
end;

function Tan(const AQuantity: TRadians): double;
begin
  result := Math.Tan(AQuantity.Value);
end;

function Cotan(const AQuantity: TRadians): double;
begin
  result := Math.Cotan(AQuantity.Value);
end;

function Secant(const AQuantity: TRadians): double;
begin
  result := Math.Secant(AQuantity.Value);
end;

function Cosecant(const AQuantity: TRadians): double;
begin
  result := Math.Cosecant(AQuantity.Value);
end;

function ArcCos(const AValue: double): TRadians;
begin
  result.Value := Math.ArcCos(AValue);
end;

function ArcSin(const AValue: double): TRadians;
begin
  result.Value := Math.ArcSin(AValue);
end;

function ArcTan(const AValue: double): TRadians;
begin
  result.Value := System.ArcTan(AValue);
end;

function ArcTan2(const x, y: double): TRadians;
begin
  result.Value := Math.ArcTan2(x, y);
end;

end.
