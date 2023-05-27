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

uses
  SysUtils;

type
  { TUnit }

  TUnit = class
  public
    class function Name: string; virtual; abstract;
    class function Symbol: string; virtual; abstract;
  end;

  TFactoredUnit = class(TUnit)
  public
    class function Factor: double; virtual; abstract;
  end;

  { TQuantity }

  generic TQuantity<U: TUnit> = record
    type TSelf = specialize TQuantity<U>;
  public
    Value: double;
    function Abs: TSelf;
    function ToString: string;
    function ToString(Precision, Digits: longint): string;
    function ToVerboseString: string;
    function ToVerboseString(Precision, Digits: longint): string;
    class operator +  (const ALeft, ARight: TSelf): TSelf;
    class operator -  (const ALeft, ARight: TSelf): TSelf;
    class operator *  (const AFactor: double; const AValue: TSelf): TSelf;
    class operator *  (const AValue: TSelf; const AFactor: double): TSelf;
    class operator /  (const AValue: TSelf; const AFactor: double): TSelf;
    class operator /  (const ALeft, ARight: TSelf): double;
    class operator mod(const ALeft, ARight: TSelf): TSelf;
    class operator =  (const ALeft, ARight: TSelf): boolean;
    class operator <  (const ALeft, ARight: TSelf): boolean;
    class operator >  (const ALeft, ARight: TSelf): boolean;
    class operator <= (const ALeft, ARight: TSelf): boolean;
    class operator >= (const ALeft, ARight: TSelf): boolean;
  end;

  { TQuantityIdentifier }

  generic TQuantityIdentifier<U: TUnit> = record
    type TSelf = specialize TQuantityIdentifier<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;
    class operator *(const {%H-}TheUnit: TSelf; const AValue: double): TBaseQuantity;
  end;

  { TFactoredQuantityIdentifier }

  generic TFactoredQuantityIdentifier<BaseU: TUnit; U: TFactoredUnit> = record
    type TSelf = specialize TFactoredQuantityIdentifier<BaseU, U>;
    type TBaseQuantity = specialize TQuantity<BaseU>;
    type TBaseFactoredQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseFactoredQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;
    class operator *(const {%H-}TheUnit: TSelf; const AValue: double): TBaseQuantity;
  end;

{ Unit of Second }

type
  TSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSecondIdentifier = specialize TQuantityIdentifier<TSecondUnit>;
  TSeconds = specialize TQuantity<TSecondUnit>;

var
  s: TSecondIdentifier;

{ Unit of SquareSecond }

type
  TSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareSecondIdentifier = specialize TQuantityIdentifier<TSquareSecondUnit>;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;

var
  s2: TSquareSecondIdentifier;

{ Unit of Meter }

type
  TMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterIdentifier = specialize TQuantityIdentifier<TMeterUnit>;
  TMeters = specialize TQuantity<TMeterUnit>;

var
  m: TMeterIdentifier;

{ Unit of SquareMeter }

type
  TSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterIdentifier = specialize TQuantityIdentifier<TSquareMeterUnit>;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;

var
  m2: TSquareMeterIdentifier;

{ Unit of CubicMeter }

type
  TCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicMeterIdentifier = specialize TQuantityIdentifier<TCubicMeterUnit>;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;

var
  m3: TCubicMeterIdentifier;

{ Unit of QuarticMeter }

type
  TQuarticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuarticMeterIdentifier = specialize TQuantityIdentifier<TQuarticMeterUnit>;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;

var
  m4: TQuarticMeterIdentifier;

{ Unit of QuinticMeter }

type
  TQuinticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuinticMeterIdentifier = specialize TQuantityIdentifier<TQuinticMeterUnit>;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;

var
  m5: TQuinticMeterIdentifier;

{ Unit of SexticMeter }

type
  TSexticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSexticMeterIdentifier = specialize TQuantityIdentifier<TSexticMeterUnit>;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;

var
  m6: TSexticMeterIdentifier;

{ Unit of Kilogram }

type
  TKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramIdentifier = specialize TQuantityIdentifier<TKilogramUnit>;
  TKilograms = specialize TQuantity<TKilogramUnit>;

var
  kg: TKilogramIdentifier;

{ Unit of SquareKilogram }

type
  TSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramIdentifier = specialize TQuantityIdentifier<TSquareKilogramUnit>;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;

var
  kg2: TSquareKilogramIdentifier;

{ Unit of Ampere }

type
  TAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TAmpereIdentifier = specialize TQuantityIdentifier<TAmpereUnit>;
  TAmperes = specialize TQuantity<TAmpereUnit>;

var
  A: TAmpereIdentifier;

{ Unit of SquareAmpere }

type
  TSquareAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareAmpereIdentifier = specialize TQuantityIdentifier<TSquareAmpereUnit>;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;

var
  A2: TSquareAmpereIdentifier;

{ Unit of Kelvin }

type
  TKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKelvinIdentifier = specialize TQuantityIdentifier<TKelvinUnit>;
  TKelvins = specialize TQuantity<TKelvinUnit>;

var
  K: TKelvinIdentifier;

{ Unit of SquareKelvin }

type
  TSquareKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKelvinIdentifier = specialize TQuantityIdentifier<TSquareKelvinUnit>;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;

var
  K2: TSquareKelvinIdentifier;

{ Unit of CubicKelvin }

type
  TCubicKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicKelvinIdentifier = specialize TQuantityIdentifier<TCubicKelvinUnit>;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;

var
  K3: TCubicKelvinIdentifier;

{ Unit of QuarticKelvin }

type
  TQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuarticKelvinIdentifier = specialize TQuantityIdentifier<TQuarticKelvinUnit>;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;

{ Unit of Mole }

type
  TMoleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMoleIdentifier = specialize TQuantityIdentifier<TMoleUnit>;
  TMoles = specialize TQuantity<TMoleUnit>;

var
  mol: TMoleIdentifier;

{ Unit of Candela }

type
  TCandelaUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCandelaIdentifier = specialize TQuantityIdentifier<TCandelaUnit>;
  TCandelas = specialize TQuantity<TCandelaUnit>;

var
  cd: TCandelaIdentifier;

{ Unit of Radian }

type
  TRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianIdentifier = specialize TQuantityIdentifier<TRadianUnit>;
  TRadians = specialize TQuantity<TRadianUnit>;

var
  rad: TRadianIdentifier;

{ Unit of Steradian }

type
  TSteradianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSteradianIdentifier = specialize TQuantityIdentifier<TSteradianUnit>;
  TSteradians = specialize TQuantity<TSteradianUnit>;

var
  sr: TSteradianIdentifier;

{ Unit of Hertz }

type
  THertzUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THertzIdentifier = specialize TQuantityIdentifier<THertzUnit>;
  THertz = specialize TQuantity<THertzUnit>;

var
  Hz: THertzIdentifier;

{ Unit of SquareHertz }

type
  TSquareHertzUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareHertzIdentifier = specialize TQuantityIdentifier<TSquareHertzUnit>;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;

{ Unit of Newton }

type
  TNewtonUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonIdentifier = specialize TQuantityIdentifier<TNewtonUnit>;
  TNewtons = specialize TQuantity<TNewtonUnit>;

var
  N: TNewtonIdentifier;

{ Unit of Pascal }

type
  TPascalUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TPascalIdentifier = specialize TQuantityIdentifier<TPascalUnit>;
  TPascals = specialize TQuantity<TPascalUnit>;

var
  Pa: TPascalIdentifier;

{ Unit of Joule }

type
  TJouleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJouleIdentifier = specialize TQuantityIdentifier<TJouleUnit>;
  TJoules = specialize TQuantity<TJouleUnit>;

var
  J: TJouleIdentifier;

{ Unit of Watt }

type
  TWattUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattIdentifier = specialize TQuantityIdentifier<TWattUnit>;
  TWatts = specialize TQuantity<TWattUnit>;

var
  W: TWattIdentifier;

{ Unit of Coulomb }

type
  TCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombIdentifier = specialize TQuantityIdentifier<TCoulombUnit>;
  TCoulombs = specialize TQuantity<TCoulombUnit>;

var
  C: TCoulombIdentifier;

{ Unit of SquareCoulomb }

type
  TSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombIdentifier = specialize TQuantityIdentifier<TSquareCoulombUnit>;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;

var
  C2: TSquareCoulombIdentifier;

{ Unit of Volt }

type
  TVoltUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltIdentifier = specialize TQuantityIdentifier<TVoltUnit>;
  TVolts = specialize TQuantity<TVoltUnit>;

var
  V: TVoltIdentifier;

{ Unit of SquareVolt }

type
  TSquareVoltUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareVoltIdentifier = specialize TQuantityIdentifier<TSquareVoltUnit>;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;

var
  V2: TSquareVoltIdentifier;

{ Unit of Farad }

type
  TFaradUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TFaradIdentifier = specialize TQuantityIdentifier<TFaradUnit>;
  TFarads = specialize TQuantity<TFaradUnit>;

var
  F: TFaradIdentifier;

{ Unit of Ohm }

type
  TOhmUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TOhmIdentifier = specialize TQuantityIdentifier<TOhmUnit>;
  TOhms = specialize TQuantity<TOhmUnit>;

var
  ohm: TOhmIdentifier;

{ Unit of Siemens }

type
  TSiemensUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSiemensIdentifier = specialize TQuantityIdentifier<TSiemensUnit>;
  TSiemens = specialize TQuantity<TSiemensUnit>;

var
  siemens: TSiemensIdentifier;

{ Unit of Weber }

type
  TWeberUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWeberIdentifier = specialize TQuantityIdentifier<TWeberUnit>;
  TWebers = specialize TQuantity<TWeberUnit>;

var
  Wb: TWeberIdentifier;

{ Unit of Tesla }

type
  TTeslaUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaIdentifier = specialize TQuantityIdentifier<TTeslaUnit>;
  TTeslas = specialize TQuantity<TTeslaUnit>;

var
  T: TTeslaIdentifier;

{ Unit of Henry }

type
  THenryUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THenryIdentifier = specialize TQuantityIdentifier<THenryUnit>;
  THenrys = specialize TQuantity<THenryUnit>;

var
  H: THenryIdentifier;

{ Unit of Lumen }

type
  TLumenUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TLumenIdentifier = specialize TQuantityIdentifier<TLumenUnit>;
  TLumens = specialize TQuantity<TLumenUnit>;

var
  lm: TLumenIdentifier;

{ Unit of Lux }

type
  TLuxUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TLuxIdentifier = specialize TQuantityIdentifier<TLuxUnit>;
  TLux = specialize TQuantity<TLuxUnit>;

var
  lx: TLuxIdentifier;

{ Unit of Bequerel }

type
  TBequerelUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TBequerelIdentifier = specialize TQuantityIdentifier<TBequerelUnit>;
  TBequerels = specialize TQuantity<TBequerelUnit>;

var
  Bq: TBequerelIdentifier;

{ Unit of Gray }

type
  TGrayUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TGrayIdentifier = specialize TQuantityIdentifier<TGrayUnit>;
  TGrays = specialize TQuantity<TGrayUnit>;

var
  Gy: TGrayIdentifier;

{ Unit of Sievert }

type
  TSievertUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSievertIdentifier = specialize TQuantityIdentifier<TSievertUnit>;
  TSieverts = specialize TQuantity<TSievertUnit>;

var
  Sv: TSievertIdentifier;

{ Unit of Katal }

type
  TKatalUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKatalIdentifier = specialize TQuantityIdentifier<TKatalUnit>;
  TKatals = specialize TQuantity<TKatalUnit>;

var
  kat: TKatalIdentifier;

{ Unit of NewtonMeter }

type
  TNewtonMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterIdentifier = specialize TQuantityIdentifier<TNewtonMeterUnit>;
  TNewtonMeters = specialize TQuantity<TNewtonMeterUnit>;

{ Unit of JoulePerRadian }

type
  TJoulePerRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerRadianIdentifier = specialize TQuantityIdentifier<TJoulePerRadianUnit>;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;

{ Unit of NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterPerRadianIdentifier = specialize TQuantityIdentifier<TNewtonMeterPerRadianUnit>;
  TNewtonMetersPerRadian = specialize TQuantity<TNewtonMeterPerRadianUnit>;

{ Unit of MeterPerSecond }

type
  TMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerSecondIdentifier = specialize TQuantityIdentifier<TMeterPerSecondUnit>;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;

{ Unit of MeterPerSquareSecond }

type
  TMeterPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerSquareSecondIdentifier = specialize TQuantityIdentifier<TMeterPerSquareSecondUnit>;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;

{ Unit of RadianPerSecond }

type
  TRadianPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerSecondIdentifier = specialize TQuantityIdentifier<TRadianPerSecondUnit>;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;

{ Unit of RadianPerSquareSecond }

type
  TRadianPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerSquareSecondIdentifier = specialize TQuantityIdentifier<TRadianPerSquareSecondUnit>;
  TRadiansPerSquareSecond = specialize TQuantity<TRadianPerSquareSecondUnit>;

{ Unit of KilogramPerMeter }

type
  TKilogramPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerMeterIdentifier = specialize TQuantityIdentifier<TKilogramPerMeterUnit>;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;

{ Unit of KilogramPerSquareMeter }

type
  TKilogramPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerSquareMeterIdentifier = specialize TQuantityIdentifier<TKilogramPerSquareMeterUnit>;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;

{ Unit of KilogramPerCubicMeter }

type
  TKilogramPerCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerCubicMeterIdentifier = specialize TQuantityIdentifier<TKilogramPerCubicMeterUnit>;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;

{ Unit of NewtonPerCubicMeter }

type
  TNewtonPerCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerCubicMeterIdentifier = specialize TQuantityIdentifier<TNewtonPerCubicMeterUnit>;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;

{ Unit of NewtonPerMeter }

type
  TNewtonPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerMeterIdentifier = specialize TQuantityIdentifier<TNewtonPerMeterUnit>;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;

{ Unit of KilogramMeterPerSecond }

type
  TKilogramMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramMeterPerSecondIdentifier = specialize TQuantityIdentifier<TKilogramMeterPerSecondUnit>;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;

{ Unit of NewtonSecond }

type
  TNewtonSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSecondIdentifier = specialize TQuantityIdentifier<TNewtonSecondUnit>;
  TNewtonSeconds = specialize TQuantity<TNewtonSecondUnit>;

{ Unit of KilogramSquareMeter }

type
  TKilogramSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterIdentifier = specialize TQuantityIdentifier<TKilogramSquareMeterUnit>;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;

{ Unit of KilogramSquareMeterPerSecond }

type
  TKilogramSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterPerSecondIdentifier = specialize TQuantityIdentifier<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;

{ Unit of SquareMeterPerSquareSecond }

type
  TSquareMeterPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareSecondIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareSecondUnit>;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;

{ Unit of SteradianPerSquareSecond }

type
  TSteradianPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSteradianPerSquareSecondIdentifier = specialize TQuantityIdentifier<TSteradianPerSquareSecondUnit>;
  TSteradiansPerSquareSecond = specialize TQuantity<TSteradianPerSquareSecondUnit>;

{ Unit of CubicMeterPerSecond }

type
  TCubicMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicMeterPerSecondIdentifier = specialize TQuantityIdentifier<TCubicMeterPerSecondUnit>;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;

{ Unit of PascalSecond }

type
  TPascalSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TPascalSecondIdentifier = specialize TQuantityIdentifier<TPascalSecondUnit>;
  TPascalSeconds = specialize TQuantity<TPascalSecondUnit>;

{ Unit of SquareMeterPerSecond }

type
  TSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSecondIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSecondUnit>;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;

{ Unit of NewtonPerSquareKilogram }

type
  TNewtonPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TNewtonPerSquareKilogramUnit>;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;

{ Unit of SquareKilogramPerMeter }

type
  TSquareKilogramPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramPerMeterIdentifier = specialize TQuantityIdentifier<TSquareKilogramPerMeterUnit>;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;

{ Unit of SquareKilogramPerSquareMeter }

type
  TSquareKilogramPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramPerSquareMeterIdentifier = specialize TQuantityIdentifier<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareKilogram }

type
  TSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareKilogramUnit>;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;

{ Unit of NewtonSquareMeterPerSquareKilogram }

type
  TNewtonSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;

{ Unit of ReciprocalKelvin }

type
  TReciprocalKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TReciprocalKelvinIdentifier = specialize TQuantityIdentifier<TReciprocalKelvinUnit>;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;

{ Unit of KilogramKelvin }

type
  TKilogramKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramKelvinIdentifier = specialize TQuantityIdentifier<TKilogramKelvinUnit>;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;

{ Unit of JoulePerKelvin }

type
  TJoulePerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKelvinIdentifier = specialize TQuantityIdentifier<TJoulePerKelvinUnit>;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;

{ Unit of JoulePerKilogram }

type
  TJoulePerKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKilogramIdentifier = specialize TQuantityIdentifier<TJoulePerKilogramUnit>;
  TJoulesPerKilogram = specialize TQuantity<TJoulePerKilogramUnit>;

{ Unit of JoulePerKilogramPerKelvin }

type
  TJoulePerKilogramPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKilogramPerKelvinIdentifier = specialize TQuantityIdentifier<TJoulePerKilogramPerKelvinUnit>;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;

{ Unit of MeterKelvin }

type
  TMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterKelvinIdentifier = specialize TQuantityIdentifier<TMeterKelvinUnit>;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;

{ Unit of KelvinPerMeter }

type
  TKelvinPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKelvinPerMeterIdentifier = specialize TQuantityIdentifier<TKelvinPerMeterUnit>;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;

{ Unit of WattPerMeter }

type
  TWattPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterIdentifier = specialize TQuantityIdentifier<TWattPerMeterUnit>;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;

{ Unit of WattPerSquareMeter }

type
  TWattPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterUnit>;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;

{ Unit of WattPerKelvin }

type
  TWattPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerKelvinUnit>;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;

{ Unit of WattPerMeterPerKelvin }

type
  TWattPerMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerMeterPerKelvinUnit>;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;

{ Unit of SquareMeterKelvin }

type
  TSquareMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterKelvinIdentifier = specialize TQuantityIdentifier<TSquareMeterKelvinUnit>;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;

{ Unit of WattPerSquareMeterPerKelvin }

type
  TWattPerSquareMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterPerKelvinUnit>;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;

{ Unit of SquareMeterQuarticKelvin }

type
  TSquareMeterQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterQuarticKelvinIdentifier = specialize TQuantityIdentifier<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;

{ Unit of WattPerQuarticKelvin }

type
  TWattPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerQuarticKelvinIdentifier = specialize TQuantityIdentifier<TWattPerQuarticKelvinUnit>;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;

{ Unit of WattPerSquareMeterPerQuarticKelvin }

type
  TWattPerSquareMeterPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerQuarticKelvinIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;

{ Unit of JoulePerMole }

type
  TJoulePerMoleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMoleIdentifier = specialize TQuantityIdentifier<TJoulePerMoleUnit>;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;

{ Unit of MoleKelvin }

type
  TMoleKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMoleKelvinIdentifier = specialize TQuantityIdentifier<TMoleKelvinUnit>;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;

{ Unit of JoulePerMolePerKelvin }

type
  TJoulePerMolePerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMolePerKelvinIdentifier = specialize TQuantityIdentifier<TJoulePerMolePerKelvinUnit>;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;

{ Unit of OhmMeter }

type
  TOhmMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TOhmMeterIdentifier = specialize TQuantityIdentifier<TOhmMeterUnit>;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;

{ Unit of VoltPerMeter }

type
  TVoltPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltPerMeterIdentifier = specialize TQuantityIdentifier<TVoltPerMeterUnit>;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;

{ Unit of CoulombPerMeter }

type
  TCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerMeterIdentifier = specialize TQuantityIdentifier<TCoulombPerMeterUnit>;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;

{ Unit of SquareCoulombPerMeter }

type
  TSquareCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerMeterIdentifier = specialize TQuantityIdentifier<TSquareCoulombPerMeterUnit>;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;

{ Unit of CoulombPerSquareMeter }

type
  TCoulombPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerSquareMeterIdentifier = specialize TQuantityIdentifier<TCoulombPerSquareMeterUnit>;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareCoulomb }

type
  TSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareCoulombUnit>;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;

{ Unit of NewtonPerSquareCoulomb }

type
  TNewtonPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TNewtonPerSquareCoulombUnit>;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;

{ Unit of NewtonSquareMeter }

type
  TNewtonSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterUnit>;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;

{ Unit of NewtonSquareMeterPerSquareCoulomb }

type
  TNewtonSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;

{ Unit of VoltMeter }

type
  TVoltMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterIdentifier = specialize TQuantityIdentifier<TVoltMeterUnit>;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;

var
  Vm: TVoltMeterIdentifier;

{ Unit of VoltMeterPerSecond }

type
  TVoltMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterPerSecondIdentifier = specialize TQuantityIdentifier<TVoltMeterPerSecondUnit>;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;

{ Unit of FaradPerMeter }

type
  TFaradPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TFaradPerMeterIdentifier = specialize TQuantityIdentifier<TFaradPerMeterUnit>;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;

{ Unit of AmperePerMeter }

type
  TAmperePerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TAmperePerMeterIdentifier = specialize TQuantityIdentifier<TAmperePerMeterUnit>;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;

{ Unit of MeterPerAmpere }

type
  TMeterPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerAmpereIdentifier = specialize TQuantityIdentifier<TMeterPerAmpereUnit>;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;

{ Unit of TeslaMeter }

type
  TTeslaMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaMeterIdentifier = specialize TQuantityIdentifier<TTeslaMeterUnit>;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;

var
  Tm: TTeslaMeterIdentifier;

{ Unit of TeslaPerAmpere }

type
  TTeslaPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaPerAmpereIdentifier = specialize TQuantityIdentifier<TTeslaPerAmpereUnit>;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;

{ Unit of HenryPerMeter }

type
  THenryPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THenryPerMeterIdentifier = specialize TQuantityIdentifier<THenryPerMeterUnit>;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;

{ Unit of RadianPerMeter }

type
  TRadianPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerMeterIdentifier = specialize TQuantityIdentifier<TRadianPerMeterUnit>;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;

{ Unit of Day }

type
  TDayUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDayIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TDayUnit>;

var
  day: TDayIdentifier;

{ Unit of Hour }

type
  THourUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THourIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, THourUnit>;

var
  hr: THourIdentifier;

{ Unit of Minute }

type
  TMinuteUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMinuteIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TMinuteUnit>;

var
  minute: TMinuteIdentifier;

{ Unit of Decisecond }

type
  TDecisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecisecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TDecisecondUnit>;

var
  ds: TDecisecondIdentifier;

{ Unit of Centisecond }

type
  TCentisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentisecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TCentisecondUnit>;

var
  cs: TCentisecondIdentifier;

{ Unit of Millisecond }

type
  TMillisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillisecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TMillisecondUnit>;

var
  ms: TMillisecondIdentifier;

{ Unit of Microsecond }

type
  TMicrosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrosecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TMicrosecondUnit>;

var
  us: TMicrosecondIdentifier;

{ Unit of Nanosecond }

type
  TNanosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanosecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TNanosecondUnit>;

var
  ns: TNanosecondIdentifier;

{ Unit of Picosecond }

type
  TPicosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicosecondIdentifier = specialize TFactoredQuantityIdentifier<TSecondUnit, TPicosecondUnit>;

var
  ps: TPicosecondIdentifier;

{ Unit of Kilometer }

type
  TKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TKilometerUnit>;

var
  km: TKilometerIdentifier;

{ Unit of Hectometer }

type
  THectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectometerIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, THectometerUnit>;

var
  hm: THectometerIdentifier;

{ Unit of Decameter }

type
  TDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecameterIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TDecameterUnit>;

var
  dam: TDecameterIdentifier;

{ Unit of Decimeter }

type
  TDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TDecimeterUnit>;

var
  dm: TDecimeterIdentifier;

{ Unit of Centimeter }

type
  TCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TCentimeterUnit>;

var
  cm: TCentimeterIdentifier;

{ Unit of Millimeter }

type
  TMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TMillimeterUnit>;

var
  mm: TMillimeterIdentifier;

{ Unit of Micrometer }

type
  TMicrometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrometerIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TMicrometerUnit>;

var
  um: TMicrometerIdentifier;

{ Unit of Nanometer }

type
  TNanometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanometerIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TNanometerUnit>;

var
  nm: TNanometerIdentifier;

{ Unit of Picometer }

type
  TPicometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicometerIdentifier = specialize TFactoredQuantityIdentifier<TMeterUnit, TPicometerUnit>;

var
  pm: TPicometerIdentifier;

{ Unit of SquareKilometer }

type
  TSquareKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareKilometerIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareKilometerUnit>;

var
  km2: TSquareKilometerIdentifier;

{ Unit of SquareHectometer }

type
  TSquareHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareHectometerIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareHectometerUnit>;

var
  hm2: TSquareHectometerIdentifier;

{ Unit of SquareDecameter }

type
  TSquareDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareDecameterIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareDecameterUnit>;

var
  dam2: TSquareDecameterIdentifier;

{ Unit of SquareDecimeter }

type
  TSquareDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareDecimeterUnit>;

var
  dm2: TSquareDecimeterIdentifier;

{ Unit of SquareCentimeter }

type
  TSquareCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareCentimeterUnit>;

var
  cm2: TSquareCentimeterIdentifier;

{ Unit of SquareMillimeter }

type
  TSquareMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TSquareMeterUnit, TSquareMillimeterUnit>;

var
  mm2: TSquareMillimeterIdentifier;

{ Unit of CubicKilometer }

type
  TCubicKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicKilometerIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicKilometerUnit>;

var
  km3: TCubicKilometerIdentifier;

{ Unit of CubicHectometer }

type
  TCubicHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicHectometerIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicHectometerUnit>;

var
  hm3: TCubicHectometerIdentifier;

{ Unit of CubicDecameter }

type
  TCubicDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicDecameterIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicDecameterUnit>;

var
  dam3: TCubicDecameterIdentifier;

{ Unit of CubicDecimeter }

type
  TCubicDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicDecimeterUnit>;

var
  dm3: TCubicDecimeterIdentifier;

{ Unit of CubicCentimeter }

type
  TCubicCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicCentimeterUnit>;

var
  cm3: TCubicCentimeterIdentifier;

{ Unit of CubicMillimeter }

type
  TCubicMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TCubicMeterUnit, TCubicMillimeterUnit>;

var
  mm3: TCubicMillimeterIdentifier;

{ Unit of QuarticKilometer }

type
  TQuarticKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticKilometerIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticKilometerUnit>;

var
  km4: TQuarticKilometerIdentifier;

{ Unit of QuarticHectometer }

type
  TQuarticHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticHectometerIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticHectometerUnit>;

var
  hm4: TQuarticHectometerIdentifier;

{ Unit of QuarticDecameter }

type
  TQuarticDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticDecameterIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticDecameterUnit>;

var
  dam4: TQuarticDecameterIdentifier;

{ Unit of QuarticDecimeter }

type
  TQuarticDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticDecimeterUnit>;

var
  dm4: TQuarticDecimeterIdentifier;

{ Unit of QuarticCentimeter }

type
  TQuarticCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticCentimeterUnit>;

var
  cm4: TQuarticCentimeterIdentifier;

{ Unit of QuarticMillimeter }

type
  TQuarticMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TQuarticMeterUnit, TQuarticMillimeterUnit>;

var
  mm4: TQuarticMillimeterIdentifier;

{ Unit of Hectogram }

type
  THectogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectogramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, THectogramUnit>;

var
  hg: THectogramIdentifier;

{ Unit of Decagram }

type
  TDecagramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecagramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TDecagramUnit>;

var
  dag: TDecagramIdentifier;

{ Unit of Gram }

type
  TGramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TGramUnit>;

var
  g: TGramIdentifier;

{ Unit of Decigram }

type
  TDecigramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecigramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TDecigramUnit>;

var
  dg: TDecigramIdentifier;

{ Unit of Centigram }

type
  TCentigramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentigramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TCentigramUnit>;

var
  cg: TCentigramIdentifier;

{ Unit of Milligram }

type
  TMilligramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilligramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TMilligramUnit>;

var
  mg: TMilligramIdentifier;

{ Unit of Microgram }

type
  TMicrogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrogramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TMicrogramUnit>;

var
  ug: TMicrogramIdentifier;

{ Unit of Nanogram }

type
  TNanogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanogramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TNanogramUnit>;

var
  ng: TNanogramIdentifier;

{ Unit of Picogram }

type
  TPicogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicogramIdentifier = specialize TFactoredQuantityIdentifier<TKilogramUnit, TPicogramUnit>;

var
  pg: TPicogramIdentifier;

{ Unit of Kiloampere }

type
  TKiloampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKiloampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TKiloampereUnit>;

var
  kA: TKiloampereIdentifier;

{ Unit of Hectoampere }

type
  THectoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectoampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, THectoampereUnit>;

var
  hA: THectoampereIdentifier;

{ Unit of Decampere }

type
  TDecampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TDecampereUnit>;

var
  daA: TDecampereIdentifier;

{ Unit of Deciampere }

type
  TDeciampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDeciampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TDeciampereUnit>;

var
  dA: TDeciampereIdentifier;

{ Unit of Centiampere }

type
  TCentiampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentiampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TCentiampereUnit>;

var
  cA: TCentiampereIdentifier;

{ Unit of Milliampere }

type
  TMilliampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TMilliampereUnit>;

var
  mA: TMilliampereIdentifier;

{ Unit of Microampere }

type
  TMicroampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicroampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TMicroampereUnit>;

var
  uA: TMicroampereIdentifier;

{ Unit of Nanoampere }

type
  TNanoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanoampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TNanoampereUnit>;

var
  nA: TNanoampereIdentifier;

{ Unit of Picoampere }

type
  TPicoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicoampereIdentifier = specialize TFactoredQuantityIdentifier<TAmpereUnit, TPicoampereUnit>;

var
  picoampere: TPicoampereIdentifier;

{ Unit of SquareMilliampere }

type
  TSquareMilliampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareMilliampereIdentifier = specialize TFactoredQuantityIdentifier<TSquareAmpereUnit, TSquareMilliampereUnit>;

var
  mA2: TSquareMilliampereIdentifier;

{ Unit of Degree }

type
  TDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDegreeIdentifier = specialize TFactoredQuantityIdentifier<TRadianUnit, TDegreeUnit>;

var
  deg: TDegreeIdentifier;

{ Unit of Gigahertz }

type
  TGigahertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigahertzIdentifier = specialize TFactoredQuantityIdentifier<THertzUnit, TGigahertzUnit>;

var
  GHz: TGigahertzIdentifier;

{ Unit of Megahertz }

type
  TMegahertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegahertzIdentifier = specialize TFactoredQuantityIdentifier<THertzUnit, TMegahertzUnit>;

var
  MHz: TMegahertzIdentifier;

{ Unit of Kilohertz }

type
  TKilohertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilohertzIdentifier = specialize TFactoredQuantityIdentifier<THertzUnit, TKilohertzUnit>;

var
  kHz: TKilohertzIdentifier;

{ Unit of Giganewton }

type
  TGiganewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGiganewtonIdentifier = specialize TFactoredQuantityIdentifier<TNewtonUnit, TGiganewtonUnit>;

var
  GN: TGiganewtonIdentifier;

{ Unit of Meganewton }

type
  TMeganewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMeganewtonIdentifier = specialize TFactoredQuantityIdentifier<TNewtonUnit, TMeganewtonUnit>;

var
  MN: TMeganewtonIdentifier;

{ Unit of Kilonewton }

type
  TKilonewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilonewtonIdentifier = specialize TFactoredQuantityIdentifier<TNewtonUnit, TKilonewtonUnit>;

var
  kN: TKilonewtonIdentifier;

{ Unit of Gigapascal }

type
  TGigapascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigapascalIdentifier = specialize TFactoredQuantityIdentifier<TPascalUnit, TGigapascalUnit>;

var
  GPa: TGigapascalIdentifier;

{ Unit of Megapascal }

type
  TMegapascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegapascalIdentifier = specialize TFactoredQuantityIdentifier<TPascalUnit, TMegapascalUnit>;

var
  MPa: TMegapascalIdentifier;

{ Unit of Kilopascal }

type
  TKilopascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilopascalIdentifier = specialize TFactoredQuantityIdentifier<TPascalUnit, TKilopascalUnit>;

var
  kPa: TKilopascalIdentifier;

{ Unit of Gigajoule }

type
  TGigajouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigajouleIdentifier = specialize TFactoredQuantityIdentifier<TJouleUnit, TGigajouleUnit>;

var
  GJ: TGigajouleIdentifier;

{ Unit of Megajoule }

type
  TMegajouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegajouleIdentifier = specialize TFactoredQuantityIdentifier<TJouleUnit, TMegajouleUnit>;

var
  MJ: TMegajouleIdentifier;

{ Unit of Kilojoule }

type
  TKilojouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilojouleIdentifier = specialize TFactoredQuantityIdentifier<TJouleUnit, TKilojouleUnit>;

var
  kJ: TKilojouleIdentifier;

{ Unit of Gigawatt }

type
  TGigawattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigawattIdentifier = specialize TFactoredQuantityIdentifier<TWattUnit, TGigawattUnit>;

var
  GW: TGigawattIdentifier;

{ Unit of Megawatt }

type
  TMegawattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegawattIdentifier = specialize TFactoredQuantityIdentifier<TWattUnit, TMegawattUnit>;

var
  megawatt: TMegawattIdentifier;

{ Unit of Kilowatt }

type
  TKilowattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilowattIdentifier = specialize TFactoredQuantityIdentifier<TWattUnit, TKilowattUnit>;

var
  kW: TKilowattIdentifier;

{ Unit of Milliwatt }

type
  TMilliwattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliwattIdentifier = specialize TFactoredQuantityIdentifier<TWattUnit, TMilliwattUnit>;

var
  mW: TMilliwattIdentifier;

{ Unit of Gigacoulomb }

type
  TGigacoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigacoulombIdentifier = specialize TFactoredQuantityIdentifier<TCoulombUnit, TGigacoulombUnit>;

var
  GC: TGigacoulombIdentifier;

{ Unit of Megacoulomb }

type
  TMegacoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegacoulombIdentifier = specialize TFactoredQuantityIdentifier<TCoulombUnit, TMegacoulombUnit>;

var
  megacoulomb: TMegacoulombIdentifier;

{ Unit of Kilocoulomb }

type
  TKilocoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilocoulombIdentifier = specialize TFactoredQuantityIdentifier<TCoulombUnit, TKilocoulombUnit>;

var
  kC: TKilocoulombIdentifier;

{ Unit of Millicoulomb }

type
  TMillicoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillicoulombIdentifier = specialize TFactoredQuantityIdentifier<TCoulombUnit, TMillicoulombUnit>;

var
  mC: TMillicoulombIdentifier;

{ Unit of Gigavolt }

type
  TGigavoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigavoltIdentifier = specialize TFactoredQuantityIdentifier<TVoltUnit, TGigavoltUnit>;

var
  GV: TGigavoltIdentifier;

{ Unit of Megavolt }

type
  TMegavoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegavoltIdentifier = specialize TFactoredQuantityIdentifier<TVoltUnit, TMegavoltUnit>;

var
  megavolt: TMegavoltIdentifier;

{ Unit of Kilovolt }

type
  TKilovoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilovoltIdentifier = specialize TFactoredQuantityIdentifier<TVoltUnit, TKilovoltUnit>;

var
  kV: TKilovoltIdentifier;

{ Unit of Millivolt }

type
  TMillivoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillivoltIdentifier = specialize TFactoredQuantityIdentifier<TVoltUnit, TMillivoltUnit>;

var
  mV: TMillivoltIdentifier;

{ Unit of Gigafarad }

type
  TGigafaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigafaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TGigafaradUnit>;

var
  GF: TGigafaradIdentifier;

{ Unit of Megafarad }

type
  TMegafaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegafaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TMegafaradUnit>;

var
  megafarad: TMegafaradIdentifier;

{ Unit of Kilofarad }

type
  TKilofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilofaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TKilofaradUnit>;

var
  kF: TKilofaradIdentifier;

{ Unit of Millifarad }

type
  TMillifaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillifaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TMillifaradUnit>;

var
  mF: TMillifaradIdentifier;

{ Unit of Microfarad }

type
  TMicrofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrofaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TMicrofaradUnit>;

var
  uF: TMicrofaradIdentifier;

{ Unit of Nanofarad }

type
  TNanofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanofaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TNanofaradUnit>;

var
  nF: TNanofaradIdentifier;

{ Unit of Picofarad }

type
  TPicofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicofaradIdentifier = specialize TFactoredQuantityIdentifier<TFaradUnit, TPicofaradUnit>;

var
  pF: TPicofaradIdentifier;

{ Unit of Gigaohm }

type
  TGigaohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigaohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TGigaohmUnit>;

var
  gigaohm: TGigaohmIdentifier;

{ Unit of Megaohm }

type
  TMegaohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegaohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TMegaohmUnit>;

var
  megaohm: TMegaohmIdentifier;

{ Unit of Kiloohm }

type
  TKiloohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKiloohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TKiloohmUnit>;

var
  kiloohm: TKiloohmIdentifier;

{ Unit of Milliohm }

type
  TMilliohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TMilliohmUnit>;

var
  milliohm: TMilliohmIdentifier;

{ Unit of Microohm }

type
  TMicroohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicroohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TMicroohmUnit>;

var
  microohm: TMicroohmIdentifier;

{ Unit of Nanoohm }

type
  TNanoohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanoohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TNanoohmUnit>;

var
  nanoohm: TNanoohmIdentifier;

{ Unit of Picoohm }

type
  TPicoohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicoohmIdentifier = specialize TFactoredQuantityIdentifier<TOhmUnit, TPicoohmUnit>;

var
  picoohm: TPicoohmIdentifier;

{ Unit of Milligray }

type
  TMilligrayUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilligrayIdentifier = specialize TFactoredQuantityIdentifier<TGrayUnit, TMilligrayUnit>;

var
  mGy: TMilligrayIdentifier;

{ Unit of MilliSievert }

type
  TMilliSievertUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliSievertIdentifier = specialize TFactoredQuantityIdentifier<TSievertUnit, TMilliSievertUnit>;

var
  mSv: TMilliSievertIdentifier;

{ Unit of JoulePerDegree }

type
  TJoulePerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TJoulePerDegreeIdentifier = specialize TFactoredQuantityIdentifier<TJoulePerRadianUnit, TJoulePerDegreeUnit>;

{ Unit of NewtonMeterPerDegree }

type
  TNewtonMeterPerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMeterPerDegreeIdentifier = specialize TFactoredQuantityIdentifier<TJoulePerRadianUnit, TNewtonMeterPerDegreeUnit>;

{ Unit of KilometerPerHour }

type
  TKilometerPerHourUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerPerHourIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSecondUnit, TKilometerPerHourUnit>;

{ Unit of DecimeterPerSecond }

type
  TDecimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterPerSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSecondUnit, TDecimeterPerSecondUnit>;

{ Unit of CentimeterPerSecond }

type
  TCentimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterPerSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSecondUnit, TCentimeterPerSecondUnit>;

{ Unit of MillimeterPerSecond }

type
  TMillimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterPerSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSecondUnit, TMillimeterPerSecondUnit>;

{ Unit of KilometerPerHourPerSecond }

type
  TKilometerPerHourPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerPerHourPerSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSquareSecondUnit, TKilometerPerHourPerSecondUnit>;

{ Unit of DecimeterPerSquareSecond }

type
  TDecimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterPerSquareSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSquareSecondUnit, TDecimeterPerSquareSecondUnit>;

{ Unit of CentimeterPerSquareSecond }

type
  TCentimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterPerSquareSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSquareSecondUnit, TCentimeterPerSquareSecondUnit>;

{ Unit of MillimeterPerSquareSecond }

type
  TMillimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterPerSquareSecondIdentifier = specialize TFactoredQuantityIdentifier<TMeterPerSquareSecondUnit, TMillimeterPerSquareSecondUnit>;

{ Unit of KilogramPerCubicMillimeter }

type
  TKilogramPerCubicMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicMillimeterUnit>;

{ Unit of KilogramPerCubicCentimeter }

type
  TKilogramPerCubicCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicCentimeterUnit>;

{ Unit of KilogramPerCubicDecimeter }

type
  TKilogramPerCubicDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicDecimeterUnit>;

{ Unit of HectogramPerCubicMeter }

type
  THectogramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectogramPerCubicMeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, THectogramPerCubicMeterUnit>;

{ Unit of DecagramPerCubicMeter }

type
  TDecagramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecagramPerCubicMeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, TDecagramPerCubicMeterUnit>;

{ Unit of GramPerCubicMeter }

type
  TGramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGramPerCubicMeterIdentifier = specialize TFactoredQuantityIdentifier<TKilogramPerCubicMeterUnit, TGramPerCubicMeterUnit>;

{ Combining units }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TSecondIdentifier; inline;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterIdentifier; inline;

// main definition [ m3 ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareMeterIdentifier; inline;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TQuarticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TQuarticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCubicMeterIdentifier; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TQuarticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareMeterIdentifier; inline;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TQuinticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TQuinticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TQuarticMeterIdentifier; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TQuinticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TQuinticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCubicMeterIdentifier; inline;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSexticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TQuinticMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TQuinticMeterIdentifier): TSexticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TQuinticMeterIdentifier; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSexticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TSexticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TQuarticMeterIdentifier; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TSexticMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TCubicMeterIdentifier; inline;

// main definition [ kg2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramIdentifier; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TSquareAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TAmpereIdentifier; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TKelvinIdentifier; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TCubicKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TCubicKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareKelvinIdentifier; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TSquareKelvinIdentifier; inline;

//
operator *(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TCubicKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TCubicKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TCubicKelvinIdentifier; inline;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianIdentifier): TSteradianIdentifier; inline;
operator /(const {%H-}ALeft: TSteradianIdentifier; const {%H-}ARight: TRadianIdentifier): TRadianIdentifier; inline;

// main definition [ Hz ] = 1 / [ s ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSecondIdentifier): THertzIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: THertzIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TSecondIdentifier): double; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: THertzIdentifier): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: THertzIdentifier): TSquareHertzIdentifier; inline;
operator /(const {%H-}ALeft: TSquareHertzIdentifier; const {%H-}ARight: THertzIdentifier): THertzIdentifier; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TNewtonIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TKilogramIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TNewtonIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TKilogramIdentifier; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TPascalIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TPascalIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TNewtonIdentifier; inline;

// main definition [ J ] = [ N ] * [ m ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonIdentifier; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TPascalIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TPascalIdentifier; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSecondIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TWattIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSecondIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TWattIdentifier): TJouleIdentifier; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TJouleIdentifier): TRadianPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TJouleIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TJouleIdentifier; inline;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: TOhmIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSquareAmpereIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TSquareAmpereIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TOhmIdentifier): TSquareAmpereIdentifier; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TNewtonIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TNewtonIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TNewtonIdentifier; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TAmpereIdentifier): TCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TSecondIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TSecondIdentifier): TCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TAmpereIdentifier): TSecondIdentifier; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TCoulombIdentifier; inline;

// main definition [ V ] = [ W ] / [ A ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TAmpereIdentifier): TVoltIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TVoltIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TAmpereIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TVoltIdentifier): TWattIdentifier; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TCoulombIdentifier): TVoltIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TVoltIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TCoulombIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TVoltIdentifier): TJouleIdentifier; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TVoltIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TVoltIdentifier): TVoltIdentifier; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TOhmIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TWattIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TWattIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TOhmIdentifier): TWattIdentifier; inline;

// main definition [ F ] = [ C ] / [ V ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TVoltIdentifier): TFaradIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TFaradIdentifier): TVoltIdentifier; inline;
operator *(const {%H-}ALeft: TFaradIdentifier; const {%H-}ARight: TVoltIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TFaradIdentifier): TCoulombIdentifier; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TJouleIdentifier): TFaradIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TFaradIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TFaradIdentifier; const {%H-}ARight: TJouleIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TFaradIdentifier): TSquareCoulombIdentifier; inline;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TAmpereIdentifier): TOhmIdentifier; inline;
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TOhmIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TAmpereIdentifier): TVoltIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TOhmIdentifier): TVoltIdentifier; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TFaradIdentifier): TOhmIdentifier; inline;
operator /(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TOhmIdentifier): TFaradIdentifier; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TFaradIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TFaradIdentifier; const {%H-}ARight: TOhmIdentifier): TSecondIdentifier; inline;

// main definition [ S ] = 1 / [ Ω ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TOhmIdentifier): TSiemensIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSiemensIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TSiemensIdentifier; const {%H-}ARight: TOhmIdentifier): double; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TSiemensIdentifier): double; inline;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TSecondIdentifier): TWeberIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TVoltIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TVoltIdentifier): TWeberIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TSecondIdentifier): TVoltIdentifier; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TTeslaIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TTeslaIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWeberIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TWeberIdentifier; inline;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TAmpereIdentifier): THenryIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: THenryIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: TAmpereIdentifier): TWeberIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: THenryIdentifier): TWeberIdentifier; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TSecondIdentifier): THenryIdentifier; inline;
operator /(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: TOhmIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TOhmIdentifier): THenryIdentifier; inline;
operator /(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: TSecondIdentifier): TOhmIdentifier; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: THertzIdentifier): THenryIdentifier; inline;
operator /(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: THenryIdentifier): THertzIdentifier; inline;
operator *(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: THertzIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: THenryIdentifier): TOhmIdentifier; inline;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const {%H-}ALeft: TCandelaIdentifier; const {%H-}ARight: TSteradianIdentifier): TLumenIdentifier; inline;
operator /(const {%H-}ALeft: TLumenIdentifier; const {%H-}ARight: TCandelaIdentifier): TSteradianIdentifier; inline;
operator *(const {%H-}ALeft: TSteradianIdentifier; const {%H-}ARight: TCandelaIdentifier): TLumenIdentifier; inline;
operator /(const {%H-}ALeft: TLumenIdentifier; const {%H-}ARight: TSteradianIdentifier): TCandelaIdentifier; inline;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const {%H-}ALeft: TLumenIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TLuxIdentifier; inline;
operator /(const {%H-}ALeft: TLumenIdentifier; const {%H-}ARight: TLuxIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TLuxIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TLumenIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TLuxIdentifier): TLumenIdentifier; inline;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TSecondIdentifier): TKatalIdentifier; inline;
operator /(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TKatalIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKatalIdentifier; const {%H-}ARight: TSecondIdentifier): TMoleIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TKatalIdentifier): TMoleIdentifier; inline;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TRadianIdentifier): TJoulePerRadianIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerRadianIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerRadianIdentifier; const {%H-}ARight: TRadianIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TJoulePerRadianIdentifier): TJouleIdentifier; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TMeterIdentifier; inline;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TMeterPerSecondIdentifier; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TMeterIdentifier; inline;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const {%H-}ALeft: TSteradianPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TSteradianPerSquareSecondIdentifier; inline;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TRadianIdentifier; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TRadianPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TMeterPerSecondIdentifier; inline;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TRadianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TRadianPerSquareSecondIdentifier): TRadianIdentifier; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TRadianPerSquareSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSquareSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TRadianPerSquareSecondIdentifier): TRadianPerSecondIdentifier; inline;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TMeterIdentifier): TKilogramPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TKilogramPerMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TKilogramPerSquareMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TKilogramPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TPascalIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TPascalIdentifier; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier; inline;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TNewtonPerMeterIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonPerMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonPerMeterIdentifier): TJouleIdentifier; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TNewtonPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TPascalIdentifier; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TKilogramIdentifier; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSecondIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TNewtonIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TNewtonIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TNewtonIdentifier; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TKilogramIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TKilogramSquareMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TKilogramSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterPerSecondIdentifier): TKilogramSquareMeterIdentifier; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TRadianPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TKilogramSquareMeterIdentifier; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TSquareMeterIdentifier; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TMeterPerSecondIdentifier; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TJouleIdentifier; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TPascalIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TPascalIdentifier; inline;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const {%H-}ALeft: TSteradianIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSteradianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSteradianIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSteradianPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSteradianIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TSteradianIdentifier; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TSteradianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSteradianPerSquareSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TRadianPerSecondIdentifier; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TSteradianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TKilogramSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSteradianPerSquareSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSteradianPerSquareSecondIdentifier): TJouleIdentifier; inline;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TCubicMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TCubicMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TCubicMeterPerSecondIdentifier): TCubicMeterIdentifier; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TCubicMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterPerSecondIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCubicMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TSquareMeterIdentifier; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSecondIdentifier): TPascalSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TPascalIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TPascalIdentifier): TPascalSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TPascalIdentifier; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TSquareMeterPerSecondIdentifier): TSquareMeterIdentifier; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TSquareMeterPerSecondIdentifier): TKilogramPerCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TPascalSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TSquareMeterPerSecondIdentifier): TPascalSecondIdentifier; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareKilogramPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TSquareKilogramIdentifier; inline;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareKilogramPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TSquareKilogramIdentifier; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareMeterIdentifier; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterPerSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterIdentifier; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonPerSquareKilogramIdentifier; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TJouleIdentifier; inline;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TKelvinIdentifier): TReciprocalKelvinIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TReciprocalKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): double; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TReciprocalKelvinIdentifier): double; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKelvinIdentifier): TKilogramKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TKilogramIdentifier; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TJoulePerKelvinIdentifier): TJouleIdentifier; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKilogramKelvinIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TJouleIdentifier; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TJoulePerKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TJoulePerKelvinIdentifier; inline;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMeterKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMeterKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TMeterIdentifier; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TKelvinIdentifier; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TMeterIdentifier): TWattPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TWattPerMeterIdentifier): TWattIdentifier; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterIdentifier): TWattIdentifier; inline;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TWattPerKelvinIdentifier): TWattIdentifier; inline;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TMeterKelvinIdentifier): TWattPerMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TMeterKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinIdentifier; const {%H-}ARight: TMeterKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TMeterKelvinIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TWattIdentifier; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const {%H-}ALeft: TWattPerMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerMeterIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TWattPerMeterIdentifier; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TWattPerMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TWattPerKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TWattPerKelvinIdentifier; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TWattPerMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TKelvinPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TWattPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinPerMeterIdentifier; const {%H-}ARight: TWattPerMeterPerKelvinIdentifier): TWattPerSquareMeterIdentifier; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareMeterIdentifier; inline;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSquareMeterKelvinIdentifier): TWattPerSquareMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TSquareMeterKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinIdentifier; const {%H-}ARight: TSquareMeterKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattIdentifier; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerSquareMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattPerSquareMeterIdentifier; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerSquareMeterPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattPerKelvinIdentifier; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareMeterQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TSquareMeterIdentifier; inline;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerQuarticKelvinIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TWattPerQuarticKelvinIdentifier): TWattIdentifier; inline;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSquareMeterQuarticKelvinIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterQuarticKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterQuarticKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattIdentifier; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattPerSquareMeterIdentifier; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattPerQuarticKelvinIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TMoleIdentifier): TJoulePerMoleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerMoleIdentifier): TMoleIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerMoleIdentifier; const {%H-}ARight: TMoleIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TJoulePerMoleIdentifier): TJouleIdentifier; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TKelvinIdentifier): TMoleKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMoleKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TMoleKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMoleKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TMoleIdentifier; inline;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TMoleKelvinIdentifier): TJoulePerMolePerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TMoleKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinIdentifier; const {%H-}ARight: TMoleKelvinIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TMoleKelvinIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TJouleIdentifier; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TJoulePerMolePerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TMoleIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TJoulePerKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TJoulePerKelvinIdentifier; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const {%H-}ALeft: TJoulePerMoleIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerMolePerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJoulePerMoleIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerMoleIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TJoulePerMolePerKelvinIdentifier): TJoulePerMoleIdentifier; inline;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TMeterIdentifier): TOhmMeterIdentifier; inline;
operator /(const {%H-}ALeft: TOhmMeterIdentifier; const {%H-}ARight: TOhmIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TOhmIdentifier): TOhmMeterIdentifier; inline;
operator /(const {%H-}ALeft: TOhmMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TOhmIdentifier; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TVoltIdentifier; inline;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TCoulombIdentifier): TVoltPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TNewtonIdentifier; inline;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TVoltPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TTeslaIdentifier): TVoltPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TTeslaIdentifier; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier; inline;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareCoulombPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TSquareCoulombIdentifier; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const {%H-}ALeft: TCoulombPerMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TSquareCoulombPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerMeterIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TSquareCoulombPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TCoulombPerMeterIdentifier; inline;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TCoulombIdentifier; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const {%H-}ALeft: TCoulombPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombPerMeterIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TCoulombPerMeterIdentifier; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareMeterIdentifier; inline;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterPerSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonSquareMeterIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonPerSquareCoulombIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TVoltPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TVoltPerMeterIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TJouleIdentifier; inline;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TVoltIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TVoltIdentifier): TVoltMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltIdentifier; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TVoltMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TVoltMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TVoltPerMeterIdentifier; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TVoltMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TVoltMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TVoltMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TVoltMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TVoltMeterPerSecondIdentifier): TVoltMeterIdentifier; inline;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const {%H-}ALeft: TFaradIdentifier; const {%H-}ARight: TMeterIdentifier): TFaradPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TFaradIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TFaradPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TFaradIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TFaradIdentifier; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TVoltMeterIdentifier): TFaradPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TVoltMeterIdentifier; inline;
operator *(const {%H-}ALeft: TFaradPerMeterIdentifier; const {%H-}ARight: TVoltMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TCoulombIdentifier; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TFaradPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TVoltPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TFaradPerMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TFaradPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TFaradPerMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TFaradPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): double; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TFaradPerMeterIdentifier): double; inline;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): TAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TAmpereIdentifier; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TMeterIdentifier; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TTeslaMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaIdentifier; inline;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TTeslaMeterIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TTeslaMeterIdentifier): TNewtonIdentifier; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TTeslaIdentifier; inline;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: TMeterIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: THenryIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): THenryIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): THenryIdentifier; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TTeslaMeterIdentifier; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const {%H-}ALeft: TTeslaPerAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaPerAmpereIdentifier; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterPerAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereIdentifier; const {%H-}ARight: TTeslaIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TTeslaIdentifier; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TAmperePerMeterIdentifier; inline;
operator *(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TTeslaIdentifier; inline;
operator *(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TTeslaIdentifier; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareAmpereIdentifier): THenryPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TSquareAmpereIdentifier; inline;
operator *(const {%H-}ALeft: THenryPerMeterIdentifier; const {%H-}ARight: TSquareAmpereIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: THenryPerMeterIdentifier): TNewtonIdentifier; inline;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TMeterIdentifier): TRadianPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TRadianPerMeterIdentifier): TRadianIdentifier; inline;

// main definition [ J/deg ] = [ J ] / [ deg ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TDegreeIdentifier): TJoulePerDegreeIdentifier; inline;

// main definition [ km/h ] = [ km ] / [ h ]
operator /(const {%H-}ALeft: TKilometerIdentifier; const {%H-}ARight: THourIdentifier): TKilometerPerHourIdentifier; inline;

// main definition [ dm/s ] = [ dm ] / [ s ]
operator /(const {%H-}ALeft: TDecimeterIdentifier; const {%H-}ARight: TSecondIdentifier): TDecimeterPerSecondIdentifier; inline;

// main definition [ cm/s ] = [ cm ] / [ s ]
operator /(const {%H-}ALeft: TCentimeterIdentifier; const {%H-}ARight: TSecondIdentifier): TCentimeterPerSecondIdentifier; inline;

// main definition [ mm/s ] = [ mm ] / [ s ]
operator /(const {%H-}ALeft: TMillimeterIdentifier; const {%H-}ARight: TSecondIdentifier): TMillimeterPerSecondIdentifier; inline;

// main definition [ km/h/s ] = [ km/h ] / [ s ]
operator /(const {%H-}ALeft: TKilometerPerHourIdentifier; const {%H-}ARight: TSecondIdentifier): TKilometerPerHourPerSecondIdentifier; inline;

// main definition [ dm/s2 ] = [ dm ] / [ s2 ]
operator /(const {%H-}ALeft: TDecimeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TDecimeterPerSquareSecondIdentifier; inline;

// main definition [ cm/s2 ] = [ cm ] / [ s2 ]
operator /(const {%H-}ALeft: TCentimeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TCentimeterPerSquareSecondIdentifier; inline;

// main definition [ mm/s2 ] = [ mm ] / [ s2 ]
operator /(const {%H-}ALeft: TMillimeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TMillimeterPerSquareSecondIdentifier; inline;

//
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TCubicMillimeterIdentifier): TKilogramPerCubicMillimeterIdentifier; inline;

//
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TCubicCentimeterIdentifier): TKilogramPerCubicCentimeterIdentifier; inline;

//
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TCubicDecimeterIdentifier): TKilogramPerCubicDecimeterIdentifier; inline;

//
operator /(const {%H-}ALeft: THectogramIdentifier; const {%H-}ARight: TCubicMeterIdentifier): THectogramPerCubicMeterIdentifier; inline;

//
operator /(const {%H-}ALeft: TDecagramIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TDecagramPerCubicMeterIdentifier; inline;

//
operator /(const {%H-}ALeft: TGramIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TGramPerCubicMeterIdentifier; inline;

{ Combining quantities }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters; inline;

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

// main definition [ kg2 ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

//
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJoules; const ARight: TRadiansPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TRadiansPerSecond): TJoules; inline;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms; inline;
operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads; inline;
operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds; inline;
operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds; inline;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys; inline;
operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes; inline;
operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers; inline;
operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers; inline;

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

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters; inline;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSteradiansPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSteradiansPerSquareSecond): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSteradiansPerSquareSecond; inline;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): TRadiansPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TRadiansPerSecond): TMeters; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerSecond): TMetersPerSecond; inline;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSquareSecond): TRadiansPerSecond; inline;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;

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

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;

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

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeters; const ARight: TRadiansPerSecond): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TRadiansPerSecond): TKilogramSquareMeters; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSteradiansPerSquareSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TKilogramSquareMeters): TJoules; inline;
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSteradiansPerSquareSecond): TJoules; inline;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;

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

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;

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

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;

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

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;

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

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;

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

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;

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

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;

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

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;

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

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter; inline;
operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys; inline;
operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters; inline;

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

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;

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

{ Equivalences }

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

{ Helpers }

{ Helper for Bequerel }

type
  TBequerelHelper = record helper for TBequerelIdentifier
    function From(const AQuantity: THertz): TBequerels;
  end;

{ Helper for Gray }

type
  TGrayHelper = record helper for TGrayIdentifier
    function From(const AQuantity: TSquareMetersPerSquareSecond): TGrays;
  end;

{ Helper for Sievert }

type
  TSievertHelper = record helper for TSievertIdentifier
    function From(const AQuantity: TSquareMetersPerSquareSecond): TSieverts;
  end;

{ Helper for NewtonMeter }

type
  TNewtonMeterHelper = record helper for TNewtonMeterIdentifier
    function From(const AQuantity: TJoules): TNewtonMeters;
  end;

{ Helper for NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianIdentifier
    function From(const AQuantity: TJoulesPerRadian): TNewtonMetersPerRadian;
  end;

{ Helper for NewtonSecond }

type
  TNewtonSecondHelper = record helper for TNewtonSecondIdentifier
    function From(const AQuantity: TKilogramMetersPerSecond): TNewtonSeconds;
  end;

{ Helper for JoulePerKilogram }

type
  TJoulePerKilogramHelper = record helper for TJoulePerKilogramIdentifier
    function From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulesPerKilogram;
  end;

implementation

uses
  Math;

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

class operator TQuantity.*(const AFactor: double; const AValue: TSelf): TSelf;
begin
  result.Value := AFactor * AValue.Value;
end;

class operator TQuantity.*(const AValue: TSelf; const AFactor: double): TSelf;
begin
  result.Value := AValue.Value * AFactor;
end;

class operator TQuantity./(const AValue: TSelf; const AFactor: double): TSelf;
begin
  result.Value := AValue.Value / AFactor;
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

{ TQuantityIdentifier }

class function TQuantityIdentifier.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.Value := AQuantity.Value;
end;

class operator TQuantityIdentifier.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue;
end;

class operator TQuantityIdentifier.*(const TheUnit: TSelf; const AValue: double): TBaseQuantity;
begin
  result.Value := AValue;
end;

{ TFactoredQuantityIdentifier }

class function TFactoredQuantityIdentifier.From(const AQuantity: TBaseQuantity): TBaseFactoredQuantity;
begin
  result.Value := AQuantity.Value / U.Factor;
end;

class operator TFactoredQuantityIdentifier.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue * U.Factor;
end;

class operator TFactoredQuantityIdentifier.*(const TheUnit: TSelf; const AValue: double): TBaseQuantity;
begin
  result.Value := U.Factor * AValue;
end;
{ Unit of TSecondUnit }

class function TSecondUnit.Symbol: string;
begin
  result := 's';
end;

class function TSecondUnit.Name: string;
begin
  result := 'second';
end;

{ Unit of TSquareSecondUnit }

class function TSquareSecondUnit.Symbol: string;
begin
  result := 's2';
end;

class function TSquareSecondUnit.Name: string;
begin
  result := 'square second';
end;

{ Unit of TMeterUnit }

class function TMeterUnit.Symbol: string;
begin
  result := 'm';
end;

class function TMeterUnit.Name: string;
begin
  result := 'meter';
end;

{ Unit of TSquareMeterUnit }

class function TSquareMeterUnit.Symbol: string;
begin
  result := 'm2';
end;

class function TSquareMeterUnit.Name: string;
begin
  result := 'square meter';
end;

{ Unit of TCubicMeterUnit }

class function TCubicMeterUnit.Symbol: string;
begin
  result := 'm3';
end;

class function TCubicMeterUnit.Name: string;
begin
  result := 'cubic meter';
end;

{ Unit of TQuarticMeterUnit }

class function TQuarticMeterUnit.Symbol: string;
begin
  result := 'm4';
end;

class function TQuarticMeterUnit.Name: string;
begin
  result := 'quartic meter';
end;

{ Unit of TQuinticMeterUnit }

class function TQuinticMeterUnit.Symbol: string;
begin
  result := 'm5';
end;

class function TQuinticMeterUnit.Name: string;
begin
  result := 'quintic meter';
end;

{ Unit of TSexticMeterUnit }

class function TSexticMeterUnit.Symbol: string;
begin
  result := 'm6';
end;

class function TSexticMeterUnit.Name: string;
begin
  result := 'sextic meter';
end;

{ Unit of TKilogramUnit }

class function TKilogramUnit.Symbol: string;
begin
  result := 'kg';
end;

class function TKilogramUnit.Name: string;
begin
  result := 'kilogram';
end;

{ Unit of TSquareKilogramUnit }

class function TSquareKilogramUnit.Symbol: string;
begin
  result := 'kg2';
end;

class function TSquareKilogramUnit.Name: string;
begin
  result := 'square kilogram';
end;

{ Unit of TAmpereUnit }

class function TAmpereUnit.Symbol: string;
begin
  result := 'A';
end;

class function TAmpereUnit.Name: string;
begin
  result := 'ampere';
end;

{ Unit of TSquareAmpereUnit }

class function TSquareAmpereUnit.Symbol: string;
begin
  result := 'A2';
end;

class function TSquareAmpereUnit.Name: string;
begin
  result := 'square ampere';
end;

{ Unit of TKelvinUnit }

class function TKelvinUnit.Symbol: string;
begin
  result := 'K';
end;

class function TKelvinUnit.Name: string;
begin
  result := 'kelvin';
end;

{ Unit of TSquareKelvinUnit }

class function TSquareKelvinUnit.Symbol: string;
begin
  result := 'K2';
end;

class function TSquareKelvinUnit.Name: string;
begin
  result := 'square kelvin';
end;

{ Unit of TCubicKelvinUnit }

class function TCubicKelvinUnit.Symbol: string;
begin
  result := 'K3';
end;

class function TCubicKelvinUnit.Name: string;
begin
  result := 'cubic kelvin';
end;

{ Unit of TQuarticKelvinUnit }

class function TQuarticKelvinUnit.Symbol: string;
begin
  result := '';
end;

class function TQuarticKelvinUnit.Name: string;
begin
  result := '';
end;

{ Unit of TMoleUnit }

class function TMoleUnit.Symbol: string;
begin
  result := 'mol';
end;

class function TMoleUnit.Name: string;
begin
  result := 'mole';
end;

{ Unit of TCandelaUnit }

class function TCandelaUnit.Symbol: string;
begin
  result := 'cd';
end;

class function TCandelaUnit.Name: string;
begin
  result := 'candela';
end;

{ Unit of TRadianUnit }

class function TRadianUnit.Symbol: string;
begin
  result := 'rad';
end;

class function TRadianUnit.Name: string;
begin
  result := 'radian';
end;

{ Unit of TSteradianUnit }

class function TSteradianUnit.Symbol: string;
begin
  result := 'sr';
end;

class function TSteradianUnit.Name: string;
begin
  result := 'steradian';
end;

{ Unit of THertzUnit }

class function THertzUnit.Symbol: string;
begin
  result := 'Hz';
end;

class function THertzUnit.Name: string;
begin
  result := 'hertz';
end;

{ Unit of TSquareHertzUnit }

class function TSquareHertzUnit.Symbol: string;
begin
  result := 'Hz2';
end;

class function TSquareHertzUnit.Name: string;
begin
  result := 'square hertz';
end;

{ Unit of TNewtonUnit }

class function TNewtonUnit.Symbol: string;
begin
  result := 'N';
end;

class function TNewtonUnit.Name: string;
begin
  result := 'newton';
end;

{ Unit of TPascalUnit }

class function TPascalUnit.Symbol: string;
begin
  result := 'Pa';
end;

class function TPascalUnit.Name: string;
begin
  result := 'pascal';
end;

{ Unit of TJouleUnit }

class function TJouleUnit.Symbol: string;
begin
  result := 'J';
end;

class function TJouleUnit.Name: string;
begin
  result := 'joule';
end;

{ Unit of TWattUnit }

class function TWattUnit.Symbol: string;
begin
  result := 'W';
end;

class function TWattUnit.Name: string;
begin
  result := 'watt';
end;

{ Unit of TCoulombUnit }

class function TCoulombUnit.Symbol: string;
begin
  result := 'C';
end;

class function TCoulombUnit.Name: string;
begin
  result := 'coulomb';
end;

{ Unit of TSquareCoulombUnit }

class function TSquareCoulombUnit.Symbol: string;
begin
  result := 'C2';
end;

class function TSquareCoulombUnit.Name: string;
begin
  result := 'square coulomb';
end;

{ Unit of TVoltUnit }

class function TVoltUnit.Symbol: string;
begin
  result := 'V';
end;

class function TVoltUnit.Name: string;
begin
  result := 'volt';
end;

{ Unit of TSquareVoltUnit }

class function TSquareVoltUnit.Symbol: string;
begin
  result := 'V2';
end;

class function TSquareVoltUnit.Name: string;
begin
  result := 'square volt';
end;

{ Unit of TFaradUnit }

class function TFaradUnit.Symbol: string;
begin
  result := 'F';
end;

class function TFaradUnit.Name: string;
begin
  result := 'farad';
end;

{ Unit of TOhmUnit }

class function TOhmUnit.Symbol: string;
begin
  result := 'Ω';
end;

class function TOhmUnit.Name: string;
begin
  result := 'ohm';
end;

{ Unit of TSiemensUnit }

class function TSiemensUnit.Symbol: string;
begin
  result := 'S';
end;

class function TSiemensUnit.Name: string;
begin
  result := 'siemens';
end;

{ Unit of TWeberUnit }

class function TWeberUnit.Symbol: string;
begin
  result := 'Wb';
end;

class function TWeberUnit.Name: string;
begin
  result := 'weber';
end;

{ Unit of TTeslaUnit }

class function TTeslaUnit.Symbol: string;
begin
  result := 'T';
end;

class function TTeslaUnit.Name: string;
begin
  result := 'tesla';
end;

{ Unit of THenryUnit }

class function THenryUnit.Symbol: string;
begin
  result := 'H';
end;

class function THenryUnit.Name: string;
begin
  result := 'henry';
end;

{ Unit of TLumenUnit }

class function TLumenUnit.Symbol: string;
begin
  result := 'lm';
end;

class function TLumenUnit.Name: string;
begin
  result := 'lumen';
end;

{ Unit of TLuxUnit }

class function TLuxUnit.Symbol: string;
begin
  result := 'lx';
end;

class function TLuxUnit.Name: string;
begin
  result := 'lux';
end;

{ Unit of TBequerelUnit }

class function TBequerelUnit.Symbol: string;
begin
  result := 'Bq';
end;

class function TBequerelUnit.Name: string;
begin
  result := 'bequerel';
end;

{ Unit of TGrayUnit }

class function TGrayUnit.Symbol: string;
begin
  result := 'Gy';
end;

class function TGrayUnit.Name: string;
begin
  result := 'gray';
end;

{ Unit of TSievertUnit }

class function TSievertUnit.Symbol: string;
begin
  result := 'Sv';
end;

class function TSievertUnit.Name: string;
begin
  result := 'sievert';
end;

{ Unit of TKatalUnit }

class function TKatalUnit.Symbol: string;
begin
  result := 'kat';
end;

class function TKatalUnit.Name: string;
begin
  result := 'katal';
end;

{ Unit of TNewtonMeterUnit }

class function TNewtonMeterUnit.Symbol: string;
begin
  result := 'N·m';
end;

class function TNewtonMeterUnit.Name: string;
begin
  result := 'newton meter';
end;

{ Unit of TJoulePerRadianUnit }

class function TJoulePerRadianUnit.Symbol: string;
begin
  result := 'J/rad';
end;

class function TJoulePerRadianUnit.Name: string;
begin
  result := 'joule per radian';
end;

{ Unit of TNewtonMeterPerRadianUnit }

class function TNewtonMeterPerRadianUnit.Symbol: string;
begin
  result := 'N·m/rad';
end;

class function TNewtonMeterPerRadianUnit.Name: string;
begin
  result := 'newton meter per radian';
end;

{ Unit of TMeterPerSecondUnit }

class function TMeterPerSecondUnit.Symbol: string;
begin
  result := 'm/s';
end;

class function TMeterPerSecondUnit.Name: string;
begin
  result := 'meter per second';
end;

{ Unit of TMeterPerSquareSecondUnit }

class function TMeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'm/s2';
end;

class function TMeterPerSquareSecondUnit.Name: string;
begin
  result := 'meter per square second';
end;

{ Unit of TRadianPerSecondUnit }

class function TRadianPerSecondUnit.Symbol: string;
begin
  result := 'rad/s';
end;

class function TRadianPerSecondUnit.Name: string;
begin
  result := 'radian per second';
end;

{ Unit of TRadianPerSquareSecondUnit }

class function TRadianPerSquareSecondUnit.Symbol: string;
begin
  result := 'rad/s2';
end;

class function TRadianPerSquareSecondUnit.Name: string;
begin
  result := 'radian per square second';
end;

{ Unit of TKilogramPerMeterUnit }

class function TKilogramPerMeterUnit.Symbol: string;
begin
  result := 'kg/m';
end;

class function TKilogramPerMeterUnit.Name: string;
begin
  result := 'kilogram per meter';
end;

{ Unit of TKilogramPerSquareMeterUnit }

class function TKilogramPerSquareMeterUnit.Symbol: string;
begin
  result := 'kg/m2';
end;

class function TKilogramPerSquareMeterUnit.Name: string;
begin
  result := 'kilogram per square meter';
end;

{ Unit of TKilogramPerCubicMeterUnit }

class function TKilogramPerCubicMeterUnit.Symbol: string;
begin
  result := 'kg/m3';
end;

class function TKilogramPerCubicMeterUnit.Name: string;
begin
  result := 'kilogram per cubic meter';
end;

{ Unit of TNewtonPerCubicMeterUnit }

class function TNewtonPerCubicMeterUnit.Symbol: string;
begin
  result := 'N/m3';
end;

class function TNewtonPerCubicMeterUnit.Name: string;
begin
  result := 'newton per cubic meter';
end;

{ Unit of TNewtonPerMeterUnit }

class function TNewtonPerMeterUnit.Symbol: string;
begin
  result := 'N/m';
end;

class function TNewtonPerMeterUnit.Name: string;
begin
  result := 'newton per meter';
end;

{ Unit of TKilogramMeterPerSecondUnit }

class function TKilogramMeterPerSecondUnit.Symbol: string;
begin
  result := 'kg·m/s';
end;

class function TKilogramMeterPerSecondUnit.Name: string;
begin
  result := 'kilogram meter per second';
end;

{ Unit of TNewtonSecondUnit }

class function TNewtonSecondUnit.Symbol: string;
begin
  result := 'N·s';
end;

class function TNewtonSecondUnit.Name: string;
begin
  result := 'newton second';
end;

{ Unit of TKilogramSquareMeterUnit }

class function TKilogramSquareMeterUnit.Symbol: string;
begin
  result := 'kg·m2';
end;

class function TKilogramSquareMeterUnit.Name: string;
begin
  result := 'kilogram square meter';
end;

{ Unit of TKilogramSquareMeterPerSecondUnit }

class function TKilogramSquareMeterPerSecondUnit.Symbol: string;
begin
  result := '';
end;

class function TKilogramSquareMeterPerSecondUnit.Name: string;
begin
  result := '';
end;

{ Unit of TSquareMeterPerSquareSecondUnit }

class function TSquareMeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'm2/s2';
end;

class function TSquareMeterPerSquareSecondUnit.Name: string;
begin
  result := 'square meter per square second';
end;

{ Unit of TSteradianPerSquareSecondUnit }

class function TSteradianPerSquareSecondUnit.Symbol: string;
begin
  result := 'rad2/s2';
end;

class function TSteradianPerSquareSecondUnit.Name: string;
begin
  result := 'square rad per square second';
end;

{ Unit of TCubicMeterPerSecondUnit }

class function TCubicMeterPerSecondUnit.Symbol: string;
begin
  result := 'm3/s';
end;

class function TCubicMeterPerSecondUnit.Name: string;
begin
  result := 'cubic meter per second';
end;

{ Unit of TPascalSecondUnit }

class function TPascalSecondUnit.Symbol: string;
begin
  result := 'Pa·s';
end;

class function TPascalSecondUnit.Name: string;
begin
  result := 'pascal second';
end;

{ Unit of TSquareMeterPerSecondUnit }

class function TSquareMeterPerSecondUnit.Symbol: string;
begin
  result := 'm2/s';
end;

class function TSquareMeterPerSecondUnit.Name: string;
begin
  result := 'square meter per second';
end;

{ Unit of TNewtonPerSquareKilogramUnit }

class function TNewtonPerSquareKilogramUnit.Symbol: string;
begin
  result := 'N/kg2';
end;

class function TNewtonPerSquareKilogramUnit.Name: string;
begin
  result := 'newton per square kilogram';
end;

{ Unit of TSquareKilogramPerMeterUnit }

class function TSquareKilogramPerMeterUnit.Symbol: string;
begin
  result := 'kg2/m';
end;

class function TSquareKilogramPerMeterUnit.Name: string;
begin
  result := 'square kilogram per meter';
end;

{ Unit of TSquareKilogramPerSquareMeterUnit }

class function TSquareKilogramPerSquareMeterUnit.Symbol: string;
begin
  result := 'kg2/m2';
end;

class function TSquareKilogramPerSquareMeterUnit.Name: string;
begin
  result := 'square kilogram per square meter';
end;

{ Unit of TSquareMeterPerSquareKilogramUnit }

class function TSquareMeterPerSquareKilogramUnit.Symbol: string;
begin
  result := 'm2/kg2';
end;

class function TSquareMeterPerSquareKilogramUnit.Name: string;
begin
  result := 'square meter per square kilogram';
end;

{ Unit of TNewtonSquareMeterPerSquareKilogramUnit }

class function TNewtonSquareMeterPerSquareKilogramUnit.Symbol: string;
begin
  result := 'N·m2/kg2';
end;

class function TNewtonSquareMeterPerSquareKilogramUnit.Name: string;
begin
  result := 'newton square meter per square kilogram';
end;

{ Unit of TReciprocalKelvinUnit }

class function TReciprocalKelvinUnit.Symbol: string;
begin
  result := '1/K';
end;

class function TReciprocalKelvinUnit.Name: string;
begin
  result := 'reciprocal kelvin';
end;

{ Unit of TKilogramKelvinUnit }

class function TKilogramKelvinUnit.Symbol: string;
begin
  result := 'kg·K';
end;

class function TKilogramKelvinUnit.Name: string;
begin
  result := 'kilogram kelvin';
end;

{ Unit of TJoulePerKelvinUnit }

class function TJoulePerKelvinUnit.Symbol: string;
begin
  result := 'J/K';
end;

class function TJoulePerKelvinUnit.Name: string;
begin
  result := 'joule per kelvin';
end;

{ Unit of TJoulePerKilogramUnit }

class function TJoulePerKilogramUnit.Symbol: string;
begin
  result := 'J/kg';
end;

class function TJoulePerKilogramUnit.Name: string;
begin
  result := 'joule per kilogram';
end;

{ Unit of TJoulePerKilogramPerKelvinUnit }

class function TJoulePerKilogramPerKelvinUnit.Symbol: string;
begin
  result := 'J/kg/K';
end;

class function TJoulePerKilogramPerKelvinUnit.Name: string;
begin
  result := 'joule per kilogram per kelvin';
end;

{ Unit of TMeterKelvinUnit }

class function TMeterKelvinUnit.Symbol: string;
begin
  result := 'm·K';
end;

class function TMeterKelvinUnit.Name: string;
begin
  result := 'meter kelvin';
end;

{ Unit of TKelvinPerMeterUnit }

class function TKelvinPerMeterUnit.Symbol: string;
begin
  result := 'K/m';
end;

class function TKelvinPerMeterUnit.Name: string;
begin
  result := 'kelvin per meter';
end;

{ Unit of TWattPerMeterUnit }

class function TWattPerMeterUnit.Symbol: string;
begin
  result := 'W/m';
end;

class function TWattPerMeterUnit.Name: string;
begin
  result := 'watt per meter';
end;

{ Unit of TWattPerSquareMeterUnit }

class function TWattPerSquareMeterUnit.Symbol: string;
begin
  result := 'W/m2';
end;

class function TWattPerSquareMeterUnit.Name: string;
begin
  result := 'watt per square meter';
end;

{ Unit of TWattPerKelvinUnit }

class function TWattPerKelvinUnit.Symbol: string;
begin
  result := 'W/K';
end;

class function TWattPerKelvinUnit.Name: string;
begin
  result := 'watt per kelvin';
end;

{ Unit of TWattPerMeterPerKelvinUnit }

class function TWattPerMeterPerKelvinUnit.Symbol: string;
begin
  result := 'W/m/K';
end;

class function TWattPerMeterPerKelvinUnit.Name: string;
begin
  result := 'watt per meter per kelvin';
end;

{ Unit of TSquareMeterKelvinUnit }

class function TSquareMeterKelvinUnit.Symbol: string;
begin
  result := 'm2·K';
end;

class function TSquareMeterKelvinUnit.Name: string;
begin
  result := 'square meter kelvin';
end;

{ Unit of TWattPerSquareMeterPerKelvinUnit }

class function TWattPerSquareMeterPerKelvinUnit.Symbol: string;
begin
  result := 'W/m2/K';
end;

class function TWattPerSquareMeterPerKelvinUnit.Name: string;
begin
  result := 'watt per square meter per kelvin';
end;

{ Unit of TSquareMeterQuarticKelvinUnit }

class function TSquareMeterQuarticKelvinUnit.Symbol: string;
begin
  result := 'm2·K4';
end;

class function TSquareMeterQuarticKelvinUnit.Name: string;
begin
  result := 'square meter quartic kelvin';
end;

{ Unit of TWattPerQuarticKelvinUnit }

class function TWattPerQuarticKelvinUnit.Symbol: string;
begin
  result := 'W/K4';
end;

class function TWattPerQuarticKelvinUnit.Name: string;
begin
  result := 'watt per quartic kelvin';
end;

{ Unit of TWattPerSquareMeterPerQuarticKelvinUnit }

class function TWattPerSquareMeterPerQuarticKelvinUnit.Symbol: string;
begin
  result := 'W/m2/K4';
end;

class function TWattPerSquareMeterPerQuarticKelvinUnit.Name: string;
begin
  result := 'watt per square meter per quartic kelvin';
end;

{ Unit of TJoulePerMoleUnit }

class function TJoulePerMoleUnit.Symbol: string;
begin
  result := 'J/mol';
end;

class function TJoulePerMoleUnit.Name: string;
begin
  result := 'joule per mole';
end;

{ Unit of TMoleKelvinUnit }

class function TMoleKelvinUnit.Symbol: string;
begin
  result := 'mol·K';
end;

class function TMoleKelvinUnit.Name: string;
begin
  result := 'mole kelvin';
end;

{ Unit of TJoulePerMolePerKelvinUnit }

class function TJoulePerMolePerKelvinUnit.Symbol: string;
begin
  result := 'J/mol/K';
end;

class function TJoulePerMolePerKelvinUnit.Name: string;
begin
  result := 'joule per mole per kelvin';
end;

{ Unit of TOhmMeterUnit }

class function TOhmMeterUnit.Symbol: string;
begin
  result := 'Ω·m';
end;

class function TOhmMeterUnit.Name: string;
begin
  result := 'ohm meter';
end;

{ Unit of TVoltPerMeterUnit }

class function TVoltPerMeterUnit.Symbol: string;
begin
  result := 'V/m';
end;

class function TVoltPerMeterUnit.Name: string;
begin
  result := 'volt per meter';
end;

{ Unit of TCoulombPerMeterUnit }

class function TCoulombPerMeterUnit.Symbol: string;
begin
  result := 'C/m';
end;

class function TCoulombPerMeterUnit.Name: string;
begin
  result := 'coulomb per meter';
end;

{ Unit of TSquareCoulombPerMeterUnit }

class function TSquareCoulombPerMeterUnit.Symbol: string;
begin
  result := 'C2/m';
end;

class function TSquareCoulombPerMeterUnit.Name: string;
begin
  result := 'square coulomb per meter';
end;

{ Unit of TCoulombPerSquareMeterUnit }

class function TCoulombPerSquareMeterUnit.Symbol: string;
begin
  result := 'C/m2';
end;

class function TCoulombPerSquareMeterUnit.Name: string;
begin
  result := 'coulomb per square meter';
end;

{ Unit of TSquareMeterPerSquareCoulombUnit }

class function TSquareMeterPerSquareCoulombUnit.Symbol: string;
begin
  result := 'm2/C2';
end;

class function TSquareMeterPerSquareCoulombUnit.Name: string;
begin
  result := 'square meter per square coulomb';
end;

{ Unit of TNewtonPerSquareCoulombUnit }

class function TNewtonPerSquareCoulombUnit.Symbol: string;
begin
  result := 'N/C2';
end;

class function TNewtonPerSquareCoulombUnit.Name: string;
begin
  result := 'newton per square coulomb';
end;

{ Unit of TNewtonSquareMeterUnit }

class function TNewtonSquareMeterUnit.Symbol: string;
begin
  result := 'N·m2';
end;

class function TNewtonSquareMeterUnit.Name: string;
begin
  result := 'newton square meter';
end;

{ Unit of TNewtonSquareMeterPerSquareCoulombUnit }

class function TNewtonSquareMeterPerSquareCoulombUnit.Symbol: string;
begin
  result := 'N·m2/C2';
end;

class function TNewtonSquareMeterPerSquareCoulombUnit.Name: string;
begin
  result := 'newton square meter per square coulomb';
end;

{ Unit of TVoltMeterUnit }

class function TVoltMeterUnit.Symbol: string;
begin
  result := 'V·m';
end;

class function TVoltMeterUnit.Name: string;
begin
  result := 'volt meter';
end;

{ Unit of TVoltMeterPerSecondUnit }

class function TVoltMeterPerSecondUnit.Symbol: string;
begin
  result := 'V·m/s';
end;

class function TVoltMeterPerSecondUnit.Name: string;
begin
  result := 'volt meter per second';
end;

{ Unit of TFaradPerMeterUnit }

class function TFaradPerMeterUnit.Symbol: string;
begin
  result := 'F/m';
end;

class function TFaradPerMeterUnit.Name: string;
begin
  result := 'farad per meter';
end;

{ Unit of TAmperePerMeterUnit }

class function TAmperePerMeterUnit.Symbol: string;
begin
  result := 'A/m';
end;

class function TAmperePerMeterUnit.Name: string;
begin
  result := 'ampere per meter';
end;

{ Unit of TMeterPerAmpereUnit }

class function TMeterPerAmpereUnit.Symbol: string;
begin
  result := 'm/A';
end;

class function TMeterPerAmpereUnit.Name: string;
begin
  result := 'meter per ampere';
end;

{ Unit of TTeslaMeterUnit }

class function TTeslaMeterUnit.Symbol: string;
begin
  result := 'T·m';
end;

class function TTeslaMeterUnit.Name: string;
begin
  result := 'tesla meter';
end;

{ Unit of TTeslaPerAmpereUnit }

class function TTeslaPerAmpereUnit.Symbol: string;
begin
  result := 'T/A';
end;

class function TTeslaPerAmpereUnit.Name: string;
begin
  result := 'tesla per ampere';
end;

{ Unit of THenryPerMeterUnit }

class function THenryPerMeterUnit.Symbol: string;
begin
  result := 'H/m';
end;

class function THenryPerMeterUnit.Name: string;
begin
  result := 'henry per meter';
end;

{ Unit of TRadianPerMeterUnit }

class function TRadianPerMeterUnit.Symbol: string;
begin
  result := 'rad/m';
end;

class function TRadianPerMeterUnit.Name: string;
begin
  result := 'radian per meter';
end;

{ Unit of TDayUnit }

class function TDayUnit.Symbol: string;
begin
  result := 'day';
end;

class function TDayUnit.Name: string;
begin
  result := 'day';
end;

class function TDayUnit.Factor: double;
begin
  result := 86400;
end;

{ Unit of THourUnit }

class function THourUnit.Symbol: string;
begin
  result := 'h';
end;

class function THourUnit.Name: string;
begin
  result := 'hour';
end;

class function THourUnit.Factor: double;
begin
  result := 3600;
end;

{ Unit of TMinuteUnit }

class function TMinuteUnit.Symbol: string;
begin
  result := 'min';
end;

class function TMinuteUnit.Name: string;
begin
  result := 'minute';
end;

class function TMinuteUnit.Factor: double;
begin
  result := 60;
end;

{ Unit of TDecisecondUnit }

class function TDecisecondUnit.Symbol: string;
begin
  result := 'ds';
end;

class function TDecisecondUnit.Name: string;
begin
  result := 'decisecond';
end;

class function TDecisecondUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TCentisecondUnit }

class function TCentisecondUnit.Symbol: string;
begin
  result := 'cs';
end;

class function TCentisecondUnit.Name: string;
begin
  result := 'centisecond';
end;

class function TCentisecondUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TMillisecondUnit }

class function TMillisecondUnit.Symbol: string;
begin
  result := 'ms';
end;

class function TMillisecondUnit.Name: string;
begin
  result := 'millisecond';
end;

class function TMillisecondUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMicrosecondUnit }

class function TMicrosecondUnit.Symbol: string;
begin
  result := 'us';
end;

class function TMicrosecondUnit.Name: string;
begin
  result := 'microsecond';
end;

class function TMicrosecondUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TNanosecondUnit }

class function TNanosecondUnit.Symbol: string;
begin
  result := 'ns';
end;

class function TNanosecondUnit.Name: string;
begin
  result := 'nanosecond';
end;

class function TNanosecondUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TPicosecondUnit }

class function TPicosecondUnit.Symbol: string;
begin
  result := 'ps';
end;

class function TPicosecondUnit.Name: string;
begin
  result := 'picosecond';
end;

class function TPicosecondUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TKilometerUnit }

class function TKilometerUnit.Symbol: string;
begin
  result := 'km';
end;

class function TKilometerUnit.Name: string;
begin
  result := 'kilometer';
end;

class function TKilometerUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of THectometerUnit }

class function THectometerUnit.Symbol: string;
begin
  result := 'hm';
end;

class function THectometerUnit.Name: string;
begin
  result := 'hectometer';
end;

class function THectometerUnit.Factor: double;
begin
  result := 1E+02;
end;

{ Unit of TDecameterUnit }

class function TDecameterUnit.Symbol: string;
begin
  result := 'dam';
end;

class function TDecameterUnit.Name: string;
begin
  result := 'decameter';
end;

class function TDecameterUnit.Factor: double;
begin
  result := 1E+01;
end;

{ Unit of TDecimeterUnit }

class function TDecimeterUnit.Symbol: string;
begin
  result := 'dm';
end;

class function TDecimeterUnit.Name: string;
begin
  result := 'decimeter';
end;

class function TDecimeterUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TCentimeterUnit }

class function TCentimeterUnit.Symbol: string;
begin
  result := 'cm';
end;

class function TCentimeterUnit.Name: string;
begin
  result := 'centimeter';
end;

class function TCentimeterUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TMillimeterUnit }

class function TMillimeterUnit.Symbol: string;
begin
  result := 'mm';
end;

class function TMillimeterUnit.Name: string;
begin
  result := 'millimeter';
end;

class function TMillimeterUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMicrometerUnit }

class function TMicrometerUnit.Symbol: string;
begin
  result := 'um';
end;

class function TMicrometerUnit.Name: string;
begin
  result := 'micrometer';
end;

class function TMicrometerUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TNanometerUnit }

class function TNanometerUnit.Symbol: string;
begin
  result := 'nm';
end;

class function TNanometerUnit.Name: string;
begin
  result := 'nanometer';
end;

class function TNanometerUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TPicometerUnit }

class function TPicometerUnit.Symbol: string;
begin
  result := 'pm';
end;

class function TPicometerUnit.Name: string;
begin
  result := 'picometer';
end;

class function TPicometerUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TSquareKilometerUnit }

class function TSquareKilometerUnit.Symbol: string;
begin
  result := 'km2';
end;

class function TSquareKilometerUnit.Name: string;
begin
  result := 'square kilometer';
end;

class function TSquareKilometerUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TSquareHectometerUnit }

class function TSquareHectometerUnit.Symbol: string;
begin
  result := 'hm2';
end;

class function TSquareHectometerUnit.Name: string;
begin
  result := 'square hectometer';
end;

class function TSquareHectometerUnit.Factor: double;
begin
  result := 1E+04;
end;

{ Unit of TSquareDecameterUnit }

class function TSquareDecameterUnit.Symbol: string;
begin
  result := 'dam2';
end;

class function TSquareDecameterUnit.Name: string;
begin
  result := 'square decameter';
end;

class function TSquareDecameterUnit.Factor: double;
begin
  result := 1E+02;
end;

{ Unit of TSquareDecimeterUnit }

class function TSquareDecimeterUnit.Symbol: string;
begin
  result := 'dm2';
end;

class function TSquareDecimeterUnit.Name: string;
begin
  result := 'square decimeter';
end;

class function TSquareDecimeterUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TSquareCentimeterUnit }

class function TSquareCentimeterUnit.Symbol: string;
begin
  result := 'cm2';
end;

class function TSquareCentimeterUnit.Name: string;
begin
  result := 'square centimeter';
end;

class function TSquareCentimeterUnit.Factor: double;
begin
  result := 1E-04;
end;

{ Unit of TSquareMillimeterUnit }

class function TSquareMillimeterUnit.Symbol: string;
begin
  result := 'mm2';
end;

class function TSquareMillimeterUnit.Name: string;
begin
  result := 'square millimeter';
end;

class function TSquareMillimeterUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TCubicKilometerUnit }

class function TCubicKilometerUnit.Symbol: string;
begin
  result := 'km3';
end;

class function TCubicKilometerUnit.Name: string;
begin
  result := 'cubic kilometer';
end;

class function TCubicKilometerUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TCubicHectometerUnit }

class function TCubicHectometerUnit.Symbol: string;
begin
  result := 'hm3';
end;

class function TCubicHectometerUnit.Name: string;
begin
  result := 'cubic hectometer';
end;

class function TCubicHectometerUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TCubicDecameterUnit }

class function TCubicDecameterUnit.Symbol: string;
begin
  result := 'dam3';
end;

class function TCubicDecameterUnit.Name: string;
begin
  result := 'cubic decameter';
end;

class function TCubicDecameterUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TCubicDecimeterUnit }

class function TCubicDecimeterUnit.Symbol: string;
begin
  result := 'dm3';
end;

class function TCubicDecimeterUnit.Name: string;
begin
  result := 'cubic decimeter';
end;

class function TCubicDecimeterUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TCubicCentimeterUnit }

class function TCubicCentimeterUnit.Symbol: string;
begin
  result := 'cm3';
end;

class function TCubicCentimeterUnit.Name: string;
begin
  result := 'cubic centimeter';
end;

class function TCubicCentimeterUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TCubicMillimeterUnit }

class function TCubicMillimeterUnit.Symbol: string;
begin
  result := 'mm3';
end;

class function TCubicMillimeterUnit.Name: string;
begin
  result := 'cubic millimeter';
end;

class function TCubicMillimeterUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TQuarticKilometerUnit }

class function TQuarticKilometerUnit.Symbol: string;
begin
  result := 'km4';
end;

class function TQuarticKilometerUnit.Name: string;
begin
  result := 'quartic kilometer';
end;

class function TQuarticKilometerUnit.Factor: double;
begin
  result := 1E+12;
end;

{ Unit of TQuarticHectometerUnit }

class function TQuarticHectometerUnit.Symbol: string;
begin
  result := 'hm4';
end;

class function TQuarticHectometerUnit.Name: string;
begin
  result := 'quartic hectometer';
end;

class function TQuarticHectometerUnit.Factor: double;
begin
  result := 1E+08;
end;

{ Unit of TQuarticDecameterUnit }

class function TQuarticDecameterUnit.Symbol: string;
begin
  result := 'dam4';
end;

class function TQuarticDecameterUnit.Name: string;
begin
  result := 'quartic decameter';
end;

class function TQuarticDecameterUnit.Factor: double;
begin
  result := 1E+04;
end;

{ Unit of TQuarticDecimeterUnit }

class function TQuarticDecimeterUnit.Symbol: string;
begin
  result := 'dm4';
end;

class function TQuarticDecimeterUnit.Name: string;
begin
  result := 'quartic decimeter';
end;

class function TQuarticDecimeterUnit.Factor: double;
begin
  result := 1E-04;
end;

{ Unit of TQuarticCentimeterUnit }

class function TQuarticCentimeterUnit.Symbol: string;
begin
  result := 'cm4';
end;

class function TQuarticCentimeterUnit.Name: string;
begin
  result := 'quartic centimeter';
end;

class function TQuarticCentimeterUnit.Factor: double;
begin
  result := 1E-08;
end;

{ Unit of TQuarticMillimeterUnit }

class function TQuarticMillimeterUnit.Symbol: string;
begin
  result := 'mm4';
end;

class function TQuarticMillimeterUnit.Name: string;
begin
  result := 'quartic millimeter';
end;

class function TQuarticMillimeterUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of THectogramUnit }

class function THectogramUnit.Symbol: string;
begin
  result := 'hg';
end;

class function THectogramUnit.Name: string;
begin
  result := 'hectogram';
end;

class function THectogramUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TDecagramUnit }

class function TDecagramUnit.Symbol: string;
begin
  result := 'dag';
end;

class function TDecagramUnit.Name: string;
begin
  result := 'decagram';
end;

class function TDecagramUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TGramUnit }

class function TGramUnit.Symbol: string;
begin
  result := 'g';
end;

class function TGramUnit.Name: string;
begin
  result := 'gram';
end;

class function TGramUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TDecigramUnit }

class function TDecigramUnit.Symbol: string;
begin
  result := 'dg';
end;

class function TDecigramUnit.Name: string;
begin
  result := 'decigram';
end;

class function TDecigramUnit.Factor: double;
begin
  result := 1E-04;
end;

{ Unit of TCentigramUnit }

class function TCentigramUnit.Symbol: string;
begin
  result := 'cg';
end;

class function TCentigramUnit.Name: string;
begin
  result := 'centigram';
end;

class function TCentigramUnit.Factor: double;
begin
  result := 1E-05;
end;

{ Unit of TMilligramUnit }

class function TMilligramUnit.Symbol: string;
begin
  result := 'mg';
end;

class function TMilligramUnit.Name: string;
begin
  result := 'milligram';
end;

class function TMilligramUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TMicrogramUnit }

class function TMicrogramUnit.Symbol: string;
begin
  result := 'ug';
end;

class function TMicrogramUnit.Name: string;
begin
  result := 'microgram';
end;

class function TMicrogramUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TNanogramUnit }

class function TNanogramUnit.Symbol: string;
begin
  result := 'ng';
end;

class function TNanogramUnit.Name: string;
begin
  result := 'nanogram';
end;

class function TNanogramUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TPicogramUnit }

class function TPicogramUnit.Symbol: string;
begin
  result := 'pg';
end;

class function TPicogramUnit.Name: string;
begin
  result := 'picogram';
end;

class function TPicogramUnit.Factor: double;
begin
  result := 1E-15;
end;

{ Unit of TKiloampereUnit }

class function TKiloampereUnit.Symbol: string;
begin
  result := 'kA';
end;

class function TKiloampereUnit.Name: string;
begin
  result := 'kiloampere';
end;

class function TKiloampereUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of THectoampereUnit }

class function THectoampereUnit.Symbol: string;
begin
  result := 'hA';
end;

class function THectoampereUnit.Name: string;
begin
  result := 'hectoampere';
end;

class function THectoampereUnit.Factor: double;
begin
  result := 1E+02;
end;

{ Unit of TDecampereUnit }

class function TDecampereUnit.Symbol: string;
begin
  result := 'daA';
end;

class function TDecampereUnit.Name: string;
begin
  result := 'decampere';
end;

class function TDecampereUnit.Factor: double;
begin
  result := 1E+01;
end;

{ Unit of TDeciampereUnit }

class function TDeciampereUnit.Symbol: string;
begin
  result := 'dA';
end;

class function TDeciampereUnit.Name: string;
begin
  result := 'deciampere';
end;

class function TDeciampereUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TCentiampereUnit }

class function TCentiampereUnit.Symbol: string;
begin
  result := 'cA';
end;

class function TCentiampereUnit.Name: string;
begin
  result := 'centiampere';
end;

class function TCentiampereUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TMilliampereUnit }

class function TMilliampereUnit.Symbol: string;
begin
  result := 'mA';
end;

class function TMilliampereUnit.Name: string;
begin
  result := 'milliampere';
end;

class function TMilliampereUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMicroampereUnit }

class function TMicroampereUnit.Symbol: string;
begin
  result := 'uA';
end;

class function TMicroampereUnit.Name: string;
begin
  result := 'microampere';
end;

class function TMicroampereUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TNanoampereUnit }

class function TNanoampereUnit.Symbol: string;
begin
  result := 'nA';
end;

class function TNanoampereUnit.Name: string;
begin
  result := 'nanoampere';
end;

class function TNanoampereUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TPicoampereUnit }

class function TPicoampereUnit.Symbol: string;
begin
  result := 'pA';
end;

class function TPicoampereUnit.Name: string;
begin
  result := 'picoampere';
end;

class function TPicoampereUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TSquareMilliampereUnit }

class function TSquareMilliampereUnit.Symbol: string;
begin
  result := 'mA2';
end;

class function TSquareMilliampereUnit.Name: string;
begin
  result := 'square milliampere';
end;

class function TSquareMilliampereUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TDegreeUnit }

class function TDegreeUnit.Symbol: string;
begin
  result := 'deg';
end;

class function TDegreeUnit.Name: string;
begin
  result := 'degree';
end;

class function TDegreeUnit.Factor: double;
begin
  result := Pi/180;
end;

{ Unit of TGigahertzUnit }

class function TGigahertzUnit.Symbol: string;
begin
  result := 'GHz';
end;

class function TGigahertzUnit.Name: string;
begin
  result := 'gigahertz';
end;

class function TGigahertzUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegahertzUnit }

class function TMegahertzUnit.Symbol: string;
begin
  result := 'MHz';
end;

class function TMegahertzUnit.Name: string;
begin
  result := 'megahertz';
end;

class function TMegahertzUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilohertzUnit }

class function TKilohertzUnit.Symbol: string;
begin
  result := 'kHz';
end;

class function TKilohertzUnit.Name: string;
begin
  result := 'kilohertz';
end;

class function TKilohertzUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TGiganewtonUnit }

class function TGiganewtonUnit.Symbol: string;
begin
  result := 'GN';
end;

class function TGiganewtonUnit.Name: string;
begin
  result := 'giganewton';
end;

class function TGiganewtonUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMeganewtonUnit }

class function TMeganewtonUnit.Symbol: string;
begin
  result := 'MN';
end;

class function TMeganewtonUnit.Name: string;
begin
  result := 'meganewton';
end;

class function TMeganewtonUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilonewtonUnit }

class function TKilonewtonUnit.Symbol: string;
begin
  result := 'kN';
end;

class function TKilonewtonUnit.Name: string;
begin
  result := 'kilonewton';
end;

class function TKilonewtonUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TGigapascalUnit }

class function TGigapascalUnit.Symbol: string;
begin
  result := 'GPa';
end;

class function TGigapascalUnit.Name: string;
begin
  result := 'gigapascal';
end;

class function TGigapascalUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegapascalUnit }

class function TMegapascalUnit.Symbol: string;
begin
  result := 'MPa';
end;

class function TMegapascalUnit.Name: string;
begin
  result := 'megapascal';
end;

class function TMegapascalUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilopascalUnit }

class function TKilopascalUnit.Symbol: string;
begin
  result := 'kPa';
end;

class function TKilopascalUnit.Name: string;
begin
  result := 'kilopascal';
end;

class function TKilopascalUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TGigajouleUnit }

class function TGigajouleUnit.Symbol: string;
begin
  result := 'GJ';
end;

class function TGigajouleUnit.Name: string;
begin
  result := '';
end;

class function TGigajouleUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegajouleUnit }

class function TMegajouleUnit.Symbol: string;
begin
  result := 'MJ';
end;

class function TMegajouleUnit.Name: string;
begin
  result := '';
end;

class function TMegajouleUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilojouleUnit }

class function TKilojouleUnit.Symbol: string;
begin
  result := 'kJ';
end;

class function TKilojouleUnit.Name: string;
begin
  result := '';
end;

class function TKilojouleUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TGigawattUnit }

class function TGigawattUnit.Symbol: string;
begin
  result := 'GW';
end;

class function TGigawattUnit.Name: string;
begin
  result := '';
end;

class function TGigawattUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegawattUnit }

class function TMegawattUnit.Symbol: string;
begin
  result := 'MW';
end;

class function TMegawattUnit.Name: string;
begin
  result := '';
end;

class function TMegawattUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilowattUnit }

class function TKilowattUnit.Symbol: string;
begin
  result := 'kW';
end;

class function TKilowattUnit.Name: string;
begin
  result := '';
end;

class function TKilowattUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TMilliwattUnit }

class function TMilliwattUnit.Symbol: string;
begin
  result := 'mW';
end;

class function TMilliwattUnit.Name: string;
begin
  result := '';
end;

class function TMilliwattUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TGigacoulombUnit }

class function TGigacoulombUnit.Symbol: string;
begin
  result := 'GC';
end;

class function TGigacoulombUnit.Name: string;
begin
  result := 'gigacoulomb';
end;

class function TGigacoulombUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegacoulombUnit }

class function TMegacoulombUnit.Symbol: string;
begin
  result := 'MC';
end;

class function TMegacoulombUnit.Name: string;
begin
  result := 'megacoulomb';
end;

class function TMegacoulombUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilocoulombUnit }

class function TKilocoulombUnit.Symbol: string;
begin
  result := 'kC';
end;

class function TKilocoulombUnit.Name: string;
begin
  result := 'kilocoulomb';
end;

class function TKilocoulombUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TMillicoulombUnit }

class function TMillicoulombUnit.Symbol: string;
begin
  result := 'mC';
end;

class function TMillicoulombUnit.Name: string;
begin
  result := 'millicoulomb';
end;

class function TMillicoulombUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TGigavoltUnit }

class function TGigavoltUnit.Symbol: string;
begin
  result := 'GV';
end;

class function TGigavoltUnit.Name: string;
begin
  result := 'gigavolt';
end;

class function TGigavoltUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegavoltUnit }

class function TMegavoltUnit.Symbol: string;
begin
  result := 'MV';
end;

class function TMegavoltUnit.Name: string;
begin
  result := 'megavolt';
end;

class function TMegavoltUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilovoltUnit }

class function TKilovoltUnit.Symbol: string;
begin
  result := 'kV';
end;

class function TKilovoltUnit.Name: string;
begin
  result := 'kilovolt';
end;

class function TKilovoltUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TMillivoltUnit }

class function TMillivoltUnit.Symbol: string;
begin
  result := 'mV';
end;

class function TMillivoltUnit.Name: string;
begin
  result := 'millivolt';
end;

class function TMillivoltUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TGigafaradUnit }

class function TGigafaradUnit.Symbol: string;
begin
  result := 'GF';
end;

class function TGigafaradUnit.Name: string;
begin
  result := 'gigafarad';
end;

class function TGigafaradUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegafaradUnit }

class function TMegafaradUnit.Symbol: string;
begin
  result := 'MF';
end;

class function TMegafaradUnit.Name: string;
begin
  result := 'megafarad';
end;

class function TMegafaradUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilofaradUnit }

class function TKilofaradUnit.Symbol: string;
begin
  result := 'kF';
end;

class function TKilofaradUnit.Name: string;
begin
  result := 'kilofarad';
end;

class function TKilofaradUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TMillifaradUnit }

class function TMillifaradUnit.Symbol: string;
begin
  result := 'mF';
end;

class function TMillifaradUnit.Name: string;
begin
  result := 'millifarad';
end;

class function TMillifaradUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMicrofaradUnit }

class function TMicrofaradUnit.Symbol: string;
begin
  result := 'uF';
end;

class function TMicrofaradUnit.Name: string;
begin
  result := 'microfarad';
end;

class function TMicrofaradUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TNanofaradUnit }

class function TNanofaradUnit.Symbol: string;
begin
  result := 'nF';
end;

class function TNanofaradUnit.Name: string;
begin
  result := 'nanofarad';
end;

class function TNanofaradUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TPicofaradUnit }

class function TPicofaradUnit.Symbol: string;
begin
  result := 'pF';
end;

class function TPicofaradUnit.Name: string;
begin
  result := 'picofarad';
end;

class function TPicofaradUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TGigaohmUnit }

class function TGigaohmUnit.Symbol: string;
begin
  result := 'GΩ';
end;

class function TGigaohmUnit.Name: string;
begin
  result := 'gigaohm';
end;

class function TGigaohmUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TMegaohmUnit }

class function TMegaohmUnit.Symbol: string;
begin
  result := 'MΩ';
end;

class function TMegaohmUnit.Name: string;
begin
  result := 'megaohm';
end;

class function TMegaohmUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKiloohmUnit }

class function TKiloohmUnit.Symbol: string;
begin
  result := 'kΩ';
end;

class function TKiloohmUnit.Name: string;
begin
  result := 'kiloohm';
end;

class function TKiloohmUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of TMilliohmUnit }

class function TMilliohmUnit.Symbol: string;
begin
  result := 'mΩ';
end;

class function TMilliohmUnit.Name: string;
begin
  result := 'milliohm';
end;

class function TMilliohmUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMicroohmUnit }

class function TMicroohmUnit.Symbol: string;
begin
  result := 'uΩ';
end;

class function TMicroohmUnit.Name: string;
begin
  result := 'microohm';
end;

class function TMicroohmUnit.Factor: double;
begin
  result := 1E-06;
end;

{ Unit of TNanoohmUnit }

class function TNanoohmUnit.Symbol: string;
begin
  result := 'nΩ';
end;

class function TNanoohmUnit.Name: string;
begin
  result := 'nanoohm';
end;

class function TNanoohmUnit.Factor: double;
begin
  result := 1E-09;
end;

{ Unit of TPicoohmUnit }

class function TPicoohmUnit.Symbol: string;
begin
  result := 'pΩ';
end;

class function TPicoohmUnit.Name: string;
begin
  result := 'picoohm';
end;

class function TPicoohmUnit.Factor: double;
begin
  result := 1E-12;
end;

{ Unit of TMilligrayUnit }

class function TMilligrayUnit.Symbol: string;
begin
  result := 'mGy';
end;

class function TMilligrayUnit.Name: string;
begin
  result := 'milli gray';
end;

class function TMilligrayUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TMilliSievertUnit }

class function TMilliSievertUnit.Symbol: string;
begin
  result := 'mSv';
end;

class function TMilliSievertUnit.Name: string;
begin
  result := 'millisievert';
end;

class function TMilliSievertUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TJoulePerDegreeUnit }

class function TJoulePerDegreeUnit.Symbol: string;
begin
  result := 'J/deg';
end;

class function TJoulePerDegreeUnit.Name: string;
begin
  result := 'joule per degree';
end;

class function TJoulePerDegreeUnit.Factor: double;
begin
  result := 180/Pi;
end;

{ Unit of TNewtonMeterPerDegreeUnit }

class function TNewtonMeterPerDegreeUnit.Symbol: string;
begin
  result := 'N·m/deg';
end;

class function TNewtonMeterPerDegreeUnit.Name: string;
begin
  result := 'newton meter per degree';
end;

class function TNewtonMeterPerDegreeUnit.Factor: double;
begin
  result := 180/Pi;
end;

{ Unit of TKilometerPerHourUnit }

class function TKilometerPerHourUnit.Symbol: string;
begin
  result := 'km/h';
end;

class function TKilometerPerHourUnit.Name: string;
begin
  result := 'kilometer per hour';
end;

class function TKilometerPerHourUnit.Factor: double;
begin
  result := 5/18;
end;

{ Unit of TDecimeterPerSecondUnit }

class function TDecimeterPerSecondUnit.Symbol: string;
begin
  result := 'dm/s';
end;

class function TDecimeterPerSecondUnit.Name: string;
begin
  result := 'decimeter per second';
end;

class function TDecimeterPerSecondUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TCentimeterPerSecondUnit }

class function TCentimeterPerSecondUnit.Symbol: string;
begin
  result := 'cm/s';
end;

class function TCentimeterPerSecondUnit.Name: string;
begin
  result := 'centimeter per second';
end;

class function TCentimeterPerSecondUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TMillimeterPerSecondUnit }

class function TMillimeterPerSecondUnit.Symbol: string;
begin
  result := 'mm/s';
end;

class function TMillimeterPerSecondUnit.Name: string;
begin
  result := 'millimeter per second';
end;

class function TMillimeterPerSecondUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TKilometerPerHourPerSecondUnit }

class function TKilometerPerHourPerSecondUnit.Symbol: string;
begin
  result := 'km/h/s';
end;

class function TKilometerPerHourPerSecondUnit.Name: string;
begin
  result := 'kilometer-hour per second';
end;

class function TKilometerPerHourPerSecondUnit.Factor: double;
begin
  result := 5/18;
end;

{ Unit of TDecimeterPerSquareSecondUnit }

class function TDecimeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'dm/s2';
end;

class function TDecimeterPerSquareSecondUnit.Name: string;
begin
  result := 'decimeter per square second';
end;

class function TDecimeterPerSquareSecondUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TCentimeterPerSquareSecondUnit }

class function TCentimeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'cm/s2';
end;

class function TCentimeterPerSquareSecondUnit.Name: string;
begin
  result := 'centimeter per square second';
end;

class function TCentimeterPerSquareSecondUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TMillimeterPerSquareSecondUnit }

class function TMillimeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'mm/s2';
end;

class function TMillimeterPerSquareSecondUnit.Name: string;
begin
  result := 'millimeter per square second';
end;

class function TMillimeterPerSquareSecondUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TKilogramPerCubicMillimeterUnit }

class function TKilogramPerCubicMillimeterUnit.Symbol: string;
begin
  result := 'kg/mm3';
end;

class function TKilogramPerCubicMillimeterUnit.Name: string;
begin
  result := 'kilogram per cubic millimeter';
end;

class function TKilogramPerCubicMillimeterUnit.Factor: double;
begin
  result := 1E+09;
end;

{ Unit of TKilogramPerCubicCentimeterUnit }

class function TKilogramPerCubicCentimeterUnit.Symbol: string;
begin
  result := 'kg/cm3';
end;

class function TKilogramPerCubicCentimeterUnit.Name: string;
begin
  result := 'kilogram per cubic centimeter';
end;

class function TKilogramPerCubicCentimeterUnit.Factor: double;
begin
  result := 1E+06;
end;

{ Unit of TKilogramPerCubicDecimeterUnit }

class function TKilogramPerCubicDecimeterUnit.Symbol: string;
begin
  result := 'kg/dm3';
end;

class function TKilogramPerCubicDecimeterUnit.Name: string;
begin
  result := 'kilogram per cubic decimeter';
end;

class function TKilogramPerCubicDecimeterUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Unit of THectogramPerCubicMeterUnit }

class function THectogramPerCubicMeterUnit.Symbol: string;
begin
  result := 'hg/m3';
end;

class function THectogramPerCubicMeterUnit.Name: string;
begin
  result := 'hectogram per cubic meter';
end;

class function THectogramPerCubicMeterUnit.Factor: double;
begin
  result := 1E-01;
end;

{ Unit of TDecagramPerCubicMeterUnit }

class function TDecagramPerCubicMeterUnit.Symbol: string;
begin
  result := 'dag/m3';
end;

class function TDecagramPerCubicMeterUnit.Name: string;
begin
  result := 'decagram per cubic meter';
end;

class function TDecagramPerCubicMeterUnit.Factor: double;
begin
  result := 1E-02;
end;

{ Unit of TGramPerCubicMeterUnit }

class function TGramPerCubicMeterUnit.Symbol: string;
begin
  result := 'g/m3';
end;

class function TGramPerCubicMeterUnit.Name: string;
begin
  result := 'gram per cubic meter';
end;

class function TGramPerCubicMeterUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Combining units }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSecondIdentifier; const ARight: TSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareSecondIdentifier; const ARight: TSecondIdentifier): TSecondIdentifier;
begin end;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterIdentifier; const ARight: TMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TMeterIdentifier): TMeterIdentifier;
begin end;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TSquareMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TMeterIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeterIdentifier; const ARight: TMeterIdentifier): TQuarticMeterIdentifier;
begin end;

operator /(const ALeft: TQuarticMeterIdentifier; const ARight: TCubicMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TCubicMeterIdentifier): TQuarticMeterIdentifier;
begin end;

operator /(const ALeft: TQuarticMeterIdentifier; const ARight: TMeterIdentifier): TCubicMeterIdentifier;
begin end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TQuarticMeterIdentifier;
begin end;

operator /(const ALeft: TQuarticMeterIdentifier; const ARight: TSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeterIdentifier; const ARight: TMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TQuarticMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TQuarticMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TMeterIdentifier): TQuarticMeterIdentifier;
begin end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeterIdentifier; const ARight: TSquareMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TCubicMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TCubicMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TSquareMeterIdentifier): TCubicMeterIdentifier;
begin end;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeterIdentifier; const ARight: TMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator /(const ALeft: TSexticMeterIdentifier; const ARight: TQuinticMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TQuinticMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator /(const ALeft: TSexticMeterIdentifier; const ARight: TMeterIdentifier): TQuinticMeterIdentifier;
begin end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeterIdentifier; const ARight: TSquareMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator /(const ALeft: TSexticMeterIdentifier; const ARight: TQuarticMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TQuarticMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator /(const ALeft: TSexticMeterIdentifier; const ARight: TSquareMeterIdentifier): TQuarticMeterIdentifier;
begin end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeterIdentifier; const ARight: TCubicMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator /(const ALeft: TSexticMeterIdentifier; const ARight: TCubicMeterIdentifier): TCubicMeterIdentifier;
begin end;

// main definition [ kg2 ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TKilogramIdentifier): TKilogramIdentifier;
begin end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereIdentifier; const ARight: TAmpereIdentifier): TSquareAmpereIdentifier;
begin end;

operator /(const ALeft: TSquareAmpereIdentifier; const ARight: TAmpereIdentifier): TAmpereIdentifier;
begin end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinIdentifier; const ARight: TKelvinIdentifier): TSquareKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareKelvinIdentifier; const ARight: TKelvinIdentifier): TKelvinIdentifier;
begin end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvinIdentifier; const ARight: TKelvinIdentifier): TCubicKelvinIdentifier;
begin end;

operator /(const ALeft: TCubicKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TCubicKelvinIdentifier;
begin end;

operator /(const ALeft: TCubicKelvinIdentifier; const ARight: TKelvinIdentifier): TSquareKelvinIdentifier;
begin end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TSquareKelvinIdentifier;
begin end;

//
operator *(const ALeft: TCubicKelvinIdentifier; const ARight: TKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TCubicKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TCubicKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TKelvinIdentifier): TCubicKelvinIdentifier;
begin end;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadianIdentifier; const ARight: TRadianIdentifier): TSteradianIdentifier;
begin end;

operator /(const ALeft: TSteradianIdentifier; const ARight: TRadianIdentifier): TRadianIdentifier;
begin end;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSecondIdentifier): THertzIdentifier;
begin end;

operator /(const ALeft: double; const ARight: THertzIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TSecondIdentifier): double;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: THertzIdentifier): double;
begin end;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertzIdentifier; const ARight: THertzIdentifier): TSquareHertzIdentifier;
begin end;

operator /(const ALeft: TSquareHertzIdentifier; const ARight: THertzIdentifier): THertzIdentifier;
begin end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TNewtonIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TKilogramIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TKilogramIdentifier): TNewtonIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TKilogramIdentifier;
begin end;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterIdentifier): TPascalIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TPascalIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TPascalIdentifier; const ARight: TSquareMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TPascalIdentifier): TNewtonIdentifier;
begin end;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TMeterIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TNewtonIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TNewtonIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TMeterIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascalIdentifier; const ARight: TCubicMeterIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TPascalIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TPascalIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TCubicMeterIdentifier): TPascalIdentifier;
begin end;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJouleIdentifier; const ARight: TSecondIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TWattIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TWattIdentifier; const ARight: TSecondIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TWattIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJouleIdentifier; const ARight: TRadianPerSecondIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TJouleIdentifier): TRadianPerSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TJouleIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TRadianPerSecondIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const ALeft: TSquareAmpereIdentifier; const ARight: TOhmIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TSquareAmpereIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TSquareAmpereIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TOhmIdentifier): TSquareAmpereIdentifier;
begin end;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TMeterPerSecondIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TNewtonIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TNewtonIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TMeterPerSecondIdentifier): TNewtonIdentifier;
begin end;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSecondIdentifier; const ARight: TAmpereIdentifier): TCoulombIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TSecondIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TSecondIdentifier): TCoulombIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TAmpereIdentifier): TSecondIdentifier;
begin end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombIdentifier; const ARight: TCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TCoulombIdentifier): TCoulombIdentifier;
begin end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWattIdentifier; const ARight: TAmpereIdentifier): TVoltIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TVoltIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TVoltIdentifier; const ARight: TAmpereIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TVoltIdentifier): TWattIdentifier;
begin end;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJouleIdentifier; const ARight: TCoulombIdentifier): TVoltIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TVoltIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TVoltIdentifier; const ARight: TCoulombIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TVoltIdentifier): TJouleIdentifier;
begin end;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVoltIdentifier; const ARight: TVoltIdentifier): TSquareVoltIdentifier;
begin end;

operator /(const ALeft: TSquareVoltIdentifier; const ARight: TVoltIdentifier): TVoltIdentifier;
begin end;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWattIdentifier; const ARight: TOhmIdentifier): TSquareVoltIdentifier;
begin end;

operator /(const ALeft: TSquareVoltIdentifier; const ARight: TWattIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TWattIdentifier): TSquareVoltIdentifier;
begin end;

operator /(const ALeft: TSquareVoltIdentifier; const ARight: TOhmIdentifier): TWattIdentifier;
begin end;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TVoltIdentifier): TFaradIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TFaradIdentifier): TVoltIdentifier;
begin end;

operator *(const ALeft: TFaradIdentifier; const ARight: TVoltIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TVoltIdentifier; const ARight: TFaradIdentifier): TCoulombIdentifier;
begin end;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TJouleIdentifier): TFaradIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TFaradIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TFaradIdentifier; const ARight: TJouleIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TJouleIdentifier; const ARight: TFaradIdentifier): TSquareCoulombIdentifier;
begin end;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVoltIdentifier; const ARight: TAmpereIdentifier): TOhmIdentifier;
begin end;

operator /(const ALeft: TVoltIdentifier; const ARight: TOhmIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TAmpereIdentifier): TVoltIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TOhmIdentifier): TVoltIdentifier;
begin end;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSecondIdentifier; const ARight: TFaradIdentifier): TOhmIdentifier;
begin end;

operator /(const ALeft: TSecondIdentifier; const ARight: TOhmIdentifier): TFaradIdentifier;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TFaradIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TFaradIdentifier; const ARight: TOhmIdentifier): TSecondIdentifier;
begin end;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhmIdentifier): TSiemensIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TSiemensIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: TSiemensIdentifier; const ARight: TOhmIdentifier): double;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TSiemensIdentifier): double;
begin end;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVoltIdentifier; const ARight: TSecondIdentifier): TWeberIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TVoltIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TVoltIdentifier): TWeberIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TSecondIdentifier): TVoltIdentifier;
begin end;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWeberIdentifier; const ARight: TSquareMeterIdentifier): TTeslaIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TTeslaIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TTeslaIdentifier; const ARight: TSquareMeterIdentifier): TWeberIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TTeslaIdentifier): TWeberIdentifier;
begin end;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWeberIdentifier; const ARight: TAmpereIdentifier): THenryIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: THenryIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: THenryIdentifier; const ARight: TAmpereIdentifier): TWeberIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: THenryIdentifier): TWeberIdentifier;
begin end;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhmIdentifier; const ARight: TSecondIdentifier): THenryIdentifier;
begin end;

operator /(const ALeft: THenryIdentifier; const ARight: TOhmIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TOhmIdentifier): THenryIdentifier;
begin end;

operator /(const ALeft: THenryIdentifier; const ARight: TSecondIdentifier): TOhmIdentifier;
begin end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhmIdentifier; const ARight: THertzIdentifier): THenryIdentifier;
begin end;

operator /(const ALeft: TOhmIdentifier; const ARight: THenryIdentifier): THertzIdentifier;
begin end;

operator *(const ALeft: THenryIdentifier; const ARight: THertzIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: THenryIdentifier): TOhmIdentifier;
begin end;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelaIdentifier; const ARight: TSteradianIdentifier): TLumenIdentifier;
begin end;

operator /(const ALeft: TLumenIdentifier; const ARight: TCandelaIdentifier): TSteradianIdentifier;
begin end;

operator *(const ALeft: TSteradianIdentifier; const ARight: TCandelaIdentifier): TLumenIdentifier;
begin end;

operator /(const ALeft: TLumenIdentifier; const ARight: TSteradianIdentifier): TCandelaIdentifier;
begin end;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumenIdentifier; const ARight: TSquareMeterIdentifier): TLuxIdentifier;
begin end;

operator /(const ALeft: TLumenIdentifier; const ARight: TLuxIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TLuxIdentifier; const ARight: TSquareMeterIdentifier): TLumenIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TLuxIdentifier): TLumenIdentifier;
begin end;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoleIdentifier; const ARight: TSecondIdentifier): TKatalIdentifier;
begin end;

operator /(const ALeft: TMoleIdentifier; const ARight: TKatalIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TKatalIdentifier; const ARight: TSecondIdentifier): TMoleIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TKatalIdentifier): TMoleIdentifier;
begin end;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJouleIdentifier; const ARight: TRadianIdentifier): TJoulePerRadianIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerRadianIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TJoulePerRadianIdentifier; const ARight: TRadianIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TRadianIdentifier; const ARight: TJoulePerRadianIdentifier): TJouleIdentifier;
begin end;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeterIdentifier; const ARight: TSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TMeterIdentifier;
begin end;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeterIdentifier; const ARight: TSquareSecondIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TSquareSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSquareSecondIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TMeterIdentifier;
begin end;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const ALeft: TSteradianPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TSteradianPerSquareSecondIdentifier;
begin end;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadianIdentifier; const ARight: TSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

operator /(const ALeft: TRadianIdentifier; const ARight: TRadianPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TSecondIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TRadianIdentifier;
begin end;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: TMeterIdentifier): TRadianPerSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TMeterIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TRadianPerSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadianIdentifier; const ARight: TSquareSecondIdentifier): TRadianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TRadianIdentifier; const ARight: TRadianPerSquareSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSquareSecondIdentifier; const ARight: TSquareSecondIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TSquareSecondIdentifier; const ARight: TRadianPerSquareSecondIdentifier): TRadianIdentifier;
begin end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadianPerSecondIdentifier; const ARight: TSecondIdentifier): TRadianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TRadianPerSecondIdentifier; const ARight: TRadianPerSquareSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSquareSecondIdentifier; const ARight: TSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TRadianPerSquareSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilogramIdentifier; const ARight: TMeterIdentifier): TKilogramPerMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramIdentifier; const ARight: TKilogramPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TKilogramPerMeterIdentifier; const ARight: TMeterIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TKilogramPerMeterIdentifier): TKilogramIdentifier;
begin end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilogramIdentifier; const ARight: TSquareMeterIdentifier): TKilogramPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramIdentifier; const ARight: TKilogramPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TKilogramPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TKilogramPerSquareMeterIdentifier): TKilogramIdentifier;
begin end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilogramIdentifier; const ARight: TCubicMeterIdentifier): TKilogramPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TCubicMeterIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TKilogramIdentifier;
begin end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerCubicMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TCubicMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TNewtonPerCubicMeterIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascalIdentifier; const ARight: TMeterIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TPascalIdentifier; const ARight: TNewtonPerCubicMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TMeterIdentifier): TPascalIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TNewtonPerCubicMeterIdentifier): TPascalIdentifier;
begin end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier;
begin end;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TMeterIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonPerMeterIdentifier; const ARight: TMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TNewtonPerMeterIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJouleIdentifier; const ARight: TSquareMeterIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TNewtonPerMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonPerMeterIdentifier; const ARight: TSquareMeterIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonPerMeterIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascalIdentifier; const ARight: TMeterIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TPascalIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TPascalIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TMeterIdentifier): TPascalIdentifier;
begin end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TMeterPerSecondIdentifier): TKilogramMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondIdentifier; const ARight: TKilogramIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TKilogramIdentifier): TKilogramMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TKilogramIdentifier;
begin end;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSecondIdentifier): TKilogramMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondIdentifier; const ARight: TNewtonIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TNewtonIdentifier): TKilogramMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TNewtonIdentifier;
begin end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TSquareMeterIdentifier): TKilogramSquareMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TKilogramIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TKilogramIdentifier): TKilogramSquareMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TKilogramIdentifier;
begin end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TKilogramSquareMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TKilogramSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TKilogramSquareMeterPerSecondIdentifier): TKilogramSquareMeterIdentifier;
begin end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TRadianPerSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: TKilogramSquareMeterIdentifier): TRadianPerSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TKilogramSquareMeterIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TKilogramSquareMeterIdentifier;
begin end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareSecondIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TSquareSecondIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareSecondIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TSquareMeterIdentifier;
begin end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJouleIdentifier; const ARight: TKilogramIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TKilogramIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TKilogramIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascalIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TPascalIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TPascalIdentifier;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TPascalIdentifier;
begin end;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradianIdentifier; const ARight: TSquareSecondIdentifier): TSteradianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSteradianIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator *(const ALeft: TSteradianPerSquareSecondIdentifier; const ARight: TSquareSecondIdentifier): TSteradianIdentifier;
begin end;

operator *(const ALeft: TSquareSecondIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TSteradianIdentifier;
begin end;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TSteradianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSteradianPerSquareSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJouleIdentifier; const ARight: TKilogramSquareMeterIdentifier): TSteradianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TKilogramSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSteradianPerSquareSecondIdentifier; const ARight: TKilogramSquareMeterIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TSteradianPerSquareSecondIdentifier): TJouleIdentifier;
begin end;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeterIdentifier; const ARight: TSecondIdentifier): TCubicMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TCubicMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TCubicMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TCubicMeterPerSecondIdentifier): TCubicMeterIdentifier;
begin end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TMeterPerSecondIdentifier): TCubicMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TCubicMeterPerSecondIdentifier; const ARight: TSquareMeterIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TSquareMeterIdentifier): TCubicMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TCubicMeterPerSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascalIdentifier; const ARight: TSecondIdentifier): TPascalSecondIdentifier;
begin end;

operator /(const ALeft: TPascalSecondIdentifier; const ARight: TPascalIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TPascalIdentifier): TPascalSecondIdentifier;
begin end;

operator /(const ALeft: TPascalSecondIdentifier; const ARight: TSecondIdentifier): TPascalIdentifier;
begin end;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSecondIdentifier): TSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TSquareMeterPerSecondIdentifier): TSquareMeterIdentifier;
begin end;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TPascalSecondIdentifier; const ARight: TSquareMeterPerSecondIdentifier): TKilogramPerCubicMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TPascalSecondIdentifier;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TSquareMeterPerSecondIdentifier): TPascalSecondIdentifier;
begin end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareKilogramIdentifier): TNewtonPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerSquareKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TNewtonPerSquareKilogramIdentifier; const ARight: TSquareKilogramIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramIdentifier; const ARight: TNewtonPerSquareKilogramIdentifier): TNewtonIdentifier;
begin end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TMeterIdentifier): TSquareKilogramPerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TSquareKilogramPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramPerMeterIdentifier; const ARight: TMeterIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareKilogramPerMeterIdentifier): TSquareKilogramIdentifier;
begin end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TSquareMeterIdentifier): TSquareKilogramPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TSquareKilogramIdentifier;
begin end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareKilogramIdentifier): TSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TNewtonIdentifier): TSquareMeterPerSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterIdentifier;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareKilogramIdentifier; const ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TNewtonPerSquareKilogramIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareMeterIdentifier): TNewtonPerSquareKilogramIdentifier;
begin end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJouleIdentifier; const ARight: TSquareKilogramPerMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramPerMeterIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramPerMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TJouleIdentifier;
begin end;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvinIdentifier): TReciprocalKelvinIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TReciprocalKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TReciprocalKelvinIdentifier; const ARight: TKelvinIdentifier): double;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TReciprocalKelvinIdentifier): double;
begin end;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TKelvinIdentifier): TKilogramKelvinIdentifier;
begin end;

operator /(const ALeft: TKilogramKelvinIdentifier; const ARight: TKilogramIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TKilogramIdentifier): TKilogramKelvinIdentifier;
begin end;

operator /(const ALeft: TKilogramKelvinIdentifier; const ARight: TKelvinIdentifier): TKilogramIdentifier;
begin end;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJouleIdentifier; const ARight: TKelvinIdentifier): TJoulePerKelvinIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerKelvinIdentifier; const ARight: TKelvinIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TJoulePerKelvinIdentifier): TJouleIdentifier;
begin end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJouleIdentifier; const ARight: TKilogramKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinIdentifier; const ARight: TKilogramKelvinIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TKilogramKelvinIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinIdentifier; const ARight: TKelvinIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulePerKelvinIdentifier; const ARight: TKilogramIdentifier): TJoulePerKilogramPerKelvinIdentifier;
begin end;

operator /(const ALeft: TJoulePerKelvinIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinIdentifier; const ARight: TKilogramIdentifier): TJoulePerKelvinIdentifier;
begin end;

operator *(const ALeft: TKilogramIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TJoulePerKelvinIdentifier;
begin end;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeterIdentifier; const ARight: TKelvinIdentifier): TMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TMeterKelvinIdentifier; const ARight: TMeterIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TMeterIdentifier): TMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TMeterKelvinIdentifier; const ARight: TKelvinIdentifier): TMeterIdentifier;
begin end;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvinIdentifier; const ARight: TMeterIdentifier): TKelvinPerMeterIdentifier;
begin end;

operator /(const ALeft: TKelvinIdentifier; const ARight: TKelvinPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TKelvinPerMeterIdentifier; const ARight: TMeterIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TKelvinPerMeterIdentifier): TKelvinIdentifier;
begin end;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWattIdentifier; const ARight: TMeterIdentifier): TWattPerMeterIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerMeterIdentifier; const ARight: TMeterIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TWattPerMeterIdentifier): TWattIdentifier;
begin end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWattIdentifier; const ARight: TSquareMeterIdentifier): TWattPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TWattPerSquareMeterIdentifier): TWattIdentifier;
begin end;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWattIdentifier; const ARight: TKelvinIdentifier): TWattPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerKelvinIdentifier; const ARight: TKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TWattPerKelvinIdentifier): TWattIdentifier;
begin end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWattIdentifier; const ARight: TMeterKelvinIdentifier): TWattPerMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TMeterKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinIdentifier; const ARight: TMeterKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TMeterKelvinIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TWattIdentifier;
begin end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattPerMeterIdentifier; const ARight: TKelvinIdentifier): TWattPerMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerMeterIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinIdentifier; const ARight: TKelvinIdentifier): TWattPerMeterIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TWattPerMeterIdentifier;
begin end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattPerKelvinIdentifier; const ARight: TMeterIdentifier): TWattPerMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerKelvinIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinIdentifier; const ARight: TMeterIdentifier): TWattPerKelvinIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TWattPerKelvinIdentifier;
begin end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TKelvinPerMeterIdentifier): TWattPerMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TKelvinPerMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinIdentifier; const ARight: TKelvinPerMeterIdentifier): TWattPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TKelvinPerMeterIdentifier; const ARight: TWattPerMeterPerKelvinIdentifier): TWattPerSquareMeterIdentifier;
begin end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TKelvinIdentifier): TSquareMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterKelvinIdentifier; const ARight: TSquareMeterIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TSquareMeterIdentifier): TSquareMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterKelvinIdentifier; const ARight: TKelvinIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWattIdentifier; const ARight: TSquareMeterKelvinIdentifier): TWattPerSquareMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TSquareMeterKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinIdentifier; const ARight: TSquareMeterKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TSquareMeterKelvinIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattIdentifier;
begin end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TKelvinIdentifier): TWattPerSquareMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinIdentifier; const ARight: TKelvinIdentifier): TWattPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattPerSquareMeterIdentifier;
begin end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattPerKelvinIdentifier; const ARight: TSquareMeterIdentifier): TWattPerSquareMeterPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerKelvinIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinIdentifier; const ARight: TSquareMeterIdentifier): TWattPerKelvinIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TWattPerSquareMeterPerKelvinIdentifier): TWattPerKelvinIdentifier;
begin end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TSquareMeterQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinIdentifier; const ARight: TQuarticKelvinIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWattIdentifier; const ARight: TQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerQuarticKelvinIdentifier; const ARight: TQuarticKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TQuarticKelvinIdentifier; const ARight: TWattPerQuarticKelvinIdentifier): TWattIdentifier;
begin end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWattIdentifier; const ARight: TSquareMeterQuarticKelvinIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const ARight: TSquareMeterQuarticKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TSquareMeterQuarticKelvinIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattIdentifier;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TQuarticKelvinIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerSquareMeterIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const ARight: TQuarticKelvinIdentifier): TWattPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TQuarticKelvinIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattPerSquareMeterIdentifier;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattPerQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TWattPerSquareMeterPerQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TWattPerQuarticKelvinIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TWattPerQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TWattPerSquareMeterPerQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier;
begin end;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJouleIdentifier; const ARight: TMoleIdentifier): TJoulePerMoleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerMoleIdentifier): TMoleIdentifier;
begin end;

operator *(const ALeft: TJoulePerMoleIdentifier; const ARight: TMoleIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TMoleIdentifier; const ARight: TJoulePerMoleIdentifier): TJouleIdentifier;
begin end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoleIdentifier; const ARight: TKelvinIdentifier): TMoleKelvinIdentifier;
begin end;

operator /(const ALeft: TMoleKelvinIdentifier; const ARight: TMoleIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TMoleIdentifier): TMoleKelvinIdentifier;
begin end;

operator /(const ALeft: TMoleKelvinIdentifier; const ARight: TKelvinIdentifier): TMoleIdentifier;
begin end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJouleIdentifier; const ARight: TMoleKelvinIdentifier): TJoulePerMolePerKelvinIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TMoleKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinIdentifier; const ARight: TMoleKelvinIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TMoleKelvinIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TJouleIdentifier;
begin end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulePerKelvinIdentifier; const ARight: TMoleIdentifier): TJoulePerMolePerKelvinIdentifier;
begin end;

operator /(const ALeft: TJoulePerKelvinIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TMoleIdentifier;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinIdentifier; const ARight: TMoleIdentifier): TJoulePerKelvinIdentifier;
begin end;

operator *(const ALeft: TMoleIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TJoulePerKelvinIdentifier;
begin end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulePerMoleIdentifier; const ARight: TKelvinIdentifier): TJoulePerMolePerKelvinIdentifier;
begin end;

operator /(const ALeft: TJoulePerMoleIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinIdentifier; const ARight: TKelvinIdentifier): TJoulePerMoleIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TJoulePerMolePerKelvinIdentifier): TJoulePerMoleIdentifier;
begin end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhmIdentifier; const ARight: TMeterIdentifier): TOhmMeterIdentifier;
begin end;

operator /(const ALeft: TOhmMeterIdentifier; const ARight: TOhmIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TOhmIdentifier): TOhmMeterIdentifier;
begin end;

operator /(const ALeft: TOhmMeterIdentifier; const ARight: TMeterIdentifier): TOhmIdentifier;
begin end;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVoltIdentifier; const ARight: TMeterIdentifier): TVoltPerMeterIdentifier;
begin end;

operator /(const ALeft: TVoltIdentifier; const ARight: TVoltPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TVoltPerMeterIdentifier; const ARight: TMeterIdentifier): TVoltIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TVoltIdentifier;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TCoulombIdentifier): TVoltPerMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TVoltPerMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TVoltPerMeterIdentifier; const ARight: TCoulombIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TVoltPerMeterIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslaIdentifier; const ARight: TMeterPerSecondIdentifier): TVoltPerMeterIdentifier;
begin end;

operator /(const ALeft: TVoltPerMeterIdentifier; const ARight: TTeslaIdentifier): TMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TTeslaIdentifier): TVoltPerMeterIdentifier;
begin end;

operator /(const ALeft: TVoltPerMeterIdentifier; const ARight: TMeterPerSecondIdentifier): TTeslaIdentifier;
begin end;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TMeterIdentifier): TCoulombPerMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TCoulombPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombPerMeterIdentifier; const ARight: TMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier;
begin end;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TMeterIdentifier): TSquareCoulombPerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareCoulombPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerMeterIdentifier; const ARight: TMeterIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareCoulombPerMeterIdentifier): TSquareCoulombIdentifier;
begin end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombPerMeterIdentifier; const ARight: TCoulombIdentifier): TSquareCoulombPerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombPerMeterIdentifier; const ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TCoulombPerMeterIdentifier): TSquareCoulombPerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombPerMeterIdentifier; const ARight: TCoulombIdentifier): TCoulombPerMeterIdentifier;
begin end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TCoulombIdentifier;
begin end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombPerMeterIdentifier; const ARight: TMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombPerMeterIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TMeterIdentifier): TCoulombPerMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TCoulombPerMeterIdentifier;
begin end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareCoulombIdentifier): TSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareCoulombIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonPerSquareCoulombIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TNewtonIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterPerSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TNewtonIdentifier): TSquareMeterPerSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareCoulombIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareMeterPerSquareCoulombIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonSquareMeterIdentifier;
begin end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareCoulombIdentifier; const ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareMeterIdentifier): TNewtonPerSquareCoulombIdentifier;
begin end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltPerMeterIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TVoltPerMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TVoltPerMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TVoltPerMeterIdentifier;
begin end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJouleIdentifier; const ARight: TSquareCoulombPerMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareCoulombPerMeterIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TJouleIdentifier;
begin end;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVoltIdentifier; const ARight: TMeterIdentifier): TVoltMeterIdentifier;
begin end;

operator /(const ALeft: TVoltMeterIdentifier; const ARight: TVoltIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TVoltIdentifier): TVoltMeterIdentifier;
begin end;

operator /(const ALeft: TVoltMeterIdentifier; const ARight: TMeterIdentifier): TVoltIdentifier;
begin end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltPerMeterIdentifier; const ARight: TSquareMeterIdentifier): TVoltMeterIdentifier;
begin end;

operator /(const ALeft: TVoltMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TVoltMeterIdentifier;
begin end;

operator /(const ALeft: TVoltMeterIdentifier; const ARight: TSquareMeterIdentifier): TVoltPerMeterIdentifier;
begin end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeterIdentifier; const ARight: TSecondIdentifier): TVoltMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TVoltMeterIdentifier; const ARight: TVoltMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TVoltMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TVoltMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TVoltMeterPerSecondIdentifier): TVoltMeterIdentifier;
begin end;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFaradIdentifier; const ARight: TMeterIdentifier): TFaradPerMeterIdentifier;
begin end;

operator /(const ALeft: TFaradIdentifier; const ARight: TFaradPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TFaradPerMeterIdentifier; const ARight: TMeterIdentifier): TFaradIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TFaradPerMeterIdentifier): TFaradIdentifier;
begin end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TVoltMeterIdentifier): TFaradPerMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TFaradPerMeterIdentifier): TVoltMeterIdentifier;
begin end;

operator *(const ALeft: TFaradPerMeterIdentifier; const ARight: TVoltMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TVoltMeterIdentifier; const ARight: TFaradPerMeterIdentifier): TCoulombIdentifier;
begin end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TFaradPerMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TFaradPerMeterIdentifier): TVoltPerMeterIdentifier;
begin end;

operator *(const ALeft: TFaradPerMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TVoltPerMeterIdentifier; const ARight: TFaradPerMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TFaradPerMeterIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TFaradPerMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TFaradPerMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): double;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TFaradPerMeterIdentifier): double;
begin end;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmpereIdentifier; const ARight: TMeterIdentifier): TAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TAmpereIdentifier; const ARight: TAmperePerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TAmperePerMeterIdentifier; const ARight: TMeterIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TAmperePerMeterIdentifier): TAmpereIdentifier;
begin end;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeterIdentifier; const ARight: TAmpereIdentifier): TMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TMeterPerAmpereIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterPerAmpereIdentifier; const ARight: TAmpereIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TMeterPerAmpereIdentifier): TMeterIdentifier;
begin end;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslaIdentifier; const ARight: TMeterIdentifier): TTeslaMeterIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterIdentifier; const ARight: TTeslaIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TTeslaIdentifier): TTeslaMeterIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterIdentifier; const ARight: TMeterIdentifier): TTeslaIdentifier;
begin end;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TAmpereIdentifier): TTeslaMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TTeslaMeterIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TTeslaMeterIdentifier; const ARight: TAmpereIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TTeslaMeterIdentifier): TNewtonIdentifier;
begin end;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslaIdentifier; const ARight: TAmpereIdentifier): TTeslaPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaIdentifier; const ARight: TTeslaPerAmpereIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TTeslaPerAmpereIdentifier; const ARight: TAmpereIdentifier): TTeslaIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TTeslaPerAmpereIdentifier): TTeslaIdentifier;
begin end;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenryIdentifier; const ARight: TMeterIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: THenryIdentifier; const ARight: THenryPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: THenryPerMeterIdentifier; const ARight: TMeterIdentifier): THenryIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: THenryPerMeterIdentifier): THenryIdentifier;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeterIdentifier; const ARight: TAmpereIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterIdentifier; const ARight: THenryPerMeterIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: THenryPerMeterIdentifier; const ARight: TAmpereIdentifier): TTeslaMeterIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: THenryPerMeterIdentifier): TTeslaMeterIdentifier;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslaPerAmpereIdentifier; const ARight: TMeterIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: THenryPerMeterIdentifier; const ARight: TTeslaPerAmpereIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TTeslaPerAmpereIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: THenryPerMeterIdentifier; const ARight: TMeterIdentifier): TTeslaPerAmpereIdentifier;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslaIdentifier; const ARight: TMeterPerAmpereIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: THenryPerMeterIdentifier; const ARight: TTeslaIdentifier): TMeterPerAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterPerAmpereIdentifier; const ARight: TTeslaIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: THenryPerMeterIdentifier; const ARight: TMeterPerAmpereIdentifier): TTeslaIdentifier;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslaIdentifier; const ARight: TAmperePerMeterIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: TTeslaIdentifier; const ARight: THenryPerMeterIdentifier): TAmperePerMeterIdentifier;
begin end;

operator *(const ALeft: THenryPerMeterIdentifier; const ARight: TAmperePerMeterIdentifier): TTeslaIdentifier;
begin end;

operator *(const ALeft: TAmperePerMeterIdentifier; const ARight: THenryPerMeterIdentifier): TTeslaIdentifier;
begin end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareAmpereIdentifier): THenryPerMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: THenryPerMeterIdentifier): TSquareAmpereIdentifier;
begin end;

operator *(const ALeft: THenryPerMeterIdentifier; const ARight: TSquareAmpereIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareAmpereIdentifier; const ARight: THenryPerMeterIdentifier): TNewtonIdentifier;
begin end;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadianIdentifier; const ARight: TMeterIdentifier): TRadianPerMeterIdentifier;
begin end;

operator /(const ALeft: TRadianIdentifier; const ARight: TRadianPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TRadianPerMeterIdentifier; const ARight: TMeterIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TRadianPerMeterIdentifier): TRadianIdentifier;
begin end;

// main definition [ J/deg ] = [ J ] / [ deg ]
operator /(const ALeft: TJouleIdentifier; const ARight: TDegreeIdentifier): TJoulePerDegreeIdentifier;
begin end;

// main definition [ km/h ] = [ km ] / [ h ]
operator /(const ALeft: TKilometerIdentifier; const ARight: THourIdentifier): TKilometerPerHourIdentifier;
begin end;

// main definition [ dm/s ] = [ dm ] / [ s ]
operator /(const ALeft: TDecimeterIdentifier; const ARight: TSecondIdentifier): TDecimeterPerSecondIdentifier;
begin end;

// main definition [ cm/s ] = [ cm ] / [ s ]
operator /(const ALeft: TCentimeterIdentifier; const ARight: TSecondIdentifier): TCentimeterPerSecondIdentifier;
begin end;

// main definition [ mm/s ] = [ mm ] / [ s ]
operator /(const ALeft: TMillimeterIdentifier; const ARight: TSecondIdentifier): TMillimeterPerSecondIdentifier;
begin end;

// main definition [ km/h/s ] = [ km/h ] / [ s ]
operator /(const ALeft: TKilometerPerHourIdentifier; const ARight: TSecondIdentifier): TKilometerPerHourPerSecondIdentifier;
begin end;

// main definition [ dm/s2 ] = [ dm ] / [ s2 ]
operator /(const ALeft: TDecimeterIdentifier; const ARight: TSquareSecondIdentifier): TDecimeterPerSquareSecondIdentifier;
begin end;

// main definition [ cm/s2 ] = [ cm ] / [ s2 ]
operator /(const ALeft: TCentimeterIdentifier; const ARight: TSquareSecondIdentifier): TCentimeterPerSquareSecondIdentifier;
begin end;

// main definition [ mm/s2 ] = [ mm ] / [ s2 ]
operator /(const ALeft: TMillimeterIdentifier; const ARight: TSquareSecondIdentifier): TMillimeterPerSquareSecondIdentifier;
begin end;

//
operator /(const ALeft: TKilogramIdentifier; const ARight: TCubicMillimeterIdentifier): TKilogramPerCubicMillimeterIdentifier;
begin end;

//
operator /(const ALeft: TKilogramIdentifier; const ARight: TCubicCentimeterIdentifier): TKilogramPerCubicCentimeterIdentifier;
begin end;

//
operator /(const ALeft: TKilogramIdentifier; const ARight: TCubicDecimeterIdentifier): TKilogramPerCubicDecimeterIdentifier;
begin end;

//
operator /(const ALeft: THectogramIdentifier; const ARight: TCubicMeterIdentifier): THectogramPerCubicMeterIdentifier;
begin end;

//
operator /(const ALeft: TDecagramIdentifier; const ARight: TCubicMeterIdentifier): TDecagramPerCubicMeterIdentifier;
begin end;

//
operator /(const ALeft: TGramIdentifier; const ARight: TCubicMeterIdentifier): TGramPerCubicMeterIdentifier;
begin end;

{ Combining quantities }

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

// main definition [ m3 ]
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

// main definition [ kg2 ]
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

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes;
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

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts;
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

// main definition [ m/s2 ] = [ m/s ] / [ s ]
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

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
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

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSteradiansPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSteradiansPerSquareSecond): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
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

// main definition [ sr ] = [ sr ] / [ s2 ]
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

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
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

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
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

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
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

{ Equivalences }

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

{ Helpers }

{ Helper for Bequerel }

function TBequerelHelper.From(const AQuantity: THertz): TBequerels;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for Gray }

function TGrayHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TGrays;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for Sievert }

function TSievertHelper.From(const AQuantity: TSquareMetersPerSquareSecond): TSieverts;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for NewtonMeter }

function TNewtonMeterHelper.From(const AQuantity: TJoules): TNewtonMeters;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for NewtonMeterPerRadian }

function TNewtonMeterPerRadianHelper.From(const AQuantity: TJoulesPerRadian): TNewtonMetersPerRadian;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for NewtonSecond }

function TNewtonSecondHelper.From(const AQuantity: TKilogramMetersPerSecond): TNewtonSeconds;
begin
  result.Value := AQuantity.Value;
end;

{ Helper for JoulePerKilogram }

function TJoulePerKilogramHelper.From(const AQuantity: TSquareMetersPerSquareKilogram): TJoulesPerKilogram;
begin
  result.Value := AQuantity.Value;
end;

end.
