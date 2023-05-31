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

  { TIdentifier }

  generic TIdentifier<U: TUnit> = record
    type TSelf = specialize TIdentifier<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;

  end;

  { TFactoredIdentifier }

  generic TFactoredIdentifier<BaseU: TUnit; U: TFactoredUnit> = record
    type TSelf = specialize TFactoredIdentifier<BaseU, U>;
    type TBaseQuantity = specialize TQuantity<BaseU>;
    type TFactoredQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TFactoredQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;
  end;

{ Unit of Second }

type
  TSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSecondId = specialize TIdentifier<TSecondUnit>;
  TSeconds = specialize TQuantity<TSecondUnit>;

var
  s: TSecondId;

{ Unit of SquareSecond }

type
  TSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareSecondId = specialize TIdentifier<TSquareSecondUnit>;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;

var
  s2: TSquareSecondId;

{ Unit of Meter }

type
  TMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterId = specialize TIdentifier<TMeterUnit>;
  TMeters = specialize TQuantity<TMeterUnit>;

var
  m: TMeterId;

{ Unit of SquareMeter }

type
  TSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterId = specialize TIdentifier<TSquareMeterUnit>;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;

var
  m2: TSquareMeterId;

{ Unit of CubicMeter }

type
  TCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicMeterId = specialize TIdentifier<TCubicMeterUnit>;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;

var
  m3: TCubicMeterId;

{ Unit of QuarticMeter }

type
  TQuarticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuarticMeterId = specialize TIdentifier<TQuarticMeterUnit>;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;

var
  m4: TQuarticMeterId;

{ Unit of QuinticMeter }

type
  TQuinticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuinticMeterId = specialize TIdentifier<TQuinticMeterUnit>;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;

var
  m5: TQuinticMeterId;

{ Unit of SexticMeter }

type
  TSexticMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSexticMeterId = specialize TIdentifier<TSexticMeterUnit>;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;

var
  m6: TSexticMeterId;

{ Unit of Kilogram }

type
  TKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramId = specialize TIdentifier<TKilogramUnit>;
  TKilograms = specialize TQuantity<TKilogramUnit>;

var
  kg: TKilogramId;

{ Unit of SquareKilogram }

type
  TSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramId = specialize TIdentifier<TSquareKilogramUnit>;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;

var
  kg2: TSquareKilogramId;

{ Unit of Ampere }

type
  TAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TAmpereId = specialize TIdentifier<TAmpereUnit>;
  TAmperes = specialize TQuantity<TAmpereUnit>;

var
  A: TAmpereId;

{ Unit of SquareAmpere }

type
  TSquareAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareAmpereId = specialize TIdentifier<TSquareAmpereUnit>;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;

var
  A2: TSquareAmpereId;

{ Unit of Kelvin }

type
  TKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKelvinId = specialize TIdentifier<TKelvinUnit>;
  TKelvins = specialize TQuantity<TKelvinUnit>;

var
  K: TKelvinId;

{ Unit of SquareKelvin }

type
  TSquareKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKelvinId = specialize TIdentifier<TSquareKelvinUnit>;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;

var
  K2: TSquareKelvinId;

{ Unit of CubicKelvin }

type
  TCubicKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicKelvinId = specialize TIdentifier<TCubicKelvinUnit>;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;

var
  K3: TCubicKelvinId;

{ Unit of QuarticKelvin }

type
  TQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuarticKelvinId = specialize TIdentifier<TQuarticKelvinUnit>;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;

{ Unit of Mole }

type
  TMoleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMoleId = specialize TIdentifier<TMoleUnit>;
  TMoles = specialize TQuantity<TMoleUnit>;

var
  mol: TMoleId;

{ Unit of Candela }

type
  TCandelaUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCandelaId = specialize TIdentifier<TCandelaUnit>;
  TCandelas = specialize TQuantity<TCandelaUnit>;

var
  cd: TCandelaId;

{ Unit of Radian }

type
  TRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianId = specialize TIdentifier<TRadianUnit>;
  TRadians = specialize TQuantity<TRadianUnit>;

var
  rad: TRadianId;

{ Unit of Steradian }

type
  TSteradianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSteradianId = specialize TIdentifier<TSteradianUnit>;
  TSteradians = specialize TQuantity<TSteradianUnit>;

var
  sr: TSteradianId;

{ Unit of Hertz }

type
  THertzUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THertzId = specialize TIdentifier<THertzUnit>;
  THertz = specialize TQuantity<THertzUnit>;

var
  Hz: THertzId;

{ Unit of SquareHertz }

type
  TSquareHertzUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareHertzId = specialize TIdentifier<TSquareHertzUnit>;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;

{ Unit of Newton }

type
  TNewtonUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonId = specialize TIdentifier<TNewtonUnit>;
  TNewtons = specialize TQuantity<TNewtonUnit>;

var
  N: TNewtonId;

{ Unit of Pascal }

type
  TPascalUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TPascalId = specialize TIdentifier<TPascalUnit>;
  TPascals = specialize TQuantity<TPascalUnit>;

var
  Pa: TPascalId;

{ Unit of Joule }

type
  TJouleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJouleId = specialize TIdentifier<TJouleUnit>;
  TJoules = specialize TQuantity<TJouleUnit>;

var
  J: TJouleId;

{ Unit of Watt }

type
  TWattUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattId = specialize TIdentifier<TWattUnit>;
  TWatts = specialize TQuantity<TWattUnit>;

var
  W: TWattId;

{ Unit of Coulomb }

type
  TCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombId = specialize TIdentifier<TCoulombUnit>;
  TCoulombs = specialize TQuantity<TCoulombUnit>;

var
  C: TCoulombId;

{ Unit of SquareCoulomb }

type
  TSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombId = specialize TIdentifier<TSquareCoulombUnit>;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;

var
  C2: TSquareCoulombId;

{ Unit of Volt }

type
  TVoltUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltId = specialize TIdentifier<TVoltUnit>;
  TVolts = specialize TQuantity<TVoltUnit>;

var
  V: TVoltId;

{ Unit of SquareVolt }

type
  TSquareVoltUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareVoltId = specialize TIdentifier<TSquareVoltUnit>;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;

var
  V2: TSquareVoltId;

{ Unit of Farad }

type
  TFaradUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TFaradId = specialize TIdentifier<TFaradUnit>;
  TFarads = specialize TQuantity<TFaradUnit>;

var
  F: TFaradId;

{ Unit of Ohm }

type
  TOhmUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TOhmId = specialize TIdentifier<TOhmUnit>;
  TOhms = specialize TQuantity<TOhmUnit>;

var
  ohm: TOhmId;

{ Unit of Siemens }

type
  TSiemensUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSiemensId = specialize TIdentifier<TSiemensUnit>;
  TSiemens = specialize TQuantity<TSiemensUnit>;

var
  siemens: TSiemensId;

{ Unit of Weber }

type
  TWeberUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWeberId = specialize TIdentifier<TWeberUnit>;
  TWebers = specialize TQuantity<TWeberUnit>;

var
  Wb: TWeberId;

{ Unit of Tesla }

type
  TTeslaUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaId = specialize TIdentifier<TTeslaUnit>;
  TTeslas = specialize TQuantity<TTeslaUnit>;

var
  T: TTeslaId;

{ Unit of Henry }

type
  THenryUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THenryId = specialize TIdentifier<THenryUnit>;
  THenrys = specialize TQuantity<THenryUnit>;

var
  H: THenryId;

{ Unit of Lumen }

type
  TLumenUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TLumenId = specialize TIdentifier<TLumenUnit>;
  TLumens = specialize TQuantity<TLumenUnit>;

var
  lm: TLumenId;

{ Unit of Lux }

type
  TLuxUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TLuxId = specialize TIdentifier<TLuxUnit>;
  TLux = specialize TQuantity<TLuxUnit>;

var
  lx: TLuxId;

{ Unit of Bequerel }

type
  TBequerelUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TBequerelId = specialize TIdentifier<TBequerelUnit>;
  TBequerels = specialize TQuantity<TBequerelUnit>;

var
  Bq: TBequerelId;

{ Unit of Gray }

type
  TGrayUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TGrayId = specialize TIdentifier<TGrayUnit>;
  TGrays = specialize TQuantity<TGrayUnit>;

var
  Gy: TGrayId;

{ Unit of Sievert }

type
  TSievertUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSievertId = specialize TIdentifier<TSievertUnit>;
  TSieverts = specialize TQuantity<TSievertUnit>;

var
  Sv: TSievertId;

{ Unit of Katal }

type
  TKatalUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKatalId = specialize TIdentifier<TKatalUnit>;
  TKatals = specialize TQuantity<TKatalUnit>;

var
  kat: TKatalId;

{ Unit of NewtonMeter }

type
  TNewtonMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterId = specialize TIdentifier<TNewtonMeterUnit>;
  TNewtonMeters = specialize TQuantity<TNewtonMeterUnit>;

{ Unit of JoulePerRadian }

type
  TJoulePerRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerRadianId = specialize TIdentifier<TJoulePerRadianUnit>;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;

{ Unit of NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterPerRadianId = specialize TIdentifier<TNewtonMeterPerRadianUnit>;
  TNewtonMetersPerRadian = specialize TQuantity<TNewtonMeterPerRadianUnit>;

{ Unit of MeterPerSecond }

type
  TMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerSecondId = specialize TIdentifier<TMeterPerSecondUnit>;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;

{ Unit of MeterPerSquareSecond }

type
  TMeterPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerSquareSecondId = specialize TIdentifier<TMeterPerSquareSecondUnit>;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;

{ Unit of RadianPerSecond }

type
  TRadianPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerSecondId = specialize TIdentifier<TRadianPerSecondUnit>;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;

{ Unit of RadianPerSquareSecond }

type
  TRadianPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerSquareSecondId = specialize TIdentifier<TRadianPerSquareSecondUnit>;
  TRadiansPerSquareSecond = specialize TQuantity<TRadianPerSquareSecondUnit>;

{ Unit of KilogramPerMeter }

type
  TKilogramPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerMeterId = specialize TIdentifier<TKilogramPerMeterUnit>;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;

{ Unit of KilogramPerSquareMeter }

type
  TKilogramPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerSquareMeterId = specialize TIdentifier<TKilogramPerSquareMeterUnit>;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;

{ Unit of KilogramPerCubicMeter }

type
  TKilogramPerCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerCubicMeterId = specialize TIdentifier<TKilogramPerCubicMeterUnit>;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;

{ Unit of NewtonPerCubicMeter }

type
  TNewtonPerCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerCubicMeterId = specialize TIdentifier<TNewtonPerCubicMeterUnit>;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;

{ Unit of NewtonPerMeter }

type
  TNewtonPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerMeterId = specialize TIdentifier<TNewtonPerMeterUnit>;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;

{ Unit of KilogramMeterPerSecond }

type
  TKilogramMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramMeterPerSecondId = specialize TIdentifier<TKilogramMeterPerSecondUnit>;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;

{ Unit of NewtonSecond }

type
  TNewtonSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSecondId = specialize TIdentifier<TNewtonSecondUnit>;
  TNewtonSeconds = specialize TQuantity<TNewtonSecondUnit>;

{ Unit of KilogramSquareMeter }

type
  TKilogramSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterId = specialize TIdentifier<TKilogramSquareMeterUnit>;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;

{ Unit of KilogramSquareMeterPerSecond }

type
  TKilogramSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterPerSecondId = specialize TIdentifier<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;

{ Unit of SquareMeterPerSquareSecond }

type
  TSquareMeterPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareSecondId = specialize TIdentifier<TSquareMeterPerSquareSecondUnit>;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;

{ Unit of SteradianPerSquareSecond }

type
  TSteradianPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSteradianPerSquareSecondId = specialize TIdentifier<TSteradianPerSquareSecondUnit>;
  TSteradiansPerSquareSecond = specialize TQuantity<TSteradianPerSquareSecondUnit>;

{ Unit of CubicMeterPerSecond }

type
  TCubicMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicMeterPerSecondId = specialize TIdentifier<TCubicMeterPerSecondUnit>;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;

{ Unit of PascalSecond }

type
  TPascalSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TPascalSecondId = specialize TIdentifier<TPascalSecondUnit>;
  TPascalSeconds = specialize TQuantity<TPascalSecondUnit>;

{ Unit of SquareMeterPerSecond }

type
  TSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSecondId = specialize TIdentifier<TSquareMeterPerSecondUnit>;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;

{ Unit of NewtonPerSquareKilogram }

type
  TNewtonPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareKilogramId = specialize TIdentifier<TNewtonPerSquareKilogramUnit>;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;

{ Unit of SquareKilogramPerMeter }

type
  TSquareKilogramPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramPerMeterId = specialize TIdentifier<TSquareKilogramPerMeterUnit>;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;

{ Unit of SquareKilogramPerSquareMeter }

type
  TSquareKilogramPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareKilogramPerSquareMeterId = specialize TIdentifier<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareKilogram }

type
  TSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareKilogramId = specialize TIdentifier<TSquareMeterPerSquareKilogramUnit>;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;

{ Unit of NewtonSquareMeterPerSquareKilogram }

type
  TNewtonSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareKilogramId = specialize TIdentifier<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;

{ Unit of ReciprocalKelvin }

type
  TReciprocalKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TReciprocalKelvinId = specialize TIdentifier<TReciprocalKelvinUnit>;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;

{ Unit of KilogramKelvin }

type
  TKilogramKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramKelvinId = specialize TIdentifier<TKilogramKelvinUnit>;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;

{ Unit of JoulePerKelvin }

type
  TJoulePerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKelvinId = specialize TIdentifier<TJoulePerKelvinUnit>;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;

{ Unit of JoulePerKilogram }

type
  TJoulePerKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKilogramId = specialize TIdentifier<TJoulePerKilogramUnit>;
  TJoulesPerKilogram = specialize TQuantity<TJoulePerKilogramUnit>;

{ Unit of JoulePerKilogramPerKelvin }

type
  TJoulePerKilogramPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerKilogramPerKelvinId = specialize TIdentifier<TJoulePerKilogramPerKelvinUnit>;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;

{ Unit of MeterKelvin }

type
  TMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterKelvinId = specialize TIdentifier<TMeterKelvinUnit>;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;

{ Unit of KelvinPerMeter }

type
  TKelvinPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKelvinPerMeterId = specialize TIdentifier<TKelvinPerMeterUnit>;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;

{ Unit of WattPerMeter }

type
  TWattPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterId = specialize TIdentifier<TWattPerMeterUnit>;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;

{ Unit of WattPerSquareMeter }

type
  TWattPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterId = specialize TIdentifier<TWattPerSquareMeterUnit>;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;

{ Unit of WattPerKelvin }

type
  TWattPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerKelvinId = specialize TIdentifier<TWattPerKelvinUnit>;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;

{ Unit of WattPerMeterPerKelvin }

type
  TWattPerMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterPerKelvinId = specialize TIdentifier<TWattPerMeterPerKelvinUnit>;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;

{ Unit of SquareMeterKelvin }

type
  TSquareMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterKelvinId = specialize TIdentifier<TSquareMeterKelvinUnit>;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;

{ Unit of WattPerSquareMeterPerKelvin }

type
  TWattPerSquareMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerKelvinId = specialize TIdentifier<TWattPerSquareMeterPerKelvinUnit>;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;

{ Unit of SquareMeterQuarticKelvin }

type
  TSquareMeterQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterQuarticKelvinId = specialize TIdentifier<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;

{ Unit of WattPerQuarticKelvin }

type
  TWattPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerQuarticKelvinId = specialize TIdentifier<TWattPerQuarticKelvinUnit>;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;

{ Unit of WattPerSquareMeterPerQuarticKelvin }

type
  TWattPerSquareMeterPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerQuarticKelvinId = specialize TIdentifier<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;

{ Unit of JoulePerMole }

type
  TJoulePerMoleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMoleId = specialize TIdentifier<TJoulePerMoleUnit>;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;

{ Unit of MoleKelvin }

type
  TMoleKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMoleKelvinId = specialize TIdentifier<TMoleKelvinUnit>;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;

{ Unit of JoulePerMolePerKelvin }

type
  TJoulePerMolePerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMolePerKelvinId = specialize TIdentifier<TJoulePerMolePerKelvinUnit>;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;

{ Unit of OhmMeter }

type
  TOhmMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TOhmMeterId = specialize TIdentifier<TOhmMeterUnit>;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;

{ Unit of VoltPerMeter }

type
  TVoltPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltPerMeterId = specialize TIdentifier<TVoltPerMeterUnit>;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;

{ Unit of CoulombPerMeter }

type
  TCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerMeterId = specialize TIdentifier<TCoulombPerMeterUnit>;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;

{ Unit of SquareCoulombPerMeter }

type
  TSquareCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerMeterId = specialize TIdentifier<TSquareCoulombPerMeterUnit>;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;

{ Unit of CoulombPerSquareMeter }

type
  TCoulombPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerSquareMeterId = specialize TIdentifier<TCoulombPerSquareMeterUnit>;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareCoulomb }

type
  TSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareCoulombId = specialize TIdentifier<TSquareMeterPerSquareCoulombUnit>;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;

{ Unit of NewtonPerSquareCoulomb }

type
  TNewtonPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareCoulombId = specialize TIdentifier<TNewtonPerSquareCoulombUnit>;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;

{ Unit of NewtonSquareMeter }

type
  TNewtonSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterId = specialize TIdentifier<TNewtonSquareMeterUnit>;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;

{ Unit of NewtonSquareMeterPerSquareCoulomb }

type
  TNewtonSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareCoulombId = specialize TIdentifier<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;

{ Unit of VoltMeter }

type
  TVoltMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterId = specialize TIdentifier<TVoltMeterUnit>;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;

var
  Vm: TVoltMeterId;

{ Unit of VoltMeterPerSecond }

type
  TVoltMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterPerSecondId = specialize TIdentifier<TVoltMeterPerSecondUnit>;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;

{ Unit of FaradPerMeter }

type
  TFaradPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TFaradPerMeterId = specialize TIdentifier<TFaradPerMeterUnit>;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;

{ Unit of AmperePerMeter }

type
  TAmperePerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TAmperePerMeterId = specialize TIdentifier<TAmperePerMeterUnit>;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;

{ Unit of MeterPerAmpere }

type
  TMeterPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerAmpereId = specialize TIdentifier<TMeterPerAmpereUnit>;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;

{ Unit of TeslaMeter }

type
  TTeslaMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaMeterId = specialize TIdentifier<TTeslaMeterUnit>;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;

var
  Tm: TTeslaMeterId;

{ Unit of TeslaPerAmpere }

type
  TTeslaPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaPerAmpereId = specialize TIdentifier<TTeslaPerAmpereUnit>;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;

{ Unit of HenryPerMeter }

type
  THenryPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  THenryPerMeterId = specialize TIdentifier<THenryPerMeterUnit>;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;

{ Unit of RadianPerMeter }

type
  TRadianPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TRadianPerMeterId = specialize TIdentifier<TRadianPerMeterUnit>;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;

{ Unit of Day }

type
  TDayUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDayId = specialize TFactoredIdentifier<TSecondUnit, TDayUnit>;

var
  day: TDayId;

{ Unit of Hour }

type
  THourUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THourId = specialize TFactoredIdentifier<TSecondUnit, THourUnit>;

var
  hour: THourId;

{ Unit of Minute }

type
  TMinuteUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMinuteId = specialize TFactoredIdentifier<TSecondUnit, TMinuteUnit>;

var
  minute: TMinuteId;

{ Unit of Decisecond }

type
  TDecisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecisecondId = specialize TFactoredIdentifier<TSecondUnit, TDecisecondUnit>;

var
  ds: TDecisecondId;

{ Unit of Centisecond }

type
  TCentisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentisecondId = specialize TFactoredIdentifier<TSecondUnit, TCentisecondUnit>;

var
  cs: TCentisecondId;

{ Unit of Millisecond }

type
  TMillisecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillisecondId = specialize TFactoredIdentifier<TSecondUnit, TMillisecondUnit>;

var
  ms: TMillisecondId;

{ Unit of Microsecond }

type
  TMicrosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrosecondId = specialize TFactoredIdentifier<TSecondUnit, TMicrosecondUnit>;

var
  us: TMicrosecondId;

{ Unit of Nanosecond }

type
  TNanosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanosecondId = specialize TFactoredIdentifier<TSecondUnit, TNanosecondUnit>;

var
  ns: TNanosecondId;

{ Unit of Picosecond }

type
  TPicosecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicosecondId = specialize TFactoredIdentifier<TSecondUnit, TPicosecondUnit>;

var
  ps: TPicosecondId;

{ Unit of Kilometer }

type
  TKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerId = specialize TFactoredIdentifier<TMeterUnit, TKilometerUnit>;

var
  km: TKilometerId;

{ Unit of Hectometer }

type
  THectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectometerId = specialize TFactoredIdentifier<TMeterUnit, THectometerUnit>;

var
  hm: THectometerId;

{ Unit of Decameter }

type
  TDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecameterId = specialize TFactoredIdentifier<TMeterUnit, TDecameterUnit>;

var
  dam: TDecameterId;

{ Unit of Decimeter }

type
  TDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterId = specialize TFactoredIdentifier<TMeterUnit, TDecimeterUnit>;

var
  dm: TDecimeterId;

{ Unit of Centimeter }

type
  TCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterId = specialize TFactoredIdentifier<TMeterUnit, TCentimeterUnit>;

var
  cm: TCentimeterId;

{ Unit of Millimeter }

type
  TMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterId = specialize TFactoredIdentifier<TMeterUnit, TMillimeterUnit>;

var
  mm: TMillimeterId;

{ Unit of Micrometer }

type
  TMicrometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrometerId = specialize TFactoredIdentifier<TMeterUnit, TMicrometerUnit>;

var
  um: TMicrometerId;

{ Unit of Nanometer }

type
  TNanometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanometerId = specialize TFactoredIdentifier<TMeterUnit, TNanometerUnit>;

var
  nm: TNanometerId;

{ Unit of Picometer }

type
  TPicometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicometerId = specialize TFactoredIdentifier<TMeterUnit, TPicometerUnit>;

var
  pm: TPicometerId;

{ Unit of SquareKilometer }

type
  TSquareKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareKilometerId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareKilometerUnit>;

var
  km2: TSquareKilometerId;

{ Unit of SquareHectometer }

type
  TSquareHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareHectometerId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareHectometerUnit>;

var
  hm2: TSquareHectometerId;

{ Unit of SquareDecameter }

type
  TSquareDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareDecameterId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareDecameterUnit>;

var
  dam2: TSquareDecameterId;

{ Unit of SquareDecimeter }

type
  TSquareDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareDecimeterId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareDecimeterUnit>;

var
  dm2: TSquareDecimeterId;

{ Unit of SquareCentimeter }

type
  TSquareCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareCentimeterId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareCentimeterUnit>;

var
  cm2: TSquareCentimeterId;

{ Unit of SquareMillimeter }

type
  TSquareMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareMillimeterId = specialize TFactoredIdentifier<TSquareMeterUnit, TSquareMillimeterUnit>;

var
  mm2: TSquareMillimeterId;

{ Unit of CubicKilometer }

type
  TCubicKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicKilometerId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicKilometerUnit>;

var
  km3: TCubicKilometerId;

{ Unit of CubicHectometer }

type
  TCubicHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicHectometerId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicHectometerUnit>;

var
  hm3: TCubicHectometerId;

{ Unit of CubicDecameter }

type
  TCubicDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicDecameterId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicDecameterUnit>;

var
  dam3: TCubicDecameterId;

{ Unit of CubicDecimeter }

type
  TCubicDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicDecimeterId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicDecimeterUnit>;

var
  dm3: TCubicDecimeterId;

{ Unit of CubicCentimeter }

type
  TCubicCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicCentimeterId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicCentimeterUnit>;

var
  cm3: TCubicCentimeterId;

{ Unit of CubicMillimeter }

type
  TCubicMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCubicMillimeterId = specialize TFactoredIdentifier<TCubicMeterUnit, TCubicMillimeterUnit>;

var
  mm3: TCubicMillimeterId;

{ Unit of QuarticKilometer }

type
  TQuarticKilometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticKilometerId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticKilometerUnit>;

var
  km4: TQuarticKilometerId;

{ Unit of QuarticHectometer }

type
  TQuarticHectometerUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticHectometerId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticHectometerUnit>;

var
  hm4: TQuarticHectometerId;

{ Unit of QuarticDecameter }

type
  TQuarticDecameterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticDecameterId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticDecameterUnit>;

var
  dam4: TQuarticDecameterId;

{ Unit of QuarticDecimeter }

type
  TQuarticDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticDecimeterId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticDecimeterUnit>;

var
  dm4: TQuarticDecimeterId;

{ Unit of QuarticCentimeter }

type
  TQuarticCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticCentimeterId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticCentimeterUnit>;

var
  cm4: TQuarticCentimeterId;

{ Unit of QuarticMillimeter }

type
  TQuarticMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TQuarticMillimeterId = specialize TFactoredIdentifier<TQuarticMeterUnit, TQuarticMillimeterUnit>;

var
  mm4: TQuarticMillimeterId;

{ Unit of Hectogram }

type
  THectogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectogramId = specialize TFactoredIdentifier<TKilogramUnit, THectogramUnit>;

var
  hg: THectogramId;

{ Unit of Decagram }

type
  TDecagramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecagramId = specialize TFactoredIdentifier<TKilogramUnit, TDecagramUnit>;

var
  dag: TDecagramId;

{ Unit of Gram }

type
  TGramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGramId = specialize TFactoredIdentifier<TKilogramUnit, TGramUnit>;

var
  g: TGramId;

{ Unit of Decigram }

type
  TDecigramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecigramId = specialize TFactoredIdentifier<TKilogramUnit, TDecigramUnit>;

var
  dg: TDecigramId;

{ Unit of Centigram }

type
  TCentigramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentigramId = specialize TFactoredIdentifier<TKilogramUnit, TCentigramUnit>;

var
  cg: TCentigramId;

{ Unit of Milligram }

type
  TMilligramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilligramId = specialize TFactoredIdentifier<TKilogramUnit, TMilligramUnit>;

var
  mg: TMilligramId;

{ Unit of Microgram }

type
  TMicrogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrogramId = specialize TFactoredIdentifier<TKilogramUnit, TMicrogramUnit>;

var
  ug: TMicrogramId;

{ Unit of Nanogram }

type
  TNanogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanogramId = specialize TFactoredIdentifier<TKilogramUnit, TNanogramUnit>;

var
  ng: TNanogramId;

{ Unit of Picogram }

type
  TPicogramUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicogramId = specialize TFactoredIdentifier<TKilogramUnit, TPicogramUnit>;

var
  pg: TPicogramId;

{ Unit of Kiloampere }

type
  TKiloampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKiloampereId = specialize TFactoredIdentifier<TAmpereUnit, TKiloampereUnit>;

var
  kA: TKiloampereId;

{ Unit of Hectoampere }

type
  THectoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectoampereId = specialize TFactoredIdentifier<TAmpereUnit, THectoampereUnit>;

var
  hA: THectoampereId;

{ Unit of Decampere }

type
  TDecampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecampereId = specialize TFactoredIdentifier<TAmpereUnit, TDecampereUnit>;

var
  daA: TDecampereId;

{ Unit of Deciampere }

type
  TDeciampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDeciampereId = specialize TFactoredIdentifier<TAmpereUnit, TDeciampereUnit>;

var
  dA: TDeciampereId;

{ Unit of Centiampere }

type
  TCentiampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentiampereId = specialize TFactoredIdentifier<TAmpereUnit, TCentiampereUnit>;

var
  cA: TCentiampereId;

{ Unit of Milliampere }

type
  TMilliampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliampereId = specialize TFactoredIdentifier<TAmpereUnit, TMilliampereUnit>;

var
  mA: TMilliampereId;

{ Unit of Microampere }

type
  TMicroampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicroampereId = specialize TFactoredIdentifier<TAmpereUnit, TMicroampereUnit>;

var
  uA: TMicroampereId;

{ Unit of Nanoampere }

type
  TNanoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanoampereId = specialize TFactoredIdentifier<TAmpereUnit, TNanoampereUnit>;

var
  nA: TNanoampereId;

{ Unit of Picoampere }

type
  TPicoampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicoampereId = specialize TFactoredIdentifier<TAmpereUnit, TPicoampereUnit>;

var
  picoampere: TPicoampereId;

{ Unit of SquareMilliampere }

type
  TSquareMilliampereUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TSquareMilliampereId = specialize TFactoredIdentifier<TSquareAmpereUnit, TSquareMilliampereUnit>;

var
  mA2: TSquareMilliampereId;

{ Unit of Degree }

type
  TDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDegreeId = specialize TFactoredIdentifier<TRadianUnit, TDegreeUnit>;

var
  deg: TDegreeId;

{ Unit of Gigahertz }

type
  TGigahertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigahertzId = specialize TFactoredIdentifier<THertzUnit, TGigahertzUnit>;

var
  GHz: TGigahertzId;

{ Unit of Megahertz }

type
  TMegahertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegahertzId = specialize TFactoredIdentifier<THertzUnit, TMegahertzUnit>;

var
  MHz: TMegahertzId;

{ Unit of Kilohertz }

type
  TKilohertzUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilohertzId = specialize TFactoredIdentifier<THertzUnit, TKilohertzUnit>;

var
  kHz: TKilohertzId;

{ Unit of Giganewton }

type
  TGiganewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGiganewtonId = specialize TFactoredIdentifier<TNewtonUnit, TGiganewtonUnit>;

var
  GN: TGiganewtonId;

{ Unit of Meganewton }

type
  TMeganewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMeganewtonId = specialize TFactoredIdentifier<TNewtonUnit, TMeganewtonUnit>;

var
  MN: TMeganewtonId;

{ Unit of Kilonewton }

type
  TKilonewtonUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilonewtonId = specialize TFactoredIdentifier<TNewtonUnit, TKilonewtonUnit>;

var
  kN: TKilonewtonId;

{ Unit of Gigapascal }

type
  TGigapascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigapascalId = specialize TFactoredIdentifier<TPascalUnit, TGigapascalUnit>;

var
  GPa: TGigapascalId;

{ Unit of Megapascal }

type
  TMegapascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegapascalId = specialize TFactoredIdentifier<TPascalUnit, TMegapascalUnit>;

var
  MPa: TMegapascalId;

{ Unit of Kilopascal }

type
  TKilopascalUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilopascalId = specialize TFactoredIdentifier<TPascalUnit, TKilopascalUnit>;

var
  kPa: TKilopascalId;

{ Unit of Gigajoule }

type
  TGigajouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigajouleId = specialize TFactoredIdentifier<TJouleUnit, TGigajouleUnit>;

var
  GJ: TGigajouleId;

{ Unit of Megajoule }

type
  TMegajouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegajouleId = specialize TFactoredIdentifier<TJouleUnit, TMegajouleUnit>;

var
  MJ: TMegajouleId;

{ Unit of Kilojoule }

type
  TKilojouleUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilojouleId = specialize TFactoredIdentifier<TJouleUnit, TKilojouleUnit>;

var
  kJ: TKilojouleId;

{ Unit of Gigawatt }

type
  TGigawattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigawattId = specialize TFactoredIdentifier<TWattUnit, TGigawattUnit>;

var
  GW: TGigawattId;

{ Unit of Megawatt }

type
  TMegawattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegawattId = specialize TFactoredIdentifier<TWattUnit, TMegawattUnit>;

var
  megawatt: TMegawattId;

{ Unit of Kilowatt }

type
  TKilowattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilowattId = specialize TFactoredIdentifier<TWattUnit, TKilowattUnit>;

var
  kW: TKilowattId;

{ Unit of Milliwatt }

type
  TMilliwattUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliwattId = specialize TFactoredIdentifier<TWattUnit, TMilliwattUnit>;

var
  mW: TMilliwattId;

{ Unit of Gigacoulomb }

type
  TGigacoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigacoulombId = specialize TFactoredIdentifier<TCoulombUnit, TGigacoulombUnit>;

var
  GC: TGigacoulombId;

{ Unit of Megacoulomb }

type
  TMegacoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegacoulombId = specialize TFactoredIdentifier<TCoulombUnit, TMegacoulombUnit>;

var
  megacoulomb: TMegacoulombId;

{ Unit of Kilocoulomb }

type
  TKilocoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilocoulombId = specialize TFactoredIdentifier<TCoulombUnit, TKilocoulombUnit>;

var
  kC: TKilocoulombId;

{ Unit of Millicoulomb }

type
  TMillicoulombUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillicoulombId = specialize TFactoredIdentifier<TCoulombUnit, TMillicoulombUnit>;

var
  mC: TMillicoulombId;

{ Unit of Gigavolt }

type
  TGigavoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigavoltId = specialize TFactoredIdentifier<TVoltUnit, TGigavoltUnit>;

var
  GV: TGigavoltId;

{ Unit of Megavolt }

type
  TMegavoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegavoltId = specialize TFactoredIdentifier<TVoltUnit, TMegavoltUnit>;

var
  megavolt: TMegavoltId;

{ Unit of Kilovolt }

type
  TKilovoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilovoltId = specialize TFactoredIdentifier<TVoltUnit, TKilovoltUnit>;

var
  kV: TKilovoltId;

{ Unit of Millivolt }

type
  TMillivoltUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillivoltId = specialize TFactoredIdentifier<TVoltUnit, TMillivoltUnit>;

var
  mV: TMillivoltId;

{ Unit of Gigafarad }

type
  TGigafaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigafaradId = specialize TFactoredIdentifier<TFaradUnit, TGigafaradUnit>;

var
  GF: TGigafaradId;

{ Unit of Megafarad }

type
  TMegafaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegafaradId = specialize TFactoredIdentifier<TFaradUnit, TMegafaradUnit>;

var
  megafarad: TMegafaradId;

{ Unit of Kilofarad }

type
  TKilofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilofaradId = specialize TFactoredIdentifier<TFaradUnit, TKilofaradUnit>;

var
  kF: TKilofaradId;

{ Unit of Millifarad }

type
  TMillifaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillifaradId = specialize TFactoredIdentifier<TFaradUnit, TMillifaradUnit>;

var
  mF: TMillifaradId;

{ Unit of Microfarad }

type
  TMicrofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicrofaradId = specialize TFactoredIdentifier<TFaradUnit, TMicrofaradUnit>;

var
  uF: TMicrofaradId;

{ Unit of Nanofarad }

type
  TNanofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanofaradId = specialize TFactoredIdentifier<TFaradUnit, TNanofaradUnit>;

var
  nF: TNanofaradId;

{ Unit of Picofarad }

type
  TPicofaradUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicofaradId = specialize TFactoredIdentifier<TFaradUnit, TPicofaradUnit>;

var
  pF: TPicofaradId;

{ Unit of Gigaohm }

type
  TGigaohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGigaohmId = specialize TFactoredIdentifier<TOhmUnit, TGigaohmUnit>;

var
  gigaohm: TGigaohmId;

{ Unit of Megaohm }

type
  TMegaohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMegaohmId = specialize TFactoredIdentifier<TOhmUnit, TMegaohmUnit>;

var
  megaohm: TMegaohmId;

{ Unit of Kiloohm }

type
  TKiloohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKiloohmId = specialize TFactoredIdentifier<TOhmUnit, TKiloohmUnit>;

var
  kiloohm: TKiloohmId;

{ Unit of Milliohm }

type
  TMilliohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliohmId = specialize TFactoredIdentifier<TOhmUnit, TMilliohmUnit>;

var
  milliohm: TMilliohmId;

{ Unit of Microohm }

type
  TMicroohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMicroohmId = specialize TFactoredIdentifier<TOhmUnit, TMicroohmUnit>;

var
  microohm: TMicroohmId;

{ Unit of Nanoohm }

type
  TNanoohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNanoohmId = specialize TFactoredIdentifier<TOhmUnit, TNanoohmUnit>;

var
  nanoohm: TNanoohmId;

{ Unit of Picoohm }

type
  TPicoohmUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TPicoohmId = specialize TFactoredIdentifier<TOhmUnit, TPicoohmUnit>;

var
  picoohm: TPicoohmId;

{ Unit of Milligray }

type
  TMilligrayUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilligrayId = specialize TFactoredIdentifier<TGrayUnit, TMilligrayUnit>;

var
  mGy: TMilligrayId;

{ Unit of MilliSievert }

type
  TMilliSievertUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMilliSievertId = specialize TFactoredIdentifier<TSievertUnit, TMilliSievertUnit>;

var
  mSv: TMilliSievertId;

{ Unit of JoulePerDegree }

type
  TJoulePerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TJoulePerDegreeId = specialize TFactoredIdentifier<TJoulePerRadianUnit, TJoulePerDegreeUnit>;

{ Unit of NewtonMeterPerDegree }

type
  TNewtonMeterPerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMeterPerDegreeId = specialize TFactoredIdentifier<TJoulePerRadianUnit, TNewtonMeterPerDegreeUnit>;

{ Unit of KilometerPerHour }

type
  TKilometerPerHourUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerPerHourId = specialize TFactoredIdentifier<TMeterPerSecondUnit, TKilometerPerHourUnit>;

{ Unit of DecimeterPerSecond }

type
  TDecimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterPerSecondId = specialize TFactoredIdentifier<TMeterPerSecondUnit, TDecimeterPerSecondUnit>;

{ Unit of CentimeterPerSecond }

type
  TCentimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterPerSecondId = specialize TFactoredIdentifier<TMeterPerSecondUnit, TCentimeterPerSecondUnit>;

{ Unit of MillimeterPerSecond }

type
  TMillimeterPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterPerSecondId = specialize TFactoredIdentifier<TMeterPerSecondUnit, TMillimeterPerSecondUnit>;

{ Unit of KilometerPerHourPerSecond }

type
  TKilometerPerHourPerSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilometerPerHourPerSecondId = specialize TFactoredIdentifier<TMeterPerSquareSecondUnit, TKilometerPerHourPerSecondUnit>;

{ Unit of DecimeterPerSquareSecond }

type
  TDecimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecimeterPerSquareSecondId = specialize TFactoredIdentifier<TMeterPerSquareSecondUnit, TDecimeterPerSquareSecondUnit>;

{ Unit of CentimeterPerSquareSecond }

type
  TCentimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TCentimeterPerSquareSecondId = specialize TFactoredIdentifier<TMeterPerSquareSecondUnit, TCentimeterPerSquareSecondUnit>;

{ Unit of MillimeterPerSquareSecond }

type
  TMillimeterPerSquareSecondUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TMillimeterPerSquareSecondId = specialize TFactoredIdentifier<TMeterPerSquareSecondUnit, TMillimeterPerSquareSecondUnit>;

{ Unit of KilogramPerCubicMillimeter }

type
  TKilogramPerCubicMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicMillimeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicMillimeterUnit>;

{ Unit of KilogramPerCubicCentimeter }

type
  TKilogramPerCubicCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicCentimeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicCentimeterUnit>;

{ Unit of KilogramPerCubicDecimeter }

type
  TKilogramPerCubicDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TKilogramPerCubicDecimeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, TKilogramPerCubicDecimeterUnit>;

{ Unit of HectogramPerCubicMeter }

type
  THectogramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  THectogramPerCubicMeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, THectogramPerCubicMeterUnit>;

{ Unit of DecagramPerCubicMeter }

type
  TDecagramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TDecagramPerCubicMeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, TDecagramPerCubicMeterUnit>;

{ Unit of GramPerCubicMeter }

type
  TGramPerCubicMeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TGramPerCubicMeterId = specialize TFactoredIdentifier<TKilogramPerCubicMeterUnit, TGramPerCubicMeterUnit>;

{ Unit of NewtonPerMillimeter }

type
  TNewtonPerMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonPerMillimeterId = specialize TFactoredIdentifier<TNewtonPerMeterUnit, TNewtonPerMillimeterUnit>;

{ Combining units }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TSecondId): TSquareSecondId; inline;
operator /(const {%H-}ALeft: TSquareSecondId; const {%H-}ARight: TSecondId): TSecondId; inline;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterId): TSquareMeterId; inline;
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterId): TMeterId; inline;

// main definition [ m3 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterId): TCubicMeterId; inline;
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSquareMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareMeterId): TCubicMeterId; inline;
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TMeterId): TSquareMeterId; inline;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TMeterId): TQuarticMeterId; inline;
operator /(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TCubicMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TCubicMeterId): TQuarticMeterId; inline;
operator /(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TMeterId): TCubicMeterId; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterId): TQuarticMeterId; inline;
operator /(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TSquareMeterId): TSquareMeterId; inline;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TMeterId): TQuinticMeterId; inline;
operator /(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TQuarticMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TQuarticMeterId): TQuinticMeterId; inline;
operator /(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TMeterId): TQuarticMeterId; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSquareMeterId): TQuinticMeterId; inline;
operator /(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TCubicMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TCubicMeterId): TQuinticMeterId; inline;
operator /(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TSquareMeterId): TCubicMeterId; inline;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TMeterId): TSexticMeterId; inline;
operator /(const {%H-}ALeft: TSexticMeterId; const {%H-}ARight: TQuinticMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TQuinticMeterId): TSexticMeterId; inline;
operator /(const {%H-}ALeft: TSexticMeterId; const {%H-}ARight: TMeterId): TQuinticMeterId; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TSquareMeterId): TSexticMeterId; inline;
operator /(const {%H-}ALeft: TSexticMeterId; const {%H-}ARight: TQuarticMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TQuarticMeterId): TSexticMeterId; inline;
operator /(const {%H-}ALeft: TSexticMeterId; const {%H-}ARight: TSquareMeterId): TQuarticMeterId; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TCubicMeterId): TSexticMeterId; inline;
operator /(const {%H-}ALeft: TSexticMeterId; const {%H-}ARight: TCubicMeterId): TCubicMeterId; inline;

// main definition [ kg2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramId): TSquareKilogramId; inline;
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TKilogramId): TKilogramId; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TAmpereId): TSquareAmpereId; inline;
operator /(const {%H-}ALeft: TSquareAmpereId; const {%H-}ARight: TAmpereId): TAmpereId; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TKelvinId): TSquareKelvinId; inline;
operator /(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TKelvinId): TKelvinId; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TKelvinId): TCubicKelvinId; inline;
operator /(const {%H-}ALeft: TCubicKelvinId; const {%H-}ARight: TSquareKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TSquareKelvinId): TCubicKelvinId; inline;
operator /(const {%H-}ALeft: TCubicKelvinId; const {%H-}ARight: TKelvinId): TSquareKelvinId; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TSquareKelvinId): TQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TSquareKelvinId): TSquareKelvinId; inline;

//
operator *(const {%H-}ALeft: TCubicKelvinId; const {%H-}ARight: TKelvinId): TQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TCubicKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TCubicKelvinId): TQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TKelvinId): TCubicKelvinId; inline;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianId): TSteradianId; inline;
operator /(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TRadianId): TRadianId; inline;

// main definition [ Hz ] = 1 / [ s ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSecondId): THertzId; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: THertzId): TSecondId; inline;
operator *(const {%H-}ALeft: THertzId; const {%H-}ARight: TSecondId): double; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: THertzId): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const {%H-}ALeft: THertzId; const {%H-}ARight: THertzId): TSquareHertzId; inline;
operator /(const {%H-}ALeft: TSquareHertzId; const {%H-}ARight: THertzId): THertzId; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TKilogramId): TMeterPerSquareSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TKilogramId): TNewtonId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterPerSquareSecondId): TKilogramId; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TPascalId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TPascalId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TSquareMeterId): TNewtonId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TPascalId): TNewtonId; inline;

// main definition [ J ] = [ N ] * [ m ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TJouleId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TNewtonId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TNewtonId): TJouleId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMeterId): TNewtonId; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TCubicMeterId): TJouleId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TPascalId): TCubicMeterId; inline;
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TPascalId): TJouleId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TCubicMeterId): TPascalId; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSecondId): TWattId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TWattId): TSecondId; inline;
operator *(const {%H-}ALeft: TWattId; const {%H-}ARight: TSecondId): TJouleId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TWattId): TJouleId; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianPerSecondId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TJouleId): TRadianPerSecondId; inline;
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TJouleId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TRadianPerSecondId): TJouleId; inline;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const {%H-}ALeft: TSquareAmpereId; const {%H-}ARight: TOhmId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareAmpereId): TOhmId; inline;
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TSquareAmpereId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TOhmId): TSquareAmpereId; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterPerSecondId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TNewtonId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TNewtonId): TWattId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterPerSecondId): TNewtonId; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TAmpereId): TCoulombId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TSecondId): TAmpereId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TSecondId): TCoulombId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TAmpereId): TSecondId; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombId): TSquareCoulombId; inline;
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TCoulombId): TCoulombId; inline;

// main definition [ V ] = [ W ] / [ A ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TAmpereId): TVoltId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TVoltId): TAmpereId; inline;
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TAmpereId): TWattId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TVoltId): TWattId; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TCoulombId): TVoltId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TVoltId): TCoulombId; inline;
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TCoulombId): TJouleId; inline;
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltId): TJouleId; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TVoltId): TSquareVoltId; inline;
operator /(const {%H-}ALeft: TSquareVoltId; const {%H-}ARight: TVoltId): TVoltId; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const {%H-}ALeft: TWattId; const {%H-}ARight: TOhmId): TSquareVoltId; inline;
operator /(const {%H-}ALeft: TSquareVoltId; const {%H-}ARight: TWattId): TOhmId; inline;
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TWattId): TSquareVoltId; inline;
operator /(const {%H-}ALeft: TSquareVoltId; const {%H-}ARight: TOhmId): TWattId; inline;

// main definition [ F ] = [ C ] / [ V ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltId): TFaradId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TFaradId): TVoltId; inline;
operator *(const {%H-}ALeft: TFaradId; const {%H-}ARight: TVoltId): TCoulombId; inline;
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TFaradId): TCoulombId; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TJouleId): TFaradId; inline;
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TFaradId): TJouleId; inline;
operator *(const {%H-}ALeft: TFaradId; const {%H-}ARight: TJouleId): TSquareCoulombId; inline;
operator *(const {%H-}ALeft: TJouleId; const {%H-}ARight: TFaradId): TSquareCoulombId; inline;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TAmpereId): TOhmId; inline;
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TOhmId): TAmpereId; inline;
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TAmpereId): TVoltId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TOhmId): TVoltId; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const {%H-}ALeft: TSecondId; const {%H-}ARight: TFaradId): TOhmId; inline;
operator /(const {%H-}ALeft: TSecondId; const {%H-}ARight: TOhmId): TFaradId; inline;
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TFaradId): TSecondId; inline;
operator *(const {%H-}ALeft: TFaradId; const {%H-}ARight: TOhmId): TSecondId; inline;

// main definition [ S ] = 1 / [ Ω ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TOhmId): TSiemensId; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSiemensId): TOhmId; inline;
operator *(const {%H-}ALeft: TSiemensId; const {%H-}ARight: TOhmId): double; inline;
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TSiemensId): double; inline;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TSecondId): TWeberId; inline;
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TVoltId): TSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TVoltId): TWeberId; inline;
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TSecondId): TVoltId; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TSquareMeterId): TTeslaId; inline;
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TTeslaId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TSquareMeterId): TWeberId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TTeslaId): TWeberId; inline;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TAmpereId): THenryId; inline;
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: THenryId): TAmpereId; inline;
operator *(const {%H-}ALeft: THenryId; const {%H-}ARight: TAmpereId): TWeberId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: THenryId): TWeberId; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TSecondId): THenryId; inline;
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: TOhmId): TSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TOhmId): THenryId; inline;
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: TSecondId): TOhmId; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const {%H-}ALeft: TOhmId; const {%H-}ARight: THertzId): THenryId; inline;
operator /(const {%H-}ALeft: TOhmId; const {%H-}ARight: THenryId): THertzId; inline;
operator *(const {%H-}ALeft: THenryId; const {%H-}ARight: THertzId): TOhmId; inline;
operator *(const {%H-}ALeft: THertzId; const {%H-}ARight: THenryId): TOhmId; inline;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const {%H-}ALeft: TCandelaId; const {%H-}ARight: TSteradianId): TLumenId; inline;
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TCandelaId): TSteradianId; inline;
operator *(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TCandelaId): TLumenId; inline;
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TSteradianId): TCandelaId; inline;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TSquareMeterId): TLuxId; inline;
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TLuxId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TLuxId; const {%H-}ARight: TSquareMeterId): TLumenId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TLuxId): TLumenId; inline;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const {%H-}ALeft: TMoleId; const {%H-}ARight: TSecondId): TKatalId; inline;
operator /(const {%H-}ALeft: TMoleId; const {%H-}ARight: TKatalId): TSecondId; inline;
operator *(const {%H-}ALeft: TKatalId; const {%H-}ARight: TSecondId): TMoleId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TKatalId): TMoleId; inline;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianId): TJoulePerRadianId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TJoulePerRadianId): TRadianId; inline;
operator *(const {%H-}ALeft: TJoulePerRadianId; const {%H-}ARight: TRadianId): TJouleId; inline;
operator *(const {%H-}ALeft: TRadianId; const {%H-}ARight: TJoulePerRadianId): TJouleId; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSecondId): TMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TSecondId): TMeterId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TMeterPerSecondId): TMeterId; inline;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TSecondId): TMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterPerSquareSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TSecondId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TMeterPerSquareSecondId): TMeterPerSecondId; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareSecondId): TMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TSquareSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TSquareSecondId): TMeterId; inline;
operator *(const {%H-}ALeft: TSquareSecondId; const {%H-}ARight: TMeterPerSquareSecondId): TMeterId; inline;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TMeterPerSquareSecondId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TMeterId): TSquareMeterPerSquareSecondId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TSquareMeterPerSquareSecondId; inline;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TSteradianPerSquareSecondId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSteradianPerSquareSecondId): TMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TMeterId): TSteradianPerSquareSecondId; inline;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSecondId): TRadianPerSecondId; inline;
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TSecondId): TRadianId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TRadianPerSecondId): TRadianId; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterId): TRadianPerSecondId; inline;
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TRadianPerSecondId): TMeterId; inline;
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TMeterId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TRadianPerSecondId): TMeterPerSecondId; inline;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSquareSecondId): TRadianPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianPerSquareSecondId): TSquareSecondId; inline;
operator *(const {%H-}ALeft: TRadianPerSquareSecondId; const {%H-}ARight: TSquareSecondId): TRadianId; inline;
operator *(const {%H-}ALeft: TSquareSecondId; const {%H-}ARight: TRadianPerSquareSecondId): TRadianId; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TSecondId): TRadianPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TRadianPerSquareSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TRadianPerSquareSecondId; const {%H-}ARight: TSecondId): TRadianPerSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TRadianPerSquareSecondId): TRadianPerSecondId; inline;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterId): TKilogramPerMeterId; inline;
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TKilogramPerMeterId; const {%H-}ARight: TMeterId): TKilogramId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TKilogramPerMeterId): TKilogramId; inline;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramPerSquareMeterId; inline;
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramPerSquareMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TKilogramPerSquareMeterId; const {%H-}ARight: TSquareMeterId): TKilogramId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TKilogramPerSquareMeterId): TKilogramId; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicMeterId): TKilogramPerCubicMeterId; inline;
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramPerCubicMeterId): TCubicMeterId; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TCubicMeterId): TKilogramId; inline;
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TKilogramPerCubicMeterId): TKilogramId; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCubicMeterId): TNewtonPerCubicMeterId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TNewtonPerCubicMeterId): TCubicMeterId; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterId; const {%H-}ARight: TCubicMeterId): TNewtonId; inline;
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TNewtonPerCubicMeterId): TNewtonId; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerCubicMeterId; inline;
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TNewtonPerCubicMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterId; const {%H-}ARight: TMeterId): TPascalId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TNewtonPerCubicMeterId): TPascalId; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterId; const {%H-}ARight: TKilogramPerCubicMeterId): TMeterPerSquareSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TNewtonPerCubicMeterId; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TKilogramPerCubicMeterId; inline;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TNewtonPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TNewtonPerMeterId; const {%H-}ARight: TMeterId): TNewtonId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TNewtonPerMeterId): TNewtonId; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareMeterId): TNewtonPerMeterId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TNewtonPerMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TNewtonPerMeterId; const {%H-}ARight: TSquareMeterId): TJouleId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TNewtonPerMeterId): TJouleId; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterId; const {%H-}ARight: TPascalId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TPascalId): TNewtonPerMeterId; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterId; const {%H-}ARight: TMeterId): TPascalId; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSecondId): TKilogramMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondId; const {%H-}ARight: TKilogramId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TKilogramId): TKilogramMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondId; const {%H-}ARight: TMeterPerSecondId): TKilogramId; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSecondId): TKilogramMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondId; const {%H-}ARight: TNewtonId): TSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TNewtonId): TKilogramMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondId; const {%H-}ARight: TSecondId): TNewtonId; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramSquareMeterId; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TKilogramId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TKilogramId): TKilogramSquareMeterId; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TSquareMeterId): TKilogramId; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TSecondId): TKilogramSquareMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TKilogramSquareMeterPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TKilogramSquareMeterPerSecondId; const {%H-}ARight: TSecondId): TKilogramSquareMeterId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TKilogramSquareMeterPerSecondId): TKilogramSquareMeterId; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondId; const {%H-}ARight: TKilogramSquareMeterId): TRadianPerSecondId; inline;
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TKilogramSquareMeterId): TKilogramSquareMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondId; const {%H-}ARight: TRadianPerSecondId): TKilogramSquareMeterId; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareSecondId): TSquareMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TSquareSecondId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TSquareSecondId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareSecondId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TSquareMeterId; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TMeterPerSecondId): TMeterPerSecondId; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramId): TSquareMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TKilogramId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TKilogramId): TJouleId; inline;
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TJouleId; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TKilogramPerCubicMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TPascalId; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TSquareMeterPerSquareSecondId): TPascalId; inline;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TSquareSecondId): TSteradianPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TSteradianPerSquareSecondId): TSquareSecondId; inline;
operator *(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TSquareSecondId): TSteradianId; inline;
operator *(const {%H-}ALeft: TSquareSecondId; const {%H-}ARight: TSteradianPerSquareSecondId): TSteradianId; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TRadianPerSecondId): TSteradianPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TRadianPerSecondId): TRadianPerSecondId; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSteradianPerSquareSecondId): TKilogramSquareMeterId; inline;
operator *(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TKilogramSquareMeterId): TJouleId; inline;
operator *(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TSteradianPerSquareSecondId): TJouleId; inline;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSecondId): TCubicMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TCubicMeterPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TCubicMeterPerSecondId; const {%H-}ARight: TSecondId): TCubicMeterId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TCubicMeterPerSecondId): TCubicMeterId; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterPerSecondId): TCubicMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TCubicMeterPerSecondId; const {%H-}ARight: TSquareMeterId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TSquareMeterId): TCubicMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TCubicMeterPerSecondId; const {%H-}ARight: TMeterPerSecondId): TSquareMeterId; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TSecondId): TPascalSecondId; inline;
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TPascalId): TSecondId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TPascalId): TPascalSecondId; inline;
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TSecondId): TPascalId; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSecondId): TSquareMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSecondId; const {%H-}ARight: TSecondId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TSquareMeterPerSecondId): TSquareMeterId; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TSquareMeterPerSecondId): TKilogramPerCubicMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TPascalSecondId; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TSquareMeterPerSecondId): TPascalSecondId; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramId): TNewtonPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TNewtonPerSquareKilogramId): TSquareKilogramId; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramId; const {%H-}ARight: TSquareKilogramId): TNewtonId; inline;
operator *(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TNewtonPerSquareKilogramId): TNewtonId; inline;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TMeterId): TSquareKilogramPerMeterId; inline;
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareKilogramPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerMeterId; const {%H-}ARight: TMeterId): TSquareKilogramId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareKilogramPerMeterId): TSquareKilogramId; inline;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId; inline;
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerSquareMeterId; const {%H-}ARight: TSquareMeterId): TSquareKilogramId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TSquareKilogramId; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TSquareKilogramId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareKilogramId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TSquareMeterId; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TNewtonId): TSquareMeterPerSquareKilogramId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramId; const {%H-}ARight: TNewtonId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TNewtonId; inline;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TNewtonId; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerSquareMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TNewtonId; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TNewtonSquareMeterId; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TNewtonPerSquareKilogramId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TNewtonPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareMeterId): TNewtonPerSquareKilogramId; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramPerMeterId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramId; const {%H-}ARight: TSquareKilogramPerMeterId): TJouleId; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramId): TJouleId; inline;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TKelvinId): TReciprocalKelvinId; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TReciprocalKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TReciprocalKelvinId; const {%H-}ARight: TKelvinId): double; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TReciprocalKelvinId): double; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKelvinId): TKilogramKelvinId; inline;
operator /(const {%H-}ALeft: TKilogramKelvinId; const {%H-}ARight: TKilogramId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TKilogramId): TKilogramKelvinId; inline;
operator /(const {%H-}ALeft: TKilogramKelvinId; const {%H-}ARight: TKelvinId): TKilogramId; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKelvinId): TJoulePerKelvinId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TJoulePerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TKelvinId): TJouleId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TJoulePerKelvinId): TJouleId; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TKilogramKelvinId; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinId; const {%H-}ARight: TKilogramKelvinId): TJouleId; inline;
operator *(const {%H-}ALeft: TKilogramKelvinId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TJouleId; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TKelvinId): TJoulePerKilogramPerKelvinId; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinId; const {%H-}ARight: TKelvinId): TSquareMeterPerSquareSecondId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TSquareMeterPerSquareSecondId; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TKilogramId): TJoulePerKilogramPerKelvinId; inline;
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TKilogramId; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinId; const {%H-}ARight: TKilogramId): TJoulePerKelvinId; inline;
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TJoulePerKilogramPerKelvinId): TJoulePerKelvinId; inline;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TKelvinId): TMeterKelvinId; inline;
operator /(const {%H-}ALeft: TMeterKelvinId; const {%H-}ARight: TMeterId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TMeterId): TMeterKelvinId; inline;
operator /(const {%H-}ALeft: TMeterKelvinId; const {%H-}ARight: TKelvinId): TMeterId; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TMeterId): TKelvinPerMeterId; inline;
operator /(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TKelvinPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TKelvinPerMeterId; const {%H-}ARight: TMeterId): TKelvinId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TKelvinPerMeterId): TKelvinId; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterId): TWattPerMeterId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TWattPerMeterId; const {%H-}ARight: TMeterId): TWattId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TWattPerMeterId): TWattId; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerSquareMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TSquareMeterId): TWattId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TWattPerSquareMeterId): TWattId; inline;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TKelvinId): TWattPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TKelvinId): TWattId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TWattPerKelvinId): TWattId; inline;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterKelvinId): TWattPerMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerMeterPerKelvinId): TMeterKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinId; const {%H-}ARight: TMeterKelvinId): TWattId; inline;
operator *(const {%H-}ALeft: TMeterKelvinId; const {%H-}ARight: TWattPerMeterPerKelvinId): TWattId; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const {%H-}ALeft: TWattPerMeterId; const {%H-}ARight: TKelvinId): TWattPerMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerMeterId; const {%H-}ARight: TWattPerMeterPerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinId; const {%H-}ARight: TKelvinId): TWattPerMeterId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TWattPerMeterPerKelvinId): TWattPerMeterId; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TMeterId): TWattPerMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TWattPerMeterPerKelvinId): TMeterId; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinId; const {%H-}ARight: TMeterId): TWattPerKelvinId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TWattPerMeterPerKelvinId): TWattPerKelvinId; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TWattPerMeterPerKelvinId): TKelvinPerMeterId; inline;
operator *(const {%H-}ALeft: TWattPerMeterPerKelvinId; const {%H-}ARight: TKelvinPerMeterId): TWattPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TKelvinPerMeterId; const {%H-}ARight: TWattPerMeterPerKelvinId): TWattPerSquareMeterId; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TKelvinId): TSquareMeterKelvinId; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinId; const {%H-}ARight: TSquareMeterId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TSquareMeterId): TSquareMeterKelvinId; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinId; const {%H-}ARight: TKelvinId): TSquareMeterId; inline;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TSquareMeterKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinId; const {%H-}ARight: TSquareMeterKelvinId): TWattId; inline;
operator *(const {%H-}ALeft: TSquareMeterKelvinId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TWattId; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinId; const {%H-}ARight: TKelvinId): TWattPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TWattPerSquareMeterId; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerKelvinId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TWattPerSquareMeterPerKelvinId): TWattPerKelvinId; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TQuarticKelvinId; inline;
operator *(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TSquareMeterQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinId; const {%H-}ARight: TQuarticKelvinId): TSquareMeterId; inline;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TQuarticKelvinId): TWattPerQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerQuarticKelvinId): TQuarticKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerQuarticKelvinId; const {%H-}ARight: TQuarticKelvinId): TWattId; inline;
operator *(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TWattPerQuarticKelvinId): TWattId; inline;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TSquareMeterQuarticKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinId; const {%H-}ARight: TSquareMeterQuarticKelvinId): TWattId; inline;
operator *(const {%H-}ALeft: TSquareMeterQuarticKelvinId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattId; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TQuarticKelvinId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinId; const {%H-}ARight: TQuarticKelvinId): TWattPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TQuarticKelvinId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattPerSquareMeterId; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const {%H-}ALeft: TWattPerQuarticKelvinId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterPerQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerQuarticKelvinId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattPerQuarticKelvinId; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleId): TJoulePerMoleId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TJoulePerMoleId): TMoleId; inline;
operator *(const {%H-}ALeft: TJoulePerMoleId; const {%H-}ARight: TMoleId): TJouleId; inline;
operator *(const {%H-}ALeft: TMoleId; const {%H-}ARight: TJoulePerMoleId): TJouleId; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const {%H-}ALeft: TMoleId; const {%H-}ARight: TKelvinId): TMoleKelvinId; inline;
operator /(const {%H-}ALeft: TMoleKelvinId; const {%H-}ARight: TMoleId): TKelvinId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TMoleId): TMoleKelvinId; inline;
operator /(const {%H-}ALeft: TMoleKelvinId; const {%H-}ARight: TKelvinId): TMoleId; inline;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleKelvinId): TJoulePerMolePerKelvinId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TJoulePerMolePerKelvinId): TMoleKelvinId; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinId; const {%H-}ARight: TMoleKelvinId): TJouleId; inline;
operator *(const {%H-}ALeft: TMoleKelvinId; const {%H-}ARight: TJoulePerMolePerKelvinId): TJouleId; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TMoleId): TJoulePerMolePerKelvinId; inline;
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TJoulePerMolePerKelvinId): TMoleId; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinId; const {%H-}ARight: TMoleId): TJoulePerKelvinId; inline;
operator *(const {%H-}ALeft: TMoleId; const {%H-}ARight: TJoulePerMolePerKelvinId): TJoulePerKelvinId; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const {%H-}ALeft: TJoulePerMoleId; const {%H-}ARight: TKelvinId): TJoulePerMolePerKelvinId; inline;
operator /(const {%H-}ALeft: TJoulePerMoleId; const {%H-}ARight: TJoulePerMolePerKelvinId): TKelvinId; inline;
operator *(const {%H-}ALeft: TJoulePerMolePerKelvinId; const {%H-}ARight: TKelvinId): TJoulePerMoleId; inline;
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TJoulePerMolePerKelvinId): TJoulePerMoleId; inline;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TMeterId): TOhmMeterId; inline;
operator /(const {%H-}ALeft: TOhmMeterId; const {%H-}ARight: TOhmId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TOhmId): TOhmMeterId; inline;
operator /(const {%H-}ALeft: TOhmMeterId; const {%H-}ARight: TMeterId): TOhmId; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltPerMeterId; inline;
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TVoltPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TMeterId): TVoltId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TVoltPerMeterId): TVoltId; inline;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCoulombId): TVoltPerMeterId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TVoltPerMeterId): TCoulombId; inline;
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TCoulombId): TNewtonId; inline;
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltPerMeterId): TNewtonId; inline;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerSecondId): TVoltPerMeterId; inline;
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TTeslaId): TMeterPerSecondId; inline;
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TTeslaId): TVoltPerMeterId; inline;
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TMeterPerSecondId): TTeslaId; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TMeterId): TCoulombPerMeterId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TMeterId): TCoulombId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TCoulombPerMeterId): TCoulombId; inline;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TMeterId): TSquareCoulombPerMeterId; inline;
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TSquareCoulombPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerMeterId; const {%H-}ARight: TMeterId): TSquareCoulombId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareCoulombPerMeterId): TSquareCoulombId; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TCoulombId): TSquareCoulombPerMeterId; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerMeterId; const {%H-}ARight: TCoulombPerMeterId): TCoulombId; inline;
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombPerMeterId): TSquareCoulombPerMeterId; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerMeterId; const {%H-}ARight: TCoulombId): TCoulombPerMeterId; inline;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TSquareMeterId): TCoulombPerSquareMeterId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombPerSquareMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TSquareMeterId): TCoulombId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TCoulombId; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TMeterId): TCoulombPerSquareMeterId; inline;
operator /(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TMeterId): TCoulombPerMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TCoulombPerMeterId; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TSquareCoulombId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareCoulombId; const {%H-}ARight: TSquareCoulombId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TSquareMeterId; inline;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareCoulombId): TNewtonPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TNewtonPerSquareCoulombId): TSquareCoulombId; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombId; const {%H-}ARight: TSquareCoulombId): TNewtonId; inline;
operator *(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TNewtonPerSquareCoulombId): TNewtonId; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TNewtonId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TNewtonId): TNewtonSquareMeterId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareMeterId): TNewtonId; inline;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TNewtonId): TSquareMeterPerSquareCoulombId; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareCoulombId; const {%H-}ARight: TNewtonId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TNewtonId; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TSquareCoulombId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TNewtonSquareMeterId; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TNewtonPerSquareCoulombId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TNewtonPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TSquareMeterId): TNewtonPerSquareCoulombId; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TCoulombPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TCoulombPerSquareMeterId): TVoltPerMeterId; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TVoltPerMeterId; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TSquareCoulombPerMeterId; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TSquareCoulombPerMeterId): TJouleId; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TJouleId; inline;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltMeterId; inline;
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TVoltId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TVoltId): TVoltMeterId; inline;
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TMeterId): TVoltId; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TSquareMeterId): TVoltMeterId; inline;
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TVoltPerMeterId): TSquareMeterId; inline;
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TVoltPerMeterId): TVoltMeterId; inline;
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TSquareMeterId): TVoltPerMeterId; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TSecondId): TVoltMeterPerSecondId; inline;
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TVoltMeterPerSecondId): TSecondId; inline;
operator *(const {%H-}ALeft: TVoltMeterPerSecondId; const {%H-}ARight: TSecondId): TVoltMeterId; inline;
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TVoltMeterPerSecondId): TVoltMeterId; inline;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const {%H-}ALeft: TFaradId; const {%H-}ARight: TMeterId): TFaradPerMeterId; inline;
operator /(const {%H-}ALeft: TFaradId; const {%H-}ARight: TFaradPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TFaradPerMeterId; const {%H-}ARight: TMeterId): TFaradId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TFaradPerMeterId): TFaradId; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltMeterId): TFaradPerMeterId; inline;
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TFaradPerMeterId): TVoltMeterId; inline;
operator *(const {%H-}ALeft: TFaradPerMeterId; const {%H-}ARight: TVoltMeterId): TCoulombId; inline;
operator *(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TFaradPerMeterId): TCoulombId; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TVoltPerMeterId): TFaradPerMeterId; inline;
operator /(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TFaradPerMeterId): TVoltPerMeterId; inline;
operator *(const {%H-}ALeft: TFaradPerMeterId; const {%H-}ARight: TVoltPerMeterId): TCoulombPerSquareMeterId; inline;
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TFaradPerMeterId): TCoulombPerSquareMeterId; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TFaradPerMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator *(const {%H-}ALeft: TFaradPerMeterId; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): double; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombId; const {%H-}ARight: TFaradPerMeterId): double; inline;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TMeterId): TAmperePerMeterId; inline;
operator /(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TAmperePerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TAmperePerMeterId; const {%H-}ARight: TMeterId): TAmpereId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TAmperePerMeterId): TAmpereId; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TAmpereId): TMeterPerAmpereId; inline;
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterPerAmpereId): TAmpereId; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereId; const {%H-}ARight: TAmpereId): TMeterId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TMeterPerAmpereId): TMeterId; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterId): TTeslaMeterId; inline;
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TTeslaId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TTeslaId): TTeslaMeterId; inline;
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TMeterId): TTeslaId; inline;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TAmpereId): TTeslaMeterId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TTeslaMeterId): TAmpereId; inline;
operator *(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TAmpereId): TNewtonId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TTeslaMeterId): TNewtonId; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmpereId): TTeslaPerAmpereId; inline;
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TTeslaPerAmpereId): TAmpereId; inline;
operator *(const {%H-}ALeft: TTeslaPerAmpereId; const {%H-}ARight: TAmpereId): TTeslaId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TTeslaPerAmpereId): TTeslaId; inline;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: THenryPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TMeterId): THenryId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: THenryPerMeterId): THenryId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TAmpereId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: THenryPerMeterId): TAmpereId; inline;
operator *(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TAmpereId): TTeslaMeterId; inline;
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: THenryPerMeterId): TTeslaMeterId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const {%H-}ALeft: TTeslaPerAmpereId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TTeslaPerAmpereId): TMeterId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TTeslaPerAmpereId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TMeterId): TTeslaPerAmpereId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerAmpereId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TTeslaId): TMeterPerAmpereId; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereId; const {%H-}ARight: TTeslaId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TMeterPerAmpereId): TTeslaId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmperePerMeterId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: THenryPerMeterId): TAmperePerMeterId; inline;
operator *(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TAmperePerMeterId): TTeslaId; inline;
operator *(const {%H-}ALeft: TAmperePerMeterId; const {%H-}ARight: THenryPerMeterId): TTeslaId; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareAmpereId): THenryPerMeterId; inline;
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: THenryPerMeterId): TSquareAmpereId; inline;
operator *(const {%H-}ALeft: THenryPerMeterId; const {%H-}ARight: TSquareAmpereId): TNewtonId; inline;
operator *(const {%H-}ALeft: TSquareAmpereId; const {%H-}ARight: THenryPerMeterId): TNewtonId; inline;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TMeterId): TRadianPerMeterId; inline;
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianPerMeterId): TMeterId; inline;
operator *(const {%H-}ALeft: TRadianPerMeterId; const {%H-}ARight: TMeterId): TRadianId; inline;
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TRadianPerMeterId): TRadianId; inline;

// main definition [ J/deg ] = [ J ] / [ deg ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TDegreeId): TJoulePerDegreeId; inline;

// main definition [ km/h ] = [ km ] / [ h ]
operator /(const {%H-}ALeft: TKilometerId; const {%H-}ARight: THourId): TKilometerPerHourId; inline;

// main definition [ dm/s ] = [ dm ] / [ s ]
operator /(const {%H-}ALeft: TDecimeterId; const {%H-}ARight: TSecondId): TDecimeterPerSecondId; inline;

// main definition [ cm/s ] = [ cm ] / [ s ]
operator /(const {%H-}ALeft: TCentimeterId; const {%H-}ARight: TSecondId): TCentimeterPerSecondId; inline;

// main definition [ mm/s ] = [ mm ] / [ s ]
operator /(const {%H-}ALeft: TMillimeterId; const {%H-}ARight: TSecondId): TMillimeterPerSecondId; inline;

// main definition [ km/h/s ] = [ km/h ] / [ s ]
operator /(const {%H-}ALeft: TKilometerPerHourId; const {%H-}ARight: TSecondId): TKilometerPerHourPerSecondId; inline;

// main definition [ dm/s2 ] = [ dm ] / [ s2 ]
operator /(const {%H-}ALeft: TDecimeterId; const {%H-}ARight: TSquareSecondId): TDecimeterPerSquareSecondId; inline;

// main definition [ cm/s2 ] = [ cm ] / [ s2 ]
operator /(const {%H-}ALeft: TCentimeterId; const {%H-}ARight: TSquareSecondId): TCentimeterPerSquareSecondId; inline;

// main definition [ mm/s2 ] = [ mm ] / [ s2 ]
operator /(const {%H-}ALeft: TMillimeterId; const {%H-}ARight: TSquareSecondId): TMillimeterPerSquareSecondId; inline;

//
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicMillimeterId): TKilogramPerCubicMillimeterId; inline;

//
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicCentimeterId): TKilogramPerCubicCentimeterId; inline;

//
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicDecimeterId): TKilogramPerCubicDecimeterId; inline;

//
operator /(const {%H-}ALeft: THectogramId; const {%H-}ARight: TCubicMeterId): THectogramPerCubicMeterId; inline;

//
operator /(const {%H-}ALeft: TDecagramId; const {%H-}ARight: TCubicMeterId): TDecagramPerCubicMeterId; inline;

//
operator /(const {%H-}ALeft: TGramId; const {%H-}ARight: TCubicMeterId): TGramPerCubicMeterId; inline;

//
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMillimeterId): TNewtonPerMillimeterId; inline;

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
  TBequerelHelper = record helper for TBequerelId
    function From(const AQuantity: THertz): TBequerels;
  end;

{ Helper for Gray }

type
  TGrayHelper = record helper for TGrayId
    function From(const AQuantity: TSquareMetersPerSquareSecond): TGrays;
  end;

{ Helper for Sievert }

type
  TSievertHelper = record helper for TSievertId
    function From(const AQuantity: TSquareMetersPerSquareSecond): TSieverts;
  end;

{ Helper for NewtonMeter }

type
  TNewtonMeterHelper = record helper for TNewtonMeterId
    function From(const AQuantity: TJoules): TNewtonMeters;
  end;

{ Helper for NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianId
    function From(const AQuantity: TJoulesPerRadian): TNewtonMetersPerRadian;
  end;

{ Helper for NewtonSecond }

type
  TNewtonSecondHelper = record helper for TNewtonSecondId
    function From(const AQuantity: TKilogramMetersPerSecond): TNewtonSeconds;
  end;

{ Helper for JoulePerKilogram }

type
  TJoulePerKilogramHelper = record helper for TJoulePerKilogramId
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

{ TIdentifier }

class function TIdentifier.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.Value := AQuantity.Value;
end;

class operator TIdentifier.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue;
end;

{ TFactoredIdentifier }

class function TFactoredIdentifier.From(const AQuantity: TBaseQuantity): TFactoredQuantity;
begin
  result.Value := AQuantity.Value / U.Factor;
end;

class operator TFactoredIdentifier.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue * U.Factor;
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

{ Unit of TNewtonPerMillimeterUnit }

class function TNewtonPerMillimeterUnit.Symbol: string;
begin
  result := 'N/mm';
end;

class function TNewtonPerMillimeterUnit.Name: string;
begin
  result := 'newton per millimeter';
end;

class function TNewtonPerMillimeterUnit.Factor: double;
begin
  result := 1E+03;
end;

{ Combining units }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSecondId; const ARight: TSecondId): TSquareSecondId;
begin end;

operator /(const ALeft: TSquareSecondId; const ARight: TSecondId): TSecondId;
begin end;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterId; const ARight: TMeterId): TSquareMeterId;
begin end;

operator /(const ALeft: TSquareMeterId; const ARight: TMeterId): TMeterId;
begin end;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeterId; const ARight: TMeterId): TCubicMeterId;
begin end;

operator /(const ALeft: TCubicMeterId; const ARight: TSquareMeterId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TSquareMeterId): TCubicMeterId;
begin end;

operator /(const ALeft: TCubicMeterId; const ARight: TMeterId): TSquareMeterId;
begin end;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeterId; const ARight: TMeterId): TQuarticMeterId;
begin end;

operator /(const ALeft: TQuarticMeterId; const ARight: TCubicMeterId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TCubicMeterId): TQuarticMeterId;
begin end;

operator /(const ALeft: TQuarticMeterId; const ARight: TMeterId): TCubicMeterId;
begin end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeterId; const ARight: TSquareMeterId): TQuarticMeterId;
begin end;

operator /(const ALeft: TQuarticMeterId; const ARight: TSquareMeterId): TSquareMeterId;
begin end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeterId; const ARight: TMeterId): TQuinticMeterId;
begin end;

operator /(const ALeft: TQuinticMeterId; const ARight: TQuarticMeterId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TQuarticMeterId): TQuinticMeterId;
begin end;

operator /(const ALeft: TQuinticMeterId; const ARight: TMeterId): TQuarticMeterId;
begin end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeterId; const ARight: TSquareMeterId): TQuinticMeterId;
begin end;

operator /(const ALeft: TQuinticMeterId; const ARight: TCubicMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TCubicMeterId): TQuinticMeterId;
begin end;

operator /(const ALeft: TQuinticMeterId; const ARight: TSquareMeterId): TCubicMeterId;
begin end;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeterId; const ARight: TMeterId): TSexticMeterId;
begin end;

operator /(const ALeft: TSexticMeterId; const ARight: TQuinticMeterId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TQuinticMeterId): TSexticMeterId;
begin end;

operator /(const ALeft: TSexticMeterId; const ARight: TMeterId): TQuinticMeterId;
begin end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeterId; const ARight: TSquareMeterId): TSexticMeterId;
begin end;

operator /(const ALeft: TSexticMeterId; const ARight: TQuarticMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TQuarticMeterId): TSexticMeterId;
begin end;

operator /(const ALeft: TSexticMeterId; const ARight: TSquareMeterId): TQuarticMeterId;
begin end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeterId; const ARight: TCubicMeterId): TSexticMeterId;
begin end;

operator /(const ALeft: TSexticMeterId; const ARight: TCubicMeterId): TCubicMeterId;
begin end;

// main definition [ kg2 ]
operator *(const ALeft: TKilogramId; const ARight: TKilogramId): TSquareKilogramId;
begin end;

operator /(const ALeft: TSquareKilogramId; const ARight: TKilogramId): TKilogramId;
begin end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereId; const ARight: TAmpereId): TSquareAmpereId;
begin end;

operator /(const ALeft: TSquareAmpereId; const ARight: TAmpereId): TAmpereId;
begin end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinId; const ARight: TKelvinId): TSquareKelvinId;
begin end;

operator /(const ALeft: TSquareKelvinId; const ARight: TKelvinId): TKelvinId;
begin end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvinId; const ARight: TKelvinId): TCubicKelvinId;
begin end;

operator /(const ALeft: TCubicKelvinId; const ARight: TSquareKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TSquareKelvinId): TCubicKelvinId;
begin end;

operator /(const ALeft: TCubicKelvinId; const ARight: TKelvinId): TSquareKelvinId;
begin end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvinId; const ARight: TSquareKelvinId): TQuarticKelvinId;
begin end;

operator /(const ALeft: TQuarticKelvinId; const ARight: TSquareKelvinId): TSquareKelvinId;
begin end;

//
operator *(const ALeft: TCubicKelvinId; const ARight: TKelvinId): TQuarticKelvinId;
begin end;

operator /(const ALeft: TQuarticKelvinId; const ARight: TCubicKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TCubicKelvinId): TQuarticKelvinId;
begin end;

operator /(const ALeft: TQuarticKelvinId; const ARight: TKelvinId): TCubicKelvinId;
begin end;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadianId; const ARight: TRadianId): TSteradianId;
begin end;

operator /(const ALeft: TSteradianId; const ARight: TRadianId): TRadianId;
begin end;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSecondId): THertzId;
begin end;

operator /(const ALeft: double; const ARight: THertzId): TSecondId;
begin end;

operator *(const ALeft: THertzId; const ARight: TSecondId): double;
begin end;

operator *(const ALeft: TSecondId; const ARight: THertzId): double;
begin end;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertzId; const ARight: THertzId): TSquareHertzId;
begin end;

operator /(const ALeft: TSquareHertzId; const ARight: THertzId): THertzId;
begin end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSquareSecondId): TNewtonId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TKilogramId): TMeterPerSquareSecondId;
begin end;

operator *(const ALeft: TMeterPerSquareSecondId; const ARight: TKilogramId): TNewtonId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TMeterPerSquareSecondId): TKilogramId;
begin end;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareMeterId): TPascalId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TPascalId): TSquareMeterId;
begin end;

operator *(const ALeft: TPascalId; const ARight: TSquareMeterId): TNewtonId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TPascalId): TNewtonId;
begin end;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtonId; const ARight: TMeterId): TJouleId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TNewtonId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TNewtonId): TJouleId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TMeterId): TNewtonId;
begin end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascalId; const ARight: TCubicMeterId): TJouleId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TPascalId): TCubicMeterId;
begin end;

operator *(const ALeft: TCubicMeterId; const ARight: TPascalId): TJouleId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TCubicMeterId): TPascalId;
begin end;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJouleId; const ARight: TSecondId): TWattId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TWattId): TSecondId;
begin end;

operator *(const ALeft: TWattId; const ARight: TSecondId): TJouleId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TWattId): TJouleId;
begin end;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJouleId; const ARight: TRadianPerSecondId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TJouleId): TRadianPerSecondId;
begin end;

operator *(const ALeft: TRadianPerSecondId; const ARight: TJouleId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TRadianPerSecondId): TJouleId;
begin end;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const ALeft: TSquareAmpereId; const ARight: TOhmId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TSquareAmpereId): TOhmId;
begin end;

operator *(const ALeft: TOhmId; const ARight: TSquareAmpereId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TOhmId): TSquareAmpereId;
begin end;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtonId; const ARight: TMeterPerSecondId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TNewtonId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TMeterPerSecondId; const ARight: TNewtonId): TWattId;
begin end;

operator /(const ALeft: TWattId; const ARight: TMeterPerSecondId): TNewtonId;
begin end;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSecondId; const ARight: TAmpereId): TCoulombId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TSecondId): TAmpereId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TSecondId): TCoulombId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TAmpereId): TSecondId;
begin end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombId; const ARight: TCoulombId): TSquareCoulombId;
begin end;

operator /(const ALeft: TSquareCoulombId; const ARight: TCoulombId): TCoulombId;
begin end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWattId; const ARight: TAmpereId): TVoltId;
begin end;

operator /(const ALeft: TWattId; const ARight: TVoltId): TAmpereId;
begin end;

operator *(const ALeft: TVoltId; const ARight: TAmpereId): TWattId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TVoltId): TWattId;
begin end;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJouleId; const ARight: TCoulombId): TVoltId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TVoltId): TCoulombId;
begin end;

operator *(const ALeft: TVoltId; const ARight: TCoulombId): TJouleId;
begin end;

operator *(const ALeft: TCoulombId; const ARight: TVoltId): TJouleId;
begin end;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVoltId; const ARight: TVoltId): TSquareVoltId;
begin end;

operator /(const ALeft: TSquareVoltId; const ARight: TVoltId): TVoltId;
begin end;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWattId; const ARight: TOhmId): TSquareVoltId;
begin end;

operator /(const ALeft: TSquareVoltId; const ARight: TWattId): TOhmId;
begin end;

operator *(const ALeft: TOhmId; const ARight: TWattId): TSquareVoltId;
begin end;

operator /(const ALeft: TSquareVoltId; const ARight: TOhmId): TWattId;
begin end;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombId; const ARight: TVoltId): TFaradId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TFaradId): TVoltId;
begin end;

operator *(const ALeft: TFaradId; const ARight: TVoltId): TCoulombId;
begin end;

operator *(const ALeft: TVoltId; const ARight: TFaradId): TCoulombId;
begin end;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombId; const ARight: TJouleId): TFaradId;
begin end;

operator /(const ALeft: TSquareCoulombId; const ARight: TFaradId): TJouleId;
begin end;

operator *(const ALeft: TFaradId; const ARight: TJouleId): TSquareCoulombId;
begin end;

operator *(const ALeft: TJouleId; const ARight: TFaradId): TSquareCoulombId;
begin end;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVoltId; const ARight: TAmpereId): TOhmId;
begin end;

operator /(const ALeft: TVoltId; const ARight: TOhmId): TAmpereId;
begin end;

operator *(const ALeft: TOhmId; const ARight: TAmpereId): TVoltId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TOhmId): TVoltId;
begin end;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSecondId; const ARight: TFaradId): TOhmId;
begin end;

operator /(const ALeft: TSecondId; const ARight: TOhmId): TFaradId;
begin end;

operator *(const ALeft: TOhmId; const ARight: TFaradId): TSecondId;
begin end;

operator *(const ALeft: TFaradId; const ARight: TOhmId): TSecondId;
begin end;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhmId): TSiemensId;
begin end;

operator /(const ALeft: double; const ARight: TSiemensId): TOhmId;
begin end;

operator *(const ALeft: TSiemensId; const ARight: TOhmId): double;
begin end;

operator *(const ALeft: TOhmId; const ARight: TSiemensId): double;
begin end;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVoltId; const ARight: TSecondId): TWeberId;
begin end;

operator /(const ALeft: TWeberId; const ARight: TVoltId): TSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TVoltId): TWeberId;
begin end;

operator /(const ALeft: TWeberId; const ARight: TSecondId): TVoltId;
begin end;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWeberId; const ARight: TSquareMeterId): TTeslaId;
begin end;

operator /(const ALeft: TWeberId; const ARight: TTeslaId): TSquareMeterId;
begin end;

operator *(const ALeft: TTeslaId; const ARight: TSquareMeterId): TWeberId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TTeslaId): TWeberId;
begin end;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWeberId; const ARight: TAmpereId): THenryId;
begin end;

operator /(const ALeft: TWeberId; const ARight: THenryId): TAmpereId;
begin end;

operator *(const ALeft: THenryId; const ARight: TAmpereId): TWeberId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: THenryId): TWeberId;
begin end;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhmId; const ARight: TSecondId): THenryId;
begin end;

operator /(const ALeft: THenryId; const ARight: TOhmId): TSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TOhmId): THenryId;
begin end;

operator /(const ALeft: THenryId; const ARight: TSecondId): TOhmId;
begin end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhmId; const ARight: THertzId): THenryId;
begin end;

operator /(const ALeft: TOhmId; const ARight: THenryId): THertzId;
begin end;

operator *(const ALeft: THenryId; const ARight: THertzId): TOhmId;
begin end;

operator *(const ALeft: THertzId; const ARight: THenryId): TOhmId;
begin end;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelaId; const ARight: TSteradianId): TLumenId;
begin end;

operator /(const ALeft: TLumenId; const ARight: TCandelaId): TSteradianId;
begin end;

operator *(const ALeft: TSteradianId; const ARight: TCandelaId): TLumenId;
begin end;

operator /(const ALeft: TLumenId; const ARight: TSteradianId): TCandelaId;
begin end;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumenId; const ARight: TSquareMeterId): TLuxId;
begin end;

operator /(const ALeft: TLumenId; const ARight: TLuxId): TSquareMeterId;
begin end;

operator *(const ALeft: TLuxId; const ARight: TSquareMeterId): TLumenId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TLuxId): TLumenId;
begin end;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoleId; const ARight: TSecondId): TKatalId;
begin end;

operator /(const ALeft: TMoleId; const ARight: TKatalId): TSecondId;
begin end;

operator *(const ALeft: TKatalId; const ARight: TSecondId): TMoleId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TKatalId): TMoleId;
begin end;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJouleId; const ARight: TRadianId): TJoulePerRadianId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TJoulePerRadianId): TRadianId;
begin end;

operator *(const ALeft: TJoulePerRadianId; const ARight: TRadianId): TJouleId;
begin end;

operator *(const ALeft: TRadianId; const ARight: TJoulePerRadianId): TJouleId;
begin end;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeterId; const ARight: TSecondId): TMeterPerSecondId;
begin end;

operator /(const ALeft: TMeterId; const ARight: TMeterPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TMeterPerSecondId; const ARight: TSecondId): TMeterId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TMeterPerSecondId): TMeterId;
begin end;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMeterPerSecondId; const ARight: TSecondId): TMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TMeterPerSecondId; const ARight: TMeterPerSquareSecondId): TSecondId;
begin end;

operator *(const ALeft: TMeterPerSquareSecondId; const ARight: TSecondId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TMeterPerSquareSecondId): TMeterPerSecondId;
begin end;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeterId; const ARight: TSquareSecondId): TMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TMeterId; const ARight: TMeterPerSquareSecondId): TSquareSecondId;
begin end;

operator *(const ALeft: TMeterPerSquareSecondId; const ARight: TSquareSecondId): TMeterId;
begin end;

operator *(const ALeft: TSquareSecondId; const ARight: TMeterPerSquareSecondId): TMeterId;
begin end;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TMeterPerSquareSecondId): TMeterId;
begin end;

operator *(const ALeft: TMeterPerSquareSecondId; const ARight: TMeterId): TSquareMeterPerSquareSecondId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TMeterPerSquareSecondId): TSquareMeterPerSquareSecondId;
begin end;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const ALeft: TSteradianPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TMeterPerSquareSecondId; const ARight: TSteradianPerSquareSecondId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TSteradianPerSquareSecondId): TMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TMeterPerSquareSecondId; const ARight: TMeterId): TSteradianPerSquareSecondId;
begin end;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadianId; const ARight: TSecondId): TRadianPerSecondId;
begin end;

operator /(const ALeft: TRadianId; const ARight: TRadianPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TRadianPerSecondId; const ARight: TSecondId): TRadianId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TRadianPerSecondId): TRadianId;
begin end;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMeterPerSecondId; const ARight: TMeterId): TRadianPerSecondId;
begin end;

operator /(const ALeft: TMeterPerSecondId; const ARight: TRadianPerSecondId): TMeterId;
begin end;

operator *(const ALeft: TRadianPerSecondId; const ARight: TMeterId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TRadianPerSecondId): TMeterPerSecondId;
begin end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadianId; const ARight: TSquareSecondId): TRadianPerSquareSecondId;
begin end;

operator /(const ALeft: TRadianId; const ARight: TRadianPerSquareSecondId): TSquareSecondId;
begin end;

operator *(const ALeft: TRadianPerSquareSecondId; const ARight: TSquareSecondId): TRadianId;
begin end;

operator *(const ALeft: TSquareSecondId; const ARight: TRadianPerSquareSecondId): TRadianId;
begin end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadianPerSecondId; const ARight: TSecondId): TRadianPerSquareSecondId;
begin end;

operator /(const ALeft: TRadianPerSecondId; const ARight: TRadianPerSquareSecondId): TSecondId;
begin end;

operator *(const ALeft: TRadianPerSquareSecondId; const ARight: TSecondId): TRadianPerSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TRadianPerSquareSecondId): TRadianPerSecondId;
begin end;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilogramId; const ARight: TMeterId): TKilogramPerMeterId;
begin end;

operator /(const ALeft: TKilogramId; const ARight: TKilogramPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TKilogramPerMeterId; const ARight: TMeterId): TKilogramId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TKilogramPerMeterId): TKilogramId;
begin end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramPerSquareMeterId;
begin end;

operator /(const ALeft: TKilogramId; const ARight: TKilogramPerSquareMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TKilogramPerSquareMeterId; const ARight: TSquareMeterId): TKilogramId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TKilogramPerSquareMeterId): TKilogramId;
begin end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilogramId; const ARight: TCubicMeterId): TKilogramPerCubicMeterId;
begin end;

operator /(const ALeft: TKilogramId; const ARight: TKilogramPerCubicMeterId): TCubicMeterId;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TCubicMeterId): TKilogramId;
begin end;

operator *(const ALeft: TCubicMeterId; const ARight: TKilogramPerCubicMeterId): TKilogramId;
begin end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtonId; const ARight: TCubicMeterId): TNewtonPerCubicMeterId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TNewtonPerCubicMeterId): TCubicMeterId;
begin end;

operator *(const ALeft: TNewtonPerCubicMeterId; const ARight: TCubicMeterId): TNewtonId;
begin end;

operator *(const ALeft: TCubicMeterId; const ARight: TNewtonPerCubicMeterId): TNewtonId;
begin end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerCubicMeterId;
begin end;

operator /(const ALeft: TPascalId; const ARight: TNewtonPerCubicMeterId): TMeterId;
begin end;

operator *(const ALeft: TNewtonPerCubicMeterId; const ARight: TMeterId): TPascalId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TNewtonPerCubicMeterId): TPascalId;
begin end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterId; const ARight: TKilogramPerCubicMeterId): TMeterPerSquareSecondId;
begin end;

operator *(const ALeft: TMeterPerSquareSecondId; const ARight: TKilogramPerCubicMeterId): TNewtonPerCubicMeterId;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterId; const ARight: TMeterPerSquareSecondId): TKilogramPerCubicMeterId;
begin end;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtonId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TNewtonPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TNewtonPerMeterId; const ARight: TMeterId): TNewtonId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TNewtonPerMeterId): TNewtonId;
begin end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJouleId; const ARight: TSquareMeterId): TNewtonPerMeterId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TNewtonPerMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TNewtonPerMeterId; const ARight: TSquareMeterId): TJouleId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TNewtonPerMeterId): TJouleId;
begin end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

operator /(const ALeft: TNewtonPerMeterId; const ARight: TPascalId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TPascalId): TNewtonPerMeterId;
begin end;

operator /(const ALeft: TNewtonPerMeterId; const ARight: TMeterId): TPascalId;
begin end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSecondId): TKilogramMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondId; const ARight: TKilogramId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TMeterPerSecondId; const ARight: TKilogramId): TKilogramMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondId; const ARight: TMeterPerSecondId): TKilogramId;
begin end;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtonId; const ARight: TSecondId): TKilogramMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondId; const ARight: TNewtonId): TSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TNewtonId): TKilogramMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramMeterPerSecondId; const ARight: TSecondId): TNewtonId;
begin end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramSquareMeterId;
begin end;

operator /(const ALeft: TKilogramSquareMeterId; const ARight: TKilogramId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TKilogramId): TKilogramSquareMeterId;
begin end;

operator /(const ALeft: TKilogramSquareMeterId; const ARight: TSquareMeterId): TKilogramId;
begin end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterId; const ARight: TSecondId): TKilogramSquareMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramSquareMeterId; const ARight: TKilogramSquareMeterPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TKilogramSquareMeterPerSecondId; const ARight: TSecondId): TKilogramSquareMeterId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TKilogramSquareMeterPerSecondId): TKilogramSquareMeterId;
begin end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeterId; const ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondId; const ARight: TKilogramSquareMeterId): TRadianPerSecondId;
begin end;

operator *(const ALeft: TRadianPerSecondId; const ARight: TKilogramSquareMeterId): TKilogramSquareMeterPerSecondId;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondId; const ARight: TRadianPerSecondId): TKilogramSquareMeterId;
begin end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareSecondId): TSquareMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TSquareMeterId; const ARight: TSquareMeterPerSquareSecondId): TSquareSecondId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TSquareSecondId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareSecondId; const ARight: TSquareMeterPerSquareSecondId): TSquareMeterId;
begin end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMeterPerSecondId; const ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TMeterPerSecondId): TMeterPerSecondId;
begin end;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJouleId; const ARight: TKilogramId): TSquareMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TSquareMeterPerSquareSecondId): TKilogramId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TKilogramId): TJouleId;
begin end;

operator *(const ALeft: TKilogramId; const ARight: TSquareMeterPerSquareSecondId): TJouleId;
begin end;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascalId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId;
begin end;

operator /(const ALeft: TPascalId; const ARight: TSquareMeterPerSquareSecondId): TKilogramPerCubicMeterId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TKilogramPerCubicMeterId): TPascalId;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TSquareMeterPerSquareSecondId): TPascalId;
begin end;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradianId; const ARight: TSquareSecondId): TSteradianPerSquareSecondId;
begin end;

operator /(const ALeft: TSteradianId; const ARight: TSteradianPerSquareSecondId): TSquareSecondId;
begin end;

operator *(const ALeft: TSteradianPerSquareSecondId; const ARight: TSquareSecondId): TSteradianId;
begin end;

operator *(const ALeft: TSquareSecondId; const ARight: TSteradianPerSquareSecondId): TSteradianId;
begin end;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadianPerSecondId; const ARight: TRadianPerSecondId): TSteradianPerSquareSecondId;
begin end;

operator /(const ALeft: TSteradianPerSquareSecondId; const ARight: TRadianPerSecondId): TRadianPerSecondId;
begin end;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJouleId; const ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TSteradianPerSquareSecondId): TKilogramSquareMeterId;
begin end;

operator *(const ALeft: TSteradianPerSquareSecondId; const ARight: TKilogramSquareMeterId): TJouleId;
begin end;

operator *(const ALeft: TKilogramSquareMeterId; const ARight: TSteradianPerSquareSecondId): TJouleId;
begin end;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeterId; const ARight: TSecondId): TCubicMeterPerSecondId;
begin end;

operator /(const ALeft: TCubicMeterId; const ARight: TCubicMeterPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TCubicMeterPerSecondId; const ARight: TSecondId): TCubicMeterId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TCubicMeterPerSecondId): TCubicMeterId;
begin end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeterId; const ARight: TMeterPerSecondId): TCubicMeterPerSecondId;
begin end;

operator /(const ALeft: TCubicMeterPerSecondId; const ARight: TSquareMeterId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TMeterPerSecondId; const ARight: TSquareMeterId): TCubicMeterPerSecondId;
begin end;

operator /(const ALeft: TCubicMeterPerSecondId; const ARight: TMeterPerSecondId): TSquareMeterId;
begin end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascalId; const ARight: TSecondId): TPascalSecondId;
begin end;

operator /(const ALeft: TPascalSecondId; const ARight: TPascalId): TSecondId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TPascalId): TPascalSecondId;
begin end;

operator /(const ALeft: TPascalSecondId; const ARight: TSecondId): TPascalId;
begin end;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeterId; const ARight: TSecondId): TSquareMeterPerSecondId;
begin end;

operator /(const ALeft: TSquareMeterId; const ARight: TSquareMeterPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TSquareMeterPerSecondId; const ARight: TSecondId): TSquareMeterId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TSquareMeterPerSecondId): TSquareMeterId;
begin end;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSecondId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId;
begin end;

operator /(const ALeft: TPascalSecondId; const ARight: TSquareMeterPerSecondId): TKilogramPerCubicMeterId;
begin end;

operator *(const ALeft: TSquareMeterPerSecondId; const ARight: TKilogramPerCubicMeterId): TPascalSecondId;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TSquareMeterPerSecondId): TPascalSecondId;
begin end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramId): TNewtonPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TNewtonPerSquareKilogramId): TSquareKilogramId;
begin end;

operator *(const ALeft: TNewtonPerSquareKilogramId; const ARight: TSquareKilogramId): TNewtonId;
begin end;

operator *(const ALeft: TSquareKilogramId; const ARight: TNewtonPerSquareKilogramId): TNewtonId;
begin end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilogramId; const ARight: TMeterId): TSquareKilogramPerMeterId;
begin end;

operator /(const ALeft: TSquareKilogramId; const ARight: TSquareKilogramPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TSquareKilogramPerMeterId; const ARight: TMeterId): TSquareKilogramId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TSquareKilogramPerMeterId): TSquareKilogramId;
begin end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilogramId; const ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId;
begin end;

operator /(const ALeft: TSquareKilogramId; const ARight: TSquareKilogramPerSquareMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareKilogramPerSquareMeterId; const ARight: TSquareMeterId): TSquareKilogramId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TSquareKilogramPerSquareMeterId): TSquareKilogramId;
begin end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TSquareMeterId; const ARight: TSquareMeterPerSquareKilogramId): TSquareKilogramId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramId; const ARight: TSquareKilogramId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareKilogramId; const ARight: TSquareMeterPerSquareKilogramId): TSquareMeterId;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TNewtonId): TSquareMeterPerSquareKilogramId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramId; const ARight: TNewtonId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TSquareMeterPerSquareKilogramId): TNewtonId;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramPerSquareMeterId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TSquareKilogramPerSquareMeterId): TNewtonId;
begin end;

operator *(const ALeft: TSquareKilogramPerSquareMeterId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TNewtonId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonSquareMeterId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TSquareKilogramId): TNewtonSquareMeterId;
begin end;

operator *(const ALeft: TSquareKilogramId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TNewtonSquareMeterId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareKilogramId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TNewtonPerSquareKilogramId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TNewtonPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TSquareMeterId): TNewtonPerSquareKilogramId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJouleId; const ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TSquareKilogramPerMeterId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramId; const ARight: TSquareKilogramPerMeterId): TJouleId;
begin end;

operator *(const ALeft: TSquareKilogramPerMeterId; const ARight: TNewtonSquareMeterPerSquareKilogramId): TJouleId;
begin end;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvinId): TReciprocalKelvinId;
begin end;

operator /(const ALeft: double; const ARight: TReciprocalKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TReciprocalKelvinId; const ARight: TKelvinId): double;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TReciprocalKelvinId): double;
begin end;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilogramId; const ARight: TKelvinId): TKilogramKelvinId;
begin end;

operator /(const ALeft: TKilogramKelvinId; const ARight: TKilogramId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TKilogramId): TKilogramKelvinId;
begin end;

operator /(const ALeft: TKilogramKelvinId; const ARight: TKelvinId): TKilogramId;
begin end;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJouleId; const ARight: TKelvinId): TJoulePerKelvinId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TJoulePerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TJoulePerKelvinId; const ARight: TKelvinId): TJouleId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TJoulePerKelvinId): TJouleId;
begin end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJouleId; const ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TJoulePerKilogramPerKelvinId): TKilogramKelvinId;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinId; const ARight: TKilogramKelvinId): TJouleId;
begin end;

operator *(const ALeft: TKilogramKelvinId; const ARight: TJoulePerKilogramPerKelvinId): TJouleId;
begin end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TJoulePerKilogramPerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinId; const ARight: TKelvinId): TSquareMeterPerSquareSecondId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TJoulePerKilogramPerKelvinId): TSquareMeterPerSquareSecondId;
begin end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulePerKelvinId; const ARight: TKilogramId): TJoulePerKilogramPerKelvinId;
begin end;

operator /(const ALeft: TJoulePerKelvinId; const ARight: TJoulePerKilogramPerKelvinId): TKilogramId;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinId; const ARight: TKilogramId): TJoulePerKelvinId;
begin end;

operator *(const ALeft: TKilogramId; const ARight: TJoulePerKilogramPerKelvinId): TJoulePerKelvinId;
begin end;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeterId; const ARight: TKelvinId): TMeterKelvinId;
begin end;

operator /(const ALeft: TMeterKelvinId; const ARight: TMeterId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TMeterId): TMeterKelvinId;
begin end;

operator /(const ALeft: TMeterKelvinId; const ARight: TKelvinId): TMeterId;
begin end;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvinId; const ARight: TMeterId): TKelvinPerMeterId;
begin end;

operator /(const ALeft: TKelvinId; const ARight: TKelvinPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TKelvinPerMeterId; const ARight: TMeterId): TKelvinId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TKelvinPerMeterId): TKelvinId;
begin end;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWattId; const ARight: TMeterId): TWattPerMeterId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TWattPerMeterId; const ARight: TMeterId): TWattId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TWattPerMeterId): TWattId;
begin end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterId): TWattPerSquareMeterId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerSquareMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TWattPerSquareMeterId; const ARight: TSquareMeterId): TWattId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TWattPerSquareMeterId): TWattId;
begin end;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWattId; const ARight: TKelvinId): TWattPerKelvinId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TWattPerKelvinId; const ARight: TKelvinId): TWattId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TWattPerKelvinId): TWattId;
begin end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWattId; const ARight: TMeterKelvinId): TWattPerMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerMeterPerKelvinId): TMeterKelvinId;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinId; const ARight: TMeterKelvinId): TWattId;
begin end;

operator *(const ALeft: TMeterKelvinId; const ARight: TWattPerMeterPerKelvinId): TWattId;
begin end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattPerMeterId; const ARight: TKelvinId): TWattPerMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattPerMeterId; const ARight: TWattPerMeterPerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinId; const ARight: TKelvinId): TWattPerMeterId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TWattPerMeterPerKelvinId): TWattPerMeterId;
begin end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattPerKelvinId; const ARight: TMeterId): TWattPerMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattPerKelvinId; const ARight: TWattPerMeterPerKelvinId): TMeterId;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinId; const ARight: TMeterId): TWattPerKelvinId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TWattPerMeterPerKelvinId): TWattPerKelvinId;
begin end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattPerSquareMeterId; const ARight: TWattPerMeterPerKelvinId): TKelvinPerMeterId;
begin end;

operator *(const ALeft: TWattPerMeterPerKelvinId; const ARight: TKelvinPerMeterId): TWattPerSquareMeterId;
begin end;

operator *(const ALeft: TKelvinPerMeterId; const ARight: TWattPerMeterPerKelvinId): TWattPerSquareMeterId;
begin end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeterId; const ARight: TKelvinId): TSquareMeterKelvinId;
begin end;

operator /(const ALeft: TSquareMeterKelvinId; const ARight: TSquareMeterId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TSquareMeterId): TSquareMeterKelvinId;
begin end;

operator /(const ALeft: TSquareMeterKelvinId; const ARight: TKelvinId): TSquareMeterId;
begin end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerSquareMeterPerKelvinId): TSquareMeterKelvinId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinId; const ARight: TSquareMeterKelvinId): TWattId;
begin end;

operator *(const ALeft: TSquareMeterKelvinId; const ARight: TWattPerSquareMeterPerKelvinId): TWattId;
begin end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattPerSquareMeterId; const ARight: TWattPerSquareMeterPerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinId; const ARight: TKelvinId): TWattPerSquareMeterId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TWattPerSquareMeterPerKelvinId): TWattPerSquareMeterId;
begin end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattPerKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId;
begin end;

operator /(const ALeft: TWattPerKelvinId; const ARight: TWattPerSquareMeterPerKelvinId): TSquareMeterId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerKelvinId; const ARight: TSquareMeterId): TWattPerKelvinId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TWattPerSquareMeterPerKelvinId): TWattPerKelvinId;
begin end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeterId; const ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinId; const ARight: TSquareMeterId): TQuarticKelvinId;
begin end;

operator *(const ALeft: TQuarticKelvinId; const ARight: TSquareMeterId): TSquareMeterQuarticKelvinId;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinId; const ARight: TQuarticKelvinId): TSquareMeterId;
begin end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWattId; const ARight: TQuarticKelvinId): TWattPerQuarticKelvinId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerQuarticKelvinId): TQuarticKelvinId;
begin end;

operator *(const ALeft: TWattPerQuarticKelvinId; const ARight: TQuarticKelvinId): TWattId;
begin end;

operator *(const ALeft: TQuarticKelvinId; const ARight: TWattPerQuarticKelvinId): TWattId;
begin end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

operator /(const ALeft: TWattId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TSquareMeterQuarticKelvinId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinId; const ARight: TSquareMeterQuarticKelvinId): TWattId;
begin end;

operator *(const ALeft: TSquareMeterQuarticKelvinId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattId;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

operator /(const ALeft: TWattPerSquareMeterId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TQuarticKelvinId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinId; const ARight: TQuarticKelvinId): TWattPerSquareMeterId;
begin end;

operator *(const ALeft: TQuarticKelvinId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattPerSquareMeterId;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattPerQuarticKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

operator /(const ALeft: TWattPerQuarticKelvinId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TSquareMeterId;
begin end;

operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinId; const ARight: TSquareMeterId): TWattPerQuarticKelvinId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TWattPerSquareMeterPerQuarticKelvinId): TWattPerQuarticKelvinId;
begin end;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJouleId; const ARight: TMoleId): TJoulePerMoleId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TJoulePerMoleId): TMoleId;
begin end;

operator *(const ALeft: TJoulePerMoleId; const ARight: TMoleId): TJouleId;
begin end;

operator *(const ALeft: TMoleId; const ARight: TJoulePerMoleId): TJouleId;
begin end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoleId; const ARight: TKelvinId): TMoleKelvinId;
begin end;

operator /(const ALeft: TMoleKelvinId; const ARight: TMoleId): TKelvinId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TMoleId): TMoleKelvinId;
begin end;

operator /(const ALeft: TMoleKelvinId; const ARight: TKelvinId): TMoleId;
begin end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJouleId; const ARight: TMoleKelvinId): TJoulePerMolePerKelvinId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TJoulePerMolePerKelvinId): TMoleKelvinId;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinId; const ARight: TMoleKelvinId): TJouleId;
begin end;

operator *(const ALeft: TMoleKelvinId; const ARight: TJoulePerMolePerKelvinId): TJouleId;
begin end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulePerKelvinId; const ARight: TMoleId): TJoulePerMolePerKelvinId;
begin end;

operator /(const ALeft: TJoulePerKelvinId; const ARight: TJoulePerMolePerKelvinId): TMoleId;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinId; const ARight: TMoleId): TJoulePerKelvinId;
begin end;

operator *(const ALeft: TMoleId; const ARight: TJoulePerMolePerKelvinId): TJoulePerKelvinId;
begin end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulePerMoleId; const ARight: TKelvinId): TJoulePerMolePerKelvinId;
begin end;

operator /(const ALeft: TJoulePerMoleId; const ARight: TJoulePerMolePerKelvinId): TKelvinId;
begin end;

operator *(const ALeft: TJoulePerMolePerKelvinId; const ARight: TKelvinId): TJoulePerMoleId;
begin end;

operator *(const ALeft: TKelvinId; const ARight: TJoulePerMolePerKelvinId): TJoulePerMoleId;
begin end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhmId; const ARight: TMeterId): TOhmMeterId;
begin end;

operator /(const ALeft: TOhmMeterId; const ARight: TOhmId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TOhmId): TOhmMeterId;
begin end;

operator /(const ALeft: TOhmMeterId; const ARight: TMeterId): TOhmId;
begin end;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVoltId; const ARight: TMeterId): TVoltPerMeterId;
begin end;

operator /(const ALeft: TVoltId; const ARight: TVoltPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TVoltPerMeterId; const ARight: TMeterId): TVoltId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TVoltPerMeterId): TVoltId;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtonId; const ARight: TCoulombId): TVoltPerMeterId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TVoltPerMeterId): TCoulombId;
begin end;

operator *(const ALeft: TVoltPerMeterId; const ARight: TCoulombId): TNewtonId;
begin end;

operator *(const ALeft: TCoulombId; const ARight: TVoltPerMeterId): TNewtonId;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslaId; const ARight: TMeterPerSecondId): TVoltPerMeterId;
begin end;

operator /(const ALeft: TVoltPerMeterId; const ARight: TTeslaId): TMeterPerSecondId;
begin end;

operator *(const ALeft: TMeterPerSecondId; const ARight: TTeslaId): TVoltPerMeterId;
begin end;

operator /(const ALeft: TVoltPerMeterId; const ARight: TMeterPerSecondId): TTeslaId;
begin end;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombId; const ARight: TMeterId): TCoulombPerMeterId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TCoulombPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TCoulombPerMeterId; const ARight: TMeterId): TCoulombId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TCoulombPerMeterId): TCoulombId;
begin end;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombId; const ARight: TMeterId): TSquareCoulombPerMeterId;
begin end;

operator /(const ALeft: TSquareCoulombId; const ARight: TSquareCoulombPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TSquareCoulombPerMeterId; const ARight: TMeterId): TSquareCoulombId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TSquareCoulombPerMeterId): TSquareCoulombId;
begin end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombPerMeterId; const ARight: TCoulombId): TSquareCoulombPerMeterId;
begin end;

operator /(const ALeft: TSquareCoulombPerMeterId; const ARight: TCoulombPerMeterId): TCoulombId;
begin end;

operator *(const ALeft: TCoulombId; const ARight: TCoulombPerMeterId): TSquareCoulombPerMeterId;
begin end;

operator /(const ALeft: TSquareCoulombPerMeterId; const ARight: TCoulombId): TCoulombPerMeterId;
begin end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombId; const ARight: TSquareMeterId): TCoulombPerSquareMeterId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TCoulombPerSquareMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterId; const ARight: TSquareMeterId): TCoulombId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TCoulombPerSquareMeterId): TCoulombId;
begin end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombPerMeterId; const ARight: TMeterId): TCoulombPerSquareMeterId;
begin end;

operator /(const ALeft: TCoulombPerMeterId; const ARight: TCoulombPerSquareMeterId): TMeterId;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterId; const ARight: TMeterId): TCoulombPerMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TCoulombPerSquareMeterId): TCoulombPerMeterId;
begin end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TSquareMeterId; const ARight: TSquareMeterPerSquareCoulombId): TSquareCoulombId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareCoulombId; const ARight: TSquareCoulombId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareCoulombId; const ARight: TSquareMeterPerSquareCoulombId): TSquareMeterId;
begin end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareCoulombId): TNewtonPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TNewtonPerSquareCoulombId): TSquareCoulombId;
begin end;

operator *(const ALeft: TNewtonPerSquareCoulombId; const ARight: TSquareCoulombId): TNewtonId;
begin end;

operator *(const ALeft: TSquareCoulombId; const ARight: TNewtonPerSquareCoulombId): TNewtonId;
begin end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterId): TNewtonSquareMeterId;
begin end;

operator /(const ALeft: TNewtonSquareMeterId; const ARight: TNewtonId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TNewtonId): TNewtonSquareMeterId;
begin end;

operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareMeterId): TNewtonId;
begin end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TNewtonId): TSquareMeterPerSquareCoulombId;
begin end;

operator *(const ALeft: TSquareMeterPerSquareCoulombId; const ARight: TNewtonId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TSquareMeterPerSquareCoulombId): TNewtonId;
begin end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonSquareMeterId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TSquareCoulombId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TSquareCoulombId): TNewtonSquareMeterId;
begin end;

operator *(const ALeft: TSquareCoulombId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TNewtonSquareMeterId;
begin end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareCoulombId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TNewtonPerSquareCoulombId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TNewtonPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TSquareMeterId): TNewtonPerSquareCoulombId;
begin end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltPerMeterId; const ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TVoltPerMeterId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TCoulombPerSquareMeterId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TCoulombPerSquareMeterId): TVoltPerMeterId;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TVoltPerMeterId;
begin end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJouleId; const ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator /(const ALeft: TJouleId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TSquareCoulombPerMeterId;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TSquareCoulombPerMeterId): TJouleId;
begin end;

operator *(const ALeft: TSquareCoulombPerMeterId; const ARight: TNewtonSquareMeterPerSquareCoulombId): TJouleId;
begin end;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVoltId; const ARight: TMeterId): TVoltMeterId;
begin end;

operator /(const ALeft: TVoltMeterId; const ARight: TVoltId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TVoltId): TVoltMeterId;
begin end;

operator /(const ALeft: TVoltMeterId; const ARight: TMeterId): TVoltId;
begin end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltPerMeterId; const ARight: TSquareMeterId): TVoltMeterId;
begin end;

operator /(const ALeft: TVoltMeterId; const ARight: TVoltPerMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TSquareMeterId; const ARight: TVoltPerMeterId): TVoltMeterId;
begin end;

operator /(const ALeft: TVoltMeterId; const ARight: TSquareMeterId): TVoltPerMeterId;
begin end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeterId; const ARight: TSecondId): TVoltMeterPerSecondId;
begin end;

operator /(const ALeft: TVoltMeterId; const ARight: TVoltMeterPerSecondId): TSecondId;
begin end;

operator *(const ALeft: TVoltMeterPerSecondId; const ARight: TSecondId): TVoltMeterId;
begin end;

operator *(const ALeft: TSecondId; const ARight: TVoltMeterPerSecondId): TVoltMeterId;
begin end;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFaradId; const ARight: TMeterId): TFaradPerMeterId;
begin end;

operator /(const ALeft: TFaradId; const ARight: TFaradPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TFaradPerMeterId; const ARight: TMeterId): TFaradId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TFaradPerMeterId): TFaradId;
begin end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombId; const ARight: TVoltMeterId): TFaradPerMeterId;
begin end;

operator /(const ALeft: TCoulombId; const ARight: TFaradPerMeterId): TVoltMeterId;
begin end;

operator *(const ALeft: TFaradPerMeterId; const ARight: TVoltMeterId): TCoulombId;
begin end;

operator *(const ALeft: TVoltMeterId; const ARight: TFaradPerMeterId): TCoulombId;
begin end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombPerSquareMeterId; const ARight: TVoltPerMeterId): TFaradPerMeterId;
begin end;

operator /(const ALeft: TCoulombPerSquareMeterId; const ARight: TFaradPerMeterId): TVoltPerMeterId;
begin end;

operator *(const ALeft: TFaradPerMeterId; const ARight: TVoltPerMeterId): TCoulombPerSquareMeterId;
begin end;

operator *(const ALeft: TVoltPerMeterId; const ARight: TFaradPerMeterId): TCoulombPerSquareMeterId;
begin end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId;
begin end;

operator /(const ALeft: double; const ARight: TFaradPerMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

operator *(const ALeft: TFaradPerMeterId; const ARight: TNewtonSquareMeterPerSquareCoulombId): double;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombId; const ARight: TFaradPerMeterId): double;
begin end;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmpereId; const ARight: TMeterId): TAmperePerMeterId;
begin end;

operator /(const ALeft: TAmpereId; const ARight: TAmperePerMeterId): TMeterId;
begin end;

operator *(const ALeft: TAmperePerMeterId; const ARight: TMeterId): TAmpereId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TAmperePerMeterId): TAmpereId;
begin end;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeterId; const ARight: TAmpereId): TMeterPerAmpereId;
begin end;

operator /(const ALeft: TMeterId; const ARight: TMeterPerAmpereId): TAmpereId;
begin end;

operator *(const ALeft: TMeterPerAmpereId; const ARight: TAmpereId): TMeterId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TMeterPerAmpereId): TMeterId;
begin end;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslaId; const ARight: TMeterId): TTeslaMeterId;
begin end;

operator /(const ALeft: TTeslaMeterId; const ARight: TTeslaId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TTeslaId): TTeslaMeterId;
begin end;

operator /(const ALeft: TTeslaMeterId; const ARight: TMeterId): TTeslaId;
begin end;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtonId; const ARight: TAmpereId): TTeslaMeterId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: TTeslaMeterId): TAmpereId;
begin end;

operator *(const ALeft: TTeslaMeterId; const ARight: TAmpereId): TNewtonId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TTeslaMeterId): TNewtonId;
begin end;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslaId; const ARight: TAmpereId): TTeslaPerAmpereId;
begin end;

operator /(const ALeft: TTeslaId; const ARight: TTeslaPerAmpereId): TAmpereId;
begin end;

operator *(const ALeft: TTeslaPerAmpereId; const ARight: TAmpereId): TTeslaId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: TTeslaPerAmpereId): TTeslaId;
begin end;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenryId; const ARight: TMeterId): THenryPerMeterId;
begin end;

operator /(const ALeft: THenryId; const ARight: THenryPerMeterId): TMeterId;
begin end;

operator *(const ALeft: THenryPerMeterId; const ARight: TMeterId): THenryId;
begin end;

operator *(const ALeft: TMeterId; const ARight: THenryPerMeterId): THenryId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeterId; const ARight: TAmpereId): THenryPerMeterId;
begin end;

operator /(const ALeft: TTeslaMeterId; const ARight: THenryPerMeterId): TAmpereId;
begin end;

operator *(const ALeft: THenryPerMeterId; const ARight: TAmpereId): TTeslaMeterId;
begin end;

operator *(const ALeft: TAmpereId; const ARight: THenryPerMeterId): TTeslaMeterId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslaPerAmpereId; const ARight: TMeterId): THenryPerMeterId;
begin end;

operator /(const ALeft: THenryPerMeterId; const ARight: TTeslaPerAmpereId): TMeterId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TTeslaPerAmpereId): THenryPerMeterId;
begin end;

operator /(const ALeft: THenryPerMeterId; const ARight: TMeterId): TTeslaPerAmpereId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslaId; const ARight: TMeterPerAmpereId): THenryPerMeterId;
begin end;

operator /(const ALeft: THenryPerMeterId; const ARight: TTeslaId): TMeterPerAmpereId;
begin end;

operator *(const ALeft: TMeterPerAmpereId; const ARight: TTeslaId): THenryPerMeterId;
begin end;

operator /(const ALeft: THenryPerMeterId; const ARight: TMeterPerAmpereId): TTeslaId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslaId; const ARight: TAmperePerMeterId): THenryPerMeterId;
begin end;

operator /(const ALeft: TTeslaId; const ARight: THenryPerMeterId): TAmperePerMeterId;
begin end;

operator *(const ALeft: THenryPerMeterId; const ARight: TAmperePerMeterId): TTeslaId;
begin end;

operator *(const ALeft: TAmperePerMeterId; const ARight: THenryPerMeterId): TTeslaId;
begin end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareAmpereId): THenryPerMeterId;
begin end;

operator /(const ALeft: TNewtonId; const ARight: THenryPerMeterId): TSquareAmpereId;
begin end;

operator *(const ALeft: THenryPerMeterId; const ARight: TSquareAmpereId): TNewtonId;
begin end;

operator *(const ALeft: TSquareAmpereId; const ARight: THenryPerMeterId): TNewtonId;
begin end;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadianId; const ARight: TMeterId): TRadianPerMeterId;
begin end;

operator /(const ALeft: TRadianId; const ARight: TRadianPerMeterId): TMeterId;
begin end;

operator *(const ALeft: TRadianPerMeterId; const ARight: TMeterId): TRadianId;
begin end;

operator *(const ALeft: TMeterId; const ARight: TRadianPerMeterId): TRadianId;
begin end;

// main definition [ J/deg ] = [ J ] / [ deg ]
operator /(const ALeft: TJouleId; const ARight: TDegreeId): TJoulePerDegreeId;
begin end;

// main definition [ km/h ] = [ km ] / [ h ]
operator /(const ALeft: TKilometerId; const ARight: THourId): TKilometerPerHourId;
begin end;

// main definition [ dm/s ] = [ dm ] / [ s ]
operator /(const ALeft: TDecimeterId; const ARight: TSecondId): TDecimeterPerSecondId;
begin end;

// main definition [ cm/s ] = [ cm ] / [ s ]
operator /(const ALeft: TCentimeterId; const ARight: TSecondId): TCentimeterPerSecondId;
begin end;

// main definition [ mm/s ] = [ mm ] / [ s ]
operator /(const ALeft: TMillimeterId; const ARight: TSecondId): TMillimeterPerSecondId;
begin end;

// main definition [ km/h/s ] = [ km/h ] / [ s ]
operator /(const ALeft: TKilometerPerHourId; const ARight: TSecondId): TKilometerPerHourPerSecondId;
begin end;

// main definition [ dm/s2 ] = [ dm ] / [ s2 ]
operator /(const ALeft: TDecimeterId; const ARight: TSquareSecondId): TDecimeterPerSquareSecondId;
begin end;

// main definition [ cm/s2 ] = [ cm ] / [ s2 ]
operator /(const ALeft: TCentimeterId; const ARight: TSquareSecondId): TCentimeterPerSquareSecondId;
begin end;

// main definition [ mm/s2 ] = [ mm ] / [ s2 ]
operator /(const ALeft: TMillimeterId; const ARight: TSquareSecondId): TMillimeterPerSquareSecondId;
begin end;

//
operator /(const ALeft: TKilogramId; const ARight: TCubicMillimeterId): TKilogramPerCubicMillimeterId;
begin end;

//
operator /(const ALeft: TKilogramId; const ARight: TCubicCentimeterId): TKilogramPerCubicCentimeterId;
begin end;

//
operator /(const ALeft: TKilogramId; const ARight: TCubicDecimeterId): TKilogramPerCubicDecimeterId;
begin end;

//
operator /(const ALeft: THectogramId; const ARight: TCubicMeterId): THectogramPerCubicMeterId;
begin end;

//
operator /(const ALeft: TDecagramId; const ARight: TCubicMeterId): TDecagramPerCubicMeterId;
begin end;

//
operator /(const ALeft: TGramId; const ARight: TCubicMeterId): TGramPerCubicMeterId;
begin end;

//
operator /(const ALeft: TNewtonId; const ARight: TMillimeterId): TNewtonPerMillimeterId;
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
