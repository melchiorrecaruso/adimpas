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

unit ADim2;

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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterId): TSquareMeterId; inline;

// main definition [ m3 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterId): TCubicMeterId; inline;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TMeterId): TQuarticMeterId; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterId): TQuarticMeterId; inline;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TMeterId): TQuinticMeterId; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSquareMeterId): TQuinticMeterId; inline;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TMeterId): TSexticMeterId; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TSquareMeterId): TSexticMeterId; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TCubicMeterId): TSexticMeterId; inline;

// main definition [ kg2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramId): TSquareKilogramId; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TAmpereId): TSquareAmpereId; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TKelvinId): TSquareKelvinId; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TKelvinId): TCubicKelvinId; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TSquareKelvinId): TQuarticKelvinId; inline;

//
operator *(const {%H-}ALeft: TCubicKelvinId; const {%H-}ARight: TKelvinId): TQuarticKelvinId; inline;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianId): TSteradianId; inline;

// main definition [ Hz ] = 1 / [ s ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSecondId): THertzId; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const {%H-}ALeft: THertzId; const {%H-}ARight: THertzId): TSquareHertzId; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonId; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TPascalId; inline;

// main definition [ J ] = [ N ] * [ m ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TJouleId; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TCubicMeterId): TJouleId; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSecondId): TWattId; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianPerSecondId): TWattId; inline;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const {%H-}ALeft: TSquareAmpereId; const {%H-}ARight: TOhmId): TWattId; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterPerSecondId): TWattId; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TAmpereId): TCoulombId; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombId): TSquareCoulombId; inline;

// main definition [ V ] = [ W ] / [ A ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TAmpereId): TVoltId; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TCoulombId): TVoltId; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TVoltId): TSquareVoltId; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const {%H-}ALeft: TWattId; const {%H-}ARight: TOhmId): TSquareVoltId; inline;

// main definition [ F ] = [ C ] / [ V ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltId): TFaradId; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TJouleId): TFaradId; inline;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TAmpereId): TOhmId; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const {%H-}ALeft: TSecondId; const {%H-}ARight: TFaradId): TOhmId; inline;

// main definition [ S ] = 1 / [ Ω ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TOhmId): TSiemensId; inline;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TSecondId): TWeberId; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TSquareMeterId): TTeslaId; inline;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TAmpereId): THenryId; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TSecondId): THenryId; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const {%H-}ALeft: TOhmId; const {%H-}ARight: THertzId): THenryId; inline;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const {%H-}ALeft: TCandelaId; const {%H-}ARight: TSteradianId): TLumenId; inline;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TSquareMeterId): TLuxId; inline;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const {%H-}ALeft: TMoleId; const {%H-}ARight: TSecondId): TKatalId; inline;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianId): TJoulePerRadianId; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSecondId): TMeterPerSecondId; inline;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TSecondId): TMeterPerSquareSecondId; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareSecondId): TMeterPerSquareSecondId; inline;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSecondId): TRadianPerSecondId; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterId): TRadianPerSecondId; inline;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSquareSecondId): TRadianPerSquareSecondId; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TSecondId): TRadianPerSquareSecondId; inline;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterId): TKilogramPerMeterId; inline;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramPerSquareMeterId; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicMeterId): TKilogramPerCubicMeterId; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCubicMeterId): TNewtonPerCubicMeterId; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerCubicMeterId; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId; inline;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareMeterId): TNewtonPerMeterId; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSecondId): TKilogramMeterPerSecondId; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSecondId): TKilogramMeterPerSecondId; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramSquareMeterId; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TSecondId): TKilogramSquareMeterPerSecondId; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareSecondId): TSquareMeterPerSquareSecondId; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramId): TSquareMeterPerSquareSecondId; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId; inline;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TSquareSecondId): TSteradianPerSquareSecondId; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TRadianPerSecondId): TSteradianPerSquareSecondId; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId; inline;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSecondId): TCubicMeterPerSecondId; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterPerSecondId): TCubicMeterPerSecondId; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TSecondId): TPascalSecondId; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSecondId): TSquareMeterPerSecondId; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramId): TNewtonPerSquareKilogramId; inline;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TMeterId): TSquareKilogramPerMeterId; inline;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TKelvinId): TReciprocalKelvinId; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKelvinId): TKilogramKelvinId; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKelvinId): TJoulePerKelvinId; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TKelvinId): TJoulePerKilogramPerKelvinId; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TKilogramId): TJoulePerKilogramPerKelvinId; inline;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TKelvinId): TMeterKelvinId; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TMeterId): TKelvinPerMeterId; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterId): TWattPerMeterId; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterId; inline;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TKelvinId): TWattPerKelvinId; inline;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterKelvinId): TWattPerMeterPerKelvinId; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const {%H-}ALeft: TWattPerMeterId; const {%H-}ARight: TKelvinId): TWattPerMeterPerKelvinId; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TMeterId): TWattPerMeterPerKelvinId; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TKelvinId): TSquareMeterKelvinId; inline;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinId): TWattPerSquareMeterPerKelvinId; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId; inline;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TQuarticKelvinId): TWattPerQuarticKelvinId; inline;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleId): TJoulePerMoleId; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const {%H-}ALeft: TMoleId; const {%H-}ARight: TKelvinId): TMoleKelvinId; inline;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleKelvinId): TJoulePerMolePerKelvinId; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TMoleId): TJoulePerMolePerKelvinId; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const {%H-}ALeft: TJoulePerMoleId; const {%H-}ARight: TKelvinId): TJoulePerMolePerKelvinId; inline;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TMeterId): TOhmMeterId; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltPerMeterId; inline;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCoulombId): TVoltPerMeterId; inline;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerSecondId): TVoltPerMeterId; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TMeterId): TCoulombPerMeterId; inline;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TMeterId): TSquareCoulombPerMeterId; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TCoulombId): TSquareCoulombPerMeterId; inline;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TSquareMeterId): TCoulombPerSquareMeterId; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TMeterId): TCoulombPerSquareMeterId; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId; inline;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareCoulombId): TNewtonPerSquareCoulombId; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterId; inline;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltMeterId; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TSquareMeterId): TVoltMeterId; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TSecondId): TVoltMeterPerSecondId; inline;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const {%H-}ALeft: TFaradId; const {%H-}ARight: TMeterId): TFaradPerMeterId; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltMeterId): TFaradPerMeterId; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TVoltPerMeterId): TFaradPerMeterId; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId; inline;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TMeterId): TAmperePerMeterId; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TAmpereId): TMeterPerAmpereId; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterId): TTeslaMeterId; inline;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TAmpereId): TTeslaMeterId; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmpereId): TTeslaPerAmpereId; inline;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TAmpereId): THenryPerMeterId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const {%H-}ALeft: TTeslaPerAmpereId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerAmpereId): THenryPerMeterId; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmperePerMeterId): THenryPerMeterId; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareAmpereId): THenryPerMeterId; inline;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TMeterId): TRadianPerMeterId; inline;

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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterId; const ARight: TMeterId): TSquareMeterId;
begin end;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeterId; const ARight: TMeterId): TCubicMeterId;
begin end;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const ALeft: TCubicMeterId; const ARight: TMeterId): TQuarticMeterId;
begin end;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const ALeft: TSquareMeterId; const ARight: TSquareMeterId): TQuarticMeterId;
begin end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeterId; const ARight: TMeterId): TQuinticMeterId;
begin end;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const ALeft: TCubicMeterId; const ARight: TSquareMeterId): TQuinticMeterId;
begin end;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const ALeft: TQuinticMeterId; const ARight: TMeterId): TSexticMeterId;
begin end;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const ALeft: TQuarticMeterId; const ARight: TSquareMeterId): TSexticMeterId;
begin end;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const ALeft: TCubicMeterId; const ARight: TCubicMeterId): TSexticMeterId;
begin end;

// main definition [ kg2 ]
operator *(const ALeft: TKilogramId; const ARight: TKilogramId): TSquareKilogramId;
begin end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereId; const ARight: TAmpereId): TSquareAmpereId;
begin end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinId; const ARight: TKelvinId): TSquareKelvinId;
begin end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvinId; const ARight: TKelvinId): TCubicKelvinId;
begin end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvinId; const ARight: TSquareKelvinId): TQuarticKelvinId;
begin end;

//
operator *(const ALeft: TCubicKelvinId; const ARight: TKelvinId): TQuarticKelvinId;
begin end;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadianId; const ARight: TRadianId): TSteradianId;
begin end;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSecondId): THertzId;
begin end;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertzId; const ARight: THertzId): TSquareHertzId;
begin end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSquareSecondId): TNewtonId;
begin end;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareMeterId): TPascalId;
begin end;

// main definition [ J ] = [ N ] * [ m ]
operator *(const ALeft: TNewtonId; const ARight: TMeterId): TJouleId;
begin end;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascalId; const ARight: TCubicMeterId): TJouleId;
begin end;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJouleId; const ARight: TSecondId): TWattId;
begin end;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const ALeft: TJouleId; const ARight: TRadianPerSecondId): TWattId;
begin end;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const ALeft: TSquareAmpereId; const ARight: TOhmId): TWattId;
begin end;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const ALeft: TNewtonId; const ARight: TMeterPerSecondId): TWattId;
begin end;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSecondId; const ARight: TAmpereId): TCoulombId;
begin end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombId; const ARight: TCoulombId): TSquareCoulombId;
begin end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWattId; const ARight: TAmpereId): TVoltId;
begin end;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const ALeft: TJouleId; const ARight: TCoulombId): TVoltId;
begin end;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVoltId; const ARight: TVoltId): TSquareVoltId;
begin end;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWattId; const ARight: TOhmId): TSquareVoltId;
begin end;

// main definition [ F ] = [ C ] / [ V ]
operator /(const ALeft: TCoulombId; const ARight: TVoltId): TFaradId;
begin end;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const ALeft: TSquareCoulombId; const ARight: TJouleId): TFaradId;
begin end;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const ALeft: TVoltId; const ARight: TAmpereId): TOhmId;
begin end;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const ALeft: TSecondId; const ARight: TFaradId): TOhmId;
begin end;

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhmId): TSiemensId;
begin end;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVoltId; const ARight: TSecondId): TWeberId;
begin end;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWeberId; const ARight: TSquareMeterId): TTeslaId;
begin end;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const ALeft: TWeberId; const ARight: TAmpereId): THenryId;
begin end;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const ALeft: TOhmId; const ARight: TSecondId): THenryId;
begin end;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const ALeft: TOhmId; const ARight: THertzId): THenryId;
begin end;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const ALeft: TCandelaId; const ARight: TSteradianId): TLumenId;
begin end;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const ALeft: TLumenId; const ARight: TSquareMeterId): TLuxId;
begin end;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoleId; const ARight: TSecondId): TKatalId;
begin end;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const ALeft: TJouleId; const ARight: TRadianId): TJoulePerRadianId;
begin end;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeterId; const ARight: TSecondId): TMeterPerSecondId;
begin end;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const ALeft: TMeterPerSecondId; const ARight: TSecondId): TMeterPerSquareSecondId;
begin end;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const ALeft: TMeterId; const ARight: TSquareSecondId): TMeterPerSquareSecondId;
begin end;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const ALeft: TSteradianPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadianId; const ARight: TSecondId): TRadianPerSecondId;
begin end;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const ALeft: TMeterPerSecondId; const ARight: TMeterId): TRadianPerSecondId;
begin end;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const ALeft: TRadianId; const ARight: TSquareSecondId): TRadianPerSquareSecondId;
begin end;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const ALeft: TRadianPerSecondId; const ARight: TSecondId): TRadianPerSquareSecondId;
begin end;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const ALeft: TKilogramId; const ARight: TMeterId): TKilogramPerMeterId;
begin end;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramPerSquareMeterId;
begin end;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilogramId; const ARight: TCubicMeterId): TKilogramPerCubicMeterId;
begin end;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtonId; const ARight: TCubicMeterId): TNewtonPerCubicMeterId;
begin end;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerCubicMeterId;
begin end;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId;
begin end;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const ALeft: TNewtonId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const ALeft: TJouleId; const ARight: TSquareMeterId): TNewtonPerMeterId;
begin end;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSecondId): TKilogramMeterPerSecondId;
begin end;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtonId; const ARight: TSecondId): TKilogramMeterPerSecondId;
begin end;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramSquareMeterId;
begin end;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterId; const ARight: TSecondId): TKilogramSquareMeterPerSecondId;
begin end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const ALeft: TKilogramSquareMeterId; const ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId;
begin end;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareSecondId): TSquareMeterPerSquareSecondId;
begin end;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMeterPerSecondId; const ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId;
begin end;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJouleId; const ARight: TKilogramId): TSquareMeterPerSquareSecondId;
begin end;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const ALeft: TPascalId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId;
begin end;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const ALeft: TSteradianId; const ARight: TSquareSecondId): TSteradianPerSquareSecondId;
begin end;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadianPerSecondId; const ARight: TRadianPerSecondId): TSteradianPerSquareSecondId;
begin end;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJouleId; const ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId;
begin end;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const ALeft: TCubicMeterId; const ARight: TSecondId): TCubicMeterPerSecondId;
begin end;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const ALeft: TSquareMeterId; const ARight: TMeterPerSecondId): TCubicMeterPerSecondId;
begin end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascalId; const ARight: TSecondId): TPascalSecondId;
begin end;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeterId; const ARight: TSecondId): TSquareMeterPerSecondId;
begin end;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalSecondId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId;
begin end;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramId): TNewtonPerSquareKilogramId;
begin end;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const ALeft: TSquareKilogramId; const ARight: TMeterId): TSquareKilogramPerMeterId;
begin end;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const ALeft: TSquareKilogramId; const ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId;
begin end;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareKilogramId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJouleId; const ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvinId): TReciprocalKelvinId;
begin end;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilogramId; const ARight: TKelvinId): TKilogramKelvinId;
begin end;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJouleId; const ARight: TKelvinId): TJoulePerKelvinId;
begin end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJouleId; const ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulePerKelvinId; const ARight: TKilogramId): TJoulePerKilogramPerKelvinId;
begin end;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeterId; const ARight: TKelvinId): TMeterKelvinId;
begin end;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvinId; const ARight: TMeterId): TKelvinPerMeterId;
begin end;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWattId; const ARight: TMeterId): TWattPerMeterId;
begin end;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterId): TWattPerSquareMeterId;
begin end;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWattId; const ARight: TKelvinId): TWattPerKelvinId;
begin end;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWattId; const ARight: TMeterKelvinId): TWattPerMeterPerKelvinId;
begin end;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const ALeft: TWattPerMeterId; const ARight: TKelvinId): TWattPerMeterPerKelvinId;
begin end;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const ALeft: TWattPerKelvinId; const ARight: TMeterId): TWattPerMeterPerKelvinId;
begin end;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId;
begin end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeterId; const ARight: TKelvinId): TSquareMeterKelvinId;
begin end;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const ALeft: TWattPerKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId;
begin end;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeterId; const ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId;
begin end;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWattId; const ARight: TQuarticKelvinId): TWattPerQuarticKelvinId;
begin end;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWattId; const ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const ALeft: TWattPerQuarticKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJouleId; const ARight: TMoleId): TJoulePerMoleId;
begin end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoleId; const ARight: TKelvinId): TMoleKelvinId;
begin end;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJouleId; const ARight: TMoleKelvinId): TJoulePerMolePerKelvinId;
begin end;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const ALeft: TJoulePerKelvinId; const ARight: TMoleId): TJoulePerMolePerKelvinId;
begin end;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const ALeft: TJoulePerMoleId; const ARight: TKelvinId): TJoulePerMolePerKelvinId;
begin end;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhmId; const ARight: TMeterId): TOhmMeterId;
begin end;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVoltId; const ARight: TMeterId): TVoltPerMeterId;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtonId; const ARight: TCoulombId): TVoltPerMeterId;
begin end;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const ALeft: TTeslaId; const ARight: TMeterPerSecondId): TVoltPerMeterId;
begin end;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombId; const ARight: TMeterId): TCoulombPerMeterId;
begin end;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const ALeft: TSquareCoulombId; const ARight: TMeterId): TSquareCoulombPerMeterId;
begin end;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const ALeft: TCoulombPerMeterId; const ARight: TCoulombId): TSquareCoulombPerMeterId;
begin end;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const ALeft: TCoulombId; const ARight: TSquareMeterId): TCoulombPerSquareMeterId;
begin end;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const ALeft: TCoulombPerMeterId; const ARight: TMeterId): TCoulombPerSquareMeterId;
begin end;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeterId; const ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId;
begin end;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareCoulombId): TNewtonPerSquareCoulombId;
begin end;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterId): TNewtonSquareMeterId;
begin end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonPerSquareCoulombId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const ALeft: TVoltPerMeterId; const ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJouleId; const ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVoltId; const ARight: TMeterId): TVoltMeterId;
begin end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltPerMeterId; const ARight: TSquareMeterId): TVoltMeterId;
begin end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltMeterId; const ARight: TSecondId): TVoltMeterPerSecondId;
begin end;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const ALeft: TFaradId; const ARight: TMeterId): TFaradPerMeterId;
begin end;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombId; const ARight: TVoltMeterId): TFaradPerMeterId;
begin end;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombPerSquareMeterId; const ARight: TVoltPerMeterId): TFaradPerMeterId;
begin end;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId;
begin end;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmpereId; const ARight: TMeterId): TAmperePerMeterId;
begin end;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeterId; const ARight: TAmpereId): TMeterPerAmpereId;
begin end;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslaId; const ARight: TMeterId): TTeslaMeterId;
begin end;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtonId; const ARight: TAmpereId): TTeslaMeterId;
begin end;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslaId; const ARight: TAmpereId): TTeslaPerAmpereId;
begin end;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const ALeft: THenryId; const ARight: TMeterId): THenryPerMeterId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeterId; const ARight: TAmpereId): THenryPerMeterId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslaPerAmpereId; const ARight: TMeterId): THenryPerMeterId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslaId; const ARight: TMeterPerAmpereId): THenryPerMeterId;
begin end;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslaId; const ARight: TAmperePerMeterId): THenryPerMeterId;
begin end;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const ALeft: TNewtonId; const ARight: TSquareAmpereId): THenryPerMeterId;
begin end;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const ALeft: TRadianId; const ARight: TMeterId): TRadianPerMeterId;
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
