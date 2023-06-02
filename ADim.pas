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
  { TQuantity }

  generic TQuantity<U> = record
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

  { TQuantityId }

  generic TQuantityId<U> = record
    type TSelf = specialize TQuantityId<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;

  end;

  { TFactoredQuantityId }

  generic TFactoredQuantityId<BaseU, U> = record
    type TSelf = specialize TFactoredQuantityId<BaseU, U>;
    type TBaseQuantity = specialize TQuantity<BaseU>;
    type TFactoredQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TFactoredQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}TheUnit: TSelf): TBaseQuantity;
  end;

{ Unit of Second }

type
  TSecondUnit = record
    const Symbol : string = 's';
    const Name   : string = 'second';
  end;
  TSeconds = specialize TQuantity<TSecondUnit>;
  TSecondId = specialize TQuantityId<TSecondUnit>;

var
  s: TSecondId;

{ Unit of SquareSecond }

type
  TSquareSecondUnit = record
    const Symbol : string = 's2';
    const Name   : string = 'square second';
  end;
  TSquareSeconds = specialize TQuantity<TSquareSecondUnit>;
  TSquareSecondId = specialize TQuantityId<TSquareSecondUnit>;

var
  s2: TSquareSecondId;

{ Unit of Meter }

type
  TMeterUnit = record
    const Symbol : string = 'm';
    const Name   : string = 'meter';
  end;
  TMeters = specialize TQuantity<TMeterUnit>;
  TMeterId = specialize TQuantityId<TMeterUnit>;

var
  m: TMeterId;

{ Unit of SquareMeter }

type
  TSquareMeterUnit = record
    const Symbol : string = 'm2';
    const Name   : string = 'square meter';
  end;
  TSquareMeters = specialize TQuantity<TSquareMeterUnit>;
  TSquareMeterId = specialize TQuantityId<TSquareMeterUnit>;

var
  m2: TSquareMeterId;

{ Unit of CubicMeter }

type
  TCubicMeterUnit = record
    const Symbol : string = 'm3';
    const Name   : string = 'cubic meter';
  end;
  TCubicMeters = specialize TQuantity<TCubicMeterUnit>;
  TCubicMeterId = specialize TQuantityId<TCubicMeterUnit>;

var
  m3: TCubicMeterId;

{ Unit of QuarticMeter }

type
  TQuarticMeterUnit = record
    const Symbol : string = 'm4';
    const Name   : string = 'quartic meter';
  end;
  TQuarticMeters = specialize TQuantity<TQuarticMeterUnit>;
  TQuarticMeterId = specialize TQuantityId<TQuarticMeterUnit>;

var
  m4: TQuarticMeterId;

{ Unit of QuinticMeter }

type
  TQuinticMeterUnit = record
    const Symbol : string = 'm5';
    const Name   : string = 'quintic meter';
  end;
  TQuinticMeters = specialize TQuantity<TQuinticMeterUnit>;
  TQuinticMeterId = specialize TQuantityId<TQuinticMeterUnit>;

var
  m5: TQuinticMeterId;

{ Unit of SexticMeter }

type
  TSexticMeterUnit = record
    const Symbol : string = 'm6';
    const Name   : string = 'sextic meter';
  end;
  TSexticMeters = specialize TQuantity<TSexticMeterUnit>;
  TSexticMeterId = specialize TQuantityId<TSexticMeterUnit>;

var
  m6: TSexticMeterId;

{ Unit of Kilogram }

type
  TKilogramUnit = record
    const Symbol : string = 'kg';
    const Name   : string = 'kilogram';
  end;
  TKilograms = specialize TQuantity<TKilogramUnit>;
  TKilogramId = specialize TQuantityId<TKilogramUnit>;

var
  kg: TKilogramId;

{ Unit of SquareKilogram }

type
  TSquareKilogramUnit = record
    const Symbol : string = 'kg2';
    const Name   : string = 'square kilogram';
  end;
  TSquareKilograms = specialize TQuantity<TSquareKilogramUnit>;
  TSquareKilogramId = specialize TQuantityId<TSquareKilogramUnit>;

var
  kg2: TSquareKilogramId;

{ Unit of Ampere }

type
  TAmpereUnit = record
    const Symbol : string = 'A';
    const Name   : string = 'ampere';
  end;
  TAmperes = specialize TQuantity<TAmpereUnit>;
  TAmpereId = specialize TQuantityId<TAmpereUnit>;

var
  A: TAmpereId;

{ Unit of SquareAmpere }

type
  TSquareAmpereUnit = record
    const Symbol : string = 'A2';
    const Name   : string = 'square ampere';
  end;
  TSquareAmperes = specialize TQuantity<TSquareAmpereUnit>;
  TSquareAmpereId = specialize TQuantityId<TSquareAmpereUnit>;

var
  A2: TSquareAmpereId;

{ Unit of Kelvin }

type
  TKelvinUnit = record
    const Symbol : string = 'K';
    const Name   : string = 'kelvin';
  end;
  TKelvins = specialize TQuantity<TKelvinUnit>;
  TKelvinId = specialize TQuantityId<TKelvinUnit>;

var
  K: TKelvinId;

{ Unit of SquareKelvin }

type
  TSquareKelvinUnit = record
    const Symbol : string = 'K2';
    const Name   : string = 'square kelvin';
  end;
  TSquareKelvins = specialize TQuantity<TSquareKelvinUnit>;
  TSquareKelvinId = specialize TQuantityId<TSquareKelvinUnit>;

var
  K2: TSquareKelvinId;

{ Unit of CubicKelvin }

type
  TCubicKelvinUnit = record
    const Symbol : string = 'K3';
    const Name   : string = 'cubic kelvin';
  end;
  TCubicKelvins = specialize TQuantity<TCubicKelvinUnit>;
  TCubicKelvinId = specialize TQuantityId<TCubicKelvinUnit>;

var
  K3: TCubicKelvinId;

{ Unit of QuarticKelvin }

type
  TQuarticKelvinUnit = record
    const Symbol : string = '';
    const Name   : string = '';
  end;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;
  TQuarticKelvinId = specialize TQuantityId<TQuarticKelvinUnit>;

{ Unit of Mole }

type
  TMoleUnit = record
    const Symbol : string = 'mol';
    const Name   : string = 'mole';
  end;
  TMoles = specialize TQuantity<TMoleUnit>;
  TMoleId = specialize TQuantityId<TMoleUnit>;

var
  mol: TMoleId;

{ Unit of Candela }

type
  TCandelaUnit = record
    const Symbol : string = 'cd';
    const Name   : string = 'candela';
  end;
  TCandelas = specialize TQuantity<TCandelaUnit>;
  TCandelaId = specialize TQuantityId<TCandelaUnit>;

var
  cd: TCandelaId;

{ Unit of Radian }

type
  TRadianUnit = record
    const Symbol : string = 'rad';
    const Name   : string = 'radian';
  end;
  TRadians = specialize TQuantity<TRadianUnit>;
  TRadianId = specialize TQuantityId<TRadianUnit>;

var
  rad: TRadianId;

{ Unit of Steradian }

type
  TSteradianUnit = record
    const Symbol : string = 'sr';
    const Name   : string = 'steradian';
  end;
  TSteradians = specialize TQuantity<TSteradianUnit>;
  TSteradianId = specialize TQuantityId<TSteradianUnit>;

var
  sr: TSteradianId;

{ Unit of Hertz }

type
  THertzUnit = record
    const Symbol : string = 'Hz';
    const Name   : string = 'hertz';
  end;
  THertz = specialize TQuantity<THertzUnit>;
  THertzId = specialize TQuantityId<THertzUnit>;

var
  Hz: THertzId;

{ Unit of SquareHertz }

type
  TSquareHertzUnit = record
    const Symbol : string = 'Hz2';
    const Name   : string = 'square hertz';
  end;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;
  TSquareHertzId = specialize TQuantityId<TSquareHertzUnit>;

{ Unit of Newton }

type
  TNewtonUnit = record
    const Symbol : string = 'N';
    const Name   : string = 'newton';
  end;
  TNewtons = specialize TQuantity<TNewtonUnit>;
  TNewtonId = specialize TQuantityId<TNewtonUnit>;

var
  N: TNewtonId;

{ Unit of Pascal }

type
  TPascalUnit = record
    const Symbol : string = 'Pa';
    const Name   : string = 'pascal';
  end;
  TPascals = specialize TQuantity<TPascalUnit>;
  TPascalId = specialize TQuantityId<TPascalUnit>;

var
  Pa: TPascalId;

{ Unit of Joule }

type
  TJouleUnit = record
    const Symbol : string = 'J';
    const Name   : string = 'joule';
  end;
  TJoules = specialize TQuantity<TJouleUnit>;
  TJouleId = specialize TQuantityId<TJouleUnit>;

var
  J: TJouleId;

{ Unit of Watt }

type
  TWattUnit = record
    const Symbol : string = 'W';
    const Name   : string = 'watt';
  end;
  TWatts = specialize TQuantity<TWattUnit>;
  TWattId = specialize TQuantityId<TWattUnit>;

var
  W: TWattId;

{ Unit of Coulomb }

type
  TCoulombUnit = record
    const Symbol : string = 'C';
    const Name   : string = 'coulomb';
  end;
  TCoulombs = specialize TQuantity<TCoulombUnit>;
  TCoulombId = specialize TQuantityId<TCoulombUnit>;

var
  C: TCoulombId;

{ Unit of SquareCoulomb }

type
  TSquareCoulombUnit = record
    const Symbol : string = 'C2';
    const Name   : string = 'square coulomb';
  end;
  TSquareCoulombs = specialize TQuantity<TSquareCoulombUnit>;
  TSquareCoulombId = specialize TQuantityId<TSquareCoulombUnit>;

var
  C2: TSquareCoulombId;

{ Unit of Volt }

type
  TVoltUnit = record
    const Symbol : string = 'V';
    const Name   : string = 'volt';
  end;
  TVolts = specialize TQuantity<TVoltUnit>;
  TVoltId = specialize TQuantityId<TVoltUnit>;

var
  V: TVoltId;

{ Unit of SquareVolt }

type
  TSquareVoltUnit = record
    const Symbol : string = 'V2';
    const Name   : string = 'square volt';
  end;
  TSquareVolts = specialize TQuantity<TSquareVoltUnit>;
  TSquareVoltId = specialize TQuantityId<TSquareVoltUnit>;

var
  V2: TSquareVoltId;

{ Unit of Farad }

type
  TFaradUnit = record
    const Symbol : string = 'F';
    const Name   : string = 'farad';
  end;
  TFarads = specialize TQuantity<TFaradUnit>;
  TFaradId = specialize TQuantityId<TFaradUnit>;

var
  F: TFaradId;

{ Unit of Ohm }

type
  TOhmUnit = record
    const Symbol : string = 'Ω';
    const Name   : string = 'ohm';
  end;
  TOhms = specialize TQuantity<TOhmUnit>;
  TOhmId = specialize TQuantityId<TOhmUnit>;

var
  ohm: TOhmId;

{ Unit of Siemens }

type
  TSiemensUnit = record
    const Symbol : string = 'S';
    const Name   : string = 'siemens';
  end;
  TSiemens = specialize TQuantity<TSiemensUnit>;
  TSiemensId = specialize TQuantityId<TSiemensUnit>;

var
  siemens: TSiemensId;

{ Unit of Weber }

type
  TWeberUnit = record
    const Symbol : string = 'Wb';
    const Name   : string = 'weber';
  end;
  TWebers = specialize TQuantity<TWeberUnit>;
  TWeberId = specialize TQuantityId<TWeberUnit>;

var
  Wb: TWeberId;

{ Unit of Tesla }

type
  TTeslaUnit = record
    const Symbol : string = 'T';
    const Name   : string = 'tesla';
  end;
  TTeslas = specialize TQuantity<TTeslaUnit>;
  TTeslaId = specialize TQuantityId<TTeslaUnit>;

var
  T: TTeslaId;

{ Unit of Henry }

type
  THenryUnit = record
    const Symbol : string = 'H';
    const Name   : string = 'henry';
  end;
  THenrys = specialize TQuantity<THenryUnit>;
  THenryId = specialize TQuantityId<THenryUnit>;

var
  H: THenryId;

{ Unit of Lumen }

type
  TLumenUnit = record
    const Symbol : string = 'lm';
    const Name   : string = 'lumen';
  end;
  TLumens = specialize TQuantity<TLumenUnit>;
  TLumenId = specialize TQuantityId<TLumenUnit>;

var
  lm: TLumenId;

{ Unit of Lux }

type
  TLuxUnit = record
    const Symbol : string = 'lx';
    const Name   : string = 'lux';
  end;
  TLux = specialize TQuantity<TLuxUnit>;
  TLuxId = specialize TQuantityId<TLuxUnit>;

var
  lx: TLuxId;

{ Unit of Bequerel }

type
  TBequerelUnit = record
    const Symbol : string = 'Bq';
    const Name   : string = 'bequerel';
  end;
  TBequerels = specialize TQuantity<TBequerelUnit>;
  TBequerelId = specialize TQuantityId<TBequerelUnit>;

var
  Bq: TBequerelId;

{ Unit of Gray }

type
  TGrayUnit = record
    const Symbol : string = 'Gy';
    const Name   : string = 'gray';
  end;
  TGrays = specialize TQuantity<TGrayUnit>;
  TGrayId = specialize TQuantityId<TGrayUnit>;

var
  Gy: TGrayId;

{ Unit of Sievert }

type
  TSievertUnit = record
    const Symbol : string = 'Sv';
    const Name   : string = 'sievert';
  end;
  TSieverts = specialize TQuantity<TSievertUnit>;
  TSievertId = specialize TQuantityId<TSievertUnit>;

var
  Sv: TSievertId;

{ Unit of Katal }

type
  TKatalUnit = record
    const Symbol : string = 'kat';
    const Name   : string = 'katal';
  end;
  TKatals = specialize TQuantity<TKatalUnit>;
  TKatalId = specialize TQuantityId<TKatalUnit>;

var
  kat: TKatalId;

{ Unit of NewtonMeter }

type
  TNewtonMeterUnit = record
    const Symbol : string = 'N·m';
    const Name   : string = 'newton meter';
  end;
  TNewtonMeters = specialize TQuantity<TNewtonMeterUnit>;
  TNewtonMeterId = specialize TQuantityId<TNewtonMeterUnit>;

{ Unit of JoulePerRadian }

type
  TJoulePerRadianUnit = record
    const Symbol : string = 'J/rad';
    const Name   : string = 'joule per radian';
  end;
  TJoulesPerRadian = specialize TQuantity<TJoulePerRadianUnit>;
  TJoulePerRadianId = specialize TQuantityId<TJoulePerRadianUnit>;

{ Unit of NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianUnit = record
    const Symbol : string = 'N·m/rad';
    const Name   : string = 'newton meter per radian';
  end;
  TNewtonMetersPerRadian = specialize TQuantity<TNewtonMeterPerRadianUnit>;
  TNewtonMeterPerRadianId = specialize TQuantityId<TNewtonMeterPerRadianUnit>;

{ Unit of MeterPerSecond }

type
  TMeterPerSecondUnit = record
    const Symbol : string = 'm/s';
    const Name   : string = 'meter per second';
  end;
  TMetersPerSecond = specialize TQuantity<TMeterPerSecondUnit>;
  TMeterPerSecondId = specialize TQuantityId<TMeterPerSecondUnit>;

{ Unit of MeterPerSquareSecond }

type
  TMeterPerSquareSecondUnit = record
    const Symbol : string = 'm/s2';
    const Name   : string = 'meter per square second';
  end;
  TMetersPerSquareSecond = specialize TQuantity<TMeterPerSquareSecondUnit>;
  TMeterPerSquareSecondId = specialize TQuantityId<TMeterPerSquareSecondUnit>;

{ Unit of RadianPerSecond }

type
  TRadianPerSecondUnit = record
    const Symbol : string = 'rad/s';
    const Name   : string = 'radian per second';
  end;
  TRadiansPerSecond = specialize TQuantity<TRadianPerSecondUnit>;
  TRadianPerSecondId = specialize TQuantityId<TRadianPerSecondUnit>;

{ Unit of RadianPerSquareSecond }

type
  TRadianPerSquareSecondUnit = record
    const Symbol : string = 'rad/s2';
    const Name   : string = 'radian per square second';
  end;
  TRadiansPerSquareSecond = specialize TQuantity<TRadianPerSquareSecondUnit>;
  TRadianPerSquareSecondId = specialize TQuantityId<TRadianPerSquareSecondUnit>;

{ Unit of KilogramPerMeter }

type
  TKilogramPerMeterUnit = record
    const Symbol : string = 'kg/m';
    const Name   : string = 'kilogram per meter';
  end;
  TKilogramsPerMeter = specialize TQuantity<TKilogramPerMeterUnit>;
  TKilogramPerMeterId = specialize TQuantityId<TKilogramPerMeterUnit>;

{ Unit of KilogramPerSquareMeter }

type
  TKilogramPerSquareMeterUnit = record
    const Symbol : string = 'kg/m2';
    const Name   : string = 'kilogram per square meter';
  end;
  TKilogramsPerSquareMeter = specialize TQuantity<TKilogramPerSquareMeterUnit>;
  TKilogramPerSquareMeterId = specialize TQuantityId<TKilogramPerSquareMeterUnit>;

{ Unit of KilogramPerCubicMeter }

type
  TKilogramPerCubicMeterUnit = record
    const Symbol : string = 'kg/m3';
    const Name   : string = 'kilogram per cubic meter';
  end;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;
  TKilogramPerCubicMeterId = specialize TQuantityId<TKilogramPerCubicMeterUnit>;

{ Unit of NewtonPerCubicMeter }

type
  TNewtonPerCubicMeterUnit = record
    const Symbol : string = 'N/m3';
    const Name   : string = 'newton per cubic meter';
  end;
  TNewtonsPerCubicMeter = specialize TQuantity<TNewtonPerCubicMeterUnit>;
  TNewtonPerCubicMeterId = specialize TQuantityId<TNewtonPerCubicMeterUnit>;

{ Unit of NewtonPerMeter }

type
  TNewtonPerMeterUnit = record
    const Symbol : string = 'N/m';
    const Name   : string = 'newton per meter';
  end;
  TNewtonsPerMeter = specialize TQuantity<TNewtonPerMeterUnit>;
  TNewtonPerMeterId = specialize TQuantityId<TNewtonPerMeterUnit>;

{ Unit of KilogramMeterPerSecond }

type
  TKilogramMeterPerSecondUnit = record
    const Symbol : string = 'kg·m/s';
    const Name   : string = 'kilogram meter per second';
  end;
  TKilogramMetersPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;
  TKilogramMeterPerSecondId = specialize TQuantityId<TKilogramMeterPerSecondUnit>;

{ Unit of NewtonSecond }

type
  TNewtonSecondUnit = record
    const Symbol : string = 'N·s';
    const Name   : string = 'newton second';
  end;
  TNewtonSeconds = specialize TQuantity<TNewtonSecondUnit>;
  TNewtonSecondId = specialize TQuantityId<TNewtonSecondUnit>;

{ Unit of KilogramSquareMeter }

type
  TKilogramSquareMeterUnit = record
    const Symbol : string = 'kg·m2';
    const Name   : string = 'kilogram square meter';
  end;
  TKilogramSquareMeters = specialize TQuantity<TKilogramSquareMeterUnit>;
  TKilogramSquareMeterId = specialize TQuantityId<TKilogramSquareMeterUnit>;

{ Unit of KilogramSquareMeterPerSecond }

type
  TKilogramSquareMeterPerSecondUnit = record
    const Symbol : string = '';
    const Name   : string = '';
  end;
  TKilogramSquareMetersPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;
  TKilogramSquareMeterPerSecondId = specialize TQuantityId<TKilogramSquareMeterPerSecondUnit>;

{ Unit of SquareMeterPerSquareSecond }

type
  TSquareMeterPerSquareSecondUnit = record
    const Symbol : string = 'm2/s2';
    const Name   : string = 'square meter per square second';
  end;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;
  TSquareMeterPerSquareSecondId = specialize TQuantityId<TSquareMeterPerSquareSecondUnit>;

{ Unit of SteradianPerSquareSecond }

type
  TSteradianPerSquareSecondUnit = record
    const Symbol : string = 'rad2/s2';
    const Name   : string = 'square rad per square second';
  end;
  TSteradiansPerSquareSecond = specialize TQuantity<TSteradianPerSquareSecondUnit>;
  TSteradianPerSquareSecondId = specialize TQuantityId<TSteradianPerSquareSecondUnit>;

{ Unit of CubicMeterPerSecond }

type
  TCubicMeterPerSecondUnit = record
    const Symbol : string = 'm3/s';
    const Name   : string = 'cubic meter per second';
  end;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;
  TCubicMeterPerSecondId = specialize TQuantityId<TCubicMeterPerSecondUnit>;

{ Unit of PascalSecond }

type
  TPascalSecondUnit = record
    const Symbol : string = 'Pa·s';
    const Name   : string = 'pascal second';
  end;
  TPascalSeconds = specialize TQuantity<TPascalSecondUnit>;
  TPascalSecondId = specialize TQuantityId<TPascalSecondUnit>;

{ Unit of SquareMeterPerSecond }

type
  TSquareMeterPerSecondUnit = record
    const Symbol : string = 'm2/s';
    const Name   : string = 'square meter per second';
  end;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;
  TSquareMeterPerSecondId = specialize TQuantityId<TSquareMeterPerSecondUnit>;

{ Unit of NewtonPerSquareKilogram }

type
  TNewtonPerSquareKilogramUnit = record
    const Symbol : string = 'N/kg2';
    const Name   : string = 'newton per square kilogram';
  end;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;
  TNewtonPerSquareKilogramId = specialize TQuantityId<TNewtonPerSquareKilogramUnit>;

{ Unit of SquareKilogramPerMeter }

type
  TSquareKilogramPerMeterUnit = record
    const Symbol : string = 'kg2/m';
    const Name   : string = 'square kilogram per meter';
  end;
  TSquareKilogramsPerMeter = specialize TQuantity<TSquareKilogramPerMeterUnit>;
  TSquareKilogramPerMeterId = specialize TQuantityId<TSquareKilogramPerMeterUnit>;

{ Unit of SquareKilogramPerSquareMeter }

type
  TSquareKilogramPerSquareMeterUnit = record
    const Symbol : string = 'kg2/m2';
    const Name   : string = 'square kilogram per square meter';
  end;
  TSquareKilogramsPerSquareMeter = specialize TQuantity<TSquareKilogramPerSquareMeterUnit>;
  TSquareKilogramPerSquareMeterId = specialize TQuantityId<TSquareKilogramPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareKilogram }

type
  TSquareMeterPerSquareKilogramUnit = record
    const Symbol : string = 'm2/kg2';
    const Name   : string = 'square meter per square kilogram';
  end;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;
  TSquareMeterPerSquareKilogramId = specialize TQuantityId<TSquareMeterPerSquareKilogramUnit>;

{ Unit of NewtonSquareMeterPerSquareKilogram }

type
  TNewtonSquareMeterPerSquareKilogramUnit = record
    const Symbol : string = 'N·m2/kg2';
    const Name   : string = 'newton square meter per square kilogram';
  end;
  TNewtonSquareMetersPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonSquareMeterPerSquareKilogramId = specialize TQuantityId<TNewtonSquareMeterPerSquareKilogramUnit>;

{ Unit of ReciprocalKelvin }

type
  TReciprocalKelvinUnit = record
    const Symbol : string = '1/K';
    const Name   : string = 'reciprocal kelvin';
  end;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;
  TReciprocalKelvinId = specialize TQuantityId<TReciprocalKelvinUnit>;

{ Unit of KilogramKelvin }

type
  TKilogramKelvinUnit = record
    const Symbol : string = 'kg·K';
    const Name   : string = 'kilogram kelvin';
  end;
  TKilogramKelvins = specialize TQuantity<TKilogramKelvinUnit>;
  TKilogramKelvinId = specialize TQuantityId<TKilogramKelvinUnit>;

{ Unit of JoulePerKelvin }

type
  TJoulePerKelvinUnit = record
    const Symbol : string = 'J/K';
    const Name   : string = 'joule per kelvin';
  end;
  TJoulesPerKelvin = specialize TQuantity<TJoulePerKelvinUnit>;
  TJoulePerKelvinId = specialize TQuantityId<TJoulePerKelvinUnit>;

{ Unit of JoulePerKilogram }

type
  TJoulePerKilogramUnit = record
    const Symbol : string = 'J/kg';
    const Name   : string = 'joule per kilogram';
  end;
  TJoulesPerKilogram = specialize TQuantity<TJoulePerKilogramUnit>;
  TJoulePerKilogramId = specialize TQuantityId<TJoulePerKilogramUnit>;

{ Unit of JoulePerKilogramPerKelvin }

type
  TJoulePerKilogramPerKelvinUnit = record
    const Symbol : string = 'J/kg/K';
    const Name   : string = 'joule per kilogram per kelvin';
  end;
  TJoulesPerKilogramPerKelvin = specialize TQuantity<TJoulePerKilogramPerKelvinUnit>;
  TJoulePerKilogramPerKelvinId = specialize TQuantityId<TJoulePerKilogramPerKelvinUnit>;

{ Unit of MeterKelvin }

type
  TMeterKelvinUnit = record
    const Symbol : string = 'm·K';
    const Name   : string = 'meter kelvin';
  end;
  TMeterKelvins = specialize TQuantity<TMeterKelvinUnit>;
  TMeterKelvinId = specialize TQuantityId<TMeterKelvinUnit>;

{ Unit of KelvinPerMeter }

type
  TKelvinPerMeterUnit = record
    const Symbol : string = 'K/m';
    const Name   : string = 'kelvin per meter';
  end;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;
  TKelvinPerMeterId = specialize TQuantityId<TKelvinPerMeterUnit>;

{ Unit of WattPerMeter }

type
  TWattPerMeterUnit = record
    const Symbol : string = 'W/m';
    const Name   : string = 'watt per meter';
  end;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;
  TWattPerMeterId = specialize TQuantityId<TWattPerMeterUnit>;

{ Unit of WattPerSquareMeter }

type
  TWattPerSquareMeterUnit = record
    const Symbol : string = 'W/m2';
    const Name   : string = 'watt per square meter';
  end;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;
  TWattPerSquareMeterId = specialize TQuantityId<TWattPerSquareMeterUnit>;

{ Unit of WattPerKelvin }

type
  TWattPerKelvinUnit = record
    const Symbol : string = 'W/K';
    const Name   : string = 'watt per kelvin';
  end;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;
  TWattPerKelvinId = specialize TQuantityId<TWattPerKelvinUnit>;

{ Unit of WattPerMeterPerKelvin }

type
  TWattPerMeterPerKelvinUnit = record
    const Symbol : string = 'W/m/K';
    const Name   : string = 'watt per meter per kelvin';
  end;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;
  TWattPerMeterPerKelvinId = specialize TQuantityId<TWattPerMeterPerKelvinUnit>;

{ Unit of SquareMeterKelvin }

type
  TSquareMeterKelvinUnit = record
    const Symbol : string = 'm2·K';
    const Name   : string = 'square meter kelvin';
  end;
  TSquareMeterKelvins = specialize TQuantity<TSquareMeterKelvinUnit>;
  TSquareMeterKelvinId = specialize TQuantityId<TSquareMeterKelvinUnit>;

{ Unit of WattPerSquareMeterPerKelvin }

type
  TWattPerSquareMeterPerKelvinUnit = record
    const Symbol : string = 'W/m2/K';
    const Name   : string = 'watt per square meter per kelvin';
  end;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;
  TWattPerSquareMeterPerKelvinId = specialize TQuantityId<TWattPerSquareMeterPerKelvinUnit>;

{ Unit of SquareMeterQuarticKelvin }

type
  TSquareMeterQuarticKelvinUnit = record
    const Symbol : string = 'm2·K4';
    const Name   : string = 'square meter quartic kelvin';
  end;
  TSquareMeterQuarticKelvins = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;
  TSquareMeterQuarticKelvinId = specialize TQuantityId<TSquareMeterQuarticKelvinUnit>;

{ Unit of WattPerQuarticKelvin }

type
  TWattPerQuarticKelvinUnit = record
    const Symbol : string = 'W/K4';
    const Name   : string = 'watt per quartic kelvin';
  end;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;
  TWattPerQuarticKelvinId = specialize TQuantityId<TWattPerQuarticKelvinUnit>;

{ Unit of WattPerSquareMeterPerQuarticKelvin }

type
  TWattPerSquareMeterPerQuarticKelvinUnit = record
    const Symbol : string = 'W/m2/K4';
    const Name   : string = 'watt per square meter per quartic kelvin';
  end;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattPerSquareMeterPerQuarticKelvinId = specialize TQuantityId<TWattPerSquareMeterPerQuarticKelvinUnit>;

{ Unit of JoulePerMole }

type
  TJoulePerMoleUnit = record
    const Symbol : string = 'J/mol';
    const Name   : string = 'joule per mole';
  end;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;
  TJoulePerMoleId = specialize TQuantityId<TJoulePerMoleUnit>;

{ Unit of MoleKelvin }

type
  TMoleKelvinUnit = record
    const Symbol : string = 'mol·K';
    const Name   : string = 'mole kelvin';
  end;
  TMoleKelvins = specialize TQuantity<TMoleKelvinUnit>;
  TMoleKelvinId = specialize TQuantityId<TMoleKelvinUnit>;

{ Unit of JoulePerMolePerKelvin }

type
  TJoulePerMolePerKelvinUnit = record
    const Symbol : string = 'J/mol/K';
    const Name   : string = 'joule per mole per kelvin';
  end;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;
  TJoulePerMolePerKelvinId = specialize TQuantityId<TJoulePerMolePerKelvinUnit>;

{ Unit of OhmMeter }

type
  TOhmMeterUnit = record
    const Symbol : string = 'Ω·m';
    const Name   : string = 'ohm meter';
  end;
  TOhmMeters = specialize TQuantity<TOhmMeterUnit>;
  TOhmMeterId = specialize TQuantityId<TOhmMeterUnit>;

{ Unit of VoltPerMeter }

type
  TVoltPerMeterUnit = record
    const Symbol : string = 'V/m';
    const Name   : string = 'volt per meter';
  end;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;
  TVoltPerMeterId = specialize TQuantityId<TVoltPerMeterUnit>;

{ Unit of CoulombPerMeter }

type
  TCoulombPerMeterUnit = record
    const Symbol : string = 'C/m';
    const Name   : string = 'coulomb per meter';
  end;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;
  TCoulombPerMeterId = specialize TQuantityId<TCoulombPerMeterUnit>;

{ Unit of SquareCoulombPerMeter }

type
  TSquareCoulombPerMeterUnit = record
    const Symbol : string = 'C2/m';
    const Name   : string = 'square coulomb per meter';
  end;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;
  TSquareCoulombPerMeterId = specialize TQuantityId<TSquareCoulombPerMeterUnit>;

{ Unit of CoulombPerSquareMeter }

type
  TCoulombPerSquareMeterUnit = record
    const Symbol : string = 'C/m2';
    const Name   : string = 'coulomb per square meter';
  end;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;
  TCoulombPerSquareMeterId = specialize TQuantityId<TCoulombPerSquareMeterUnit>;

{ Unit of SquareMeterPerSquareCoulomb }

type
  TSquareMeterPerSquareCoulombUnit = record
    const Symbol : string = 'm2/C2';
    const Name   : string = 'square meter per square coulomb';
  end;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;
  TSquareMeterPerSquareCoulombId = specialize TQuantityId<TSquareMeterPerSquareCoulombUnit>;

{ Unit of NewtonPerSquareCoulomb }

type
  TNewtonPerSquareCoulombUnit = record
    const Symbol : string = 'N/C2';
    const Name   : string = 'newton per square coulomb';
  end;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;
  TNewtonPerSquareCoulombId = specialize TQuantityId<TNewtonPerSquareCoulombUnit>;

{ Unit of NewtonSquareMeter }

type
  TNewtonSquareMeterUnit = record
    const Symbol : string = 'N·m2';
    const Name   : string = 'newton square meter';
  end;
  TNewtonSquareMeters = specialize TQuantity<TNewtonSquareMeterUnit>;
  TNewtonSquareMeterId = specialize TQuantityId<TNewtonSquareMeterUnit>;

{ Unit of NewtonSquareMeterPerSquareCoulomb }

type
  TNewtonSquareMeterPerSquareCoulombUnit = record
    const Symbol : string = 'N·m2/C2';
    const Name   : string = 'newton square meter per square coulomb';
  end;
  TNewtonSquareMetersPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonSquareMeterPerSquareCoulombId = specialize TQuantityId<TNewtonSquareMeterPerSquareCoulombUnit>;

{ Unit of VoltMeter }

type
  TVoltMeterUnit = record
    const Symbol : string = 'V·m';
    const Name   : string = 'volt meter';
  end;
  TVoltMeters = specialize TQuantity<TVoltMeterUnit>;
  TVoltMeterId = specialize TQuantityId<TVoltMeterUnit>;

var
  Vm: TVoltMeterId;

{ Unit of VoltMeterPerSecond }

type
  TVoltMeterPerSecondUnit = record
    const Symbol : string = 'V·m/s';
    const Name   : string = 'volt meter per second';
  end;
  TVoltMetersPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;
  TVoltMeterPerSecondId = specialize TQuantityId<TVoltMeterPerSecondUnit>;

{ Unit of FaradPerMeter }

type
  TFaradPerMeterUnit = record
    const Symbol : string = 'F/m';
    const Name   : string = 'farad per meter';
  end;
  TFaradsPerMeter = specialize TQuantity<TFaradPerMeterUnit>;
  TFaradPerMeterId = specialize TQuantityId<TFaradPerMeterUnit>;

{ Unit of AmperePerMeter }

type
  TAmperePerMeterUnit = record
    const Symbol : string = 'A/m';
    const Name   : string = 'ampere per meter';
  end;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;
  TAmperePerMeterId = specialize TQuantityId<TAmperePerMeterUnit>;

{ Unit of MeterPerAmpere }

type
  TMeterPerAmpereUnit = record
    const Symbol : string = 'm/A';
    const Name   : string = 'meter per ampere';
  end;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;
  TMeterPerAmpereId = specialize TQuantityId<TMeterPerAmpereUnit>;

{ Unit of TeslaMeter }

type
  TTeslaMeterUnit = record
    const Symbol : string = 'T·m';
    const Name   : string = 'tesla meter';
  end;
  TTeslaMeters = specialize TQuantity<TTeslaMeterUnit>;
  TTeslaMeterId = specialize TQuantityId<TTeslaMeterUnit>;

var
  Tm: TTeslaMeterId;

{ Unit of TeslaPerAmpere }

type
  TTeslaPerAmpereUnit = record
    const Symbol : string = 'T/A';
    const Name   : string = 'tesla per ampere';
  end;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;
  TTeslaPerAmpereId = specialize TQuantityId<TTeslaPerAmpereUnit>;

{ Unit of HenryPerMeter }

type
  THenryPerMeterUnit = record
    const Symbol : string = 'H/m';
    const Name   : string = 'henry per meter';
  end;
  THenrysPerMeter = specialize TQuantity<THenryPerMeterUnit>;
  THenryPerMeterId = specialize TQuantityId<THenryPerMeterUnit>;

{ Unit of RadianPerMeter }

type
  TRadianPerMeterUnit = record
    const Symbol : string = 'rad/m';
    const Name   : string = 'radian per meter';
  end;
  TRadiansPerMeter = specialize TQuantity<TRadianPerMeterUnit>;
  TRadianPerMeterId = specialize TQuantityId<TRadianPerMeterUnit>;

{ Unit of Day }

type
  TDayUnit = record
    const Symbol : string = 'day';
    const Name   : string = 'day';
    const Factor : double = 86400;
  end;
  TDayId = specialize TFactoredQuantityId<TSecondUnit, TDayUnit>;

var
  day: TDayId;

{ Unit of Hour }

type
  THourUnit = record
    const Symbol : string = 'h';
    const Name   : string = 'hour';
    const Factor : double = 3600;
  end;
  THourId = specialize TFactoredQuantityId<TSecondUnit, THourUnit>;

var
  hour: THourId;

{ Unit of Minute }

type
  TMinuteUnit = record
    const Symbol : string = 'min';
    const Name   : string = 'minute';
    const Factor : double = 60;
  end;
  TMinuteId = specialize TFactoredQuantityId<TSecondUnit, TMinuteUnit>;

var
  minute: TMinuteId;

{ Unit of Decisecond }

type
  TDecisecondUnit = record
    const Symbol : string = 'ds';
    const Name   : string = 'decisecond';
    const Factor : double = 1E-01;
  end;
  TDecisecondId = specialize TFactoredQuantityId<TSecondUnit, TDecisecondUnit>;

var
  ds: TDecisecondId;

{ Unit of Centisecond }

type
  TCentisecondUnit = record
    const Symbol : string = 'cs';
    const Name   : string = 'centisecond';
    const Factor : double = 1E-02;
  end;
  TCentisecondId = specialize TFactoredQuantityId<TSecondUnit, TCentisecondUnit>;

var
  cs: TCentisecondId;

{ Unit of Millisecond }

type
  TMillisecondUnit = record
    const Symbol : string = 'ms';
    const Name   : string = 'millisecond';
    const Factor : double = 1E-03;
  end;
  TMillisecondId = specialize TFactoredQuantityId<TSecondUnit, TMillisecondUnit>;

var
  ms: TMillisecondId;

{ Unit of Microsecond }

type
  TMicrosecondUnit = record
    const Symbol : string = 'us';
    const Name   : string = 'microsecond';
    const Factor : double = 1E-06;
  end;
  TMicrosecondId = specialize TFactoredQuantityId<TSecondUnit, TMicrosecondUnit>;

var
  us: TMicrosecondId;

{ Unit of Nanosecond }

type
  TNanosecondUnit = record
    const Symbol : string = 'ns';
    const Name   : string = 'nanosecond';
    const Factor : double = 1E-09;
  end;
  TNanosecondId = specialize TFactoredQuantityId<TSecondUnit, TNanosecondUnit>;

var
  ns: TNanosecondId;

{ Unit of Picosecond }

type
  TPicosecondUnit = record
    const Symbol : string = 'ps';
    const Name   : string = 'picosecond';
    const Factor : double = 1E-12;
  end;
  TPicosecondId = specialize TFactoredQuantityId<TSecondUnit, TPicosecondUnit>;

var
  ps: TPicosecondId;

{ Unit of Kilometer }

type
  TKilometerUnit = record
    const Symbol : string = 'km';
    const Name   : string = 'kilometer';
    const Factor : double = 1E+03;
  end;
  TKilometerId = specialize TFactoredQuantityId<TMeterUnit, TKilometerUnit>;

var
  km: TKilometerId;

{ Unit of Hectometer }

type
  THectometerUnit = record
    const Symbol : string = 'hm';
    const Name   : string = 'hectometer';
    const Factor : double = 1E+02;
  end;
  THectometerId = specialize TFactoredQuantityId<TMeterUnit, THectometerUnit>;

var
  hm: THectometerId;

{ Unit of Decameter }

type
  TDecameterUnit = record
    const Symbol : string = 'dam';
    const Name   : string = 'decameter';
    const Factor : double = 1E+01;
  end;
  TDecameterId = specialize TFactoredQuantityId<TMeterUnit, TDecameterUnit>;

var
  dam: TDecameterId;

{ Unit of Decimeter }

type
  TDecimeterUnit = record
    const Symbol : string = 'dm';
    const Name   : string = 'decimeter';
    const Factor : double = 1E-01;
  end;
  TDecimeterId = specialize TFactoredQuantityId<TMeterUnit, TDecimeterUnit>;

var
  dm: TDecimeterId;

{ Unit of Centimeter }

type
  TCentimeterUnit = record
    const Symbol : string = 'cm';
    const Name   : string = 'centimeter';
    const Factor : double = 1E-02;
  end;
  TCentimeterId = specialize TFactoredQuantityId<TMeterUnit, TCentimeterUnit>;

var
  cm: TCentimeterId;

{ Unit of Millimeter }

type
  TMillimeterUnit = record
    const Symbol : string = 'mm';
    const Name   : string = 'millimeter';
    const Factor : double = 1E-03;
  end;
  TMillimeterId = specialize TFactoredQuantityId<TMeterUnit, TMillimeterUnit>;

var
  mm: TMillimeterId;

{ Unit of Micrometer }

type
  TMicrometerUnit = record
    const Symbol : string = 'um';
    const Name   : string = 'micrometer';
    const Factor : double = 1E-06;
  end;
  TMicrometerId = specialize TFactoredQuantityId<TMeterUnit, TMicrometerUnit>;

var
  um: TMicrometerId;

{ Unit of Nanometer }

type
  TNanometerUnit = record
    const Symbol : string = 'nm';
    const Name   : string = 'nanometer';
    const Factor : double = 1E-09;
  end;
  TNanometerId = specialize TFactoredQuantityId<TMeterUnit, TNanometerUnit>;

var
  nm: TNanometerId;

{ Unit of Picometer }

type
  TPicometerUnit = record
    const Symbol : string = 'pm';
    const Name   : string = 'picometer';
    const Factor : double = 1E-12;
  end;
  TPicometerId = specialize TFactoredQuantityId<TMeterUnit, TPicometerUnit>;

var
  pm: TPicometerId;

{ Unit of SquareKilometer }

type
  TSquareKilometerUnit = record
    const Symbol : string = 'km2';
    const Name   : string = 'square kilometer';
    const Factor : double = 1E+06;
  end;
  TSquareKilometerId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareKilometerUnit>;

var
  km2: TSquareKilometerId;

{ Unit of SquareHectometer }

type
  TSquareHectometerUnit = record
    const Symbol : string = 'hm2';
    const Name   : string = 'square hectometer';
    const Factor : double = 1E+04;
  end;
  TSquareHectometerId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareHectometerUnit>;

var
  hm2: TSquareHectometerId;

{ Unit of SquareDecameter }

type
  TSquareDecameterUnit = record
    const Symbol : string = 'dam2';
    const Name   : string = 'square decameter';
    const Factor : double = 1E+02;
  end;
  TSquareDecameterId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareDecameterUnit>;

var
  dam2: TSquareDecameterId;

{ Unit of SquareDecimeter }

type
  TSquareDecimeterUnit = record
    const Symbol : string = 'dm2';
    const Name   : string = 'square decimeter';
    const Factor : double = 1E-02;
  end;
  TSquareDecimeterId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareDecimeterUnit>;

var
  dm2: TSquareDecimeterId;

{ Unit of SquareCentimeter }

type
  TSquareCentimeterUnit = record
    const Symbol : string = 'cm2';
    const Name   : string = 'square centimeter';
    const Factor : double = 1E-04;
  end;
  TSquareCentimeterId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareCentimeterUnit>;

var
  cm2: TSquareCentimeterId;

{ Unit of SquareMillimeter }

type
  TSquareMillimeterUnit = record
    const Symbol : string = 'mm2';
    const Name   : string = 'square millimeter';
    const Factor : double = 1E-06;
  end;
  TSquareMillimeterId = specialize TFactoredQuantityId<TSquareMeterUnit, TSquareMillimeterUnit>;

var
  mm2: TSquareMillimeterId;

{ Unit of CubicKilometer }

type
  TCubicKilometerUnit = record
    const Symbol : string = 'km3';
    const Name   : string = 'cubic kilometer';
    const Factor : double = 1E+09;
  end;
  TCubicKilometerId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicKilometerUnit>;

var
  km3: TCubicKilometerId;

{ Unit of CubicHectometer }

type
  TCubicHectometerUnit = record
    const Symbol : string = 'hm3';
    const Name   : string = 'cubic hectometer';
    const Factor : double = 1E+06;
  end;
  TCubicHectometerId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicHectometerUnit>;

var
  hm3: TCubicHectometerId;

{ Unit of CubicDecameter }

type
  TCubicDecameterUnit = record
    const Symbol : string = 'dam3';
    const Name   : string = 'cubic decameter';
    const Factor : double = 1E+03;
  end;
  TCubicDecameterId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicDecameterUnit>;

var
  dam3: TCubicDecameterId;

{ Unit of CubicDecimeter }

type
  TCubicDecimeterUnit = record
    const Symbol : string = 'dm3';
    const Name   : string = 'cubic decimeter';
    const Factor : double = 1E-03;
  end;
  TCubicDecimeterId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicDecimeterUnit>;

var
  dm3: TCubicDecimeterId;

{ Unit of CubicCentimeter }

type
  TCubicCentimeterUnit = record
    const Symbol : string = 'cm3';
    const Name   : string = 'cubic centimeter';
    const Factor : double = 1E-06;
  end;
  TCubicCentimeterId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicCentimeterUnit>;

var
  cm3: TCubicCentimeterId;

{ Unit of CubicMillimeter }

type
  TCubicMillimeterUnit = record
    const Symbol : string = 'mm3';
    const Name   : string = 'cubic millimeter';
    const Factor : double = 1E-09;
  end;
  TCubicMillimeterId = specialize TFactoredQuantityId<TCubicMeterUnit, TCubicMillimeterUnit>;

var
  mm3: TCubicMillimeterId;

{ Unit of QuarticKilometer }

type
  TQuarticKilometerUnit = record
    const Symbol : string = 'km4';
    const Name   : string = 'quartic kilometer';
    const Factor : double = 1E+12;
  end;
  TQuarticKilometerId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticKilometerUnit>;

var
  km4: TQuarticKilometerId;

{ Unit of QuarticHectometer }

type
  TQuarticHectometerUnit = record
    const Symbol : string = 'hm4';
    const Name   : string = 'quartic hectometer';
    const Factor : double = 1E+08;
  end;
  TQuarticHectometerId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticHectometerUnit>;

var
  hm4: TQuarticHectometerId;

{ Unit of QuarticDecameter }

type
  TQuarticDecameterUnit = record
    const Symbol : string = 'dam4';
    const Name   : string = 'quartic decameter';
    const Factor : double = 1E+04;
  end;
  TQuarticDecameterId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticDecameterUnit>;

var
  dam4: TQuarticDecameterId;

{ Unit of QuarticDecimeter }

type
  TQuarticDecimeterUnit = record
    const Symbol : string = 'dm4';
    const Name   : string = 'quartic decimeter';
    const Factor : double = 1E-04;
  end;
  TQuarticDecimeterId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticDecimeterUnit>;

var
  dm4: TQuarticDecimeterId;

{ Unit of QuarticCentimeter }

type
  TQuarticCentimeterUnit = record
    const Symbol : string = 'cm4';
    const Name   : string = 'quartic centimeter';
    const Factor : double = 1E-08;
  end;
  TQuarticCentimeterId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticCentimeterUnit>;

var
  cm4: TQuarticCentimeterId;

{ Unit of QuarticMillimeter }

type
  TQuarticMillimeterUnit = record
    const Symbol : string = 'mm4';
    const Name   : string = 'quartic millimeter';
    const Factor : double = 1E-12;
  end;
  TQuarticMillimeterId = specialize TFactoredQuantityId<TQuarticMeterUnit, TQuarticMillimeterUnit>;

var
  mm4: TQuarticMillimeterId;

{ Unit of Hectogram }

type
  THectogramUnit = record
    const Symbol : string = 'hg';
    const Name   : string = 'hectogram';
    const Factor : double = 1E-01;
  end;
  THectogramId = specialize TFactoredQuantityId<TKilogramUnit, THectogramUnit>;

var
  hg: THectogramId;

{ Unit of Decagram }

type
  TDecagramUnit = record
    const Symbol : string = 'dag';
    const Name   : string = 'decagram';
    const Factor : double = 1E-02;
  end;
  TDecagramId = specialize TFactoredQuantityId<TKilogramUnit, TDecagramUnit>;

var
  dag: TDecagramId;

{ Unit of Gram }

type
  TGramUnit = record
    const Symbol : string = 'g';
    const Name   : string = 'gram';
    const Factor : double = 1E-03;
  end;
  TGramId = specialize TFactoredQuantityId<TKilogramUnit, TGramUnit>;

var
  g: TGramId;

{ Unit of Decigram }

type
  TDecigramUnit = record
    const Symbol : string = 'dg';
    const Name   : string = 'decigram';
    const Factor : double = 1E-04;
  end;
  TDecigramId = specialize TFactoredQuantityId<TKilogramUnit, TDecigramUnit>;

var
  dg: TDecigramId;

{ Unit of Centigram }

type
  TCentigramUnit = record
    const Symbol : string = 'cg';
    const Name   : string = 'centigram';
    const Factor : double = 1E-05;
  end;
  TCentigramId = specialize TFactoredQuantityId<TKilogramUnit, TCentigramUnit>;

var
  cg: TCentigramId;

{ Unit of Milligram }

type
  TMilligramUnit = record
    const Symbol : string = 'mg';
    const Name   : string = 'milligram';
    const Factor : double = 1E-06;
  end;
  TMilligramId = specialize TFactoredQuantityId<TKilogramUnit, TMilligramUnit>;

var
  mg: TMilligramId;

{ Unit of Microgram }

type
  TMicrogramUnit = record
    const Symbol : string = 'ug';
    const Name   : string = 'microgram';
    const Factor : double = 1E-09;
  end;
  TMicrogramId = specialize TFactoredQuantityId<TKilogramUnit, TMicrogramUnit>;

var
  ug: TMicrogramId;

{ Unit of Nanogram }

type
  TNanogramUnit = record
    const Symbol : string = 'ng';
    const Name   : string = 'nanogram';
    const Factor : double = 1E-12;
  end;
  TNanogramId = specialize TFactoredQuantityId<TKilogramUnit, TNanogramUnit>;

var
  ng: TNanogramId;

{ Unit of Picogram }

type
  TPicogramUnit = record
    const Symbol : string = 'pg';
    const Name   : string = 'picogram';
    const Factor : double = 1E-15;
  end;
  TPicogramId = specialize TFactoredQuantityId<TKilogramUnit, TPicogramUnit>;

var
  pg: TPicogramId;

{ Unit of Kiloampere }

type
  TKiloampereUnit = record
    const Symbol : string = 'kA';
    const Name   : string = 'kiloampere';
    const Factor : double = 1E+03;
  end;
  TKiloampereId = specialize TFactoredQuantityId<TAmpereUnit, TKiloampereUnit>;

var
  kA: TKiloampereId;

{ Unit of Hectoampere }

type
  THectoampereUnit = record
    const Symbol : string = 'hA';
    const Name   : string = 'hectoampere';
    const Factor : double = 1E+02;
  end;
  THectoampereId = specialize TFactoredQuantityId<TAmpereUnit, THectoampereUnit>;

var
  hA: THectoampereId;

{ Unit of Decampere }

type
  TDecampereUnit = record
    const Symbol : string = 'daA';
    const Name   : string = 'decampere';
    const Factor : double = 1E+01;
  end;
  TDecampereId = specialize TFactoredQuantityId<TAmpereUnit, TDecampereUnit>;

var
  daA: TDecampereId;

{ Unit of Deciampere }

type
  TDeciampereUnit = record
    const Symbol : string = 'dA';
    const Name   : string = 'deciampere';
    const Factor : double = 1E-01;
  end;
  TDeciampereId = specialize TFactoredQuantityId<TAmpereUnit, TDeciampereUnit>;

var
  dA: TDeciampereId;

{ Unit of Centiampere }

type
  TCentiampereUnit = record
    const Symbol : string = 'cA';
    const Name   : string = 'centiampere';
    const Factor : double = 1E-02;
  end;
  TCentiampereId = specialize TFactoredQuantityId<TAmpereUnit, TCentiampereUnit>;

var
  cA: TCentiampereId;

{ Unit of Milliampere }

type
  TMilliampereUnit = record
    const Symbol : string = 'mA';
    const Name   : string = 'milliampere';
    const Factor : double = 1E-03;
  end;
  TMilliampereId = specialize TFactoredQuantityId<TAmpereUnit, TMilliampereUnit>;

var
  mA: TMilliampereId;

{ Unit of Microampere }

type
  TMicroampereUnit = record
    const Symbol : string = 'uA';
    const Name   : string = 'microampere';
    const Factor : double = 1E-06;
  end;
  TMicroampereId = specialize TFactoredQuantityId<TAmpereUnit, TMicroampereUnit>;

var
  uA: TMicroampereId;

{ Unit of Nanoampere }

type
  TNanoampereUnit = record
    const Symbol : string = 'nA';
    const Name   : string = 'nanoampere';
    const Factor : double = 1E-09;
  end;
  TNanoampereId = specialize TFactoredQuantityId<TAmpereUnit, TNanoampereUnit>;

var
  nA: TNanoampereId;

{ Unit of Picoampere }

type
  TPicoampereUnit = record
    const Symbol : string = 'pA';
    const Name   : string = 'picoampere';
    const Factor : double = 1E-12;
  end;
  TPicoampereId = specialize TFactoredQuantityId<TAmpereUnit, TPicoampereUnit>;

var
  picoampere: TPicoampereId;

{ Unit of SquareMilliampere }

type
  TSquareMilliampereUnit = record
    const Symbol : string = 'mA2';
    const Name   : string = 'square milliampere';
    const Factor : double = 1E-06;
  end;
  TSquareMilliampereId = specialize TFactoredQuantityId<TSquareAmpereUnit, TSquareMilliampereUnit>;

var
  mA2: TSquareMilliampereId;

{ Unit of Degree }

type
  TDegreeUnit = record
    const Symbol : string = 'deg';
    const Name   : string = 'degree';
    const Factor : double = Pi/180;
  end;
  TDegreeId = specialize TFactoredQuantityId<TRadianUnit, TDegreeUnit>;

var
  deg: TDegreeId;

{ Unit of Gigahertz }

type
  TGigahertzUnit = record
    const Symbol : string = 'GHz';
    const Name   : string = 'gigahertz';
    const Factor : double = 1E+09;
  end;
  TGigahertzId = specialize TFactoredQuantityId<THertzUnit, TGigahertzUnit>;

var
  GHz: TGigahertzId;

{ Unit of Megahertz }

type
  TMegahertzUnit = record
    const Symbol : string = 'MHz';
    const Name   : string = 'megahertz';
    const Factor : double = 1E+06;
  end;
  TMegahertzId = specialize TFactoredQuantityId<THertzUnit, TMegahertzUnit>;

var
  MHz: TMegahertzId;

{ Unit of Kilohertz }

type
  TKilohertzUnit = record
    const Symbol : string = 'kHz';
    const Name   : string = 'kilohertz';
    const Factor : double = 1E+03;
  end;
  TKilohertzId = specialize TFactoredQuantityId<THertzUnit, TKilohertzUnit>;

var
  kHz: TKilohertzId;

{ Unit of Giganewton }

type
  TGiganewtonUnit = record
    const Symbol : string = 'GN';
    const Name   : string = 'giganewton';
    const Factor : double = 1E+09;
  end;
  TGiganewtonId = specialize TFactoredQuantityId<TNewtonUnit, TGiganewtonUnit>;

var
  GN: TGiganewtonId;

{ Unit of Meganewton }

type
  TMeganewtonUnit = record
    const Symbol : string = 'MN';
    const Name   : string = 'meganewton';
    const Factor : double = 1E+06;
  end;
  TMeganewtonId = specialize TFactoredQuantityId<TNewtonUnit, TMeganewtonUnit>;

var
  MN: TMeganewtonId;

{ Unit of Kilonewton }

type
  TKilonewtonUnit = record
    const Symbol : string = 'kN';
    const Name   : string = 'kilonewton';
    const Factor : double = 1E+03;
  end;
  TKilonewtonId = specialize TFactoredQuantityId<TNewtonUnit, TKilonewtonUnit>;

var
  kN: TKilonewtonId;

{ Unit of Gigapascal }

type
  TGigapascalUnit = record
    const Symbol : string = 'GPa';
    const Name   : string = 'gigapascal';
    const Factor : double = 1E+09;
  end;
  TGigapascalId = specialize TFactoredQuantityId<TPascalUnit, TGigapascalUnit>;

var
  GPa: TGigapascalId;

{ Unit of Megapascal }

type
  TMegapascalUnit = record
    const Symbol : string = 'MPa';
    const Name   : string = 'megapascal';
    const Factor : double = 1E+06;
  end;
  TMegapascalId = specialize TFactoredQuantityId<TPascalUnit, TMegapascalUnit>;

var
  MPa: TMegapascalId;

{ Unit of Kilopascal }

type
  TKilopascalUnit = record
    const Symbol : string = 'kPa';
    const Name   : string = 'kilopascal';
    const Factor : double = 1E+03;
  end;
  TKilopascalId = specialize TFactoredQuantityId<TPascalUnit, TKilopascalUnit>;

var
  kPa: TKilopascalId;

{ Unit of Gigajoule }

type
  TGigajouleUnit = record
    const Symbol : string = 'GJ';
    const Name   : string = '';
    const Factor : double = 1E+09;
  end;
  TGigajouleId = specialize TFactoredQuantityId<TJouleUnit, TGigajouleUnit>;

var
  GJ: TGigajouleId;

{ Unit of Megajoule }

type
  TMegajouleUnit = record
    const Symbol : string = 'MJ';
    const Name   : string = '';
    const Factor : double = 1E+06;
  end;
  TMegajouleId = specialize TFactoredQuantityId<TJouleUnit, TMegajouleUnit>;

var
  MJ: TMegajouleId;

{ Unit of Kilojoule }

type
  TKilojouleUnit = record
    const Symbol : string = 'kJ';
    const Name   : string = '';
    const Factor : double = 1E+03;
  end;
  TKilojouleId = specialize TFactoredQuantityId<TJouleUnit, TKilojouleUnit>;

var
  kJ: TKilojouleId;

{ Unit of Gigawatt }

type
  TGigawattUnit = record
    const Symbol : string = 'GW';
    const Name   : string = '';
    const Factor : double = 1E+09;
  end;
  TGigawattId = specialize TFactoredQuantityId<TWattUnit, TGigawattUnit>;

var
  GW: TGigawattId;

{ Unit of Megawatt }

type
  TMegawattUnit = record
    const Symbol : string = 'MW';
    const Name   : string = '';
    const Factor : double = 1E+06;
  end;
  TMegawattId = specialize TFactoredQuantityId<TWattUnit, TMegawattUnit>;

var
  megawatt: TMegawattId;

{ Unit of Kilowatt }

type
  TKilowattUnit = record
    const Symbol : string = 'kW';
    const Name   : string = '';
    const Factor : double = 1E+03;
  end;
  TKilowattId = specialize TFactoredQuantityId<TWattUnit, TKilowattUnit>;

var
  kW: TKilowattId;

{ Unit of Milliwatt }

type
  TMilliwattUnit = record
    const Symbol : string = 'mW';
    const Name   : string = '';
    const Factor : double = 1E-03;
  end;
  TMilliwattId = specialize TFactoredQuantityId<TWattUnit, TMilliwattUnit>;

var
  mW: TMilliwattId;

{ Unit of Gigacoulomb }

type
  TGigacoulombUnit = record
    const Symbol : string = 'GC';
    const Name   : string = 'gigacoulomb';
    const Factor : double = 1E+09;
  end;
  TGigacoulombId = specialize TFactoredQuantityId<TCoulombUnit, TGigacoulombUnit>;

var
  GC: TGigacoulombId;

{ Unit of Megacoulomb }

type
  TMegacoulombUnit = record
    const Symbol : string = 'MC';
    const Name   : string = 'megacoulomb';
    const Factor : double = 1E+06;
  end;
  TMegacoulombId = specialize TFactoredQuantityId<TCoulombUnit, TMegacoulombUnit>;

var
  megacoulomb: TMegacoulombId;

{ Unit of Kilocoulomb }

type
  TKilocoulombUnit = record
    const Symbol : string = 'kC';
    const Name   : string = 'kilocoulomb';
    const Factor : double = 1E+03;
  end;
  TKilocoulombId = specialize TFactoredQuantityId<TCoulombUnit, TKilocoulombUnit>;

var
  kC: TKilocoulombId;

{ Unit of Millicoulomb }

type
  TMillicoulombUnit = record
    const Symbol : string = 'mC';
    const Name   : string = 'millicoulomb';
    const Factor : double = 1E-03;
  end;
  TMillicoulombId = specialize TFactoredQuantityId<TCoulombUnit, TMillicoulombUnit>;

var
  mC: TMillicoulombId;

{ Unit of Gigavolt }

type
  TGigavoltUnit = record
    const Symbol : string = 'GV';
    const Name   : string = 'gigavolt';
    const Factor : double = 1E+09;
  end;
  TGigavoltId = specialize TFactoredQuantityId<TVoltUnit, TGigavoltUnit>;

var
  GV: TGigavoltId;

{ Unit of Megavolt }

type
  TMegavoltUnit = record
    const Symbol : string = 'MV';
    const Name   : string = 'megavolt';
    const Factor : double = 1E+06;
  end;
  TMegavoltId = specialize TFactoredQuantityId<TVoltUnit, TMegavoltUnit>;

var
  megavolt: TMegavoltId;

{ Unit of Kilovolt }

type
  TKilovoltUnit = record
    const Symbol : string = 'kV';
    const Name   : string = 'kilovolt';
    const Factor : double = 1E+03;
  end;
  TKilovoltId = specialize TFactoredQuantityId<TVoltUnit, TKilovoltUnit>;

var
  kV: TKilovoltId;

{ Unit of Millivolt }

type
  TMillivoltUnit = record
    const Symbol : string = 'mV';
    const Name   : string = 'millivolt';
    const Factor : double = 1E-03;
  end;
  TMillivoltId = specialize TFactoredQuantityId<TVoltUnit, TMillivoltUnit>;

var
  mV: TMillivoltId;

{ Unit of Gigafarad }

type
  TGigafaradUnit = record
    const Symbol : string = 'GF';
    const Name   : string = 'gigafarad';
    const Factor : double = 1E+09;
  end;
  TGigafaradId = specialize TFactoredQuantityId<TFaradUnit, TGigafaradUnit>;

var
  GF: TGigafaradId;

{ Unit of Megafarad }

type
  TMegafaradUnit = record
    const Symbol : string = 'MF';
    const Name   : string = 'megafarad';
    const Factor : double = 1E+06;
  end;
  TMegafaradId = specialize TFactoredQuantityId<TFaradUnit, TMegafaradUnit>;

var
  megafarad: TMegafaradId;

{ Unit of Kilofarad }

type
  TKilofaradUnit = record
    const Symbol : string = 'kF';
    const Name   : string = 'kilofarad';
    const Factor : double = 1E+03;
  end;
  TKilofaradId = specialize TFactoredQuantityId<TFaradUnit, TKilofaradUnit>;

var
  kF: TKilofaradId;

{ Unit of Millifarad }

type
  TMillifaradUnit = record
    const Symbol : string = 'mF';
    const Name   : string = 'millifarad';
    const Factor : double = 1E-03;
  end;
  TMillifaradId = specialize TFactoredQuantityId<TFaradUnit, TMillifaradUnit>;

var
  mF: TMillifaradId;

{ Unit of Microfarad }

type
  TMicrofaradUnit = record
    const Symbol : string = 'uF';
    const Name   : string = 'microfarad';
    const Factor : double = 1E-06;
  end;
  TMicrofaradId = specialize TFactoredQuantityId<TFaradUnit, TMicrofaradUnit>;

var
  uF: TMicrofaradId;

{ Unit of Nanofarad }

type
  TNanofaradUnit = record
    const Symbol : string = 'nF';
    const Name   : string = 'nanofarad';
    const Factor : double = 1E-09;
  end;
  TNanofaradId = specialize TFactoredQuantityId<TFaradUnit, TNanofaradUnit>;

var
  nF: TNanofaradId;

{ Unit of Picofarad }

type
  TPicofaradUnit = record
    const Symbol : string = 'pF';
    const Name   : string = 'picofarad';
    const Factor : double = 1E-12;
  end;
  TPicofaradId = specialize TFactoredQuantityId<TFaradUnit, TPicofaradUnit>;

var
  pF: TPicofaradId;

{ Unit of Gigaohm }

type
  TGigaohmUnit = record
    const Symbol : string = 'GΩ';
    const Name   : string = 'gigaohm';
    const Factor : double = 1E+09;
  end;
  TGigaohmId = specialize TFactoredQuantityId<TOhmUnit, TGigaohmUnit>;

var
  gigaohm: TGigaohmId;

{ Unit of Megaohm }

type
  TMegaohmUnit = record
    const Symbol : string = 'MΩ';
    const Name   : string = 'megaohm';
    const Factor : double = 1E+06;
  end;
  TMegaohmId = specialize TFactoredQuantityId<TOhmUnit, TMegaohmUnit>;

var
  megaohm: TMegaohmId;

{ Unit of Kiloohm }

type
  TKiloohmUnit = record
    const Symbol : string = 'kΩ';
    const Name   : string = 'kiloohm';
    const Factor : double = 1E+03;
  end;
  TKiloohmId = specialize TFactoredQuantityId<TOhmUnit, TKiloohmUnit>;

var
  kiloohm: TKiloohmId;

{ Unit of Milliohm }

type
  TMilliohmUnit = record
    const Symbol : string = 'mΩ';
    const Name   : string = 'milliohm';
    const Factor : double = 1E-03;
  end;
  TMilliohmId = specialize TFactoredQuantityId<TOhmUnit, TMilliohmUnit>;

var
  milliohm: TMilliohmId;

{ Unit of Microohm }

type
  TMicroohmUnit = record
    const Symbol : string = 'uΩ';
    const Name   : string = 'microohm';
    const Factor : double = 1E-06;
  end;
  TMicroohmId = specialize TFactoredQuantityId<TOhmUnit, TMicroohmUnit>;

var
  microohm: TMicroohmId;

{ Unit of Nanoohm }

type
  TNanoohmUnit = record
    const Symbol : string = 'nΩ';
    const Name   : string = 'nanoohm';
    const Factor : double = 1E-09;
  end;
  TNanoohmId = specialize TFactoredQuantityId<TOhmUnit, TNanoohmUnit>;

var
  nanoohm: TNanoohmId;

{ Unit of Picoohm }

type
  TPicoohmUnit = record
    const Symbol : string = 'pΩ';
    const Name   : string = 'picoohm';
    const Factor : double = 1E-12;
  end;
  TPicoohmId = specialize TFactoredQuantityId<TOhmUnit, TPicoohmUnit>;

var
  picoohm: TPicoohmId;

{ Unit of Milligray }

type
  TMilligrayUnit = record
    const Symbol : string = 'mGy';
    const Name   : string = 'milli gray';
    const Factor : double = 1E-03;
  end;
  TMilligrayId = specialize TFactoredQuantityId<TGrayUnit, TMilligrayUnit>;

var
  mGy: TMilligrayId;

{ Unit of MilliSievert }

type
  TMilliSievertUnit = record
    const Symbol : string = 'mSv';
    const Name   : string = 'millisievert';
    const Factor : double = 1E-03;
  end;
  TMilliSievertId = specialize TFactoredQuantityId<TSievertUnit, TMilliSievertUnit>;

var
  mSv: TMilliSievertId;

{ Unit of JoulePerDegree }

type
  TJoulePerDegreeUnit = record
    const Symbol : string = 'J/deg';
    const Name   : string = 'joule per degree';
    const Factor : double = 180/Pi;
  end;
  TJoulePerDegreeId = specialize TFactoredQuantityId<TJoulePerRadianUnit, TJoulePerDegreeUnit>;

{ Unit of NewtonMeterPerDegree }

type
  TNewtonMeterPerDegreeUnit = record
    const Symbol : string = 'N·m/deg';
    const Name   : string = 'newton meter per degree';
    const Factor : double = 180/Pi;
  end;
  TNewtonMeterPerDegreeId = specialize TFactoredQuantityId<TJoulePerRadianUnit, TNewtonMeterPerDegreeUnit>;

{ Unit of KilometerPerHour }

type
  TKilometerPerHourUnit = record
    const Symbol : string = 'km/h';
    const Name   : string = 'kilometer per hour';
    const Factor : double = 5/18;
  end;
  TKilometerPerHourId = specialize TFactoredQuantityId<TMeterPerSecondUnit, TKilometerPerHourUnit>;

{ Unit of DecimeterPerSecond }

type
  TDecimeterPerSecondUnit = record
    const Symbol : string = 'dm/s';
    const Name   : string = 'decimeter per second';
    const Factor : double = 1E-01;
  end;
  TDecimeterPerSecondId = specialize TFactoredQuantityId<TMeterPerSecondUnit, TDecimeterPerSecondUnit>;

{ Unit of CentimeterPerSecond }

type
  TCentimeterPerSecondUnit = record
    const Symbol : string = 'cm/s';
    const Name   : string = 'centimeter per second';
    const Factor : double = 1E-02;
  end;
  TCentimeterPerSecondId = specialize TFactoredQuantityId<TMeterPerSecondUnit, TCentimeterPerSecondUnit>;

{ Unit of MillimeterPerSecond }

type
  TMillimeterPerSecondUnit = record
    const Symbol : string = 'mm/s';
    const Name   : string = 'millimeter per second';
    const Factor : double = 1E-03;
  end;
  TMillimeterPerSecondId = specialize TFactoredQuantityId<TMeterPerSecondUnit, TMillimeterPerSecondUnit>;

{ Unit of KilometerPerHourPerSecond }

type
  TKilometerPerHourPerSecondUnit = record
    const Symbol : string = 'km/h/s';
    const Name   : string = 'kilometer-hour per second';
    const Factor : double = 5/18;
  end;
  TKilometerPerHourPerSecondId = specialize TFactoredQuantityId<TMeterPerSquareSecondUnit, TKilometerPerHourPerSecondUnit>;

{ Unit of DecimeterPerSquareSecond }

type
  TDecimeterPerSquareSecondUnit = record
    const Symbol : string = 'dm/s2';
    const Name   : string = 'decimeter per square second';
    const Factor : double = 1E-01;
  end;
  TDecimeterPerSquareSecondId = specialize TFactoredQuantityId<TMeterPerSquareSecondUnit, TDecimeterPerSquareSecondUnit>;

{ Unit of CentimeterPerSquareSecond }

type
  TCentimeterPerSquareSecondUnit = record
    const Symbol : string = 'cm/s2';
    const Name   : string = 'centimeter per square second';
    const Factor : double = 1E-02;
  end;
  TCentimeterPerSquareSecondId = specialize TFactoredQuantityId<TMeterPerSquareSecondUnit, TCentimeterPerSquareSecondUnit>;

{ Unit of MillimeterPerSquareSecond }

type
  TMillimeterPerSquareSecondUnit = record
    const Symbol : string = 'mm/s2';
    const Name   : string = 'millimeter per square second';
    const Factor : double = 1E-03;
  end;
  TMillimeterPerSquareSecondId = specialize TFactoredQuantityId<TMeterPerSquareSecondUnit, TMillimeterPerSquareSecondUnit>;

{ Unit of KilogramPerCubicMillimeter }

type
  TKilogramPerCubicMillimeterUnit = record
    const Symbol : string = 'kg/mm3';
    const Name   : string = 'kilogram per cubic millimeter';
    const Factor : double = 1E+09;
  end;
  TKilogramPerCubicMillimeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, TKilogramPerCubicMillimeterUnit>;

{ Unit of KilogramPerCubicCentimeter }

type
  TKilogramPerCubicCentimeterUnit = record
    const Symbol : string = 'kg/cm3';
    const Name   : string = 'kilogram per cubic centimeter';
    const Factor : double = 1E+06;
  end;
  TKilogramPerCubicCentimeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, TKilogramPerCubicCentimeterUnit>;

{ Unit of KilogramPerCubicDecimeter }

type
  TKilogramPerCubicDecimeterUnit = record
    const Symbol : string = 'kg/dm3';
    const Name   : string = 'kilogram per cubic decimeter';
    const Factor : double = 1E+03;
  end;
  TKilogramPerCubicDecimeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, TKilogramPerCubicDecimeterUnit>;

{ Unit of HectogramPerCubicMeter }

type
  THectogramPerCubicMeterUnit = record
    const Symbol : string = 'hg/m3';
    const Name   : string = 'hectogram per cubic meter';
    const Factor : double = 1E-01;
  end;
  THectogramPerCubicMeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, THectogramPerCubicMeterUnit>;

{ Unit of DecagramPerCubicMeter }

type
  TDecagramPerCubicMeterUnit = record
    const Symbol : string = 'dag/m3';
    const Name   : string = 'decagram per cubic meter';
    const Factor : double = 1E-02;
  end;
  TDecagramPerCubicMeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, TDecagramPerCubicMeterUnit>;

{ Unit of GramPerCubicMeter }

type
  TGramPerCubicMeterUnit = record
    const Symbol : string = 'g/m3';
    const Name   : string = 'gram per cubic meter';
    const Factor : double = 1E-03;
  end;
  TGramPerCubicMeterId = specialize TFactoredQuantityId<TKilogramPerCubicMeterUnit, TGramPerCubicMeterUnit>;

{ Unit of NewtonPerMillimeter }

type
  TNewtonPerMillimeterUnit = record
    const Symbol : string = 'N/mm';
    const Name   : string = 'newton per millimeter';
    const Factor : double = 1E+03;
  end;
  TNewtonPerMillimeterId = specialize TFactoredQuantityId<TNewtonPerMeterUnit, TNewtonPerMillimeterUnit>;

{ Combining units & quantities }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TSecondId): TSquareSecondId; inline;
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TMeterId): TSquareMeterId; inline;
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

// main definition [ m3 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterId): TCubicMeterId; inline;
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

// main definition [ m4 ] = [ m3 ] * [ m ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TMeterId): TQuarticMeterId; inline;
operator *(const ALeft: TCubicMeters; const ARight: TMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TCubicMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TCubicMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TMeters): TCubicMeters; inline;

// alternative definition [ m4 ] = [ m2 ] * [ m2 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareMeterId): TQuarticMeterId; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters; inline;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TMeterId): TQuinticMeterId; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TQuarticMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuarticMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TMeters): TQuarticMeters; inline;

// alternative definition [ m5 ] = [ m3 ] * [ m2 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSquareMeterId): TQuinticMeterId; inline;
operator *(const ALeft: TCubicMeters; const ARight: TSquareMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TCubicMeters): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCubicMeters): TQuinticMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TSquareMeters): TCubicMeters; inline;

// main definition [ m6 ] = [ m5 ] * [ m ]
operator *(const {%H-}ALeft: TQuinticMeterId; const {%H-}ARight: TMeterId): TSexticMeterId; inline;
operator *(const ALeft: TQuinticMeters; const ARight: TMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuinticMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TQuinticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TMeters): TQuinticMeters; inline;

// alternative definition [ m6 ] = [ m4 ] * [ m2 ]
operator *(const {%H-}ALeft: TQuarticMeterId; const {%H-}ARight: TSquareMeterId): TSexticMeterId; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TQuarticMeters): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TSquareMeters): TQuarticMeters; inline;

// alternative definition [ m6 ] = [ m3 ] * [ m3 ]
operator *(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TCubicMeterId): TSexticMeterId; inline;
operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters; inline;
operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters; inline;

// main definition [ kg2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKilogramId): TSquareKilogramId; inline;
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TAmpereId): TSquareAmpereId; inline;
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TKelvinId): TSquareKelvinId; inline;
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TKelvinId): TCubicKelvinId; inline;
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const {%H-}ALeft: TSquareKelvinId; const {%H-}ARight: TSquareKelvinId): TQuarticKelvinId; inline;
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

//
operator *(const {%H-}ALeft: TCubicKelvinId; const {%H-}ARight: TKelvinId): TQuarticKelvinId; inline;
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ sr ] = [ rad ] * [ rad ]
operator *(const {%H-}ALeft: TRadianId; const {%H-}ARight: TRadianId): TSteradianId; inline;
operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians; inline;
operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians; inline;

// main definition [ Hz ] = 1 / [ s ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSecondId): THertzId; inline;
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const {%H-}ALeft: THertzId; const {%H-}ARight: THertzId): TSquareHertzId; inline;
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonId; inline;
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TPascalId; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;

// main definition [ J ] = [ N ] * [ m ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TJouleId; inline;
operator *(const ALeft: TNewtons; const ARight: TMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TNewtons): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TMeters): TNewtons; inline;

// alternative definition [ J ] = [ Pa ] * [ m3 ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TCubicMeterId): TJouleId; inline;
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSecondId): TWattId; inline;
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;

// alternative definition [ W ] = [ J ] * [ rad/s ]
operator *(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianPerSecondId): TWattId; inline;
operator *(const ALeft: TJoules; const ARight: TRadiansPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TRadiansPerSecond): TJoules; inline;

// alternative definition [ W ] = [ A2 ] * [ Ω ]
operator *(const {%H-}ALeft: TSquareAmpereId; const {%H-}ARight: TOhmId): TWattId; inline;
operator *(const ALeft: TSquareAmperes; const ARight: TOhms): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TSquareAmperes): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TSquareAmperes): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TOhms): TSquareAmperes; inline;

// alternative definition [ W ] = [ N ] * [ m/s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterPerSecondId): TWattId; inline;
operator *(const ALeft: TNewtons; const ARight: TMetersPerSecond): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TNewtons): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TNewtons): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TMetersPerSecond): TNewtons; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const {%H-}ALeft: TSecondId; const {%H-}ARight: TAmpereId): TCoulombId; inline;
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TCoulombId): TSquareCoulombId; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

// main definition [ V ] = [ W ] / [ A ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TAmpereId): TVoltId; inline;
operator /(const ALeft: TWatts; const ARight: TAmperes): TVolts; inline;
operator /(const ALeft: TWatts; const ARight: TVolts): TAmperes; inline;
operator *(const ALeft: TVolts; const ARight: TAmperes): TWatts; inline;
operator *(const ALeft: TAmperes; const ARight: TVolts): TWatts; inline;

// alternative definition [ V ] = [ J ] / [ C ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TCoulombId): TVoltId; inline;
operator /(const ALeft: TJoules; const ARight: TCoulombs): TVolts; inline;
operator /(const ALeft: TJoules; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TCoulombs): TJoules; inline;
operator *(const ALeft: TCoulombs; const ARight: TVolts): TJoules; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TVoltId): TSquareVoltId; inline;
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const {%H-}ALeft: TWattId; const {%H-}ARight: TOhmId): TSquareVoltId; inline;
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;

// main definition [ F ] = [ C ] / [ V ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltId): TFaradId; inline;
operator /(const ALeft: TCoulombs; const ARight: TVolts): TFarads; inline;
operator /(const ALeft: TCoulombs; const ARight: TFarads): TVolts; inline;
operator *(const ALeft: TFarads; const ARight: TVolts): TCoulombs; inline;
operator *(const ALeft: TVolts; const ARight: TFarads): TCoulombs; inline;

// alternative definition [ F ] = [ C2 ] / [ J ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TJouleId): TFaradId; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TJoules): TFarads; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TFarads): TJoules; inline;
operator *(const ALeft: TFarads; const ARight: TJoules): TSquareCoulombs; inline;
operator *(const ALeft: TJoules; const ARight: TFarads): TSquareCoulombs; inline;

// main definition [ Ω ] = [ V ] / [ A ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TAmpereId): TOhmId; inline;
operator /(const ALeft: TVolts; const ARight: TAmperes): TOhms; inline;
operator /(const ALeft: TVolts; const ARight: TOhms): TAmperes; inline;
operator *(const ALeft: TOhms; const ARight: TAmperes): TVolts; inline;
operator *(const ALeft: TAmperes; const ARight: TOhms): TVolts; inline;

// alternative definition [ Ω ] = [ s ] / [ F ]
operator /(const {%H-}ALeft: TSecondId; const {%H-}ARight: TFaradId): TOhmId; inline;
operator /(const ALeft: TSeconds; const ARight: TFarads): TOhms; inline;
operator /(const ALeft: TSeconds; const ARight: TOhms): TFarads; inline;
operator *(const ALeft: TOhms; const ARight: TFarads): TSeconds; inline;
operator *(const ALeft: TFarads; const ARight: TOhms): TSeconds; inline;

// main definition [ S ] = 1 / [ Ω ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TOhmId): TSiemensId; inline;
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TSecondId): TWeberId; inline;
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TSquareMeterId): TTeslaId; inline;
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;

// main definition [ H ] = [ Wb ] / [ A ]
operator /(const {%H-}ALeft: TWeberId; const {%H-}ARight: TAmpereId): THenryId; inline;
operator /(const ALeft: TWebers; const ARight: TAmperes): THenrys; inline;
operator /(const ALeft: TWebers; const ARight: THenrys): TAmperes; inline;
operator *(const ALeft: THenrys; const ARight: TAmperes): TWebers; inline;
operator *(const ALeft: TAmperes; const ARight: THenrys): TWebers; inline;

// alternative definition [ H ] = [ Ω ] * [ s ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TSecondId): THenryId; inline;
operator *(const ALeft: TOhms; const ARight: TSeconds): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TOhms): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TOhms): THenrys; inline;
operator /(const ALeft: THenrys; const ARight: TSeconds): TOhms; inline;

// alternative definition [ H ] = [ Ω ] / [ Hz ]
operator /(const {%H-}ALeft: TOhmId; const {%H-}ARight: THertzId): THenryId; inline;
operator /(const ALeft: TOhms; const ARight: THertz): THenrys; inline;
operator /(const ALeft: TOhms; const ARight: THenrys): THertz; inline;
operator *(const ALeft: THenrys; const ARight: THertz): TOhms; inline;
operator *(const ALeft: THertz; const ARight: THenrys): TOhms; inline;

// main definition [ lm ] = [ cd ] * [ sr ]
operator *(const {%H-}ALeft: TCandelaId; const {%H-}ARight: TSteradianId): TLumenId; inline;
operator *(const ALeft: TCandelas; const ARight: TSteradians): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TCandelas): TSteradians; inline;
operator *(const ALeft: TSteradians; const ARight: TCandelas): TLumens; inline;
operator /(const ALeft: TLumens; const ARight: TSteradians): TCandelas; inline;

// main definition [ lx ] = [ lm ] / [ m2 ]
operator /(const {%H-}ALeft: TLumenId; const {%H-}ARight: TSquareMeterId): TLuxId; inline;
operator /(const ALeft: TLumens; const ARight: TSquareMeters): TLux; inline;
operator /(const ALeft: TLumens; const ARight: TLux): TSquareMeters; inline;
operator *(const ALeft: TLux; const ARight: TSquareMeters): TLumens; inline;
operator *(const ALeft: TSquareMeters; const ARight: TLux): TLumens; inline;

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const {%H-}ALeft: TMoleId; const {%H-}ARight: TSecondId): TKatalId; inline;
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;

// main definition [ J/rad ] = [ J ] / [ rad ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TRadianId): TJoulePerRadianId; inline;
operator /(const ALeft: TJoules; const ARight: TRadians): TJoulesPerRadian; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerRadian): TRadians; inline;
operator *(const ALeft: TJoulesPerRadian; const ARight: TRadians): TJoules; inline;
operator *(const ALeft: TRadians; const ARight: TJoulesPerRadian): TJoules; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSecondId): TMeterPerSecondId; inline;
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;

// main definition [ m/s2 ] = [ m/s ] / [ s ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TSecondId): TMeterPerSquareSecondId; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMetersPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSeconds): TMetersPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSquareSecond): TMetersPerSecond; inline;

// alternative definition [ m/s2 ] = [ m ] / [ s2 ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TSquareSecondId): TMeterPerSquareSecondId; inline;
operator /(const ALeft: TMeters; const ARight: TSquareSeconds): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TSquareSeconds): TMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TMetersPerSquareSecond): TMeters; inline;

// alternative definition [ m/s2 ] = [ m2/s2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TMeters; const ARight: TMetersPerSquareSecond): TSquareMetersPerSquareSecond; inline;

// alternative definition [ m/s2 ] = [ rad2/s2 ] * [ m ]
operator *(const {%H-}ALeft: TSteradianPerSquareSecondId; const {%H-}ARight: TMeterId): TMeterPerSquareSecondId; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSteradiansPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSteradiansPerSquareSecond): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSteradiansPerSquareSecond; inline;

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSecondId): TRadianPerSecondId; inline;
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;

// alternative definition [ rad/s ] = [ m/s ] / [ m ]
operator /(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterId): TRadianPerSecondId; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): TRadiansPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TRadiansPerSecond): TMeters; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerSecond): TMetersPerSecond; inline;

// main definition [ rad/s2 ] = [ rad ] / [ s2 ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TSquareSecondId): TRadianPerSquareSecondId; inline;
operator /(const ALeft: TRadians; const ARight: TSquareSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSquareSeconds): TRadians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TRadiansPerSquareSecond): TRadians; inline;

// main definition [ rad/s2 ] = [ rad/s ] / [ s ]
operator /(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TSecondId): TRadianPerSquareSecondId; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadiansPerSquareSecond; inline;
operator /(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSquareSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSquareSecond; const ARight: TSeconds): TRadiansPerSecond; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSquareSecond): TRadiansPerSecond; inline;

// main definition [ kg/m ] = [ kg ] / [ m ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterId): TKilogramPerMeterId; inline;
operator /(const ALeft: TKilograms; const ARight: TMeters): TKilogramsPerMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TKilogramsPerMeter; const ARight: TMeters): TKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TKilogramsPerMeter): TKilograms; inline;

// main definition [ kg/m2 ] = [ kg ] / [ m2 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramPerSquareMeterId; inline;
operator /(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsPerSquareMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TKilogramsPerSquareMeter; const ARight: TSquareMeters): TKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilogramsPerSquareMeter): TKilograms; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TCubicMeterId): TKilogramPerCubicMeterId; inline;
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCubicMeterId): TNewtonPerCubicMeterId; inline;
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerCubicMeterId; inline;
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterId; const {%H-}ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

// main definition [ N/m ] = [ N ] / [ m ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;
operator /(const ALeft: TNewtons; const ARight: TMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TNewtons; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerMeter): TNewtons; inline;

// alternative definition [ N/m ] = [ J ] / [ m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareMeterId): TNewtonPerMeterId; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TNewtonsPerMeter; const ARight: TSquareMeters): TJoules; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerMeter): TJoules; inline;

// alternative definition [ N/m ] = [ Pa ] * [ m ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TMeterId): TNewtonPerMeterId; inline;
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TMeterPerSecondId): TKilogramMeterPerSecondId; inline;
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;

// alternative definition [ N*s ] = [ N ] * [ s ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSecondId): TKilogramMeterPerSecondId; inline;
operator *(const ALeft: TNewtons; const ARight: TSeconds): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TNewtons): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TKilogramMetersPerSecond; inline;
operator /(const ALeft: TKilogramMetersPerSecond; const ARight: TSeconds): TNewtons; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TSquareMeterId): TKilogramSquareMeterId; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramSquareMeters; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSquareMeters): TKilograms; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TSecondId): TKilogramSquareMeterPerSecondId; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TSeconds): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMeters; const ARight: TKilogramSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TSeconds): TKilogramSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramSquareMetersPerSecond): TKilogramSquareMeters; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] * [ rad/s ]
operator *(const {%H-}ALeft: TKilogramSquareMeterId; const {%H-}ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId; inline;
operator *(const ALeft: TKilogramSquareMeters; const ARight: TRadiansPerSecond): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TKilogramSquareMeters): TRadiansPerSecond; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TKilogramSquareMeters): TKilogramSquareMetersPerSecond; inline;
operator /(const ALeft: TKilogramSquareMetersPerSecond; const ARight: TRadiansPerSecond): TKilogramSquareMeters; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareSecondId): TSquareMeterPerSquareSecondId; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const {%H-}ALeft: TMeterPerSecondId; const {%H-}ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramId): TSquareMeterPerSquareSecondId; inline;
operator /(const ALeft: TJoules; const ARight: TKilograms): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSquareMetersPerSquareSecond): TKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilograms): TJoules; inline;
operator *(const ALeft: TKilograms; const ARight: TSquareMetersPerSquareSecond): TJoules; inline;

// alternative definition [ m2/s2 ] = [ Pa ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;

// main definition [ sr ] = [ sr ] / [ s2 ]
operator /(const {%H-}ALeft: TSteradianId; const {%H-}ARight: TSquareSecondId): TSteradianPerSquareSecondId; inline;
operator /(const ALeft: TSteradians; const ARight: TSquareSeconds): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradians; const ARight: TSteradiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TSquareSeconds): TSteradians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSteradiansPerSquareSecond): TSteradians; inline;

// alternative definition [ sr/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const {%H-}ALeft: TRadianPerSecondId; const {%H-}ARight: TRadianPerSecondId): TSteradianPerSquareSecondId; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond; inline;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramSquareMeters): TSteradiansPerSquareSecond; inline;
operator /(const ALeft: TJoules; const ARight: TSteradiansPerSquareSecond): TKilogramSquareMeters; inline;
operator *(const ALeft: TSteradiansPerSquareSecond; const ARight: TKilogramSquareMeters): TJoules; inline;
operator *(const ALeft: TKilogramSquareMeters; const ARight: TSteradiansPerSquareSecond): TJoules; inline;

// main definition [ m3/s ] = [ m3 ] / [ s ]
operator /(const {%H-}ALeft: TCubicMeterId; const {%H-}ARight: TSecondId): TCubicMeterPerSecondId; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSeconds): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMeters; const ARight: TCubicMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TCubicMetersPerSecond; const ARight: TSeconds): TCubicMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TCubicMetersPerSecond): TCubicMeters; inline;

// alternative definition [ m3/s ] = [ m2 ] * [ m/s ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TMeterPerSecondId): TCubicMeterPerSecondId; inline;
operator *(const ALeft: TSquareMeters; const ARight: TMetersPerSecond): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TSquareMeters): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSquareMeters): TCubicMetersPerSecond; inline;
operator /(const ALeft: TCubicMetersPerSecond; const ARight: TMetersPerSecond): TSquareMeters; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const {%H-}ALeft: TPascalId; const {%H-}ARight: TSecondId): TPascalSecondId; inline;
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TPascals): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalSeconds; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSeconds): TPascals; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSecondId): TSquareMeterPerSecondId; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const {%H-}ALeft: TPascalSecondId; const {%H-}ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator /(const ALeft: TPascalSeconds; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalSeconds; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalSeconds; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramId): TNewtonPerSquareKilogramId; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;

// main definition [ kg2/m ] = [ kg2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TMeterId): TSquareKilogramPerMeterId; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TMeters): TSquareKilogramsPerMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TMeters): TSquareKilograms; inline;
operator *(const ALeft: TMeters; const ARight: TSquareKilogramsPerMeter): TSquareKilograms; inline;

// main definition [ kg2/m2 ] = [ kg2 ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareKilogramId; const {%H-}ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareMeters): TSquareKilogramsPerSquareMeter; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TSquareKilogramsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TSquareMeters): TSquareKilograms; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareKilogramsPerSquareMeter): TSquareKilograms; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons; inline;

// main definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareKilograms): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonSquareMetersPerSquareKilogram): TNewtonSquareMeters; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId; inline;
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareKilogram): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonSquareMetersPerSquareKilogram): TJoules; inline;

// main definition [ 1/K ] = 1 / [ K ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TKelvinId): TReciprocalKelvinId; inline;
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const {%H-}ALeft: TKilogramId; const {%H-}ARight: TKelvinId): TKilogramKelvinId; inline;
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKilograms): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramKelvins; inline;
operator /(const ALeft: TKilogramKelvins; const ARight: TKelvins): TKilograms; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKelvinId): TJoulePerKelvinId; inline;
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramKelvins): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramKelvins; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramKelvins): TJoules; inline;
operator *(const ALeft: TKilogramKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoules; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondId; const {%H-}ARight: TKelvinId): TJoulePerKilogramPerKelvinId; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKelvins): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TJoulesPerKilogramPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TSquareMetersPerSquareSecond; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TKilogramId): TJoulePerKilogramPerKelvinId; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const {%H-}ALeft: TMeterId; const {%H-}ARight: TKelvinId): TMeterKelvinId; inline;
operator *(const ALeft: TMeters; const ARight: TKelvins): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMeterKelvins; inline;
operator /(const ALeft: TMeterKelvins; const ARight: TKelvins): TMeters; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const {%H-}ALeft: TKelvinId; const {%H-}ARight: TMeterId): TKelvinPerMeterId; inline;
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterId): TWattPerMeterId; inline;
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterId; inline;
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TKelvinId): TWattPerKelvinId; inline;
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TMeterKelvinId): TWattPerMeterPerKelvinId; inline;
operator /(const ALeft: TWatts; const ARight: TMeterKelvins): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMeterKelvins; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeterKelvins): TWatts; inline;
operator *(const ALeft: TMeterKelvins; const ARight: TWattsPerMeterPerKelvin): TWatts; inline;

// alternative definition [ W/m/K ] = [ W/m ] / [ K ]
operator /(const {%H-}ALeft: TWattPerMeterId; const {%H-}ARight: TKelvinId): TWattPerMeterPerKelvinId; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TKelvins): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerMeter; const ARight: TWattsPerMeterPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvins): TWattsPerMeter; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerMeterPerKelvin): TWattsPerMeter; inline;

// alternative definition [ W/m/K ] = [ W/K ] / [ m ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TMeterId): TWattPerMeterPerKelvinId; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TMeters): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerMeterPerKelvin): TMeters; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMeters): TWattsPerKelvin; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeterPerKelvin): TWattsPerKelvin; inline;

// alternative definition [ W/m/K ] = [ W/m2 ] / [ K/m ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvinsPerMeter): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerMeterPerKelvin): TKelvinsPerMeter; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TKelvinsPerMeter): TWattsPerSquareMeter; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TWattsPerMeterPerKelvin): TWattsPerSquareMeter; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TKelvinId): TSquareMeterKelvinId; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TSquareMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMeterKelvins; inline;
operator /(const ALeft: TSquareMeterKelvins; const ARight: TKelvins): TSquareMeters; inline;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const ALeft: TWatts; const ARight: TSquareMeterKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeterKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeterKelvins): TWatts; inline;
operator *(const ALeft: TSquareMeterKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWatts; inline;

// alternative definition [ W/m2/K ] = [ W/m2 ] / [ K ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TKelvinId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TKelvins): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TKelvins): TWattsPerSquareMeter; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerSquareMeter; inline;

// alternative definition [ W/m2/K ] = [ W/K ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWattsPerKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMeters): TWattsPerKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerKelvin): TWattsPerKelvin; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId; inline;
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMeterQuarticKelvins; inline;
operator /(const ALeft: TSquareMeterQuarticKelvins; const ARight: TQuarticKelvins): TSquareMeters; inline;

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TQuarticKelvinId): TWattPerQuarticKelvinId; inline;
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const {%H-}ALeft: TWattId; const {%H-}ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const ALeft: TWatts; const ARight: TSquareMeterQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeterQuarticKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeterQuarticKelvins): TWatts; inline;
operator *(const ALeft: TSquareMeterQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts; inline;

// alternative definition [ W/m2/K4 ] = [ W/m2 ] / [ K4 ]
operator /(const {%H-}ALeft: TWattPerSquareMeterId; const {%H-}ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TQuarticKelvins): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerSquareMeter; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TQuarticKelvins): TWattsPerSquareMeter; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerSquareMeter; inline;

// alternative definition [ W/m2/K4 ] = [ W/K4 ] / [ m2 ]
operator /(const {%H-}ALeft: TWattPerQuarticKelvinId; const {%H-}ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWattsPerQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMeters): TWattsPerQuarticKelvin; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWattsPerQuarticKelvin; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleId): TJoulePerMoleId; inline;
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const {%H-}ALeft: TMoleId; const {%H-}ARight: TKelvinId): TMoleKelvinId; inline;
operator *(const ALeft: TMoles; const ARight: TKelvins): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TMoles): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMoleKelvins; inline;
operator /(const ALeft: TMoleKelvins; const ARight: TKelvins): TMoles; inline;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TMoleKelvinId): TJoulePerMolePerKelvinId; inline;
operator /(const ALeft: TJoules; const ARight: TMoleKelvins): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMoleKelvins; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoleKelvins): TJoules; inline;
operator *(const ALeft: TMoleKelvins; const ARight: TJoulesPerMolePerKelvin): TJoules; inline;

// alternative definition [ J/mol/K ] = [ J/K ] / [ mol ]
operator /(const {%H-}ALeft: TJoulePerKelvinId; const {%H-}ARight: TMoleId): TJoulePerMolePerKelvinId; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TMoles): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerMolePerKelvin): TMoles; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMoles): TJoulesPerKelvin; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMolePerKelvin): TJoulesPerKelvin; inline;

// alternative definition [ J/mol/K ] = [ J/mol ] / [ K ]
operator /(const {%H-}ALeft: TJoulePerMoleId; const {%H-}ARight: TKelvinId): TJoulePerMolePerKelvinId; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TKelvins): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoulesPerMole; const ARight: TJoulesPerMolePerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TKelvins): TJoulesPerMole; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerMolePerKelvin): TJoulesPerMole; inline;

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const {%H-}ALeft: TOhmId; const {%H-}ARight: TMeterId): TOhmMeterId; inline;
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TOhms): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmMeters; inline;
operator /(const ALeft: TOhmMeters; const ARight: TMeters): TOhms; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltPerMeterId; inline;
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;

// alternative definition [ V/m ] = [ N/C ] = [ N ] / [ C ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TCoulombId): TVoltPerMeterId; inline;
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TVoltsPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TVoltsPerMeter): TCoulombs; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TCoulombs): TNewtons; inline;
operator *(const ALeft: TCoulombs; const ARight: TVoltsPerMeter): TNewtons; inline;

// alternative definition [ V/m ] = [ N/C ] = [ T ] * [ m/s ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerSecondId): TVoltPerMeterId; inline;
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TTeslas): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TVoltsPerMeter; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TMetersPerSecond): TTeslas; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TMeterId): TCoulombPerMeterId; inline;
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;

// main definition [ C2/m ] = [ C2 ] / [ m ]
operator /(const {%H-}ALeft: TSquareCoulombId; const {%H-}ARight: TMeterId): TSquareCoulombPerMeterId; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TMeters): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TMeters): TSquareCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TSquareCoulombsPerMeter): TSquareCoulombs; inline;

// alternative definition [ C2/m ] = [ C/m ] * [ C ]
operator *(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TCoulombId): TSquareCoulombPerMeterId; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TCoulombs): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombsPerMeter): TCoulombs; inline;
operator *(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TSquareCoulombsPerMeter; inline;
operator /(const ALeft: TSquareCoulombsPerMeter; const ARight: TCoulombs): TCoulombsPerMeter; inline;

// main definition [ C/m2 ] = [ C ] / [ m2 ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TSquareMeterId): TCoulombPerSquareMeterId; inline;
operator /(const ALeft: TCoulombs; const ARight: TSquareMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareMeters): TCoulombs; inline;
operator *(const ALeft: TSquareMeters; const ARight: TCoulombsPerSquareMeter): TCoulombs; inline;

// alternative definition [ C/m2 ] = [ C/m ] / [ m ]
operator /(const {%H-}ALeft: TCoulombPerMeterId; const {%H-}ARight: TMeterId): TCoulombPerSquareMeterId; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerMeter; const ARight: TCoulombsPerSquareMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TMeters): TCoulombsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerSquareMeter): TCoulombsPerMeter; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TSquareMeterId; const {%H-}ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareCoulombId): TNewtonPerSquareCoulombId; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterId; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtons): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonSquareMeters; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareMeters): TNewtons; inline;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonSquareMeterId; const {%H-}ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TSquareCoulombs): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMeters; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonSquareMetersPerSquareCoulomb): TNewtonSquareMeters; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombId; const {%H-}ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ V/m ] / [ C/m2 ]
operator /(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TCoulombsPerSquareMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TVoltsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TVoltsPerMeter; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TVoltsPerMeter; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const {%H-}ALeft: TJouleId; const {%H-}ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId; inline;
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonSquareMetersPerSquareCoulomb): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): TJoules; inline;

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const {%H-}ALeft: TVoltId; const {%H-}ARight: TMeterId): TVoltMeterId; inline;
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVolts): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TMeters): TVolts; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const {%H-}ALeft: TVoltPerMeterId; const {%H-}ARight: TSquareMeterId): TVoltMeterId; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltMeters; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSquareMeters): TVoltsPerMeter; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const {%H-}ALeft: TVoltMeterId; const {%H-}ARight: TSecondId): TVoltMeterPerSecondId; inline;
operator /(const ALeft: TVoltMeters; const ARight: TSeconds): TVoltMetersPerSecond; inline;
operator /(const ALeft: TVoltMeters; const ARight: TVoltMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TVoltMetersPerSecond; const ARight: TSeconds): TVoltMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltMetersPerSecond): TVoltMeters; inline;

// main definition [ F/m ] = [ F ] / [ m ]
operator /(const {%H-}ALeft: TFaradId; const {%H-}ARight: TMeterId): TFaradPerMeterId; inline;
operator /(const ALeft: TFarads; const ARight: TMeters): TFaradsPerMeter; inline;
operator /(const ALeft: TFarads; const ARight: TFaradsPerMeter): TMeters; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TMeters): TFarads; inline;
operator *(const ALeft: TMeters; const ARight: TFaradsPerMeter): TFarads; inline;

// alternative definition [ F/m ] = [ C ] / [ V*m ]
operator /(const {%H-}ALeft: TCoulombId; const {%H-}ARight: TVoltMeterId): TFaradPerMeterId; inline;
operator /(const ALeft: TCoulombs; const ARight: TVoltMeters): TFaradsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TFaradsPerMeter): TVoltMeters; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltMeters): TCoulombs; inline;
operator *(const ALeft: TVoltMeters; const ARight: TFaradsPerMeter): TCoulombs; inline;

// alternative definition [ F/m ] = [ C/m2 ] / [ N/C ]
operator /(const {%H-}ALeft: TCoulombPerSquareMeterId; const {%H-}ARight: TVoltPerMeterId): TFaradPerMeterId; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TVoltsPerMeter): TFaradsPerMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TFaradsPerMeter): TVoltsPerMeter; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TVoltsPerMeter): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TFaradsPerMeter): TCoulombsPerSquareMeter; inline;

// alternative definition [ F/m ] = [ 1 ] / [ N*m2/C2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId; inline;
operator /(const ALeft: double; const ARight: TNewtonSquareMetersPerSquareCoulomb): TFaradsPerMeter; inline;
operator /(const ALeft: double; const ARight: TFaradsPerMeter): TNewtonSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TFaradsPerMeter; const ARight: TNewtonSquareMetersPerSquareCoulomb): double; inline;
operator *(const ALeft: TNewtonSquareMetersPerSquareCoulomb; const ARight: TFaradsPerMeter): double; inline;

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const {%H-}ALeft: TAmpereId; const {%H-}ARight: TMeterId): TAmperePerMeterId; inline;
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const {%H-}ALeft: TMeterId; const {%H-}ARight: TAmpereId): TMeterPerAmpereId; inline;
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterId): TTeslaMeterId; inline;
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TTeslas): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslaMeters; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TMeters): TTeslas; inline;

// main definition [ T*m ] = [ N/A ] = [ N ] / [ A ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TAmpereId): TTeslaMeterId; inline;
operator /(const ALeft: TNewtons; const ARight: TAmperes): TTeslaMeters; inline;
operator /(const ALeft: TNewtons; const ARight: TTeslaMeters): TAmperes; inline;
operator *(const ALeft: TTeslaMeters; const ARight: TAmperes): TNewtons; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslaMeters): TNewtons; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmpereId): TTeslaPerAmpereId; inline;
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;

// main definition [ H/m ] = [ H ] / [ m ]
operator /(const {%H-}ALeft: THenryId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;
operator /(const ALeft: THenrys; const ARight: TMeters): THenrysPerMeter; inline;
operator /(const ALeft: THenrys; const ARight: THenrysPerMeter): TMeters; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TMeters): THenrys; inline;
operator *(const ALeft: TMeters; const ARight: THenrysPerMeter): THenrys; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T*m ] / [ A ]
operator /(const {%H-}ALeft: TTeslaMeterId; const {%H-}ARight: TAmpereId): THenryPerMeterId; inline;
operator /(const ALeft: TTeslaMeters; const ARight: TAmperes): THenrysPerMeter; inline;
operator /(const ALeft: TTeslaMeters; const ARight: THenrysPerMeter): TAmperes; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperes): TTeslaMeters; inline;
operator *(const ALeft: TAmperes; const ARight: THenrysPerMeter): TTeslaMeters; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T/A ] * [ m ]
operator *(const {%H-}ALeft: TTeslaPerAmpereId; const {%H-}ARight: TMeterId): THenryPerMeterId; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslasPerAmpere): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMeters): TTeslasPerAmpere; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] * [ m/A ]
operator *(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TMeterPerAmpereId): THenryPerMeterId; inline;
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TTeslas): TMetersPerAmpere; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): THenrysPerMeter; inline;
operator /(const ALeft: THenrysPerMeter; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ H/ m ] = [ T*m/A ] = [ T ] / [ A/m ]
operator /(const {%H-}ALeft: TTeslaId; const {%H-}ARight: TAmperePerMeterId): THenryPerMeterId; inline;
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): THenrysPerMeter; inline;
operator /(const ALeft: TTeslas; const ARight: THenrysPerMeter): TAmperesPerMeter; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TAmperesPerMeter): TTeslas; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: THenrysPerMeter): TTeslas; inline;

// alternative definition [ H/m ] = [ N/A2 ] = [ N ] / [ A2 ]
operator /(const {%H-}ALeft: TNewtonId; const {%H-}ARight: TSquareAmpereId): THenryPerMeterId; inline;
operator /(const ALeft: TNewtons; const ARight: TSquareAmperes): THenrysPerMeter; inline;
operator /(const ALeft: TNewtons; const ARight: THenrysPerMeter): TSquareAmperes; inline;
operator *(const ALeft: THenrysPerMeter; const ARight: TSquareAmperes): TNewtons; inline;
operator *(const ALeft: TSquareAmperes; const ARight: THenrysPerMeter): TNewtons; inline;

// main definition [ rad/m ] = [ rad ] / [ m ]
operator /(const {%H-}ALeft: TRadianId; const {%H-}ARight: TMeterId): TRadianPerMeterId; inline;
operator /(const ALeft: TRadians; const ARight: TMeters): TRadiansPerMeter; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerMeter): TMeters; inline;
operator *(const ALeft: TRadiansPerMeter; const ARight: TMeters): TRadians; inline;
operator *(const ALeft: TMeters; const ARight: TRadiansPerMeter): TRadians; inline;

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

{ TQuantityId }

class function TQuantityId.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.Value := AQuantity.Value;
end;

class operator TQuantityId.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue;
end;

{ TFactoredQuantityId }

class function TFactoredQuantityId.From(const AQuantity: TBaseQuantity): TFactoredQuantity;
begin
  result.Value := AQuantity.Value / U.Factor;
end;

class operator TFactoredQuantityId.*(const AValue: double; const TheUnit: TSelf): TBaseQuantity;
begin
  result.Value := AValue * U.Factor;
end;

{ Combining units & quantities  }

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSecondId; const ARight: TSecondId): TSquareSecondId;
begin end;

operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterId; const ARight: TMeterId): TSquareMeterId;
begin end;

operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeterId; const ARight: TMeterId): TCubicMeterId;
begin end;

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
operator *(const ALeft: TCubicMeterId; const ARight: TMeterId): TQuarticMeterId;
begin end;

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
operator *(const ALeft: TSquareMeterId; const ARight: TSquareMeterId): TQuarticMeterId;
begin end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TSquareMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m5 ] = [ m4 ] * [ m ]
operator *(const ALeft: TQuarticMeterId; const ARight: TMeterId): TQuinticMeterId;
begin end;

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
operator *(const ALeft: TCubicMeterId; const ARight: TSquareMeterId): TQuinticMeterId;
begin end;

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
operator *(const ALeft: TQuinticMeterId; const ARight: TMeterId): TSexticMeterId;
begin end;

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
operator *(const ALeft: TQuarticMeterId; const ARight: TSquareMeterId): TSexticMeterId;
begin end;

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
operator *(const ALeft: TCubicMeterId; const ARight: TCubicMeterId): TSexticMeterId;
begin end;

operator *(const ALeft: TCubicMeters; const ARight: TCubicMeters): TSexticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSexticMeters; const ARight: TCubicMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ kg2 ]
operator *(const ALeft: TKilogramId; const ARight: TKilogramId): TSquareKilogramId;
begin end;

operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereId; const ARight: TAmpereId): TSquareAmpereId;
begin end;

operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinId; const ARight: TKelvinId): TSquareKelvinId;
begin end;

operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvinId; const ARight: TKelvinId): TCubicKelvinId;
begin end;

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
operator *(const ALeft: TSquareKelvinId; const ARight: TSquareKelvinId): TQuarticKelvinId;
begin end;

operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

//
operator *(const ALeft: TCubicKelvinId; const ARight: TKelvinId): TQuarticKelvinId;
begin end;

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
operator *(const ALeft: TRadianId; const ARight: TRadianId): TSteradianId;
begin end;

operator *(const ALeft: TRadians; const ARight: TRadians): TSteradians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradians; const ARight: TRadians): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSecondId): THertzId;
begin end;

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
operator *(const ALeft: THertzId; const ARight: THertzId): TSquareHertzId;
begin end;

operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSquareSecondId): TNewtonId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TSquareMeterId): TPascalId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TMeterId): TJouleId;
begin end;

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
operator *(const ALeft: TPascalId; const ARight: TCubicMeterId): TJouleId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TSecondId): TWattId;
begin end;

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
operator *(const ALeft: TJouleId; const ARight: TRadianPerSecondId): TWattId;
begin end;

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
operator *(const ALeft: TSquareAmpereId; const ARight: TOhmId): TWattId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TMeterPerSecondId): TWattId;
begin end;

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
operator *(const ALeft: TSecondId; const ARight: TAmpereId): TCoulombId;
begin end;

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
operator *(const ALeft: TCoulombId; const ARight: TCoulombId): TSquareCoulombId;
begin end;

operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ V ] = [ W ] / [ A ]
operator /(const ALeft: TWattId; const ARight: TAmpereId): TVoltId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TCoulombId): TVoltId;
begin end;

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
operator *(const ALeft: TVoltId; const ARight: TVoltId): TSquareVoltId;
begin end;

operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWattId; const ARight: TOhmId): TSquareVoltId;
begin end;

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
operator /(const ALeft: TCoulombId; const ARight: TVoltId): TFaradId;
begin end;

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
operator /(const ALeft: TSquareCoulombId; const ARight: TJouleId): TFaradId;
begin end;

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
operator /(const ALeft: TVoltId; const ARight: TAmpereId): TOhmId;
begin end;

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
operator /(const ALeft: TSecondId; const ARight: TFaradId): TOhmId;
begin end;

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
operator /(const ALeft: double; const ARight: TOhmId): TSiemensId;
begin end;

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
operator *(const ALeft: TVoltId; const ARight: TSecondId): TWeberId;
begin end;

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
operator /(const ALeft: TWeberId; const ARight: TSquareMeterId): TTeslaId;
begin end;

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
operator /(const ALeft: TWeberId; const ARight: TAmpereId): THenryId;
begin end;

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
operator *(const ALeft: TOhmId; const ARight: TSecondId): THenryId;
begin end;

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
operator /(const ALeft: TOhmId; const ARight: THertzId): THenryId;
begin end;

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
operator *(const ALeft: TCandelaId; const ARight: TSteradianId): TLumenId;
begin end;

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
operator /(const ALeft: TLumenId; const ARight: TSquareMeterId): TLuxId;
begin end;

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
operator /(const ALeft: TMoleId; const ARight: TSecondId): TKatalId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TRadianId): TJoulePerRadianId;
begin end;

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
operator /(const ALeft: TMeterId; const ARight: TSecondId): TMeterPerSecondId;
begin end;

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
operator /(const ALeft: TMeterPerSecondId; const ARight: TSecondId): TMeterPerSquareSecondId;
begin end;

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
operator /(const ALeft: TMeterId; const ARight: TSquareSecondId): TMeterPerSquareSecondId;
begin end;

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
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

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
operator *(const ALeft: TSteradianPerSquareSecondId; const ARight: TMeterId): TMeterPerSquareSecondId;
begin end;

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
operator /(const ALeft: TRadianId; const ARight: TSecondId): TRadianPerSecondId;
begin end;

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
operator /(const ALeft: TMeterPerSecondId; const ARight: TMeterId): TRadianPerSecondId;
begin end;

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
operator /(const ALeft: TRadianId; const ARight: TSquareSecondId): TRadianPerSquareSecondId;
begin end;

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
operator /(const ALeft: TRadianPerSecondId; const ARight: TSecondId): TRadianPerSquareSecondId;
begin end;

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
operator /(const ALeft: TKilogramId; const ARight: TMeterId): TKilogramPerMeterId;
begin end;

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
operator /(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramPerSquareMeterId;
begin end;

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
operator /(const ALeft: TKilogramId; const ARight: TCubicMeterId): TKilogramPerCubicMeterId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TCubicMeterId): TNewtonPerCubicMeterId;
begin end;

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
operator /(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerCubicMeterId;
begin end;

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
operator *(const ALeft: TKilogramPerCubicMeterId; const ARight: TMeterPerSquareSecondId): TNewtonPerCubicMeterId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TSquareMeterId): TNewtonPerMeterId;
begin end;

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
operator *(const ALeft: TPascalId; const ARight: TMeterId): TNewtonPerMeterId;
begin end;

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
operator *(const ALeft: TKilogramId; const ARight: TMeterPerSecondId): TKilogramMeterPerSecondId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TSecondId): TKilogramMeterPerSecondId;
begin end;

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
operator *(const ALeft: TKilogramId; const ARight: TSquareMeterId): TKilogramSquareMeterId;
begin end;

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
operator /(const ALeft: TKilogramSquareMeterId; const ARight: TSecondId): TKilogramSquareMeterPerSecondId;
begin end;

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
operator *(const ALeft: TKilogramSquareMeterId; const ARight: TRadianPerSecondId): TKilogramSquareMeterPerSecondId;
begin end;

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
operator /(const ALeft: TSquareMeterId; const ARight: TSquareSecondId): TSquareMeterPerSquareSecondId;
begin end;

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
operator *(const ALeft: TMeterPerSecondId; const ARight: TMeterPerSecondId): TSquareMeterPerSquareSecondId;
begin end;

operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ m2/s2 ] = [ J ] / [ kg ]
operator /(const ALeft: TJouleId; const ARight: TKilogramId): TSquareMeterPerSquareSecondId;
begin end;

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
operator /(const ALeft: TPascalId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSquareSecondId;
begin end;

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
operator /(const ALeft: TSteradianId; const ARight: TSquareSecondId): TSteradianPerSquareSecondId;
begin end;

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
operator *(const ALeft: TRadianPerSecondId; const ARight: TRadianPerSecondId): TSteradianPerSquareSecondId;
begin end;

operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSteradiansPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSteradiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ sr/s2 ] = [ J ] / [ kg*m2 ]
operator /(const ALeft: TJouleId; const ARight: TKilogramSquareMeterId): TSteradianPerSquareSecondId;
begin end;

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
operator /(const ALeft: TCubicMeterId; const ARight: TSecondId): TCubicMeterPerSecondId;
begin end;

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
operator *(const ALeft: TSquareMeterId; const ARight: TMeterPerSecondId): TCubicMeterPerSecondId;
begin end;

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
operator *(const ALeft: TPascalId; const ARight: TSecondId): TPascalSecondId;
begin end;

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
operator /(const ALeft: TSquareMeterId; const ARight: TSecondId): TSquareMeterPerSecondId;
begin end;

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
operator /(const ALeft: TPascalSecondId; const ARight: TKilogramPerCubicMeterId): TSquareMeterPerSecondId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramId): TNewtonPerSquareKilogramId;
begin end;

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
operator /(const ALeft: TSquareKilogramId; const ARight: TMeterId): TSquareKilogramPerMeterId;
begin end;

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
operator /(const ALeft: TSquareKilogramId; const ARight: TSquareMeterId): TSquareKilogramPerSquareMeterId;
begin end;

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
operator /(const ALeft: TSquareMeterId; const ARight: TSquareKilogramId): TSquareMeterPerSquareKilogramId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TSquareKilogramPerSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

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
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareKilogramId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

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
operator *(const ALeft: TNewtonPerSquareKilogramId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TSquareKilogramPerMeterId): TNewtonSquareMeterPerSquareKilogramId;
begin end;

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
operator /(const ALeft: double; const ARight: TKelvinId): TReciprocalKelvinId;
begin end;

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
operator *(const ALeft: TKilogramId; const ARight: TKelvinId): TKilogramKelvinId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TKelvinId): TJoulePerKelvinId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TKilogramKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

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
operator /(const ALeft: TSquareMeterPerSquareSecondId; const ARight: TKelvinId): TJoulePerKilogramPerKelvinId;
begin end;

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
operator /(const ALeft: TJoulePerKelvinId; const ARight: TKilogramId): TJoulePerKilogramPerKelvinId;
begin end;

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
operator *(const ALeft: TMeterId; const ARight: TKelvinId): TMeterKelvinId;
begin end;

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
operator /(const ALeft: TKelvinId; const ARight: TMeterId): TKelvinPerMeterId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TMeterId): TWattPerMeterId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TSquareMeterId): TWattPerSquareMeterId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TKelvinId): TWattPerKelvinId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TMeterKelvinId): TWattPerMeterPerKelvinId;
begin end;

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
operator /(const ALeft: TWattPerMeterId; const ARight: TKelvinId): TWattPerMeterPerKelvinId;
begin end;

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
operator /(const ALeft: TWattPerKelvinId; const ARight: TMeterId): TWattPerMeterPerKelvinId;
begin end;

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
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinPerMeterId): TWattPerMeterPerKelvinId;
begin end;

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
operator *(const ALeft: TSquareMeterId; const ARight: TKelvinId): TSquareMeterKelvinId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TSquareMeterKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

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
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TKelvinId): TWattPerSquareMeterPerKelvinId;
begin end;

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
operator /(const ALeft: TWattPerKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerKelvinId;
begin end;

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
operator *(const ALeft: TSquareMeterId; const ARight: TQuarticKelvinId): TSquareMeterQuarticKelvinId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TQuarticKelvinId): TWattPerQuarticKelvinId;
begin end;

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
operator /(const ALeft: TWattId; const ARight: TSquareMeterQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

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
operator /(const ALeft: TWattPerSquareMeterId; const ARight: TQuarticKelvinId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

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
operator /(const ALeft: TWattPerQuarticKelvinId; const ARight: TSquareMeterId): TWattPerSquareMeterPerQuarticKelvinId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TMoleId): TJoulePerMoleId;
begin end;

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
operator *(const ALeft: TMoleId; const ARight: TKelvinId): TMoleKelvinId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TMoleKelvinId): TJoulePerMolePerKelvinId;
begin end;

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
operator /(const ALeft: TJoulePerKelvinId; const ARight: TMoleId): TJoulePerMolePerKelvinId;
begin end;

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
operator /(const ALeft: TJoulePerMoleId; const ARight: TKelvinId): TJoulePerMolePerKelvinId;
begin end;

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
operator *(const ALeft: TOhmId; const ARight: TMeterId): TOhmMeterId;
begin end;

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
operator /(const ALeft: TVoltId; const ARight: TMeterId): TVoltPerMeterId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TCoulombId): TVoltPerMeterId;
begin end;

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
operator *(const ALeft: TTeslaId; const ARight: TMeterPerSecondId): TVoltPerMeterId;
begin end;

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
operator /(const ALeft: TCoulombId; const ARight: TMeterId): TCoulombPerMeterId;
begin end;

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
operator /(const ALeft: TSquareCoulombId; const ARight: TMeterId): TSquareCoulombPerMeterId;
begin end;

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
operator *(const ALeft: TCoulombPerMeterId; const ARight: TCoulombId): TSquareCoulombPerMeterId;
begin end;

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
operator /(const ALeft: TCoulombId; const ARight: TSquareMeterId): TCoulombPerSquareMeterId;
begin end;

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
operator /(const ALeft: TCoulombPerMeterId; const ARight: TMeterId): TCoulombPerSquareMeterId;
begin end;

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
operator /(const ALeft: TSquareMeterId; const ARight: TSquareCoulombId): TSquareMeterPerSquareCoulombId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TSquareCoulombId): TNewtonPerSquareCoulombId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterId): TNewtonSquareMeterId;
begin end;

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
operator *(const ALeft: TNewtonId; const ARight: TSquareMeterPerSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

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
operator /(const ALeft: TNewtonSquareMeterId; const ARight: TSquareCoulombId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

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
operator *(const ALeft: TNewtonPerSquareCoulombId; const ARight: TSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

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
operator /(const ALeft: TVoltPerMeterId; const ARight: TCoulombPerSquareMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

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
operator /(const ALeft: TJouleId; const ARight: TSquareCoulombPerMeterId): TNewtonSquareMeterPerSquareCoulombId;
begin end;

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
operator *(const ALeft: TVoltId; const ARight: TMeterId): TVoltMeterId;
begin end;

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
operator *(const ALeft: TVoltPerMeterId; const ARight: TSquareMeterId): TVoltMeterId;
begin end;

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
operator /(const ALeft: TVoltMeterId; const ARight: TSecondId): TVoltMeterPerSecondId;
begin end;

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
operator /(const ALeft: TFaradId; const ARight: TMeterId): TFaradPerMeterId;
begin end;

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
operator /(const ALeft: TCoulombId; const ARight: TVoltMeterId): TFaradPerMeterId;
begin end;

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
operator /(const ALeft: TCoulombPerSquareMeterId; const ARight: TVoltPerMeterId): TFaradPerMeterId;
begin end;

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
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombId): TFaradPerMeterId;
begin end;

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
operator /(const ALeft: TAmpereId; const ARight: TMeterId): TAmperePerMeterId;
begin end;

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
operator /(const ALeft: TMeterId; const ARight: TAmpereId): TMeterPerAmpereId;
begin end;

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
operator *(const ALeft: TTeslaId; const ARight: TMeterId): TTeslaMeterId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TAmpereId): TTeslaMeterId;
begin end;

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
operator /(const ALeft: TTeslaId; const ARight: TAmpereId): TTeslaPerAmpereId;
begin end;

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
operator /(const ALeft: THenryId; const ARight: TMeterId): THenryPerMeterId;
begin end;

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
operator /(const ALeft: TTeslaMeterId; const ARight: TAmpereId): THenryPerMeterId;
begin end;

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
operator *(const ALeft: TTeslaPerAmpereId; const ARight: TMeterId): THenryPerMeterId;
begin end;

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
operator *(const ALeft: TTeslaId; const ARight: TMeterPerAmpereId): THenryPerMeterId;
begin end;

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
operator /(const ALeft: TTeslaId; const ARight: TAmperePerMeterId): THenryPerMeterId;
begin end;

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
operator /(const ALeft: TNewtonId; const ARight: TSquareAmpereId): THenryPerMeterId;
begin end;

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
operator /(const ALeft: TRadianId; const ARight: TMeterId): TRadianPerMeterId;
begin end;

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
