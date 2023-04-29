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

{ Unit of AmperePerMeter }

type
  TAmperePerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TAmperePerMeterIdentifier = specialize TQuantityIdentifier<TAmperePerMeterUnit>;
  TAmperesPerMeter = specialize TQuantity<TAmperePerMeterUnit>;

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

{ Unit of CoulombPerMeter }

type
  TCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerMeterIdentifier = specialize TQuantityIdentifier<TCoulombPerMeterUnit>;
  TCoulombsPerMeter = specialize TQuantity<TCoulombPerMeterUnit>;

{ Unit of CoulombPerSquareMeter }

type
  TCoulombPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCoulombPerSquareMeterIdentifier = specialize TQuantityIdentifier<TCoulombPerSquareMeterUnit>;
  TCoulombsPerSquareMeter = specialize TQuantity<TCoulombPerSquareMeterUnit>;

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

{ Unit of CubicMeterPerSecond }

type
  TCubicMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TCubicMeterPerSecondIdentifier = specialize TQuantityIdentifier<TCubicMeterPerSecondUnit>;
  TCubicMetersPerSecond = specialize TQuantity<TCubicMeterPerSecondUnit>;

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

{ Unit of JoulePerMole }

type
  TJoulePerMoleUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMoleIdentifier = specialize TQuantityIdentifier<TJoulePerMoleUnit>;
  TJoulesPerMole = specialize TQuantity<TJoulePerMoleUnit>;

{ Unit of JoulePerMolePerKelvin }

type
  TJoulePerMolePerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TJoulePerMolePerKelvinIdentifier = specialize TQuantityIdentifier<TJoulePerMolePerKelvinUnit>;
  TJoulesPerMolePerKelvin = specialize TQuantity<TJoulePerMolePerKelvinUnit>;

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

{ Unit of KelvinPerMeter }

type
  TKelvinPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKelvinPerMeterIdentifier = specialize TQuantityIdentifier<TKelvinPerMeterUnit>;
  TKelvinsPerMeter = specialize TQuantity<TKelvinPerMeterUnit>;

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

{ Unit of KilogramKelvin }

type
  TKilogramKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramKelvinIdentifier = specialize TQuantityIdentifier<TKilogramKelvinUnit>;
  TKilogramsKelvin = specialize TQuantity<TKilogramKelvinUnit>;

{ Unit of KilogramMeterPerSecond }

type
  TKilogramMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramMeterPerSecondIdentifier = specialize TQuantityIdentifier<TKilogramMeterPerSecondUnit>;
  TKilogramsMeterPerSecond = specialize TQuantity<TKilogramMeterPerSecondUnit>;

{ Unit of KilogramPerCubicMeter }

type
  TKilogramPerCubicMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramPerCubicMeterIdentifier = specialize TQuantityIdentifier<TKilogramPerCubicMeterUnit>;
  TKilogramsPerCubicMeter = specialize TQuantity<TKilogramPerCubicMeterUnit>;

{ Unit of KilogramSquareMeter }

type
  TKilogramSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterIdentifier = specialize TQuantityIdentifier<TKilogramSquareMeterUnit>;
  TKilogramsSquareMeter = specialize TQuantity<TKilogramSquareMeterUnit>;

{ Unit of KilogramSquareMeterPerSecond }

type
  TKilogramSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TKilogramSquareMeterPerSecondIdentifier = specialize TQuantityIdentifier<TKilogramSquareMeterPerSecondUnit>;
  TKilogramsSquareMeterPerSecond = specialize TQuantity<TKilogramSquareMeterPerSecondUnit>;

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

{ Unit of MeterKelvin }

type
  TMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterKelvinIdentifier = specialize TQuantityIdentifier<TMeterKelvinUnit>;
  TMetersKelvin = specialize TQuantity<TMeterKelvinUnit>;

{ Unit of MeterPerAmpere }

type
  TMeterPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMeterPerAmpereIdentifier = specialize TQuantityIdentifier<TMeterPerAmpereUnit>;
  TMetersPerAmpere = specialize TQuantity<TMeterPerAmpereUnit>;

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

{ Unit of MoleKelvin }

type
  TMoleKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TMoleKelvinIdentifier = specialize TQuantityIdentifier<TMoleKelvinUnit>;
  TMolesKelvin = specialize TQuantity<TMoleKelvinUnit>;

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

{ Unit of NewtonMeter }

type
  TNewtonMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterIdentifier = specialize TQuantityIdentifier<TNewtonMeterUnit>;
  TNewtonsMeter = specialize TQuantity<TNewtonMeterUnit>;

var
  Nm: TNewtonMeterIdentifier;

{ Unit of NewtonMeterPerRadian }

type
  TNewtonMeterPerRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonMeterPerRadianIdentifier = specialize TQuantityIdentifier<TNewtonMeterPerRadianUnit>;
  TNewtonsMeterPerRadian = specialize TQuantity<TNewtonMeterPerRadianUnit>;

{ Unit of NewtonPerAmpere }

type
  TNewtonPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerAmpereIdentifier = specialize TQuantityIdentifier<TNewtonPerAmpereUnit>;
  TNewtonsPerAmpere = specialize TQuantity<TNewtonPerAmpereUnit>;

{ Unit of NewtonPerCoulomb }

type
  TNewtonPerCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerCoulombIdentifier = specialize TQuantityIdentifier<TNewtonPerCoulombUnit>;
  TNewtonsPerCoulomb = specialize TQuantity<TNewtonPerCoulombUnit>;

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

{ Unit of NewtonPerSquareCoulomb }

type
  TNewtonPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TNewtonPerSquareCoulombUnit>;
  TNewtonsPerSquareCoulomb = specialize TQuantity<TNewtonPerSquareCoulombUnit>;

{ Unit of NewtonPerSquareKilogram }

type
  TNewtonPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TNewtonPerSquareKilogramUnit>;
  TNewtonsPerSquareKilogram = specialize TQuantity<TNewtonPerSquareKilogramUnit>;

{ Unit of NewtonSquareMeter }

type
  TNewtonSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterUnit>;
  TNewtonsSquareMeter = specialize TQuantity<TNewtonSquareMeterUnit>;

{ Unit of NewtonSquareMeterPerCoulomb }

type
  TNewtonSquareMeterPerCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerCoulombIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterPerCoulombUnit>;
  TNewtonsSquareMeterPerCoulomb = specialize TQuantity<TNewtonSquareMeterPerCoulombUnit>;

{ Unit of NewtonSquareMeterPerSquareCoulomb }

type
  TNewtonSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterPerSquareCoulombUnit>;
  TNewtonsSquareMeterPerSquareCoulomb = specialize TQuantity<TNewtonSquareMeterPerSquareCoulombUnit>;

{ Unit of NewtonSquareMeterPerSquareKilogram }

type
  TNewtonSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSquareMeterPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TNewtonSquareMeterPerSquareKilogramUnit>;
  TNewtonsSquareMeterPerSquareKilogram = specialize TQuantity<TNewtonSquareMeterPerSquareKilogramUnit>;

{ Unit of NewtonSecond }

type
  TNewtonSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TNewtonSecondIdentifier = specialize TQuantityIdentifier<TNewtonSecondUnit>;
  TNewtonsSecond = specialize TQuantity<TNewtonSecondUnit>;

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

{ Unit of OhmMeter }

type
  TOhmMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TOhmMeterIdentifier = specialize TQuantityIdentifier<TOhmMeterUnit>;
  TOhmsMeter = specialize TQuantity<TOhmMeterUnit>;

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

{ Unit of PascalSecond }

type
  TPascalSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TPascalSecondIdentifier = specialize TQuantityIdentifier<TPascalSecondUnit>;
  TPascalsSecond = specialize TQuantity<TPascalSecondUnit>;

{ Unit of QuarticKelvin }

type
  TQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TQuarticKelvinIdentifier = specialize TQuantityIdentifier<TQuarticKelvinUnit>;
  TQuarticKelvins = specialize TQuantity<TQuarticKelvinUnit>;

var
  K4: TQuarticKelvinIdentifier;

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

{ Unit of ReciprocalKelvin }

type
  TReciprocalKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TReciprocalKelvinIdentifier = specialize TQuantityIdentifier<TReciprocalKelvinUnit>;
  TReciprocalKelvins = specialize TQuantity<TReciprocalKelvinUnit>;

{ Unit of ReciprocalMeter }

type
  TReciprocalMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TReciprocalMeterIdentifier = specialize TQuantityIdentifier<TReciprocalMeterUnit>;
  TReciprocalMeters = specialize TQuantity<TReciprocalMeterUnit>;

{ Unit of ReciprocalSquareMeter }

type
  TReciprocalSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TReciprocalSquareMeterIdentifier = specialize TQuantityIdentifier<TReciprocalSquareMeterUnit>;
  TReciprocalSquareMeters = specialize TQuantity<TReciprocalSquareMeterUnit>;

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

{ Unit of SquareAmperePerMeter }

type
  TSquareAmperePerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareAmperePerMeterIdentifier = specialize TQuantityIdentifier<TSquareAmperePerMeterUnit>;
  TSquareAmperesPerMeter = specialize TQuantity<TSquareAmperePerMeterUnit>;

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

{ Unit of SquareCoulombPerMeter }

type
  TSquareCoulombPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerMeterIdentifier = specialize TQuantityIdentifier<TSquareCoulombPerMeterUnit>;
  TSquareCoulombsPerMeter = specialize TQuantity<TSquareCoulombPerMeterUnit>;

{ Unit of SquareCoulombPerSquareMeter }

type
  TSquareCoulombPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerSquareMeterIdentifier = specialize TQuantityIdentifier<TSquareCoulombPerSquareMeterUnit>;
  TSquareCoulombsPerSquareMeter = specialize TQuantity<TSquareCoulombPerSquareMeterUnit>;

{ Unit of SquareCoulombPerNewton }

type
  TSquareCoulombPerNewtonUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerNewtonIdentifier = specialize TQuantityIdentifier<TSquareCoulombPerNewtonUnit>;
  TSquareCoulombsPerNewton = specialize TQuantity<TSquareCoulombPerNewtonUnit>;

{ Unit of SquareCoulombPerNewtonPerSquareMeter }

type
  TSquareCoulombPerNewtonPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareCoulombPerNewtonPerSquareMeterIdentifier = specialize TQuantityIdentifier<TSquareCoulombPerNewtonPerSquareMeterUnit>;
  TSquareCoulombsPerNewtonPerSquareMeter = specialize TQuantity<TSquareCoulombPerNewtonPerSquareMeterUnit>;

{ Unit of SquareHertz }

type
  TSquareHertzUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareHertzIdentifier = specialize TQuantityIdentifier<TSquareHertzUnit>;
  TSquareHertz = specialize TQuantity<TSquareHertzUnit>;

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

{ Unit of SquareMeterKelvin }

type
  TSquareMeterKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterKelvinIdentifier = specialize TQuantityIdentifier<TSquareMeterKelvinUnit>;
  TSquareMetersKelvin = specialize TQuantity<TSquareMeterKelvinUnit>;

{ Unit of SquareMeterPerCoulomb }

type
  TSquareMeterPerCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerCoulombIdentifier = specialize TQuantityIdentifier<TSquareMeterPerCoulombUnit>;
  TSquareMetersPerCoulomb = specialize TQuantity<TSquareMeterPerCoulombUnit>;

{ Unit of SquareMeterPerSquareCoulomb }

type
  TSquareMeterPerSquareCoulombUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareCoulombIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareCoulombUnit>;
  TSquareMetersPerSquareCoulomb = specialize TQuantity<TSquareMeterPerSquareCoulombUnit>;

{ Unit of SquareMeterPerSquareKilogram }

type
  TSquareMeterPerSquareKilogramUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareKilogramIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareKilogramUnit>;
  TSquareMetersPerSquareKilogram = specialize TQuantity<TSquareMeterPerSquareKilogramUnit>;

{ Unit of SquareMeterPerSecond }

type
  TSquareMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSecondIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSecondUnit>;
  TSquareMetersPerSecond = specialize TQuantity<TSquareMeterPerSecondUnit>;

{ Unit of SquareMeterPerSquareSecond }

type
  TSquareMeterPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterPerSquareSecondIdentifier = specialize TQuantityIdentifier<TSquareMeterPerSquareSecondUnit>;
  TSquareMetersPerSquareSecond = specialize TQuantity<TSquareMeterPerSquareSecondUnit>;

{ Unit of SquareMeterQuarticKelvin }

type
  TSquareMeterQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareMeterQuarticKelvinIdentifier = specialize TQuantityIdentifier<TSquareMeterQuarticKelvinUnit>;
  TSquareMetersQuarticKelvin = specialize TQuantity<TSquareMeterQuarticKelvinUnit>;

{ Unit of SquareRadian }

type
  TSquareRadianUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareRadianIdentifier = specialize TQuantityIdentifier<TSquareRadianUnit>;
  TSquareRadians = specialize TQuantity<TSquareRadianUnit>;

var
  rad2: TSquareRadianIdentifier;

{ Unit of SquareRadianPerSquareSecond }

type
  TSquareRadianPerSquareSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TSquareRadianPerSquareSecondIdentifier = specialize TQuantityIdentifier<TSquareRadianPerSquareSecondUnit>;
  TSquareRadiansPerSquareSecond = specialize TQuantity<TSquareRadianPerSquareSecondUnit>;

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

{ Unit of TeslaMeter }

type
  TTeslaMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaMeterIdentifier = specialize TQuantityIdentifier<TTeslaMeterUnit>;
  TTeslasMeter = specialize TQuantity<TTeslaMeterUnit>;

var
  Tm: TTeslaMeterIdentifier;

{ Unit of TeslaMeterPerAmpere }

type
  TTeslaMeterPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaMeterPerAmpereIdentifier = specialize TQuantityIdentifier<TTeslaMeterPerAmpereUnit>;
  TTeslasMeterPerAmpere = specialize TQuantity<TTeslaMeterPerAmpereUnit>;

{ Unit of TeslaPerAmpere }

type
  TTeslaPerAmpereUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TTeslaPerAmpereIdentifier = specialize TQuantityIdentifier<TTeslaPerAmpereUnit>;
  TTeslasPerAmpere = specialize TQuantity<TTeslaPerAmpereUnit>;

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

{ Unit of VoltMeter }

type
  TVoltMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterIdentifier = specialize TQuantityIdentifier<TVoltMeterUnit>;
  TVoltsMeter = specialize TQuantity<TVoltMeterUnit>;

var
  Vm: TVoltMeterIdentifier;

{ Unit of VoltMeterPerSecond }

type
  TVoltMeterPerSecondUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltMeterPerSecondIdentifier = specialize TQuantityIdentifier<TVoltMeterPerSecondUnit>;
  TVoltsMeterPerSecond = specialize TQuantity<TVoltMeterPerSecondUnit>;

{ Unit of VoltPerMeter }

type
  TVoltPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TVoltPerMeterIdentifier = specialize TQuantityIdentifier<TVoltPerMeterUnit>;
  TVoltsPerMeter = specialize TQuantity<TVoltPerMeterUnit>;

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

{ Unit of WattPerKelvin }

type
  TWattPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerKelvinUnit>;
  TWattsPerKelvin = specialize TQuantity<TWattPerKelvinUnit>;

{ Unit of WattPerMeter }

type
  TWattPerMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterIdentifier = specialize TQuantityIdentifier<TWattPerMeterUnit>;
  TWattsPerMeter = specialize TQuantity<TWattPerMeterUnit>;

{ Unit of WattPerMeterPerKelvin }

type
  TWattPerMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerMeterPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerMeterPerKelvinUnit>;
  TWattsPerMeterPerKelvin = specialize TQuantity<TWattPerMeterPerKelvinUnit>;

{ Unit of WattPerQuarticKelvin }

type
  TWattPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerQuarticKelvinIdentifier = specialize TQuantityIdentifier<TWattPerQuarticKelvinUnit>;
  TWattsPerQuarticKelvin = specialize TQuantity<TWattPerQuarticKelvinUnit>;

{ Unit of WattPerSquareMeter }

type
  TWattPerSquareMeterUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterUnit>;
  TWattsPerSquareMeter = specialize TQuantity<TWattPerSquareMeterUnit>;

{ Unit of WattPerSquareMeterPerKelvin }

type
  TWattPerSquareMeterPerKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerKelvinIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterPerKelvinUnit>;
  TWattsPerSquareMeterPerKelvin = specialize TQuantity<TWattPerSquareMeterPerKelvinUnit>;

{ Unit of WattPerSquareMeterPerQuarticKelvin }

type
  TWattPerSquareMeterPerQuarticKelvinUnit = class(TUnit)
    class function Name: string; override;
    class function Symbol: string; override;
  end;
  TWattPerSquareMeterPerQuarticKelvinIdentifier = specialize TQuantityIdentifier<TWattPerSquareMeterPerQuarticKelvinUnit>;
  TWattsPerSquareMeterPerQuarticKelvin = specialize TQuantity<TWattPerSquareMeterPerQuarticKelvinUnit>;

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
  nanometer: TNanometerIdentifier;

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

{ Unit of NewtonMillimeter }

type
  TNewtonMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TNewtonMeterUnit, TNewtonMillimeterUnit>;

var
  Nmm: TNewtonMillimeterIdentifier;

{ Unit of NewtonMeterPerDegree }

type
  TNewtonMeterPerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMeterPerDegreeIdentifier = specialize TFactoredQuantityIdentifier<TNewtonMeterPerRadianUnit, TNewtonMeterPerDegreeUnit>;

{ Unit of NewtonMillimeterPerRadian }

type
  TNewtonMillimeterPerRadianUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMillimeterPerRadianIdentifier = specialize TFactoredQuantityIdentifier<TNewtonMeterPerRadianUnit, TNewtonMillimeterPerRadianUnit>;

{ Unit of NewtonMillimeterPerDegree }

type
  TNewtonMillimeterPerDegreeUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonMillimeterPerDegreeIdentifier = specialize TFactoredQuantityIdentifier<TNewtonMeterPerRadianUnit, TNewtonMillimeterPerDegreeUnit>;

{ Unit of NewtonPerDecimeter }

type
  TNewtonPerDecimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonPerDecimeterIdentifier = specialize TFactoredQuantityIdentifier<TNewtonPerMeterUnit, TNewtonPerDecimeterUnit>;

{ Unit of NewtonPerCentimeter }

type
  TNewtonPerCentimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonPerCentimeterIdentifier = specialize TFactoredQuantityIdentifier<TNewtonPerMeterUnit, TNewtonPerCentimeterUnit>;

{ Unit of NewtonPerMillimeter }

type
  TNewtonPerMillimeterUnit = class(TFactoredUnit)
    class function Name: string; override;
    class function Symbol: string; override;
    class function Factor: double; override;
  end;
  TNewtonPerMillimeterIdentifier = specialize TFactoredQuantityIdentifier<TNewtonPerMeterUnit, TNewtonPerMillimeterUnit>;

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

{ Combining units }

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): TAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TAmpereIdentifier; inline;

// alternative definition [ A/m ] = [ A ] * [ 1/m ]
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TReciprocalMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TAmpereIdentifier; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TAmpereIdentifier): TCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TSecondIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TSecondIdentifier): TCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TAmpereIdentifier): TSecondIdentifier; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier; inline;

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

// alternative definition [ C/m2 ] = [ C ] * [ 1/m2 ]
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TReciprocalSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TReciprocalSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalSquareMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TReciprocalSquareMeterIdentifier): TCoulombIdentifier; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TCubicKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TCubicKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareKelvinIdentifier; inline;

// main definition [ m3 ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareMeterIdentifier; inline;

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

// main definition [ Hz ] = 1 / [ s ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSecondIdentifier): THertzIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: THertzIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TSecondIdentifier): double; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: THertzIdentifier): double; inline;

// main definition [ J ] = [ Pa ] * [ m3 ]
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TPascalIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TPascalIdentifier; inline;

// alternative definition [ J ] = [ kg*m2 ] * [ rad2/s2 ]
operator *(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TSquareRadianPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareRadianPerSquareSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TJouleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TKilogramSquareMeterIdentifier; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TJoulePerKelvinIdentifier): TJouleIdentifier; inline;

// main definition [ J/kg ] = [ J ] / [ kg ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramIdentifier): TJoulePerKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerKilogramIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramIdentifier; const {%H-}ARight: TKilogramIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TJoulePerKilogramIdentifier): TJouleIdentifier; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TKilogramKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKilogramKelvinIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TJouleIdentifier; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const {%H-}ALeft: TJoulePerKilogramIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJoulePerKilogramIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TJoulePerKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TJoulePerKilogramIdentifier; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TJoulePerKilogramPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TJoulePerKelvinIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerKilogramPerKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TJoulePerKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TJoulePerKilogramPerKelvinIdentifier): TJoulePerKelvinIdentifier; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TMoleIdentifier): TJoulePerMoleIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TJoulePerMoleIdentifier): TMoleIdentifier; inline;
operator *(const {%H-}ALeft: TJoulePerMoleIdentifier; const {%H-}ARight: TMoleIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TJoulePerMoleIdentifier): TJouleIdentifier; inline;

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

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TSecondIdentifier): TKatalIdentifier; inline;
operator /(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TKatalIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKatalIdentifier; const {%H-}ARight: TSecondIdentifier): TMoleIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TKatalIdentifier): TMoleIdentifier; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TKelvinPerMeterIdentifier): TKelvinIdentifier; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKelvinIdentifier): TKilogramKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TKilogramIdentifier; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TKilogramIdentifier; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TKilogramPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TKilogramIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKilogramIdentifier; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: THertzIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): THertzIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TKilogramSquareMeterIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: THertzIdentifier): TKilogramSquareMeterIdentifier; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TKilogramSquareMeterIdentifier; const {%H-}ARight: TKilogramSquareMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TKilogramSquareMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TKilogramSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TKilogramSquareMeterPerSecondIdentifier): TKilogramSquareMeterIdentifier; inline;

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

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMeterKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMeterKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TMeterIdentifier; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TMeterIdentifier; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TMeterIdentifier; inline;

// alternative definition [ m/s ] = [ m ] * [ hz ]
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: THertzIdentifier): TMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TMeterIdentifier): THertzIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: THertzIdentifier): TMeterIdentifier; inline;

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
operator *(const {%H-}ALeft: TSquareRadianPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareRadianPerSquareSecondIdentifier; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const {%H-}ALeft: TMoleIdentifier; const {%H-}ARight: TKelvinIdentifier): TMoleKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMoleKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TMoleIdentifier): TMoleKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TMoleKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TMoleIdentifier; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TNewtonIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TKilogramIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramIdentifier): TNewtonIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TKilogramIdentifier; inline;

// main definition [ Nm ] = [ N ] * [ m ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m/rad ] = [ N*m ] / [ rad ]
operator /(const {%H-}ALeft: TNewtonMeterIdentifier; const {%H-}ARight: TRadianIdentifier): TNewtonMeterPerRadianIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonMeterIdentifier; const {%H-}ARight: TNewtonMeterPerRadianIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonMeterPerRadianIdentifier; const {%H-}ARight: TRadianIdentifier): TNewtonMeterIdentifier; inline;
operator *(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TNewtonMeterPerRadianIdentifier): TNewtonMeterIdentifier; inline;

// main definition [ N/A ] = [ N ] / [ A ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TAmpereIdentifier): TNewtonPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TNewtonPerAmpereIdentifier): TNewtonIdentifier; inline;

// main definition [ N/C ] = [ N ] / [ C ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TCoulombIdentifier): TNewtonPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N/C ] = [ m/s ] * [ T ]
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TTeslaIdentifier): TNewtonPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TTeslaIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TNewtonPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterPerSecondIdentifier; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonPerCubicMeterIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCubicMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TPascalIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TNewtonPerCubicMeterIdentifier): TPascalIdentifier; inline;

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

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TNewtonPerSquareCoulombIdentifier): TNewtonIdentifier; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TNewtonPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonIdentifier; inline;

// main definition [ N*m2/C ] = [ N ] * [ m2/C ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterPerCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerCoulombIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterPerCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerCoulombIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerCoulombIdentifier; const {%H-}ARight: TSquareMeterPerCoulombIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N*m2/C ] = [ N*m2 ] / [ C ]
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerCoulombIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TNewtonSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TNewtonSquareMeterPerCoulombIdentifier): TNewtonSquareMeterIdentifier; inline;

// alternative definition [ N*m2/C ] = [ N/C ] * [ m2 ]
operator *(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerCoulombIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonPerCoulombIdentifier; inline;

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

// alternative definition [ N*m2/C2 ] = [ N ] / [ C2/m2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ N/C ] / [ C/m2 ]
operator /(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TCoulombPerSquareMeterIdentifier): TNewtonPerCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonPerCoulombIdentifier; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TJouleIdentifier; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareMeterPerSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

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

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramPerMeterIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramPerMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TJouleIdentifier; inline;

// main definition [ N*s ] = [ N ] * [ s ]
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSecondIdentifier): TNewtonSecondIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSecondIdentifier; const {%H-}ARight: TNewtonIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TNewtonIdentifier): TNewtonSecondIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TNewtonIdentifier; inline;

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

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TMeterIdentifier): TOhmMeterIdentifier; inline;
operator /(const {%H-}ALeft: TOhmMeterIdentifier; const {%H-}ARight: TOhmIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TOhmIdentifier): TOhmMeterIdentifier; inline;
operator /(const {%H-}ALeft: TOhmMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TOhmIdentifier; inline;

// alternative definition [ Ω*m ] = [ Ω ] / [ 1/m ]
operator /(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TOhmMeterIdentifier; inline;
operator /(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TOhmMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator *(const {%H-}ALeft: TOhmMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TOhmMeterIdentifier): TOhmIdentifier; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TPascalIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TPascalIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TNewtonIdentifier; inline;

// alternative definition [ Pa ] = [ N/m ] / [ m ]
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TPascalIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TMeterIdentifier): TNewtonPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TPascalIdentifier): TNewtonPerMeterIdentifier; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const {%H-}ALeft: TKilogramPerCubicMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TPascalIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TKilogramPerCubicMeterIdentifier): TPascalIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TSecondIdentifier): TPascalSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TPascalIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TPascalIdentifier): TPascalSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TPascalIdentifier; inline;

// alternative definition [ Pa*s ] = [ Pa ] / [ Hz ]
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: THertzIdentifier): TPascalSecondIdentifier; inline;
operator /(const {%H-}ALeft: TPascalIdentifier; const {%H-}ARight: TPascalSecondIdentifier): THertzIdentifier; inline;
operator *(const {%H-}ALeft: TPascalSecondIdentifier; const {%H-}ARight: THertzIdentifier): TPascalIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TPascalSecondIdentifier): TPascalIdentifier; inline;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const {%H-}ALeft: TCubicKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TCubicKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TCubicKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TCubicKelvinIdentifier; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TSquareKelvinIdentifier): TSquareKelvinIdentifier; inline;

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

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianPerSecondIdentifier; inline;
operator /(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TRadianIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TRadianIdentifier; inline;

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

// main definition [ 1/K ] = 1 / [ K ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TKelvinIdentifier): TReciprocalKelvinIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TReciprocalKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): double; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TReciprocalKelvinIdentifier): double; inline;

// main definition [ 1/m ] = 1 / [ m ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TReciprocalMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TMeterIdentifier): double; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): double; inline;

// alternative definition [ 1/m ] = [ m ] / [ m2 ]
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TMeterIdentifier; inline;

// alternative definition [ 1/m ] = [ m2 ] / [ m3 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TSquareMeterIdentifier; inline;

// alternative definition [ 1/m ] = [ m3 ] / [ m4 ]
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TQuarticMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TQuarticMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TCubicMeterIdentifier; inline;

// alternative definition [ 1/m ] = [ m4 ] / [ m5 ]
operator /(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TQuinticMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuarticMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TQuinticMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TQuinticMeterIdentifier): TQuarticMeterIdentifier; inline;
operator *(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TQuarticMeterIdentifier; inline;

// alternative definition [ 1/m ] = [ m5 ] / [ m6 ]
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TSexticMeterIdentifier): TReciprocalMeterIdentifier; inline;
operator /(const {%H-}ALeft: TQuinticMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TSexticMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalMeterIdentifier; const {%H-}ARight: TSexticMeterIdentifier): TQuinticMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSexticMeterIdentifier; const {%H-}ARight: TReciprocalMeterIdentifier): TQuinticMeterIdentifier; inline;

// main definition [ 1/m2 ] = [ 1 ] / [ m2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSquareMeterIdentifier): TReciprocalSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TReciprocalSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): double; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TReciprocalSquareMeterIdentifier): double; inline;

//
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TReciprocalSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TReciprocalSquareMeterIdentifier): TCubicMeterIdentifier; inline;
operator *(const {%H-}ALeft: TReciprocalSquareMeterIdentifier; const {%H-}ARight: TCubicMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCubicMeterIdentifier; const {%H-}ARight: TReciprocalSquareMeterIdentifier): TMeterIdentifier; inline;

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

// main definition [ S ] = 1 / [ Ω ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TOhmIdentifier): TSiemensIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSiemensIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TSiemensIdentifier; const {%H-}ARight: TOhmIdentifier): double; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TSiemensIdentifier): double; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TSquareAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TAmpereIdentifier; inline;

// main definition [ A2/m ] = [ A2] / [ m ]
operator /(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareAmpereIdentifier; const {%H-}ARight: TSquareAmperePerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareAmperePerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TSquareAmperePerMeterIdentifier): TSquareAmpereIdentifier; inline;

// alternative definition [ A2/m ] = [ A/m ] * [ A ]
operator *(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TSquareAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareAmperePerMeterIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TSquareAmperePerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareAmperePerMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TAmperePerMeterIdentifier; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TCoulombIdentifier; inline;

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

// main definition [ C2/m2 ] = [ C2 ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareCoulombPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombPerSquareMeterIdentifier): TSquareCoulombIdentifier; inline;

// main definition [ C2/N ] = [ C2 ] / [ N ]
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareCoulombPerNewtonIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonIdentifier): TSquareCoulombIdentifier; inline;

// main definition [ C2/N/m2 ] = [ C2 ] / [N*m2 ]
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TNewtonSquareMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombIdentifier; inline;

// alternative definition [ C2/N/m2 ] = [ C2/N ] / [ m2 ]
operator /(const {%H-}ALeft: TSquareCoulombPerNewtonIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerNewtonIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareCoulombPerNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombPerNewtonIdentifier; inline;

// alternative definition [ C2/N/m2 ] = [ C2/m2 ] / [ N ]
operator /(const {%H-}ALeft: TSquareCoulombPerSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareCoulombPerSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TNewtonIdentifier): TSquareCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombPerSquareMeterIdentifier; inline;

// alternative definition [ C2/N/m2 ] = [ C/m2 ] / [ N/C ]
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombPerSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonPerCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TNewtonPerCoulombIdentifier): TCoulombPerSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TNewtonPerCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier; inline;

// alternative definition [ C2/N/m2 ] = [ A ] / [ V*m/s ]
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TVoltMeterPerSecondIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TVoltMeterPerSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TVoltMeterPerSecondIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TVoltMeterPerSecondIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TAmpereIdentifier; inline;

// alternative definition [ C2/N/m2 ] = [ C ] / [ V*m ]
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TVoltMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TVoltMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TVoltMeterIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TVoltMeterIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TCoulombIdentifier; inline;

// alternative definition [ C2/N/m2 ] = 1 / [ N*m2/C2 ]
operator /(const {%H-}ALeft: double; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: double; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const {%H-}ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): double; inline;
operator *(const {%H-}ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: THertzIdentifier): TSquareHertzIdentifier; inline;
operator /(const {%H-}ALeft: TSquareHertzIdentifier; const {%H-}ARight: THertzIdentifier): THertzIdentifier; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TKelvinIdentifier; inline;

// main definition [ kg2 ]
operator *(const {%H-}ALeft: TKilogramIdentifier; const {%H-}ARight: TKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TKilogramIdentifier): TKilogramIdentifier; inline;

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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TMeterIdentifier; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareMeterKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TSquareMeterIdentifier; inline;

// main definition [ m2/C ] = [ m2 ] / [ C ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TCoulombIdentifier): TSquareMeterPerCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerCoulombIdentifier): TCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerCoulombIdentifier; const {%H-}ARight: TCoulombIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TCoulombIdentifier; const {%H-}ARight: TSquareMeterPerCoulombIdentifier): TSquareMeterIdentifier; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TSquareMeterPerSquareCoulombIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareCoulombIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareCoulombIdentifier; const {%H-}ARight: TSquareCoulombIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareCoulombIdentifier; const {%H-}ARight: TSquareMeterPerSquareCoulombIdentifier): TSquareMeterIdentifier; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TSquareMeterPerSquareKilogramIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareKilogramIdentifier; const {%H-}ARight: TSquareKilogramIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareKilogramIdentifier; const {%H-}ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareMeterIdentifier; inline;

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

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSquareMeterPerSquareSecondIdentifier): TSquareMeterIdentifier; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const {%H-}ALeft: TMeterPerSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TSquareMeterPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterPerSquareSecondIdentifier; const {%H-}ARight: TMeterPerSecondIdentifier): TMeterPerSecondIdentifier; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TSquareMeterQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TSquareMeterQuarticKelvinIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TSquareMeterIdentifier; inline;

// main definition [ rad2 ] = [ rad ] * [ rad ]
operator *(const {%H-}ALeft: TRadianIdentifier; const {%H-}ARight: TRadianIdentifier): TSquareRadianIdentifier; inline;
operator /(const {%H-}ALeft: TSquareRadianIdentifier; const {%H-}ARight: TRadianIdentifier): TRadianIdentifier; inline;

// main definition [ rad2/s2 ] = [ rad2 ] / [ s2 ]
operator /(const {%H-}ALeft: TSquareRadianIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareRadianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareRadianIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TSquareSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSquareRadianPerSquareSecondIdentifier; const {%H-}ARight: TSquareSecondIdentifier): TSquareRadianIdentifier; inline;
operator *(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSquareRadianPerSquareSecondIdentifier): TSquareRadianIdentifier; inline;

// alternative definition [ rad2/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const {%H-}ALeft: TRadianPerSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TSquareRadianPerSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareRadianPerSquareSecondIdentifier; const {%H-}ARight: TRadianPerSecondIdentifier): TRadianPerSecondIdentifier; inline;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TSquareSecondIdentifier; inline;
operator /(const {%H-}ALeft: TSquareSecondIdentifier; const {%H-}ARight: TSecondIdentifier): TSecondIdentifier; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TVoltIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TVoltIdentifier): TVoltIdentifier; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TOhmIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TWattIdentifier): TOhmIdentifier; inline;
operator *(const {%H-}ALeft: TOhmIdentifier; const {%H-}ARight: TWattIdentifier): TSquareVoltIdentifier; inline;
operator /(const {%H-}ALeft: TSquareVoltIdentifier; const {%H-}ARight: TOhmIdentifier): TWattIdentifier; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TTeslaIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TTeslaIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWeberIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TWeberIdentifier; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TTeslaIdentifier): TTeslaMeterIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaIdentifier; inline;

// main definition [ T*m/A ] = [ T*m ] / [ A ]
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaMeterIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TTeslaMeterIdentifier; inline;

// alternative definition [ T*m/A ] = [ T/A ] * [ m ]
operator *(const {%H-}ALeft: TTeslaPerAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TMeterIdentifier): TTeslaPerAmpereIdentifier; inline;

// alternative definition [ T*m/A ] = [ T ] * [ m/A ]
operator *(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TTeslaIdentifier): TMeterPerAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TMeterPerAmpereIdentifier; const {%H-}ARight: TTeslaIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TMeterPerAmpereIdentifier): TTeslaIdentifier; inline;

// alternative definition [ T*m/A ] = [ T ] / [ A/m ]
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TAmperePerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TAmperePerMeterIdentifier): TTeslaIdentifier; inline;
operator *(const {%H-}ALeft: TAmperePerMeterIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TTeslaIdentifier; inline;

// alternative definition [ T*m/A ] = [ N/m ] / [ A2/m ]
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TSquareAmperePerMeterIdentifier): TTeslaMeterPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TNewtonPerMeterIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TSquareAmperePerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaMeterPerAmpereIdentifier; const {%H-}ARight: TSquareAmperePerMeterIdentifier): TNewtonPerMeterIdentifier; inline;
operator *(const {%H-}ALeft: TSquareAmperePerMeterIdentifier; const {%H-}ARight: TTeslaMeterPerAmpereIdentifier): TNewtonPerMeterIdentifier; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaPerAmpereIdentifier; inline;
operator /(const {%H-}ALeft: TTeslaIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TAmpereIdentifier; inline;
operator *(const {%H-}ALeft: TTeslaPerAmpereIdentifier; const {%H-}ARight: TAmpereIdentifier): TTeslaIdentifier; inline;
operator *(const {%H-}ALeft: TAmpereIdentifier; const {%H-}ARight: TTeslaPerAmpereIdentifier): TTeslaIdentifier; inline;

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

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TVoltPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TVoltIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TVoltPerMeterIdentifier): TVoltIdentifier; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TSecondIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: TWattIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSecondIdentifier): TJouleIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TWattIdentifier): TJouleIdentifier; inline;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const {%H-}ALeft: TJouleIdentifier; const {%H-}ARight: THertzIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TJouleIdentifier): THertzIdentifier; inline;
operator *(const {%H-}ALeft: THertzIdentifier; const {%H-}ARight: TJouleIdentifier): TWattIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: THertzIdentifier): TJouleIdentifier; inline;

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

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattPerKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerKelvinIdentifier): TKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerKelvinIdentifier; const {%H-}ARight: TKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TKelvinIdentifier; const {%H-}ARight: TWattPerKelvinIdentifier): TWattIdentifier; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TMeterIdentifier): TWattPerMeterIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerMeterIdentifier): TMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerMeterIdentifier; const {%H-}ARight: TMeterIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TMeterIdentifier; const {%H-}ARight: TWattPerMeterIdentifier): TWattIdentifier; inline;

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

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerQuarticKelvinIdentifier; const {%H-}ARight: TQuarticKelvinIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TQuarticKelvinIdentifier; const {%H-}ARight: TWattPerQuarticKelvinIdentifier): TWattIdentifier; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattPerSquareMeterIdentifier; inline;
operator /(const {%H-}ALeft: TWattIdentifier; const {%H-}ARight: TWattPerSquareMeterIdentifier): TSquareMeterIdentifier; inline;
operator *(const {%H-}ALeft: TWattPerSquareMeterIdentifier; const {%H-}ARight: TSquareMeterIdentifier): TWattIdentifier; inline;
operator *(const {%H-}ALeft: TSquareMeterIdentifier; const {%H-}ARight: TWattPerSquareMeterIdentifier): TWattIdentifier; inline;

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

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const {%H-}ALeft: TVoltIdentifier; const {%H-}ARight: TSecondIdentifier): TWeberIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TVoltIdentifier): TSecondIdentifier; inline;
operator *(const {%H-}ALeft: TSecondIdentifier; const {%H-}ARight: TVoltIdentifier): TWeberIdentifier; inline;
operator /(const {%H-}ALeft: TWeberIdentifier; const {%H-}ARight: TSecondIdentifier): TVoltIdentifier; inline;

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

// main definition [ N*m/deg ] = [ N*m ] / [ deg ]
operator /(const {%H-}ALeft: TNewtonMeterIdentifier; const {%H-}ARight: TDegreeIdentifier): TNewtonMeterPerDegreeIdentifier; inline;

// main definition [ N*mm/rad ] = [ N*mm ] / [ rad ]
operator /(const {%H-}ALeft: TNewtonMillimeterIdentifier; const {%H-}ARight: TRadianIdentifier): TNewtonMillimeterPerRadianIdentifier; inline;

// main definition [ N*mm/deg ] = [ N*mm ] / [ deg ]
operator /(const {%H-}ALeft: TNewtonMillimeterIdentifier; const {%H-}ARight: TDegreeIdentifier): TNewtonMillimeterPerDegreeIdentifier; inline;

// main definition [ N/dm ] = [ N ] / [ dm ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TDecimeterIdentifier): TNewtonPerDecimeterIdentifier; inline;

// main definition [ N/cm ] = [ N ] / [ cm ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TCentimeterIdentifier): TNewtonPerCentimeterIdentifier; inline;

// main definition [ N/mm ] = [ N ] / [ mm ]
operator /(const {%H-}ALeft: TNewtonIdentifier; const {%H-}ARight: TMillimeterIdentifier): TNewtonPerMillimeterIdentifier; inline;

{ Combining quantities }

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmperes; const ARight: TMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TMeters; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TMeters): TAmperes; inline;
operator *(const ALeft: TMeters; const ARight: TAmperesPerMeter): TAmperes; inline;

// alternative definition [ A/m ] = [ A ] * [ 1/m ]
operator *(const ALeft: TAmperes; const ARight: TReciprocalMeters): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperesPerMeter; const ARight: TAmperes): TReciprocalMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TAmperes): TAmperesPerMeter; inline;
operator /(const ALeft: TAmperesPerMeter; const ARight: TReciprocalMeters): TAmperes; inline;

// main definition [ C ] = [ s ] * [ A ]
operator *(const ALeft: TSeconds; const ARight: TAmperes): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TSeconds): TAmperes; inline;
operator *(const ALeft: TAmperes; const ARight: TSeconds): TCoulombs; inline;
operator /(const ALeft: TCoulombs; const ARight: TAmperes): TSeconds; inline;

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombs; const ARight: TMeters): TCoulombsPerMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TCoulombsPerMeter): TMeters; inline;
operator *(const ALeft: TCoulombsPerMeter; const ARight: TMeters): TCoulombs; inline;
operator *(const ALeft: TMeters; const ARight: TCoulombsPerMeter): TCoulombs; inline;

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

// alternative definition [ C/m2 ] = [ C ] * [ 1/m2 ]
operator *(const ALeft: TCoulombs; const ARight: TReciprocalSquareMeters): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TCoulombs): TReciprocalSquareMeters; inline;
operator *(const ALeft: TReciprocalSquareMeters; const ARight: TCoulombs): TCoulombsPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TReciprocalSquareMeters): TCoulombs; inline;

// main definition [ K3 ] = [ K2 ] * [ K ]
operator *(const ALeft: TSquareKelvins; const ARight: TKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TSquareKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareKelvins): TCubicKelvins; inline;
operator /(const ALeft: TCubicKelvins; const ARight: TKelvins): TSquareKelvins; inline;

// main definition [ m3 ]
operator *(const ALeft: TSquareMeters; const ARight: TMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TSquareMeters): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareMeters): TCubicMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TMeters): TSquareMeters; inline;

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

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSeconds): THertz; inline;
operator /(const ALeft: double; const ARight: THertz): TSeconds; inline;
operator *(const ALeft: THertz; const ARight: TSeconds): double; inline;
operator *(const ALeft: TSeconds; const ARight: THertz): double; inline;

// main definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascals; const ARight: TCubicMeters): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TPascals): TCubicMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TPascals): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TCubicMeters): TPascals; inline;

// alternative definition [ J ] = [ kg*m2 ] * [ rad2/s2 ]
operator *(const ALeft: TKilogramsSquareMeter; const ARight: TSquareRadiansPerSquareSecond): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TKilogramsSquareMeter): TSquareRadiansPerSquareSecond; inline;
operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TKilogramsSquareMeter): TJoules; inline;
operator /(const ALeft: TJoules; const ARight: TSquareRadiansPerSquareSecond): TKilogramsSquareMeter; inline;

// main definition [ J/K ] = [ J ] / [ K ]
operator /(const ALeft: TJoules; const ARight: TKelvins): TJoulesPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKelvin; const ARight: TKelvins): TJoules; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKelvin): TJoules; inline;

// main definition [ J/kg ] = [ J ] / [ kg ]
operator /(const ALeft: TJoules; const ARight: TKilograms): TJoulesPerKilogram; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogram): TKilograms; inline;
operator *(const ALeft: TJoulesPerKilogram; const ARight: TKilograms): TJoules; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogram): TJoules; inline;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramsKelvin): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramsKelvin; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramsKelvin): TJoules; inline;
operator *(const ALeft: TKilogramsKelvin; const ARight: TJoulesPerKilogramPerKelvin): TJoules; inline;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TJoulesPerKilogram; const ARight: TKelvins): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoulesPerKilogram; const ARight: TJoulesPerKilogramPerKelvin): TKelvins; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TJoulesPerKilogram; inline;
operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKilogram; inline;

// alternative definition [ J/kg/K ] = [ J/K ] / [ kg ]
operator /(const ALeft: TJoulesPerKelvin; const ARight: TKilograms): TJoulesPerKilogramPerKelvin; inline;
operator /(const ALeft: TJoulesPerKelvin; const ARight: TJoulesPerKilogramPerKelvin): TKilograms; inline;
operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilograms): TJoulesPerKelvin; inline;
operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKelvin; inline;

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJoules; const ARight: TMoles): TJoulesPerMole; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMole): TMoles; inline;
operator *(const ALeft: TJoulesPerMole; const ARight: TMoles): TJoules; inline;
operator *(const ALeft: TMoles; const ARight: TJoulesPerMole): TJoules; inline;

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMolesKelvin): TJoulesPerMolePerKelvin; inline;
operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMolesKelvin; inline;
operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMolesKelvin): TJoules; inline;
operator *(const ALeft: TMolesKelvin; const ARight: TJoulesPerMolePerKelvin): TJoules; inline;

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

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoles; const ARight: TSeconds): TKatals; inline;
operator /(const ALeft: TMoles; const ARight: TKatals): TSeconds; inline;
operator *(const ALeft: TKatals; const ARight: TSeconds): TMoles; inline;
operator *(const ALeft: TSeconds; const ARight: TKatals): TMoles; inline;

// main definition [ K/m ] = [ K ] / [ m ]
operator /(const ALeft: TKelvins; const ARight: TMeters): TKelvinsPerMeter; inline;
operator /(const ALeft: TKelvins; const ARight: TKelvinsPerMeter): TMeters; inline;
operator *(const ALeft: TKelvinsPerMeter; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TMeters; const ARight: TKelvinsPerMeter): TKelvins; inline;

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramsKelvin; inline;
operator /(const ALeft: TKilogramsKelvin; const ARight: TKilograms): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramsKelvin; inline;
operator /(const ALeft: TKilogramsKelvin; const ARight: TKelvins): TKilograms; inline;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramsMeterPerSecond; inline;
operator /(const ALeft: TKilogramsMeterPerSecond; const ARight: TKilograms): TMetersPerSecond; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramsMeterPerSecond; inline;
operator /(const ALeft: TKilogramsMeterPerSecond; const ARight: TMetersPerSecond): TKilograms; inline;

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilograms; const ARight: TCubicMeters): TKilogramsPerCubicMeter; inline;
operator /(const ALeft: TKilograms; const ARight: TKilogramsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TCubicMeters): TKilograms; inline;
operator *(const ALeft: TCubicMeters; const ARight: TKilogramsPerCubicMeter): TKilograms; inline;

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsSquareMeter; inline;
operator /(const ALeft: TKilogramsSquareMeter; const ARight: TKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramsSquareMeter; inline;
operator /(const ALeft: TKilogramsSquareMeter; const ARight: TSquareMeters): TKilograms; inline;

// main definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramsSquareMeter; const ARight: THertz): TKilogramsSquareMeterPerSecond; inline;
operator /(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: TKilogramsSquareMeter): THertz; inline;
operator *(const ALeft: THertz; const ARight: TKilogramsSquareMeter): TKilogramsSquareMeterPerSecond; inline;
operator /(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: THertz): TKilogramsSquareMeter; inline;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramsSquareMeter; const ARight: TSeconds): TKilogramsSquareMeterPerSecond; inline;
operator /(const ALeft: TKilogramsSquareMeter; const ARight: TKilogramsSquareMeterPerSecond): TSeconds; inline;
operator *(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: TSeconds): TKilogramsSquareMeter; inline;
operator *(const ALeft: TSeconds; const ARight: TKilogramsSquareMeterPerSecond): TKilogramsSquareMeter; inline;

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

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMetersKelvin; inline;
operator /(const ALeft: TMetersKelvin; const ARight: TMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMeters): TMetersKelvin; inline;
operator /(const ALeft: TMetersKelvin; const ARight: TKelvins): TMeters; inline;

// main definition [ m/A ] = [ m ] / [ A ]
operator /(const ALeft: TMeters; const ARight: TAmperes): TMetersPerAmpere; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerAmpere): TAmperes; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TAmperes): TMeters; inline;
operator *(const ALeft: TAmperes; const ARight: TMetersPerAmpere): TMeters; inline;

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeters; const ARight: TSeconds): TMetersPerSecond; inline;
operator /(const ALeft: TMeters; const ARight: TMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TMetersPerSecond; const ARight: TSeconds): TMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TMetersPerSecond): TMeters; inline;

// alternative definition [ m/s ] = [ m ] * [ hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz; inline;
operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond; inline;
operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters; inline;

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
operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareRadiansPerSquareSecond): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TSquareRadiansPerSquareSecond): TMetersPerSquareSecond; inline;
operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareRadiansPerSquareSecond; inline;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMolesKelvin; inline;
operator /(const ALeft: TMolesKelvin; const ARight: TMoles): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TMoles): TMolesKelvin; inline;
operator /(const ALeft: TMolesKelvin; const ARight: TKelvins): TMoles; inline;

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSquareSecond): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TKilograms): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilograms): TNewtons; inline;
operator /(const ALeft: TNewtons; const ARight: TMetersPerSquareSecond): TKilograms; inline;

// main definition [ Nm ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TNewtonsMeter; inline;
operator /(const ALeft: TNewtonsMeter; const ARight: TNewtons): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TNewtons): TNewtonsMeter; inline;
operator /(const ALeft: TNewtonsMeter; const ARight: TMeters): TNewtons; inline;

// main definition [ N*m/rad ] = [ N*m ] / [ rad ]
operator /(const ALeft: TNewtonsMeter; const ARight: TRadians): TNewtonsMeterPerRadian; inline;
operator /(const ALeft: TNewtonsMeter; const ARight: TNewtonsMeterPerRadian): TRadians; inline;
operator *(const ALeft: TNewtonsMeterPerRadian; const ARight: TRadians): TNewtonsMeter; inline;
operator *(const ALeft: TRadians; const ARight: TNewtonsMeterPerRadian): TNewtonsMeter; inline;

// main definition [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TNewtonsPerAmpere; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerAmpere): TAmperes; inline;
operator *(const ALeft: TNewtonsPerAmpere; const ARight: TAmperes): TNewtons; inline;
operator *(const ALeft: TAmperes; const ARight: TNewtonsPerAmpere): TNewtons; inline;

// main definition [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TNewtonsPerCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCoulomb): TCoulombs; inline;
operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TCoulombs): TNewtons; inline;
operator *(const ALeft: TCoulombs; const ARight: TNewtonsPerCoulomb): TNewtons; inline;

// alternative definition [ N/C ] = [ m/s ] * [ T ]
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TNewtonsPerCoulomb; inline;
operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TMetersPerSecond): TTeslas; inline;
operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TNewtonsPerCoulomb; inline;
operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TTeslas): TMetersPerSecond; inline;

// main definition [ N/m3 ] = [ N ] / [ m3 ]
operator /(const ALeft: TNewtons; const ARight: TCubicMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCubicMeter): TCubicMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TCubicMeters): TNewtons; inline;
operator *(const ALeft: TCubicMeters; const ARight: TNewtonsPerCubicMeter): TNewtons; inline;

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TMetersPerSquareSecond): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TKilogramsPerCubicMeter): TMetersPerSquareSecond; inline;
operator *(const ALeft: TMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TNewtonsPerCubicMeter; const ARight: TMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

// alternative definition [ N/m3 ] = [ Pa ] / [ m ]
operator /(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerCubicMeter; inline;
operator /(const ALeft: TPascals; const ARight: TNewtonsPerCubicMeter): TMeters; inline;
operator *(const ALeft: TNewtonsPerCubicMeter; const ARight: TMeters): TPascals; inline;
operator *(const ALeft: TMeters; const ARight: TNewtonsPerCubicMeter): TPascals; inline;

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

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombs): TNewtonsPerSquareCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtons; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsPerSquareCoulomb): TNewtons; inline;

// main definition [ N/kg2 ] = [ N ] / [ kg2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilograms): TNewtonsPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareKilograms): TNewtons; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsPerSquareKilogram): TNewtons; inline;

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonsSquareMeter; inline;
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtons): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonsSquareMeter; inline;
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareMeters): TNewtons; inline;

// main definition [ N*m2/C ] = [ N ] * [ m2/C ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerCoulomb): TNewtonsSquareMeterPerCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TNewtons): TSquareMetersPerCoulomb; inline;
operator *(const ALeft: TSquareMetersPerCoulomb; const ARight: TNewtons): TNewtonsSquareMeterPerCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TSquareMetersPerCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C ] = [ N*m2 ] / [ C ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TCoulombs): TNewtonsSquareMeterPerCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerCoulomb): TCoulombs; inline;
operator *(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TCoulombs): TNewtonsSquareMeter; inline;
operator *(const ALeft: TCoulombs; const ARight: TNewtonsSquareMeterPerCoulomb): TNewtonsSquareMeter; inline;

// alternative definition [ N*m2/C ] = [ N/C ] * [ m2 ]
operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TSquareMeters): TNewtonsSquareMeterPerCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TNewtonsPerCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerCoulomb): TNewtonsSquareMeterPerCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TSquareMeters): TNewtonsPerCoulomb; inline;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareCoulombs): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonsSquareMeter; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtonsSquareMeter; inline;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ N ] / [ C2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombsPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerSquareMeter): TNewtons; inline;
operator *(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtons; inline;

// alternative definition [ N*m2/C2 ] = [ N/C ] / [ C/m2 ]
operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TCoulombsPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TNewtonsPerCoulomb; inline;
operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtonsPerCoulomb; inline;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerMeter; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TJoules; inline;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareKilograms): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilograms): TNewtonsSquareMeter; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsSquareMeterPerSquareKilogram): TNewtonsSquareMeter; inline;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram; inline;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TNewtons; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilogramsPerSquareMeter; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons; inline;
operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TNewtons; inline;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonsSquareMeterPerSquareKilogram; inline;
operator /(const ALeft: TJoules; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilogramsPerMeter; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules; inline;
operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TJoules; inline;

// main definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtons; const ARight: TSeconds): TNewtonsSecond; inline;
operator /(const ALeft: TNewtonsSecond; const ARight: TNewtons): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TNewtons): TNewtonsSecond; inline;
operator /(const ALeft: TNewtonsSecond; const ARight: TSeconds): TNewtons; inline;

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

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmsMeter; inline;
operator /(const ALeft: TOhmsMeter; const ARight: TOhms): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TOhms): TOhmsMeter; inline;
operator /(const ALeft: TOhmsMeter; const ARight: TMeters): TOhms; inline;

// alternative definition [ Ω*m ] = [ Ω ] / [ 1/m ]
operator /(const ALeft: TOhms; const ARight: TReciprocalMeters): TOhmsMeter; inline;
operator /(const ALeft: TOhms; const ARight: TOhmsMeter): TReciprocalMeters; inline;
operator *(const ALeft: TOhmsMeter; const ARight: TReciprocalMeters): TOhms; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TOhmsMeter): TOhms; inline;

// main definition [ Pa ] = [ N ] / [ m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareMeters): TPascals; inline;
operator /(const ALeft: TNewtons; const ARight: TPascals): TSquareMeters; inline;
operator *(const ALeft: TPascals; const ARight: TSquareMeters): TNewtons; inline;
operator *(const ALeft: TSquareMeters; const ARight: TPascals): TNewtons; inline;

// alternative definition [ Pa ] = [ N/m ] / [ m ]
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters; inline;
operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter; inline;
operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter; inline;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals; inline;
operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter; inline;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalsSecond; inline;
operator /(const ALeft: TPascalsSecond; const ARight: TPascals): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalsSecond; inline;
operator /(const ALeft: TPascalsSecond; const ARight: TSeconds): TPascals; inline;

// alternative definition [ Pa*s ] = [ Pa ] / [ Hz ]
operator /(const ALeft: TPascals; const ARight: THertz): TPascalsSecond; inline;
operator /(const ALeft: TPascals; const ARight: TPascalsSecond): THertz; inline;
operator *(const ALeft: TPascalsSecond; const ARight: THertz): TPascals; inline;
operator *(const ALeft: THertz; const ARight: TPascalsSecond): TPascals; inline;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvins; const ARight: TKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TCubicKelvins): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TCubicKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TKelvins): TCubicKelvins; inline;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins; inline;
operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins; inline;

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

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadians; const ARight: TSeconds): TRadiansPerSecond; inline;
operator /(const ALeft: TRadians; const ARight: TRadiansPerSecond): TSeconds; inline;
operator *(const ALeft: TRadiansPerSecond; const ARight: TSeconds): TRadians; inline;
operator *(const ALeft: TSeconds; const ARight: TRadiansPerSecond): TRadians; inline;

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

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvins): TReciprocalKelvins; inline;
operator /(const ALeft: double; const ARight: TReciprocalKelvins): TKelvins; inline;
operator *(const ALeft: TReciprocalKelvins; const ARight: TKelvins): double; inline;
operator *(const ALeft: TKelvins; const ARight: TReciprocalKelvins): double; inline;

// main definition [ 1/m ] = 1 / [ m ]
operator /(const ALeft: double; const ARight: TMeters): TReciprocalMeters; inline;
operator /(const ALeft: double; const ARight: TReciprocalMeters): TMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TMeters): double; inline;
operator *(const ALeft: TMeters; const ARight: TReciprocalMeters): double; inline;

// alternative definition [ 1/m ] = [ m ] / [ m2 ]
operator /(const ALeft: TMeters; const ARight: TSquareMeters): TReciprocalMeters; inline;
operator /(const ALeft: TMeters; const ARight: TReciprocalMeters): TSquareMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TSquareMeters): TMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TReciprocalMeters): TMeters; inline;

// alternative definition [ 1/m ] = [ m2 ] / [ m3 ]
operator /(const ALeft: TSquareMeters; const ARight: TCubicMeters): TReciprocalMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TReciprocalMeters): TCubicMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TCubicMeters): TSquareMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TReciprocalMeters): TSquareMeters; inline;

// alternative definition [ 1/m ] = [ m3 ] / [ m4 ]
operator /(const ALeft: TCubicMeters; const ARight: TQuarticMeters): TReciprocalMeters; inline;
operator /(const ALeft: TCubicMeters; const ARight: TReciprocalMeters): TQuarticMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TQuarticMeters): TCubicMeters; inline;
operator *(const ALeft: TQuarticMeters; const ARight: TReciprocalMeters): TCubicMeters; inline;

// alternative definition [ 1/m ] = [ m4 ] / [ m5 ]
operator /(const ALeft: TQuarticMeters; const ARight: TQuinticMeters): TReciprocalMeters; inline;
operator /(const ALeft: TQuarticMeters; const ARight: TReciprocalMeters): TQuinticMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TQuinticMeters): TQuarticMeters; inline;
operator *(const ALeft: TQuinticMeters; const ARight: TReciprocalMeters): TQuarticMeters; inline;

// alternative definition [ 1/m ] = [ m5 ] / [ m6 ]
operator /(const ALeft: TQuinticMeters; const ARight: TSexticMeters): TReciprocalMeters; inline;
operator /(const ALeft: TQuinticMeters; const ARight: TReciprocalMeters): TSexticMeters; inline;
operator *(const ALeft: TReciprocalMeters; const ARight: TSexticMeters): TQuinticMeters; inline;
operator *(const ALeft: TSexticMeters; const ARight: TReciprocalMeters): TQuinticMeters; inline;

// main definition [ 1/m2 ] = [ 1 ] / [ m2 ]
operator /(const ALeft: double; const ARight: TSquareMeters): TReciprocalSquareMeters; inline;
operator /(const ALeft: double; const ARight: TReciprocalSquareMeters): TSquareMeters; inline;
operator *(const ALeft: TReciprocalSquareMeters; const ARight: TSquareMeters): double; inline;
operator *(const ALeft: TSquareMeters; const ARight: TReciprocalSquareMeters): double; inline;

//
operator /(const ALeft: TMeters; const ARight: TCubicMeters): TReciprocalSquareMeters; inline;
operator /(const ALeft: TMeters; const ARight: TReciprocalSquareMeters): TCubicMeters; inline;
operator *(const ALeft: TReciprocalSquareMeters; const ARight: TCubicMeters): TMeters; inline;
operator *(const ALeft: TCubicMeters; const ARight: TReciprocalSquareMeters): TMeters; inline;

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

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhms): TSiemens; inline;
operator /(const ALeft: double; const ARight: TSiemens): TOhms; inline;
operator *(const ALeft: TSiemens; const ARight: TOhms): double; inline;
operator *(const ALeft: TOhms; const ARight: TSiemens): double; inline;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes; inline;

// main definition [ A2/m ] = [ A2] / [ m ]
operator /(const ALeft: TSquareAmperes; const ARight: TMeters): TSquareAmperesPerMeter; inline;
operator /(const ALeft: TSquareAmperes; const ARight: TSquareAmperesPerMeter): TMeters; inline;
operator *(const ALeft: TSquareAmperesPerMeter; const ARight: TMeters): TSquareAmperes; inline;
operator *(const ALeft: TMeters; const ARight: TSquareAmperesPerMeter): TSquareAmperes; inline;

// alternative definition [ A2/m ] = [ A/m ] * [ A ]
operator *(const ALeft: TAmperesPerMeter; const ARight: TAmperes): TSquareAmperesPerMeter; inline;
operator /(const ALeft: TSquareAmperesPerMeter; const ARight: TAmperesPerMeter): TAmperes; inline;
operator *(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TSquareAmperesPerMeter; inline;
operator /(const ALeft: TSquareAmperesPerMeter; const ARight: TAmperes): TAmperesPerMeter; inline;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombs; const ARight: TCoulombs): TSquareCoulombs; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TCoulombs): TCoulombs; inline;

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

// main definition [ C2/m2 ] = [ C2 ] / [ m2 ]
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareMeters): TSquareCoulombsPerSquareMeter; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TSquareMeters): TSquareCoulombs; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareCoulombsPerSquareMeter): TSquareCoulombs; inline;

// main definition [ C2/N ] = [ C2 ] / [ N ]
operator /(const ALeft: TSquareCoulombs; const ARight: TNewtons): TSquareCoulombsPerNewton; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerNewton): TNewtons; inline;
operator *(const ALeft: TSquareCoulombsPerNewton; const ARight: TNewtons): TSquareCoulombs; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareCoulombsPerNewton): TSquareCoulombs; inline;

// main definition [ C2/N/m2 ] = [ C2 ] / [N*m2 ]
operator /(const ALeft: TSquareCoulombs; const ARight: TNewtonsSquareMeter): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsSquareMeter; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsSquareMeter): TSquareCoulombs; inline;
operator *(const ALeft: TNewtonsSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombs; inline;

// alternative definition [ C2/N/m2 ] = [ C2/N ] / [ m2 ]
operator /(const ALeft: TSquareCoulombsPerNewton; const ARight: TSquareMeters): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TSquareCoulombsPerNewton; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TSquareMeters): TSquareCoulombsPerNewton; inline;
operator *(const ALeft: TSquareMeters; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombsPerNewton; inline;

// alternative definition [ C2/N/m2 ] = [ C2/m2 ] / [ N ]
operator /(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TNewtons): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtons; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtons): TSquareCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtons; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombsPerSquareMeter; inline;

// alternative definition [ C2/N/m2 ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonsPerCoulomb): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsPerCoulomb; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsPerCoulomb): TCoulombsPerSquareMeter; inline;
operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TCoulombsPerSquareMeter; inline;

// alternative definition [ C2/N/m2 ] = [ A ] / [ V*m/s ]
operator /(const ALeft: TAmperes; const ARight: TVoltsMeterPerSecond): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TAmperes; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TVoltsMeterPerSecond; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TVoltsMeterPerSecond): TAmperes; inline;
operator *(const ALeft: TVoltsMeterPerSecond; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TAmperes; inline;

// alternative definition [ C2/N/m2 ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltsMeter): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: TCoulombs; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TVoltsMeter; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TVoltsMeter): TCoulombs; inline;
operator *(const ALeft: TVoltsMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TCoulombs; inline;

// alternative definition [ C2/N/m2 ] = 1 / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerNewtonPerSquareMeter; inline;
operator /(const ALeft: double; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb; inline;
operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): double; inline;
operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): double; inline;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertz; const ARight: THertz): TSquareHertz; inline;
operator /(const ALeft: TSquareHertz; const ARight: THertz): THertz; inline;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins; inline;
operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins; inline;

// main definition [ kg2 ]
operator *(const ALeft: TKilograms; const ARight: TKilograms): TSquareKilograms; inline;
operator /(const ALeft: TSquareKilograms; const ARight: TKilograms): TKilograms; inline;

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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters; inline;
operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters; inline;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMetersKelvin; inline;
operator /(const ALeft: TSquareMetersKelvin; const ARight: TSquareMeters): TKelvins; inline;
operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMetersKelvin; inline;
operator /(const ALeft: TSquareMetersKelvin; const ARight: TKelvins): TSquareMeters; inline;

// main definition [ m2/C ] = [ m2 ] / [ C ]
operator /(const ALeft: TSquareMeters; const ARight: TCoulombs): TSquareMetersPerCoulomb; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerCoulomb): TCoulombs; inline;
operator *(const ALeft: TSquareMetersPerCoulomb; const ARight: TCoulombs): TSquareMeters; inline;
operator *(const ALeft: TCoulombs; const ARight: TSquareMetersPerCoulomb): TSquareMeters; inline;

// main definition [ m2/C2 ] = [ m2 ] / [ C2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareCoulombs): TSquareMetersPerSquareCoulomb; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareCoulomb): TSquareCoulombs; inline;
operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TSquareCoulombs): TSquareMeters; inline;
operator *(const ALeft: TSquareCoulombs; const ARight: TSquareMetersPerSquareCoulomb): TSquareMeters; inline;

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareKilograms): TSquareMetersPerSquareKilogram; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareKilogram): TSquareKilograms; inline;
operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TSquareKilograms): TSquareMeters; inline;
operator *(const ALeft: TSquareKilograms; const ARight: TSquareMetersPerSquareKilogram): TSquareMeters; inline;

// main definition [ m2/s ] = [ m2 ] / [ s ]
operator /(const ALeft: TSquareMeters; const ARight: TSeconds): TSquareMetersPerSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSecond): TSeconds; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TSeconds): TSquareMeters; inline;
operator *(const ALeft: TSeconds; const ARight: TSquareMetersPerSecond): TSquareMeters; inline;

// alternative definition [ m2/s ] = [ Pa*s ] / [ kg/m3 ]
operator /(const ALeft: TPascalsSecond; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond; inline;
operator /(const ALeft: TPascalsSecond; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter; inline;
operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalsSecond; inline;
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalsSecond; inline;

// main definition [ m2/s2 ] = [ m2 ] / [ s2 ]
operator /(const ALeft: TSquareMeters; const ARight: TSquareSeconds): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TSquareSeconds): TSquareMeters; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareMetersPerSquareSecond): TSquareMeters; inline;

// alternative definition [ m2/s2 ] = [ m/s ] / [ m/s ]
operator *(const ALeft: TMetersPerSecond; const ARight: TMetersPerSecond): TSquareMetersPerSquareSecond; inline;
operator /(const ALeft: TSquareMetersPerSquareSecond; const ARight: TMetersPerSecond): TMetersPerSecond; inline;

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMetersQuarticKelvin; inline;
operator /(const ALeft: TSquareMetersQuarticKelvin; const ARight: TSquareMeters): TQuarticKelvins; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMetersQuarticKelvin; inline;
operator /(const ALeft: TSquareMetersQuarticKelvin; const ARight: TQuarticKelvins): TSquareMeters; inline;

// main definition [ rad2 ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSquareRadians; inline;
operator /(const ALeft: TSquareRadians; const ARight: TRadians): TRadians; inline;

// main definition [ rad2/s2 ] = [ rad2 ] / [ s2 ]
operator /(const ALeft: TSquareRadians; const ARight: TSquareSeconds): TSquareRadiansPerSquareSecond; inline;
operator /(const ALeft: TSquareRadians; const ARight: TSquareRadiansPerSquareSecond): TSquareSeconds; inline;
operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TSquareSeconds): TSquareRadians; inline;
operator *(const ALeft: TSquareSeconds; const ARight: TSquareRadiansPerSquareSecond): TSquareRadians; inline;

// alternative definition [ rad2/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSquareRadiansPerSquareSecond; inline;
operator /(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond; inline;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSeconds; const ARight: TSeconds): TSquareSeconds; inline;
operator /(const ALeft: TSquareSeconds; const ARight: TSeconds): TSeconds; inline;

// main definition [ V2 ] = [ V ] * [ V ]
operator *(const ALeft: TVolts; const ARight: TVolts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TVolts): TVolts; inline;

// alternative definition [ V2 ] = [ W ] * [ Ω ]
operator *(const ALeft: TWatts; const ARight: TOhms): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TWatts): TOhms; inline;
operator *(const ALeft: TOhms; const ARight: TWatts): TSquareVolts; inline;
operator /(const ALeft: TSquareVolts; const ARight: TOhms): TWatts; inline;

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWebers; const ARight: TSquareMeters): TTeslas; inline;
operator /(const ALeft: TWebers; const ARight: TTeslas): TSquareMeters; inline;
operator *(const ALeft: TTeslas; const ARight: TSquareMeters): TWebers; inline;
operator *(const ALeft: TSquareMeters; const ARight: TTeslas): TWebers; inline;

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslasMeter; inline;
operator /(const ALeft: TTeslasMeter; const ARight: TTeslas): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslasMeter; inline;
operator /(const ALeft: TTeslasMeter; const ARight: TMeters): TTeslas; inline;

// main definition [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslasMeter; const ARight: TAmperes): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslasMeter; const ARight: TTeslasMeterPerAmpere): TAmperes; inline;
operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TAmperes): TTeslasMeter; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasMeterPerAmpere): TTeslasMeter; inline;

// alternative definition [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TTeslasPerAmpere): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TMeters): TTeslasPerAmpere; inline;

// alternative definition [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TTeslas): TMetersPerAmpere; inline;
operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TMetersPerAmpere): TTeslas; inline;

// alternative definition [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasMeterPerAmpere): TAmperesPerMeter; inline;
operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TAmperesPerMeter): TTeslas; inline;
operator *(const ALeft: TAmperesPerMeter; const ARight: TTeslasMeterPerAmpere): TTeslas; inline;

// alternative definition [ T*m/A ] = [ N/m ] / [ A2/m ]
operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareAmperesPerMeter): TTeslasMeterPerAmpere; inline;
operator /(const ALeft: TNewtonsPerMeter; const ARight: TTeslasMeterPerAmpere): TSquareAmperesPerMeter; inline;
operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TSquareAmperesPerMeter): TNewtonsPerMeter; inline;
operator *(const ALeft: TSquareAmperesPerMeter; const ARight: TTeslasMeterPerAmpere): TNewtonsPerMeter; inline;

// main definition [ T/A ] = [ T ] / [ A ]
operator /(const ALeft: TTeslas; const ARight: TAmperes): TTeslasPerAmpere; inline;
operator /(const ALeft: TTeslas; const ARight: TTeslasPerAmpere): TAmperes; inline;
operator *(const ALeft: TTeslasPerAmpere; const ARight: TAmperes): TTeslas; inline;
operator *(const ALeft: TAmperes; const ARight: TTeslasPerAmpere): TTeslas; inline;

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

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltsMeter; inline;
operator /(const ALeft: TVoltsMeter; const ARight: TVolts): TMeters; inline;
operator *(const ALeft: TMeters; const ARight: TVolts): TVoltsMeter; inline;
operator /(const ALeft: TVoltsMeter; const ARight: TMeters): TVolts; inline;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltsMeter; inline;
operator /(const ALeft: TVoltsMeter; const ARight: TVoltsPerMeter): TSquareMeters; inline;
operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltsMeter; inline;
operator /(const ALeft: TVoltsMeter; const ARight: TSquareMeters): TVoltsPerMeter; inline;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltsMeter; const ARight: TSeconds): TVoltsMeterPerSecond; inline;
operator /(const ALeft: TVoltsMeter; const ARight: TVoltsMeterPerSecond): TSeconds; inline;
operator *(const ALeft: TVoltsMeterPerSecond; const ARight: TSeconds): TVoltsMeter; inline;
operator *(const ALeft: TSeconds; const ARight: TVoltsMeterPerSecond): TVoltsMeter; inline;

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVolts; const ARight: TMeters): TVoltsPerMeter; inline;
operator /(const ALeft: TVolts; const ARight: TVoltsPerMeter): TMeters; inline;
operator *(const ALeft: TVoltsPerMeter; const ARight: TMeters): TVolts; inline;
operator *(const ALeft: TMeters; const ARight: TVoltsPerMeter): TVolts; inline;

// main definition [ W ] = [ J ] / [ s ]
operator /(const ALeft: TJoules; const ARight: TSeconds): TWatts; inline;
operator /(const ALeft: TJoules; const ARight: TWatts): TSeconds; inline;
operator *(const ALeft: TWatts; const ARight: TSeconds): TJoules; inline;
operator *(const ALeft: TSeconds; const ARight: TWatts): TJoules; inline;

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJoules; const ARight: THertz): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: TJoules): THertz; inline;
operator *(const ALeft: THertz; const ARight: TJoules): TWatts; inline;
operator /(const ALeft: TWatts; const ARight: THertz): TJoules; inline;

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

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWatts; const ARight: TKelvins): TWattsPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerKelvin): TKelvins; inline;
operator *(const ALeft: TWattsPerKelvin; const ARight: TKelvins): TWatts; inline;
operator *(const ALeft: TKelvins; const ARight: TWattsPerKelvin): TWatts; inline;

// main definition [ W/m ] = [ W ] / [ m ]
operator /(const ALeft: TWatts; const ARight: TMeters): TWattsPerMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeter): TMeters; inline;
operator *(const ALeft: TWattsPerMeter; const ARight: TMeters): TWatts; inline;
operator *(const ALeft: TMeters; const ARight: TWattsPerMeter): TWatts; inline;

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMetersKelvin): TWattsPerMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMetersKelvin; inline;
operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMetersKelvin): TWatts; inline;
operator *(const ALeft: TMetersKelvin; const ARight: TWattsPerMeterPerKelvin): TWatts; inline;

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

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWatts; const ARight: TQuarticKelvins): TWattsPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerQuarticKelvin): TQuarticKelvins; inline;
operator *(const ALeft: TWattsPerQuarticKelvin; const ARight: TQuarticKelvins): TWatts; inline;
operator *(const ALeft: TQuarticKelvins; const ARight: TWattsPerQuarticKelvin): TWatts; inline;

// main definition [ W/m2 ] = [ W ] / [ m2 ]
operator /(const ALeft: TWatts; const ARight: TSquareMeters): TWattsPerSquareMeter; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeter): TSquareMeters; inline;
operator *(const ALeft: TWattsPerSquareMeter; const ARight: TSquareMeters): TWatts; inline;
operator *(const ALeft: TSquareMeters; const ARight: TWattsPerSquareMeter): TWatts; inline;

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMetersKelvin): TWattsPerSquareMeterPerKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMetersKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMetersKelvin): TWatts; inline;
operator *(const ALeft: TSquareMetersKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TWatts; inline;

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

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMetersQuarticKelvin): TWattsPerSquareMeterPerQuarticKelvin; inline;
operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMetersQuarticKelvin; inline;
operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMetersQuarticKelvin): TWatts; inline;
operator *(const ALeft: TSquareMetersQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts; inline;

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

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVolts; const ARight: TSeconds): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TVolts): TSeconds; inline;
operator *(const ALeft: TSeconds; const ARight: TVolts): TWebers; inline;
operator /(const ALeft: TWebers; const ARight: TSeconds): TVolts; inline;

{ Power units }

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
function CubicPower(AQuantity: TMeters): TCubicMeters;
function CubicRoot(AQuantity: TCubicMeters): TMeters;
function SquarePower(AQuantity: TSquareKelvins): TQuarticKelvins;
function SquareRoot(AQuantity: TQuarticKelvins): TSquareKelvins;
function QuarticPower(AQuantity: TKelvins): TQuarticKelvins;
function QuarticRoot(AQuantity: TQuarticKelvins): TKelvins;
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
function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
function SquarePower(AQuantity: THertz): TSquareHertz;
function SquareRoot(AQuantity: TSquareHertz): THertz;
function SquarePower(AQuantity: TKelvins): TSquareKelvins;
function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
function SquarePower(AQuantity: TKilograms): TSquareKilograms;
function SquareRoot(AQuantity: TSquareKilograms): TKilograms;
function SquarePower(AQuantity: TMeters): TSquareMeters;
function SquareRoot(AQuantity: TSquareMeters): TMeters;
function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
function SquarePower(AQuantity: TRadians): TSquareRadians;
function SquareRoot(AQuantity: TSquareRadians): TRadians;
function SquarePower(AQuantity: TRadiansPerSecond): TSquareRadiansPerSquareSecond;
function SquareRoot(AQuantity: TSquareRadiansPerSquareSecond): TRadiansPerSecond;
function SquarePower(AQuantity: TSeconds): TSquareSeconds;
function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
function SquarePower(AQuantity: TVolts): TSquareVolts;
function SquareRoot(AQuantity: TSquareVolts): TVolts;

{ Equivalences }

operator := (AQuantity: TRadiansPerSecond): THertz; inline;
operator := (AQuantity: THertz): TRadiansPerSecond; inline;
operator := (AQuantity: TBequerels): THertz; inline;
operator := (AQuantity: THertz): TBequerels; inline;
operator := (AQuantity: TNewtonsMeter): TJoules; inline;
operator := (AQuantity: TJoules): TNewtonsMeter; inline;
operator := (AQuantity: TSquareMetersPerSquareSecond): TJoulesPerKilogram; inline;
operator := (AQuantity: TJoulesPerKilogram): TSquareMetersPerSquareSecond; inline;
operator := (AQuantity: TSieverts): TJoulesPerKilogram; inline;
operator := (AQuantity: TJoulesPerKilogram): TSieverts; inline;
operator := (AQuantity: TGrays): TJoulesPerKilogram; inline;
operator := (AQuantity: TJoulesPerKilogram): TGrays; inline;
operator := (AQuantity: TNewtonsSecond): TKilogramsMeterPerSecond; inline;
operator := (AQuantity: TKilogramsMeterPerSecond): TNewtonsSecond; inline;
operator := (AQuantity: TTeslasMeter): TNewtonsPerAmpere; inline;
operator := (AQuantity: TNewtonsPerAmpere): TTeslasMeter; inline;
operator := (AQuantity: TVoltsPerMeter): TNewtonsPerCoulomb; inline;
operator := (AQuantity: TNewtonsPerCoulomb): TVoltsPerMeter; inline;
operator := (AQuantity: double): TRadians; inline;
operator := (AQuantity: TRadians): double; inline;
operator := (AQuantity: TRadiansPerSquareSecond): TSquareHertz; inline;
operator := (AQuantity: TSquareHertz): TRadiansPerSquareSecond; inline;
operator := (AQuantity: TNewtonsSquareMeterPerCoulomb): TVoltsMeter; inline;
operator := (AQuantity: TVoltsMeter): TNewtonsSquareMeterPerCoulomb; inline;

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

uses
  Math;

{ TQuantity }

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
{ Unit of TAmpereUnit }

class function TAmpereUnit.Symbol: string;
begin
  result := 'A';
end;

class function TAmpereUnit.Name: string;
begin
  result := 'ampere';
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

{ Unit of TBequerelUnit }

class function TBequerelUnit.Symbol: string;
begin
  result := 'Bq';
end;

class function TBequerelUnit.Name: string;
begin
  result := 'bequerel';
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

{ Unit of TCoulombUnit }

class function TCoulombUnit.Symbol: string;
begin
  result := 'C';
end;

class function TCoulombUnit.Name: string;
begin
  result := 'coulomb';
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

{ Unit of TCoulombPerSquareMeterUnit }

class function TCoulombPerSquareMeterUnit.Symbol: string;
begin
  result := 'C/m2';
end;

class function TCoulombPerSquareMeterUnit.Name: string;
begin
  result := 'coulomb per square meter';
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

{ Unit of TCubicMeterUnit }

class function TCubicMeterUnit.Symbol: string;
begin
  result := 'm3';
end;

class function TCubicMeterUnit.Name: string;
begin
  result := 'cubic meter';
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

{ Unit of TFaradUnit }

class function TFaradUnit.Symbol: string;
begin
  result := 'F';
end;

class function TFaradUnit.Name: string;
begin
  result := 'farad';
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

{ Unit of THenryUnit }

class function THenryUnit.Symbol: string;
begin
  result := 'H';
end;

class function THenryUnit.Name: string;
begin
  result := 'henry';
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

{ Unit of TJouleUnit }

class function TJouleUnit.Symbol: string;
begin
  result := 'J';
end;

class function TJouleUnit.Name: string;
begin
  result := 'joule';
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

{ Unit of TJoulePerMoleUnit }

class function TJoulePerMoleUnit.Symbol: string;
begin
  result := 'J/mol';
end;

class function TJoulePerMoleUnit.Name: string;
begin
  result := 'joule per mole';
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

{ Unit of TKatalUnit }

class function TKatalUnit.Symbol: string;
begin
  result := 'kat';
end;

class function TKatalUnit.Name: string;
begin
  result := 'katal';
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

{ Unit of TKelvinPerMeterUnit }

class function TKelvinPerMeterUnit.Symbol: string;
begin
  result := 'K/m';
end;

class function TKelvinPerMeterUnit.Name: string;
begin
  result := 'kelvin per meter';
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

{ Unit of TKilogramKelvinUnit }

class function TKilogramKelvinUnit.Symbol: string;
begin
  result := 'kg·K';
end;

class function TKilogramKelvinUnit.Name: string;
begin
  result := 'kilogram kelvin';
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

{ Unit of TKilogramPerCubicMeterUnit }

class function TKilogramPerCubicMeterUnit.Symbol: string;
begin
  result := 'kg/m3';
end;

class function TKilogramPerCubicMeterUnit.Name: string;
begin
  result := 'kilogram per cubic meter';
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
  result := 'kg·m2/s';
end;

class function TKilogramSquareMeterPerSecondUnit.Name: string;
begin
  result := 'kilogram square meter per second';
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

{ Unit of TMeterUnit }

class function TMeterUnit.Symbol: string;
begin
  result := 'm';
end;

class function TMeterUnit.Name: string;
begin
  result := 'meter';
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

{ Unit of TMeterPerAmpereUnit }

class function TMeterPerAmpereUnit.Symbol: string;
begin
  result := 'm/A';
end;

class function TMeterPerAmpereUnit.Name: string;
begin
  result := 'meter per ampere';
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

{ Unit of TMoleUnit }

class function TMoleUnit.Symbol: string;
begin
  result := 'mol';
end;

class function TMoleUnit.Name: string;
begin
  result := 'mole';
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

{ Unit of TNewtonUnit }

class function TNewtonUnit.Symbol: string;
begin
  result := 'N';
end;

class function TNewtonUnit.Name: string;
begin
  result := 'newton';
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

{ Unit of TNewtonMeterPerRadianUnit }

class function TNewtonMeterPerRadianUnit.Symbol: string;
begin
  result := 'N·m/rad';
end;

class function TNewtonMeterPerRadianUnit.Name: string;
begin
  result := 'newton meter per radian';
end;

{ Unit of TNewtonPerAmpereUnit }

class function TNewtonPerAmpereUnit.Symbol: string;
begin
  result := 'N/A';
end;

class function TNewtonPerAmpereUnit.Name: string;
begin
  result := 'newton per ampere';
end;

{ Unit of TNewtonPerCoulombUnit }

class function TNewtonPerCoulombUnit.Symbol: string;
begin
  result := 'N/C';
end;

class function TNewtonPerCoulombUnit.Name: string;
begin
  result := 'newton per coulomb';
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

{ Unit of TNewtonPerSquareCoulombUnit }

class function TNewtonPerSquareCoulombUnit.Symbol: string;
begin
  result := 'N/C2';
end;

class function TNewtonPerSquareCoulombUnit.Name: string;
begin
  result := 'newton per square coulomb';
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

{ Unit of TNewtonSquareMeterUnit }

class function TNewtonSquareMeterUnit.Symbol: string;
begin
  result := 'N·m2';
end;

class function TNewtonSquareMeterUnit.Name: string;
begin
  result := 'newton square meter';
end;

{ Unit of TNewtonSquareMeterPerCoulombUnit }

class function TNewtonSquareMeterPerCoulombUnit.Symbol: string;
begin
  result := 'N·m2/C';
end;

class function TNewtonSquareMeterPerCoulombUnit.Name: string;
begin
  result := 'newton square meter per coulomb';
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

{ Unit of TNewtonSquareMeterPerSquareKilogramUnit }

class function TNewtonSquareMeterPerSquareKilogramUnit.Symbol: string;
begin
  result := 'N·m2/kg2';
end;

class function TNewtonSquareMeterPerSquareKilogramUnit.Name: string;
begin
  result := 'newton square meter per square kilogram';
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

{ Unit of TOhmUnit }

class function TOhmUnit.Symbol: string;
begin
  result := 'Ω';
end;

class function TOhmUnit.Name: string;
begin
  result := 'ohm';
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

{ Unit of TPascalUnit }

class function TPascalUnit.Symbol: string;
begin
  result := 'Pa';
end;

class function TPascalUnit.Name: string;
begin
  result := 'pascal';
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

{ Unit of TQuarticKelvinUnit }

class function TQuarticKelvinUnit.Symbol: string;
begin
  result := 'K4';
end;

class function TQuarticKelvinUnit.Name: string;
begin
  result := 'quartic kelvin';
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

{ Unit of TRadianUnit }

class function TRadianUnit.Symbol: string;
begin
  result := 'rad';
end;

class function TRadianUnit.Name: string;
begin
  result := 'radian';
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

{ Unit of TReciprocalKelvinUnit }

class function TReciprocalKelvinUnit.Symbol: string;
begin
  result := '1/K';
end;

class function TReciprocalKelvinUnit.Name: string;
begin
  result := 'reciprocal kelvin';
end;

{ Unit of TReciprocalMeterUnit }

class function TReciprocalMeterUnit.Symbol: string;
begin
  result := '1/m';
end;

class function TReciprocalMeterUnit.Name: string;
begin
  result := 'reciprocal meter';
end;

{ Unit of TReciprocalSquareMeterUnit }

class function TReciprocalSquareMeterUnit.Symbol: string;
begin
  result := '1/m2';
end;

class function TReciprocalSquareMeterUnit.Name: string;
begin
  result := 'reciprocal square meter';
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

{ Unit of TSexticMeterUnit }

class function TSexticMeterUnit.Symbol: string;
begin
  result := 'm6';
end;

class function TSexticMeterUnit.Name: string;
begin
  result := 'sextic meter';
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

{ Unit of TSievertUnit }

class function TSievertUnit.Symbol: string;
begin
  result := 'Sv';
end;

class function TSievertUnit.Name: string;
begin
  result := 'sievert';
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

{ Unit of TSquareAmperePerMeterUnit }

class function TSquareAmperePerMeterUnit.Symbol: string;
begin
  result := 'A2/m';
end;

class function TSquareAmperePerMeterUnit.Name: string;
begin
  result := 'square ampere per meter';
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

{ Unit of TSquareCoulombPerMeterUnit }

class function TSquareCoulombPerMeterUnit.Symbol: string;
begin
  result := 'C2/m';
end;

class function TSquareCoulombPerMeterUnit.Name: string;
begin
  result := 'square coulomb per meter';
end;

{ Unit of TSquareCoulombPerSquareMeterUnit }

class function TSquareCoulombPerSquareMeterUnit.Symbol: string;
begin
  result := 'C2/m2';
end;

class function TSquareCoulombPerSquareMeterUnit.Name: string;
begin
  result := 'square coulomb per square meter';
end;

{ Unit of TSquareCoulombPerNewtonUnit }

class function TSquareCoulombPerNewtonUnit.Symbol: string;
begin
  result := 'C2/N';
end;

class function TSquareCoulombPerNewtonUnit.Name: string;
begin
  result := 'square coulomb per newton';
end;

{ Unit of TSquareCoulombPerNewtonPerSquareMeterUnit }

class function TSquareCoulombPerNewtonPerSquareMeterUnit.Symbol: string;
begin
  result := 'C2/N/m2';
end;

class function TSquareCoulombPerNewtonPerSquareMeterUnit.Name: string;
begin
  result := 'square coulomb per newton per square meter';
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

{ Unit of TSquareKelvinUnit }

class function TSquareKelvinUnit.Symbol: string;
begin
  result := 'K2';
end;

class function TSquareKelvinUnit.Name: string;
begin
  result := 'square kelvin';
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

{ Unit of TSquareMeterUnit }

class function TSquareMeterUnit.Symbol: string;
begin
  result := 'm2';
end;

class function TSquareMeterUnit.Name: string;
begin
  result := 'square meter';
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

{ Unit of TSquareMeterPerCoulombUnit }

class function TSquareMeterPerCoulombUnit.Symbol: string;
begin
  result := 'm2/C';
end;

class function TSquareMeterPerCoulombUnit.Name: string;
begin
  result := 'square meter per coulomb';
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

{ Unit of TSquareMeterPerSquareKilogramUnit }

class function TSquareMeterPerSquareKilogramUnit.Symbol: string;
begin
  result := 'm2/kg2';
end;

class function TSquareMeterPerSquareKilogramUnit.Name: string;
begin
  result := 'square meter per square kilogram';
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

{ Unit of TSquareMeterPerSquareSecondUnit }

class function TSquareMeterPerSquareSecondUnit.Symbol: string;
begin
  result := 'm2/s2';
end;

class function TSquareMeterPerSquareSecondUnit.Name: string;
begin
  result := 'square meter per square second';
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

{ Unit of TSquareRadianUnit }

class function TSquareRadianUnit.Symbol: string;
begin
  result := 'rad2';
end;

class function TSquareRadianUnit.Name: string;
begin
  result := 'square radian';
end;

{ Unit of TSquareRadianPerSquareSecondUnit }

class function TSquareRadianPerSquareSecondUnit.Symbol: string;
begin
  result := 'rad2/s2';
end;

class function TSquareRadianPerSquareSecondUnit.Name: string;
begin
  result := 'square rad per square second';
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

{ Unit of TSquareVoltUnit }

class function TSquareVoltUnit.Symbol: string;
begin
  result := 'V2';
end;

class function TSquareVoltUnit.Name: string;
begin
  result := 'square volt';
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

{ Unit of TTeslaUnit }

class function TTeslaUnit.Symbol: string;
begin
  result := 'T';
end;

class function TTeslaUnit.Name: string;
begin
  result := 'tesla';
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

{ Unit of TTeslaMeterPerAmpereUnit }

class function TTeslaMeterPerAmpereUnit.Symbol: string;
begin
  result := 'T·m/A';
end;

class function TTeslaMeterPerAmpereUnit.Name: string;
begin
  result := 'tesla meter per ampere';
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

{ Unit of TVoltUnit }

class function TVoltUnit.Symbol: string;
begin
  result := 'V';
end;

class function TVoltUnit.Name: string;
begin
  result := 'volt';
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

{ Unit of TVoltPerMeterUnit }

class function TVoltPerMeterUnit.Symbol: string;
begin
  result := 'V/m';
end;

class function TVoltPerMeterUnit.Name: string;
begin
  result := 'volt per meter';
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

{ Unit of TWattPerKelvinUnit }

class function TWattPerKelvinUnit.Symbol: string;
begin
  result := 'W/K';
end;

class function TWattPerKelvinUnit.Name: string;
begin
  result := 'watt per kelvin';
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

{ Unit of TWattPerMeterPerKelvinUnit }

class function TWattPerMeterPerKelvinUnit.Symbol: string;
begin
  result := 'W/m/K';
end;

class function TWattPerMeterPerKelvinUnit.Name: string;
begin
  result := 'watt per meter per kelvin';
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

{ Unit of TWattPerSquareMeterUnit }

class function TWattPerSquareMeterUnit.Symbol: string;
begin
  result := 'W/m2';
end;

class function TWattPerSquareMeterUnit.Name: string;
begin
  result := 'watt per square meter';
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

{ Unit of TWattPerSquareMeterPerQuarticKelvinUnit }

class function TWattPerSquareMeterPerQuarticKelvinUnit.Symbol: string;
begin
  result := 'W/m2/K4';
end;

class function TWattPerSquareMeterPerQuarticKelvinUnit.Name: string;
begin
  result := 'watt per square meter per quartic kelvin';
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

{ Unit of TNewtonMillimeterUnit }

class function TNewtonMillimeterUnit.Symbol: string;
begin
  result := 'N·mm';
end;

class function TNewtonMillimeterUnit.Name: string;
begin
  result := 'newton millimeter';
end;

class function TNewtonMillimeterUnit.Factor: double;
begin
  result := 1E-03;
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

{ Unit of TNewtonMillimeterPerRadianUnit }

class function TNewtonMillimeterPerRadianUnit.Symbol: string;
begin
  result := 'N·mm/rad';
end;

class function TNewtonMillimeterPerRadianUnit.Name: string;
begin
  result := 'newton millimeter per radian';
end;

class function TNewtonMillimeterPerRadianUnit.Factor: double;
begin
  result := 1E-03;
end;

{ Unit of TNewtonMillimeterPerDegreeUnit }

class function TNewtonMillimeterPerDegreeUnit.Symbol: string;
begin
  result := 'N·mm/deg';
end;

class function TNewtonMillimeterPerDegreeUnit.Name: string;
begin
  result := 'newton millimeter per degree';
end;

class function TNewtonMillimeterPerDegreeUnit.Factor: double;
begin
  result := 9/50/Pi;
end;

{ Unit of TNewtonPerDecimeterUnit }

class function TNewtonPerDecimeterUnit.Symbol: string;
begin
  result := 'N/dm';
end;

class function TNewtonPerDecimeterUnit.Name: string;
begin
  result := 'newton per decimeter';
end;

class function TNewtonPerDecimeterUnit.Factor: double;
begin
  result := 1E+01;
end;

{ Unit of TNewtonPerCentimeterUnit }

class function TNewtonPerCentimeterUnit.Symbol: string;
begin
  result := 'N/cm';
end;

class function TNewtonPerCentimeterUnit.Name: string;
begin
  result := 'newton per centimeter';
end;

class function TNewtonPerCentimeterUnit.Factor: double;
begin
  result := 1E+02;
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

{ Combining units }

// main definition [ A/m ] = [ A ] / [ m ]
operator /(const ALeft: TAmpereIdentifier; const ARight: TMeterIdentifier): TAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TAmpereIdentifier; const ARight: TAmperePerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TAmperePerMeterIdentifier; const ARight: TMeterIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TAmperePerMeterIdentifier): TAmpereIdentifier;
begin end;

// alternative definition [ A/m ] = [ A ] * [ 1/m ]
operator *(const ALeft: TAmpereIdentifier; const ARight: TReciprocalMeterIdentifier): TAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TAmperePerMeterIdentifier; const ARight: TAmpereIdentifier): TReciprocalMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TAmpereIdentifier): TAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TAmperePerMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TAmpereIdentifier;
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

// main definition [ C/m ] = [ C ] / [ m ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TMeterIdentifier): TCoulombPerMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TCoulombPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombPerMeterIdentifier; const ARight: TMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TCoulombPerMeterIdentifier): TCoulombIdentifier;
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

// alternative definition [ C/m2 ] = [ C ] * [ 1/m2 ]
operator *(const ALeft: TCoulombIdentifier; const ARight: TReciprocalSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TCoulombIdentifier): TReciprocalSquareMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalSquareMeterIdentifier; const ARight: TCoulombIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TReciprocalSquareMeterIdentifier): TCoulombIdentifier;
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

// main definition [ m3 ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TSquareMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TMeterIdentifier): TSquareMeterIdentifier;
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

// main definition [ Hz ] = 1 / [ s ]
operator /(const ALeft: double; const ARight: TSecondIdentifier): THertzIdentifier;
begin end;

operator /(const ALeft: double; const ARight: THertzIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TSecondIdentifier): double;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: THertzIdentifier): double;
begin end;

// main definition [ J ] = [ Pa ] * [ m3 ]
operator *(const ALeft: TPascalIdentifier; const ARight: TCubicMeterIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TPascalIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TPascalIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TCubicMeterIdentifier): TPascalIdentifier;
begin end;

// alternative definition [ J ] = [ kg*m2 ] * [ rad2/s2 ]
operator *(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TKilogramSquareMeterIdentifier): TSquareRadianPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TSquareRadianPerSquareSecondIdentifier; const ARight: TKilogramSquareMeterIdentifier): TJouleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TKilogramSquareMeterIdentifier;
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

// main definition [ J/kg ] = [ J ] / [ kg ]
operator /(const ALeft: TJouleIdentifier; const ARight: TKilogramIdentifier): TJoulePerKilogramIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerKilogramIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TJoulePerKilogramIdentifier; const ARight: TKilogramIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TKilogramIdentifier; const ARight: TJoulePerKilogramIdentifier): TJouleIdentifier;
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
operator /(const ALeft: TJoulePerKilogramIdentifier; const ARight: TKelvinIdentifier): TJoulePerKilogramPerKelvinIdentifier;
begin end;

operator /(const ALeft: TJoulePerKilogramIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TJoulePerKilogramPerKelvinIdentifier; const ARight: TKelvinIdentifier): TJoulePerKilogramIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TJoulePerKilogramPerKelvinIdentifier): TJoulePerKilogramIdentifier;
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

// main definition [ J/mol ] = [ J ] / [ mol ]
operator /(const ALeft: TJouleIdentifier; const ARight: TMoleIdentifier): TJoulePerMoleIdentifier;
begin end;

operator /(const ALeft: TJouleIdentifier; const ARight: TJoulePerMoleIdentifier): TMoleIdentifier;
begin end;

operator *(const ALeft: TJoulePerMoleIdentifier; const ARight: TMoleIdentifier): TJouleIdentifier;
begin end;

operator *(const ALeft: TMoleIdentifier; const ARight: TJoulePerMoleIdentifier): TJouleIdentifier;
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

// main definition [ kat ] = [ mol ] / [ s ]
operator /(const ALeft: TMoleIdentifier; const ARight: TSecondIdentifier): TKatalIdentifier;
begin end;

operator /(const ALeft: TMoleIdentifier; const ARight: TKatalIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TKatalIdentifier; const ARight: TSecondIdentifier): TMoleIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TKatalIdentifier): TMoleIdentifier;
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

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TKelvinIdentifier): TKilogramKelvinIdentifier;
begin end;

operator /(const ALeft: TKilogramKelvinIdentifier; const ARight: TKilogramIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TKilogramIdentifier): TKilogramKelvinIdentifier;
begin end;

operator /(const ALeft: TKilogramKelvinIdentifier; const ARight: TKelvinIdentifier): TKilogramIdentifier;
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

// main definition [ kg/m3 ] = [ kg ] / [ m3 ]
operator /(const ALeft: TKilogramIdentifier; const ARight: TCubicMeterIdentifier): TKilogramPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TKilogramIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TCubicMeterIdentifier): TKilogramIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TKilogramIdentifier;
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

// main definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramSquareMeterIdentifier; const ARight: THertzIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: TKilogramSquareMeterIdentifier): THertzIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TKilogramSquareMeterIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: THertzIdentifier): TKilogramSquareMeterIdentifier;
begin end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TSecondIdentifier): TKilogramSquareMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TKilogramSquareMeterIdentifier; const ARight: TKilogramSquareMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TKilogramSquareMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TKilogramSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TKilogramSquareMeterPerSecondIdentifier): TKilogramSquareMeterIdentifier;
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

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeterIdentifier; const ARight: TKelvinIdentifier): TMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TMeterKelvinIdentifier; const ARight: TMeterIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TMeterIdentifier): TMeterKelvinIdentifier;
begin end;

operator /(const ALeft: TMeterKelvinIdentifier; const ARight: TKelvinIdentifier): TMeterIdentifier;
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

// main definition [ m/s ] = [ m ] / [ s ]
operator /(const ALeft: TMeterIdentifier; const ARight: TSecondIdentifier): TMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TMeterPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TMeterPerSecondIdentifier): TMeterIdentifier;
begin end;

// alternative definition [ m/s ] = [ m ] * [ hz ]
operator *(const ALeft: TMeterIdentifier; const ARight: THertzIdentifier): TMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: TMeterIdentifier): THertzIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TMeterIdentifier): TMeterPerSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSecondIdentifier; const ARight: THertzIdentifier): TMeterIdentifier;
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
operator *(const ALeft: TSquareRadianPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TMeterIdentifier): TSquareRadianPerSquareSecondIdentifier;
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

// main definition [ N ] = [ kg ] * [ m/s2 ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TNewtonIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TKilogramIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TKilogramIdentifier): TNewtonIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TKilogramIdentifier;
begin end;

// main definition [ Nm ] = [ N ] * [ m ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TMeterIdentifier): TNewtonMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonMeterIdentifier; const ARight: TNewtonIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TNewtonIdentifier): TNewtonMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonMeterIdentifier; const ARight: TMeterIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N*m/rad ] = [ N*m ] / [ rad ]
operator /(const ALeft: TNewtonMeterIdentifier; const ARight: TRadianIdentifier): TNewtonMeterPerRadianIdentifier;
begin end;

operator /(const ALeft: TNewtonMeterIdentifier; const ARight: TNewtonMeterPerRadianIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TNewtonMeterPerRadianIdentifier; const ARight: TRadianIdentifier): TNewtonMeterIdentifier;
begin end;

operator *(const ALeft: TRadianIdentifier; const ARight: TNewtonMeterPerRadianIdentifier): TNewtonMeterIdentifier;
begin end;

// main definition [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TAmpereIdentifier): TNewtonPerAmpereIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerAmpereIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TNewtonPerAmpereIdentifier; const ARight: TAmpereIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TNewtonPerAmpereIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TCoulombIdentifier): TNewtonPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerCoulombIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TCoulombIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TNewtonPerCoulombIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N/C ] = [ m/s ] * [ T ]
operator *(const ALeft: TMeterPerSecondIdentifier; const ARight: TTeslaIdentifier): TNewtonPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TMeterPerSecondIdentifier): TTeslaIdentifier;
begin end;

operator *(const ALeft: TTeslaIdentifier; const ARight: TMeterPerSecondIdentifier): TNewtonPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TTeslaIdentifier): TMeterPerSecondIdentifier;
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

// alternative definition [ N/m3 ] = [ kg/m3 ] * [ m/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TMeterPerSquareSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TNewtonPerCubicMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCubicMeterIdentifier; const ARight: TMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier;
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

// main definition [ N/C2 ] = [ N ] / [ C2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonPerSquareCoulombIdentifier; const ARight: TSquareCoulombIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombIdentifier; const ARight: TNewtonPerSquareCoulombIdentifier): TNewtonIdentifier;
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

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TNewtonIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TNewtonIdentifier;
begin end;

// main definition [ N*m2/C ] = [ N ] * [ m2/C ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterPerCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerCoulombIdentifier; const ARight: TNewtonIdentifier): TSquareMeterPerCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerCoulombIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerCoulombIdentifier; const ARight: TSquareMeterPerCoulombIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N*m2/C ] = [ N*m2 ] / [ C ]
operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerCoulombIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerCoulombIdentifier; const ARight: TCoulombIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TNewtonSquareMeterPerCoulombIdentifier): TNewtonSquareMeterIdentifier;
begin end;

// alternative definition [ N*m2/C ] = [ N/C ] * [ m2 ]
operator *(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TSquareMeterIdentifier): TNewtonSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerCoulombIdentifier; const ARight: TNewtonPerCoulombIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TNewtonPerCoulombIdentifier): TNewtonSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerCoulombIdentifier; const ARight: TSquareMeterIdentifier): TNewtonPerCoulombIdentifier;
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

// alternative definition [ N*m2/C2 ] = [ N ] / [ C2/m2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareCoulombPerSquareMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonIdentifier;
begin end;

// alternative definition [ N*m2/C2 ] = [ N/C ] / [ C/m2 ]
operator /(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TCoulombPerSquareMeterIdentifier): TNewtonPerCoulombIdentifier;
begin end;

operator *(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TNewtonPerCoulombIdentifier;
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

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TNewtonIdentifier): TSquareMeterPerSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramIdentifier; const ARight: TNewtonIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier;
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

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TNewtonIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TSquareKilogramPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramPerSquareMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareKilogramIdentifier): TNewtonIdentifier;
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

// main definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtonIdentifier; const ARight: TSecondIdentifier): TNewtonSecondIdentifier;
begin end;

operator /(const ALeft: TNewtonSecondIdentifier; const ARight: TNewtonIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TNewtonIdentifier): TNewtonSecondIdentifier;
begin end;

operator /(const ALeft: TNewtonSecondIdentifier; const ARight: TSecondIdentifier): TNewtonIdentifier;
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

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhmIdentifier; const ARight: TMeterIdentifier): TOhmMeterIdentifier;
begin end;

operator /(const ALeft: TOhmMeterIdentifier; const ARight: TOhmIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TOhmIdentifier): TOhmMeterIdentifier;
begin end;

operator /(const ALeft: TOhmMeterIdentifier; const ARight: TMeterIdentifier): TOhmIdentifier;
begin end;

// alternative definition [ Ω*m ] = [ Ω ] / [ 1/m ]
operator /(const ALeft: TOhmIdentifier; const ARight: TReciprocalMeterIdentifier): TOhmMeterIdentifier;
begin end;

operator /(const ALeft: TOhmIdentifier; const ARight: TOhmMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator *(const ALeft: TOhmMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TOhmMeterIdentifier): TOhmIdentifier;
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

// alternative definition [ Pa ] = [ N/m ] / [ m ]
operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TMeterIdentifier): TPascalIdentifier;
begin end;

operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TPascalIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TPascalIdentifier; const ARight: TMeterIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TPascalIdentifier): TNewtonPerMeterIdentifier;
begin end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramPerCubicMeterIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TPascalIdentifier;
begin end;

operator /(const ALeft: TPascalIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TSquareMeterPerSquareSecondIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareSecondIdentifier; const ARight: TKilogramPerCubicMeterIdentifier): TPascalIdentifier;
begin end;

operator /(const ALeft: TPascalIdentifier; const ARight: TSquareMeterPerSquareSecondIdentifier): TKilogramPerCubicMeterIdentifier;
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

// alternative definition [ Pa*s ] = [ Pa ] / [ Hz ]
operator /(const ALeft: TPascalIdentifier; const ARight: THertzIdentifier): TPascalSecondIdentifier;
begin end;

operator /(const ALeft: TPascalIdentifier; const ARight: TPascalSecondIdentifier): THertzIdentifier;
begin end;

operator *(const ALeft: TPascalSecondIdentifier; const ARight: THertzIdentifier): TPascalIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TPascalSecondIdentifier): TPascalIdentifier;
begin end;

// main definition [ K4 ] = [ K3 ] * [ K ]
operator *(const ALeft: TCubicKelvinIdentifier; const ARight: TKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TCubicKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TCubicKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TKelvinIdentifier): TCubicKelvinIdentifier;
begin end;

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TQuarticKelvinIdentifier; const ARight: TSquareKelvinIdentifier): TSquareKelvinIdentifier;
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

// main definition [ rad/s ] = [ rad ] / [ s ]
operator /(const ALeft: TRadianIdentifier; const ARight: TSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

operator /(const ALeft: TRadianIdentifier; const ARight: TRadianPerSecondIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TSecondIdentifier): TRadianIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TRadianIdentifier;
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

// main definition [ 1/K ] = 1 / [ K ]
operator /(const ALeft: double; const ARight: TKelvinIdentifier): TReciprocalKelvinIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TReciprocalKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TReciprocalKelvinIdentifier; const ARight: TKelvinIdentifier): double;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TReciprocalKelvinIdentifier): double;
begin end;

// main definition [ 1/m ] = 1 / [ m ]
operator /(const ALeft: double; const ARight: TMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TReciprocalMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TMeterIdentifier): double;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TReciprocalMeterIdentifier): double;
begin end;

// alternative definition [ 1/m ] = [ m ] / [ m2 ]
operator /(const ALeft: TMeterIdentifier; const ARight: TSquareMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TSquareMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TMeterIdentifier;
begin end;

// alternative definition [ 1/m ] = [ m2 ] / [ m3 ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TCubicMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TCubicMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TSquareMeterIdentifier;
begin end;

// alternative definition [ 1/m ] = [ m3 ] / [ m4 ]
operator /(const ALeft: TCubicMeterIdentifier; const ARight: TQuarticMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: TCubicMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TQuarticMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TQuarticMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TQuarticMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TCubicMeterIdentifier;
begin end;

// alternative definition [ 1/m ] = [ m4 ] / [ m5 ]
operator /(const ALeft: TQuarticMeterIdentifier; const ARight: TQuinticMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: TQuarticMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TQuinticMeterIdentifier): TQuarticMeterIdentifier;
begin end;

operator *(const ALeft: TQuinticMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TQuarticMeterIdentifier;
begin end;

// alternative definition [ 1/m ] = [ m5 ] / [ m6 ]
operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TSexticMeterIdentifier): TReciprocalMeterIdentifier;
begin end;

operator /(const ALeft: TQuinticMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TSexticMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalMeterIdentifier; const ARight: TSexticMeterIdentifier): TQuinticMeterIdentifier;
begin end;

operator *(const ALeft: TSexticMeterIdentifier; const ARight: TReciprocalMeterIdentifier): TQuinticMeterIdentifier;
begin end;

// main definition [ 1/m2 ] = [ 1 ] / [ m2 ]
operator /(const ALeft: double; const ARight: TSquareMeterIdentifier): TReciprocalSquareMeterIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TReciprocalSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): double;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TReciprocalSquareMeterIdentifier): double;
begin end;

//
operator /(const ALeft: TMeterIdentifier; const ARight: TCubicMeterIdentifier): TReciprocalSquareMeterIdentifier;
begin end;

operator /(const ALeft: TMeterIdentifier; const ARight: TReciprocalSquareMeterIdentifier): TCubicMeterIdentifier;
begin end;

operator *(const ALeft: TReciprocalSquareMeterIdentifier; const ARight: TCubicMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TCubicMeterIdentifier; const ARight: TReciprocalSquareMeterIdentifier): TMeterIdentifier;
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

// main definition [ S ] = 1 / [ Ω ]
operator /(const ALeft: double; const ARight: TOhmIdentifier): TSiemensIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TSiemensIdentifier): TOhmIdentifier;
begin end;

operator *(const ALeft: TSiemensIdentifier; const ARight: TOhmIdentifier): double;
begin end;

operator *(const ALeft: TOhmIdentifier; const ARight: TSiemensIdentifier): double;
begin end;

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmpereIdentifier; const ARight: TAmpereIdentifier): TSquareAmpereIdentifier;
begin end;

operator /(const ALeft: TSquareAmpereIdentifier; const ARight: TAmpereIdentifier): TAmpereIdentifier;
begin end;

// main definition [ A2/m ] = [ A2] / [ m ]
operator /(const ALeft: TSquareAmpereIdentifier; const ARight: TMeterIdentifier): TSquareAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareAmpereIdentifier; const ARight: TSquareAmperePerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TSquareAmperePerMeterIdentifier; const ARight: TMeterIdentifier): TSquareAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TSquareAmperePerMeterIdentifier): TSquareAmpereIdentifier;
begin end;

// alternative definition [ A2/m ] = [ A/m ] * [ A ]
operator *(const ALeft: TAmperePerMeterIdentifier; const ARight: TAmpereIdentifier): TSquareAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareAmperePerMeterIdentifier; const ARight: TAmperePerMeterIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TAmperePerMeterIdentifier): TSquareAmperePerMeterIdentifier;
begin end;

operator /(const ALeft: TSquareAmperePerMeterIdentifier; const ARight: TAmpereIdentifier): TAmperePerMeterIdentifier;
begin end;

// main definition [ C2 ] = [ C ] * [ C ]
operator *(const ALeft: TCoulombIdentifier; const ARight: TCoulombIdentifier): TSquareCoulombIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TCoulombIdentifier): TCoulombIdentifier;
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

// main definition [ C2/m2 ] = [ C2 ] / [ m2 ]
operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareMeterIdentifier): TSquareCoulombPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareCoulombPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TSquareCoulombPerSquareMeterIdentifier): TSquareCoulombIdentifier;
begin end;

// main definition [ C2/N ] = [ C2 ] / [ N ]
operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TNewtonIdentifier): TSquareCoulombPerNewtonIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareCoulombPerNewtonIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonIdentifier; const ARight: TNewtonIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareCoulombPerNewtonIdentifier): TSquareCoulombIdentifier;
begin end;

// main definition [ C2/N/m2 ] = [ C2 ] / [N*m2 ]
operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TNewtonSquareMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterIdentifier): TSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TNewtonSquareMeterIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = [ C2/N ] / [ m2 ]
operator /(const ALeft: TSquareCoulombPerNewtonIdentifier; const ARight: TSquareMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombPerNewtonIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TSquareMeterIdentifier): TSquareCoulombPerNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombPerNewtonIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = [ C2/m2 ] / [ N ]
operator /(const ALeft: TSquareCoulombPerSquareMeterIdentifier; const ARight: TNewtonIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareCoulombPerSquareMeterIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TNewtonIdentifier): TSquareCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TSquareCoulombPerSquareMeterIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TNewtonPerCoulombIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombPerSquareMeterIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonPerCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TNewtonPerCoulombIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

operator *(const ALeft: TNewtonPerCoulombIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TCoulombPerSquareMeterIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = [ A ] / [ V*m/s ]
operator /(const ALeft: TAmpereIdentifier; const ARight: TVoltMeterPerSecondIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TAmpereIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TVoltMeterPerSecondIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TVoltMeterPerSecondIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TVoltMeterPerSecondIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TAmpereIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombIdentifier; const ARight: TVoltMeterIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: TCoulombIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TVoltMeterIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TVoltMeterIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TVoltMeterIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TCoulombIdentifier;
begin end;

// alternative definition [ C2/N/m2 ] = 1 / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): TSquareCoulombPerNewtonPerSquareMeterIdentifier;
begin end;

operator /(const ALeft: double; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): TNewtonSquareMeterPerSquareCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareCoulombPerNewtonPerSquareMeterIdentifier; const ARight: TNewtonSquareMeterPerSquareCoulombIdentifier): double;
begin end;

operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombIdentifier; const ARight: TSquareCoulombPerNewtonPerSquareMeterIdentifier): double;
begin end;

// main definition [ Hz2 ] = [ Hz ] * [ Hz ]
operator *(const ALeft: THertzIdentifier; const ARight: THertzIdentifier): TSquareHertzIdentifier;
begin end;

operator /(const ALeft: TSquareHertzIdentifier; const ARight: THertzIdentifier): THertzIdentifier;
begin end;

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvinIdentifier; const ARight: TKelvinIdentifier): TSquareKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareKelvinIdentifier; const ARight: TKelvinIdentifier): TKelvinIdentifier;
begin end;

// main definition [ kg2 ]
operator *(const ALeft: TKilogramIdentifier; const ARight: TKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TSquareKilogramIdentifier; const ARight: TKilogramIdentifier): TKilogramIdentifier;
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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeterIdentifier; const ARight: TMeterIdentifier): TSquareMeterIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TMeterIdentifier): TMeterIdentifier;
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

// main definition [ m2/C ] = [ m2 ] / [ C ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TCoulombIdentifier): TSquareMeterPerCoulombIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerCoulombIdentifier): TCoulombIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerCoulombIdentifier; const ARight: TCoulombIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TCoulombIdentifier; const ARight: TSquareMeterPerCoulombIdentifier): TSquareMeterIdentifier;
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

// main definition [ m2/kg2 ] = [ m2 ] / [ kg2 ]
operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareKilogramIdentifier): TSquareMeterPerSquareKilogramIdentifier;
begin end;

operator /(const ALeft: TSquareMeterIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareKilogramIdentifier;
begin end;

operator *(const ALeft: TSquareMeterPerSquareKilogramIdentifier; const ARight: TSquareKilogramIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TSquareKilogramIdentifier; const ARight: TSquareMeterPerSquareKilogramIdentifier): TSquareMeterIdentifier;
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

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeterIdentifier; const ARight: TQuarticKelvinIdentifier): TSquareMeterQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TQuarticKelvinIdentifier; const ARight: TSquareMeterIdentifier): TSquareMeterQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TSquareMeterQuarticKelvinIdentifier; const ARight: TQuarticKelvinIdentifier): TSquareMeterIdentifier;
begin end;

// main definition [ rad2 ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadianIdentifier; const ARight: TRadianIdentifier): TSquareRadianIdentifier;
begin end;

operator /(const ALeft: TSquareRadianIdentifier; const ARight: TRadianIdentifier): TRadianIdentifier;
begin end;

// main definition [ rad2/s2 ] = [ rad2 ] / [ s2 ]
operator /(const ALeft: TSquareRadianIdentifier; const ARight: TSquareSecondIdentifier): TSquareRadianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareRadianIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator *(const ALeft: TSquareRadianPerSquareSecondIdentifier; const ARight: TSquareSecondIdentifier): TSquareRadianIdentifier;
begin end;

operator *(const ALeft: TSquareSecondIdentifier; const ARight: TSquareRadianPerSquareSecondIdentifier): TSquareRadianIdentifier;
begin end;

// alternative definition [ rad2/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadianPerSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TSquareRadianPerSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareRadianPerSquareSecondIdentifier; const ARight: TRadianPerSecondIdentifier): TRadianPerSecondIdentifier;
begin end;

// main definition [ s2 ] = [ s ] * [ s ]
operator *(const ALeft: TSecondIdentifier; const ARight: TSecondIdentifier): TSquareSecondIdentifier;
begin end;

operator /(const ALeft: TSquareSecondIdentifier; const ARight: TSecondIdentifier): TSecondIdentifier;
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

// main definition [ T ] = [ Wb ] / [ m2 ]
operator /(const ALeft: TWeberIdentifier; const ARight: TSquareMeterIdentifier): TTeslaIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TTeslaIdentifier): TSquareMeterIdentifier;
begin end;

operator *(const ALeft: TTeslaIdentifier; const ARight: TSquareMeterIdentifier): TWeberIdentifier;
begin end;

operator *(const ALeft: TSquareMeterIdentifier; const ARight: TTeslaIdentifier): TWeberIdentifier;
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

// main definition [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslaMeterIdentifier; const ARight: TAmpereIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TAmpereIdentifier;
begin end;

operator *(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TAmpereIdentifier): TTeslaMeterIdentifier;
begin end;

operator *(const ALeft: TAmpereIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TTeslaMeterIdentifier;
begin end;

// alternative definition [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslaPerAmpereIdentifier; const ARight: TMeterIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TTeslaPerAmpereIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TTeslaPerAmpereIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TMeterIdentifier): TTeslaPerAmpereIdentifier;
begin end;

// alternative definition [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslaIdentifier; const ARight: TMeterPerAmpereIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TTeslaIdentifier): TMeterPerAmpereIdentifier;
begin end;

operator *(const ALeft: TMeterPerAmpereIdentifier; const ARight: TTeslaIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TMeterPerAmpereIdentifier): TTeslaIdentifier;
begin end;

// alternative definition [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslaIdentifier; const ARight: TAmperePerMeterIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TTeslaIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TAmperePerMeterIdentifier;
begin end;

operator *(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TAmperePerMeterIdentifier): TTeslaIdentifier;
begin end;

operator *(const ALeft: TAmperePerMeterIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TTeslaIdentifier;
begin end;

// alternative definition [ T*m/A ] = [ N/m ] / [ A2/m ]
operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TSquareAmperePerMeterIdentifier): TTeslaMeterPerAmpereIdentifier;
begin end;

operator /(const ALeft: TNewtonPerMeterIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TSquareAmperePerMeterIdentifier;
begin end;

operator *(const ALeft: TTeslaMeterPerAmpereIdentifier; const ARight: TSquareAmperePerMeterIdentifier): TNewtonPerMeterIdentifier;
begin end;

operator *(const ALeft: TSquareAmperePerMeterIdentifier; const ARight: TTeslaMeterPerAmpereIdentifier): TNewtonPerMeterIdentifier;
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

// main definition [ V/m ] = [ V ] / [ m ]
operator /(const ALeft: TVoltIdentifier; const ARight: TMeterIdentifier): TVoltPerMeterIdentifier;
begin end;

operator /(const ALeft: TVoltIdentifier; const ARight: TVoltPerMeterIdentifier): TMeterIdentifier;
begin end;

operator *(const ALeft: TVoltPerMeterIdentifier; const ARight: TMeterIdentifier): TVoltIdentifier;
begin end;

operator *(const ALeft: TMeterIdentifier; const ARight: TVoltPerMeterIdentifier): TVoltIdentifier;
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

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJouleIdentifier; const ARight: THertzIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TJouleIdentifier): THertzIdentifier;
begin end;

operator *(const ALeft: THertzIdentifier; const ARight: TJouleIdentifier): TWattIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: THertzIdentifier): TJouleIdentifier;
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

// main definition [ W/K ] = [ W ] / [ K ]
operator /(const ALeft: TWattIdentifier; const ARight: TKelvinIdentifier): TWattPerKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerKelvinIdentifier): TKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerKelvinIdentifier; const ARight: TKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TKelvinIdentifier; const ARight: TWattPerKelvinIdentifier): TWattIdentifier;
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

// main definition [ W/K4 ] = [ W ] / [ K4 ]
operator /(const ALeft: TWattIdentifier; const ARight: TQuarticKelvinIdentifier): TWattPerQuarticKelvinIdentifier;
begin end;

operator /(const ALeft: TWattIdentifier; const ARight: TWattPerQuarticKelvinIdentifier): TQuarticKelvinIdentifier;
begin end;

operator *(const ALeft: TWattPerQuarticKelvinIdentifier; const ARight: TQuarticKelvinIdentifier): TWattIdentifier;
begin end;

operator *(const ALeft: TQuarticKelvinIdentifier; const ARight: TWattPerQuarticKelvinIdentifier): TWattIdentifier;
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

// main definition [ Wb ] = [ V ] * [ s ]
operator *(const ALeft: TVoltIdentifier; const ARight: TSecondIdentifier): TWeberIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TVoltIdentifier): TSecondIdentifier;
begin end;

operator *(const ALeft: TSecondIdentifier; const ARight: TVoltIdentifier): TWeberIdentifier;
begin end;

operator /(const ALeft: TWeberIdentifier; const ARight: TSecondIdentifier): TVoltIdentifier;
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

// main definition [ N*m/deg ] = [ N*m ] / [ deg ]
operator /(const ALeft: TNewtonMeterIdentifier; const ARight: TDegreeIdentifier): TNewtonMeterPerDegreeIdentifier;
begin end;

// main definition [ N*mm/rad ] = [ N*mm ] / [ rad ]
operator /(const ALeft: TNewtonMillimeterIdentifier; const ARight: TRadianIdentifier): TNewtonMillimeterPerRadianIdentifier;
begin end;

// main definition [ N*mm/deg ] = [ N*mm ] / [ deg ]
operator /(const ALeft: TNewtonMillimeterIdentifier; const ARight: TDegreeIdentifier): TNewtonMillimeterPerDegreeIdentifier;
begin end;

// main definition [ N/dm ] = [ N ] / [ dm ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TDecimeterIdentifier): TNewtonPerDecimeterIdentifier;
begin end;

// main definition [ N/cm ] = [ N ] / [ cm ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TCentimeterIdentifier): TNewtonPerCentimeterIdentifier;
begin end;

// main definition [ N/mm ] = [ N ] / [ mm ]
operator /(const ALeft: TNewtonIdentifier; const ARight: TMillimeterIdentifier): TNewtonPerMillimeterIdentifier;
begin end;

{ Combining quantities }

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

// alternative definition [ A/m ] = [ A ] * [ 1/m ]
operator *(const ALeft: TAmperes; const ARight: TReciprocalMeters): TAmperesPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAmperesPerMeter; const ARight: TAmperes): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TAmperes): TAmperesPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TAmperesPerMeter; const ARight: TReciprocalMeters): TAmperes;
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

// alternative definition [ C/m2 ] = [ C ] * [ 1/m2 ]
operator *(const ALeft: TCoulombs; const ARight: TReciprocalSquareMeters): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TCoulombs): TReciprocalSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalSquareMeters; const ARight: TCoulombs): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TReciprocalSquareMeters): TCoulombs;
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

// main definition [ J ] = [ Pa ] * [ m3 ]
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

// alternative definition [ J ] = [ kg*m2 ] * [ rad2/s2 ]
operator *(const ALeft: TKilogramsSquareMeter; const ARight: TSquareRadiansPerSquareSecond): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TKilogramsSquareMeter): TSquareRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TKilogramsSquareMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TSquareRadiansPerSquareSecond): TKilogramsSquareMeter;
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

// main definition [ J/kg ] = [ J ] / [ kg ]
operator /(const ALeft: TJoules; const ARight: TKilograms): TJoulesPerKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogram): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogram; const ARight: TKilograms): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilograms; const ARight: TJoulesPerKilogram): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ J/kg/K ] = [ J ] / [ kg*K ]
operator /(const ALeft: TJoules; const ARight: TKilogramsKelvin): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerKilogramPerKelvin): TKilogramsKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKilogramsKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramsKelvin; const ARight: TJoulesPerKilogramPerKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ J/kg/K ] = [ J/kg ] / [ K ]
operator /(const ALeft: TJoulesPerKilogram; const ARight: TKelvins): TJoulesPerKilogramPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoulesPerKilogram; const ARight: TJoulesPerKilogramPerKelvin): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerKilogramPerKelvin; const ARight: TKelvins): TJoulesPerKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TJoulesPerKilogramPerKelvin): TJoulesPerKilogram;
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

// main definition [ J/mol/K ] = [ J ] / [ mol * K ]
operator /(const ALeft: TJoules; const ARight: TMolesKelvin): TJoulesPerMolePerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TJoulesPerMolePerKelvin): TMolesKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TJoulesPerMolePerKelvin; const ARight: TMolesKelvin): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMolesKelvin; const ARight: TJoulesPerMolePerKelvin): TJoules;
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

// main definition [ kg*K] = [ kg ] * [ K ]
operator *(const ALeft: TKilograms; const ARight: TKelvins): TKilogramsKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsKelvin; const ARight: TKilograms): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TKilograms): TKilogramsKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsKelvin; const ARight: TKelvins): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ kg*m/s ] = [kg ] * [ m/s ]
operator *(const ALeft: TKilograms; const ARight: TMetersPerSecond): TKilogramsMeterPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsMeterPerSecond; const ARight: TKilograms): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerSecond; const ARight: TKilograms): TKilogramsMeterPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsMeterPerSecond; const ARight: TMetersPerSecond): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// main definition [ kg*m2 ] = [ kg ] * [ m2 ]
operator *(const ALeft: TKilograms; const ARight: TSquareMeters): TKilogramsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsSquareMeter; const ARight: TKilograms): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TKilograms): TKilogramsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsSquareMeter; const ARight: TSquareMeters): TKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ kg*m2/s ] = [ kg*m2 ] * [ Hz ]
operator *(const ALeft: TKilogramsSquareMeter; const ARight: THertz): TKilogramsSquareMeterPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: TKilogramsSquareMeter): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: TKilogramsSquareMeter): TKilogramsSquareMeterPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: THertz): TKilogramsSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ kg*m2/s ] = [ kg*m2 ] / [ s ]
operator /(const ALeft: TKilogramsSquareMeter; const ARight: TSeconds): TKilogramsSquareMeterPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TKilogramsSquareMeter; const ARight: TKilogramsSquareMeterPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKilogramsSquareMeterPerSecond; const ARight: TSeconds): TKilogramsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TKilogramsSquareMeterPerSecond): TKilogramsSquareMeter;
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

// main definition [ m*K ] = [ m ] * [ K ]
operator *(const ALeft: TMeters; const ARight: TKelvins): TMetersKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersKelvin; const ARight: TMeters): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TMeters): TMetersKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersKelvin; const ARight: TKelvins): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// alternative definition [ m/s ] = [ m ] * [ hz ]
operator *(const ALeft: TMeters; const ARight: THertz): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: TMeters): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: TMeters): TMetersPerSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSecond; const ARight: THertz): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
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
operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TMeters): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TSquareRadiansPerSquareSecond): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSquareRadiansPerSquareSecond): TMetersPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMetersPerSquareSecond; const ARight: TMeters): TSquareRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ mol*K ] = [ mol ] * [ K ]
operator *(const ALeft: TMoles; const ARight: TKelvins): TMolesKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMolesKelvin; const ARight: TMoles): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TMoles): TMolesKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TMolesKelvin; const ARight: TKelvins): TMoles;
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

// main definition [ Nm ] = [ N ] * [ m ]
operator *(const ALeft: TNewtons; const ARight: TMeters): TNewtonsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsMeter; const ARight: TNewtons): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TNewtons): TNewtonsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsMeter; const ARight: TMeters): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N*m/rad ] = [ N*m ] / [ rad ]
operator /(const ALeft: TNewtonsMeter; const ARight: TRadians): TNewtonsMeterPerRadian;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsMeter; const ARight: TNewtonsMeterPerRadian): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsMeterPerRadian; const ARight: TRadians): TNewtonsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TRadians; const ARight: TNewtonsMeterPerRadian): TNewtonsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ N/A ] = [ N ] / [ A ]
operator /(const ALeft: TNewtons; const ARight: TAmperes): TNewtonsPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerAmpere): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerAmpere; const ARight: TAmperes): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TNewtonsPerAmpere): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ N/C ] = [ N ] / [ C ]
operator /(const ALeft: TNewtons; const ARight: TCoulombs): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsPerCoulomb): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TCoulombs): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TNewtonsPerCoulomb): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N/C ] = [ m/s ] * [ T ]
operator *(const ALeft: TMetersPerSecond; const ARight: TTeslas): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TMetersPerSecond): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslas; const ARight: TMetersPerSecond): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TTeslas): TMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// main definition [ N*m2 ] = [ N ] * [ m2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMeters): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtons): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtons): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareMeters): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N*m2/C ] = [ N ] * [ m2/C ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerCoulomb): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TNewtons): TSquareMetersPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerCoulomb; const ARight: TNewtons): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TSquareMetersPerCoulomb): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/C ] = [ N*m2 ] / [ C ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TCoulombs): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerCoulomb): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TCoulombs): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TNewtonsSquareMeterPerCoulomb): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/C ] = [ N/C ] * [ m2 ]
operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TSquareMeters): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TNewtonsPerCoulomb): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerCoulomb): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerCoulomb; const ARight: TSquareMeters): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ N*m2/C2 ] = [ N ] * [ m2/C2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareCoulomb): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TNewtons): TSquareMetersPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareCoulomb; const ARight: TNewtons): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareMetersPerSquareCoulomb): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N*m2 ] / [ C2 ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareCoulombs): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombs): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombs; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N/C2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TNewtonsPerSquareCoulomb): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareCoulomb): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareMeters): TNewtonsPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N ] / [ C2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareCoulombsPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerSquareMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ N/C ] / [ C/m2 ]
operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TCoulombsPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsPerCoulomb; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TCoulombsPerSquareMeter): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/C2 ] = [ J ] / [ C2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareCoulombsPerMeter): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ N*m2/kg2 ] = [ N ] * [ m2/kg2 ]
operator *(const ALeft: TNewtons; const ARight: TSquareMetersPerSquareKilogram): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TNewtons): TSquareMetersPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareKilogram; const ARight: TNewtons): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareMetersPerSquareKilogram): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ N*m2 ] / [ kg2 ]
operator /(const ALeft: TNewtonsSquareMeter; const ARight: TSquareKilograms): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilograms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilograms): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilograms; const ARight: TNewtonsSquareMeterPerSquareKilogram): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ N/kg2 ] * [ m2 ]
operator *(const ALeft: TNewtonsPerSquareKilogram; const ARight: TSquareMeters): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TNewtonsPerSquareKilogram): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TNewtonsPerSquareKilogram): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareMeters): TNewtonsPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ N ] / [ kg2/m2 ]
operator /(const ALeft: TNewtons; const ARight: TSquareKilogramsPerSquareMeter): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtons; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilogramsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilogramsPerSquareMeter): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TNewtons;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ N*m2/kg2 ] = [ J ] / [ kg2/m ]
operator /(const ALeft: TJoules; const ARight: TSquareKilogramsPerMeter): TNewtonsSquareMeterPerSquareKilogram;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TJoules; const ARight: TNewtonsSquareMeterPerSquareKilogram): TSquareKilogramsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareKilogram; const ARight: TSquareKilogramsPerMeter): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareKilogramsPerMeter; const ARight: TNewtonsSquareMeterPerSquareKilogram): TJoules;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ N*s ] = [ N ] * [ s ]
operator *(const ALeft: TNewtons; const ARight: TSeconds): TNewtonsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSecond; const ARight: TNewtons): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TNewtons): TNewtonsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TNewtonsSecond; const ARight: TSeconds): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// main definition [ Ω*m ] = [ Ω ] * [ m ]
operator *(const ALeft: TOhms; const ARight: TMeters): TOhmsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TOhmsMeter; const ARight: TOhms): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TOhms): TOhmsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TOhmsMeter; const ARight: TMeters): TOhms;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ Ω*m ] = [ Ω ] / [ 1/m ]
operator /(const ALeft: TOhms; const ARight: TReciprocalMeters): TOhmsMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TOhms; const ARight: TOhmsMeter): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TOhmsMeter; const ARight: TReciprocalMeters): TOhms;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TOhmsMeter): TOhms;
begin
  result.Value := ALeft.Value * ARight.Value;
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

// alternative definition [ Pa ] = [ N/m ] / [ m ]
operator /(const ALeft: TNewtonsPerMeter; const ARight: TMeters): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TPascals): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPascals; const ARight: TMeters): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TPascals): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ Pa ] = [ kg/m3 ] * [ m2/s2 ]
operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSquareSecond): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascals; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSquareSecond; const ARight: TKilogramsPerCubicMeter): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascals; const ARight: TSquareMetersPerSquareSecond): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ Pa*s ] = [ Pa ] * [ s ]
operator *(const ALeft: TPascals; const ARight: TSeconds): TPascalsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascalsSecond; const ARight: TPascals): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TPascals): TPascalsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TPascalsSecond; const ARight: TSeconds): TPascals;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ Pa*s ] = [ Pa ] / [ Hz ]
operator /(const ALeft: TPascals; const ARight: THertz): TPascalsSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPascals; const ARight: TPascalsSecond): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TPascalsSecond; const ARight: THertz): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: TPascalsSecond): TPascals;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ K4 ] = [ K3 ] * [ K ]
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

// alternative definition [ K4 ] = [ K2 ] * [ K2 ]
operator *(const ALeft: TSquareKelvins; const ARight: TSquareKelvins): TQuarticKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TQuarticKelvins; const ARight: TSquareKelvins): TSquareKelvins;
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

// main definition [ 1/m ] = 1 / [ m ]
operator /(const ALeft: double; const ARight: TMeters): TReciprocalMeters;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TReciprocalMeters): TMeters;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TMeters): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TReciprocalMeters): double;
begin
  result := ALeft.Value * ARight.Value;
end;

// alternative definition [ 1/m ] = [ m ] / [ m2 ]
operator /(const ALeft: TMeters; const ARight: TSquareMeters): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMeters; const ARight: TReciprocalMeters): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TSquareMeters): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TReciprocalMeters): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ 1/m ] = [ m2 ] / [ m3 ]
operator /(const ALeft: TSquareMeters; const ARight: TCubicMeters): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TReciprocalMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TCubicMeters): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCubicMeters; const ARight: TReciprocalMeters): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ 1/m ] = [ m3 ] / [ m4 ]
operator /(const ALeft: TCubicMeters; const ARight: TQuarticMeters): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCubicMeters; const ARight: TReciprocalMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TQuarticMeters): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TQuarticMeters; const ARight: TReciprocalMeters): TCubicMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ 1/m ] = [ m4 ] / [ m5 ]
operator /(const ALeft: TQuarticMeters; const ARight: TQuinticMeters): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TQuarticMeters; const ARight: TReciprocalMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TQuinticMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TQuinticMeters; const ARight: TReciprocalMeters): TQuarticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ 1/m ] = [ m5 ] / [ m6 ]
operator /(const ALeft: TQuinticMeters; const ARight: TSexticMeters): TReciprocalMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TQuinticMeters; const ARight: TReciprocalMeters): TSexticMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalMeters; const ARight: TSexticMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSexticMeters; const ARight: TReciprocalMeters): TQuinticMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ 1/m2 ] = [ 1 ] / [ m2 ]
operator /(const ALeft: double; const ARight: TSquareMeters): TReciprocalSquareMeters;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TReciprocalSquareMeters): TSquareMeters;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TReciprocalSquareMeters; const ARight: TSquareMeters): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TReciprocalSquareMeters): double;
begin
  result := ALeft.Value * ARight.Value;
end;

//
operator /(const ALeft: TMeters; const ARight: TCubicMeters): TReciprocalSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TMeters; const ARight: TReciprocalSquareMeters): TCubicMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TReciprocalSquareMeters; const ARight: TCubicMeters): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCubicMeters; const ARight: TReciprocalSquareMeters): TMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
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

// main definition [ A2 ] = [ A ] * [ A ]
operator *(const ALeft: TAmperes; const ARight: TAmperes): TSquareAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TAmperes): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ A2/m ] = [ A2] / [ m ]
operator /(const ALeft: TSquareAmperes; const ARight: TMeters): TSquareAmperesPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareAmperes; const ARight: TSquareAmperesPerMeter): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareAmperesPerMeter; const ARight: TMeters): TSquareAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TSquareAmperesPerMeter): TSquareAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ A2/m ] = [ A/m ] * [ A ]
operator *(const ALeft: TAmperesPerMeter; const ARight: TAmperes): TSquareAmperesPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareAmperesPerMeter; const ARight: TAmperesPerMeter): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TAmperesPerMeter): TSquareAmperesPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareAmperesPerMeter; const ARight: TAmperes): TAmperesPerMeter;
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

// main definition [ C2/m2 ] = [ C2 ] / [ m2 ]
operator /(const ALeft: TSquareCoulombs; const ARight: TSquareMeters): TSquareCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TSquareMeters): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareCoulombsPerSquareMeter): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ C2/N ] = [ C2 ] / [ N ]
operator /(const ALeft: TSquareCoulombs; const ARight: TNewtons): TSquareCoulombsPerNewton;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerNewton): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewton; const ARight: TNewtons): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtons; const ARight: TSquareCoulombsPerNewton): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// main definition [ C2/N/m2 ] = [ C2 ] / [N*m2 ]
operator /(const ALeft: TSquareCoulombs; const ARight: TNewtonsSquareMeter): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombs; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsSquareMeter): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = [ C2/N ] / [ m2 ]
operator /(const ALeft: TSquareCoulombsPerNewton; const ARight: TSquareMeters): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombsPerNewton; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TSquareMeters): TSquareCoulombsPerNewton;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombsPerNewton;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = [ C2/m2 ] / [ N ]
operator /(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TNewtons): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareCoulombsPerSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtons;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtons): TSquareCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtons; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TSquareCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = [ C/m2 ] / [ N/C ]
operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TNewtonsPerCoulomb): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombsPerSquareMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsPerCoulomb): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtonsPerCoulomb; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TCoulombsPerSquareMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = [ A ] / [ V*m/s ]
operator /(const ALeft: TAmperes; const ARight: TVoltsMeterPerSecond): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TAmperes; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TVoltsMeterPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TVoltsMeterPerSecond): TAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVoltsMeterPerSecond; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TAmperes;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = [ C ] / [ V*m ]
operator /(const ALeft: TCoulombs; const ARight: TVoltsMeter): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TCoulombs; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TVoltsMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TVoltsMeter): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TVoltsMeter; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TCoulombs;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ C2/N/m2 ] = 1 / [ N*m2/C2 ]
operator /(const ALeft: double; const ARight: TNewtonsSquareMeterPerSquareCoulomb): TSquareCoulombsPerNewtonPerSquareMeter;
begin
  result.Value := ALeft / ARight.Value;
end;

operator /(const ALeft: double; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): TNewtonsSquareMeterPerSquareCoulomb;
begin
  result.Value := ALeft / ARight.Value;
end;

operator *(const ALeft: TSquareCoulombsPerNewtonPerSquareMeter; const ARight: TNewtonsSquareMeterPerSquareCoulomb): double;
begin
  result := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TNewtonsSquareMeterPerSquareCoulomb; const ARight: TSquareCoulombsPerNewtonPerSquareMeter): double;
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

// main definition [ K2 ] = [ K ] * [ K ]
operator *(const ALeft: TKelvins; const ARight: TKelvins): TSquareKelvins;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareKelvins; const ARight: TKelvins): TKelvins;
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

// main definition [ m2 ] = [ m ] * [ m ]
operator *(const ALeft: TMeters; const ARight: TMeters): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TMeters): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m2*K ] = [ m2 ] * [ K ]
operator *(const ALeft: TSquareMeters; const ARight: TKelvins): TSquareMetersKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersKelvin; const ARight: TSquareMeters): TKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TKelvins; const ARight: TSquareMeters): TSquareMetersKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersKelvin; const ARight: TKelvins): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ m2/C ] = [ m2 ] / [ C ]
operator /(const ALeft: TSquareMeters; const ARight: TCoulombs): TSquareMetersPerCoulomb;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareMeters; const ARight: TSquareMetersPerCoulomb): TCoulombs;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerCoulomb; const ARight: TCoulombs): TSquareMeters;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TCoulombs; const ARight: TSquareMetersPerCoulomb): TSquareMeters;
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
operator /(const ALeft: TPascalsSecond; const ARight: TKilogramsPerCubicMeter): TSquareMetersPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TPascalsSecond; const ARight: TSquareMetersPerSecond): TKilogramsPerCubicMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMetersPerSecond; const ARight: TKilogramsPerCubicMeter): TPascalsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TKilogramsPerCubicMeter; const ARight: TSquareMetersPerSecond): TPascalsSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
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

// main definition [ m2*K4 ] = [ m2 ] * [ K4 ]
operator *(const ALeft: TSquareMeters; const ARight: TQuarticKelvins): TSquareMetersQuarticKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersQuarticKelvin; const ARight: TSquareMeters): TQuarticKelvins;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TQuarticKelvins; const ARight: TSquareMeters): TSquareMetersQuarticKelvin;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareMetersQuarticKelvin; const ARight: TQuarticKelvins): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ rad2 ] = [ rad ] * [ rad ]
operator *(const ALeft: TRadians; const ARight: TRadians): TSquareRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareRadians; const ARight: TRadians): TRadians;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ rad2/s2 ] = [ rad2 ] / [ s2 ]
operator /(const ALeft: TSquareRadians; const ARight: TSquareSeconds): TSquareRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TSquareRadians; const ARight: TSquareRadiansPerSquareSecond): TSquareSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TSquareSeconds): TSquareRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareSeconds; const ARight: TSquareRadiansPerSquareSecond): TSquareRadians;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ rad2/s2 ] = [ rad/s ] * [ rad/s ]
operator *(const ALeft: TRadiansPerSecond; const ARight: TRadiansPerSecond): TSquareRadiansPerSquareSecond;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TSquareRadiansPerSquareSecond; const ARight: TRadiansPerSecond): TRadiansPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
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

// main definition [ T*m ] = [ T ] * [ m ]
operator *(const ALeft: TTeslas; const ARight: TMeters): TTeslasMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeter; const ARight: TTeslas): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TTeslas): TTeslasMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeter; const ARight: TMeters): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ T*m/A ] = [ T*m ] / [ A ]
operator /(const ALeft: TTeslasMeter; const ARight: TAmperes): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTeslasMeter; const ARight: TTeslasMeterPerAmpere): TAmperes;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TAmperes): TTeslasMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperes; const ARight: TTeslasMeterPerAmpere): TTeslasMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ T*m/A ] = [ T/A ] * [ m ]
operator *(const ALeft: TTeslasPerAmpere; const ARight: TMeters): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TTeslasPerAmpere): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TTeslasPerAmpere): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TMeters): TTeslasPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ T*m/A ] = [ T ] * [ m/A ]
operator *(const ALeft: TTeslas; const ARight: TMetersPerAmpere): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TTeslas): TMetersPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMetersPerAmpere; const ARight: TTeslas): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TTeslasMeterPerAmpere; const ARight: TMetersPerAmpere): TTeslas;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ T*m/A ] = [ T ] / [ A/m ]
operator /(const ALeft: TTeslas; const ARight: TAmperesPerMeter): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TTeslas; const ARight: TTeslasMeterPerAmpere): TAmperesPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TAmperesPerMeter): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TAmperesPerMeter; const ARight: TTeslasMeterPerAmpere): TTeslas;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

// alternative definition [ T*m/A ] = [ N/m ] / [ A2/m ]
operator /(const ALeft: TNewtonsPerMeter; const ARight: TSquareAmperesPerMeter): TTeslasMeterPerAmpere;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TNewtonsPerMeter; const ARight: TTeslasMeterPerAmpere): TSquareAmperesPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TTeslasMeterPerAmpere; const ARight: TSquareAmperesPerMeter): TNewtonsPerMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareAmperesPerMeter; const ARight: TTeslasMeterPerAmpere): TNewtonsPerMeter;
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

// main definition [ V*m ] = [ V ] * [ m ]
operator *(const ALeft: TVolts; const ARight: TMeters): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsMeter; const ARight: TVolts): TMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TMeters; const ARight: TVolts): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsMeter; const ARight: TMeters): TVolts;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// alternative definition [ V*m ] = [ V/m ] * [ m2 ]
operator *(const ALeft: TVoltsPerMeter; const ARight: TSquareMeters): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsMeter; const ARight: TVoltsPerMeter): TSquareMeters;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TSquareMeters; const ARight: TVoltsPerMeter): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TVoltsMeter; const ARight: TSquareMeters): TVoltsPerMeter;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

// main definition [ V*m/s ] = [ V*m ] / [ s ]
operator /(const ALeft: TVoltsMeter; const ARight: TSeconds): TVoltsMeterPerSecond;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TVoltsMeter; const ARight: TVoltsMeterPerSecond): TSeconds;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TVoltsMeterPerSecond; const ARight: TSeconds): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSeconds; const ARight: TVoltsMeterPerSecond): TVoltsMeter;
begin
  result.Value := ALeft.Value * ARight.Value;
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

// alternative definition [ W ] = [ J ] * [ Hz ]
operator *(const ALeft: TJoules; const ARight: THertz): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TJoules): THertz;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: THertz; const ARight: TJoules): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: THertz): TJoules;
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

// main definition [ W/m/K ] = [ W ] / [ m*K ]
operator /(const ALeft: TWatts; const ARight: TMetersKelvin): TWattsPerMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerMeterPerKelvin): TMetersKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerMeterPerKelvin; const ARight: TMetersKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TMetersKelvin; const ARight: TWattsPerMeterPerKelvin): TWatts;
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

// main definition [ W/m2/K ] = [ W ] / [ m2*K ]
operator /(const ALeft: TWatts; const ARight: TSquareMetersKelvin): TWattsPerSquareMeterPerKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerKelvin): TSquareMetersKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerKelvin; const ARight: TSquareMetersKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMetersKelvin; const ARight: TWattsPerSquareMeterPerKelvin): TWatts;
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

// main definition [ W/m2/K4 ] = [ W ] / [ m2*K4 ]
operator /(const ALeft: TWatts; const ARight: TSquareMetersQuarticKelvin): TWattsPerSquareMeterPerQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator /(const ALeft: TWatts; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TSquareMetersQuarticKelvin;
begin
  result.Value := ALeft.Value / ARight.Value;
end;

operator *(const ALeft: TWattsPerSquareMeterPerQuarticKelvin; const ARight: TSquareMetersQuarticKelvin): TWatts;
begin
  result.Value := ALeft.Value * ARight.Value;
end;

operator *(const ALeft: TSquareMetersQuarticKelvin; const ARight: TWattsPerSquareMeterPerQuarticKelvin): TWatts;
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

{ Power quantities }

function CubicPower(AQuantity: TKelvins): TCubicKelvins;
begin
  result.Value := Power(AQuantity.Value, 3);
end;

function CubicRoot(AQuantity: TCubicKelvins): TKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/3);
end;

function CubicPower(AQuantity: TMeters): TCubicMeters;
begin
  result.Value := Power(AQuantity.Value, 3);
end;

function CubicRoot(AQuantity: TCubicMeters): TMeters;
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

function SquarePower(AQuantity: TCoulombs): TSquareCoulombs;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareCoulombs): TCoulombs;
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

function SquarePower(AQuantity: TKelvins): TSquareKelvins;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareKelvins): TKelvins;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TKilograms): TSquareKilograms;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareKilograms): TKilograms;
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

function SquarePower(AQuantity: TMetersPerSecond): TSquareMetersPerSquareSecond;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareMetersPerSquareSecond): TMetersPerSecond;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TRadians): TSquareRadians;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareRadians): TRadians;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TRadiansPerSecond): TSquareRadiansPerSquareSecond;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareRadiansPerSquareSecond): TRadiansPerSecond;
begin
  result.Value := Power(AQuantity.Value, 1/2);
end;

function SquarePower(AQuantity: TSeconds): TSquareSeconds;
begin
  result.Value := Power(AQuantity.Value, 2);
end;

function SquareRoot(AQuantity: TSquareSeconds): TSeconds;
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

{ Equivalences }

operator := (AQuantity: TRadiansPerSecond): THertz;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: THertz): TRadiansPerSecond;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TBequerels): THertz;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: THertz): TBequerels;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TNewtonsMeter): TJoules;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TJoules): TNewtonsMeter;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TSquareMetersPerSquareSecond): TJoulesPerKilogram;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TJoulesPerKilogram): TSquareMetersPerSquareSecond;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TSieverts): TJoulesPerKilogram;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TJoulesPerKilogram): TSieverts;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TGrays): TJoulesPerKilogram;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TJoulesPerKilogram): TGrays;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TNewtonsSecond): TKilogramsMeterPerSecond;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TKilogramsMeterPerSecond): TNewtonsSecond;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TTeslasMeter): TNewtonsPerAmpere;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TNewtonsPerAmpere): TTeslasMeter;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TVoltsPerMeter): TNewtonsPerCoulomb;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TNewtonsPerCoulomb): TVoltsPerMeter;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: double): TRadians;
begin
  result.Value := AQuantity;
end;

operator := (AQuantity: TRadians): double;
begin
  result := AQuantity.Value;
end;

operator := (AQuantity: TRadiansPerSquareSecond): TSquareHertz;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TSquareHertz): TRadiansPerSquareSecond;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TNewtonsSquareMeterPerCoulomb): TVoltsMeter;
begin
  result.Value := AQuantity.Value;
end;

operator := (AQuantity: TVoltsMeter): TNewtonsSquareMeterPerCoulomb;
begin
  result.Value := AQuantity.Value;
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
