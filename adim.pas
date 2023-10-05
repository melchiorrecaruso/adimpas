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
  ADimPas library built on 05/10/2023.

  Number of base units: 121
  Number of factored units: 63
  Number of operators: 961 (208 external, 753 internal)
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

{ Quantity of square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of volt meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of poiseuille }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square joule }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of quartic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton square meter per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of sextic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of lumen }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square newton }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty;
  class operator *(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of mole kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton square meter per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of quartic meter second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i adim.inc}
  class operator /(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of tesla meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of quintic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSquareMeterQty): TSteradianQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TSquareMeterQty): TSquareMeterSteradianQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianQty): TSquareMeterSteradianQty;
  class operator /(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TSquareMeterQty): TQuarticKelvinQty;
  class operator *(const ALeft: TQuarticKelvinQty; const ARight: TSquareMeterQty): TSquareMeterQuarticKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareMeterQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TSexticMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticMeterQty): TSexticMeterQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSexticMeterQty;
  class operator /(const ALeft: TQuinticMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCubicMeterQty): TQuinticMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TQuinticMeterQty;
  class operator /(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of weber }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i adim.inc}
  class operator /(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty;
  class operator *(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty;
  class operator *(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square volt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ohm meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty;
  class operator *(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic meter per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty;
  class operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of tesla }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i adim.inc}
  class operator /(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TSquareMeterQty): TWeberQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TTeslaQty): TWeberQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of hertz }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square coulomb per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of pascal }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty;
  class operator *(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of reciprocal meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ampere per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of reciprocal kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of tesla per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of henry per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty;
  class operator *(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty;
  class operator /(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty;
  class operator *(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of katal }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of radian per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty;
  class operator /(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb per mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty;
  class operator *(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double;
  class operator *(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double;
  class operator /(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty;
  class operator /(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty;
  class operator *(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty;
  class operator /(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty;
  class operator *(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
  class operator /(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of reciprocal mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic meter per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of katal per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of radian per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i adim.inc}
  class operator :=(const AQuantity: THertzQty): TRadianPerSecondQty;
  class operator :=(const AQuantity: TRadianPerSecondQty): THertzQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of candela }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty;
  class operator *(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of volt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i adim.inc}
  class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty;
  class operator *(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty;
  class operator /(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty;
  class operator /(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty;
  class operator *(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty;
  class operator /(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty;
  class operator *(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i adim.inc}
  class operator *(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty;
  class operator *(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty;
  class operator *(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty;
  class operator *(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty;
  class operator /(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty;
  class operator *(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square kilogram per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty;
  class operator *(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i adim.inc}
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty;
  class operator *(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty;
  class operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty;
  class operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty;
  class operator /(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty;
  class operator *(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty;
  class operator *(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty;
  class operator /(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty;
  class operator /(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square second per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): double;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): double;
  class operator /(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square kilogram per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty;
  class operator *(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty;
  class operator *(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of candela per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of lumen per watt }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty;
  class operator *(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty;
  class operator *(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of volt per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram square meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty;
  class operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty;
  class operator *(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty;
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty;
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty;
  class operator *(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ohm }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i adim.inc}
  class operator /(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty;
  class operator *(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty;
  class operator *(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty;
  class operator /(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty;
  class operator *(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty;
  class operator *(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty;
  class operator /(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty;
  class operator *(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per square meter per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterQty;
  class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterQty): TWattPerKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerKelvinQty;
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerSquareMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerSquareMeterQty;
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterKelvinQty;
  class operator *(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterKelvinQty): TWattQty;
  class operator *(const ALeft: TSquareMeterKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton per square kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty;
  class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i adim.inc}
  class operator *(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramPerCubicMeterQty): double;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): double;
  class operator /(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty;
  class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty;
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty;
  class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per square meter per steradian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$i adim.inc}
  class operator /(const ALeft: TWattPerSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterQty;
  class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSteradianQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSteradianQty;
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSteradianQty;
  class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSteradianQty): TWattPerSquareMeterQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSquareMeterQty;
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterSteradianQty;
  class operator *(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterSteradianQty): TWattQty;
  class operator *(const ALeft: TSquareMeterSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of mole }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i adim.inc}
  class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty;
  class operator *(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty;
  class operator *(const ALeft: TReciprocalMoleQty; const ARight: TMoleQty): double;
  class operator *(const ALeft: TMoleQty; const ARight: TReciprocalMoleQty): double;
  class operator /(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty;
  class operator /(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty;
  class operator *(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty;
  class operator *(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty;
  class operator *(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty;
  class operator *(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty;
  class operator /(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty;
  class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty;
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty;
  class operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty;
  class operator /(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty;
  class operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of henry }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i adim.inc}
  class operator /(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty;
  class operator *(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty;
  class operator *(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty;
  class operator /(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty;
  class operator *(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty;
  class operator *(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of mole per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square kilogram per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty;
  class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of coulomb per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per square meter per quartic kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQty;
  class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerQuarticKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerQuarticKelvinQty;
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TQuarticKelvinQty;
  class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterQty;
  class operator *(const ALeft: TQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerSquareMeterQty;
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
  class operator *(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQuarticKelvinQty): TWattQty;
  class operator *(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per quartic meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty;
  class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty;
  class operator *(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kilogram per quartic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of watt per meter per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty;
  class operator /(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty;
  class operator *(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i adim.inc}
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty;
  class operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty;
  class operator *(const ALeft: TReciprocalMeterQty; const ARight: TMeterQty): double;
  class operator *(const ALeft: TMeterQty; const ARight: TReciprocalMeterQty): double;
  class operator /(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty;
  class operator *(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty;
  class operator *(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty;
  class operator /(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty;
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty;
  class operator *(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty;
  class operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty;
  class operator *(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty;
  class operator /(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty;
  class operator /(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty;
  class operator *(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty;
  class operator *(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty;
  class operator *(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty;
  class operator /(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty;
  class operator /(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty;
  class operator *(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty;
  class operator *(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty;
  class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
  class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty;
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty;
  class operator *(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty;
  class operator *(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty;
  class operator /(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty;
  class operator /(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty;
  class operator *(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty;
  class operator *(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty;
  class operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty;
  class operator *(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty;
  class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty;
  class operator /(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty;
  class operator /(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty;
  class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty;
  class operator *(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty;
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty;
  class operator *(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty;
  class operator *(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty;
  class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty;
  class operator /(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty;
  class operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
  class operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty;
  class operator *(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty;
  class operator /(const ALeft: TSexticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TQuinticMeterQty): TSexticMeterQty;
  class operator *(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TSexticMeterQty;
  class operator /(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TQuarticMeterQty): TQuinticMeterQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
  class operator /(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCubicMeterQty): TQuarticMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TCubicMeterQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TMeterQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of kelvin per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i adim.inc}
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty;
  class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty;
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty;
  class operator /(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty;
  class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i adim.inc}
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty;
  class operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty;
  class operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty;
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty;
  class operator *(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty;
  class operator *(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty;
  class operator /(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty;
  class operator /(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty;
  class operator *(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty;
  class operator *(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty;
  class operator /(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty;
  class operator *(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty;
  class operator *(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty;
  class operator *(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty;
  class operator *(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty;
  class operator *(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty;
  class operator *(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty;
  class operator /(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty;
  class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty;
  class operator /(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty;
  class operator *(const ALeft: THertzQty; const ARight: TSecondQty): double;
  class operator *(const ALeft: TSecondQty; const ARight: THertzQty): double;
  class operator /(const ALeft: double; const ARight: TSecondQty): THertzQty;
  class operator /(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square meter per square coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
  class operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of siemens }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TSiemensQty): TOhmQty;
  class operator *(const ALeft: TSiemensQty; const ARight: TOhmQty): double;
  class operator *(const ALeft: TOhmQty; const ARight: TSiemensQty): double;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per mole per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TJoulePerMoleQty; const ARight: TJoulePerMolePerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TKelvinQty): TJoulePerMoleQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerMoleQty;
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TMoleQty;
  class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleQty): TJoulePerKelvinQty;
  class operator *(const ALeft: TMoleQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerKelvinQty;
  class operator /(const ALeft: TJouleQty; const ARight: TJoulePerMolePerKelvinQty): TMoleKelvinQty;
  class operator *(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleKelvinQty): TJouleQty;
  class operator *(const ALeft: TMoleKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i adim.inc}
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty;
  class operator *(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty;
  class operator /(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty;
  class operator *(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter per second squared }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondSquaredQty): TCubicMeterPerSquareSecondQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty;
  class operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TKilogramPerCubicMeterQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TNewtonPerCubicMeterQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondSquaredQty): TKilogramQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramQty): TNewtonQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredQty): TNewtonQty;
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareSecondQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareSecondQty): TMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondSquaredQty): TSecondQty;
  class operator *(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of farad }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i adim.inc}
  class operator *(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty;
  class operator *(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty;
  class operator /(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty;
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty;
  class operator *(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty;
  class operator *(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty;
  class operator *(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty;
  class operator *(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ampere per square meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty;
  class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty;
  class operator /(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of farad per meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty;
  class operator *(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty;
  class operator /(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator *(const ALeft: TFaradPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): double;
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TFaradPerMeterQty): double;
  class operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TFaradPerMeterQty): TVoltPerMeterQty;
  class operator *(const ALeft: TFaradPerMeterQty; const ARight: TVoltPerMeterQty): TCoulombPerSquareMeterQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TFaradPerMeterQty): TCoulombPerSquareMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TFaradPerMeterQty): TVoltMeterQty;
  class operator *(const ALeft: TFaradPerMeterQty; const ARight: TVoltMeterQty): TCoulombQty;
  class operator *(const ALeft: TVoltMeterQty; const ARight: TFaradPerMeterQty): TCoulombQty;
  class operator /(const ALeft: TFaradQty; const ARight: TFaradPerMeterQty): TMeterQty;
  class operator *(const ALeft: TFaradPerMeterQty; const ARight: TMeterQty): TFaradQty;
  class operator *(const ALeft: TMeterQty; const ARight: TFaradPerMeterQty): TFaradQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square hertz }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty;
  class operator *(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty;
  class operator /(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareHertzQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSecondSquaredQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
  class operator /(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty;
  class operator /(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty;
  class operator /(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TSquareSecondQty): double;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareHertzQty): double;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of gray per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty;
  class operator *(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per kilogram per kelvin }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramQty;
  class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramQty): TJoulePerKelvinQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TJoulePerKilogramPerKelvinQty): TJoulePerKelvinQty;
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TJoulePerKilogramPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKelvinQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TJouleQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramKelvinQty;
  class operator *(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramKelvinQty): TJouleQty;
  class operator *(const ALeft: TKilogramKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of lux }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty;
  class operator *(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of volt meter per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of day }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TDayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=THourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of minute }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMinuteQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square day }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareDayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square minute }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareMinuteQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of astronomical }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TAstronomicalQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of nautical mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNauticalMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square mile }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty;

{ Quantity of cubic inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic foot }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of cubic yard }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TCubicYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of litre }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TLitreQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of gallon }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TGallonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty;
operator /(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty;
operator /(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty;
operator /(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty;
operator /(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty;

{ Quantity of tonne }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TTonneQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of pound }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ounce }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TOunceQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of stone }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TStoneQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of ton }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TTonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of degree celsius }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of degree fahrenheit }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty;
operator /(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty;

{ Quantity of degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of square degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TSquareDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: double; const ARight: THertzQty): TSecondQty;
operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty;
operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty;
operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty;
operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty;
operator /(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty;

{ Quantity of radian per second squared }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$i adim.inc}
  class operator /(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareHertzQty): TRadianQty;
  class operator /(const ALeft: TRadianPerSecondSquaredQty; const ARight: TRadianQty): TSquareHertzQty;
  class operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondSquaredQty): TSquareSecondQty;
  class operator *(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareSecondQty): TRadianQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSecondSquaredQty): TSecondQty;
  class operator *(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSecondQty): TRadianPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSecondSquaredQty;
operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSecondSquaredQty;
operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSecondSquaredQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSecondSquaredQty;

{ Quantity of steradian per square second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty;
  class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty;
  class operator /(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty;
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty;
operator *(const ALeft: TSteradianQty; const ARight: TSquareHertzQty): TSteradianPerSquareSecondQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TSteradianQty): TSteradianPerSquareSecondQty;
operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty;
operator /(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty;

{ Quantity of meter per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMeterPerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of mile per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMilePerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of nautical mile per hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareHertzQty;

{ Quantity of meter per second per second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of meter per hour per second }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty;

{ Quantity of newton second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty;
operator *(const ALeft: TKilogramSquareMeterQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty;
operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty;
operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty;

{ Quantity of pound force }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForceQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty;
operator /(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;

{ Quantity of bar }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TBarQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of pound per square inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of joule per cubic meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty;
operator /(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty;
operator /(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty;
operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty;
operator /(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;

{ Quantity of watt hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TWattHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of elettronvolt }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TElettronvoltQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton meter }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of pound force inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForceInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty;
operator *(const ALeft: TJouleQty; const ARight: THertzQty): TWattQty;
operator *(const ALeft: THertzQty; const ARight: TJouleQty): TWattQty;
operator /(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty;
operator /(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty;

{ Quantity of ampere hour }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TAmpereHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty;
operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty;
operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty;
operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty;
operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty;
operator /(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty;
operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty;
operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty;
operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty;
operator /(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty;
operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty;
operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty;
operator /(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty;
operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty;
operator /(const ALeft: TLumenQty; const ARight: TSteradianQty): TCandelaQty;
operator /(const ALeft: TLumenQty; const ARight: TSquareMeterQty): TLuxQty;

{ Quantity of bequerel }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of gray }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of sievert }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty;
operator /(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty;

{ Quantity of joule per degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton meter per radian }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton meter per degree }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty;
operator /(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty;
operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty;
operator /(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty;
operator /(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty;

{ Quantity of pound force per inch }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty;
operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty;
operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty;
operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty;

{ Quantity of pascal second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty;
operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty;
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty;
operator /(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty;
operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty;
operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondSquaredQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty;
operator /(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty;
operator *(const ALeft: TSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerKilogramQty;
operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareSecondQty): TCubicMeterPerKilogramQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty;
operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty;
operator /(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty;
operator /(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty;

{ Quantity of joule per kilogram }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty;
operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty;
operator /(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty;
operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty;
operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty;
operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty;
operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty;
operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TSquareMeterQty): TKelvinQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterKelvinQty): TWattPerSquareMeterPerKelvinQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinQty): TWattPerSquareMeterPerKelvinQty;
operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerKelvinQty;
operator /(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TQuarticKelvinQty): TSquareMeterQty;
operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinQty): TWattPerQuarticKelvinQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerQuarticKelvinQty;
operator /(const ALeft: TJouleQty; const ARight: TJoulePerMoleQty): TMoleQty;
operator /(const ALeft: TMoleKelvinQty; const ARight: TKelvinQty): TMoleQty;
operator /(const ALeft: TJouleQty; const ARight: TMoleKelvinQty): TJoulePerMolePerKelvinQty;
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleQty): TJoulePerMolePerKelvinQty;
operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinQty): TJoulePerMolePerKelvinQty;
operator /(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty;
operator /(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty;
operator /(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty;

{ Quantity of newton per coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty;
operator /(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty;
operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty;
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty;
operator /(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty;
operator /(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty;
operator /(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty;

{ Quantity of newton square meter per coulomb }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty;
operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty;
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty;
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty;
operator /(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty;
operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty;
operator /(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty;

{ Quantity of newton per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty;
operator /(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty;
operator /(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty;
operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty;
operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty;
operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty;

{ Quantity of tesla meter per ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of newton per square ampere }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty;
operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty;
operator /(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty;
operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty;
operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty;
operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty;

{ Quantity of joule second }
{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of elettronvolt second }
{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty;
operator /(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty;
operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty;
operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty;
operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty;
operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty;
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty;
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty;
operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty;
operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSteradianQty): TSquareMeterQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterSteradianQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty;
operator /(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty;

{ Unit of cubic meter per kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicMetersPerKilogram = TCubicMeterPerKilogramQty;

{ Unit of tesla }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaQty}{$DEFINE TUnit:=TTeslaUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTeslas = TTeslaQty;

var
  T: TTeslaUnit;

const
  mT: TTeslaQty = (FValue: 1E-03);
  miT: TTeslaQty = (FValue: 1E-06);
  nT: TTeslaQty = (FValue: 1E-09);

{ Unit of square kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKelvinQty}{$DEFINE TUnit:=TSquareKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareKelvins = TSquareKelvinQty;

var
  K2: TSquareKelvinUnit;

{ Unit of ohm meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOhmMeterQty}{$DEFINE TUnit:=TOhmMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TOhmMeters = TOhmMeterQty;

{ Unit of hertz }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=THertzUnit}{$i adim.inc}
  class operator *(const ALeft: TMeterQty; const ARight: THertzUnit): TMeterPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  THertz = THertzQty;

var
  Hz: THertzUnit;

const
  THz: THertzQty = (FValue: 1E+12);
  GHz: THertzQty = (FValue: 1E+09);
  MHz: THertzQty = (FValue: 1E+06);
  kHz: THertzQty = (FValue: 1E+03);

{ Unit of square volt }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareVoltQty}{$DEFINE TUnit:=TSquareVoltUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareVolts = TSquareVoltQty;

var
  V2: TSquareVoltUnit;

const
  kV2: TSquareVoltQty = (FValue: 1E+06);
  mV2: TSquareVoltQty = (FValue: 1E-06);

{ Unit of joule per mole }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerMoleQty}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerMole = TJoulePerMoleQty;

{ Unit of square coulomb per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareCoulombsPerMeter = TSquareCoulombPerMeterQty;

{ Unit of pascal }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TPascalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPascals = TPascalQty;

var
  Pa: TPascalUnit;

const
  TPa: TPascalQty = (FValue: 1E+12);
  GPa: TPascalQty = (FValue: 1E+09);
  MPa: TPascalQty = (FValue: 1E+06);
  kPa: TPascalQty = (FValue: 1E+03);

{ Unit of reciprocal meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalMeterQty}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TReciprocalMeters = TReciprocalMeterQty;

{ Unit of newton per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerMeter = TNewtonPerMeterQty;

{ Unit of newton per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerCubicMeter = TNewtonPerCubicMeterQty;

{ Unit of watt per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerMeterQty}{$DEFINE TUnit:=TWattPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerMeter = TWattPerMeterQty;

{ Unit of ampere per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmperePerMeterQty}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TAmperesPerMeter = TAmperePerMeterQty;

{ Unit of reciprocal kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TReciprocalKelvins = TReciprocalKelvinQty;

{ Unit of tesla per ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTeslasPerAmpere = TTeslaPerAmpereQty;

{ Unit of kilogram per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerMeter = TKilogramPerMeterQty;

{ Unit of henry per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=THenryPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  THenriesPerMeter = THenryPerMeterQty;

{ Unit of katal }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKatalQty}{$DEFINE TUnit:=TKatalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKatals = TKatalQty;

var
  kat: TKatalUnit;

{ Unit of square meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMetersPerSecond = TSquareMeterPerSecondQty;

{ Unit of radian per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerMeterQty}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TRadiansPerMeter = TRadianPerMeterQty;

{ Unit of watt }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattQty}{$DEFINE TUnit:=TWattUnit}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TWatts = TWattQty;

var
  W: TWattUnit;

const
  TW: TWattQty = (FValue: 1E+12);
  GW: TWattQty = (FValue: 1E+09);
  MW: TWattQty = (FValue: 1E+06);
  kW: TWattQty = (FValue: 1E+03);
  milliW: TWattQty = (FValue: 1E-03);

{ Unit of coulomb per mole }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombsPerMole = TCoulombPerMoleQty;

{ Unit of kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKelvinQty}{$DEFINE TUnit:=TKelvinUnit}{$i adim.inc}
  class operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinUnit): TJoulePerMolePerKelvinQty;
  class operator *(const ALeft: TMoleQty; const ARight: TKelvinUnit): TMoleKelvinQty;
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinUnit): TWattPerSquareMeterPerKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinUnit): TSquareMeterKelvinQty;
  class operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinUnit): TWattPerMeterPerKelvinQty;
  class operator /(const ALeft: TWattQty; const ARight: TKelvinUnit): TWattPerKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKelvinUnit): TMeterKelvinQty;
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinUnit): TJoulePerKilogramPerKelvinQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKelvinUnit): TJoulePerKelvinQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TKelvinUnit): TKilogramKelvinQty;
  class operator /(const ALeft: double; const ARight: TKelvinUnit): TReciprocalKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TKelvins = TKelvinQty;

var
  K: TKelvinUnit;

{ Unit of joule }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TJouleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoules = TJouleQty;

var
  J: TJouleUnit;

const
  TJ: TJouleQty = (FValue: 1E+12);
  GJ: TJouleQty = (FValue: 1E+09);
  MJ: TJouleQty = (FValue: 1E+06);
  kJ: TJouleQty = (FValue: 1E+03);

{ Unit of reciprocal mole }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalMoleQty}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TReciprocalMoles = TReciprocalMoleQty;

{ Unit of coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombQty}{$DEFINE TUnit:=TCoulombUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TCoulombUnit): TVoltPerMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TCoulombUnit): TVoltQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombs = TCoulombQty;

var
  C: TCoulombUnit;

const
  kC: TCoulombQty = (FValue: 1E+03);
  hC: TCoulombQty = (FValue: 1E+02);
  daC: TCoulombQty = (FValue: 1E+01);
  dC: TCoulombQty = (FValue: 1E-01);
  cC: TCoulombQty = (FValue: 1E-02);
  mC: TCoulombQty = (FValue: 1E-03);
  miC: TCoulombQty = (FValue: 1E-06);
  nC: TCoulombQty = (FValue: 1E-09);
  pC: TCoulombQty = (FValue: 1E-12);

{ Unit of cubic meter per square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicMetersPerSquareSecond = TCubicMeterPerSquareSecondQty;

{ Unit of katal per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKatalsPerCubicMeter = TKatalPerCubicMeterQty;

{ Unit of watt per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerKelvinQty}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerKelvin = TWattPerKelvinQty;

{ Unit of joule per radian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerRadian = TJoulePerRadianQty;

{ Unit of radian per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerSecondQty}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TRadiansPerSecond = TRadianPerSecondQty;

{ Unit of candela }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCandelaQty}{$DEFINE TUnit:=TCandelaUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCandelas = TCandelaQty;

var
  cd: TCandelaUnit;

{ Unit of volt }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltQty}{$DEFINE TUnit:=TVoltUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TVoltUnit): TFaradQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TVolts = TVoltQty;

var
  V: TVoltUnit;

const
  kV: TVoltQty = (FValue: 1E+03);
  mV: TVoltQty = (FValue: 1E-03);

{ Unit of weber }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWeberQty}{$DEFINE TUnit:=TWeberUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWebers = TWeberQty;

var
  Wb: TWeberUnit;

{ Unit of square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterQty}{$DEFINE TUnit:=TSquareMeterUnit}{$i adim.inc}
  class operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerSteradianQty;
  class operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterUnit): TCandelaPerSquareMeterQty;
  class operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterUnit): TAmperePerSquareMeterQty;
  class operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterUnit): TSquareSecondPerSquareMeterQty;
  class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterUnit): TCoulombPerSquareMeterQty;
  class operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerQuarticKelvinQty;
  class operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerKelvinQty;
  class operator /(const ALeft: TWattQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterQty;
  class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterUnit): TSquareKilogramPerSquareMeterQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterQty;
  class operator /(const ALeft: TLumenQty; const ARight: TSquareMeterUnit): TLuxQty;
  class operator /(const ALeft: TWeberQty; const ARight: TSquareMeterUnit): TTeslaQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramPerSquareMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMeters = TSquareMeterQty;

var
  m2: TSquareMeterUnit;

const
  km2: TSquareMeterQty = (FValue: 1E+06);
  dm2: TSquareMeterQty = (FValue: 1E-02);
  cm2: TSquareMeterQty = (FValue: 1E-04);
  mm2: TSquareMeterQty = (FValue: 1E-06);
  mim2: TSquareMeterQty = (FValue: 1E-12);
  nm2: TSquareMeterQty = (FValue: 1E-18);
  pm2: TSquareMeterQty = (FValue: 1E-24);

{ Unit of square meter per square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMetersPerSquareSecond = TSquareMeterPerSquareSecondQty;

{ Unit of quintic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuinticMeterQty}{$DEFINE TUnit:=TQuinticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TQuinticMeters = TQuinticMeterQty;

var
  m5: TQuinticMeterUnit;

const
  km5: TQuinticMeterQty = (FValue: 1E+15);
  dm5: TQuinticMeterQty = (FValue: 1E-05);
  cm5: TQuinticMeterQty = (FValue: 1E-10);
  mm5: TQuinticMeterQty = (FValue: 1E-15);
  mim5: TQuinticMeterQty = (FValue: 1E-30);
  nm5: TQuinticMeterQty = (FValue: 1E-45);
  pm5: TQuinticMeterQty = (FValue: 1E-60);

{ Unit of radian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianQty}{$DEFINE TUnit:=TRadianUnit}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TRadians = TRadianQty;

var
  rad: TRadianUnit;

{ Unit of square kilogram per square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareKilogramsPerSquareSecond = TSquareKilogramPerSquareSecondQty;

{ Unit of tesla meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TTeslaMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTeslaMeters = TTeslaMeterQty;

{ Unit of meter kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterKelvinQty}{$DEFINE TUnit:=TMeterKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMeterKelvins = TMeterKelvinQty;

{ Unit of watt per steradian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSteradianQty}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerSteradian = TWattPerSteradianQty;

{ Unit of ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmpereQty}{$DEFINE TUnit:=TAmpereUnit}{$i adim.inc}
  class operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereUnit): THenryPerMeterQty;
  class operator /(const ALeft: TTeslaQty; const ARight: TAmpereUnit): TTeslaPerAmpereQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TAmpereUnit): TTeslaMeterQty;
  class operator /(const ALeft: TMeterQty; const ARight: TAmpereUnit): TMeterPerAmpereQty;
  class operator /(const ALeft: TWeberQty; const ARight: TAmpereUnit): THenryQty;
  class operator /(const ALeft: TVoltQty; const ARight: TAmpereUnit): TOhmQty;
  class operator /(const ALeft: TWattQty; const ARight: TAmpereUnit): TVoltQty;
  class operator *(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TAmperes = TAmpereQty;

var
  A: TAmpereUnit;

const
  kA: TAmpereQty = (FValue: 1E+03);
  hA: TAmpereQty = (FValue: 1E+02);
  daA: TAmpereQty = (FValue: 1E+01);
  dA: TAmpereQty = (FValue: 1E-01);
  cA: TAmpereQty = (FValue: 1E-02);
  mA: TAmpereQty = (FValue: 1E-03);
  miA: TAmpereQty = (FValue: 1E-06);
  nA: TAmpereQty = (FValue: 1E-09);
  picoA: TAmpereQty = (FValue: 1E-12);

{ Unit of square second per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareSecondsPerSquareMeter = TSquareSecondPerSquareMeterQty;

{ Unit of newton }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonQty}{$DEFINE TUnit:=TNewtonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtons = TNewtonQty;

var
  N: TNewtonUnit;

const
  GN: TNewtonQty = (FValue: 1E+09);
  MN: TNewtonQty = (FValue: 1E+06);
  kN: TNewtonQty = (FValue: 1E+03);
  hN: TNewtonQty = (FValue: 1E+02);
  daN: TNewtonQty = (FValue: 1E+01);

{ Unit of square kilogram per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareKilogramsPerMeter = TSquareKilogramPerMeterQty;

{ Unit of watt per quartic kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerQuarticKelvin = TWattPerQuarticKelvinQty;

{ Unit of quartic meter second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TQuarticMeterSeconds = TQuarticMeterSecondQty;

{ Unit of quartic kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticKelvinQty}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i adim.inc}
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinUnit): TWattPerSquareMeterPerQuarticKelvinQty;
  class operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinUnit): TWattPerQuarticKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinUnit): TSquareMeterQuarticKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TQuarticKelvins = TQuarticKelvinQty;

var
  K4: TQuarticKelvinUnit;

{ Unit of candela per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCandelasPerSquareMeter = TCandelaPerSquareMeterQty;

{ Unit of square meter per square kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMetersPerSquareKilogram = TSquareMeterPerSquareKilogramQty;

{ Unit of lumen per watt }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLumenPerWattQty}{$DEFINE TUnit:=TLumenPerWattUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TLumensPerWatt = TLumenPerWattQty;

{ Unit of kilogram per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerSecond = TKilogramPerSecondQty;

{ Unit of watt per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerSquareMeter = TWattPerSquareMeterQty;

{ Unit of newton square meter per square coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonSquareMetersPerSquareCoulomb = TNewtonSquareMeterPerSquareCoulombQty;

{ Unit of volt per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TVoltsPerMeter = TVoltPerMeterQty;

{ Unit of mole kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMoleKelvinQty}{$DEFINE TUnit:=TMoleKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMoleKelvins = TMoleKelvinQty;

{ Unit of square ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareAmpereQty}{$DEFINE TUnit:=TSquareAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareAmperes = TSquareAmpereQty;

var
  A2: TSquareAmpereUnit;

const
  kA2: TSquareAmpereQty = (FValue: 1E+06);
  hA2: TSquareAmpereQty = (FValue: 1E+04);
  daA2: TSquareAmpereQty = (FValue: 1E+02);
  dA2: TSquareAmpereQty = (FValue: 1E-02);
  cA2: TSquareAmpereQty = (FValue: 1E-04);
  mA2: TSquareAmpereQty = (FValue: 1E-06);
  miA2: TSquareAmpereQty = (FValue: 1E-12);
  nA2: TSquareAmpereQty = (FValue: 1E-18);
  picoA2: TSquareAmpereQty = (FValue: 1E-24);

{ Unit of kilogram square meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramSquareMetersPerSecond = TKilogramSquareMeterPerSecondQty;

{ Unit of kilogram square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramSquareSeconds = TKilogramSquareSecondQty;

{ Unit of steradian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSteradianQty}{$DEFINE TUnit:=TSteradianUnit}{$i adim.inc}
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianUnit): TWattPerSquareMeterPerSteradianQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianUnit): TSquareMeterSteradianQty;
  class operator /(const ALeft: TWattQty; const ARight: TSteradianUnit): TWattPerSteradianQty;
  class operator *(const ALeft: TCandelaQty; const ARight: TSteradianUnit): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSteradians = TSteradianQty;

var
  sr: TSteradianUnit;

{ Unit of coulomb per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombsPerMeter = TCoulombPerMeterQty;

{ Unit of square newton }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareNewtonQty}{$DEFINE TUnit:=TSquareNewtonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareNewtons = TSquareNewtonQty;

var
  N2: TSquareNewtonUnit;

const
  GN2: TSquareNewtonQty = (FValue: 1E+18);
  MN2: TSquareNewtonQty = (FValue: 1E+12);
  kN2: TSquareNewtonQty = (FValue: 1E+06);
  hN2: TSquareNewtonQty = (FValue: 1E+04);
  daN2: TSquareNewtonQty = (FValue: 1E+02);

{ Unit of kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramQty}{$DEFINE TUnit:=TKilogramUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TKilogramUnit): TCoulombPerKilogramQty;
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramUnit): TJoulePerKilogramPerKelvinQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramUnit): TCubicMeterPerKilogramQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramUnit): TSquareMeterPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilograms = TKilogramQty;

var
  kg: TKilogramUnit;
  hg: TKilogramQty = (FValue: 1E-01);
  dag: TKilogramQty = (FValue: 1E-02);
  g: TKilogramQty = (FValue: 1E-03);
  dg: TKilogramQty = (FValue: 1E-04);
  cg: TKilogramQty = (FValue: 1E-05);
  mg: TKilogramQty = (FValue: 1E-06);
  mig: TKilogramQty = (FValue: 1E-09);
  ng: TKilogramQty = (FValue: 1E-12);
  pg: TKilogramQty = (FValue: 1E-15);

{ Unit of ohm }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOhmQty}{$DEFINE TUnit:=TOhmUnit}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TOhmUnit): TSiemensQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TOhms = TOhmQty;

var
  ohm: TOhmUnit;

const
  Gohm: TOhmQty = (FValue: 1E+09);
  megaohm: TOhmQty = (FValue: 1E+06);
  kohm: TOhmQty = (FValue: 1E+03);
  mohm: TOhmQty = (FValue: 1E-03);
  miohm: TOhmQty = (FValue: 1E-06);
  nohm: TOhmQty = (FValue: 1E-09);

{ Unit of kilogram square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramSquareMeters = TKilogramSquareMeterQty;

{ Unit of watt per square meter per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerSquareMeterPerKelvin = TWattPerSquareMeterPerKelvinQty;

{ Unit of lumen }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLumenQty}{$DEFINE TUnit:=TLumenUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TLumens = TLumenQty;

var
  lm: TLumenUnit;

{ Unit of newton per square kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerSquareKilogram = TNewtonPerSquareKilogramQty;

{ Unit of square meter quartic kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMeterQuarticKelvins = TSquareMeterQuarticKelvinQty;

{ Unit of kilogram per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerCubicMeter = TKilogramPerCubicMeterQty;

{ Unit of watt per square meter per steradian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerSquareMeterPerSteradian = TWattPerSquareMeterPerSteradianQty;

{ Unit of kilogram per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerSquareMeter = TKilogramPerSquareMeterQty;

{ Unit of kilogram kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramKelvinQty}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramKelvins = TKilogramKelvinQty;

{ Unit of mole }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMoleQty}{$DEFINE TUnit:=TMoleUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TMoleUnit): TCoulombPerMoleQty;
  class operator /(const ALeft: double; const ARight: TMoleUnit): TReciprocalMoleQty;
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleUnit): TJoulePerMolePerKelvinQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMoleUnit): TJoulePerMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TMoles = TMoleQty;

var
  mol: TMoleUnit;

const
  kmol: TMoleQty = (FValue: 1E+03);
  hmol: TMoleQty = (FValue: 1E+02);
  damol: TMoleQty = (FValue: 1E+01);

{ Unit of joule per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerKelvin = TJoulePerKelvinQty;

{ Unit of coulomb per kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombsPerKilogram = TCoulombPerKilogramQty;

{ Unit of cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterQty}{$DEFINE TUnit:=TCubicMeterUnit}{$i adim.inc}
  class operator /(const ALeft: TKatalQty; const ARight: TCubicMeterUnit): TKatalPerCubicMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterUnit): TCoulombPerCubicMeterQty;
  class operator /(const ALeft: TMoleQty; const ARight: TCubicMeterUnit): TMolePerCubicMeterQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterUnit): TKilogramPerCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicMeters = TCubicMeterQty;

var
  m3: TCubicMeterUnit;

const
  km3: TCubicMeterQty = (FValue: 1E+09);
  dm3: TCubicMeterQty = (FValue: 1E-03);
  cm3: TCubicMeterQty = (FValue: 1E-06);
  mm3: TCubicMeterQty = (FValue: 1E-09);
  mim3: TCubicMeterQty = (FValue: 1E-18);
  nm3: TCubicMeterQty = (FValue: 1E-27);
  pm3: TCubicMeterQty = (FValue: 1E-36);

{ Unit of coulomb per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombsPerSquareMeter = TCoulombPerSquareMeterQty;

{ Unit of newton per square coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerSquareCoulomb = TNewtonPerSquareCoulombQty;

{ Unit of cubic meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicMetersPerSecond = TCubicMeterPerSecondQty;

{ Unit of meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i adim.inc}
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondUnit): TKilogramMeterPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerSecond = TMeterPerSecondQty;

{ Unit of henry }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryQty}{$DEFINE TUnit:=THenryUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  THenries = THenryQty;

var
  H: THenryUnit;

const
  mH: THenryQty = (FValue: 1E-03);
  miH: THenryQty = (FValue: 1E-06);
  nH: THenryQty = (FValue: 1E-09);

{ Unit of mole per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMolesPerCubicMeter = TMolePerCubicMeterQty;

{ Unit of square meter kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMeterKelvins = TSquareMeterKelvinQty;

{ Unit of sextic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSexticMeterQty}{$DEFINE TUnit:=TSexticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSexticMeters = TSexticMeterQty;

var
  m6: TSexticMeterUnit;

const
  km6: TSexticMeterQty = (FValue: 1E+18);
  dm6: TSexticMeterQty = (FValue: 1E-06);
  cm6: TSexticMeterQty = (FValue: 1E-12);
  mm6: TSexticMeterQty = (FValue: 1E-18);
  mim6: TSexticMeterQty = (FValue: 1E-36);
  nm6: TSexticMeterQty = (FValue: 1E-54);
  pm6: TSexticMeterQty = (FValue: 1E-72);

{ Unit of newton square meter per square kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonSquareMetersPerSquareKilogram = TNewtonSquareMeterPerSquareKilogramQty;

{ Unit of square kilogram per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareKilogramsPerSquareMeter = TSquareKilogramPerSquareMeterQty;

{ Unit of quartic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticMeterQty}{$DEFINE TUnit:=TQuarticMeterUnit}{$i adim.inc}
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterPerSecondQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TQuarticMeters = TQuarticMeterQty;

var
  m4: TQuarticMeterUnit;

const
  km4: TQuarticMeterQty = (FValue: 1E+12);
  dm4: TQuarticMeterQty = (FValue: 1E-04);
  cm4: TQuarticMeterQty = (FValue: 1E-08);
  mm4: TQuarticMeterQty = (FValue: 1E-12);
  mim4: TQuarticMeterQty = (FValue: 1E-24);
  nm4: TQuarticMeterQty = (FValue: 1E-36);
  pm4: TQuarticMeterQty = (FValue: 1E-48);

{ Unit of kilogram meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramMetersPerSecond = TKilogramMeterPerSecondQty;

{ Unit of square joule }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareJouleQty}{$DEFINE TUnit:=TSquareJouleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareJoules = TSquareJouleQty;

var
  J2: TSquareJouleUnit;

const
  TJ2: TSquareJouleQty = (FValue: 1E+24);
  GJ2: TSquareJouleQty = (FValue: 1E+18);
  MJ2: TSquareJouleQty = (FValue: 1E+12);
  kJ2: TSquareJouleQty = (FValue: 1E+06);

{ Unit of coulomb per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCoulombsPerCubicMeter = TCoulombPerCubicMeterQty;

{ Unit of poiseuille }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPoiseuilleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPoiseuilles = TPoiseuilleQty;

var
  Pl: TPoiseuilleUnit;

const
  cPl: TPoiseuilleQty = (FValue: 1E-02);
  mPl: TPoiseuilleQty = (FValue: 1E-03);
  miPl: TPoiseuilleQty = (FValue: 1E-06);

{ Unit of watt per square meter per quartic kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerSquareMeterPerQuarticKelvin = TWattPerSquareMeterPerQuarticKelvinQty;

{ Unit of kilogram per quartic meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerQuarticMeterPerSecond = TKilogramPerQuarticMeterPerSecondQty;

{ Unit of kilogram per quartic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKilogramsPerQuarticMeter = TKilogramPerQuarticMeterQty;

{ Unit of watt per meter per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattsPerMeterPerKelvin = TWattPerMeterPerKelvinQty;

{ Unit of meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterQty}{$DEFINE TUnit:=TMeterUnit}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TMeterUnit): TReciprocalMeterQty;
  class operator /(const ALeft: TRadianQty; const ARight: TMeterUnit): TRadianPerMeterQty;
  class operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterUnit): THenryPerMeterQty;
  class operator /(const ALeft: THenryQty; const ARight: TMeterUnit): THenryPerMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TMeterUnit): TTeslaMeterQty;
  class operator /(const ALeft: TAmpereQty; const ARight: TMeterUnit): TAmperePerMeterQty;
  class operator /(const ALeft: TFaradQty; const ARight: TMeterUnit): TFaradPerMeterQty;
  class operator *(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltMeterQty;
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterUnit): TSquareCoulombPerMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TMeterUnit): TCoulombPerMeterQty;
  class operator /(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltPerMeterQty;
  class operator *(const ALeft: TOhmQty; const ARight: TMeterUnit): TOhmMeterQty;
  class operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterUnit): TWattPerMeterPerKelvinQty;
  class operator /(const ALeft: TWattQty; const ARight: TMeterUnit): TWattPerMeterQty;
  class operator /(const ALeft: TKelvinQty; const ARight: TMeterUnit): TKelvinPerMeterQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterUnit): TSquareKilogramPerMeterQty;
  class operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterUnit): TKilogramPerSecondQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TMeterUnit): TNewtonPerMeterQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TMeterUnit): TJouleQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramPerMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TMeters = TMeterQty;

var
  m: TMeterUnit;

const
  km: TMeterQty = (FValue: 1E+03);
  dm: TMeterQty = (FValue: 1E-01);
  cm: TMeterQty = (FValue: 1E-02);
  mm: TMeterQty = (FValue: 1E-03);
  mim: TMeterQty = (FValue: 1E-06);
  nm: TMeterQty = (FValue: 1E-09);
  pm: TMeterQty = (FValue: 1E-12);

{ Unit of kelvin per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TKelvinsPerMeter = TKelvinPerMeterQty;

{ Unit of newton square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonSquareMeters = TNewtonSquareMeterQty;

{ Unit of second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSecondQty}{$DEFINE TUnit:=TSecondUnit}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondUnit): TGrayPerSecondQty;
  class operator /(const ALeft: TVoltMeterQty; const ARight: TSecondUnit): TVoltMeterPerSecondQty;
  class operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondUnit): TKilogramPerQuarticMeterPerSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondUnit): TQuarticMeterSecondQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSecondUnit): TSquareMeterPerSecondQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSecondUnit): TPoiseuilleQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty;
  class operator /(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty;
  class operator *(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty;
  class operator *(const ALeft: TJouleQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSecondSquaredQty;
  class operator /(const ALeft: TMeterQty; const ARight: TSecondUnit): TMeterPerSecondQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSecondSquaredQty;
  class operator /(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty;
  class operator /(const ALeft: double; const ARight: TSecondUnit): THertzQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSeconds = TSecondQty;

var
  s: TSecondUnit;

const
  ds: TSecondQty = (FValue: 1E-01);
  cs: TSecondQty = (FValue: 1E-02);
  ms: TSecondQty = (FValue: 1E-03);
  mis: TSecondQty = (FValue: 1E-06);
  ns: TSecondQty = (FValue: 1E-09);
  ps: TSecondQty = (FValue: 1E-12);

{ Unit of square meter per square coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMetersPerSquareCoulomb = TSquareMeterPerSquareCoulombQty;

{ Unit of square coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareCoulombQty}{$DEFINE TUnit:=TSquareCoulombUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombUnit): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombUnit): TNewtonPerSquareCoulombQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombUnit): TSquareMeterPerSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareCoulombs = TSquareCoulombQty;

var
  C2: TSquareCoulombUnit;

const
  kC2: TSquareCoulombQty = (FValue: 1E+06);
  hC2: TSquareCoulombQty = (FValue: 1E+04);
  daC2: TSquareCoulombQty = (FValue: 1E+02);
  dC2: TSquareCoulombQty = (FValue: 1E-02);
  cC2: TSquareCoulombQty = (FValue: 1E-04);
  mC2: TSquareCoulombQty = (FValue: 1E-06);
  miC2: TSquareCoulombQty = (FValue: 1E-12);
  nC2: TSquareCoulombQty = (FValue: 1E-18);
  pC2: TSquareCoulombQty = (FValue: 1E-24);

{ Unit of siemens }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSiemensQty}{$DEFINE TUnit:=TSiemensUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSiemens = TSiemensQty;

var
  siemens: TSiemensUnit;

const
  millisiemens: TSiemensQty = (FValue: 1E-03);
  microsiemens: TSiemensQty = (FValue: 1E-06);
  nanosiemens: TSiemensQty = (FValue: 1E-09);

{ Unit of joule per mole per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerMolePerKelvin = TJoulePerMolePerKelvinQty;

{ Unit of square meter steradian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMeterSteradians = TSquareMeterSteradianQty;

{ Unit of meter per ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerAmpere = TMeterPerAmpereQty;

{ Unit of meter per second squared }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$DEFINE TUnit:=TMeterPerSecondSquaredUnit}{$i adim.inc}
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredUnit): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerSecondSquared = TMeterPerSecondSquaredQty;

{ Unit of farad }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFaradQty}{$DEFINE TUnit:=TFaradUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TFarads = TFaradQty;

var
  F: TFaradUnit;

const
  mF: TFaradQty = (FValue: 1E-03);
  miF: TFaradQty = (FValue: 1E-06);
  nF: TFaradQty = (FValue: 1E-09);
  pF: TFaradQty = (FValue: 1E-12);

{ Unit of ampere per square meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TAmperesPerSquareMeter = TAmperePerSquareMeterQty;

{ Unit of farad per meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFaradPerMeterQty}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TFaradsPerMeter = TFaradPerMeterQty;

{ Unit of volt meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TVoltMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TVoltMeters = TVoltMeterQty;

{ Unit of square hertz }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareHertzQty}{$DEFINE TUnit:=TSquareHertzUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareHertz = TSquareHertzQty;

var
  Hz2: TSquareHertzUnit;

const
  THz2: TSquareHertzQty = (FValue: 1E+24);
  GHz2: TSquareHertzQty = (FValue: 1E+18);
  MHz2: TSquareHertzQty = (FValue: 1E+12);
  kHz2: TSquareHertzQty = (FValue: 1E+06);

{ Unit of gray per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TGrayPerSecondQty}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TGraysPerSecond = TGrayPerSecondQty;

{ Unit of joule per kilogram per kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerKilogramPerKelvin = TJoulePerKilogramPerKelvinQty;

{ Unit of square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareSecondQty}{$DEFINE TUnit:=TSquareSecondUnit}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondUnit): TCubicMeterPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondUnit): TKilogramSquareSecondQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSecondSquaredQty;
  class operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondUnit): TSteradianPerSquareSecondQty;
  class operator /(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSecondSquaredQty;
  class operator /(const ALeft: double; const ARight: TSquareSecondUnit): TSquareHertzQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareSeconds = TSquareSecondQty;

var
  s2: TSquareSecondUnit;

const
  ds2: TSquareSecondQty = (FValue: 1E-02);
  cs2: TSquareSecondQty = (FValue: 1E-04);
  ms2: TSquareSecondQty = (FValue: 1E-06);
  mis2: TSquareSecondQty = (FValue: 1E-12);
  ns2: TSquareSecondQty = (FValue: 1E-18);
  ps2: TSquareSecondQty = (FValue: 1E-24);

{ Unit of cubic kelvin }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicKelvinQty}{$DEFINE TUnit:=TCubicKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicKelvins = TCubicKelvinQty;

var
  K3: TCubicKelvinUnit;

{ Unit of lux }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLuxQty}{$DEFINE TUnit:=TLuxUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TLux = TLuxQty;

var
  lx: TLuxUnit;

{ Unit of volt meter per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TVoltMetersPerSecond = TVoltMeterPerSecondQty;

{ Unit of square kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramQty}{$DEFINE TUnit:=TSquareKilogramUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramUnit): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramUnit): TSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramUnit): TNewtonPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareKilograms = TSquareKilogramQty;

var
  kg2: TSquareKilogramUnit;
  hg2: TSquareKilogramQty = (FValue: 1E-02);
  dag2: TSquareKilogramQty = (FValue: 1E-04);
  g2: TSquareKilogramQty = (FValue: 1E-06);
  dg2: TSquareKilogramQty = (FValue: 1E-08);
  cg2: TSquareKilogramQty = (FValue: 1E-10);
  mg2: TSquareKilogramQty = (FValue: 1E-12);
  mig2: TSquareKilogramQty = (FValue: 1E-18);
  ng2: TSquareKilogramQty = (FValue: 1E-24);
  pg2: TSquareKilogramQty = (FValue: 1E-30);

{ Unit of day }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDayQty}{$DEFINE TUnit:=TDayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TDays = TSecondQty;

const
  day: TSecondQty = (FValue: 86400);

{ Unit of hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THourQty}{$DEFINE TUnit:=THourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  THours = TSecondQty;

const
  hr: TSecondQty = (FValue: 3600);

{ Unit of minute }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMinuteQty}{$DEFINE TUnit:=TMinuteUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMinutes = TSecondQty;

const
  minute: TSecondQty = (FValue: 60);

{ Unit of square day }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareDayQty}{$DEFINE TUnit:=TSquareDayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareDays = TSquareSecondQty;

const
  day2: TSquareSecondQty = (FValue: 7464960000);

{ Unit of square hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareHourQty}{$DEFINE TUnit:=TSquareHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareHours = TSquareSecondQty;

const
  hr2: TSquareSecondQty = (FValue: 12960000);

{ Unit of square minute }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMinuteQty}{$DEFINE TUnit:=TSquareMinuteUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMinutes = TSquareSecondQty;

const
  minute2: TSquareSecondQty = (FValue: 3600);

{ Unit of astronomical }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAstronomicalQty}{$DEFINE TUnit:=TAstronomicalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TAstronomical = TMeterQty;

const
  au: TMeterQty = (FValue: 149597870691);

{ Unit of inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TInchQty}{$DEFINE TUnit:=TInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TInches = TMeterQty;

const
  inch: TMeterQty = (FValue: 0.0254);

{ Unit of foot }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFootQty}{$DEFINE TUnit:=TFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TFeet = TMeterQty;

const
  ft: TMeterQty = (FValue: 0.3048);

{ Unit of yard }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TYardQty}{$DEFINE TUnit:=TYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TYards = TMeterQty;

const
  yd: TMeterQty = (FValue: 0.9144);

{ Unit of mile }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMileQty}{$DEFINE TUnit:=TMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMiles = TMeterQty;

const
  mi: TMeterQty = (FValue: 1609.344);

{ Unit of nautical mile }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNauticalMileQty}{$DEFINE TUnit:=TNauticalMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNauticalMiles = TMeterQty;

const
  nmi: TMeterQty = (FValue: 1852);

{ Unit of square inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareInchQty}{$DEFINE TUnit:=TSquareInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareInches = TSquareMeterQty;

const
  inch2: TSquareMeterQty = (FValue: 0.00064516);

{ Unit of square foot }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareFootQty}{$DEFINE TUnit:=TSquareFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareFeet = TSquareMeterQty;

const
  ft2: TSquareMeterQty = (FValue: 0.09290304);

{ Unit of square yard }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareYardQty}{$DEFINE TUnit:=TSquareYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareYards = TSquareMeterQty;

const
  yd2: TSquareMeterQty = (FValue: 0.83612736);

{ Unit of square mile }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMileQty}{$DEFINE TUnit:=TSquareMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareMiles = TSquareMeterQty;

const
  mi2: TSquareMeterQty = (FValue: 2589988.110336);

{ Unit of cubic inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicInchQty}{$DEFINE TUnit:=TCubicInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicInches = TCubicMeterQty;

const
  inch3: TCubicMeterQty = (FValue: 0.000016387064);

{ Unit of cubic foot }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicFootQty}{$DEFINE TUnit:=TCubicFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicFeet = TCubicMeterQty;

const
  ft3: TCubicMeterQty = (FValue: 0.028316846592);

{ Unit of cubic yard }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicYardQty}{$DEFINE TUnit:=TCubicYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TCubicYards = TCubicMeterQty;

const
  yd3: TCubicMeterQty = (FValue: 0.764554857984);

{ Unit of litre }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLitreQty}{$DEFINE TUnit:=TLitreUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TLitres = TCubicMeterQty;

const
  L: TCubicMeterQty = (FValue: 1E-03);

const
  dL: TCubicMeterQty = (FValue: 1E-03 * 1E-01);
  cL: TCubicMeterQty = (FValue: 1E-03 * 1E-02);
  mL: TCubicMeterQty = (FValue: 1E-03 * 1E-03);

{ Unit of gallon }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TGallonQty}{$DEFINE TUnit:=TGallonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TGallons = TCubicMeterQty;

const
  gal: TCubicMeterQty = (FValue: 0.0037854119678);

{ Unit of tonne }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTonneQty}{$DEFINE TUnit:=TTonneUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTonnes = TKilogramQty;

const
  tonne: TKilogramQty = (FValue: 1E+03);

const
  gigatonne: TKilogramQty = (FValue: 1E+03 * 1E+09);
  megatonne: TKilogramQty = (FValue: 1E+03 * 1E+06);
  kilotonne: TKilogramQty = (FValue: 1E+03 * 1E+03);

{ Unit of pound }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundQty}{$DEFINE TUnit:=TPoundUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPounds = TKilogramQty;

const
  lb: TKilogramQty = (FValue: 0.45359237);

{ Unit of ounce }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOunceQty}{$DEFINE TUnit:=TOunceUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TOunces = TKilogramQty;

const
  oz: TKilogramQty = (FValue: 0.028349523125);

{ Unit of stone }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TStoneQty}{$DEFINE TUnit:=TStoneUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TStones = TKilogramQty;

const
  st: TKilogramQty = (FValue: 6.35029318);

{ Unit of ton }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTonQty}{$DEFINE TUnit:=TTonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTons = TKilogramQty;

const
  ton: TKilogramQty = (FValue: 907.18474);

{ Unit of degree celsius }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TDegreesCelsius = TKelvinQty;

var
  degC: TDegreeCelsiusUnit;

{ Unit of degree fahrenheit }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TDegreesFahrenheit = TKelvinQty;

var
  degF: TDegreeFahrenheitUnit;

{ Unit of degree }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeQty}{$DEFINE TUnit:=TDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TDegrees = TRadianQty;

const
  deg: TRadianQty = (FValue: Pi/180);

{ Unit of square degree }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareDegreeQty}{$DEFINE TUnit:=TSquareDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSquareDegrees = TSteradianQty;

const
  deg2: TSteradianQty = (FValue: Pi*Pi/32400);

{ Unit of radian per second squared }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$DEFINE TUnit:=TRadianPerSecondSquaredUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TRadiansPerSecondSquared = TSquareHertzQty;

{ Unit of steradian per square second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSteradiansPerSquareSecond = TSquareHertzQty;

{ Unit of meter per hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerHourQty}{$DEFINE TUnit:=TMeterPerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerHour = TMeterPerSecondQty;

{ Unit of mile per hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMilePerHourQty}{$DEFINE TUnit:=TMilePerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMilesPerHour = TMeterPerSecondQty;

{ Unit of nautical mile per hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNauticalMilesPerHour = TMeterPerSecondQty;

{ Unit of meter per second per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerSecondPerSecond = TMeterPerSecondSquaredQty;

{ Unit of meter per hour per second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TMetersPerHourPerSecond = TMeterPerSecondSquaredQty;

{ Unit of newton second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSecondQty}{$DEFINE TUnit:=TNewtonSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonSeconds = TKilogramMeterPerSecondQty;

{ Unit of pound force }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForceQty}{$DEFINE TUnit:=TPoundForceUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPoundsForce = TNewtonQty;

const
  lbf: TNewtonQty = (FValue: 4.4482216152605);

{ Unit of bar }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TBarQty}{$DEFINE TUnit:=TBarUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TBars = TPascalQty;

const
  bar: TPascalQty = (FValue: 1E+05);

const
  kbar: TPascalQty = (FValue: 1E+05 * 1E+03);
  mbar: TPascalQty = (FValue: 1E+05 * 1E-03);

{ Unit of pound per square inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPoundsPerSquareInch = TPascalQty;

const
  psi: TPascalQty = (FValue: 6894.75729316836);

const
  kpsi: TPascalQty = (FValue: 6894.75729316836 * 1E+03);

{ Unit of joule per cubic meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerCubicMeter = TPascalQty;

{ Unit of watt hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattHourQty}{$DEFINE TUnit:=TWattHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TWattHours = TJouleQty;

{ Unit of elettronvolt }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TElettronvoltQty}{$DEFINE TUnit:=TElettronvoltUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TElettronvolts = TJouleQty;

const
  eV: TJouleQty = (FValue: 1.60217742320523E-019);

const
  TeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+12);
  GeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+09);
  MeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+06);
  keV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+03);

{ Unit of newton meter }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonMeterQty}{$DEFINE TUnit:=TNewtonMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonMeters = TJouleQty;

{ Unit of pound force inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForceInchQty}{$DEFINE TUnit:=TPoundForceInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPoundForceInches = TJouleQty;

{ Unit of ampere hour }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmpereHourQty}{$DEFINE TUnit:=TAmpereHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TAmpereHours = TCoulombQty;

{ Unit of bequerel }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TBequerelQty}{$DEFINE TUnit:=TBequerelUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TBequerels = THertzQty;

var
  Bq: THertzUnit;

const
  kBq: THertzQty = (FValue: 1E+03);
  mBq: THertzQty = (FValue: 1E-03);
  miBq: THertzQty = (FValue: 1E-06);
  nBq: THertzQty = (FValue: 1E-09);
  pBq: THertzQty = (FValue: 1E-12);

{ Unit of gray }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TGrayQty}{$DEFINE TUnit:=TGrayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TGrays = TSquareMeterPerSquareSecondQty;

var
  Gy: TSquareMeterPerSquareSecondUnit;

const
  kGy: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
  mGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
  miGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
  nGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

{ Unit of sievert }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSievertQty}{$DEFINE TUnit:=TSievertUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TSieverts = TSquareMeterPerSquareSecondQty;

var
  Sv: TSquareMeterPerSquareSecondUnit;

const
  kSv: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
  mSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
  miSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
  nSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

{ Unit of joule per degree }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerDegree = TJoulePerRadianQty;

{ Unit of newton meter per radian }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonMetersPerRadian = TJoulePerRadianQty;

{ Unit of newton meter per degree }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonMetersPerDegree = TJoulePerRadianQty;

{ Unit of pound force per inch }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPoundsForcePerInch = TNewtonPerMeterQty;

{ Unit of pascal second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPascalSecondQty}{$DEFINE TUnit:=TPascalSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TPascalSeconds = TPoiseuilleQty;

{ Unit of joule per kilogram }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJoulesPerKilogram = TSquareMeterPerSquareSecondQty;

{ Unit of newton per coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerCoulomb = TVoltPerMeterQty;

{ Unit of newton square meter per coulomb }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonSquareMetersPerCoulomb = TVoltMeterQty;

{ Unit of newton per ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerAmpere = TTeslaMeterQty;

{ Unit of tesla meter per ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TTeslaMetersPerAmpere = THenryPerMeterQty;

{ Unit of newton per square ampere }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TNewtonsPerSquareAmpere = THenryPerMeterQty;

{ Unit of joule second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJouleSecondQty}{$DEFINE TUnit:=TJouleSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TJouleSeconds = TKilogramSquareMeterPerSecondQty;

{ Unit of elettronvolt second }
{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

type
  TElettronvoltSeconds = TKilogramSquareMeterPerSecondQty;

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
  THertzHelper = record helper for THertzQty
    function ToBequerel: TBequerelQty;
    function ToRadianPerSecond: TRadianPerSecondQty;
  end;

type
  TRadianPerSecondHelper = record helper for TRadianPerSecondQty
    function ToHertz: THertzQty;
  end;

type
  TSquareHertzHelper = record helper for TSquareHertzQty
    function ToSteradianPerSquareSecond: TSteradianPerSquareSecondQty;
    function ToRadianPerSecondSquared: TRadianPerSecondSquaredQty;
  end;

type
  TRadianPerSecondSquaredHelper = record helper for TRadianPerSecondSquaredQty
    function ToSquareHertz: TSquareHertzQty;
  end;

type
  TSteradianPerSquareSecondHelper = record helper for TSteradianPerSquareSecondQty
    function ToSquareHertz: TSquareHertzQty;
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
  TMeterPerSecondPerSecondHelper = record helper for TMeterPerSecondPerSecondQty
    function ToMeterPerSecondSquared: TMeterPerSecondSquaredQty;
  end;

type
  TKilogramMeterPerSecondHelper = record helper for TKilogramMeterPerSecondQty
    function ToNewtonSecond: TNewtonSecondQty;
  end;

type
  TNewtonSecondHelper = record helper for TNewtonSecondQty
    function ToKilogramMeterPerSecond: TKilogramMeterPerSecondQty;
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
  TJoulePerCubicMeterHelper = record helper for TJoulePerCubicMeterQty
    function ToPascal: TPascalQty;
  end;

type
  TJouleHelper = record helper for TJouleQty
    function ToPoundForceInch: TPoundForceInchQty;
    function ToNewtonMeter: TNewtonMeterQty;
    function ToElettronvolt: TElettronvoltQty;
    function ToWattHour: TWattHourQty;
  end;

type
  TNewtonMeterHelper = record helper for TNewtonMeterQty
    function ToJoule: TJouleQty;
  end;

type
  TCoulombHelper = record helper for TCoulombQty
    function ToAmpereHour: TAmpereHourQty;
  end;

type
  TBequerelHelper = record helper for TBequerelQty
    function ToHertz: THertzQty;
  end;

type
  TSquareMeterPerSquareSecondHelper = record helper for TSquareMeterPerSquareSecondQty
    function ToJoulePerKilogram: TJoulePerKilogramQty;
    function ToSievert: TSievertQty;
    function ToGray: TGrayQty;
  end;

type
  TGrayHelper = record helper for TGrayQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

type
  TSievertHelper = record helper for TSievertQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

type
  TJoulePerRadianHelper = record helper for TJoulePerRadianQty
    function ToNewtonMeterPerDegree: TNewtonMeterPerDegreeQty;
    function ToNewtonMeterPerRadian: TNewtonMeterPerRadianQty;
    function ToJoulePerDegree: TJoulePerDegreeQty;
  end;

type
  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianQty
    function ToJoulePerRadian: TJoulePerRadianQty;
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
  TPascalSecondHelper = record helper for TPascalSecondQty
    function ToPoiseuille: TPoiseuilleQty;
  end;

type
  TJoulePerKilogramHelper = record helper for TJoulePerKilogramQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

type
  TVoltPerMeterHelper = record helper for TVoltPerMeterQty
    function ToNewtonPerCoulomb: TNewtonPerCoulombQty;
  end;

type
  TNewtonPerCoulombHelper = record helper for TNewtonPerCoulombQty
    function ToVoltPerMeter: TVoltPerMeterQty;
  end;

type
  TVoltMeterHelper = record helper for TVoltMeterQty
    function ToNewtonSquareMeterPerCoulomb: TNewtonSquareMeterPerCoulombQty;
  end;

type
  TNewtonSquareMeterPerCoulombHelper = record helper for TNewtonSquareMeterPerCoulombQty
    function ToVoltMeter: TVoltMeterQty;
  end;

type
  TTeslaMeterHelper = record helper for TTeslaMeterQty
    function ToNewtonPerAmpere: TNewtonPerAmpereQty;
  end;

type
  TNewtonPerAmpereHelper = record helper for TNewtonPerAmpereQty
    function ToTeslaMeter: TTeslaMeterQty;
  end;

type
  THenryPerMeterHelper = record helper for THenryPerMeterQty
    function ToNewtonPerSquareAmpere: TNewtonPerSquareAmpereQty;
    function ToTeslaMeterPerAmpere: TTeslaMeterPerAmpereQty;
  end;

type
  TTeslaMeterPerAmpereHelper = record helper for TTeslaMeterPerAmpereQty
    function ToHenryPerMeter: THenryPerMeterQty;
  end;

type
  TNewtonPerSquareAmpereHelper = record helper for TNewtonPerSquareAmpereQty
    function ToHenryPerMeter: THenryPerMeterQty;
  end;

type
  TKilogramSquareMeterPerSecondHelper = record helper for TKilogramSquareMeterPerSecondQty
    function ToElettronvoltSecond: TElettronvoltSecondQty;
    function ToJouleSecond: TJouleSecondQty;
  end;

type
  TJouleSecondHelper = record helper for TJouleSecondQty
    function ToKilogramSquareMeterPerSecond: TKilogramSquareMeterPerSecondQty;
  end;

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

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i adim.inc}
class function TCubicMeterPerKilogramQty.Symbol: string; begin result := '%sm3/%sg' end;
class function TCubicMeterPerKilogramQty.SingularName: string; begin result := 'cubic %smeter per %sgram' end;
class function TCubicMeterPerKilogramQty.PluralName: string; begin result := 'cubic %smeters per %sgram' end;
class function TCubicMeterPerKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pKilo]; end;
class function TCubicMeterPerKilogramQty.Exponents: TExponents; begin result := [3, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i adim.inc}
class function TTeslaQty.Symbol: string; begin result := '%sT' end;
class function TTeslaQty.SingularName: string; begin result := '%stesla' end;
class function TTeslaQty.PluralName: string; begin result := '%steslas' end;
class function TTeslaQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TTeslaQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i adim.inc}
class function TSquareKelvinQty.Symbol: string; begin result := '%sK2' end;
class function TSquareKelvinQty.SingularName: string; begin result := 'square %skelvin' end;
class function TSquareKelvinQty.PluralName: string; begin result := 'square %skelvins' end;
class function TSquareKelvinQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareKelvinQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i adim.inc}
class function TOhmMeterQty.Symbol: string; begin result := '%sΩ·%sm' end;
class function TOhmMeterQty.SingularName: string; begin result := '%sohm %smeter' end;
class function TOhmMeterQty.PluralName: string; begin result := '%sohm %smeters' end;
class function TOhmMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TOhmMeterQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i adim.inc}
class function THertzQty.Symbol: string; begin result := '%sHz' end;
class function THertzQty.SingularName: string; begin result := '%shertz' end;
class function THertzQty.PluralName: string; begin result := '%shertz' end;
class function THertzQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function THertzQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i adim.inc}
class function TSquareVoltQty.Symbol: string; begin result := '%sV2' end;
class function TSquareVoltQty.SingularName: string; begin result := 'square %svolt' end;
class function TSquareVoltQty.PluralName: string; begin result := 'square %svolts' end;
class function TSquareVoltQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareVoltQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i adim.inc}
class function TJoulePerMoleQty.Symbol: string; begin result := '%sJ/%smol' end;
class function TJoulePerMoleQty.SingularName: string; begin result := '%sjoule per %smole' end;
class function TJoulePerMoleQty.PluralName: string; begin result := '%sjoules per %smole' end;
class function TJoulePerMoleQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TJoulePerMoleQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i adim.inc}
class function TSquareCoulombPerMeterQty.Symbol: string; begin result := '%sC2/%sm' end;
class function TSquareCoulombPerMeterQty.SingularName: string; begin result := 'square %scoulomb per %smeter' end;
class function TSquareCoulombPerMeterQty.PluralName: string; begin result := 'square %scoulombs per %smeter' end;
class function TSquareCoulombPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareCoulombPerMeterQty.Exponents: TExponents; begin result := [2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i adim.inc}
class function TPascalQty.Symbol: string; begin result := '%sPa' end;
class function TPascalQty.SingularName: string; begin result := '%spascal' end;
class function TPascalQty.PluralName: string; begin result := '%spascals' end;
class function TPascalQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TPascalQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i adim.inc}
class function TReciprocalMeterQty.Symbol: string; begin result := '1/%sm' end;
class function TReciprocalMeterQty.SingularName: string; begin result := 'reciprocal %smeter' end;
class function TReciprocalMeterQty.PluralName: string; begin result := 'reciprocal %smeters' end;
class function TReciprocalMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TReciprocalMeterQty.Exponents: TExponents; begin result := [-1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i adim.inc}
class function TNewtonPerMeterQty.Symbol: string; begin result := '%sN/%sm' end;
class function TNewtonPerMeterQty.SingularName: string; begin result := '%snewton per %smeter' end;
class function TNewtonPerMeterQty.PluralName: string; begin result := '%snewtons per %smeter' end;
class function TNewtonPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i adim.inc}
class function TNewtonPerCubicMeterQty.Symbol: string; begin result := '%sN/%sm3' end;
class function TNewtonPerCubicMeterQty.SingularName: string; begin result := '%snewton per cubic %smeter' end;
class function TNewtonPerCubicMeterQty.PluralName: string; begin result := '%snewtons per cubic %smeter' end;
class function TNewtonPerCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i adim.inc}
class function TWattPerMeterQty.Symbol: string; begin result := '%sW/%sm' end;
class function TWattPerMeterQty.SingularName: string; begin result := '%swatt per %smeter' end;
class function TWattPerMeterQty.PluralName: string; begin result := '%swatts per %smeter' end;
class function TWattPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TWattPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i adim.inc}
class function TAmperePerMeterQty.Symbol: string; begin result := '%sA/%sm' end;
class function TAmperePerMeterQty.SingularName: string; begin result := '%sampere per %smeter' end;
class function TAmperePerMeterQty.PluralName: string; begin result := '%samperes per %smeter' end;
class function TAmperePerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TAmperePerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i adim.inc}
class function TReciprocalKelvinQty.Symbol: string; begin result := '1/%sK' end;
class function TReciprocalKelvinQty.SingularName: string; begin result := 'reciprocal %skelvin' end;
class function TReciprocalKelvinQty.PluralName: string; begin result := 'reciprocal %skelvin' end;
class function TReciprocalKelvinQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TReciprocalKelvinQty.Exponents: TExponents; begin result := [-1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i adim.inc}
class function TTeslaPerAmpereQty.Symbol: string; begin result := '%sT/%sA' end;
class function TTeslaPerAmpereQty.SingularName: string; begin result := '%stesla per %sampere' end;
class function TTeslaPerAmpereQty.PluralName: string; begin result := '%steslas per %sampere' end;
class function TTeslaPerAmpereQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TTeslaPerAmpereQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i adim.inc}
class function TKilogramPerMeterQty.Symbol: string; begin result := '%sg/%sm' end;
class function TKilogramPerMeterQty.SingularName: string; begin result := '%sgram per %smeter' end;
class function TKilogramPerMeterQty.PluralName: string; begin result := '%sgrams per %smeter' end;
class function TKilogramPerMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i adim.inc}
class function THenryPerMeterQty.Symbol: string; begin result := '%sH/%sm' end;
class function THenryPerMeterQty.SingularName: string; begin result := '%shenry per %smeter' end;
class function THenryPerMeterQty.PluralName: string; begin result := '%shenries per %smeter' end;
class function THenryPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function THenryPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i adim.inc}
class function TKatalQty.Symbol: string; begin result := '%skat' end;
class function TKatalQty.SingularName: string; begin result := '%skatal' end;
class function TKatalQty.PluralName: string; begin result := '%skatals' end;
class function TKatalQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TKatalQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i adim.inc}
class function TSquareMeterPerSecondQty.Symbol: string; begin result := '%sm2/%ss' end;
class function TSquareMeterPerSecondQty.SingularName: string; begin result := 'square %smeter per %ssecond' end;
class function TSquareMeterPerSecondQty.PluralName: string; begin result := 'square %smeters per %ssecond' end;
class function TSquareMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareMeterPerSecondQty.Exponents: TExponents; begin result := [2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i adim.inc}
class function TRadianPerMeterQty.Symbol: string; begin result := 'rad/%sm' end;
class function TRadianPerMeterQty.SingularName: string; begin result := 'radian per %smeter' end;
class function TRadianPerMeterQty.PluralName: string; begin result := 'radians per %smeter' end;
class function TRadianPerMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TRadianPerMeterQty.Exponents: TExponents; begin result := [-1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i adim.inc}
class function TWattQty.Symbol: string; begin result := '%sW' end;
class function TWattQty.SingularName: string; begin result := '%swatt' end;
class function TWattQty.PluralName: string; begin result := '%swatts' end;
class function TWattQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TWattQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i adim.inc}
class function TCoulombPerMoleQty.Symbol: string; begin result := '%sC/%smol' end;
class function TCoulombPerMoleQty.SingularName: string; begin result := '%scoulomb per %smole' end;
class function TCoulombPerMoleQty.PluralName: string; begin result := '%scoulombs per %smole' end;
class function TCoulombPerMoleQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCoulombPerMoleQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i adim.inc}
class function TKelvinQty.Symbol: string; begin result := '%sK' end;
class function TKelvinQty.SingularName: string; begin result := '%skelvin' end;
class function TKelvinQty.PluralName: string; begin result := '%skelvins' end;
class function TKelvinQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TKelvinQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i adim.inc}
class function TJouleQty.Symbol: string; begin result := '%sJ' end;
class function TJouleQty.SingularName: string; begin result := '%sjoule' end;
class function TJouleQty.PluralName: string; begin result := '%sjoules' end;
class function TJouleQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TJouleQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i adim.inc}
class function TReciprocalMoleQty.Symbol: string; begin result := '1/%smol' end;
class function TReciprocalMoleQty.SingularName: string; begin result := 'reciprocal %smole' end;
class function TReciprocalMoleQty.PluralName: string; begin result := 'reciprocal %smoles' end;
class function TReciprocalMoleQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TReciprocalMoleQty.Exponents: TExponents; begin result := [-1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i adim.inc}
class function TCoulombQty.Symbol: string; begin result := '%sC' end;
class function TCoulombQty.SingularName: string; begin result := '%scoulomb' end;
class function TCoulombQty.PluralName: string; begin result := '%scoulombs' end;
class function TCoulombQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TCoulombQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i adim.inc}
class function TCubicMeterPerSquareSecondQty.Symbol: string; begin result := '%sm3/%ss2' end;
class function TCubicMeterPerSquareSecondQty.SingularName: string; begin result := 'cubic %smeter per square %ssecond' end;
class function TCubicMeterPerSquareSecondQty.PluralName: string; begin result := 'cubic %smeters per square %ssecond' end;
class function TCubicMeterPerSquareSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCubicMeterPerSquareSecondQty.Exponents: TExponents; begin result := [3, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i adim.inc}
class function TKatalPerCubicMeterQty.Symbol: string; begin result := '%skat/%sm3' end;
class function TKatalPerCubicMeterQty.SingularName: string; begin result := '%skatal per cubic %smeter' end;
class function TKatalPerCubicMeterQty.PluralName: string; begin result := '%skatals per cubic %smeter' end;
class function TKatalPerCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TKatalPerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i adim.inc}
class function TWattPerKelvinQty.Symbol: string; begin result := '%sW/%sK' end;
class function TWattPerKelvinQty.SingularName: string; begin result := '%swatt per %skelvin' end;
class function TWattPerKelvinQty.PluralName: string; begin result := '%swatts per %skelvin' end;
class function TWattPerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TWattPerKelvinQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i adim.inc}
class function TJoulePerRadianQty.Symbol: string; begin result := '%sJ/rad' end;
class function TJoulePerRadianQty.SingularName: string; begin result := '%sjoule per radian' end;
class function TJoulePerRadianQty.PluralName: string; begin result := '%sjoules per radian' end;
class function TJoulePerRadianQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TJoulePerRadianQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i adim.inc}
class function TRadianPerSecondQty.Symbol: string; begin result := 'rad/%ss' end;
class function TRadianPerSecondQty.SingularName: string; begin result := 'radian per %ssecond' end;
class function TRadianPerSecondQty.PluralName: string; begin result := 'radians per %ssecond' end;
class function TRadianPerSecondQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TRadianPerSecondQty.Exponents: TExponents; begin result := [-1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i adim.inc}
class function TCandelaQty.Symbol: string; begin result := '%scd' end;
class function TCandelaQty.SingularName: string; begin result := '%scandela' end;
class function TCandelaQty.PluralName: string; begin result := '%scandelas' end;
class function TCandelaQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TCandelaQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i adim.inc}
class function TVoltQty.Symbol: string; begin result := '%sV' end;
class function TVoltQty.SingularName: string; begin result := '%svolt' end;
class function TVoltQty.PluralName: string; begin result := '%svolts' end;
class function TVoltQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TVoltQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i adim.inc}
class function TWeberQty.Symbol: string; begin result := '%sWb' end;
class function TWeberQty.SingularName: string; begin result := '%sweber' end;
class function TWeberQty.PluralName: string; begin result := '%swebers' end;
class function TWeberQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TWeberQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQty}{$i adim.inc}
class function TSquareMeterQty.Symbol: string; begin result := '%sm2' end;
class function TSquareMeterQty.SingularName: string; begin result := 'square %smeter' end;
class function TSquareMeterQty.PluralName: string; begin result := 'square %smeters' end;
class function TSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareMeterQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i adim.inc}
class function TSquareMeterPerSquareSecondQty.Symbol: string; begin result := '%sm2/%ss2' end;
class function TSquareMeterPerSquareSecondQty.SingularName: string; begin result := 'square %smeter per square %ssecond' end;
class function TSquareMeterPerSquareSecondQty.PluralName: string; begin result := 'square %smeters per square %ssecond' end;
class function TSquareMeterPerSquareSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareMeterPerSquareSecondQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i adim.inc}
class function TQuinticMeterQty.Symbol: string; begin result := '%sm5' end;
class function TQuinticMeterQty.SingularName: string; begin result := 'quintic %smeter' end;
class function TQuinticMeterQty.PluralName: string; begin result := 'quintic %smeters' end;
class function TQuinticMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TQuinticMeterQty.Exponents: TExponents; begin result := [5]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i adim.inc}
class function TRadianQty.Symbol: string; begin result := 'rad' end;
class function TRadianQty.SingularName: string; begin result := 'radian' end;
class function TRadianQty.PluralName: string; begin result := 'radians' end;
class function TRadianQty.Prefixes: TPrefixes; begin result := []; end;
class function TRadianQty.Exponents: TExponents; begin result := []; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i adim.inc}
class function TSquareKilogramPerSquareSecondQty.Symbol: string; begin result := '%sg2/%ss2' end;
class function TSquareKilogramPerSquareSecondQty.SingularName: string; begin result := 'square %sgram per square %ssecond' end;
class function TSquareKilogramPerSquareSecondQty.PluralName: string; begin result := 'square %sgrams per square %ssecond' end;
class function TSquareKilogramPerSquareSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TSquareKilogramPerSquareSecondQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i adim.inc}
class function TTeslaMeterQty.Symbol: string; begin result := '%sT·%sm' end;
class function TTeslaMeterQty.SingularName: string; begin result := '%stesla %smeter' end;
class function TTeslaMeterQty.PluralName: string; begin result := '%stesla %smeters' end;
class function TTeslaMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TTeslaMeterQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i adim.inc}
class function TMeterKelvinQty.Symbol: string; begin result := '%sm·%sK' end;
class function TMeterKelvinQty.SingularName: string; begin result := '%smeter %skelvin' end;
class function TMeterKelvinQty.PluralName: string; begin result := '%smeter %skelvins' end;
class function TMeterKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMeterKelvinQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i adim.inc}
class function TWattPerSteradianQty.Symbol: string; begin result := '%sW/sr' end;
class function TWattPerSteradianQty.SingularName: string; begin result := '%swatt per steradian' end;
class function TWattPerSteradianQty.PluralName: string; begin result := '%swatts per steradian' end;
class function TWattPerSteradianQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TWattPerSteradianQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i adim.inc}
class function TAmpereQty.Symbol: string; begin result := '%sA' end;
class function TAmpereQty.SingularName: string; begin result := '%sampere' end;
class function TAmpereQty.PluralName: string; begin result := '%samperes' end;
class function TAmpereQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TAmpereQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i adim.inc}
class function TSquareSecondPerSquareMeterQty.Symbol: string; begin result := '%ss2/%sm2' end;
class function TSquareSecondPerSquareMeterQty.SingularName: string; begin result := 'square %ssecond per square %smeter' end;
class function TSquareSecondPerSquareMeterQty.PluralName: string; begin result := 'square %sseconds per square %smeter' end;
class function TSquareSecondPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareSecondPerSquareMeterQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i adim.inc}
class function TNewtonQty.Symbol: string; begin result := '%sN' end;
class function TNewtonQty.SingularName: string; begin result := '%snewton' end;
class function TNewtonQty.PluralName: string; begin result := '%snewtons' end;
class function TNewtonQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TNewtonQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i adim.inc}
class function TSquareKilogramPerMeterQty.Symbol: string; begin result := '%sg2/%sm' end;
class function TSquareKilogramPerMeterQty.SingularName: string; begin result := 'square %sgram per %smeter' end;
class function TSquareKilogramPerMeterQty.PluralName: string; begin result := 'square %sgrams per %smeter' end;
class function TSquareKilogramPerMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TSquareKilogramPerMeterQty.Exponents: TExponents; begin result := [2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i adim.inc}
class function TWattPerQuarticKelvinQty.Symbol: string; begin result := '%sW/%sK4' end;
class function TWattPerQuarticKelvinQty.SingularName: string; begin result := '%swatt per quartic %skelvin' end;
class function TWattPerQuarticKelvinQty.PluralName: string; begin result := '%swatts per quartic %skelvin' end;
class function TWattPerQuarticKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TWattPerQuarticKelvinQty.Exponents: TExponents; begin result := [1, -4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i adim.inc}
class function TQuarticMeterSecondQty.Symbol: string; begin result := '%sm4·%ss' end;
class function TQuarticMeterSecondQty.SingularName: string; begin result := 'quartic %smeter %ssecond' end;
class function TQuarticMeterSecondQty.PluralName: string; begin result := 'quartic %smeter %sseconds' end;
class function TQuarticMeterSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TQuarticMeterSecondQty.Exponents: TExponents; begin result := [4, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i adim.inc}
class function TQuarticKelvinQty.Symbol: string; begin result := '%sK4' end;
class function TQuarticKelvinQty.SingularName: string; begin result := 'quartic %skelvin' end;
class function TQuarticKelvinQty.PluralName: string; begin result := 'quartic %skelvins' end;
class function TQuarticKelvinQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TQuarticKelvinQty.Exponents: TExponents; begin result := [4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i adim.inc}
class function TCandelaPerSquareMeterQty.Symbol: string; begin result := '%scd/%sm2' end;
class function TCandelaPerSquareMeterQty.SingularName: string; begin result := '%scandela per square %smeter' end;
class function TCandelaPerSquareMeterQty.PluralName: string; begin result := '%scandelas per square %smeter' end;
class function TCandelaPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCandelaPerSquareMeterQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i adim.inc}
class function TSquareMeterPerSquareKilogramQty.Symbol: string; begin result := '%sm2/%sg2' end;
class function TSquareMeterPerSquareKilogramQty.SingularName: string; begin result := 'square %smeter per square %sgram' end;
class function TSquareMeterPerSquareKilogramQty.PluralName: string; begin result := 'square %smeters per square %sgram' end;
class function TSquareMeterPerSquareKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pKilo]; end;
class function TSquareMeterPerSquareKilogramQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i adim.inc}
class function TLumenPerWattQty.Symbol: string; begin result := '%slm/%sW' end;
class function TLumenPerWattQty.SingularName: string; begin result := '%slumen per %swatt' end;
class function TLumenPerWattQty.PluralName: string; begin result := '%slumens per %swatt' end;
class function TLumenPerWattQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TLumenPerWattQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i adim.inc}
class function TKilogramPerSecondQty.Symbol: string; begin result := '%sg/%ss' end;
class function TKilogramPerSecondQty.SingularName: string; begin result := '%sgram per %ssecond' end;
class function TKilogramPerSecondQty.PluralName: string; begin result := '%sgrams per %ssecond' end;
class function TKilogramPerSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramPerSecondQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i adim.inc}
class function TWattPerSquareMeterQty.Symbol: string; begin result := '%sW/%sm2' end;
class function TWattPerSquareMeterQty.SingularName: string; begin result := '%swatt per square %smeter' end;
class function TWattPerSquareMeterQty.PluralName: string; begin result := '%swatts per square %smeter' end;
class function TWattPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TWattPerSquareMeterQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i adim.inc}
class function TNewtonSquareMeterPerSquareCoulombQty.Symbol: string; begin result := '%sN·%sm2/%sC2' end;
class function TNewtonSquareMeterPerSquareCoulombQty.SingularName: string; begin result := '%snewton square %smeter per square %scoulomb' end;
class function TNewtonSquareMeterPerSquareCoulombQty.PluralName: string; begin result := '%snewton square %smeters per square %scoulomb' end;
class function TNewtonSquareMeterPerSquareCoulombQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TNewtonSquareMeterPerSquareCoulombQty.Exponents: TExponents; begin result := [1, 2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i adim.inc}
class function TVoltPerMeterQty.Symbol: string; begin result := '%sV/%sm' end;
class function TVoltPerMeterQty.SingularName: string; begin result := '%svolt per %smeter' end;
class function TVoltPerMeterQty.PluralName: string; begin result := '%svolts per %smeter' end;
class function TVoltPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TVoltPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i adim.inc}
class function TMoleKelvinQty.Symbol: string; begin result := '%smol·%sK' end;
class function TMoleKelvinQty.SingularName: string; begin result := '%smole %skelvin' end;
class function TMoleKelvinQty.PluralName: string; begin result := '%smole %skelvins' end;
class function TMoleKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMoleKelvinQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i adim.inc}
class function TSquareAmpereQty.Symbol: string; begin result := '%sA2' end;
class function TSquareAmpereQty.SingularName: string; begin result := 'square %sampere' end;
class function TSquareAmpereQty.PluralName: string; begin result := 'square %samperes' end;
class function TSquareAmpereQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareAmpereQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i adim.inc}
class function TKilogramSquareMeterPerSecondQty.Symbol: string; begin result := '%sg·%sm2/%ss' end;
class function TKilogramSquareMeterPerSecondQty.SingularName: string; begin result := '%sgram square %smeter per %ssecond' end;
class function TKilogramSquareMeterPerSecondQty.PluralName: string; begin result := '%sgram square %smeters per %ssecond' end;
class function TKilogramSquareMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone, pNone]; end;
class function TKilogramSquareMeterPerSecondQty.Exponents: TExponents; begin result := [1, 2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i adim.inc}
class function TKilogramSquareSecondQty.Symbol: string; begin result := '%sg·%ss2' end;
class function TKilogramSquareSecondQty.SingularName: string; begin result := '%sgram square %ssecond' end;
class function TKilogramSquareSecondQty.PluralName: string; begin result := '%sgram square %sseconds' end;
class function TKilogramSquareSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramSquareSecondQty.Exponents: TExponents; begin result := [1, 2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i adim.inc}
class function TSteradianQty.Symbol: string; begin result := 'sr' end;
class function TSteradianQty.SingularName: string; begin result := 'steradian' end;
class function TSteradianQty.PluralName: string; begin result := 'steradians' end;
class function TSteradianQty.Prefixes: TPrefixes; begin result := []; end;
class function TSteradianQty.Exponents: TExponents; begin result := []; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i adim.inc}
class function TCoulombPerMeterQty.Symbol: string; begin result := '%sC/%sm' end;
class function TCoulombPerMeterQty.SingularName: string; begin result := '%scoulomb per %smeter' end;
class function TCoulombPerMeterQty.PluralName: string; begin result := '%scoulombs per %smeter' end;
class function TCoulombPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCoulombPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i adim.inc}
class function TSquareNewtonQty.Symbol: string; begin result := '%sN2' end;
class function TSquareNewtonQty.SingularName: string; begin result := 'square %snewton' end;
class function TSquareNewtonQty.PluralName: string; begin result := 'square %snewtons' end;
class function TSquareNewtonQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareNewtonQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i adim.inc}
class function TKilogramQty.Symbol: string; begin result := '%sg' end;
class function TKilogramQty.SingularName: string; begin result := '%sgram' end;
class function TKilogramQty.PluralName: string; begin result := '%sgrams' end;
class function TKilogramQty.Prefixes: TPrefixes; begin result := [pKilo]; end;
class function TKilogramQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i adim.inc}
class function TOhmQty.Symbol: string; begin result := '%sΩ' end;
class function TOhmQty.SingularName: string; begin result := '%sohm' end;
class function TOhmQty.PluralName: string; begin result := '%sohms' end;
class function TOhmQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TOhmQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i adim.inc}
class function TKilogramSquareMeterQty.Symbol: string; begin result := '%sg·%sm2' end;
class function TKilogramSquareMeterQty.SingularName: string; begin result := '%sgram square %smeter' end;
class function TKilogramSquareMeterQty.PluralName: string; begin result := '%sgram square %smeters' end;
class function TKilogramSquareMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramSquareMeterQty.Exponents: TExponents; begin result := [1, 2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$i adim.inc}
class function TWattPerSquareMeterPerKelvinQty.Symbol: string; begin result := '%sW/%sm2/%sK' end;
class function TWattPerSquareMeterPerKelvinQty.SingularName: string; begin result := '%swatt per square %smeter per %skelvin' end;
class function TWattPerSquareMeterPerKelvinQty.PluralName: string; begin result := '%swatts per square %smeter per %skelvin' end;
class function TWattPerSquareMeterPerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TWattPerSquareMeterPerKelvinQty.Exponents: TExponents; begin result := [1, -2, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i adim.inc}
class function TLumenQty.Symbol: string; begin result := '%slm' end;
class function TLumenQty.SingularName: string; begin result := '%slumen' end;
class function TLumenQty.PluralName: string; begin result := '%slumens' end;
class function TLumenQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TLumenQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i adim.inc}
class function TNewtonPerSquareKilogramQty.Symbol: string; begin result := '%sN/%sg2' end;
class function TNewtonPerSquareKilogramQty.SingularName: string; begin result := '%snewton per square %sgram' end;
class function TNewtonPerSquareKilogramQty.PluralName: string; begin result := '%snewtons per square %sgram' end;
class function TNewtonPerSquareKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pKilo]; end;
class function TNewtonPerSquareKilogramQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i adim.inc}
class function TSquareMeterQuarticKelvinQty.Symbol: string; begin result := '%sm2·%sK4' end;
class function TSquareMeterQuarticKelvinQty.SingularName: string; begin result := 'square %smeter quartic %skelvin' end;
class function TSquareMeterQuarticKelvinQty.PluralName: string; begin result := 'square %smeter quartic %skelvins' end;
class function TSquareMeterQuarticKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareMeterQuarticKelvinQty.Exponents: TExponents; begin result := [2, 4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i adim.inc}
class function TKilogramPerCubicMeterQty.Symbol: string; begin result := '%sg/%sm3' end;
class function TKilogramPerCubicMeterQty.SingularName: string; begin result := '%sgram per cubic %smeter' end;
class function TKilogramPerCubicMeterQty.PluralName: string; begin result := '%sgrams per cubic %smeter' end;
class function TKilogramPerCubicMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramPerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$i adim.inc}
class function TWattPerSquareMeterPerSteradianQty.Symbol: string; begin result := '%sW/%sm2/sr' end;
class function TWattPerSquareMeterPerSteradianQty.SingularName: string; begin result := '%swatt per square %smeter per steradian' end;
class function TWattPerSquareMeterPerSteradianQty.PluralName: string; begin result := '%swatts per square %smeter per steradian' end;
class function TWattPerSquareMeterPerSteradianQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TWattPerSquareMeterPerSteradianQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i adim.inc}
class function TKilogramPerSquareMeterQty.Symbol: string; begin result := '%sg/%sm2' end;
class function TKilogramPerSquareMeterQty.SingularName: string; begin result := '%sgram per square %smeter' end;
class function TKilogramPerSquareMeterQty.PluralName: string; begin result := '%sgrams per square %smeter' end;
class function TKilogramPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramPerSquareMeterQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i adim.inc}
class function TKilogramKelvinQty.Symbol: string; begin result := '%sg·%sK' end;
class function TKilogramKelvinQty.SingularName: string; begin result := '%sgram %skelvin' end;
class function TKilogramKelvinQty.PluralName: string; begin result := '%sgram %skelvins' end;
class function TKilogramKelvinQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramKelvinQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i adim.inc}
class function TMoleQty.Symbol: string; begin result := '%smol' end;
class function TMoleQty.SingularName: string; begin result := '%smole' end;
class function TMoleQty.PluralName: string; begin result := '%smoles' end;
class function TMoleQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TMoleQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i adim.inc}
class function TJoulePerKelvinQty.Symbol: string; begin result := '%sJ/%sK' end;
class function TJoulePerKelvinQty.SingularName: string; begin result := '%sjoule per %skelvin' end;
class function TJoulePerKelvinQty.PluralName: string; begin result := '%sjoules per %skelvin' end;
class function TJoulePerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TJoulePerKelvinQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i adim.inc}
class function TCoulombPerKilogramQty.Symbol: string; begin result := '%sC/%sg' end;
class function TCoulombPerKilogramQty.SingularName: string; begin result := '%scoulomb per %sgram' end;
class function TCoulombPerKilogramQty.PluralName: string; begin result := '%scoulombs per %sgram' end;
class function TCoulombPerKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pKilo]; end;
class function TCoulombPerKilogramQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i adim.inc}
class function TCubicMeterQty.Symbol: string; begin result := '%sm3' end;
class function TCubicMeterQty.SingularName: string; begin result := 'cubic %smeter' end;
class function TCubicMeterQty.PluralName: string; begin result := 'cubic %smeters' end;
class function TCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TCubicMeterQty.Exponents: TExponents; begin result := [3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i adim.inc}
class function TCoulombPerSquareMeterQty.Symbol: string; begin result := '%sC/%sm2' end;
class function TCoulombPerSquareMeterQty.SingularName: string; begin result := '%scoulomb per square %smeter' end;
class function TCoulombPerSquareMeterQty.PluralName: string; begin result := '%scoulombs per square %smeter' end;
class function TCoulombPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCoulombPerSquareMeterQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i adim.inc}
class function TNewtonPerSquareCoulombQty.Symbol: string; begin result := '%sN/%sC2' end;
class function TNewtonPerSquareCoulombQty.SingularName: string; begin result := '%snewton per square %scoulomb' end;
class function TNewtonPerSquareCoulombQty.PluralName: string; begin result := '%snewtons per square %scoulomb' end;
class function TNewtonPerSquareCoulombQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerSquareCoulombQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i adim.inc}
class function TCubicMeterPerSecondQty.Symbol: string; begin result := '%sm3/%ss' end;
class function TCubicMeterPerSecondQty.SingularName: string; begin result := 'cubic %smeter per %ssecond' end;
class function TCubicMeterPerSecondQty.PluralName: string; begin result := 'cubic %smeters per %ssecond' end;
class function TCubicMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCubicMeterPerSecondQty.Exponents: TExponents; begin result := [3, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i adim.inc}
class function TMeterPerSecondQty.Symbol: string; begin result := '%sm/%ss' end;
class function TMeterPerSecondQty.SingularName: string; begin result := '%smeter per %ssecond' end;
class function TMeterPerSecondQty.PluralName: string; begin result := '%smeters per %ssecond' end;
class function TMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMeterPerSecondQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i adim.inc}
class function THenryQty.Symbol: string; begin result := '%sH' end;
class function THenryQty.SingularName: string; begin result := '%shenry' end;
class function THenryQty.PluralName: string; begin result := '%shenries' end;
class function THenryQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function THenryQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i adim.inc}
class function TMolePerCubicMeterQty.Symbol: string; begin result := '%smol/%sm3' end;
class function TMolePerCubicMeterQty.SingularName: string; begin result := '%smole per cubic %smeter' end;
class function TMolePerCubicMeterQty.PluralName: string; begin result := '%smoles per cubic %smeter' end;
class function TMolePerCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMolePerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i adim.inc}
class function TSquareMeterKelvinQty.Symbol: string; begin result := '%sm2·%sK' end;
class function TSquareMeterKelvinQty.SingularName: string; begin result := 'square %smeter %skelvin' end;
class function TSquareMeterKelvinQty.PluralName: string; begin result := 'square %smeter %skelvins' end;
class function TSquareMeterKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareMeterKelvinQty.Exponents: TExponents; begin result := [2, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i adim.inc}
class function TSexticMeterQty.Symbol: string; begin result := '%sm6' end;
class function TSexticMeterQty.SingularName: string; begin result := 'sextic %smeter' end;
class function TSexticMeterQty.PluralName: string; begin result := 'sextic %smeters' end;
class function TSexticMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSexticMeterQty.Exponents: TExponents; begin result := [6]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i adim.inc}
class function TNewtonSquareMeterPerSquareKilogramQty.Symbol: string; begin result := '%sN·%sm2/%sg2' end;
class function TNewtonSquareMeterPerSquareKilogramQty.SingularName: string; begin result := '%snewton square %smeter per square %sgram' end;
class function TNewtonSquareMeterPerSquareKilogramQty.PluralName: string; begin result := '%snewton square %smeters per square %sgram' end;
class function TNewtonSquareMeterPerSquareKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pKilo]; end;
class function TNewtonSquareMeterPerSquareKilogramQty.Exponents: TExponents; begin result := [1, 2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i adim.inc}
class function TSquareKilogramPerSquareMeterQty.Symbol: string; begin result := '%sg2/%sm2' end;
class function TSquareKilogramPerSquareMeterQty.SingularName: string; begin result := 'square %sgram per square %smeter' end;
class function TSquareKilogramPerSquareMeterQty.PluralName: string; begin result := 'square %sgrams per square %smeter' end;
class function TSquareKilogramPerSquareMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TSquareKilogramPerSquareMeterQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i adim.inc}
class function TQuarticMeterQty.Symbol: string; begin result := '%sm4' end;
class function TQuarticMeterQty.SingularName: string; begin result := 'quartic %smeter' end;
class function TQuarticMeterQty.PluralName: string; begin result := 'quartic %smeters' end;
class function TQuarticMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TQuarticMeterQty.Exponents: TExponents; begin result := [4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i adim.inc}
class function TKilogramMeterPerSecondQty.Symbol: string; begin result := '%sg·%sm/%ss' end;
class function TKilogramMeterPerSecondQty.SingularName: string; begin result := '%sgram %smeter per %ssecond' end;
class function TKilogramMeterPerSecondQty.PluralName: string; begin result := '%sgram %smeters per %ssecond' end;
class function TKilogramMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone, pNone]; end;
class function TKilogramMeterPerSecondQty.Exponents: TExponents; begin result := [1, 1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i adim.inc}
class function TSquareJouleQty.Symbol: string; begin result := '%sJ2' end;
class function TSquareJouleQty.SingularName: string; begin result := 'square %sjoule' end;
class function TSquareJouleQty.PluralName: string; begin result := 'square %sjoules' end;
class function TSquareJouleQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareJouleQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i adim.inc}
class function TCoulombPerCubicMeterQty.Symbol: string; begin result := '%sC/%sm3' end;
class function TCoulombPerCubicMeterQty.SingularName: string; begin result := '%scoulomb per cubic %smeter' end;
class function TCoulombPerCubicMeterQty.PluralName: string; begin result := '%scoulombs per cubic %smeter' end;
class function TCoulombPerCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TCoulombPerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i adim.inc}
class function TPoiseuilleQty.Symbol: string; begin result := '%sPl' end;
class function TPoiseuilleQty.SingularName: string; begin result := '%spoiseuille' end;
class function TPoiseuilleQty.PluralName: string; begin result := '%spoiseuilles' end;
class function TPoiseuilleQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TPoiseuilleQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$i adim.inc}
class function TWattPerSquareMeterPerQuarticKelvinQty.Symbol: string; begin result := '%sW/%sm2/%sK4' end;
class function TWattPerSquareMeterPerQuarticKelvinQty.SingularName: string; begin result := '%swatt per square %smeter per quartic %skelvin' end;
class function TWattPerSquareMeterPerQuarticKelvinQty.PluralName: string; begin result := '%swatts per square %smeter per quartic %skelvin' end;
class function TWattPerSquareMeterPerQuarticKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TWattPerSquareMeterPerQuarticKelvinQty.Exponents: TExponents; begin result := [1, -2, -4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i adim.inc}
class function TKilogramPerQuarticMeterPerSecondQty.Symbol: string; begin result := '%sg/%sm4/%ss' end;
class function TKilogramPerQuarticMeterPerSecondQty.SingularName: string; begin result := '%sgram per quartic %smeter per %ssecond' end;
class function TKilogramPerQuarticMeterPerSecondQty.PluralName: string; begin result := '%sgrams per quartic %smeter per %ssecond' end;
class function TKilogramPerQuarticMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pKilo, pNone, pNone]; end;
class function TKilogramPerQuarticMeterPerSecondQty.Exponents: TExponents; begin result := [1, -4, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i adim.inc}
class function TKilogramPerQuarticMeterQty.Symbol: string; begin result := '%sg/%sm4' end;
class function TKilogramPerQuarticMeterQty.SingularName: string; begin result := '%sgram per quartic %smeter' end;
class function TKilogramPerQuarticMeterQty.PluralName: string; begin result := '%sgrams per quartic %smeter' end;
class function TKilogramPerQuarticMeterQty.Prefixes: TPrefixes; begin result := [pKilo, pNone]; end;
class function TKilogramPerQuarticMeterQty.Exponents: TExponents; begin result := [1, -4]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i adim.inc}
class function TWattPerMeterPerKelvinQty.Symbol: string; begin result := '%sW/%sm/%sK' end;
class function TWattPerMeterPerKelvinQty.SingularName: string; begin result := '%swatt per %smeter per %skelvin' end;
class function TWattPerMeterPerKelvinQty.PluralName: string; begin result := '%swatts per %smeter per %skelvin' end;
class function TWattPerMeterPerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TWattPerMeterPerKelvinQty.Exponents: TExponents; begin result := [1, -1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i adim.inc}
class function TMeterQty.Symbol: string; begin result := '%sm' end;
class function TMeterQty.SingularName: string; begin result := '%smeter' end;
class function TMeterQty.PluralName: string; begin result := '%smeters' end;
class function TMeterQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TMeterQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i adim.inc}
class function TKelvinPerMeterQty.Symbol: string; begin result := '%sK/%sm' end;
class function TKelvinPerMeterQty.SingularName: string; begin result := '%skelvin per %smeter' end;
class function TKelvinPerMeterQty.PluralName: string; begin result := '%skelvins per %smeter' end;
class function TKelvinPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TKelvinPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i adim.inc}
class function TNewtonSquareMeterQty.Symbol: string; begin result := '%sN·%sm2' end;
class function TNewtonSquareMeterQty.SingularName: string; begin result := '%snewton square %smeter' end;
class function TNewtonSquareMeterQty.PluralName: string; begin result := '%snewton square %smeters' end;
class function TNewtonSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonSquareMeterQty.Exponents: TExponents; begin result := [1, 2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i adim.inc}
class function TSecondQty.Symbol: string; begin result := '%ss' end;
class function TSecondQty.SingularName: string; begin result := '%ssecond' end;
class function TSecondQty.PluralName: string; begin result := '%sseconds' end;
class function TSecondQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSecondQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i adim.inc}
class function TSquareMeterPerSquareCoulombQty.Symbol: string; begin result := '%sm2/%sC2' end;
class function TSquareMeterPerSquareCoulombQty.SingularName: string; begin result := 'square %smeter per square %scoulomb' end;
class function TSquareMeterPerSquareCoulombQty.PluralName: string; begin result := 'square %smeters per square %scoulomb' end;
class function TSquareMeterPerSquareCoulombQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TSquareMeterPerSquareCoulombQty.Exponents: TExponents; begin result := [2, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i adim.inc}
class function TSquareCoulombQty.Symbol: string; begin result := '%sC2' end;
class function TSquareCoulombQty.SingularName: string; begin result := 'square %scoulomb' end;
class function TSquareCoulombQty.PluralName: string; begin result := 'square %scoulombs' end;
class function TSquareCoulombQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareCoulombQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i adim.inc}
class function TSiemensQty.Symbol: string; begin result := '%sS' end;
class function TSiemensQty.SingularName: string; begin result := '%ssiemens' end;
class function TSiemensQty.PluralName: string; begin result := '%ssiemens' end;
class function TSiemensQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSiemensQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$i adim.inc}
class function TJoulePerMolePerKelvinQty.Symbol: string; begin result := '%sJ/%smol/%sK' end;
class function TJoulePerMolePerKelvinQty.SingularName: string; begin result := '%sjoule per %smole per %skelvin' end;
class function TJoulePerMolePerKelvinQty.PluralName: string; begin result := '%sjoules per %smole per %skelvin' end;
class function TJoulePerMolePerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TJoulePerMolePerKelvinQty.Exponents: TExponents; begin result := [1, -1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i adim.inc}
class function TSquareMeterSteradianQty.Symbol: string; begin result := '%sm2·sr' end;
class function TSquareMeterSteradianQty.SingularName: string; begin result := 'square %smeter steradian' end;
class function TSquareMeterSteradianQty.PluralName: string; begin result := 'square %smeter steradians' end;
class function TSquareMeterSteradianQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareMeterSteradianQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i adim.inc}
class function TMeterPerAmpereQty.Symbol: string; begin result := '%sm/%sA' end;
class function TMeterPerAmpereQty.SingularName: string; begin result := '%smeter per %sampere' end;
class function TMeterPerAmpereQty.PluralName: string; begin result := '%smeters per %sampere' end;
class function TMeterPerAmpereQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMeterPerAmpereQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$i adim.inc}
class function TMeterPerSecondSquaredQty.Symbol: string; begin result := '%sm/%ss2' end;
class function TMeterPerSecondSquaredQty.SingularName: string; begin result := '%smeter per %ssecond squared' end;
class function TMeterPerSecondSquaredQty.PluralName: string; begin result := '%smeters per %ssecond squared' end;
class function TMeterPerSecondSquaredQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMeterPerSecondSquaredQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i adim.inc}
class function TFaradQty.Symbol: string; begin result := '%sF' end;
class function TFaradQty.SingularName: string; begin result := '%sfarad' end;
class function TFaradQty.PluralName: string; begin result := '%sfarads' end;
class function TFaradQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TFaradQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i adim.inc}
class function TAmperePerSquareMeterQty.Symbol: string; begin result := '%sA/%sm2' end;
class function TAmperePerSquareMeterQty.SingularName: string; begin result := '%sampere per square %smeter' end;
class function TAmperePerSquareMeterQty.PluralName: string; begin result := '%samperes per square %smeter' end;
class function TAmperePerSquareMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TAmperePerSquareMeterQty.Exponents: TExponents; begin result := [1, -2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i adim.inc}
class function TFaradPerMeterQty.Symbol: string; begin result := '%sF/%sm' end;
class function TFaradPerMeterQty.SingularName: string; begin result := '%sfarad per %smeter' end;
class function TFaradPerMeterQty.PluralName: string; begin result := '%sfarads per %smeter' end;
class function TFaradPerMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TFaradPerMeterQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i adim.inc}
class function TVoltMeterQty.Symbol: string; begin result := '%sV·%sm' end;
class function TVoltMeterQty.SingularName: string; begin result := '%svolt %smeter' end;
class function TVoltMeterQty.PluralName: string; begin result := '%svolt %smeters' end;
class function TVoltMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TVoltMeterQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i adim.inc}
class function TSquareHertzQty.Symbol: string; begin result := '%sHz2' end;
class function TSquareHertzQty.SingularName: string; begin result := 'square %shertz' end;
class function TSquareHertzQty.PluralName: string; begin result := 'square %shertz' end;
class function TSquareHertzQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareHertzQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i adim.inc}
class function TGrayPerSecondQty.Symbol: string; begin result := '%sGy/%ss' end;
class function TGrayPerSecondQty.SingularName: string; begin result := '%sgray per %ssecond' end;
class function TGrayPerSecondQty.PluralName: string; begin result := '%sgrays per %ssecond' end;
class function TGrayPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TGrayPerSecondQty.Exponents: TExponents; begin result := [1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$i adim.inc}
class function TJoulePerKilogramPerKelvinQty.Symbol: string; begin result := '%sJ/%sg/%sK' end;
class function TJoulePerKilogramPerKelvinQty.SingularName: string; begin result := '%sjoule per %sgram per %skelvin' end;
class function TJoulePerKilogramPerKelvinQty.PluralName: string; begin result := '%sjoules per %sgram per %skelvin' end;
class function TJoulePerKilogramPerKelvinQty.Prefixes: TPrefixes; begin result := [pNone, pKilo, pNone]; end;
class function TJoulePerKilogramPerKelvinQty.Exponents: TExponents; begin result := [1, -1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i adim.inc}
class function TSquareSecondQty.Symbol: string; begin result := '%ss2' end;
class function TSquareSecondQty.SingularName: string; begin result := 'square %ssecond' end;
class function TSquareSecondQty.PluralName: string; begin result := 'square %sseconds' end;
class function TSquareSecondQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSquareSecondQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i adim.inc}
class function TCubicKelvinQty.Symbol: string; begin result := '%sK3' end;
class function TCubicKelvinQty.SingularName: string; begin result := 'cubic %skelvin' end;
class function TCubicKelvinQty.PluralName: string; begin result := 'cubic %skelvins' end;
class function TCubicKelvinQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TCubicKelvinQty.Exponents: TExponents; begin result := [3]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i adim.inc}
class function TLuxQty.Symbol: string; begin result := '%slx' end;
class function TLuxQty.SingularName: string; begin result := '%slux' end;
class function TLuxQty.PluralName: string; begin result := '%slux' end;
class function TLuxQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TLuxQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i adim.inc}
class function TVoltMeterPerSecondQty.Symbol: string; begin result := '%sV·%sm/%ss' end;
class function TVoltMeterPerSecondQty.SingularName: string; begin result := '%svolt %smeter per %ssecond' end;
class function TVoltMeterPerSecondQty.PluralName: string; begin result := '%svolt %smeters per %ssecond' end;
class function TVoltMeterPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TVoltMeterPerSecondQty.Exponents: TExponents; begin result := [1, 1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i adim.inc}
class function TSquareKilogramQty.Symbol: string; begin result := '%sg2' end;
class function TSquareKilogramQty.SingularName: string; begin result := 'square %sgram' end;
class function TSquareKilogramQty.PluralName: string; begin result := 'square %sgrams' end;
class function TSquareKilogramQty.Prefixes: TPrefixes; begin result := [pKilo]; end;
class function TSquareKilogramQty.Exponents: TExponents; begin result := [2]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TDayQty}{$i adim.inc}
class function TDayQty.Symbol: string; begin result := 'd' end;
class function TDayQty.SingularName: string; begin result := 'day' end;
class function TDayQty.PluralName: string; begin result := 'days' end;
class function TDayQty.Prefixes: TPrefixes; begin result := []; end;
class function TDayQty.Exponents: TExponents; begin result := []; end;
class function TDayQty.ToBaseFactor: double; begin result := 86400; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=THourQty}{$i adim.inc}
class function THourQty.Symbol: string; begin result := 'h' end;
class function THourQty.SingularName: string; begin result := 'hour' end;
class function THourQty.PluralName: string; begin result := 'hours' end;
class function THourQty.Prefixes: TPrefixes; begin result := []; end;
class function THourQty.Exponents: TExponents; begin result := []; end;
class function THourQty.ToBaseFactor: double; begin result := 3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMinuteQty}{$i adim.inc}
class function TMinuteQty.Symbol: string; begin result := 'min' end;
class function TMinuteQty.SingularName: string; begin result := 'minute' end;
class function TMinuteQty.PluralName: string; begin result := 'minutes' end;
class function TMinuteQty.Prefixes: TPrefixes; begin result := []; end;
class function TMinuteQty.Exponents: TExponents; begin result := []; end;
class function TMinuteQty.ToBaseFactor: double; begin result := 60; end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareDayQty}{$i adim.inc}
class function TSquareDayQty.Symbol: string; begin result := 'd2' end;
class function TSquareDayQty.SingularName: string; begin result := 'square day' end;
class function TSquareDayQty.PluralName: string; begin result := 'square days' end;
class function TSquareDayQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareDayQty.Exponents: TExponents; begin result := []; end;
class function TSquareDayQty.ToBaseFactor: double; begin result := 7464960000; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareHourQty}{$i adim.inc}
class function TSquareHourQty.Symbol: string; begin result := 'h2' end;
class function TSquareHourQty.SingularName: string; begin result := 'square hour' end;
class function TSquareHourQty.PluralName: string; begin result := 'square hours' end;
class function TSquareHourQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareHourQty.Exponents: TExponents; begin result := []; end;
class function TSquareHourQty.ToBaseFactor: double; begin result := 12960000; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareMinuteQty}{$i adim.inc}
class function TSquareMinuteQty.Symbol: string; begin result := 'min2' end;
class function TSquareMinuteQty.SingularName: string; begin result := 'square minute' end;
class function TSquareMinuteQty.PluralName: string; begin result := 'square minutes' end;
class function TSquareMinuteQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareMinuteQty.Exponents: TExponents; begin result := []; end;
class function TSquareMinuteQty.ToBaseFactor: double; begin result := 3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TAstronomicalQty}{$i adim.inc}
class function TAstronomicalQty.Symbol: string; begin result := 'au' end;
class function TAstronomicalQty.SingularName: string; begin result := 'astronomical unit' end;
class function TAstronomicalQty.PluralName: string; begin result := 'astronomical units' end;
class function TAstronomicalQty.Prefixes: TPrefixes; begin result := []; end;
class function TAstronomicalQty.Exponents: TExponents; begin result := []; end;
class function TAstronomicalQty.ToBaseFactor: double; begin result := 149597870691; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TInchQty}{$i adim.inc}
class function TInchQty.Symbol: string; begin result := 'in' end;
class function TInchQty.SingularName: string; begin result := 'inch' end;
class function TInchQty.PluralName: string; begin result := 'inches' end;
class function TInchQty.Prefixes: TPrefixes; begin result := []; end;
class function TInchQty.Exponents: TExponents; begin result := []; end;
class function TInchQty.ToBaseFactor: double; begin result := 0.0254; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TFootQty}{$i adim.inc}
class function TFootQty.Symbol: string; begin result := 'ft' end;
class function TFootQty.SingularName: string; begin result := 'foot' end;
class function TFootQty.PluralName: string; begin result := 'feet' end;
class function TFootQty.Prefixes: TPrefixes; begin result := []; end;
class function TFootQty.Exponents: TExponents; begin result := []; end;
class function TFootQty.ToBaseFactor: double; begin result := 0.3048; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TYardQty}{$i adim.inc}
class function TYardQty.Symbol: string; begin result := 'yd' end;
class function TYardQty.SingularName: string; begin result := 'yard' end;
class function TYardQty.PluralName: string; begin result := 'yards' end;
class function TYardQty.Prefixes: TPrefixes; begin result := []; end;
class function TYardQty.Exponents: TExponents; begin result := []; end;
class function TYardQty.ToBaseFactor: double; begin result := 0.9144; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMileQty}{$i adim.inc}
class function TMileQty.Symbol: string; begin result := 'mi' end;
class function TMileQty.SingularName: string; begin result := 'mile' end;
class function TMileQty.PluralName: string; begin result := 'miles' end;
class function TMileQty.Prefixes: TPrefixes; begin result := []; end;
class function TMileQty.Exponents: TExponents; begin result := []; end;
class function TMileQty.ToBaseFactor: double; begin result := 1609.344; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNauticalMileQty}{$i adim.inc}
class function TNauticalMileQty.Symbol: string; begin result := 'nmi' end;
class function TNauticalMileQty.SingularName: string; begin result := 'nautical mile' end;
class function TNauticalMileQty.PluralName: string; begin result := 'nautical miles' end;
class function TNauticalMileQty.Prefixes: TPrefixes; begin result := []; end;
class function TNauticalMileQty.Exponents: TExponents; begin result := []; end;
class function TNauticalMileQty.ToBaseFactor: double; begin result := 1852; end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareInchQty}{$i adim.inc}
class function TSquareInchQty.Symbol: string; begin result := 'in2' end;
class function TSquareInchQty.SingularName: string; begin result := 'square inch' end;
class function TSquareInchQty.PluralName: string; begin result := 'square inches' end;
class function TSquareInchQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareInchQty.Exponents: TExponents; begin result := []; end;
class function TSquareInchQty.ToBaseFactor: double; begin result := 0.00064516; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareFootQty}{$i adim.inc}
class function TSquareFootQty.Symbol: string; begin result := 'ft2' end;
class function TSquareFootQty.SingularName: string; begin result := 'square foot' end;
class function TSquareFootQty.PluralName: string; begin result := 'square feet' end;
class function TSquareFootQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareFootQty.Exponents: TExponents; begin result := []; end;
class function TSquareFootQty.ToBaseFactor: double; begin result := 0.09290304; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareYardQty}{$i adim.inc}
class function TSquareYardQty.Symbol: string; begin result := 'yd2' end;
class function TSquareYardQty.SingularName: string; begin result := 'square yard' end;
class function TSquareYardQty.PluralName: string; begin result := 'square yards' end;
class function TSquareYardQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareYardQty.Exponents: TExponents; begin result := []; end;
class function TSquareYardQty.ToBaseFactor: double; begin result := 0.83612736; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareMileQty}{$i adim.inc}
class function TSquareMileQty.Symbol: string; begin result := 'mi2' end;
class function TSquareMileQty.SingularName: string; begin result := 'square mile' end;
class function TSquareMileQty.PluralName: string; begin result := 'square miles' end;
class function TSquareMileQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareMileQty.Exponents: TExponents; begin result := []; end;
class function TSquareMileQty.ToBaseFactor: double; begin result := 2589988.110336; end;

class operator TMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicInchQty}{$i adim.inc}
class function TCubicInchQty.Symbol: string; begin result := 'in3' end;
class function TCubicInchQty.SingularName: string; begin result := 'cubic inch' end;
class function TCubicInchQty.PluralName: string; begin result := 'cubic inches' end;
class function TCubicInchQty.Prefixes: TPrefixes; begin result := []; end;
class function TCubicInchQty.Exponents: TExponents; begin result := []; end;
class function TCubicInchQty.ToBaseFactor: double; begin result := 0.000016387064; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicFootQty}{$i adim.inc}
class function TCubicFootQty.Symbol: string; begin result := 'ft3' end;
class function TCubicFootQty.SingularName: string; begin result := 'cubic foot' end;
class function TCubicFootQty.PluralName: string; begin result := 'cubic feet' end;
class function TCubicFootQty.Prefixes: TPrefixes; begin result := []; end;
class function TCubicFootQty.Exponents: TExponents; begin result := []; end;
class function TCubicFootQty.ToBaseFactor: double; begin result := 0.028316846592; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TCubicYardQty}{$i adim.inc}
class function TCubicYardQty.Symbol: string; begin result := 'yd3' end;
class function TCubicYardQty.SingularName: string; begin result := 'cubic yard' end;
class function TCubicYardQty.PluralName: string; begin result := 'cubic yards' end;
class function TCubicYardQty.Prefixes: TPrefixes; begin result := []; end;
class function TCubicYardQty.Exponents: TExponents; begin result := []; end;
class function TCubicYardQty.ToBaseFactor: double; begin result := 0.764554857984; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TLitreQty}{$i adim.inc}
class function TLitreQty.Symbol: string; begin result := '%sL' end;
class function TLitreQty.SingularName: string; begin result := '%slitre' end;
class function TLitreQty.PluralName: string; begin result := '%slitres' end;
class function TLitreQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TLitreQty.Exponents: TExponents; begin result := [1]; end;
class function TLitreQty.ToBaseFactor: double; begin result := 1E-03; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TGallonQty}{$i adim.inc}
class function TGallonQty.Symbol: string; begin result := 'gal' end;
class function TGallonQty.SingularName: string; begin result := 'gallon' end;
class function TGallonQty.PluralName: string; begin result := 'gallons' end;
class function TGallonQty.Prefixes: TPrefixes; begin result := []; end;
class function TGallonQty.Exponents: TExponents; begin result := []; end;
class function TGallonQty.ToBaseFactor: double; begin result := 0.0037854119678; end;

class operator TMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TCubicMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TQuarticMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCubicMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TQuinticMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSexticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TQuarticMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TSexticMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterQty./(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TTonneQty}{$i adim.inc}
class function TTonneQty.Symbol: string; begin result := '%st' end;
class function TTonneQty.SingularName: string; begin result := '%stonne' end;
class function TTonneQty.PluralName: string; begin result := '%stonnes' end;
class function TTonneQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TTonneQty.Exponents: TExponents; begin result := [1]; end;
class function TTonneQty.ToBaseFactor: double; begin result := 1E+03; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundQty}{$i adim.inc}
class function TPoundQty.Symbol: string; begin result := 'lb' end;
class function TPoundQty.SingularName: string; begin result := 'pound' end;
class function TPoundQty.PluralName: string; begin result := 'pounds' end;
class function TPoundQty.Prefixes: TPrefixes; begin result := []; end;
class function TPoundQty.Exponents: TExponents; begin result := []; end;
class function TPoundQty.ToBaseFactor: double; begin result := 0.45359237; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TOunceQty}{$i adim.inc}
class function TOunceQty.Symbol: string; begin result := 'oz' end;
class function TOunceQty.SingularName: string; begin result := 'ounce' end;
class function TOunceQty.PluralName: string; begin result := 'ounces' end;
class function TOunceQty.Prefixes: TPrefixes; begin result := []; end;
class function TOunceQty.Exponents: TExponents; begin result := []; end;
class function TOunceQty.ToBaseFactor: double; begin result := 0.028349523125; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TStoneQty}{$i adim.inc}
class function TStoneQty.Symbol: string; begin result := 'st' end;
class function TStoneQty.SingularName: string; begin result := 'stone' end;
class function TStoneQty.PluralName: string; begin result := 'stones' end;
class function TStoneQty.Prefixes: TPrefixes; begin result := []; end;
class function TStoneQty.Exponents: TExponents; begin result := []; end;
class function TStoneQty.ToBaseFactor: double; begin result := 6.35029318; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TTonQty}{$i adim.inc}
class function TTonQty.Symbol: string; begin result := 'ton' end;
class function TTonQty.SingularName: string; begin result := 'ton' end;
class function TTonQty.PluralName: string; begin result := 'tons' end;
class function TTonQty.Prefixes: TPrefixes; begin result := []; end;
class function TTonQty.Exponents: TExponents; begin result := []; end;
class function TTonQty.ToBaseFactor: double; begin result := 907.18474; end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i adim.inc}
class function TDegreeCelsiusQty.Symbol: string; begin result := 'ºC' end;
class function TDegreeCelsiusQty.SingularName: string; begin result := 'degree Celsius' end;
class function TDegreeCelsiusQty.PluralName: string; begin result := 'degrees Celsius' end;
class function TDegreeCelsiusQty.Prefixes: TPrefixes; begin result := []; end;
class function TDegreeCelsiusQty.Exponents: TExponents; begin result := []; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i adim.inc}
class function TDegreeFahrenheitQty.Symbol: string; begin result := 'ºF' end;
class function TDegreeFahrenheitQty.SingularName: string; begin result := 'degree Fahrenheit' end;
class function TDegreeFahrenheitQty.PluralName: string; begin result := 'degrees Fahrenheit' end;
class function TDegreeFahrenheitQty.Prefixes: TPrefixes; begin result := []; end;
class function TDegreeFahrenheitQty.Exponents: TExponents; begin result := []; end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKelvinQty.*(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TDegreeQty}{$i adim.inc}
class function TDegreeQty.Symbol: string; begin result := 'deg' end;
class function TDegreeQty.SingularName: string; begin result := 'degree' end;
class function TDegreeQty.PluralName: string; begin result := 'degrees' end;
class function TDegreeQty.Prefixes: TPrefixes; begin result := []; end;
class function TDegreeQty.Exponents: TExponents; begin result := []; end;
class function TDegreeQty.ToBaseFactor: double; begin result := Pi/180; end;

class operator TRadianQty.*(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianQty./(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TSquareDegreeQty}{$i adim.inc}
class function TSquareDegreeQty.Symbol: string; begin result := 'deg2' end;
class function TSquareDegreeQty.SingularName: string; begin result := 'square degree' end;
class function TSquareDegreeQty.PluralName: string; begin result := 'square degrees' end;
class function TSquareDegreeQty.Prefixes: TPrefixes; begin result := []; end;
class function TSquareDegreeQty.Exponents: TExponents; begin result := []; end;
class function TSquareDegreeQty.ToBaseFactor: double; begin result := Pi*Pi/32400; end;

class operator TSecondQty./(const ALeft: double; const ARight: TSecondQty): THertzQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: THertzQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: THertzQty; const ARight: TSecondQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: THertzQty): TSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareHertzQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TSquareSecondQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianQty.*(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianQty.*(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondQty.:=(const AQuantity: TRadianPerSecondQty): THertzQty;
begin
  result.FValue := AQuantity.FValue;
end;

class operator TRadianPerSecondQty.:=(const AQuantity: THertzQty): TRadianPerSecondQty;
begin
  result.FValue := AQuantity.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$i adim.inc}
class function TRadianPerSecondSquaredQty.Symbol: string; begin result := 'rad/%ss2' end;
class function TRadianPerSecondSquaredQty.SingularName: string; begin result := 'radian per %ssecond squared' end;
class function TRadianPerSecondSquaredQty.PluralName: string; begin result := 'radians per %ssecond squared' end;
class function TRadianPerSecondSquaredQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TRadianPerSecondSquaredQty.Exponents: TExponents; begin result := [-2]; end;

operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSecondSquaredQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TSquareSecondQty; const ARight: TRadianPerSecondSquaredQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty.*(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianQty; const ARight: TRadianPerSecondSquaredQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondSquaredQty; const ARight: TRadianQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondSquaredQty./(const ALeft: TRadianPerSecondSquaredQty; const ARight: TSquareHertzQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i adim.inc}
class function TSteradianPerSquareSecondQty.Symbol: string; begin result := 'rad2/%ss2' end;
class function TSteradianPerSquareSecondQty.SingularName: string; begin result := 'square radian per square %ssecond' end;
class function TSteradianPerSquareSecondQty.PluralName: string; begin result := 'square radians per square %ssecond' end;
class function TSteradianPerSquareSecondQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSteradianPerSquareSecondQty.Exponents: TExponents; begin result := [-2]; end;

operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSteradianQty; const ARight: TSquareHertzQty): TSteradianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertzQty; const ARight: TSteradianQty): TSteradianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMeterPerHourQty}{$i adim.inc}
class function TMeterPerHourQty.Symbol: string; begin result := '%sm/h' end;
class function TMeterPerHourQty.SingularName: string; begin result := '%smeter per hour' end;
class function TMeterPerHourQty.PluralName: string; begin result := '%smeters per hour' end;
class function TMeterPerHourQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TMeterPerHourQty.Exponents: TExponents; begin result := [1]; end;
class function TMeterPerHourQty.ToBaseFactor: double; begin result := 1/3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMilePerHourQty}{$i adim.inc}
class function TMilePerHourQty.Symbol: string; begin result := 'mi/h' end;
class function TMilePerHourQty.SingularName: string; begin result := 'mile per hour' end;
class function TMilePerHourQty.PluralName: string; begin result := 'miles per hour' end;
class function TMilePerHourQty.Prefixes: TPrefixes; begin result := []; end;
class function TMilePerHourQty.Exponents: TExponents; begin result := []; end;
class function TMilePerHourQty.ToBaseFactor: double; begin result := 0.44704; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i adim.inc}
class function TNauticalMilePerHourQty.Symbol: string; begin result := 'nmi/h' end;
class function TNauticalMilePerHourQty.SingularName: string; begin result := 'nautical mile per hour' end;
class function TNauticalMilePerHourQty.PluralName: string; begin result := 'nautical miles per hour' end;
class function TNauticalMilePerHourQty.Prefixes: TPrefixes; begin result := []; end;
class function TNauticalMilePerHourQty.Exponents: TExponents; begin result := []; end;
class function TNauticalMilePerHourQty.ToBaseFactor: double; begin result := 463/900; end;

operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondSquaredQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareHertzQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i adim.inc}
class function TMeterPerSecondPerSecondQty.Symbol: string; begin result := '%sm/%ss/%ss' end;
class function TMeterPerSecondPerSecondQty.SingularName: string; begin result := '%smeter per %ssecond per %ssecond' end;
class function TMeterPerSecondPerSecondQty.PluralName: string; begin result := '%smeters per %ssecond per %ssecond' end;
class function TMeterPerSecondPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TMeterPerSecondPerSecondQty.Exponents: TExponents; begin result := [1, -1, -1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i adim.inc}
class function TMeterPerHourPerSecondQty.Symbol: string; begin result := '%sm/h/%ss' end;
class function TMeterPerHourPerSecondQty.SingularName: string; begin result := '%smeter per hour per %ssecond' end;
class function TMeterPerHourPerSecondQty.PluralName: string; begin result := '%smeters per hour per %ssecond' end;
class function TMeterPerHourPerSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TMeterPerHourPerSecondQty.Exponents: TExponents; begin result := [1, -1]; end;
class function TMeterPerHourPerSecondQty.ToBaseFactor: double; begin result := 1/3600; end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i adim.inc}
class function TNewtonSecondQty.Symbol: string; begin result := '%sN·%ss' end;
class function TNewtonSecondQty.SingularName: string; begin result := '%snewton %ssecond' end;
class function TNewtonSecondQty.PluralName: string; begin result := '%snewton %sseconds' end;
class function TNewtonSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonSecondQty.Exponents: TExponents; begin result := [1, 1]; end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramSquareMeterQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TNewtonQty; const ARight: TMeterPerSecondSquaredQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForceQty}{$i adim.inc}
class function TPoundForceQty.Symbol: string; begin result := 'lbf' end;
class function TPoundForceQty.SingularName: string; begin result := 'pound-force' end;
class function TPoundForceQty.PluralName: string; begin result := 'pounds-force' end;
class function TPoundForceQty.Prefixes: TPrefixes; begin result := []; end;
class function TPoundForceQty.Exponents: TExponents; begin result := []; end;
class function TPoundForceQty.ToBaseFactor: double; begin result := 4.4482216152605; end;

class operator TNewtonQty.*(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonQty./(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TBarQty}{$i adim.inc}
class function TBarQty.Symbol: string; begin result := '%sbar' end;
class function TBarQty.SingularName: string; begin result := '%sbar' end;
class function TBarQty.PluralName: string; begin result := '%sbars' end;
class function TBarQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TBarQty.Exponents: TExponents; begin result := [1]; end;
class function TBarQty.ToBaseFactor: double; begin result := 1E+05; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i adim.inc}
class function TPoundPerSquareInchQty.Symbol: string; begin result := '%spsi' end;
class function TPoundPerSquareInchQty.SingularName: string; begin result := '%spound per square inch' end;
class function TPoundPerSquareInchQty.PluralName: string; begin result := '%spounds per square inch' end;
class function TPoundPerSquareInchQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TPoundPerSquareInchQty.Exponents: TExponents; begin result := [1]; end;
class function TPoundPerSquareInchQty.ToBaseFactor: double; begin result := 6894.75729316836; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i adim.inc}
class function TJoulePerCubicMeterQty.Symbol: string; begin result := '%sJ/%sm3' end;
class function TJoulePerCubicMeterQty.SingularName: string; begin result := '%sjoule per cubic %smeter' end;
class function TJoulePerCubicMeterQty.PluralName: string; begin result := '%sjoules per cubic %smeter' end;
class function TJoulePerCubicMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TJoulePerCubicMeterQty.Exponents: TExponents; begin result := [1, -3]; end;

class operator TMeterQty.*(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJouleQty.*(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJouleQty.*(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJouleQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TWattHourQty}{$i adim.inc}
class function TWattHourQty.Symbol: string; begin result := '%sW·h' end;
class function TWattHourQty.SingularName: string; begin result := '%swatt hour' end;
class function TWattHourQty.PluralName: string; begin result := '%swatt hours' end;
class function TWattHourQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TWattHourQty.Exponents: TExponents; begin result := [1]; end;
class function TWattHourQty.ToBaseFactor: double; begin result := 3600; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TElettronvoltQty}{$i adim.inc}
class function TElettronvoltQty.Symbol: string; begin result := '%seV' end;
class function TElettronvoltQty.SingularName: string; begin result := '%selettronvolt' end;
class function TElettronvoltQty.PluralName: string; begin result := '%selettronvolts' end;
class function TElettronvoltQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TElettronvoltQty.Exponents: TExponents; begin result := [1]; end;
class function TElettronvoltQty.ToBaseFactor: double; begin result := 1.60217742320523E-019; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i adim.inc}
class function TNewtonMeterQty.Symbol: string; begin result := '%sN·%sm' end;
class function TNewtonMeterQty.SingularName: string; begin result := '%snewton %smeter' end;
class function TNewtonMeterQty.PluralName: string; begin result := '%snewton %smeters' end;
class function TNewtonMeterQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonMeterQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForceInchQty}{$i adim.inc}
class function TPoundForceInchQty.Symbol: string; begin result := 'lbf·in' end;
class function TPoundForceInchQty.SingularName: string; begin result := 'pound-force inch' end;
class function TPoundForceInchQty.PluralName: string; begin result := 'pound-force inches' end;
class function TPoundForceInchQty.Prefixes: TPrefixes; begin result := []; end;
class function TPoundForceInchQty.Exponents: TExponents; begin result := []; end;
class function TPoundForceInchQty.ToBaseFactor: double; begin result := 0.112984829027617; end;

class operator TSecondQty./(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TJouleQty; const ARight: THertzQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: TJouleQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TAmpereHourQty}{$i adim.inc}
class function TAmpereHourQty.Symbol: string; begin result := '%sA·h' end;
class function TAmpereHourQty.SingularName: string; begin result := '%sampere hour' end;
class function TAmpereHourQty.PluralName: string; begin result := '%sampere hours' end;
class function TAmpereHourQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TAmpereHourQty.Exponents: TExponents; begin result := [1]; end;
class function TAmpereHourQty.ToBaseFactor: double; begin result := 3600; end;

class operator TCoulombQty.*(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombQty./(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty./(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty./(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty./(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TOhmQty; const ARight: TSiemensQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TSiemensQty; const ARight: TOhmQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty./(const ALeft: double; const ARight: TSiemensQty): TOhmQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TSquareMeterQty; const ARight: TTeslaQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TTeslaQty; const ARight: TSquareMeterQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaQty./(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty.*(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty./(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty./(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCandelaQty.*(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaQty.*(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaQty./(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumenQty; const ARight: TSteradianQty): TCandelaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumenQty; const ARight: TSquareMeterQty): TLuxQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TLuxQty.*(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLuxQty.*(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLuxQty./(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i adim.inc}
class function TBequerelQty.Symbol: string; begin result := '%sBq' end;
class function TBequerelQty.SingularName: string; begin result := '%sbequerel' end;
class function TBequerelQty.PluralName: string; begin result := '%sbequerels' end;
class function TBequerelQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TBequerelQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i adim.inc}
class function TGrayQty.Symbol: string; begin result := '%sGy' end;
class function TGrayQty.SingularName: string; begin result := '%sgray' end;
class function TGrayQty.PluralName: string; begin result := '%sgrays' end;
class function TGrayQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TGrayQty.Exponents: TExponents; begin result := [1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i adim.inc}
class function TSievertQty.Symbol: string; begin result := '%sSv' end;
class function TSievertQty.SingularName: string; begin result := '%ssievert' end;
class function TSievertQty.PluralName: string; begin result := '%ssieverts' end;
class function TSievertQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TSievertQty.Exponents: TExponents; begin result := [1]; end;

class operator TSecondQty./(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianQty./(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianQty.*(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianQty.*(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i adim.inc}
class function TJoulePerDegreeQty.Symbol: string; begin result := '%sJ/deg' end;
class function TJoulePerDegreeQty.SingularName: string; begin result := '%sjoule per degree' end;
class function TJoulePerDegreeQty.PluralName: string; begin result := '%sjoules per degree' end;
class function TJoulePerDegreeQty.Prefixes: TPrefixes; begin result := [pNone]; end;
class function TJoulePerDegreeQty.Exponents: TExponents; begin result := [1]; end;
class function TJoulePerDegreeQty.ToBaseFactor: double; begin result := 180/Pi; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i adim.inc}
class function TNewtonMeterPerRadianQty.Symbol: string; begin result := '%sN·%sm/rad' end;
class function TNewtonMeterPerRadianQty.SingularName: string; begin result := '%snewton %smeter per radian' end;
class function TNewtonMeterPerRadianQty.PluralName: string; begin result := '%snewton %smeters per radian' end;
class function TNewtonMeterPerRadianQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonMeterPerRadianQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i adim.inc}
class function TNewtonMeterPerDegreeQty.Symbol: string; begin result := '%sN·%sm/deg' end;
class function TNewtonMeterPerDegreeQty.SingularName: string; begin result := '%snewton %smeter per degree' end;
class function TNewtonMeterPerDegreeQty.PluralName: string; begin result := '%snewton %smeters per degree' end;
class function TNewtonMeterPerDegreeQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonMeterPerDegreeQty.Exponents: TExponents; begin result := [1, 1]; end;
class function TNewtonMeterPerDegreeQty.ToBaseFactor: double; begin result := 180/Pi; end;

operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSecondSquaredQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i adim.inc}
class function TPoundForcePerInchQty.Symbol: string; begin result := 'lbf/in' end;
class function TPoundForcePerInchQty.SingularName: string; begin result := 'pound-force per inch' end;
class function TPoundForcePerInchQty.PluralName: string; begin result := 'pounds-force per inch' end;
class function TPoundForcePerInchQty.Prefixes: TPrefixes; begin result := []; end;
class function TPoundForcePerInchQty.Exponents: TExponents; begin result := []; end;
class function TPoundForcePerInchQty.ToBaseFactor: double; begin result := 175.126835246476; end;

class operator TSecondQty./(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i adim.inc}
class function TPascalSecondQty.Symbol: string; begin result := '%sPa·%ss' end;
class function TPascalSecondQty.SingularName: string; begin result := '%spascal %ssecond' end;
class function TPascalSecondQty.PluralName: string; begin result := '%spascal %sseconds' end;
class function TPascalSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TPascalSecondQty.Exponents: TExponents; begin result := [1, 1]; end;

class operator TSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty./(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramPerCubicMeterQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TMeterPerSecondSquaredQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty.*(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondSquaredQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondSquaredQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSecondSquaredQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty./(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty./(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareSecondQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareSecondQty./(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareSecondQty.*(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareSecondQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKelvinQty.*(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i adim.inc}
class function TJoulePerKilogramQty.Symbol: string; begin result := '%sJ/%sg' end;
class function TJoulePerKilogramQty.SingularName: string; begin result := '%sjoule per %sgram' end;
class function TJoulePerKilogramQty.PluralName: string; begin result := '%sjoules per %sgram' end;
class function TJoulePerKilogramQty.Prefixes: TPrefixes; begin result := [pNone, pKilo]; end;
class function TJoulePerKilogramQty.Exponents: TExponents; begin result := [1, -1]; end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKelvinQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TJoulePerKilogramPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramQty; const ARight: TJoulePerKilogramPerKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinPerMeterQty./(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterQty.*(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerKelvinQty.*(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinPerMeterQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TSquareMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TQuarticKelvinQty; const ARight: TSquareMeterQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TSquareMeterQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TQuarticKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinQty): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TJoulePerMoleQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoleKelvinQty; const ARight: TKelvinQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TMoleKelvinQty): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerMolePerKelvinQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleQty): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinQty): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TKelvinQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerMoleQty; const ARight: TJoulePerMolePerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i adim.inc}
class function TNewtonPerCoulombQty.Symbol: string; begin result := '%sN/%sC' end;
class function TNewtonPerCoulombQty.SingularName: string; begin result := '%snewton per %scoulomb' end;
class function TNewtonPerCoulombQty.PluralName: string; begin result := '%snewtons per %scoulomb' end;
class function TNewtonPerCoulombQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerCoulombQty.Exponents: TExponents; begin result := [1, -1]; end;

class operator TMeterQty./(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerMeterQty.*(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMeterQty.*(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMeterQty./(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i adim.inc}
class function TNewtonSquareMeterPerCoulombQty.Symbol: string; begin result := '%sN·%sm2/%sC' end;
class function TNewtonSquareMeterPerCoulombQty.SingularName: string; begin result := '%snewton square %smeter per %scoulomb' end;
class function TNewtonSquareMeterPerCoulombQty.PluralName: string; begin result := '%snewton square %smeters per %scoulomb' end;
class function TNewtonSquareMeterPerCoulombQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TNewtonSquareMeterPerCoulombQty.Exponents: TExponents; begin result := [1, 2, -1]; end;

operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltMeterPerSecondQty./(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TMeterQty; const ARight: TFaradPerMeterQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TMeterQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TFaradQty; const ARight: TFaradPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltMeterQty; const ARight: TFaradPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombQty; const ARight: TFaradPerMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TFaradPerMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltPerMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TFaradPerMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TFaradPerMeterQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerAmpereQty./(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i adim.inc}
class function TNewtonPerAmpereQty.Symbol: string; begin result := '%sN/%sA' end;
class function TNewtonPerAmpereQty.SingularName: string; begin result := '%snewton per %sampere' end;
class function TNewtonPerAmpereQty.PluralName: string; begin result := '%snewtons per %sampere' end;
class function TNewtonPerAmpereQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerAmpereQty.Exponents: TExponents; begin result := [1, -1]; end;

class operator TAmpereQty./(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerAmpereQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i adim.inc}
class function TTeslaMeterPerAmpereQty.Symbol: string; begin result := '%sT·%sm/%sA' end;
class function TTeslaMeterPerAmpereQty.SingularName: string; begin result := '%stesla %smeter per %sampere' end;
class function TTeslaMeterPerAmpereQty.PluralName: string; begin result := '%stesla %smeters per %sampere' end;
class function TTeslaMeterPerAmpereQty.Prefixes: TPrefixes; begin result := [pNone, pNone, pNone]; end;
class function TTeslaMeterPerAmpereQty.Exponents: TExponents; begin result := [1, 1, -1]; end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i adim.inc}
class function TNewtonPerSquareAmpereQty.Symbol: string; begin result := '%sN/%sA2' end;
class function TNewtonPerSquareAmpereQty.SingularName: string; begin result := '%snewton per square %sampere' end;
class function TNewtonPerSquareAmpereQty.PluralName: string; begin result := '%snewtons per square %sampere' end;
class function TNewtonPerSquareAmpereQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TNewtonPerSquareAmpereQty.Exponents: TExponents; begin result := [1, -2]; end;

class operator TMeterQty./(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TReciprocalMeterQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TReciprocalMeterQty; const ARight: TMeterQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJouleQty.*(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJouleQty./(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i adim.inc}
class function TJouleSecondQty.Symbol: string; begin result := '%sJ·%ss' end;
class function TJouleSecondQty.SingularName: string; begin result := '%sjoule %ssecond' end;
class function TJouleSecondQty.PluralName: string; begin result := '%sjoule %sseconds' end;
class function TJouleSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TJouleSecondQty.Exponents: TExponents; begin result := [1, 1]; end;

{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i adim.inc}
class function TElettronvoltSecondQty.Symbol: string; begin result := '%seV·%ss' end;
class function TElettronvoltSecondQty.SingularName: string; begin result := '%selettronvolt %ssecond' end;
class function TElettronvoltSecondQty.PluralName: string; begin result := '%selettronvolt %sseconds' end;
class function TElettronvoltSecondQty.Prefixes: TPrefixes; begin result := [pNone, pNone]; end;
class function TElettronvoltSecondQty.Exponents: TExponents; begin result := [1, 1]; end;
class function TElettronvoltSecondQty.ToBaseFactor: double; begin result := 1.60217742320523E-019; end;

operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TLumenPerWattQty.*(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLumenPerWattQty.*(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLumenPerWattQty./(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty./(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TReciprocalMoleQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TReciprocalMoleQty; const ARight: TMoleQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty./(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty./(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMolePerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMolePerCubicMeterQty.*(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMolePerCubicMeterQty./(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaPerSquareMeterQty./(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerKilogramQty.*(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerKilogramQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TGrayPerSecondQty.*(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TGrayPerSecondQty.*(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TGrayPerSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSteradianQty.*(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSteradianQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSteradianQty; const ARight: TSquareMeterQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TSquareMeterSteradianQty; const ARight: TSquareMeterQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSteradianQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterSteradianQty): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSteradianQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKatalPerCubicMeterQty./(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaQty}{$DEFINE TUnit:=TTeslaUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKelvinQty}{$DEFINE TUnit:=TSquareKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOhmMeterQty}{$DEFINE TUnit:=TOhmMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=THertzUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareVoltQty}{$DEFINE TUnit:=TSquareVoltUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerMoleQty}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TPascalUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalMeterQty}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerMeterQty}{$DEFINE TUnit:=TWattPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmperePerMeterQty}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=THenryPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKatalQty}{$DEFINE TUnit:=TKatalUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerMeterQty}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattQty}{$DEFINE TUnit:=TWattUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKelvinQty}{$DEFINE TUnit:=TKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TJouleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalMoleQty}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombQty}{$DEFINE TUnit:=TCoulombUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerKelvinQty}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerSecondQty}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCandelaQty}{$DEFINE TUnit:=TCandelaUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltQty}{$DEFINE TUnit:=TVoltUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWeberQty}{$DEFINE TUnit:=TWeberUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterQty}{$DEFINE TUnit:=TSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuinticMeterQty}{$DEFINE TUnit:=TQuinticMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianQty}{$DEFINE TUnit:=TRadianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TTeslaMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterKelvinQty}{$DEFINE TUnit:=TMeterKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSteradianQty}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmpereQty}{$DEFINE TUnit:=TAmpereUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonQty}{$DEFINE TUnit:=TNewtonUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticKelvinQty}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLumenPerWattQty}{$DEFINE TUnit:=TLumenPerWattUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMoleKelvinQty}{$DEFINE TUnit:=TMoleKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareAmpereQty}{$DEFINE TUnit:=TSquareAmpereUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSteradianQty}{$DEFINE TUnit:=TSteradianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareNewtonQty}{$DEFINE TUnit:=TSquareNewtonUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramQty}{$DEFINE TUnit:=TKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOhmQty}{$DEFINE TUnit:=TOhmUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLumenQty}{$DEFINE TUnit:=TLumenUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramKelvinQty}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMoleQty}{$DEFINE TUnit:=TMoleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterQty}{$DEFINE TUnit:=TCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryQty}{$DEFINE TUnit:=THenryUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSexticMeterQty}{$DEFINE TUnit:=TSexticMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticMeterQty}{$DEFINE TUnit:=TQuarticMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareJouleQty}{$DEFINE TUnit:=TSquareJouleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPoiseuilleUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterQty}{$DEFINE TUnit:=TMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSecondQty}{$DEFINE TUnit:=TSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareCoulombQty}{$DEFINE TUnit:=TSquareCoulombUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSiemensQty}{$DEFINE TUnit:=TSiemensUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSecondSquaredQty}{$DEFINE TUnit:=TMeterPerSecondSquaredUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFaradQty}{$DEFINE TUnit:=TFaradUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFaradPerMeterQty}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TVoltMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareHertzQty}{$DEFINE TUnit:=TSquareHertzUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TGrayPerSecondQty}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareSecondQty}{$DEFINE TUnit:=TSquareSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicKelvinQty}{$DEFINE TUnit:=TCubicKelvinUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLuxQty}{$DEFINE TUnit:=TLuxUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramQty}{$DEFINE TUnit:=TSquareKilogramUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDayQty}{$DEFINE TUnit:=TDayUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THourQty}{$DEFINE TUnit:=THourUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMinuteQty}{$DEFINE TUnit:=TMinuteUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareDayQty}{$DEFINE TUnit:=TSquareDayUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareHourQty}{$DEFINE TUnit:=TSquareHourUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMinuteQty}{$DEFINE TUnit:=TSquareMinuteUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAstronomicalQty}{$DEFINE TUnit:=TAstronomicalUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TInchQty}{$DEFINE TUnit:=TInchUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFootQty}{$DEFINE TUnit:=TFootUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TYardQty}{$DEFINE TUnit:=TYardUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMileQty}{$DEFINE TUnit:=TMileUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNauticalMileQty}{$DEFINE TUnit:=TNauticalMileUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareInchQty}{$DEFINE TUnit:=TSquareInchUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareFootQty}{$DEFINE TUnit:=TSquareFootUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareYardQty}{$DEFINE TUnit:=TSquareYardUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMileQty}{$DEFINE TUnit:=TSquareMileUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicInchQty}{$DEFINE TUnit:=TCubicInchUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicFootQty}{$DEFINE TUnit:=TCubicFootUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicYardQty}{$DEFINE TUnit:=TCubicYardUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLitreQty}{$DEFINE TUnit:=TLitreUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TGallonQty}{$DEFINE TUnit:=TGallonUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTonneQty}{$DEFINE TUnit:=TTonneUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundQty}{$DEFINE TUnit:=TPoundUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOunceQty}{$DEFINE TUnit:=TOunceUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TStoneQty}{$DEFINE TUnit:=TStoneUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTonQty}{$DEFINE TUnit:=TTonUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeQty}{$DEFINE TUnit:=TDegreeUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareDegreeQty}{$DEFINE TUnit:=TSquareDegreeUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: double; const ARight: TSecondUnit): THertzQty;
begin
  result.FValue := ALeft;
end;

class operator TSquareSecondUnit./(const ALeft: double; const ARight: TSquareSecondUnit): TSquareHertzQty;
begin
  result.FValue := ALeft;
end;

class operator TSecondUnit./(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerSecondSquaredQty}{$DEFINE TUnit:=TRadianPerSecondSquaredUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i adim.inc}

class operator TSquareSecondUnit./(const ALeft: TSteradianQty; const ARight: TSquareSecondUnit): TSteradianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TMeterQty; const ARight: TSecondUnit): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator THertzUnit.*(const ALeft: TMeterQty; const ARight: THertzUnit): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerHourQty}{$DEFINE TUnit:=TMeterPerHourUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMilePerHourQty}{$DEFINE TUnit:=TMilePerHourUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSecondSquaredQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i adim.inc}

class operator TSquareSecondUnit./(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterPerSecondUnit.*(const ALeft: TKilogramQty; const ARight: TMeterPerSecondUnit): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSecondQty}{$DEFINE TUnit:=TNewtonSecondUnit}{$i adim.inc}

class operator TSquareMeterUnit.*(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCubicMeterUnit./(const ALeft: TKilogramQty; const ARight: TCubicMeterUnit): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterPerSecondSquaredUnit.*(const ALeft: TKilogramQty; const ARight: TMeterPerSecondSquaredUnit): TNewtonQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForceQty}{$DEFINE TUnit:=TPoundForceUnit}{$i adim.inc}

class operator TSquareMeterUnit./(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TBarQty}{$DEFINE TUnit:=TBarUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i adim.inc}

class operator TMeterUnit.*(const ALeft: TNewtonQty; const ARight: TMeterUnit): TJouleQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKilogramUnit./(const ALeft: TJouleQty; const ARight: TKilogramUnit): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit.*(const ALeft: TJouleQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattHourQty}{$DEFINE TUnit:=TWattHourUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TElettronvoltQty}{$DEFINE TUnit:=TElettronvoltUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonMeterQty}{$DEFINE TUnit:=TNewtonMeterUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForceInchQty}{$DEFINE TUnit:=TPoundForceInchUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit.*(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmpereHourQty}{$DEFINE TUnit:=TAmpereHourUnit}{$i adim.inc}

class operator TAmpereUnit./(const ALeft: TWattQty; const ARight: TAmpereUnit): TVoltQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCoulombUnit./(const ALeft: TJouleQty; const ARight: TCoulombUnit): TVoltQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TVoltUnit./(const ALeft: TCoulombQty; const ARight: TVoltUnit): TFaradQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit./(const ALeft: TVoltQty; const ARight: TAmpereUnit): TOhmQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TOhmUnit./(const ALeft: double; const ARight: TOhmUnit): TSiemensQty;
begin
  result.FValue := ALeft;
end;

class operator TSecondUnit.*(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TWeberQty; const ARight: TSquareMeterUnit): TTeslaQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit./(const ALeft: TWeberQty; const ARight: TAmpereUnit): THenryQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSteradianUnit.*(const ALeft: TCandelaQty; const ARight: TSteradianUnit): TLumenQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TLumenQty; const ARight: TSquareMeterUnit): TLuxQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TBequerelQty}{$DEFINE TUnit:=TBequerelUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TGrayQty}{$DEFINE TUnit:=TGrayUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSievertQty}{$DEFINE TUnit:=TSievertUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TRadianUnit./(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i adim.inc}

class operator TCubicMeterUnit./(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TNewtonQty; const ARight: TMeterUnit): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit.*(const ALeft: TPascalQty; const ARight: TSecondUnit): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TPoiseuilleQty; const ARight: TMeterUnit): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPascalSecondQty}{$DEFINE TUnit:=TPascalSecondUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TSquareMeterQty; const ARight: TSecondUnit): TSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TQuarticMeterUnit./(const ALeft: TKilogramQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit.*(const ALeft: TQuarticMeterQty; const ARight: TSecondUnit): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TQuarticMeterUnit./(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondUnit): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKilogramUnit./(const ALeft: TCubicMeterQty; const ARight: TKilogramUnit): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit.*(const ALeft: TKilogramQty; const ARight: TSquareSecondUnit): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TCubicMeterQty; const ARight: TSquareSecondUnit): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit.*(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareKilogramUnit./(const ALeft: TNewtonQty; const ARight: TSquareKilogramUnit): TNewtonPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TSquareKilogramQty; const ARight: TMeterUnit): TSquareKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterUnit): TSquareKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareKilogramUnit./(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramUnit): TSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareKilogramUnit./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramUnit): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: double; const ARight: TKelvinUnit): TReciprocalKelvinQty;
begin
  result.FValue := ALeft;
end;

class operator TKelvinUnit.*(const ALeft: TKilogramQty; const ARight: TKelvinUnit): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: TJouleQty; const ARight: TKelvinUnit): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i adim.inc}

class operator TKelvinUnit./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinUnit): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKilogramUnit./(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramUnit): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit.*(const ALeft: TMeterQty; const ARight: TKelvinUnit): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TKelvinQty; const ARight: TMeterUnit): TKelvinPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TWattQty; const ARight: TMeterUnit): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TWattQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: TWattQty; const ARight: TKelvinUnit): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: TWattPerMeterQty; const ARight: TKelvinUnit): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TWattPerKelvinQty; const ARight: TMeterUnit): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit.*(const ALeft: TSquareMeterQty; const ARight: TKelvinUnit): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinUnit): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TQuarticKelvinUnit.*(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinUnit): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TQuarticKelvinUnit./(const ALeft: TWattQty; const ARight: TQuarticKelvinUnit): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TQuarticKelvinUnit./(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinUnit): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMoleUnit./(const ALeft: TJouleQty; const ARight: TMoleUnit): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit.*(const ALeft: TMoleQty; const ARight: TKelvinUnit): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMoleUnit./(const ALeft: TJoulePerKelvinQty; const ARight: TMoleUnit): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKelvinUnit./(const ALeft: TJoulePerMoleQty; const ARight: TKelvinUnit): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TOhmQty; const ARight: TMeterUnit): TOhmMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCoulombUnit./(const ALeft: TNewtonQty; const ARight: TCoulombUnit): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i adim.inc}

class operator TMeterUnit./(const ALeft: TCoulombQty; const ARight: TMeterUnit): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TSquareCoulombQty; const ARight: TMeterUnit): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TCoulombQty; const ARight: TSquareMeterUnit): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareCoulombUnit./(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombUnit): TSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareCoulombUnit./(const ALeft: TNewtonQty; const ARight: TSquareCoulombUnit): TNewtonPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareCoulombUnit./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombUnit): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterUnit): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TVoltQty; const ARight: TMeterUnit): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: TVoltMeterQty; const ARight: TSecondUnit): TVoltMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TFaradQty; const ARight: TMeterUnit): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TAmpereQty; const ARight: TMeterUnit): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit./(const ALeft: TMeterQty; const ARight: TAmpereUnit): TMeterPerAmpereQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TTeslaQty; const ARight: TMeterUnit): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit./(const ALeft: TNewtonQty; const ARight: TAmpereUnit): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i adim.inc}

class operator TAmpereUnit./(const ALeft: TTeslaQty; const ARight: TAmpereUnit): TTeslaPerAmpereQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: THenryQty; const ARight: TMeterUnit): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit./(const ALeft: TTeslaMeterQty; const ARight: TAmpereUnit): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterUnit): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i adim.inc}

class operator TMeterUnit./(const ALeft: TRadianQty; const ARight: TMeterUnit): TRadianPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: double; const ARight: TMeterUnit): TReciprocalMeterQty;
begin
  result.FValue := ALeft;
end;

class operator TSquareMeterUnit./(const ALeft: TSquareSecondQty; const ARight: TSquareMeterUnit): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJouleSecondQty}{$DEFINE TUnit:=TJouleSecondUnit}{$i adim.inc}
{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i adim.inc}

class operator TWattUnit./(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMoleUnit./(const ALeft: double; const ARight: TMoleUnit): TReciprocalMoleQty;
begin
  result.FValue := ALeft;
end;

class operator TSquareMeterUnit./(const ALeft: TAmpereQty; const ARight: TSquareMeterUnit): TAmperePerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCubicMeterUnit./(const ALeft: TMoleQty; const ARight: TCubicMeterUnit): TMolePerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TCandelaQty; const ARight: TSquareMeterUnit): TCandelaPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCubicMeterUnit./(const ALeft: TCoulombQty; const ARight: TCubicMeterUnit): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TKilogramUnit./(const ALeft: TCoulombQty; const ARight: TKilogramUnit): TCoulombPerKilogramQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondUnit): TGrayPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSteradianUnit./(const ALeft: TWattQty; const ARight: TSteradianUnit): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSteradianUnit.*(const ALeft: TSquareMeterQty; const ARight: TSteradianUnit): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSteradianUnit./(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianUnit): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterUnit): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCubicMeterUnit./(const ALeft: TKatalQty; const ARight: TCubicMeterUnit): TKatalPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMoleUnit./(const ALeft: TCoulombQty; const ARight: TMoleUnit): TCoulombPerMoleQty;
begin
  result.FValue := ALeft.FValue;
end;

function TSecondHelper.ToDay: TDayQty;
begin
  result.FValue := FValue / TDayQty.ToBaseFactor;
end;

function TSecondHelper.ToHour: THourQty;
begin
  result.FValue := FValue / THourQty.ToBaseFactor;
end;

function TSecondHelper.ToMinute: TMinuteQty;
begin
  result.FValue := FValue / TMinuteQty.ToBaseFactor;
end;

function TSquareSecondHelper.ToSquareDay: TSquareDayQty;
begin
  result.FValue := FValue / TSquareDayQty.ToBaseFactor;
end;

function TSquareSecondHelper.ToSquareHour: TSquareHourQty;
begin
  result.FValue := FValue / TSquareHourQty.ToBaseFactor;
end;

function TSquareSecondHelper.ToSquareMinute: TSquareMinuteQty;
begin
  result.FValue := FValue / TSquareMinuteQty.ToBaseFactor;
end;

function TMeterHelper.ToAstronomical: TAstronomicalQty;
begin
  result.FValue := FValue / TAstronomicalQty.ToBaseFactor;
end;

function TMeterHelper.ToInch: TInchQty;
begin
  result.FValue := FValue / TInchQty.ToBaseFactor;
end;

function TMeterHelper.ToFoot: TFootQty;
begin
  result.FValue := FValue / TFootQty.ToBaseFactor;
end;

function TMeterHelper.ToYard: TYardQty;
begin
  result.FValue := FValue / TYardQty.ToBaseFactor;
end;

function TMeterHelper.ToMile: TMileQty;
begin
  result.FValue := FValue / TMileQty.ToBaseFactor;
end;

function TMeterHelper.ToNauticalMile: TNauticalMileQty;
begin
  result.FValue := FValue / TNauticalMileQty.ToBaseFactor;
end;

function TSquareMeterHelper.ToSquareInch: TSquareInchQty;
begin
  result.FValue := FValue / TSquareInchQty.ToBaseFactor;
end;

function TSquareMeterHelper.ToSquareFoot: TSquareFootQty;
begin
  result.FValue := FValue / TSquareFootQty.ToBaseFactor;
end;

function TSquareMeterHelper.ToSquareYard: TSquareYardQty;
begin
  result.FValue := FValue / TSquareYardQty.ToBaseFactor;
end;

function TSquareMeterHelper.ToSquareMile: TSquareMileQty;
begin
  result.FValue := FValue / TSquareMileQty.ToBaseFactor;
end;

function TCubicMeterHelper.ToCubicInch: TCubicInchQty;
begin
  result.FValue := FValue / TCubicInchQty.ToBaseFactor;
end;

function TCubicMeterHelper.ToCubicFoot: TCubicFootQty;
begin
  result.FValue := FValue / TCubicFootQty.ToBaseFactor;
end;

function TCubicMeterHelper.ToCubicYard: TCubicYardQty;
begin
  result.FValue := FValue / TCubicYardQty.ToBaseFactor;
end;

function TCubicMeterHelper.ToLitre: TLitreQty;
begin
  result.FValue := FValue / TLitreQty.ToBaseFactor;
end;

function TCubicMeterHelper.ToGallon: TGallonQty;
begin
  result.FValue := FValue / TGallonQty.ToBaseFactor;
end;

function TKilogramHelper.ToTonne: TTonneQty;
begin
  result.FValue := FValue / TTonneQty.ToBaseFactor;
end;

function TKilogramHelper.ToPound: TPoundQty;
begin
  result.FValue := FValue / TPoundQty.ToBaseFactor;
end;

function TKilogramHelper.ToOunce: TOunceQty;
begin
  result.FValue := FValue / TOunceQty.ToBaseFactor;
end;

function TKilogramHelper.ToStone: TStoneQty;
begin
  result.FValue := FValue / TStoneQty.ToBaseFactor;
end;

function TKilogramHelper.ToTon: TTonQty;
begin
  result.FValue := FValue / TTonQty.ToBaseFactor;
end;

function TDegreeCelsiusHelper.ToKelvin: TKelvinQty;
begin
  result.FValue := FValue + 273.15;
end;

function TKelvinHelper.ToDegreeCelsius: TDegreeCelsiusQty;
begin
  result.FValue := FValue - 273.15;
end;

function TDegreeFahrenheitHelper.ToKelvin: TKelvinQty;
begin
  result.FValue := 5/9 * (FValue - 32) + 273.15;
end;

function TKelvinHelper.ToDegreeFahrenheit: TDegreeFahrenheitQty;
begin
  result.FValue := 9/5 * FValue - 459.67;
end;

function TRadianHelper.ToDegree: TDegreeQty;
begin
  result.FValue := FValue / TDegreeQty.ToBaseFactor;
end;

function TSteradianHelper.ToSquareDegree: TSquareDegreeQty;
begin
  result.FValue := FValue / TSquareDegreeQty.ToBaseFactor;
end;

function THertzHelper.ToRadianPerSecond: TRadianPerSecondQty;
begin
  result.FValue := FValue;
end;

function TRadianPerSecondHelper.ToHertz: THertzQty;
begin
  result.FValue := FValue;
end;

function TSquareHertzHelper.ToRadianPerSecondSquared: TRadianPerSecondSquaredQty;
begin
  result.FValue := FValue;
end;

function TRadianPerSecondSquaredHelper.ToSquareHertz: TSquareHertzQty;
begin
  result.FValue := FValue;
end;

function TSquareHertzHelper.ToSteradianPerSquareSecond: TSteradianPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TSteradianPerSquareSecondHelper.ToSquareHertz: TSquareHertzQty;
begin
  result.FValue := FValue;
end;

function TMeterPerSecondHelper.ToMeterPerHour: TMeterPerHourQty;
begin
  result.FValue := FValue / TMeterPerHourQty.ToBaseFactor;
end;

function TMeterPerSecondHelper.ToMilePerHour: TMilePerHourQty;
begin
  result.FValue := FValue / TMilePerHourQty.ToBaseFactor;
end;

function TMeterPerSecondHelper.ToNauticalMilePerHour: TNauticalMilePerHourQty;
begin
  result.FValue := FValue / TNauticalMilePerHourQty.ToBaseFactor;
end;

function TMeterPerSecondSquaredHelper.ToMeterPerSecondPerSecond: TMeterPerSecondPerSecondQty;
begin
  result.FValue := FValue;
end;

function TMeterPerSecondPerSecondHelper.ToMeterPerSecondSquared: TMeterPerSecondSquaredQty;
begin
  result.FValue := FValue;
end;

function TMeterPerSecondSquaredHelper.ToMeterPerHourPerSecond: TMeterPerHourPerSecondQty;
begin
  result.FValue := FValue / TMeterPerHourPerSecondQty.ToBaseFactor;
end;

function TKilogramMeterPerSecondHelper.ToNewtonSecond: TNewtonSecondQty;
begin
  result.FValue := FValue;
end;

function TNewtonSecondHelper.ToKilogramMeterPerSecond: TKilogramMeterPerSecondQty;
begin
  result.FValue := FValue;
end;

function TNewtonHelper.ToPoundForce: TPoundForceQty;
begin
  result.FValue := FValue / TPoundForceQty.ToBaseFactor;
end;

function TPascalHelper.ToBar: TBarQty;
begin
  result.FValue := FValue / TBarQty.ToBaseFactor;
end;

function TPascalHelper.ToPoundPerSquareInch: TPoundPerSquareInchQty;
begin
  result.FValue := FValue / TPoundPerSquareInchQty.ToBaseFactor;
end;

function TPascalHelper.ToJoulePerCubicMeter: TJoulePerCubicMeterQty;
begin
  result.FValue := FValue;
end;

function TJoulePerCubicMeterHelper.ToPascal: TPascalQty;
begin
  result.FValue := FValue;
end;

function TJouleHelper.ToWattHour: TWattHourQty;
begin
  result.FValue := FValue / TWattHourQty.ToBaseFactor;
end;

function TJouleHelper.ToElettronvolt: TElettronvoltQty;
begin
  result.FValue := FValue / TElettronvoltQty.ToBaseFactor;
end;

function TJouleHelper.ToNewtonMeter: TNewtonMeterQty;
begin
  result.FValue := FValue;
end;

function TNewtonMeterHelper.ToJoule: TJouleQty;
begin
  result.FValue := FValue;
end;

function TJouleHelper.ToPoundForceInch: TPoundForceInchQty;
begin
  result.FValue := FValue / TPoundForceInchQty.ToBaseFactor;
end;

function TCoulombHelper.ToAmpereHour: TAmpereHourQty;
begin
  result.FValue := FValue / TAmpereHourQty.ToBaseFactor;
end;

function THertzHelper.ToBequerel: TBequerelQty;
begin
  result.FValue := FValue;
end;

function TBequerelHelper.ToHertz: THertzQty;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareSecondHelper.ToGray: TGrayQty;
begin
  result.FValue := FValue;
end;

function TGrayHelper.ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareSecondHelper.ToSievert: TSievertQty;
begin
  result.FValue := FValue;
end;

function TSievertHelper.ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TJoulePerRadianHelper.ToJoulePerDegree: TJoulePerDegreeQty;
begin
  result.FValue := FValue / TJoulePerDegreeQty.ToBaseFactor;
end;

function TJoulePerRadianHelper.ToNewtonMeterPerRadian: TNewtonMeterPerRadianQty;
begin
  result.FValue := FValue;
end;

function TNewtonMeterPerRadianHelper.ToJoulePerRadian: TJoulePerRadianQty;
begin
  result.FValue := FValue;
end;

function TJoulePerRadianHelper.ToNewtonMeterPerDegree: TNewtonMeterPerDegreeQty;
begin
  result.FValue := FValue / TNewtonMeterPerDegreeQty.ToBaseFactor;
end;

function TNewtonPerMeterHelper.ToPoundForcePerInch: TPoundForcePerInchQty;
begin
  result.FValue := FValue / TPoundForcePerInchQty.ToBaseFactor;
end;

function TPoiseuilleHelper.ToPascalSecond: TPascalSecondQty;
begin
  result.FValue := FValue;
end;

function TPascalSecondHelper.ToPoiseuille: TPoiseuilleQty;
begin
  result.FValue := FValue;
end;

function TSquareMeterPerSquareSecondHelper.ToJoulePerKilogram: TJoulePerKilogramQty;
begin
  result.FValue := FValue;
end;

function TJoulePerKilogramHelper.ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TVoltPerMeterHelper.ToNewtonPerCoulomb: TNewtonPerCoulombQty;
begin
  result.FValue := FValue;
end;

function TNewtonPerCoulombHelper.ToVoltPerMeter: TVoltPerMeterQty;
begin
  result.FValue := FValue;
end;

function TVoltMeterHelper.ToNewtonSquareMeterPerCoulomb: TNewtonSquareMeterPerCoulombQty;
begin
  result.FValue := FValue;
end;

function TNewtonSquareMeterPerCoulombHelper.ToVoltMeter: TVoltMeterQty;
begin
  result.FValue := FValue;
end;

function TTeslaMeterHelper.ToNewtonPerAmpere: TNewtonPerAmpereQty;
begin
  result.FValue := FValue;
end;

function TNewtonPerAmpereHelper.ToTeslaMeter: TTeslaMeterQty;
begin
  result.FValue := FValue;
end;

function THenryPerMeterHelper.ToTeslaMeterPerAmpere: TTeslaMeterPerAmpereQty;
begin
  result.FValue := FValue;
end;

function TTeslaMeterPerAmpereHelper.ToHenryPerMeter: THenryPerMeterQty;
begin
  result.FValue := FValue;
end;

function THenryPerMeterHelper.ToNewtonPerSquareAmpere: TNewtonPerSquareAmpereQty;
begin
  result.FValue := FValue;
end;

function TNewtonPerSquareAmpereHelper.ToHenryPerMeter: THenryPerMeterQty;
begin
  result.FValue := FValue;
end;

function TKilogramSquareMeterPerSecondHelper.ToJouleSecond: TJouleSecondQty;
begin
  result.FValue := FValue;
end;

function TJouleSecondHelper.ToKilogramSquareMeterPerSecond: TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := FValue;
end;

function TKilogramSquareMeterPerSecondHelper.ToElettronvoltSecond: TElettronvoltSecondQty;
begin
  result.FValue := FValue / TElettronvoltSecondQty.ToBaseFactor;
end;

function SquarePower(AQuantity: TSecondQty): TSquareSecondQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareSecondQty): TSecondQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMeterQty): TSquareMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMeterQty): TMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TMeterQty): TCubicMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicMeterQty): TMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareMeterQty): TQuarticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticMeterQty): TSquareMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TMeterQty): TQuarticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticMeterQty): TMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function QuinticPower(AQuantity: TMeterQty): TQuinticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 5);
end;

function QuinticRoot(AQuantity: TQuinticMeterQty): TMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/5);
end;

function SquarePower(AQuantity: TCubicMeterQty): TSexticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSexticMeterQty): TCubicMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TSquareMeterQty): TSexticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TSexticMeterQty): TSquareMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SexticPower(AQuantity: TMeterQty): TSexticMeterQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 6);
end;

function SexticRoot(AQuantity: TSexticMeterQty): TMeterQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/6);
end;

function SquarePower(AQuantity: TAmpereQty): TSquareAmpereQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareAmpereQty): TAmpereQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKelvinQty): TSquareKelvinQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKelvinQty): TKelvinQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function CubicPower(AQuantity: TKelvinQty): TCubicKelvinQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 3);
end;

function CubicRoot(AQuantity: TCubicKelvinQty): TKelvinQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/3);
end;

function SquarePower(AQuantity: TSquareKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TQuarticKelvinQty): TSquareKelvinQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function QuarticPower(AQuantity: TKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 4);
end;

function QuarticRoot(AQuantity: TQuarticKelvinQty): TKelvinQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/4);
end;

function SquarePower(AQuantity: TRadianQty): TSteradianQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSteradianQty): TRadianQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: THertzQty): TSquareHertzQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareHertzQty): THertzQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareMeterPerSquareSecondQty): TMeterPerSecondQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TNewtonQty): TSquareNewtonQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareNewtonQty): TNewtonQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TCoulombQty): TSquareCoulombQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareCoulombQty): TCoulombQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TVoltQty): TSquareVoltQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareVoltQty): TVoltQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKilogramPerSquareSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

function SquarePower(AQuantity: TJouleQty): TSquareJouleQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareJouleQty): TJouleQty;
begin
  result.FValue := Power(AQuantity.FValue, 1/2);
end;

{ Trigonometric functions }

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

end.

