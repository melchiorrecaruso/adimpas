{
  Description: ADimPas library.

  Copyright (C) 2023-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  ADimPas library built on 07/04/2024.

  Number of base units: 128
  Number of factored units: 66
  Number of operators: 1025 (255 external, 770 internal)
}

unit ADim;

{$H+}
{$modeswitch advancedrecords}
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

{ TQuantity classes }

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareJouleSquareSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonCubicMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i adim.inc}
  class operator /(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty;
  class operator *(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i adim.inc}
  class operator /(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramSquareMeterPerSquareSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TNewtonQty): TNewtonCubicMeterQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonCubicMeterQty;
  class operator /(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TSquareSecondQty): TSquareJouleQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareJouleQty): TSquareJouleSquareSecondQty;
  class operator *(const ALeft: TSquareJouleQty; const ARight: TSquareSecondQty): TSquareJouleSquareSecondQty;
  class operator *(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareSecondQty): TKilogramMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TNewtonQty): TKilogramMeterQty;
  class operator /(const ALeft: TKilogramMeterQty; const ARight: TSquareSecondQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty;
  class operator *(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonSquareMeterQty): TNewtonCubicMeterQty;
  class operator *(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TNewtonCubicMeterQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty;
  class operator *(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty;
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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty;
  class operator *(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i adim.inc}
  class operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: THertzQty; const ARight: TSecondQty): double;
  class operator *(const ALeft: TSecondQty; const ARight: THertzQty): double;
  class operator /(const ALeft: double; const ARight: TSecondQty): THertzQty;
  class operator /(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMinuteQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareDayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMinuteQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAstronomicalQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNauticalMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAngstromQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMileQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicFootQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicYardQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLitreQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGallonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TKilogramQty): TNewtonCubicMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TNewtonCubicMeterQty): TSquareJouleSquareSecondQty;
  class operator *(const ALeft: TNewtonCubicMeterQty; const ARight: TKilogramQty): TSquareJouleSquareSecondQty;
  class operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty;
  class operator *(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareKilogramSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TJouleQty): TSquareKilogramSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TKilogramMeterQty; const ARight: TKilogramQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramQty): TKilogramMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramMeterQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTonneQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOunceQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TStoneQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTonQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i adim.inc}
  class operator *(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty;
  class operator *(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty;
  class operator /(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty;
  class operator /(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty;
  class operator *(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty;
  class operator /(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty;
  class operator /(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty;
  class operator *(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty;
  class operator /(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty;
  class operator *(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
  class operator /(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i adim.inc}
  class operator /(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty;
  class operator *(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty;
  class operator *(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty;
  class operator *(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty;
  class operator /(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty;
  class operator /(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty;
  class operator *(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i adim.inc}
  class operator /(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty;
  class operator *(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty;
  class operator *(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty;
  class operator /(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty;
  class operator /(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty;
  class operator *(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty;
  class operator /(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i adim.inc}
  class operator :=(const AQuantity: TRadianPerSecondQty): THertzQty;
  class operator :=(const AQuantity: THertzQty): TRadianPerSecondQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty;
  class operator /(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty;
  class operator *(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty;
  class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TRadianPerSquareSecondQty; const ARight: TSquareHertzQty): TRadianQty;
  class operator /(const ALeft: TRadianPerSquareSecondQty; const ARight: TRadianQty): TSquareHertzQty;
  class operator /(const ALeft: TRadianQty; const ARight: TRadianPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TRadianPerSquareSecondQty; const ARight: TSquareSecondQty): TRadianQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TRadianPerSquareSecondQty): TRadianQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSquareSecondQty): TSecondQty;
  class operator *(const ALeft: TRadianPerSquareSecondQty; const ARight: TSecondQty): TRadianPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TRadianPerSquareSecondQty): TRadianPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty;
  class operator /(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty;
  class operator /(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty;
  class operator /(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMilePerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSquareSecondQty): TKilogramQty;
  class operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSquareSecondQty): TNewtonQty;
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TMeterPerSquareSecondQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareHertzQty): TMeterQty;
  class operator /(const ALeft: TMeterPerSquareSecondQty; const ARight: TMeterQty): TSquareHertzQty;
  class operator /(const ALeft: TMeterQty; const ARight: TMeterPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSquareSecondQty): TSecondQty;
  class operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty;
  class operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty;
  class operator /(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TKilogramMeterPerSecondQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TSquareKilogramSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TMeterQty;
  class operator /(const ALeft: TKilogramMeterQty; const ARight: TKilogramMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TKilogramMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramMeterPerSecondQty): TKilogramMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TSquareJouleSquareSecondQty;
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty;
  class operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty;
  class operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TReciprocalMeterQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TReciprocalMeterQty): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TReciprocalMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramMeterPerSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty;
  class operator /(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSecondPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSecondQty; const ARight: TSecondPerMeterQty): TMeterQty;
  class operator *(const ALeft: TSecondPerMeterQty; const ARight: TMeterQty): TSecondQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSecondPerMeterQty): TSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TKilogramPerMeterQty; const ARight: TSecondPerMeterQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TKilogramPerMeterQty; const ARight: TKilogramPerSecondQty): TSecondPerMeterQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty;
  class operator *(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty;
  class operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty;
  class operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundPerCubicInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundForceQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty;
  class operator *(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty;
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty;
  class operator *(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty;
  class operator /(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
  class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TBarQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TElettronvoltQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonMeterQty): TNewtonCubicMeterQty;
  class operator *(const ALeft: TNewtonMeterQty; const ARight: TSquareMeterQty): TNewtonCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundForceInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRydbergQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCalorieQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty;
  class operator *(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty;
  class operator *(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty;
  class operator /(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty;
  class operator /(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty;
  class operator /(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty;
  class operator /(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty;
  class operator /(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty;
  class operator *(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty;
  class operator *(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmpereHourQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i adim.inc}
  class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty;
  class operator *(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty;
  class operator /(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty;
  class operator *(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty;
  class operator /(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty;
  class operator *(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty;
  class operator /(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty;
  class operator *(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty;
  class operator /(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty;
  class operator *(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i adim.inc}
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty;
  class operator *(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty;
  class operator *(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty;
  class operator *(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty;
  class operator *(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i adim.inc}
  class operator /(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty;
  class operator *(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty;
  class operator /(const ALeft: TWeberQty; const ARight: TOhmQty): TCoulombQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TOhmQty): TWeberQty;
  class operator *(const ALeft: TOhmQty; const ARight: TCoulombQty): TWeberQty;
  class operator /(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty;
  class operator *(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty;
  class operator /(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty;
  class operator *(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty;
  class operator *(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty;
  class operator /(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty;
  class operator *(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty;
  class operator *(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty;
  class operator /(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty;
  class operator *(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty;
  class operator *(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i adim.inc}
  class operator /(const ALeft: TAmpereQty; const ARight: TSiemensQty): TVoltQty;
  class operator *(const ALeft: TSiemensQty; const ARight: TVoltQty): TAmpereQty;
  class operator *(const ALeft: TVoltQty; const ARight: TSiemensQty): TAmpereQty;
  class operator /(const ALeft: TSiemensQty; const ARight: THertzQty): TFaradQty;
  class operator /(const ALeft: TSiemensQty; const ARight: TFaradQty): THertzQty;
  class operator /(const ALeft: TFaradQty; const ARight: TSiemensQty): TSecondQty;
  class operator *(const ALeft: TSiemensQty; const ARight: TSecondQty): TFaradQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSiemensQty): TFaradQty;
  class operator /(const ALeft: double; const ARight: TSiemensQty): TOhmQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i adim.inc}
  class operator /(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty;
  class operator /(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TTeslaQty): TWeberQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TSquareMeterQty): TWeberQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TTeslaQty): TCoulombQty;
  class operator *(const ALeft: TTeslaQty; const ARight: TCoulombQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TTeslaQty): TKilogramPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i adim.inc}
  class operator /(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty;
  class operator *(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty;
  class operator *(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty;
  class operator /(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty;
  class operator /(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty;
  class operator /(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty;
  class operator *(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty;
  class operator *(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty;
  class operator *(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
  class operator /(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSquareSecondQty;
  class operator /(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty;
  class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty;
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty;
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty;
  class operator /(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty;
  class operator *(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty;
  class operator *(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty;
  class operator /(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty;
  class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty;
  class operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty;
  class operator *(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty;
  class operator *(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty;
  class operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty;
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty;
  class operator *(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty;
  class operator /(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty;
  class operator *(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i adim.inc}
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty;
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSquareSecondQty;
  class operator /(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TSquareMeterQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty;
  class operator *(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty;
  class operator *(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty;
  class operator *(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty;
  class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty;
  class operator *(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double;
  class operator *(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty;
  class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty;
  class operator *(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty;
  class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty;
  class operator *(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty;
  class operator /(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty;
  class operator *(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty;
  class operator /(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty;
  class operator *(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty;
  class operator /(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty;
  class operator *(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty;
  class operator *(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty;
  class operator *(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty;
  class operator *(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty;
  class operator /(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty;
  class operator /(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty;
  class operator *(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty;
  class operator /(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty;
  class operator *(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty;
  class operator *(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty;
  class operator *(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty;
  class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty;
  class operator /(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
  class operator /(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty;
  class operator *(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty;
  class operator *(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i adim.inc}
  class operator *(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty;
  class operator /(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty;
  class operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
  class operator *(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty;
  class operator *(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty;
  class operator *(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty;
  class operator *(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty;
  class operator *(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty;
  class operator *(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty;
  class operator *(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i adim.inc}
  class operator /(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty;
  class operator *(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i adim.inc}
  class operator /(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty;
  class operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty;
  class operator *(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty;
  class operator *(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty;
  class operator /(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty;
  class operator *(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty;
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty;
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty;
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty;
  class operator /(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty;
  class operator /(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty;
  class operator *(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty;
  class operator /(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty;
  class operator *(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty;
  class operator *(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i adim.inc}
  class operator /(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty;
  class operator *(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty;
  class operator *(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty;
  class operator /(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty;
  class operator /(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty;
  class operator *(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty;
  class operator *(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i adim.inc}
  class operator /(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty;
  class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty;
  class operator /(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty;
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
  class operator *(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty;
  class operator /(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty;
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty;
  class operator *(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty;
  class operator *(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty;
  class operator *(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i adim.inc}
  class operator /(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty;
  class operator *(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty;
  class operator *(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty;
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i adim.inc}
  class operator /(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty;
  class operator *(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty;
  class operator *(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i adim.inc}
  class operator /(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty;
  class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty;
  class operator *(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty;
  class operator *(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty;
  class operator *(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{ External Operators }

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty;
operator /(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty;
operator /(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty;
operator /(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty;
operator /(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty;
operator /(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty;
operator /(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty;
operator /(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty;
operator /(const ALeft: double; const ARight: THertzQty): TSecondQty;
operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty;
operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty;
operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty;
operator /(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty;
operator *(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty;
operator *(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty;
operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSquareSecondQty;
operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSquareSecondQty;
operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSquareSecondQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSquareSecondQty;
operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty;
operator *(const ALeft: TSteradianQty; const ARight: TSquareHertzQty): TSteradianPerSquareSecondQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TSteradianQty): TSteradianPerSquareSecondQty;
operator /(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty;
operator *(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty;
operator *(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty;
operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSquareSecondQty;
operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSquareSecondQty;
operator *(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSquareSecondQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSquareSecondQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSquareSecondQty;
operator /(const ALeft: TKilogramMeterQty; const ARight: TMeterQty): TKilogramQty;
operator /(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty;
operator /(const ALeft: TKilogramMeterQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
operator *(const ALeft: TMeterQty; const ARight: TKilogramPerSecondQty): TKilogramMeterPerSecondQty;
operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty;
operator /(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: TKilogramSquareMeterQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: THertzQty; const ARight: TKilogramSquareMeterQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TReciprocalMeterQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TSecondQty; const ARight: TMeterQty): TSecondPerMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty;
operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSecondPerMeterQty): TKilogramPerMeterQty;
operator *(const ALeft: TSecondPerMeterQty; const ARight: TKilogramPerSecondQty): TKilogramPerMeterQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty;
operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty;
operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSquareSecondQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
operator *(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty;
operator *(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
operator /(const ALeft: TKilogramMeterQty; const ARight: TNewtonQty): TSquareSecondQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty;
operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty;
operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty;
operator /(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty;
operator /(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
operator /(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty;
operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty;
operator *(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty;
operator *(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TJouleQty): TKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty;
operator /(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty;
operator *(const ALeft: TJouleQty; const ARight: THertzQty): TWattQty;
operator *(const ALeft: THertzQty; const ARight: TJouleQty): TWattQty;
operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty;
operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty;
operator *(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty;
operator /(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty;
operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty;
operator /(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty;
operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty;
operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty;
operator /(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty;
operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty;
operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty;
operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty;
operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty;
operator /(const ALeft: TFaradQty; const ARight: TSecondQty): TSiemensQty;
operator *(const ALeft: TFaradQty; const ARight: THertzQty): TSiemensQty;
operator *(const ALeft: THertzQty; const ARight: TFaradQty): TSiemensQty;
operator /(const ALeft: TAmpereQty; const ARight: TVoltQty): TSiemensQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TCoulombQty): TTeslaQty;
operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty;
operator /(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty;
operator /(const ALeft: TWeberQty; const ARight: TCoulombQty): TOhmQty;
operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty;
operator *(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty;
operator *(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty;
operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty;
operator /(const ALeft: TLumenQty; const ARight: TSteradianQty): TCandelaQty;
operator /(const ALeft: TLumenQty; const ARight: TSquareMeterQty): TLuxQty;
operator *(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty;
operator *(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty;
operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty;
operator /(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty;
operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSquareSecondQty): TNewtonPerCubicMeterQty;
operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty;
operator *(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty;
operator *(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty;
operator *(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty;
operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty;
operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty;
operator *(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty;
operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty;
operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty;
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty;
operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty;
operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty;
operator /(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty;
operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty;
operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty;
operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty;
operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSquareSecondQty): TCubicMeterPerSquareSecondQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonQty): TCubicMeterQty;
operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TSquareMeterQty): TNewtonMeterQty;
operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonSquareMeterQty): TMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty;
operator /(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty;
operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty;
operator *(const ALeft: TSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerKilogramQty;
operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareSecondQty): TCubicMeterPerKilogramQty;
operator *(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty;
operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty;
operator *(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty;
operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty;
operator /(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty;
operator /(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty;
operator /(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty;
operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty;
operator /(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty;
operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty;
operator /(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty;
operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty;
operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty;
operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty;
operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty;
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
operator /(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty;
operator /(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty;
operator *(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty;
operator *(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty;
operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty;
operator *(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty;
operator *(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty;
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty;
operator /(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty;
operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty;
operator /(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty;
operator /(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty;
operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty;
operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty;
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty;
operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty;
operator /(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty;
operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty;
operator /(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty;
operator /(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty;
operator /(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty;
operator /(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty;
operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty;
operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty;
operator *(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty;
operator *(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty;
operator *(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty;
operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty;
operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty;
operator /(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty;
operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty;
operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty;
operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty;
operator *(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty;
operator *(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty;
operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TSquareJouleQty): TSquareSecondQty;
operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TNewtonCubicMeterQty): TKilogramQty;
operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty;
operator /(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty;
operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty;
operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty;
operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty;
operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty;
operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty;
operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty;
operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty;
operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty;
operator /(const ALeft: TSquareMeterSteradianQty; const ARight: TSteradianQty): TSquareMeterQty;
operator /(const ALeft: TWattQty; const ARight: TSquareMeterSteradianQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty;
operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty;
operator /(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty;
operator /(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty;

{ TUnit classes }

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKatalQty}{$DEFINE TUnit:=TKatalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterQty}{$DEFINE TUnit:=TMeterUnit}{$i adim.inc}
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
  class operator /(const ALeft: TSecondQty; const ARight: TMeterUnit): TSecondPerMeterQty;
  class operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterUnit): TKilogramMeterPerSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerMoleQty}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicKelvinQty}{$DEFINE TUnit:=TCubicKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMoleKelvinQty}{$DEFINE TUnit:=TMoleKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=THertzUnit}{$i adim.inc}
  class operator *(const ALeft: TMeterQty; const ARight: THertzUnit): TMeterPerSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKelvinQty}{$DEFINE TUnit:=TSquareKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSecondQty}{$DEFINE TUnit:=TSecondUnit}{$i adim.inc}
  class operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondUnit): TGrayPerSecondQty;
  class operator /(const ALeft: TVoltMeterQty; const ARight: TSecondUnit): TVoltMeterPerSecondQty;
  class operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondUnit): TKilogramPerQuarticMeterPerSecondQty;
  class operator *(const ALeft: TQuarticMeterQty; const ARight: TSecondUnit): TQuarticMeterSecondQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSecondUnit): TSquareMeterPerSecondQty;
  class operator *(const ALeft: TPascalQty; const ARight: TSecondUnit): TPoiseuilleQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty;
  class operator /(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty;
  class operator *(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty;
  class operator /(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty;
  class operator *(const ALeft: TJouleQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
  class operator /(const ALeft: TKilogramMeterQty; const ARight: TSecondUnit): TKilogramMeterPerSecondQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty;
  class operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterQty; const ARight: TSecondUnit): TMeterPerSecondQty;
  class operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSquareSecondQty;
  class operator /(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty;
  class operator /(const ALeft: double; const ARight: TSecondUnit): THertzQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDayQty}{$DEFINE TUnit:=TDayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THourQty}{$DEFINE TUnit:=THourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMinuteQty}{$DEFINE TUnit:=TMinuteUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareSecondQty}{$DEFINE TUnit:=TSquareSecondUnit}{$i adim.inc}
  class operator *(const ALeft: TSquareJouleQty; const ARight: TSquareSecondUnit): TSquareJouleSquareSecondQty;
  class operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondUnit): TCubicMeterPerSquareSecondQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareSecondUnit): TKilogramSquareSecondQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty;
  class operator /(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSquareSecondQty;
  class operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondUnit): TSteradianPerSquareSecondQty;
  class operator /(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareDayQty}{$DEFINE TUnit:=TSquareDayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareHourQty}{$DEFINE TUnit:=TSquareHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMinuteQty}{$DEFINE TUnit:=TSquareMinuteUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAstronomicalQty}{$DEFINE TUnit:=TAstronomicalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TInchQty}{$DEFINE TUnit:=TInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFootQty}{$DEFINE TUnit:=TFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TYardQty}{$DEFINE TUnit:=TYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMileQty}{$DEFINE TUnit:=TMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNauticalMileQty}{$DEFINE TUnit:=TNauticalMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAngstromQty}{$DEFINE TUnit:=TAngstromUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

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
  class operator *(const ALeft: TTeslaQty; const ARight: TSquareMeterUnit): TWeberQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramPerSquareMeterQty;
  class operator *(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareInchQty}{$DEFINE TUnit:=TSquareInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareFootQty}{$DEFINE TUnit:=TSquareFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareYardQty}{$DEFINE TUnit:=TSquareYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMileQty}{$DEFINE TUnit:=TSquareMileUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterQty}{$DEFINE TUnit:=TCubicMeterUnit}{$i adim.inc}
  class operator /(const ALeft: TKatalQty; const ARight: TCubicMeterUnit): TKatalPerCubicMeterQty;
  class operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterUnit): TCoulombPerCubicMeterQty;
  class operator /(const ALeft: TMoleQty; const ARight: TCubicMeterUnit): TMolePerCubicMeterQty;
  class operator *(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonCubicMeterQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterUnit): TKilogramPerCubicMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicInchQty}{$DEFINE TUnit:=TCubicInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicFootQty}{$DEFINE TUnit:=TCubicFootUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicYardQty}{$DEFINE TUnit:=TCubicYardUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLitreQty}{$DEFINE TUnit:=TLitreUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TGallonQty}{$DEFINE TUnit:=TGallonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticMeterQty}{$DEFINE TUnit:=TQuarticMeterUnit}{$i adim.inc}
  class operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterPerSecondQty;
  class operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterUnit): TKilogramPerQuarticMeterQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuinticMeterQty}{$DEFINE TUnit:=TQuinticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSexticMeterQty}{$DEFINE TUnit:=TSexticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramQty}{$DEFINE TUnit:=TKilogramUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TKilogramUnit): TCoulombPerKilogramQty;
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramUnit): TJoulePerKilogramPerKelvinQty;
  class operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramUnit): TCubicMeterPerKilogramQty;
  class operator /(const ALeft: TJouleQty; const ARight: TKilogramUnit): TSquareMeterPerSquareSecondQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTonneQty}{$DEFINE TUnit:=TTonneUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundQty}{$DEFINE TUnit:=TPoundUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOunceQty}{$DEFINE TUnit:=TOunceUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TStoneQty}{$DEFINE TUnit:=TStoneUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTonQty}{$DEFINE TUnit:=TTonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramQty}{$DEFINE TUnit:=TSquareKilogramUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramUnit): TNewtonSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramUnit): TSquareMeterPerSquareKilogramQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramUnit): TNewtonPerSquareKilogramQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmpereQty}{$DEFINE TUnit:=TAmpereUnit}{$i adim.inc}
  class operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereUnit): THenryPerMeterQty;
  class operator /(const ALeft: TTeslaQty; const ARight: TAmpereUnit): TTeslaPerAmpereQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TAmpereUnit): TTeslaMeterQty;
  class operator /(const ALeft: TMeterQty; const ARight: TAmpereUnit): TMeterPerAmpereQty;
  class operator /(const ALeft: TWeberQty; const ARight: TAmpereUnit): THenryQty;
  class operator *(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareAmpereQty}{$DEFINE TUnit:=TSquareAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

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

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticKelvinQty}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i adim.inc}
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinUnit): TWattPerSquareMeterPerQuarticKelvinQty;
  class operator /(const ALeft: TWattQty; const ARight: TQuarticKelvinUnit): TWattPerQuarticKelvinQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinUnit): TSquareMeterQuarticKelvinQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMoleQty}{$DEFINE TUnit:=TMoleUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TMoleUnit): TCoulombPerMoleQty;
  class operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleUnit): TJoulePerMolePerKelvinQty;
  class operator /(const ALeft: TJouleQty; const ARight: TMoleUnit): TJoulePerMoleQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCandelaQty}{$DEFINE TUnit:=TCandelaUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianQty}{$DEFINE TUnit:=TRadianUnit}{$i adim.inc}
  class operator /(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TDegreeQty}{$DEFINE TUnit:=TDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSteradianQty}{$DEFINE TUnit:=TSteradianUnit}{$i adim.inc}
  class operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianUnit): TWattPerSquareMeterPerSteradianQty;
  class operator *(const ALeft: TSquareMeterQty; const ARight: TSteradianUnit): TSquareMeterSteradianQty;
  class operator /(const ALeft: TWattQty; const ARight: TSteradianUnit): TWattPerSteradianQty;
  class operator *(const ALeft: TCandelaQty; const ARight: TSteradianUnit): TLumenQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareDegreeQty}{$DEFINE TUnit:=TSquareDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareHertzQty}{$DEFINE TUnit:=TSquareHertzUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerSecondQty}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerSquareSecondQty}{$DEFINE TUnit:=TRadianPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerHourQty}{$DEFINE TUnit:=TMeterPerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMilePerHourQty}{$DEFINE TUnit:=TMilePerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$DEFINE TUnit:=TMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramMeterQty}{$DEFINE TUnit:=TKilogramMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramSquareMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TNewtonSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalMeterQty}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TNewtonMeterSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSecondPerMeterQty}{$DEFINE TUnit:=TSecondPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundPerCubicInchQty}{$DEFINE TUnit:=TPoundPerCubicInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonQty}{$DEFINE TUnit:=TNewtonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForceQty}{$DEFINE TUnit:=TPoundForceUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareNewtonQty}{$DEFINE TUnit:=TSquareNewtonUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TPascalUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TBarQty}{$DEFINE TUnit:=TBarUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TJouleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattHourQty}{$DEFINE TUnit:=TWattHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TElettronvoltQty}{$DEFINE TUnit:=TElettronvoltUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TNewtonMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForceInchQty}{$DEFINE TUnit:=TPoundForceInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRydbergQty}{$DEFINE TUnit:=TRydbergUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCalorieQty}{$DEFINE TUnit:=TCalorieUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattQty}{$DEFINE TUnit:=TWattUnit}{$i adim.inc}
  class operator /(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombQty}{$DEFINE TUnit:=TCoulombUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonQty; const ARight: TCoulombUnit): TVoltPerMeterQty;
  class operator /(const ALeft: TJouleQty; const ARight: TCoulombUnit): TVoltQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmpereHourQty}{$DEFINE TUnit:=TAmpereHourUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareCoulombQty}{$DEFINE TUnit:=TSquareCoulombUnit}{$i adim.inc}
  class operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombUnit): TNewtonSquareMeterPerSquareCoulombQty;
  class operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombUnit): TNewtonPerSquareCoulombQty;
  class operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombUnit): TSquareMeterPerSquareCoulombQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltQty}{$DEFINE TUnit:=TVoltUnit}{$i adim.inc}
  class operator /(const ALeft: TCoulombQty; const ARight: TVoltUnit): TFaradQty;
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareVoltQty}{$DEFINE TUnit:=TSquareVoltUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFaradQty}{$DEFINE TUnit:=TFaradUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOhmQty}{$DEFINE TUnit:=TOhmUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSiemensQty}{$DEFINE TUnit:=TSiemensUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaQty}{$DEFINE TUnit:=TTeslaUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWeberQty}{$DEFINE TUnit:=TWeberUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryQty}{$DEFINE TUnit:=THenryUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLumenQty}{$DEFINE TUnit:=TLumenUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLuxQty}{$DEFINE TUnit:=TLuxUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=TBequerelUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TGrayUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSievertUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPoiseuilleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPascalSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonCubicMeterQty}{$DEFINE TUnit:=TNewtonCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramKelvinQty}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterKelvinQty}{$DEFINE TUnit:=TMeterKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerMeterQty}{$DEFINE TUnit:=TWattPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerKelvinQty}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TOhmMeterQty}{$DEFINE TUnit:=TOhmMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TVoltMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TFaradPerMeterQty}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmperePerMeterQty}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TTeslaMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=THenryPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TRadianPerMeterQty}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareJouleQty}{$DEFINE TUnit:=TSquareJouleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TJouleSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareJouleSquareSecondQty}{$DEFINE TUnit:=TSquareJouleSquareSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TLumenPerWattQty}{$DEFINE TUnit:=TLumenPerWattUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TReciprocalMoleQty}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TGrayPerSecondQty}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSteradianQty}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i adim.inc}
{$DEFINE INTF_END}{$i adim.inc}

{ Quantity of TJoulesPerKelvin }

type
  TJoulesPerKelvin = TJoulePerKelvinQty;

const
  rsJoulePerKelvinSymbol     = '%sJ/%sK';
  rsJoulePerKelvinName       = '%sjoule per %skelvin';
  rsJoulePerKelvinPluralName = '%sjoules per %skelvin';

const
  cJoulePerKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cJoulePerKelvinExponents : TExponents = (1, -1);

{ Quantity of TKatals }

type
  TKatals = TKatalQty;

var
  kat: TKatalUnit;

const
  rsKatalSymbol     = '%skat';
  rsKatalName       = '%skatal';
  rsKatalPluralName = '%skatals';

const
  cKatalPrefixes  : TPrefixes  = (pNone);
  cKatalExponents : TExponents = (1);

{ Quantity of TMeters }

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

const
  rsMeterSymbol     = '%sm';
  rsMeterName       = '%smeter';
  rsMeterPluralName = '%smeters';

const
  cMeterPrefixes  : TPrefixes  = (pNone);
  cMeterExponents : TExponents = (1);

{ Quantity of TJoulesPerMole }

type
  TJoulesPerMole = TJoulePerMoleQty;

const
  rsJoulePerMoleSymbol     = '%sJ/%smol';
  rsJoulePerMoleName       = '%sjoule per %smole';
  rsJoulePerMolePluralName = '%sjoules per %smole';

const
  cJoulePerMolePrefixes  : TPrefixes  = (pNone, pNone);
  cJoulePerMoleExponents : TExponents = (1, -1);

{ Quantity of TSquareKilogramsPerSquareSecond }

type
  TSquareKilogramsPerSquareSecond = TSquareKilogramPerSquareSecondQty;

const
  rsSquareKilogramPerSquareSecondSymbol     = '%sg2/%ss2';
  rsSquareKilogramPerSquareSecondName       = 'square %sgram per square %ssecond';
  rsSquareKilogramPerSquareSecondPluralName = 'square %sgrams per square %ssecond';

const
  cSquareKilogramPerSquareSecondPrefixes  : TPrefixes  = (pKilo, pNone);
  cSquareKilogramPerSquareSecondExponents : TExponents = (2, -2);

{ Quantity of TCubicKelvins }

type
  TCubicKelvins = TCubicKelvinQty;

var
  K3: TCubicKelvinUnit;

const
  rsCubicKelvinSymbol     = '%sK3';
  rsCubicKelvinName       = 'cubic %skelvin';
  rsCubicKelvinPluralName = 'cubic %skelvins';

const
  cCubicKelvinPrefixes  : TPrefixes  = (pNone);
  cCubicKelvinExponents : TExponents = (3);

{ Quantity of TMoleKelvins }

type
  TMoleKelvins = TMoleKelvinQty;

const
  rsMoleKelvinSymbol     = '%smol·%sK';
  rsMoleKelvinName       = '%smole %skelvin';
  rsMoleKelvinPluralName = '%smole %skelvins';

const
  cMoleKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cMoleKelvinExponents : TExponents = (1, 1);

{ Quantity of TNewtonsPerSquareKilogram }

type
  TNewtonsPerSquareKilogram = TNewtonPerSquareKilogramQty;

const
  rsNewtonPerSquareKilogramSymbol     = '%sN/%sg2';
  rsNewtonPerSquareKilogramName       = '%snewton per square %sgram';
  rsNewtonPerSquareKilogramPluralName = '%snewtons per square %sgram';

const
  cNewtonPerSquareKilogramPrefixes  : TPrefixes  = (pNone, pKilo);
  cNewtonPerSquareKilogramExponents : TExponents = (1, -2);

{ Quantity of TSquareMetersPerSquareKilogram }

type
  TSquareMetersPerSquareKilogram = TSquareMeterPerSquareKilogramQty;

const
  rsSquareMeterPerSquareKilogramSymbol     = '%sm2/%sg2';
  rsSquareMeterPerSquareKilogramName       = 'square %smeter per square %sgram';
  rsSquareMeterPerSquareKilogramPluralName = 'square %smeters per square %sgram';

const
  cSquareMeterPerSquareKilogramPrefixes  : TPrefixes  = (pNone, pKilo);
  cSquareMeterPerSquareKilogramExponents : TExponents = (2, -2);

{ Quantity of THertz }

type
  THertz = THertzQty;

var
  Hz: THertzUnit;

const
  THz: THertzQty = (FValue: 1E+12);
  GHz: THertzQty = (FValue: 1E+09);
  MHz: THertzQty = (FValue: 1E+06);
  kHz: THertzQty = (FValue: 1E+03);

const
  rsHertzSymbol     = '%sHz';
  rsHertzName       = '%shertz';
  rsHertzPluralName = '%shertz';

const
  cHertzPrefixes  : TPrefixes  = (pNone);
  cHertzExponents : TExponents = (1);

{ Quantity of TSquareKelvins }

type
  TSquareKelvins = TSquareKelvinQty;

var
  K2: TSquareKelvinUnit;

const
  rsSquareKelvinSymbol     = '%sK2';
  rsSquareKelvinName       = 'square %skelvin';
  rsSquareKelvinPluralName = 'square %skelvins';

const
  cSquareKelvinPrefixes  : TPrefixes  = (pNone);
  cSquareKelvinExponents : TExponents = (2);

{ Quantity of TSquareMetersPerSquareSecond }

type
  TSquareMetersPerSquareSecond = TSquareMeterPerSquareSecondQty;

const
  rsSquareMeterPerSquareSecondSymbol     = '%sm2/%ss2';
  rsSquareMeterPerSquareSecondName       = 'square %smeter per square %ssecond';
  rsSquareMeterPerSquareSecondPluralName = 'square %smeters per square %ssecond';

const
  cSquareMeterPerSquareSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareMeterPerSquareSecondExponents : TExponents = (2, -2);

{ Quantity of TSeconds }

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

const
  rsSecondSymbol     = '%ss';
  rsSecondName       = '%ssecond';
  rsSecondPluralName = '%sseconds';

const
  cSecondPrefixes  : TPrefixes  = (pNone);
  cSecondExponents : TExponents = (1);

{ Quantity of TDays }

type
  TDays = TSecondQty;

const
  day: TSecondQty = (FValue: 86400);

const
  rsDaySymbol     = 'd';
  rsDayName       = 'day';
  rsDayPluralName = 'days';

const
  cDayPrefixes  : TPrefixes  = ();
  cDayExponents : TExponents = ();
  cDayFactor                 = 86400;

{ Quantity of THours }

type
  THours = TSecondQty;

const
  hr: TSecondQty = (FValue: 3600);

const
  rsHourSymbol     = 'h';
  rsHourName       = 'hour';
  rsHourPluralName = 'hours';

const
  cHourPrefixes  : TPrefixes  = ();
  cHourExponents : TExponents = ();
  cHourFactor                 = 3600;

{ Quantity of TMinutes }

type
  TMinutes = TSecondQty;

const
  minute: TSecondQty = (FValue: 60);

const
  rsMinuteSymbol     = 'min';
  rsMinuteName       = 'minute';
  rsMinutePluralName = 'minutes';

const
  cMinutePrefixes  : TPrefixes  = ();
  cMinuteExponents : TExponents = ();
  cMinuteFactor                 = 60;

{ Quantity of TSquareSeconds }

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

const
  rsSquareSecondSymbol     = '%ss2';
  rsSquareSecondName       = 'square %ssecond';
  rsSquareSecondPluralName = 'square %sseconds';

const
  cSquareSecondPrefixes  : TPrefixes  = (pNone);
  cSquareSecondExponents : TExponents = (2);

{ Quantity of TSquareDays }

type
  TSquareDays = TSquareSecondQty;

const
  day2: TSquareSecondQty = (FValue: 7464960000);

const
  rsSquareDaySymbol     = 'd2';
  rsSquareDayName       = 'square day';
  rsSquareDayPluralName = 'square days';

const
  cSquareDayPrefixes  : TPrefixes  = ();
  cSquareDayExponents : TExponents = ();
  cSquareDayFactor                 = 7464960000;

{ Quantity of TSquareHours }

type
  TSquareHours = TSquareSecondQty;

const
  hr2: TSquareSecondQty = (FValue: 12960000);

const
  rsSquareHourSymbol     = 'h2';
  rsSquareHourName       = 'square hour';
  rsSquareHourPluralName = 'square hours';

const
  cSquareHourPrefixes  : TPrefixes  = ();
  cSquareHourExponents : TExponents = ();
  cSquareHourFactor                 = 12960000;

{ Quantity of TSquareMinutes }

type
  TSquareMinutes = TSquareSecondQty;

const
  minute2: TSquareSecondQty = (FValue: 3600);

const
  rsSquareMinuteSymbol     = 'min2';
  rsSquareMinuteName       = 'square minute';
  rsSquareMinutePluralName = 'square minutes';

const
  cSquareMinutePrefixes  : TPrefixes  = ();
  cSquareMinuteExponents : TExponents = ();
  cSquareMinuteFactor                 = 3600;

{ Quantity of TAstronomical }

type
  TAstronomical = TMeterQty;

const
  au: TMeterQty = (FValue: 149597870691);

const
  rsAstronomicalSymbol     = 'au';
  rsAstronomicalName       = 'astronomical unit';
  rsAstronomicalPluralName = 'astronomical units';

const
  cAstronomicalPrefixes  : TPrefixes  = ();
  cAstronomicalExponents : TExponents = ();
  cAstronomicalFactor                 = 149597870691;

{ Quantity of TInches }

type
  TInches = TMeterQty;

const
  inch: TMeterQty = (FValue: 0.0254);

const
  rsInchSymbol     = 'in';
  rsInchName       = 'inch';
  rsInchPluralName = 'inches';

const
  cInchPrefixes  : TPrefixes  = ();
  cInchExponents : TExponents = ();
  cInchFactor                 = 0.0254;

{ Quantity of TFeet }

type
  TFeet = TMeterQty;

const
  ft: TMeterQty = (FValue: 0.3048);

const
  rsFootSymbol     = 'ft';
  rsFootName       = 'foot';
  rsFootPluralName = 'feet';

const
  cFootPrefixes  : TPrefixes  = ();
  cFootExponents : TExponents = ();
  cFootFactor                 = 0.3048;

{ Quantity of TYards }

type
  TYards = TMeterQty;

const
  yd: TMeterQty = (FValue: 0.9144);

const
  rsYardSymbol     = 'yd';
  rsYardName       = 'yard';
  rsYardPluralName = 'yards';

const
  cYardPrefixes  : TPrefixes  = ();
  cYardExponents : TExponents = ();
  cYardFactor                 = 0.9144;

{ Quantity of TMiles }

type
  TMiles = TMeterQty;

const
  mi: TMeterQty = (FValue: 1609.344);

const
  rsMileSymbol     = 'mi';
  rsMileName       = 'mile';
  rsMilePluralName = 'miles';

const
  cMilePrefixes  : TPrefixes  = ();
  cMileExponents : TExponents = ();
  cMileFactor                 = 1609.344;

{ Quantity of TNauticalMiles }

type
  TNauticalMiles = TMeterQty;

const
  nmi: TMeterQty = (FValue: 1852);

const
  rsNauticalMileSymbol     = 'nmi';
  rsNauticalMileName       = 'nautical mile';
  rsNauticalMilePluralName = 'nautical miles';

const
  cNauticalMilePrefixes  : TPrefixes  = ();
  cNauticalMileExponents : TExponents = ();
  cNauticalMileFactor                 = 1852;

{ Quantity of TAngstroms }

type
  TAngstroms = TMeterQty;

const
  angstrom: TMeterQty = (FValue: 1E-10);

const
  rsAngstromSymbol     = '%sÅ';
  rsAngstromName       = '%sangstrom';
  rsAngstromPluralName = '%sangstroms';

const
  cAngstromPrefixes  : TPrefixes  = (pNone);
  cAngstromExponents : TExponents = (1);
  cAngstromFactor                 = 1E-10;

{ Quantity of TSquareMeters }

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

const
  rsSquareMeterSymbol     = '%sm2';
  rsSquareMeterName       = 'square %smeter';
  rsSquareMeterPluralName = 'square %smeters';

const
  cSquareMeterPrefixes  : TPrefixes  = (pNone);
  cSquareMeterExponents : TExponents = (2);

{ Quantity of TSquareInches }

type
  TSquareInches = TSquareMeterQty;

const
  inch2: TSquareMeterQty = (FValue: 0.00064516);

const
  rsSquareInchSymbol     = 'in2';
  rsSquareInchName       = 'square inch';
  rsSquareInchPluralName = 'square inches';

const
  cSquareInchPrefixes  : TPrefixes  = ();
  cSquareInchExponents : TExponents = ();
  cSquareInchFactor                 = 0.00064516;

{ Quantity of TSquareFeet }

type
  TSquareFeet = TSquareMeterQty;

const
  ft2: TSquareMeterQty = (FValue: 0.09290304);

const
  rsSquareFootSymbol     = 'ft2';
  rsSquareFootName       = 'square foot';
  rsSquareFootPluralName = 'square feet';

const
  cSquareFootPrefixes  : TPrefixes  = ();
  cSquareFootExponents : TExponents = ();
  cSquareFootFactor                 = 0.09290304;

{ Quantity of TSquareYards }

type
  TSquareYards = TSquareMeterQty;

const
  yd2: TSquareMeterQty = (FValue: 0.83612736);

const
  rsSquareYardSymbol     = 'yd2';
  rsSquareYardName       = 'square yard';
  rsSquareYardPluralName = 'square yards';

const
  cSquareYardPrefixes  : TPrefixes  = ();
  cSquareYardExponents : TExponents = ();
  cSquareYardFactor                 = 0.83612736;

{ Quantity of TSquareMiles }

type
  TSquareMiles = TSquareMeterQty;

const
  mi2: TSquareMeterQty = (FValue: 2589988.110336);

const
  rsSquareMileSymbol     = 'mi2';
  rsSquareMileName       = 'square mile';
  rsSquareMilePluralName = 'square miles';

const
  cSquareMilePrefixes  : TPrefixes  = ();
  cSquareMileExponents : TExponents = ();
  cSquareMileFactor                 = 2589988.110336;

{ Quantity of TCubicMeters }

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

const
  rsCubicMeterSymbol     = '%sm3';
  rsCubicMeterName       = 'cubic %smeter';
  rsCubicMeterPluralName = 'cubic %smeters';

const
  cCubicMeterPrefixes  : TPrefixes  = (pNone);
  cCubicMeterExponents : TExponents = (3);

{ Quantity of TCubicInches }

type
  TCubicInches = TCubicMeterQty;

const
  inch3: TCubicMeterQty = (FValue: 0.000016387064);

const
  rsCubicInchSymbol     = 'in3';
  rsCubicInchName       = 'cubic inch';
  rsCubicInchPluralName = 'cubic inches';

const
  cCubicInchPrefixes  : TPrefixes  = ();
  cCubicInchExponents : TExponents = ();
  cCubicInchFactor                 = 0.000016387064;

{ Quantity of TCubicFeet }

type
  TCubicFeet = TCubicMeterQty;

const
  ft3: TCubicMeterQty = (FValue: 0.028316846592);

const
  rsCubicFootSymbol     = 'ft3';
  rsCubicFootName       = 'cubic foot';
  rsCubicFootPluralName = 'cubic feet';

const
  cCubicFootPrefixes  : TPrefixes  = ();
  cCubicFootExponents : TExponents = ();
  cCubicFootFactor                 = 0.028316846592;

{ Quantity of TCubicYards }

type
  TCubicYards = TCubicMeterQty;

const
  yd3: TCubicMeterQty = (FValue: 0.764554857984);

const
  rsCubicYardSymbol     = 'yd3';
  rsCubicYardName       = 'cubic yard';
  rsCubicYardPluralName = 'cubic yards';

const
  cCubicYardPrefixes  : TPrefixes  = ();
  cCubicYardExponents : TExponents = ();
  cCubicYardFactor                 = 0.764554857984;

{ Quantity of TLitres }

type
  TLitres = TCubicMeterQty;

const
  L: TCubicMeterQty = (FValue: 1E-03);

const
  dL: TCubicMeterQty = (FValue: 1E-03 * 1E-01);
  cL: TCubicMeterQty = (FValue: 1E-03 * 1E-02);
  mL: TCubicMeterQty = (FValue: 1E-03 * 1E-03);

const
  rsLitreSymbol     = '%sL';
  rsLitreName       = '%slitre';
  rsLitrePluralName = '%slitres';

const
  cLitrePrefixes  : TPrefixes  = (pNone);
  cLitreExponents : TExponents = (1);
  cLitreFactor                 = 1E-03;

{ Quantity of TGallons }

type
  TGallons = TCubicMeterQty;

const
  gal: TCubicMeterQty = (FValue: 0.0037854119678);

const
  rsGallonSymbol     = 'gal';
  rsGallonName       = 'gallon';
  rsGallonPluralName = 'gallons';

const
  cGallonPrefixes  : TPrefixes  = ();
  cGallonExponents : TExponents = ();
  cGallonFactor                 = 0.0037854119678;

{ Quantity of TQuarticMeters }

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

const
  rsQuarticMeterSymbol     = '%sm4';
  rsQuarticMeterName       = 'quartic %smeter';
  rsQuarticMeterPluralName = 'quartic %smeters';

const
  cQuarticMeterPrefixes  : TPrefixes  = (pNone);
  cQuarticMeterExponents : TExponents = (4);

{ Quantity of TQuinticMeters }

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

const
  rsQuinticMeterSymbol     = '%sm5';
  rsQuinticMeterName       = 'quintic %smeter';
  rsQuinticMeterPluralName = 'quintic %smeters';

const
  cQuinticMeterPrefixes  : TPrefixes  = (pNone);
  cQuinticMeterExponents : TExponents = (5);

{ Quantity of TSexticMeters }

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

const
  rsSexticMeterSymbol     = '%sm6';
  rsSexticMeterName       = 'sextic %smeter';
  rsSexticMeterPluralName = 'sextic %smeters';

const
  cSexticMeterPrefixes  : TPrefixes  = (pNone);
  cSexticMeterExponents : TExponents = (6);

{ Quantity of TKilograms }

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

const
  rsKilogramSymbol     = '%sg';
  rsKilogramName       = '%sgram';
  rsKilogramPluralName = '%sgrams';

const
  cKilogramPrefixes  : TPrefixes  = (pKilo);
  cKilogramExponents : TExponents = (1);

{ Quantity of TTonnes }

type
  TTonnes = TKilogramQty;

const
  tonne: TKilogramQty = (FValue: 1E+03);

const
  gigatonne: TKilogramQty = (FValue: 1E+03 * 1E+09);
  megatonne: TKilogramQty = (FValue: 1E+03 * 1E+06);
  kilotonne: TKilogramQty = (FValue: 1E+03 * 1E+03);

const
  rsTonneSymbol     = '%st';
  rsTonneName       = '%stonne';
  rsTonnePluralName = '%stonnes';

const
  cTonnePrefixes  : TPrefixes  = (pNone);
  cTonneExponents : TExponents = (1);
  cTonneFactor                 = 1E+03;

{ Quantity of TPounds }

type
  TPounds = TKilogramQty;

const
  lb: TKilogramQty = (FValue: 0.45359237);

const
  rsPoundSymbol     = 'lb';
  rsPoundName       = 'pound';
  rsPoundPluralName = 'pounds';

const
  cPoundPrefixes  : TPrefixes  = ();
  cPoundExponents : TExponents = ();
  cPoundFactor                 = 0.45359237;

{ Quantity of TOunces }

type
  TOunces = TKilogramQty;

const
  oz: TKilogramQty = (FValue: 0.028349523125);

const
  rsOunceSymbol     = 'oz';
  rsOunceName       = 'ounce';
  rsOuncePluralName = 'ounces';

const
  cOuncePrefixes  : TPrefixes  = ();
  cOunceExponents : TExponents = ();
  cOunceFactor                 = 0.028349523125;

{ Quantity of TStones }

type
  TStones = TKilogramQty;

const
  st: TKilogramQty = (FValue: 6.35029318);

const
  rsStoneSymbol     = 'st';
  rsStoneName       = 'stone';
  rsStonePluralName = 'stones';

const
  cStonePrefixes  : TPrefixes  = ();
  cStoneExponents : TExponents = ();
  cStoneFactor                 = 6.35029318;

{ Quantity of TTons }

type
  TTons = TKilogramQty;

const
  ton: TKilogramQty = (FValue: 907.18474);

const
  rsTonSymbol     = 'ton';
  rsTonName       = 'ton';
  rsTonPluralName = 'tons';

const
  cTonPrefixes  : TPrefixes  = ();
  cTonExponents : TExponents = ();
  cTonFactor                 = 907.18474;

{ Quantity of TSquareKilograms }

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

const
  rsSquareKilogramSymbol     = '%sg2';
  rsSquareKilogramName       = 'square %sgram';
  rsSquareKilogramPluralName = 'square %sgrams';

const
  cSquareKilogramPrefixes  : TPrefixes  = (pKilo);
  cSquareKilogramExponents : TExponents = (2);

{ Quantity of TAmperes }

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

const
  rsAmpereSymbol     = '%sA';
  rsAmpereName       = '%sampere';
  rsAmperePluralName = '%samperes';

const
  cAmperePrefixes  : TPrefixes  = (pNone);
  cAmpereExponents : TExponents = (1);

{ Quantity of TSquareAmperes }

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

const
  rsSquareAmpereSymbol     = '%sA2';
  rsSquareAmpereName       = 'square %sampere';
  rsSquareAmperePluralName = 'square %samperes';

const
  cSquareAmperePrefixes  : TPrefixes  = (pNone);
  cSquareAmpereExponents : TExponents = (2);

{ Quantity of TKelvins }

type
  TKelvins = TKelvinQty;

var
  K: TKelvinUnit;

const
  rsKelvinSymbol     = '%sK';
  rsKelvinName       = '%skelvin';
  rsKelvinPluralName = '%skelvins';

const
  cKelvinPrefixes  : TPrefixes  = (pNone);
  cKelvinExponents : TExponents = (1);

{ Quantity of TDegreesCelsius }

type
  TDegreesCelsius = TKelvinQty;

var
  degC: TDegreeCelsiusUnit;

const
  rsDegreeCelsiusSymbol     = 'ºC';
  rsDegreeCelsiusName       = 'degree Celsius';
  rsDegreeCelsiusPluralName = 'degrees Celsius';

const
  cDegreeCelsiusPrefixes  : TPrefixes  = ();
  cDegreeCelsiusExponents : TExponents = ();

{ Quantity of TDegreesFahrenheit }

type
  TDegreesFahrenheit = TKelvinQty;

var
  degF: TDegreeFahrenheitUnit;

const
  rsDegreeFahrenheitSymbol     = 'ºF';
  rsDegreeFahrenheitName       = 'degree Fahrenheit';
  rsDegreeFahrenheitPluralName = 'degrees Fahrenheit';

const
  cDegreeFahrenheitPrefixes  : TPrefixes  = ();
  cDegreeFahrenheitExponents : TExponents = ();

{ Quantity of TQuarticKelvins }

type
  TQuarticKelvins = TQuarticKelvinQty;

var
  K4: TQuarticKelvinUnit;

const
  rsQuarticKelvinSymbol     = '%sK4';
  rsQuarticKelvinName       = 'quartic %skelvin';
  rsQuarticKelvinPluralName = 'quartic %skelvins';

const
  cQuarticKelvinPrefixes  : TPrefixes  = (pNone);
  cQuarticKelvinExponents : TExponents = (4);

{ Quantity of TMoles }

type
  TMoles = TMoleQty;

var
  mol: TMoleUnit;

const
   kmol: TMoleQty = (FValue: 1E+03);
   hmol: TMoleQty = (FValue: 1E+02);
  damol: TMoleQty = (FValue: 1E+01);

const
  rsMoleSymbol     = '%smol';
  rsMoleName       = '%smole';
  rsMolePluralName = '%smoles';

const
  cMolePrefixes  : TPrefixes  = (pNone);
  cMoleExponents : TExponents = (1);

{ Quantity of TCandelas }

type
  TCandelas = TCandelaQty;

var
  cd: TCandelaUnit;

const
  rsCandelaSymbol     = '%scd';
  rsCandelaName       = '%scandela';
  rsCandelaPluralName = '%scandelas';

const
  cCandelaPrefixes  : TPrefixes  = (pNone);
  cCandelaExponents : TExponents = (1);

{ Quantity of TRadians }

type
  TRadians = TRadianQty;

var
  rad: TRadianUnit;

const
  rsRadianSymbol     = 'rad';
  rsRadianName       = 'radian';
  rsRadianPluralName = 'radians';

const
  cRadianPrefixes  : TPrefixes  = ();
  cRadianExponents : TExponents = ();

{ Quantity of TDegrees }

type
  TDegrees = TRadianQty;

const
  deg: TRadianQty = (FValue: Pi/180);

const
  rsDegreeSymbol     = 'deg';
  rsDegreeName       = 'degree';
  rsDegreePluralName = 'degrees';

const
  cDegreePrefixes  : TPrefixes  = ();
  cDegreeExponents : TExponents = ();
  cDegreeFactor                 = Pi/180;

{ Quantity of TSteradians }

type
  TSteradians = TSteradianQty;

var
  sr: TSteradianUnit;

const
  rsSteradianSymbol     = 'sr';
  rsSteradianName       = 'steradian';
  rsSteradianPluralName = 'steradians';

const
  cSteradianPrefixes  : TPrefixes  = ();
  cSteradianExponents : TExponents = ();

{ Quantity of TSquareDegrees }

type
  TSquareDegrees = TSteradianQty;

const
  deg2: TSteradianQty = (FValue: Pi*Pi/32400);

const
  rsSquareDegreeSymbol     = 'deg2';
  rsSquareDegreeName       = 'square degree';
  rsSquareDegreePluralName = 'square degrees';

const
  cSquareDegreePrefixes  : TPrefixes  = ();
  cSquareDegreeExponents : TExponents = ();
  cSquareDegreeFactor                 = Pi*Pi/32400;

{ Quantity of TSquareHertz }

type
  TSquareHertz = TSquareHertzQty;

var
  Hz2: TSquareHertzUnit;

const
  THz2: TSquareHertzQty = (FValue: 1E+24);
  GHz2: TSquareHertzQty = (FValue: 1E+18);
  MHz2: TSquareHertzQty = (FValue: 1E+12);
  kHz2: TSquareHertzQty = (FValue: 1E+06);

const
  rsSquareHertzSymbol     = '%sHz2';
  rsSquareHertzName       = 'square %shertz';
  rsSquareHertzPluralName = 'square %shertz';

const
  cSquareHertzPrefixes  : TPrefixes  = (pNone);
  cSquareHertzExponents : TExponents = (2);

{ Quantity of TRadiansPerSecond }

type
  TRadiansPerSecond = TRadianPerSecondQty;

const
  rsRadianPerSecondSymbol     = 'rad/%ss';
  rsRadianPerSecondName       = 'radian per %ssecond';
  rsRadianPerSecondPluralName = 'radians per %ssecond';

const
  cRadianPerSecondPrefixes  : TPrefixes  = (pNone);
  cRadianPerSecondExponents : TExponents = (-1);

{ Quantity of TRadiansPerSquareSecond }

type
  TRadiansPerSquareSecond = TRadianPerSquareSecondQty;

const
  rsRadianPerSquareSecondSymbol     = 'rad/%ss2';
  rsRadianPerSquareSecondName       = 'radian per %ssecond squared';
  rsRadianPerSquareSecondPluralName = 'radians per %ssecond squared';

const
  cRadianPerSquareSecondPrefixes  : TPrefixes  = (pNone);
  cRadianPerSquareSecondExponents : TExponents = (-2);

{ Quantity of TSteradiansPerSquareSecond }

type
  TSteradiansPerSquareSecond = TSteradianPerSquareSecondQty;

const
  rsSteradianPerSquareSecondSymbol     = 'sr/%ss2';
  rsSteradianPerSquareSecondName       = 'steradian per square %ssecond';
  rsSteradianPerSquareSecondPluralName = 'steradians per square %ssecond';

const
  cSteradianPerSquareSecondPrefixes  : TPrefixes  = (pNone);
  cSteradianPerSquareSecondExponents : TExponents = (-2);

{ Quantity of TMetersPerSecond }

type
  TMetersPerSecond = TMeterPerSecondQty;

const
  rsMeterPerSecondSymbol     = '%sm/%ss';
  rsMeterPerSecondName       = '%smeter per %ssecond';
  rsMeterPerSecondPluralName = '%smeters per %ssecond';

const
  cMeterPerSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cMeterPerSecondExponents : TExponents = (1, -1);

{ Quantity of TMetersPerHour }

type
  TMetersPerHour = TMeterPerSecondQty;

const
  rsMeterPerHourSymbol     = '%sm/h';
  rsMeterPerHourName       = '%smeter per hour';
  rsMeterPerHourPluralName = '%smeters per hour';

const
  cMeterPerHourPrefixes  : TPrefixes  = (pNone);
  cMeterPerHourExponents : TExponents = (1);
  cMeterPerHourFactor                 = 1/3600;

{ Quantity of TMilesPerHour }

type
  TMilesPerHour = TMeterPerSecondQty;

const
  rsMilePerHourSymbol     = 'mi/h';
  rsMilePerHourName       = 'mile per hour';
  rsMilePerHourPluralName = 'miles per hour';

const
  cMilePerHourPrefixes  : TPrefixes  = ();
  cMilePerHourExponents : TExponents = ();
  cMilePerHourFactor                 = 0.44704;

{ Quantity of TNauticalMilesPerHour }

type
  TNauticalMilesPerHour = TMeterPerSecondQty;

const
  rsNauticalMilePerHourSymbol     = 'nmi/h';
  rsNauticalMilePerHourName       = 'nautical mile per hour';
  rsNauticalMilePerHourPluralName = 'nautical miles per hour';

const
  cNauticalMilePerHourPrefixes  : TPrefixes  = ();
  cNauticalMilePerHourExponents : TExponents = ();
  cNauticalMilePerHourFactor                 = 463/900;

{ Quantity of TMetersPerSquareSecond }

type
  TMetersPerSquareSecond = TMeterPerSquareSecondQty;

const
  rsMeterPerSquareSecondSymbol     = '%sm/%ss2';
  rsMeterPerSquareSecondName       = '%smeter per %ssecond squared';
  rsMeterPerSquareSecondPluralName = '%smeters per %ssecond squared';

const
  cMeterPerSquareSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cMeterPerSquareSecondExponents : TExponents = (1, -2);

{ Quantity of TMetersPerSecondPerSecond }

type
  TMetersPerSecondPerSecond = TMeterPerSquareSecondQty;

const
  rsMeterPerSecondPerSecondSymbol     = '%sm/%ss/%ss';
  rsMeterPerSecondPerSecondName       = '%smeter per %ssecond per %ssecond';
  rsMeterPerSecondPerSecondPluralName = '%smeters per %ssecond per %ssecond';

const
  cMeterPerSecondPerSecondPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cMeterPerSecondPerSecondExponents : TExponents = (1, -1, -1);

{ Quantity of TMetersPerHourPerSecond }

type
  TMetersPerHourPerSecond = TMeterPerSquareSecondQty;

const
  rsMeterPerHourPerSecondSymbol     = '%sm/h/%ss';
  rsMeterPerHourPerSecondName       = '%smeter per hour per %ssecond';
  rsMeterPerHourPerSecondPluralName = '%smeters per hour per %ssecond';

const
  cMeterPerHourPerSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cMeterPerHourPerSecondExponents : TExponents = (1, -1);
  cMeterPerHourPerSecondFactor                 = 1/3600;

{ Quantity of TKilogramMeters }

type
  TKilogramMeters = TKilogramMeterQty;

const
  rsKilogramMeterSymbol     = '%sg·%sm';
  rsKilogramMeterName       = '%sgram %smeter';
  rsKilogramMeterPluralName = '%sgram %smeters';

const
  cKilogramMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramMeterExponents : TExponents = (1, 1);

{ Quantity of TKilogramsPerSecond }

type
  TKilogramsPerSecond = TKilogramPerSecondQty;

const
  rsKilogramPerSecondSymbol     = '%sg/%ss';
  rsKilogramPerSecondName       = '%sgram per %ssecond';
  rsKilogramPerSecondPluralName = '%sgrams per %ssecond';

const
  cKilogramPerSecondPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramPerSecondExponents : TExponents = (1, -1);

{ Quantity of TKilogramMetersPerSecond }

type
  TKilogramMetersPerSecond = TKilogramMeterPerSecondQty;

const
  rsKilogramMeterPerSecondSymbol     = '%sg·%sm/%ss';
  rsKilogramMeterPerSecondName       = '%sgram %smeter per %ssecond';
  rsKilogramMeterPerSecondPluralName = '%sgram %smeters per %ssecond';

const
  cKilogramMeterPerSecondPrefixes  : TPrefixes  = (pKilo, pNone, pNone);
  cKilogramMeterPerSecondExponents : TExponents = (1, 1, -1);

{ Quantity of TSquareKilogramSquareMetersPerSquareSecond }

type
  TSquareKilogramSquareMetersPerSquareSecond = TSquareKilogramSquareMeterPerSquareSecondQty;

const
  rsSquareKilogramSquareMeterPerSquareSecondSymbol     = '%sg2·%sm2/%ss2';
  rsSquareKilogramSquareMeterPerSquareSecondName       = 'square%sgram square%smeter per square%ssecond';
  rsSquareKilogramSquareMeterPerSquareSecondPluralName = 'square%sgram square%smeters per square%ssecond';

const
  cSquareKilogramSquareMeterPerSquareSecondPrefixes  : TPrefixes  = (pKilo, pNone, pNone);
  cSquareKilogramSquareMeterPerSquareSecondExponents : TExponents = (2, 2, -2);

{ Quantity of TNewtonSeconds }

type
  TNewtonSeconds = TKilogramMeterPerSecondQty;

const
  rsNewtonSecondSymbol     = '%sN·%ss';
  rsNewtonSecondName       = '%snewton %ssecond';
  rsNewtonSecondPluralName = '%snewton %sseconds';

const
  cNewtonSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonSecondExponents : TExponents = (1, 1);

{ Quantity of TReciprocalMeters }

type
  TReciprocalMeters = TReciprocalMeterQty;

const
  rsReciprocalMeterSymbol     = '1/%sm';
  rsReciprocalMeterName       = 'reciprocal %smeter';
  rsReciprocalMeterPluralName = 'reciprocal %smeters';

const
  cReciprocalMeterPrefixes  : TPrefixes  = (pNone);
  cReciprocalMeterExponents : TExponents = (-1);

{ Quantity of TKilogramSquareMeters }

type
  TKilogramSquareMeters = TKilogramSquareMeterQty;

const
  rsKilogramSquareMeterSymbol     = '%sg·%sm2';
  rsKilogramSquareMeterName       = '%sgram square %smeter';
  rsKilogramSquareMeterPluralName = '%sgram square %smeters';

const
  cKilogramSquareMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramSquareMeterExponents : TExponents = (1, 2);

{ Quantity of TKilogramSquareMetersPerSecond }

type
  TKilogramSquareMetersPerSecond = TKilogramSquareMeterPerSecondQty;

const
  rsKilogramSquareMeterPerSecondSymbol     = '%sg·%sm2/%ss';
  rsKilogramSquareMeterPerSecondName       = '%sgram square %smeter per %ssecond';
  rsKilogramSquareMeterPerSecondPluralName = '%sgram square %smeters per %ssecond';

const
  cKilogramSquareMeterPerSecondPrefixes  : TPrefixes  = (pKilo, pNone, pNone);
  cKilogramSquareMeterPerSecondExponents : TExponents = (1, 2, -1);

{ Quantity of TNewtonMeterSeconds }

type
  TNewtonMeterSeconds = TKilogramSquareMeterPerSecondQty;

const
  rsNewtonMeterSecondSymbol     = '%sN·%sm·%ss';
  rsNewtonMeterSecondName       = '%snewton %smeter %ssecond';
  rsNewtonMeterSecondPluralName = '%snewton %smeter %sseconds';

const
  cNewtonMeterSecondPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cNewtonMeterSecondExponents : TExponents = (1, 1, 1);

{ Quantity of TSecondsPerMeter }

type
  TSecondsPerMeter = TSecondPerMeterQty;

const
  rsSecondPerMeterSymbol     = '%ss/%sm';
  rsSecondPerMeterName       = '%ssecond per %smeter';
  rsSecondPerMeterPluralName = '%sseconds per %smeter';

const
  cSecondPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cSecondPerMeterExponents : TExponents = (1, -1);

{ Quantity of TKilogramsPerMeter }

type
  TKilogramsPerMeter = TKilogramPerMeterQty;

const
  rsKilogramPerMeterSymbol     = '%sg/%sm';
  rsKilogramPerMeterName       = '%sgram per %smeter';
  rsKilogramPerMeterPluralName = '%sgrams per %smeter';

const
  cKilogramPerMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramPerMeterExponents : TExponents = (1, -1);

{ Quantity of TKilogramsPerSquareMeter }

type
  TKilogramsPerSquareMeter = TKilogramPerSquareMeterQty;

const
  rsKilogramPerSquareMeterSymbol     = '%sg/%sm2';
  rsKilogramPerSquareMeterName       = '%sgram per square %smeter';
  rsKilogramPerSquareMeterPluralName = '%sgrams per square %smeter';

const
  cKilogramPerSquareMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramPerSquareMeterExponents : TExponents = (1, -2);

{ Quantity of TKilogramsPerCubicMeter }

type
  TKilogramsPerCubicMeter = TKilogramPerCubicMeterQty;

const
  rsKilogramPerCubicMeterSymbol     = '%sg/%sm3';
  rsKilogramPerCubicMeterName       = '%sgram per cubic %smeter';
  rsKilogramPerCubicMeterPluralName = '%sgrams per cubic %smeter';

const
  cKilogramPerCubicMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramPerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TPoundsPerCubicInch }

type
  TPoundsPerCubicInch = TKilogramPerCubicMeterQty;

const
  rsPoundPerCubicInchSymbol     = 'lb/in3';
  rsPoundPerCubicInchName       = 'pound per cubic inch';
  rsPoundPerCubicInchPluralName = 'pounds per cubic inch';

const
  cPoundPerCubicInchPrefixes  : TPrefixes  = ();
  cPoundPerCubicInchExponents : TExponents = ();
  cPoundPerCubicInchFactor                 = 27679.9047102031;

{ Quantity of TNewtons }

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

const
  rsNewtonSymbol     = '%sN';
  rsNewtonName       = '%snewton';
  rsNewtonPluralName = '%snewtons';

const
  cNewtonPrefixes  : TPrefixes  = (pNone);
  cNewtonExponents : TExponents = (1);

{ Quantity of TPoundsForce }

type
  TPoundsForce = TNewtonQty;

const
  lbf: TNewtonQty = (FValue: 4.4482216152605);

const
  rsPoundForceSymbol     = 'lbf';
  rsPoundForceName       = 'pound-force';
  rsPoundForcePluralName = 'pounds-force';

const
  cPoundForcePrefixes  : TPrefixes  = ();
  cPoundForceExponents : TExponents = ();
  cPoundForceFactor                 = 4.4482216152605;

{ Quantity of TSquareNewtons }

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

const
  rsSquareNewtonSymbol     = '%sN2';
  rsSquareNewtonName       = 'square %snewton';
  rsSquareNewtonPluralName = 'square %snewtons';

const
  cSquareNewtonPrefixes  : TPrefixes  = (pNone);
  cSquareNewtonExponents : TExponents = (2);

{ Quantity of TPascals }

type
  TPascals = TPascalQty;

var
  Pa: TPascalUnit;

const
  TPa: TPascalQty = (FValue: 1E+12);
  GPa: TPascalQty = (FValue: 1E+09);
  MPa: TPascalQty = (FValue: 1E+06);
  kPa: TPascalQty = (FValue: 1E+03);

const
  rsPascalSymbol     = '%sPa';
  rsPascalName       = '%spascal';
  rsPascalPluralName = '%spascals';

const
  cPascalPrefixes  : TPrefixes  = (pNone);
  cPascalExponents : TExponents = (1);

{ Quantity of TBars }

type
  TBars = TPascalQty;

const
  bar: TPascalQty = (FValue: 1E+05);

const
  kbar: TPascalQty = (FValue: 1E+05 * 1E+03);
  mbar: TPascalQty = (FValue: 1E+05 * 1E-03);

const
  rsBarSymbol     = '%sbar';
  rsBarName       = '%sbar';
  rsBarPluralName = '%sbars';

const
  cBarPrefixes  : TPrefixes  = (pNone);
  cBarExponents : TExponents = (1);
  cBarFactor                 = 1E+05;

{ Quantity of TPoundsPerSquareInch }

type
  TPoundsPerSquareInch = TPascalQty;

const
  psi: TPascalQty = (FValue: 6894.75729316836);

const
  kpsi: TPascalQty = (FValue: 6894.75729316836 * 1E+03);

const
  rsPoundPerSquareInchSymbol     = '%spsi';
  rsPoundPerSquareInchName       = '%spound per square inch';
  rsPoundPerSquareInchPluralName = '%spounds per square inch';

const
  cPoundPerSquareInchPrefixes  : TPrefixes  = (pNone);
  cPoundPerSquareInchExponents : TExponents = (1);
  cPoundPerSquareInchFactor                 = 6894.75729316836;

{ Quantity of TJoulesPerCubicMeter }

type
  TJoulesPerCubicMeter = TPascalQty;

const
  rsJoulePerCubicMeterSymbol     = '%sJ/%sm3';
  rsJoulePerCubicMeterName       = '%sjoule per cubic %smeter';
  rsJoulePerCubicMeterPluralName = '%sjoules per cubic %smeter';

const
  cJoulePerCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cJoulePerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TJoules }

type
  TJoules = TJouleQty;

var
  J: TJouleUnit;

const
  TJ: TJouleQty = (FValue: 1E+12);
  GJ: TJouleQty = (FValue: 1E+09);
  MJ: TJouleQty = (FValue: 1E+06);
  kJ: TJouleQty = (FValue: 1E+03);

const
  rsJouleSymbol     = '%sJ';
  rsJouleName       = '%sjoule';
  rsJoulePluralName = '%sjoules';

const
  cJoulePrefixes  : TPrefixes  = (pNone);
  cJouleExponents : TExponents = (1);

{ Quantity of TWattHours }

type
  TWattHours = TJouleQty;

const
  rsWattHourSymbol     = '%sW·h';
  rsWattHourName       = '%swatt hour';
  rsWattHourPluralName = '%swatt hours';

const
  cWattHourPrefixes  : TPrefixes  = (pNone);
  cWattHourExponents : TExponents = (1);
  cWattHourFactor                 = 3600;

{ Quantity of TElettronvolts }

type
  TElettronvolts = TJouleQty;

const
  eV: TJouleQty = (FValue: 1.60217742320523E-019);

const
  TeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+12);
  GeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+09);
  MeV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+06);
  keV: TJouleQty = (FValue: 1.60217742320523E-019 * 1E+03);

const
  rsElettronvoltSymbol     = '%seV';
  rsElettronvoltName       = '%selettronvolt';
  rsElettronvoltPluralName = '%selettronvolts';

const
  cElettronvoltPrefixes  : TPrefixes  = (pNone);
  cElettronvoltExponents : TExponents = (1);
  cElettronvoltFactor                 = 1.60217742320523E-019;

{ Quantity of TNewtonMeters }

type
  TNewtonMeters = TJouleQty;

const
  rsNewtonMeterSymbol     = '%sN·%sm';
  rsNewtonMeterName       = '%snewton %smeter';
  rsNewtonMeterPluralName = '%snewton %smeters';

const
  cNewtonMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonMeterExponents : TExponents = (1, 1);

{ Quantity of TPoundForceInches }

type
  TPoundForceInches = TJouleQty;

const
  rsPoundForceInchSymbol     = 'lbf·in';
  rsPoundForceInchName       = 'pound-force inch';
  rsPoundForceInchPluralName = 'pound-force inches';

const
  cPoundForceInchPrefixes  : TPrefixes  = ();
  cPoundForceInchExponents : TExponents = ();
  cPoundForceInchFactor                 = 0.112984829027617;

{ Quantity of TRydbergs }

type
  TRydbergs = TJouleQty;

const
  Ry: TJouleQty = (FValue: 2.179872361103542E-18);

const
  rsRydbergSymbol     = '%sRy';
  rsRydbergName       = '%srydberg';
  rsRydbergPluralName = '%srydbergs';

const
  cRydbergPrefixes  : TPrefixes  = (pNone);
  cRydbergExponents : TExponents = (1);
  cRydbergFactor                 = 2.179872361103542E-18;

{ Quantity of TCalories }

type
  TCalories = TJouleQty;

const
  cal: TJouleQty = (FValue: 4.184);

const
  Mcal: TJouleQty = (FValue: 4.184 * 1E+06);
  kcal: TJouleQty = (FValue: 4.184 * 1E+03);

const
  rsCalorieSymbol     = '%scal';
  rsCalorieName       = '%scalorie';
  rsCaloriePluralName = '%scalories';

const
  cCaloriePrefixes  : TPrefixes  = (pNone);
  cCalorieExponents : TExponents = (1);
  cCalorieFactor                 = 4.184;

{ Quantity of TJoulesPerRadian }

type
  TJoulesPerRadian = TJoulePerRadianQty;

const
  rsJoulePerRadianSymbol     = '%sJ/rad';
  rsJoulePerRadianName       = '%sjoule per radian';
  rsJoulePerRadianPluralName = '%sjoules per radian';

const
  cJoulePerRadianPrefixes  : TPrefixes  = (pNone);
  cJoulePerRadianExponents : TExponents = (1);

{ Quantity of TJoulesPerDegree }

type
  TJoulesPerDegree = TJoulePerRadianQty;

const
  rsJoulePerDegreeSymbol     = '%sJ/deg';
  rsJoulePerDegreeName       = '%sjoule per degree';
  rsJoulePerDegreePluralName = '%sjoules per degree';

const
  cJoulePerDegreePrefixes  : TPrefixes  = (pNone);
  cJoulePerDegreeExponents : TExponents = (1);
  cJoulePerDegreeFactor                 = 180/Pi;

{ Quantity of TNewtonMetersPerRadian }

type
  TNewtonMetersPerRadian = TJoulePerRadianQty;

const
  rsNewtonMeterPerRadianSymbol     = '%sN·%sm/rad';
  rsNewtonMeterPerRadianName       = '%snewton %smeter per radian';
  rsNewtonMeterPerRadianPluralName = '%snewton %smeters per radian';

const
  cNewtonMeterPerRadianPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonMeterPerRadianExponents : TExponents = (1, 1);

{ Quantity of TNewtonMetersPerDegree }

type
  TNewtonMetersPerDegree = TJoulePerRadianQty;

const
  rsNewtonMeterPerDegreeSymbol     = '%sN·%sm/deg';
  rsNewtonMeterPerDegreeName       = '%snewton %smeter per degree';
  rsNewtonMeterPerDegreePluralName = '%snewton %smeters per degree';

const
  cNewtonMeterPerDegreePrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonMeterPerDegreeExponents : TExponents = (1, 1);
  cNewtonMeterPerDegreeFactor                 = 180/Pi;

{ Quantity of TWatts }

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

const
  rsWattSymbol     = '%sW';
  rsWattName       = '%swatt';
  rsWattPluralName = '%swatts';

const
  cWattPrefixes  : TPrefixes  = (pNone);
  cWattExponents : TExponents = (1);

{ Quantity of TCoulombs }

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

const
  rsCoulombSymbol     = '%sC';
  rsCoulombName       = '%scoulomb';
  rsCoulombPluralName = '%scoulombs';

const
  cCoulombPrefixes  : TPrefixes  = (pNone);
  cCoulombExponents : TExponents = (1);

{ Quantity of TAmpereHours }

type
  TAmpereHours = TCoulombQty;

const
  rsAmpereHourSymbol     = '%sA·h';
  rsAmpereHourName       = '%sampere hour';
  rsAmpereHourPluralName = '%sampere hours';

const
  cAmpereHourPrefixes  : TPrefixes  = (pNone);
  cAmpereHourExponents : TExponents = (1);
  cAmpereHourFactor                 = 3600;

{ Quantity of TSquareCoulombs }

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

const
  rsSquareCoulombSymbol     = '%sC2';
  rsSquareCoulombName       = 'square %scoulomb';
  rsSquareCoulombPluralName = 'square %scoulombs';

const
  cSquareCoulombPrefixes  : TPrefixes  = (pNone);
  cSquareCoulombExponents : TExponents = (2);

{ Quantity of TVolts }

type
  TVolts = TVoltQty;

var
  V: TVoltUnit;

const
  kV: TVoltQty = (FValue: 1E+03);
  mV: TVoltQty = (FValue: 1E-03);

const
  rsVoltSymbol     = '%sV';
  rsVoltName       = '%svolt';
  rsVoltPluralName = '%svolts';

const
  cVoltPrefixes  : TPrefixes  = (pNone);
  cVoltExponents : TExponents = (1);

{ Quantity of TSquareVolts }

type
  TSquareVolts = TSquareVoltQty;

var
  V2: TSquareVoltUnit;

const
  kV2: TSquareVoltQty = (FValue: 1E+06);
  mV2: TSquareVoltQty = (FValue: 1E-06);

const
  rsSquareVoltSymbol     = '%sV2';
  rsSquareVoltName       = 'square %svolt';
  rsSquareVoltPluralName = 'square %svolts';

const
  cSquareVoltPrefixes  : TPrefixes  = (pNone);
  cSquareVoltExponents : TExponents = (2);

{ Quantity of TFarads }

type
  TFarads = TFaradQty;

var
  F: TFaradUnit;

const
   mF: TFaradQty = (FValue: 1E-03);
  miF: TFaradQty = (FValue: 1E-06);
   nF: TFaradQty = (FValue: 1E-09);
   pF: TFaradQty = (FValue: 1E-12);

const
  rsFaradSymbol     = '%sF';
  rsFaradName       = '%sfarad';
  rsFaradPluralName = '%sfarads';

const
  cFaradPrefixes  : TPrefixes  = (pNone);
  cFaradExponents : TExponents = (1);

{ Quantity of TOhms }

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

const
  rsOhmSymbol     = '%sΩ';
  rsOhmName       = '%sohm';
  rsOhmPluralName = '%sohms';

const
  cOhmPrefixes  : TPrefixes  = (pNone);
  cOhmExponents : TExponents = (1);

{ Quantity of TSiemens }

type
  TSiemens = TSiemensQty;

var
  siemens: TSiemensUnit;

const
  millisiemens: TSiemensQty = (FValue: 1E-03);
  microsiemens: TSiemensQty = (FValue: 1E-06);
   nanosiemens: TSiemensQty = (FValue: 1E-09);

const
  rsSiemensSymbol     = '%sS';
  rsSiemensName       = '%ssiemens';
  rsSiemensPluralName = '%ssiemens';

const
  cSiemensPrefixes  : TPrefixes  = (pNone);
  cSiemensExponents : TExponents = (1);

{ Quantity of TTeslas }

type
  TTeslas = TTeslaQty;

var
  T: TTeslaUnit;

const
   mT: TTeslaQty = (FValue: 1E-03);
  miT: TTeslaQty = (FValue: 1E-06);
   nT: TTeslaQty = (FValue: 1E-09);

const
  rsTeslaSymbol     = '%sT';
  rsTeslaName       = '%stesla';
  rsTeslaPluralName = '%steslas';

const
  cTeslaPrefixes  : TPrefixes  = (pNone);
  cTeslaExponents : TExponents = (1);

{ Quantity of TWebers }

type
  TWebers = TWeberQty;

var
  Wb: TWeberUnit;

const
  rsWeberSymbol     = '%sWb';
  rsWeberName       = '%sweber';
  rsWeberPluralName = '%swebers';

const
  cWeberPrefixes  : TPrefixes  = (pNone);
  cWeberExponents : TExponents = (1);

{ Quantity of THenries }

type
  THenries = THenryQty;

var
  H: THenryUnit;

const
   mH: THenryQty = (FValue: 1E-03);
  miH: THenryQty = (FValue: 1E-06);
   nH: THenryQty = (FValue: 1E-09);

const
  rsHenrySymbol     = '%sH';
  rsHenryName       = '%shenry';
  rsHenryPluralName = '%shenries';

const
  cHenryPrefixes  : TPrefixes  = (pNone);
  cHenryExponents : TExponents = (1);

{ Quantity of TLumens }

type
  TLumens = TLumenQty;

var
  lm: TLumenUnit;

const
  rsLumenSymbol     = '%slm';
  rsLumenName       = '%slumen';
  rsLumenPluralName = '%slumens';

const
  cLumenPrefixes  : TPrefixes  = (pNone);
  cLumenExponents : TExponents = (1);

{ Quantity of TLux }

type
  TLux = TLuxQty;

var
  lx: TLuxUnit;

const
  rsLuxSymbol     = '%slx';
  rsLuxName       = '%slux';
  rsLuxPluralName = '%slux';

const
  cLuxPrefixes  : TPrefixes  = (pNone);
  cLuxExponents : TExponents = (1);

{ Quantity of TBequerels }

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

const
  rsBequerelSymbol     = '%sBq';
  rsBequerelName       = '%sbequerel';
  rsBequerelPluralName = '%sbequerels';

const
  cBequerelPrefixes  : TPrefixes  = (pNone);
  cBequerelExponents : TExponents = (1);

{ Quantity of TGrays }

type
  TGrays = TSquareMeterPerSquareSecondQty;

var
  Gy: TSquareMeterPerSquareSecondUnit;

const
   kGy: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
   mGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
  miGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
   nGy: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

const
  rsGraySymbol     = '%sGy';
  rsGrayName       = '%sgray';
  rsGrayPluralName = '%sgrays';

const
  cGrayPrefixes  : TPrefixes  = (pNone);
  cGrayExponents : TExponents = (1);

{ Quantity of TSieverts }

type
  TSieverts = TSquareMeterPerSquareSecondQty;

var
  Sv: TSquareMeterPerSquareSecondUnit;

const
   kSv: TSquareMeterPerSquareSecondQty = (FValue: 1E+03);
   mSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-03);
  miSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-06);
   nSv: TSquareMeterPerSquareSecondQty = (FValue: 1E-09);

const
  rsSievertSymbol     = '%sSv';
  rsSievertName       = '%ssievert';
  rsSievertPluralName = '%ssieverts';

const
  cSievertPrefixes  : TPrefixes  = (pNone);
  cSievertExponents : TExponents = (1);

{ Quantity of TNewtonsPerCubicMeter }

type
  TNewtonsPerCubicMeter = TNewtonPerCubicMeterQty;

const
  rsNewtonPerCubicMeterSymbol     = '%sN/%sm3';
  rsNewtonPerCubicMeterName       = '%snewton per cubic %smeter';
  rsNewtonPerCubicMeterPluralName = '%snewtons per cubic %smeter';

const
  cNewtonPerCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TNewtonsPerMeter }

type
  TNewtonsPerMeter = TNewtonPerMeterQty;

const
  rsNewtonPerMeterSymbol     = '%sN/%sm';
  rsNewtonPerMeterName       = '%snewton per %smeter';
  rsNewtonPerMeterPluralName = '%snewtons per %smeter';

const
  cNewtonPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerMeterExponents : TExponents = (1, -1);

{ Quantity of TPoundsForcePerInch }

type
  TPoundsForcePerInch = TNewtonPerMeterQty;

const
  rsPoundForcePerInchSymbol     = 'lbf/in';
  rsPoundForcePerInchName       = 'pound-force per inch';
  rsPoundForcePerInchPluralName = 'pounds-force per inch';

const
  cPoundForcePerInchPrefixes  : TPrefixes  = ();
  cPoundForcePerInchExponents : TExponents = ();
  cPoundForcePerInchFactor                 = 175.126835246476;

{ Quantity of TCubicMetersPerSecond }

type
  TCubicMetersPerSecond = TCubicMeterPerSecondQty;

const
  rsCubicMeterPerSecondSymbol     = '%sm3/%ss';
  rsCubicMeterPerSecondName       = 'cubic %smeter per %ssecond';
  rsCubicMeterPerSecondPluralName = 'cubic %smeters per %ssecond';

const
  cCubicMeterPerSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cCubicMeterPerSecondExponents : TExponents = (3, -1);

{ Quantity of TPoiseuilles }

type
  TPoiseuilles = TPoiseuilleQty;

var
  Pl: TPoiseuilleUnit;

const
   cPl: TPoiseuilleQty = (FValue: 1E-02);
   mPl: TPoiseuilleQty = (FValue: 1E-03);
  miPl: TPoiseuilleQty = (FValue: 1E-06);

const
  rsPoiseuilleSymbol     = '%sPl';
  rsPoiseuilleName       = '%spoiseuille';
  rsPoiseuillePluralName = '%spoiseuilles';

const
  cPoiseuillePrefixes  : TPrefixes  = (pNone);
  cPoiseuilleExponents : TExponents = (1);

{ Quantity of TPascalSeconds }

type
  TPascalSeconds = TPoiseuilleQty;

const
  rsPascalSecondSymbol     = '%sPa·%ss';
  rsPascalSecondName       = '%spascal %ssecond';
  rsPascalSecondPluralName = '%spascal %sseconds';

const
  cPascalSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cPascalSecondExponents : TExponents = (1, 1);

{ Quantity of TSquareMetersPerSecond }

type
  TSquareMetersPerSecond = TSquareMeterPerSecondQty;

const
  rsSquareMeterPerSecondSymbol     = '%sm2/%ss';
  rsSquareMeterPerSecondName       = 'square %smeter per %ssecond';
  rsSquareMeterPerSecondPluralName = 'square %smeters per %ssecond';

const
  cSquareMeterPerSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareMeterPerSecondExponents : TExponents = (2, -1);

{ Quantity of TKilogramsPerQuarticMeter }

type
  TKilogramsPerQuarticMeter = TKilogramPerQuarticMeterQty;

const
  rsKilogramPerQuarticMeterSymbol     = '%sg/%sm4';
  rsKilogramPerQuarticMeterName       = '%sgram per quartic %smeter';
  rsKilogramPerQuarticMeterPluralName = '%sgrams per quartic %smeter';

const
  cKilogramPerQuarticMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramPerQuarticMeterExponents : TExponents = (1, -4);

{ Quantity of TQuarticMeterSeconds }

type
  TQuarticMeterSeconds = TQuarticMeterSecondQty;

const
  rsQuarticMeterSecondSymbol     = '%sm4·%ss';
  rsQuarticMeterSecondName       = 'quartic %smeter %ssecond';
  rsQuarticMeterSecondPluralName = 'quartic %smeter %sseconds';

const
  cQuarticMeterSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cQuarticMeterSecondExponents : TExponents = (4, 1);

{ Quantity of TKilogramsPerQuarticMeterPerSecond }

type
  TKilogramsPerQuarticMeterPerSecond = TKilogramPerQuarticMeterPerSecondQty;

const
  rsKilogramPerQuarticMeterPerSecondSymbol     = '%sg/%sm4/%ss';
  rsKilogramPerQuarticMeterPerSecondName       = '%sgram per quartic %smeter per %ssecond';
  rsKilogramPerQuarticMeterPerSecondPluralName = '%sgrams per quartic %smeter per %ssecond';

const
  cKilogramPerQuarticMeterPerSecondPrefixes  : TPrefixes  = (pKilo, pNone, pNone);
  cKilogramPerQuarticMeterPerSecondExponents : TExponents = (1, -4, -1);

{ Quantity of TCubicMetersPerKilogram }

type
  TCubicMetersPerKilogram = TCubicMeterPerKilogramQty;

const
  rsCubicMeterPerKilogramSymbol     = '%sm3/%sg';
  rsCubicMeterPerKilogramName       = 'cubic %smeter per %sgram';
  rsCubicMeterPerKilogramPluralName = 'cubic %smeters per %sgram';

const
  cCubicMeterPerKilogramPrefixes  : TPrefixes  = (pNone, pKilo);
  cCubicMeterPerKilogramExponents : TExponents = (3, -1);

{ Quantity of TKilogramSquareSeconds }

type
  TKilogramSquareSeconds = TKilogramSquareSecondQty;

const
  rsKilogramSquareSecondSymbol     = '%sg·%ss2';
  rsKilogramSquareSecondName       = '%sgram square %ssecond';
  rsKilogramSquareSecondPluralName = '%sgram square %sseconds';

const
  cKilogramSquareSecondPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramSquareSecondExponents : TExponents = (1, 2);

{ Quantity of TCubicMetersPerSquareSecond }

type
  TCubicMetersPerSquareSecond = TCubicMeterPerSquareSecondQty;

const
  rsCubicMeterPerSquareSecondSymbol     = '%sm3/%ss2';
  rsCubicMeterPerSquareSecondName       = 'cubic %smeter per square %ssecond';
  rsCubicMeterPerSquareSecondPluralName = 'cubic %smeters per square %ssecond';

const
  cCubicMeterPerSquareSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cCubicMeterPerSquareSecondExponents : TExponents = (3, -2);

{ Quantity of TNewtonSquareMeters }

type
  TNewtonSquareMeters = TNewtonSquareMeterQty;

const
  rsNewtonSquareMeterSymbol     = '%sN·%sm2';
  rsNewtonSquareMeterName       = '%snewton square %smeter';
  rsNewtonSquareMeterPluralName = '%snewton square %smeters';

const
  cNewtonSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonSquareMeterExponents : TExponents = (1, 2);

{ Quantity of TNewtonCubicMeters }

type
  TNewtonCubicMeters = TNewtonCubicMeterQty;

const
  rsNewtonCubicMeterSymbol     = '%sN·%sm3';
  rsNewtonCubicMeterName       = '%snewton cubic %smeter';
  rsNewtonCubicMeterPluralName = '%snewton cubic %smeters';

const
  cNewtonCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonCubicMeterExponents : TExponents = (1, 3);

{ Quantity of TSquareKilogramsPerMeter }

type
  TSquareKilogramsPerMeter = TSquareKilogramPerMeterQty;

const
  rsSquareKilogramPerMeterSymbol     = '%sg2/%sm';
  rsSquareKilogramPerMeterName       = 'square %sgram per %smeter';
  rsSquareKilogramPerMeterPluralName = 'square %sgrams per %smeter';

const
  cSquareKilogramPerMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cSquareKilogramPerMeterExponents : TExponents = (2, -1);

{ Quantity of TSquareKilogramsPerSquareMeter }

type
  TSquareKilogramsPerSquareMeter = TSquareKilogramPerSquareMeterQty;

const
  rsSquareKilogramPerSquareMeterSymbol     = '%sg2/%sm2';
  rsSquareKilogramPerSquareMeterName       = 'square %sgram per square %smeter';
  rsSquareKilogramPerSquareMeterPluralName = 'square %sgrams per square %smeter';

const
  cSquareKilogramPerSquareMeterPrefixes  : TPrefixes  = (pKilo, pNone);
  cSquareKilogramPerSquareMeterExponents : TExponents = (2, -2);

{ Quantity of TNewtonSquareMetersPerSquareKilogram }

type
  TNewtonSquareMetersPerSquareKilogram = TNewtonSquareMeterPerSquareKilogramQty;

const
  rsNewtonSquareMeterPerSquareKilogramSymbol     = '%sN·%sm2/%sg2';
  rsNewtonSquareMeterPerSquareKilogramName       = '%snewton square %smeter per square %sgram';
  rsNewtonSquareMeterPerSquareKilogramPluralName = '%snewton square %smeters per square %sgram';

const
  cNewtonSquareMeterPerSquareKilogramPrefixes  : TPrefixes  = (pNone, pNone, pKilo);
  cNewtonSquareMeterPerSquareKilogramExponents : TExponents = (1, 2, -2);

{ Quantity of TReciprocalKelvins }

type
  TReciprocalKelvins = TReciprocalKelvinQty;

const
  rsReciprocalKelvinSymbol     = '1/%sK';
  rsReciprocalKelvinName       = 'reciprocal %skelvin';
  rsReciprocalKelvinPluralName = 'reciprocal %skelvin';

const
  cReciprocalKelvinPrefixes  : TPrefixes  = (pNone);
  cReciprocalKelvinExponents : TExponents = (-1);

{ Quantity of TKilogramKelvins }

type
  TKilogramKelvins = TKilogramKelvinQty;

const
  rsKilogramKelvinSymbol     = '%sg·%sK';
  rsKilogramKelvinName       = '%sgram %skelvin';
  rsKilogramKelvinPluralName = '%sgram %skelvins';

const
  cKilogramKelvinPrefixes  : TPrefixes  = (pKilo, pNone);
  cKilogramKelvinExponents : TExponents = (1, 1);

{ Quantity of TJoulesPerKilogram }

type
  TJoulesPerKilogram = TSquareMeterPerSquareSecondQty;

const
  rsJoulePerKilogramSymbol     = '%sJ/%sg';
  rsJoulePerKilogramName       = '%sjoule per %sgram';
  rsJoulePerKilogramPluralName = '%sjoules per %sgram';

const
  cJoulePerKilogramPrefixes  : TPrefixes  = (pNone, pKilo);
  cJoulePerKilogramExponents : TExponents = (1, -1);

{ Quantity of TJoulesPerKilogramPerKelvin }

type
  TJoulesPerKilogramPerKelvin = TJoulePerKilogramPerKelvinQty;

const
  rsJoulePerKilogramPerKelvinSymbol     = '%sJ/%sg/%sK';
  rsJoulePerKilogramPerKelvinName       = '%sjoule per %sgram per %skelvin';
  rsJoulePerKilogramPerKelvinPluralName = '%sjoules per %sgram per %skelvin';

const
  cJoulePerKilogramPerKelvinPrefixes  : TPrefixes  = (pNone, pKilo, pNone);
  cJoulePerKilogramPerKelvinExponents : TExponents = (1, -1, -1);

{ Quantity of TMeterKelvins }

type
  TMeterKelvins = TMeterKelvinQty;

const
  rsMeterKelvinSymbol     = '%sm·%sK';
  rsMeterKelvinName       = '%smeter %skelvin';
  rsMeterKelvinPluralName = '%smeter %skelvins';

const
  cMeterKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cMeterKelvinExponents : TExponents = (1, 1);

{ Quantity of TKelvinsPerMeter }

type
  TKelvinsPerMeter = TKelvinPerMeterQty;

const
  rsKelvinPerMeterSymbol     = '%sK/%sm';
  rsKelvinPerMeterName       = '%skelvin per %smeter';
  rsKelvinPerMeterPluralName = '%skelvins per %smeter';

const
  cKelvinPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cKelvinPerMeterExponents : TExponents = (1, -1);

{ Quantity of TWattsPerMeter }

type
  TWattsPerMeter = TWattPerMeterQty;

const
  rsWattPerMeterSymbol     = '%sW/%sm';
  rsWattPerMeterName       = '%swatt per %smeter';
  rsWattPerMeterPluralName = '%swatts per %smeter';

const
  cWattPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cWattPerMeterExponents : TExponents = (1, -1);

{ Quantity of TWattsPerSquareMeter }

type
  TWattsPerSquareMeter = TWattPerSquareMeterQty;

const
  rsWattPerSquareMeterSymbol     = '%sW/%sm2';
  rsWattPerSquareMeterName       = '%swatt per square %smeter';
  rsWattPerSquareMeterPluralName = '%swatts per square %smeter';

const
  cWattPerSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cWattPerSquareMeterExponents : TExponents = (1, -2);

{ Quantity of TWattsPerKelvin }

type
  TWattsPerKelvin = TWattPerKelvinQty;

const
  rsWattPerKelvinSymbol     = '%sW/%sK';
  rsWattPerKelvinName       = '%swatt per %skelvin';
  rsWattPerKelvinPluralName = '%swatts per %skelvin';

const
  cWattPerKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cWattPerKelvinExponents : TExponents = (1, -1);

{ Quantity of TWattsPerMeterPerKelvin }

type
  TWattsPerMeterPerKelvin = TWattPerMeterPerKelvinQty;

const
  rsWattPerMeterPerKelvinSymbol     = '%sW/%sm/%sK';
  rsWattPerMeterPerKelvinName       = '%swatt per %smeter per %skelvin';
  rsWattPerMeterPerKelvinPluralName = '%swatts per %smeter per %skelvin';

const
  cWattPerMeterPerKelvinPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cWattPerMeterPerKelvinExponents : TExponents = (1, -1, -1);

{ Quantity of TSquareMeterKelvins }

type
  TSquareMeterKelvins = TSquareMeterKelvinQty;

const
  rsSquareMeterKelvinSymbol     = '%sm2·%sK';
  rsSquareMeterKelvinName       = 'square %smeter %skelvin';
  rsSquareMeterKelvinPluralName = 'square %smeter %skelvins';

const
  cSquareMeterKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareMeterKelvinExponents : TExponents = (2, 1);

{ Quantity of TWattsPerSquareMeterPerKelvin }

type
  TWattsPerSquareMeterPerKelvin = TWattPerSquareMeterPerKelvinQty;

const
  rsWattPerSquareMeterPerKelvinSymbol     = '%sW/%sm2/%sK';
  rsWattPerSquareMeterPerKelvinName       = '%swatt per square %smeter per %skelvin';
  rsWattPerSquareMeterPerKelvinPluralName = '%swatts per square %smeter per %skelvin';

const
  cWattPerSquareMeterPerKelvinPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cWattPerSquareMeterPerKelvinExponents : TExponents = (1, -2, -1);

{ Quantity of TSquareMeterQuarticKelvins }

type
  TSquareMeterQuarticKelvins = TSquareMeterQuarticKelvinQty;

const
  rsSquareMeterQuarticKelvinSymbol     = '%sm2·%sK4';
  rsSquareMeterQuarticKelvinName       = 'square %smeter quartic %skelvin';
  rsSquareMeterQuarticKelvinPluralName = 'square %smeter quartic %skelvins';

const
  cSquareMeterQuarticKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareMeterQuarticKelvinExponents : TExponents = (2, 4);

{ Quantity of TWattsPerQuarticKelvin }

type
  TWattsPerQuarticKelvin = TWattPerQuarticKelvinQty;

const
  rsWattPerQuarticKelvinSymbol     = '%sW/%sK4';
  rsWattPerQuarticKelvinName       = '%swatt per quartic %skelvin';
  rsWattPerQuarticKelvinPluralName = '%swatts per quartic %skelvin';

const
  cWattPerQuarticKelvinPrefixes  : TPrefixes  = (pNone, pNone);
  cWattPerQuarticKelvinExponents : TExponents = (1, -4);

{ Quantity of TWattsPerSquareMeterPerQuarticKelvin }

type
  TWattsPerSquareMeterPerQuarticKelvin = TWattPerSquareMeterPerQuarticKelvinQty;

const
  rsWattPerSquareMeterPerQuarticKelvinSymbol     = '%sW/%sm2/%sK4';
  rsWattPerSquareMeterPerQuarticKelvinName       = '%swatt per square %smeter per quartic %skelvin';
  rsWattPerSquareMeterPerQuarticKelvinPluralName = '%swatts per square %smeter per quartic %skelvin';

const
  cWattPerSquareMeterPerQuarticKelvinPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cWattPerSquareMeterPerQuarticKelvinExponents : TExponents = (1, -2, -4);

{ Quantity of TJoulesPerMolePerKelvin }

type
  TJoulesPerMolePerKelvin = TJoulePerMolePerKelvinQty;

const
  rsJoulePerMolePerKelvinSymbol     = '%sJ/%smol/%sK';
  rsJoulePerMolePerKelvinName       = '%sjoule per %smole per %skelvin';
  rsJoulePerMolePerKelvinPluralName = '%sjoules per %smole per %skelvin';

const
  cJoulePerMolePerKelvinPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cJoulePerMolePerKelvinExponents : TExponents = (1, -1, -1);

{ Quantity of TOhmMeters }

type
  TOhmMeters = TOhmMeterQty;

const
  rsOhmMeterSymbol     = '%sΩ·%sm';
  rsOhmMeterName       = '%sohm %smeter';
  rsOhmMeterPluralName = '%sohm %smeters';

const
  cOhmMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cOhmMeterExponents : TExponents = (1, 1);

{ Quantity of TVoltsPerMeter }

type
  TVoltsPerMeter = TVoltPerMeterQty;

const
  rsVoltPerMeterSymbol     = '%sV/%sm';
  rsVoltPerMeterName       = '%svolt per %smeter';
  rsVoltPerMeterPluralName = '%svolts per %smeter';

const
  cVoltPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cVoltPerMeterExponents : TExponents = (1, -1);

{ Quantity of TNewtonsPerCoulomb }

type
  TNewtonsPerCoulomb = TVoltPerMeterQty;

const
  rsNewtonPerCoulombSymbol     = '%sN/%sC';
  rsNewtonPerCoulombName       = '%snewton per %scoulomb';
  rsNewtonPerCoulombPluralName = '%snewtons per %scoulomb';

const
  cNewtonPerCoulombPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerCoulombExponents : TExponents = (1, -1);

{ Quantity of TCoulombsPerMeter }

type
  TCoulombsPerMeter = TCoulombPerMeterQty;

const
  rsCoulombPerMeterSymbol     = '%sC/%sm';
  rsCoulombPerMeterName       = '%scoulomb per %smeter';
  rsCoulombPerMeterPluralName = '%scoulombs per %smeter';

const
  cCoulombPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cCoulombPerMeterExponents : TExponents = (1, -1);

{ Quantity of TSquareCoulombsPerMeter }

type
  TSquareCoulombsPerMeter = TSquareCoulombPerMeterQty;

const
  rsSquareCoulombPerMeterSymbol     = '%sC2/%sm';
  rsSquareCoulombPerMeterName       = 'square %scoulomb per %smeter';
  rsSquareCoulombPerMeterPluralName = 'square %scoulombs per %smeter';

const
  cSquareCoulombPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareCoulombPerMeterExponents : TExponents = (2, -1);

{ Quantity of TCoulombsPerSquareMeter }

type
  TCoulombsPerSquareMeter = TCoulombPerSquareMeterQty;

const
  rsCoulombPerSquareMeterSymbol     = '%sC/%sm2';
  rsCoulombPerSquareMeterName       = '%scoulomb per square %smeter';
  rsCoulombPerSquareMeterPluralName = '%scoulombs per square %smeter';

const
  cCoulombPerSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cCoulombPerSquareMeterExponents : TExponents = (1, -2);

{ Quantity of TSquareMetersPerSquareCoulomb }

type
  TSquareMetersPerSquareCoulomb = TSquareMeterPerSquareCoulombQty;

const
  rsSquareMeterPerSquareCoulombSymbol     = '%sm2/%sC2';
  rsSquareMeterPerSquareCoulombName       = 'square %smeter per square %scoulomb';
  rsSquareMeterPerSquareCoulombPluralName = 'square %smeters per square %scoulomb';

const
  cSquareMeterPerSquareCoulombPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareMeterPerSquareCoulombExponents : TExponents = (2, -2);

{ Quantity of TNewtonsPerSquareCoulomb }

type
  TNewtonsPerSquareCoulomb = TNewtonPerSquareCoulombQty;

const
  rsNewtonPerSquareCoulombSymbol     = '%sN/%sC2';
  rsNewtonPerSquareCoulombName       = '%snewton per square %scoulomb';
  rsNewtonPerSquareCoulombPluralName = '%snewtons per square %scoulomb';

const
  cNewtonPerSquareCoulombPrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerSquareCoulombExponents : TExponents = (1, -2);

{ Quantity of TNewtonSquareMetersPerSquareCoulomb }

type
  TNewtonSquareMetersPerSquareCoulomb = TNewtonSquareMeterPerSquareCoulombQty;

const
  rsNewtonSquareMeterPerSquareCoulombSymbol     = '%sN·%sm2/%sC2';
  rsNewtonSquareMeterPerSquareCoulombName       = '%snewton square %smeter per square %scoulomb';
  rsNewtonSquareMeterPerSquareCoulombPluralName = '%snewton square %smeters per square %scoulomb';

const
  cNewtonSquareMeterPerSquareCoulombPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cNewtonSquareMeterPerSquareCoulombExponents : TExponents = (1, 2, -2);

{ Quantity of TVoltMeters }

type
  TVoltMeters = TVoltMeterQty;

const
  rsVoltMeterSymbol     = '%sV·%sm';
  rsVoltMeterName       = '%svolt %smeter';
  rsVoltMeterPluralName = '%svolt %smeters';

const
  cVoltMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cVoltMeterExponents : TExponents = (1, 1);

{ Quantity of TNewtonSquareMetersPerCoulomb }

type
  TNewtonSquareMetersPerCoulomb = TVoltMeterQty;

const
  rsNewtonSquareMeterPerCoulombSymbol     = '%sN·%sm2/%sC';
  rsNewtonSquareMeterPerCoulombName       = '%snewton square %smeter per %scoulomb';
  rsNewtonSquareMeterPerCoulombPluralName = '%snewton square %smeters per %scoulomb';

const
  cNewtonSquareMeterPerCoulombPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cNewtonSquareMeterPerCoulombExponents : TExponents = (1, 2, -1);

{ Quantity of TVoltMetersPerSecond }

type
  TVoltMetersPerSecond = TVoltMeterPerSecondQty;

const
  rsVoltMeterPerSecondSymbol     = '%sV·%sm/%ss';
  rsVoltMeterPerSecondName       = '%svolt %smeter per %ssecond';
  rsVoltMeterPerSecondPluralName = '%svolt %smeters per %ssecond';

const
  cVoltMeterPerSecondPrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cVoltMeterPerSecondExponents : TExponents = (1, 1, -1);

{ Quantity of TFaradsPerMeter }

type
  TFaradsPerMeter = TFaradPerMeterQty;

const
  rsFaradPerMeterSymbol     = '%sF/%sm';
  rsFaradPerMeterName       = '%sfarad per %smeter';
  rsFaradPerMeterPluralName = '%sfarads per %smeter';

const
  cFaradPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cFaradPerMeterExponents : TExponents = (1, -1);

{ Quantity of TAmperesPerMeter }

type
  TAmperesPerMeter = TAmperePerMeterQty;

const
  rsAmperePerMeterSymbol     = '%sA/%sm';
  rsAmperePerMeterName       = '%sampere per %smeter';
  rsAmperePerMeterPluralName = '%samperes per %smeter';

const
  cAmperePerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cAmperePerMeterExponents : TExponents = (1, -1);

{ Quantity of TMetersPerAmpere }

type
  TMetersPerAmpere = TMeterPerAmpereQty;

const
  rsMeterPerAmpereSymbol     = '%sm/%sA';
  rsMeterPerAmpereName       = '%smeter per %sampere';
  rsMeterPerAmperePluralName = '%smeters per %sampere';

const
  cMeterPerAmperePrefixes  : TPrefixes  = (pNone, pNone);
  cMeterPerAmpereExponents : TExponents = (1, -1);

{ Quantity of TTeslaMeters }

type
  TTeslaMeters = TTeslaMeterQty;

const
  rsTeslaMeterSymbol     = '%sT·%sm';
  rsTeslaMeterName       = '%stesla %smeter';
  rsTeslaMeterPluralName = '%stesla %smeters';

const
  cTeslaMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cTeslaMeterExponents : TExponents = (1, 1);

{ Quantity of TNewtonsPerAmpere }

type
  TNewtonsPerAmpere = TTeslaMeterQty;

const
  rsNewtonPerAmpereSymbol     = '%sN/%sA';
  rsNewtonPerAmpereName       = '%snewton per %sampere';
  rsNewtonPerAmperePluralName = '%snewtons per %sampere';

const
  cNewtonPerAmperePrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerAmpereExponents : TExponents = (1, -1);

{ Quantity of TTeslasPerAmpere }

type
  TTeslasPerAmpere = TTeslaPerAmpereQty;

const
  rsTeslaPerAmpereSymbol     = '%sT/%sA';
  rsTeslaPerAmpereName       = '%stesla per %sampere';
  rsTeslaPerAmperePluralName = '%steslas per %sampere';

const
  cTeslaPerAmperePrefixes  : TPrefixes  = (pNone, pNone);
  cTeslaPerAmpereExponents : TExponents = (1, -1);

{ Quantity of THenriesPerMeter }

type
  THenriesPerMeter = THenryPerMeterQty;

const
  rsHenryPerMeterSymbol     = '%sH/%sm';
  rsHenryPerMeterName       = '%shenry per %smeter';
  rsHenryPerMeterPluralName = '%shenries per %smeter';

const
  cHenryPerMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cHenryPerMeterExponents : TExponents = (1, -1);

{ Quantity of TTeslaMetersPerAmpere }

type
  TTeslaMetersPerAmpere = THenryPerMeterQty;

const
  rsTeslaMeterPerAmpereSymbol     = '%sT·%sm/%sA';
  rsTeslaMeterPerAmpereName       = '%stesla %smeter per %sampere';
  rsTeslaMeterPerAmperePluralName = '%stesla %smeters per %sampere';

const
  cTeslaMeterPerAmperePrefixes  : TPrefixes  = (pNone, pNone, pNone);
  cTeslaMeterPerAmpereExponents : TExponents = (1, 1, -1);

{ Quantity of TNewtonsPerSquareAmpere }

type
  TNewtonsPerSquareAmpere = THenryPerMeterQty;

const
  rsNewtonPerSquareAmpereSymbol     = '%sN/%sA2';
  rsNewtonPerSquareAmpereName       = '%snewton per square %sampere';
  rsNewtonPerSquareAmperePluralName = '%snewtons per square %sampere';

const
  cNewtonPerSquareAmperePrefixes  : TPrefixes  = (pNone, pNone);
  cNewtonPerSquareAmpereExponents : TExponents = (1, -2);

{ Quantity of TRadiansPerMeter }

type
  TRadiansPerMeter = TRadianPerMeterQty;

const
  rsRadianPerMeterSymbol     = 'rad/%sm';
  rsRadianPerMeterName       = 'radian per %smeter';
  rsRadianPerMeterPluralName = 'radians per %smeter';

const
  cRadianPerMeterPrefixes  : TPrefixes  = (pNone);
  cRadianPerMeterExponents : TExponents = (-1);

{ Quantity of TSquareSecondsPerSquareMeter }

type
  TSquareSecondsPerSquareMeter = TSquareSecondPerSquareMeterQty;

const
  rsSquareSecondPerSquareMeterSymbol     = '%ss2/%sm2';
  rsSquareSecondPerSquareMeterName       = 'square %ssecond per square %smeter';
  rsSquareSecondPerSquareMeterPluralName = 'square %sseconds per square %smeter';

const
  cSquareSecondPerSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareSecondPerSquareMeterExponents : TExponents = (2, -2);

{ Quantity of TSquareJoules }

type
  TSquareJoules = TSquareJouleQty;

var
  J2: TSquareJouleUnit;

const
  TJ2: TSquareJouleQty = (FValue: 1E+24);
  GJ2: TSquareJouleQty = (FValue: 1E+18);
  MJ2: TSquareJouleQty = (FValue: 1E+12);
  kJ2: TSquareJouleQty = (FValue: 1E+06);

const
  rsSquareJouleSymbol     = '%sJ2';
  rsSquareJouleName       = 'square %sjoule';
  rsSquareJoulePluralName = 'square %sjoules';

const
  cSquareJoulePrefixes  : TPrefixes  = (pNone);
  cSquareJouleExponents : TExponents = (2);

{ Quantity of TJouleSeconds }

type
  TJouleSeconds = TKilogramSquareMeterPerSecondQty;

const
  rsJouleSecondSymbol     = '%sJ·%ss';
  rsJouleSecondName       = '%sjoule %ssecond';
  rsJouleSecondPluralName = '%sjoule %sseconds';

const
  cJouleSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cJouleSecondExponents : TExponents = (1, 1);

{ Quantity of TElettronvoltSeconds }

type
  TElettronvoltSeconds = TKilogramSquareMeterPerSecondQty;

const
  rsElettronvoltSecondSymbol     = '%seV·%ss';
  rsElettronvoltSecondName       = '%selettronvolt %ssecond';
  rsElettronvoltSecondPluralName = '%selettronvolt %sseconds';

const
  cElettronvoltSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cElettronvoltSecondExponents : TExponents = (1, 1);
  cElettronvoltSecondFactor                 = 1.60217742320523E-019;

{ Quantity of TSquareJouleSquareSeconds }

type
  TSquareJouleSquareSeconds = TSquareJouleSquareSecondQty;

const
  rsSquareJouleSquareSecondSymbol     = '%sJ2·%ss2';
  rsSquareJouleSquareSecondName       = 'square %sjoule square %ssecond';
  rsSquareJouleSquareSecondPluralName = 'square %sjoule square %sseconds';

const
  cSquareJouleSquareSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cSquareJouleSquareSecondExponents : TExponents = (2, 2);

{ Quantity of TLumensPerWatt }

type
  TLumensPerWatt = TLumenPerWattQty;

const
  rsLumenPerWattSymbol     = '%slm/%sW';
  rsLumenPerWattName       = '%slumen per %swatt';
  rsLumenPerWattPluralName = '%slumens per %swatt';

const
  cLumenPerWattPrefixes  : TPrefixes  = (pNone, pNone);
  cLumenPerWattExponents : TExponents = (1, -1);

{ Quantity of TReciprocalMoles }

type
  TReciprocalMoles = TReciprocalMoleQty;

const
  rsReciprocalMoleSymbol     = '1/%smol';
  rsReciprocalMoleName       = 'reciprocal %smole';
  rsReciprocalMolePluralName = 'reciprocal %smoles';

const
  cReciprocalMolePrefixes  : TPrefixes  = (pNone);
  cReciprocalMoleExponents : TExponents = (-1);

{ Quantity of TAmperesPerSquareMeter }

type
  TAmperesPerSquareMeter = TAmperePerSquareMeterQty;

const
  rsAmperePerSquareMeterSymbol     = '%sA/%sm2';
  rsAmperePerSquareMeterName       = '%sampere per square %smeter';
  rsAmperePerSquareMeterPluralName = '%samperes per square %smeter';

const
  cAmperePerSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cAmperePerSquareMeterExponents : TExponents = (1, -2);

{ Quantity of TMolesPerCubicMeter }

type
  TMolesPerCubicMeter = TMolePerCubicMeterQty;

const
  rsMolePerCubicMeterSymbol     = '%smol/%sm3';
  rsMolePerCubicMeterName       = '%smole per cubic %smeter';
  rsMolePerCubicMeterPluralName = '%smoles per cubic %smeter';

const
  cMolePerCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cMolePerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TCandelasPerSquareMeter }

type
  TCandelasPerSquareMeter = TCandelaPerSquareMeterQty;

const
  rsCandelaPerSquareMeterSymbol     = '%scd/%sm2';
  rsCandelaPerSquareMeterName       = '%scandela per square %smeter';
  rsCandelaPerSquareMeterPluralName = '%scandelas per square %smeter';

const
  cCandelaPerSquareMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cCandelaPerSquareMeterExponents : TExponents = (1, -2);

{ Quantity of TCoulombsPerCubicMeter }

type
  TCoulombsPerCubicMeter = TCoulombPerCubicMeterQty;

const
  rsCoulombPerCubicMeterSymbol     = '%sC/%sm3';
  rsCoulombPerCubicMeterName       = '%scoulomb per cubic %smeter';
  rsCoulombPerCubicMeterPluralName = '%scoulombs per cubic %smeter';

const
  cCoulombPerCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cCoulombPerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TCoulombsPerKilogram }

type
  TCoulombsPerKilogram = TCoulombPerKilogramQty;

const
  rsCoulombPerKilogramSymbol     = '%sC/%sg';
  rsCoulombPerKilogramName       = '%scoulomb per %sgram';
  rsCoulombPerKilogramPluralName = '%scoulombs per %sgram';

const
  cCoulombPerKilogramPrefixes  : TPrefixes  = (pNone, pKilo);
  cCoulombPerKilogramExponents : TExponents = (1, -1);

{ Quantity of TGraysPerSecond }

type
  TGraysPerSecond = TGrayPerSecondQty;

const
  rsGrayPerSecondSymbol     = '%sGy/%ss';
  rsGrayPerSecondName       = '%sgray per %ssecond';
  rsGrayPerSecondPluralName = '%sgrays per %ssecond';

const
  cGrayPerSecondPrefixes  : TPrefixes  = (pNone, pNone);
  cGrayPerSecondExponents : TExponents = (1, -1);

{ Quantity of TWattsPerSteradian }

type
  TWattsPerSteradian = TWattPerSteradianQty;

const
  rsWattPerSteradianSymbol     = '%sW/sr';
  rsWattPerSteradianName       = '%swatt per steradian';
  rsWattPerSteradianPluralName = '%swatts per steradian';

const
  cWattPerSteradianPrefixes  : TPrefixes  = (pNone);
  cWattPerSteradianExponents : TExponents = (1);

{ Quantity of TSquareMeterSteradians }

type
  TSquareMeterSteradians = TSquareMeterSteradianQty;

const
  rsSquareMeterSteradianSymbol     = '%sm2·sr';
  rsSquareMeterSteradianName       = 'square %smeter steradian';
  rsSquareMeterSteradianPluralName = 'square %smeter steradians';

const
  cSquareMeterSteradianPrefixes  : TPrefixes  = (pNone);
  cSquareMeterSteradianExponents : TExponents = (2);

{ Quantity of TWattsPerSquareMeterPerSteradian }

type
  TWattsPerSquareMeterPerSteradian = TWattPerSquareMeterPerSteradianQty;

const
  rsWattPerSquareMeterPerSteradianSymbol     = '%sW/%sm2/sr';
  rsWattPerSquareMeterPerSteradianName       = '%swatt per square %smeter per steradian';
  rsWattPerSquareMeterPerSteradianPluralName = '%swatts per square %smeter per steradian';

const
  cWattPerSquareMeterPerSteradianPrefixes  : TPrefixes  = (pNone, pNone);
  cWattPerSquareMeterPerSteradianExponents : TExponents = (1, -2);

{ Quantity of TKatalsPerCubicMeter }

type
  TKatalsPerCubicMeter = TKatalPerCubicMeterQty;

const
  rsKatalPerCubicMeterSymbol     = '%skat/%sm3';
  rsKatalPerCubicMeterName       = '%skatal per cubic %smeter';
  rsKatalPerCubicMeterPluralName = '%skatals per cubic %smeter';

const
  cKatalPerCubicMeterPrefixes  : TPrefixes  = (pNone, pNone);
  cKatalPerCubicMeterExponents : TExponents = (1, -3);

{ Quantity of TCoulombsPerMole }

type
  TCoulombsPerMole = TCoulombPerMoleQty;

const
  rsCoulombPerMoleSymbol     = '%sC/%smol';
  rsCoulombPerMoleName       = '%scoulomb per %smole';
  rsCoulombPerMolePluralName = '%scoulombs per %smole';

const
  cCoulombPerMolePrefixes  : TPrefixes  = (pNone, pNone);
  cCoulombPerMoleExponents : TExponents = (1, -1);

{ Helpers }

type
  TSecondHelper = record helper for TSecondQty
    function ToMinute: TMinuteQty;
    function ToHour: THourQty;
    function ToDay: TDayQty;
  end;

  TSquareSecondHelper = record helper for TSquareSecondQty
    function ToSquareMinute: TSquareMinuteQty;
    function ToSquareHour: TSquareHourQty;
    function ToSquareDay: TSquareDayQty;
  end;

  TMeterHelper = record helper for TMeterQty
    function ToAngstrom: TAngstromQty;
    function ToNauticalMile: TNauticalMileQty;
    function ToMile: TMileQty;
    function ToYard: TYardQty;
    function ToFoot: TFootQty;
    function ToInch: TInchQty;
    function ToAstronomical: TAstronomicalQty;
  end;

  TSquareMeterHelper = record helper for TSquareMeterQty
    function ToSquareMile: TSquareMileQty;
    function ToSquareYard: TSquareYardQty;
    function ToSquareFoot: TSquareFootQty;
    function ToSquareInch: TSquareInchQty;
  end;

  TCubicMeterHelper = record helper for TCubicMeterQty
    function ToGallon: TGallonQty;
    function ToLitre: TLitreQty;
    function ToCubicYard: TCubicYardQty;
    function ToCubicFoot: TCubicFootQty;
    function ToCubicInch: TCubicInchQty;
  end;

  TKilogramHelper = record helper for TKilogramQty
    function ToTon: TTonQty;
    function ToStone: TStoneQty;
    function ToOunce: TOunceQty;
    function ToPound: TPoundQty;
    function ToTonne: TTonneQty;
  end;

  TDegreeCelsiusHelper = record helper for TDegreeCelsiusQty
    function ToKelvin: TKelvinQty;
  end;

  TKelvinHelper = record helper for TKelvinQty
    function ToDegreeFahrenheit: TDegreeFahrenheitQty;
    function ToDegreeCelsius: TDegreeCelsiusQty;
  end;

  TDegreeFahrenheitHelper = record helper for TDegreeFahrenheitQty
    function ToKelvin: TKelvinQty;
  end;

  TRadianHelper = record helper for TRadianQty
    function ToDegree: TDegreeQty;
  end;

  TSteradianHelper = record helper for TSteradianQty
    function ToSquareDegree: TSquareDegreeQty;
  end;

  TRadianPerSecondHelper = record helper for TRadianPerSecondQty
    function ToHertz: THertzQty;
  end;

  THertzHelper = record helper for THertzQty
    function ToBequerel: TBequerelQty;
    function ToRadianPerSecond: TRadianPerSecondQty;
  end;

  TSquareHertzHelper = record helper for TSquareHertzQty
    function ToSteradianPerSquareSecond: TSteradianPerSquareSecondQty;
    function ToRadianPerSquareSecond: TRadianPerSquareSecondQty;
  end;

  TRadianPerSquareSecondHelper = record helper for TRadianPerSquareSecondQty
    function ToSquareHertz: TSquareHertzQty;
  end;

  TSteradianPerSquareSecondHelper = record helper for TSteradianPerSquareSecondQty
    function ToSquareHertz: TSquareHertzQty;
  end;

  TMeterPerSecondHelper = record helper for TMeterPerSecondQty
    function ToNauticalMilePerHour: TNauticalMilePerHourQty;
    function ToMilePerHour: TMilePerHourQty;
    function ToMeterPerHour: TMeterPerHourQty;
  end;

  TMeterPerSquareSecondHelper = record helper for TMeterPerSquareSecondQty
    function ToMeterPerHourPerSecond: TMeterPerHourPerSecondQty;
    function ToMeterPerSecondPerSecond: TMeterPerSecondPerSecondQty;
  end;

  TMeterPerSecondPerSecondHelper = record helper for TMeterPerSecondPerSecondQty
    function ToMeterPerSquareSecond: TMeterPerSquareSecondQty;
  end;

  TKilogramMeterPerSecondHelper = record helper for TKilogramMeterPerSecondQty
    function ToNewtonSecond: TNewtonSecondQty;
  end;

  TNewtonSecondHelper = record helper for TNewtonSecondQty
    function ToKilogramMeterPerSecond: TKilogramMeterPerSecondQty;
  end;

  TKilogramSquareMeterPerSecondHelper = record helper for TKilogramSquareMeterPerSecondQty
    function ToElettronvoltSecond: TElettronvoltSecondQty;
    function ToJouleSecond: TJouleSecondQty;
    function ToNewtonMeterSecond: TNewtonMeterSecondQty;
  end;

  TNewtonMeterSecondHelper = record helper for TNewtonMeterSecondQty
    function ToKilogramSquareMeterPerSecond: TKilogramSquareMeterPerSecondQty;
  end;

  TKilogramPerCubicMeterHelper = record helper for TKilogramPerCubicMeterQty
    function ToPoundPerCubicInch: TPoundPerCubicInchQty;
  end;

  TNewtonHelper = record helper for TNewtonQty
    function ToPoundForce: TPoundForceQty;
  end;

  TPascalHelper = record helper for TPascalQty
    function ToJoulePerCubicMeter: TJoulePerCubicMeterQty;
    function ToPoundPerSquareInch: TPoundPerSquareInchQty;
    function ToBar: TBarQty;
  end;

  TJoulePerCubicMeterHelper = record helper for TJoulePerCubicMeterQty
    function ToPascal: TPascalQty;
  end;

  TJouleHelper = record helper for TJouleQty
    function ToCalorie: TCalorieQty;
    function ToRydberg: TRydbergQty;
    function ToPoundForceInch: TPoundForceInchQty;
    function ToNewtonMeter: TNewtonMeterQty;
    function ToElettronvolt: TElettronvoltQty;
    function ToWattHour: TWattHourQty;
  end;

  TNewtonMeterHelper = record helper for TNewtonMeterQty
    function ToJoule: TJouleQty;
  end;

  TJoulePerRadianHelper = record helper for TJoulePerRadianQty
    function ToNewtonMeterPerDegree: TNewtonMeterPerDegreeQty;
    function ToNewtonMeterPerRadian: TNewtonMeterPerRadianQty;
    function ToJoulePerDegree: TJoulePerDegreeQty;
  end;

  TNewtonMeterPerRadianHelper = record helper for TNewtonMeterPerRadianQty
    function ToJoulePerRadian: TJoulePerRadianQty;
  end;

  TCoulombHelper = record helper for TCoulombQty
    function ToAmpereHour: TAmpereHourQty;
  end;

  TBequerelHelper = record helper for TBequerelQty
    function ToHertz: THertzQty;
  end;

  TSquareMeterPerSquareSecondHelper = record helper for TSquareMeterPerSquareSecondQty
    function ToJoulePerKilogram: TJoulePerKilogramQty;
    function ToSievert: TSievertQty;
    function ToGray: TGrayQty;
  end;

  TGrayHelper = record helper for TGrayQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

  TSievertHelper = record helper for TSievertQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

  TNewtonPerMeterHelper = record helper for TNewtonPerMeterQty
    function ToPoundForcePerInch: TPoundForcePerInchQty;
  end;

  TPoiseuilleHelper = record helper for TPoiseuilleQty
    function ToPascalSecond: TPascalSecondQty;
  end;

  TPascalSecondHelper = record helper for TPascalSecondQty
    function ToPoiseuille: TPoiseuilleQty;
  end;

  TJoulePerKilogramHelper = record helper for TJoulePerKilogramQty
    function ToSquareMeterPerSquareSecond: TSquareMeterPerSquareSecondQty;
  end;

  TVoltPerMeterHelper = record helper for TVoltPerMeterQty
    function ToNewtonPerCoulomb: TNewtonPerCoulombQty;
  end;

  TNewtonPerCoulombHelper = record helper for TNewtonPerCoulombQty
    function ToVoltPerMeter: TVoltPerMeterQty;
  end;

  TVoltMeterHelper = record helper for TVoltMeterQty
    function ToNewtonSquareMeterPerCoulomb: TNewtonSquareMeterPerCoulombQty;
  end;

  TNewtonSquareMeterPerCoulombHelper = record helper for TNewtonSquareMeterPerCoulombQty
    function ToVoltMeter: TVoltMeterQty;
  end;

  TTeslaMeterHelper = record helper for TTeslaMeterQty
    function ToNewtonPerAmpere: TNewtonPerAmpereQty;
  end;

  TNewtonPerAmpereHelper = record helper for TNewtonPerAmpereQty
    function ToTeslaMeter: TTeslaMeterQty;
  end;

  THenryPerMeterHelper = record helper for THenryPerMeterQty
    function ToNewtonPerSquareAmpere: TNewtonPerSquareAmpereQty;
    function ToTeslaMeterPerAmpere: TTeslaMeterPerAmpereQty;
  end;

  TTeslaMeterPerAmpereHelper = record helper for TTeslaMeterPerAmpereQty
    function ToHenryPerMeter: THenryPerMeterQty;
  end;

  TNewtonPerSquareAmpereHelper = record helper for TNewtonPerSquareAmpereQty
    function ToHenryPerMeter: THenryPerMeterQty;
  end;

  TJouleSecondHelper = record helper for TJouleSecondQty
    function ToKilogramSquareMeterPerSecond: TKilogramSquareMeterPerSecondQty;
  end;

{ Power functions }

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
function SquarePower(AQuantity: TKilogramMeterPerSecondQty): TSquareKilogramSquareMeterPerSquareSecondQty;
function SquareRoot(AQuantity: TSquareKilogramSquareMeterPerSquareSecondQty): TKilogramMeterPerSecondQty;
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
function SquarePower(AQuantity: TKilogramSquareMeterPerSecondQty): TSquareJouleSquareSecondQty;
function SquareRoot(AQuantity: TSquareJouleSquareSecondQty): TKilogramSquareMeterPerSecondQty;

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

{ Math functions }

generic function Min<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
generic function Max<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;

{ Prefix table }

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

{ Useful routines }

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

{ TQuantity classes }

{$DEFINE CSYMBOL:=rsJoulePerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerKelvinName}
{$DEFINE CPLURALNAME:=rsJoulePerKelvinPluralName}
{$DEFINE CPREFIXES:=cJoulePerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKatalSymbol}
{$DEFINE CSINGULARNAME:=rsKatalName}
{$DEFINE CPLURALNAME:=rsKatalPluralName}
{$DEFINE CPREFIXES:=cKatalPrefixes}
{$DEFINE CEXPONENTS:=cKatalExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMeterSymbol}
{$DEFINE CSINGULARNAME:=rsMeterName}
{$DEFINE CPLURALNAME:=rsMeterPluralName}
{$DEFINE CPREFIXES:=cMeterPrefixes}
{$DEFINE CEXPONENTS:=cMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterQty}{$i adim.inc}

class operator TMeterQty./(const ALeft: TNewtonCubicMeterQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonSquareMeterQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TJouleQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TJouleQty; const ARight: TMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TNewtonQty; const ARight: TMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSexticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TQuinticMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TQuarticMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TCubicMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TCubicMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterQty./(const ALeft: TSquareMeterQty; const ARight: TMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterQty.*(const ALeft: TMeterQty; const ARight: TMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsJoulePerMoleSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerMoleName}
{$DEFINE CPLURALNAME:=rsJoulePerMolePluralName}
{$DEFINE CPREFIXES:=cJoulePerMolePrefixes}
{$DEFINE CEXPONENTS:=cJoulePerMoleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMoleQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareKilogramPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKilogramPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsSquareKilogramPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSquareKilogramPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareKilogramPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$i adim.inc}

class operator TSquareKilogramPerSquareSecondQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareKilogramPerSquareSecondQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCubicKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsCubicKelvinName}
{$DEFINE CPLURALNAME:=rsCubicKelvinPluralName}
{$DEFINE CPREFIXES:=cCubicKelvinPrefixes}
{$DEFINE CEXPONENTS:=cCubicKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMoleKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsMoleKelvinName}
{$DEFINE CPLURALNAME:=rsMoleKelvinPluralName}
{$DEFINE CPREFIXES:=cMoleKelvinPrefixes}
{$DEFINE CEXPONENTS:=cMoleKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonPerSquareKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerSquareKilogramName}
{$DEFINE CPLURALNAME:=rsNewtonPerSquareKilogramPluralName}
{$DEFINE CPREFIXES:=cNewtonPerSquareKilogramPrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerSquareKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$i adim.inc}

class operator TNewtonPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TNewtonPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterPerSquareKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterPerSquareKilogramName}
{$DEFINE CPLURALNAME:=rsSquareMeterPerSquareKilogramPluralName}
{$DEFINE CPREFIXES:=cSquareMeterPerSquareKilogramPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterPerSquareKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$i adim.inc}

class operator TSquareMeterPerSquareKilogramQty./(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterPerSquareKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsHertzSymbol}
{$DEFINE CSINGULARNAME:=rsHertzName}
{$DEFINE CPLURALNAME:=rsHertzPluralName}
{$DEFINE CPREFIXES:=cHertzPrefixes}
{$DEFINE CEXPONENTS:=cHertzExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THertzQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKelvinName}
{$DEFINE CPLURALNAME:=rsSquareKelvinPluralName}
{$DEFINE CPREFIXES:=cSquareKelvinPrefixes}
{$DEFINE CEXPONENTS:=cSquareKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKelvinQty}{$i adim.inc}

class operator TSquareKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TSquareKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKelvinQty.*(const ALeft: TSquareKelvinQty; const ARight: TSquareKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsSquareMeterPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSquareMeterPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$i adim.inc}

class operator TSquareMeterPerSquareSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSecondName}
{$DEFINE CPLURALNAME:=rsSecondPluralName}
{$DEFINE CPREFIXES:=cSecondPrefixes}
{$DEFINE CEXPONENTS:=cSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSecondQty}{$i adim.inc}

class operator TSecondQty./(const ALeft: TQuarticMeterSecondQty; const ARight: TSecondQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TQuarticMeterQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TQuarticMeterQty; const ARight: TSecondQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: THertzQty; const ARight: TSecondQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: THertzQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TSecondQty./(const ALeft: double; const ARight: TSecondQty): THertzQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSecondQty./(const ALeft: TSquareSecondQty; const ARight: TSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondQty.*(const ALeft: TSecondQty; const ARight: TSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsDaySymbol}
{$DEFINE CSINGULARNAME:=rsDayName}
{$DEFINE CPLURALNAME:=rsDayPluralName}
{$DEFINE CPREFIXES:=cDayPrefixes}
{$DEFINE CEXPONENTS:=cDayExponents}
{$DEFINE CFACTOR:=cDayFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDayQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsHourSymbol}
{$DEFINE CSINGULARNAME:=rsHourName}
{$DEFINE CPLURALNAME:=rsHourPluralName}
{$DEFINE CPREFIXES:=cHourPrefixes}
{$DEFINE CEXPONENTS:=cHourExponents}
{$DEFINE CFACTOR:=cHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMinuteSymbol}
{$DEFINE CSINGULARNAME:=rsMinuteName}
{$DEFINE CPLURALNAME:=rsMinutePluralName}
{$DEFINE CPREFIXES:=cMinutePrefixes}
{$DEFINE CEXPONENTS:=cMinuteExponents}
{$DEFINE CFACTOR:=cMinuteFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMinuteQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareSecondName}
{$DEFINE CPLURALNAME:=rsSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondQty}{$i adim.inc}

class operator TSquareSecondQty./(const ALeft: TSquareJouleSquareSecondQty; const ARight: TSquareSecondQty): TSquareJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSquareJouleQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TSquareJouleQty; const ARight: TSquareSecondQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TJouleQty; const ARight: TSquareSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TJouleQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TNewtonQty; const ARight: TSquareSecondQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TNewtonQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondQty./(const ALeft: TKilogramMeterQty; const ARight: TSquareSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareDaySymbol}
{$DEFINE CSINGULARNAME:=rsSquareDayName}
{$DEFINE CPLURALNAME:=rsSquareDayPluralName}
{$DEFINE CPREFIXES:=cSquareDayPrefixes}
{$DEFINE CEXPONENTS:=cSquareDayExponents}
{$DEFINE CFACTOR:=cSquareDayFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareDayQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareHourSymbol}
{$DEFINE CSINGULARNAME:=rsSquareHourName}
{$DEFINE CPLURALNAME:=rsSquareHourPluralName}
{$DEFINE CPREFIXES:=cSquareHourPrefixes}
{$DEFINE CEXPONENTS:=cSquareHourExponents}
{$DEFINE CFACTOR:=cSquareHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareMinuteSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMinuteName}
{$DEFINE CPLURALNAME:=rsSquareMinutePluralName}
{$DEFINE CPREFIXES:=cSquareMinutePrefixes}
{$DEFINE CEXPONENTS:=cSquareMinuteExponents}
{$DEFINE CFACTOR:=cSquareMinuteFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMinuteQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsAstronomicalSymbol}
{$DEFINE CSINGULARNAME:=rsAstronomicalName}
{$DEFINE CPLURALNAME:=rsAstronomicalPluralName}
{$DEFINE CPREFIXES:=cAstronomicalPrefixes}
{$DEFINE CEXPONENTS:=cAstronomicalExponents}
{$DEFINE CFACTOR:=cAstronomicalFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAstronomicalQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsInchSymbol}
{$DEFINE CSINGULARNAME:=rsInchName}
{$DEFINE CPLURALNAME:=rsInchPluralName}
{$DEFINE CPREFIXES:=cInchPrefixes}
{$DEFINE CEXPONENTS:=cInchExponents}
{$DEFINE CFACTOR:=cInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsFootSymbol}
{$DEFINE CSINGULARNAME:=rsFootName}
{$DEFINE CPLURALNAME:=rsFootPluralName}
{$DEFINE CPREFIXES:=cFootPrefixes}
{$DEFINE CEXPONENTS:=cFootExponents}
{$DEFINE CFACTOR:=cFootFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFootQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsYardSymbol}
{$DEFINE CSINGULARNAME:=rsYardName}
{$DEFINE CPLURALNAME:=rsYardPluralName}
{$DEFINE CPREFIXES:=cYardPrefixes}
{$DEFINE CEXPONENTS:=cYardExponents}
{$DEFINE CFACTOR:=cYardFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TYardQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMileSymbol}
{$DEFINE CSINGULARNAME:=rsMileName}
{$DEFINE CPLURALNAME:=rsMilePluralName}
{$DEFINE CPREFIXES:=cMilePrefixes}
{$DEFINE CEXPONENTS:=cMileExponents}
{$DEFINE CFACTOR:=cMileFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMileQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNauticalMileSymbol}
{$DEFINE CSINGULARNAME:=rsNauticalMileName}
{$DEFINE CPLURALNAME:=rsNauticalMilePluralName}
{$DEFINE CPREFIXES:=cNauticalMilePrefixes}
{$DEFINE CEXPONENTS:=cNauticalMileExponents}
{$DEFINE CFACTOR:=cNauticalMileFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNauticalMileQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsAngstromSymbol}
{$DEFINE CSINGULARNAME:=rsAngstromName}
{$DEFINE CPLURALNAME:=rsAngstromPluralName}
{$DEFINE CPREFIXES:=cAngstromPrefixes}
{$DEFINE CEXPONENTS:=cAngstromExponents}
{$DEFINE CFACTOR:=cAngstromFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAngstromQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterName}
{$DEFINE CPLURALNAME:=rsSquareMeterPluralName}
{$DEFINE CPREFIXES:=cSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQty}{$i adim.inc}

class operator TSquareMeterQty./(const ALeft: TSquareMeterSteradianQty; const ARight: TSquareMeterQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSteradianQty; const ARight: TSquareMeterQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSteradianQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TSquareMeterQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TQuarticKelvinQty; const ARight: TSquareMeterQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TSexticMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TQuarticMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TQuinticMeterQty; const ARight: TSquareMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCubicMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TQuinticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterQty./(const ALeft: TQuarticMeterQty; const ARight: TSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareInchSymbol}
{$DEFINE CSINGULARNAME:=rsSquareInchName}
{$DEFINE CPLURALNAME:=rsSquareInchPluralName}
{$DEFINE CPREFIXES:=cSquareInchPrefixes}
{$DEFINE CEXPONENTS:=cSquareInchExponents}
{$DEFINE CFACTOR:=cSquareInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareFootSymbol}
{$DEFINE CSINGULARNAME:=rsSquareFootName}
{$DEFINE CPLURALNAME:=rsSquareFootPluralName}
{$DEFINE CPREFIXES:=cSquareFootPrefixes}
{$DEFINE CEXPONENTS:=cSquareFootExponents}
{$DEFINE CFACTOR:=cSquareFootFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareFootQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareYardSymbol}
{$DEFINE CSINGULARNAME:=rsSquareYardName}
{$DEFINE CPLURALNAME:=rsSquareYardPluralName}
{$DEFINE CPREFIXES:=cSquareYardPrefixes}
{$DEFINE CEXPONENTS:=cSquareYardExponents}
{$DEFINE CFACTOR:=cSquareYardFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareYardQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareMileSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMileName}
{$DEFINE CPLURALNAME:=rsSquareMilePluralName}
{$DEFINE CPREFIXES:=cSquareMilePrefixes}
{$DEFINE CEXPONENTS:=cSquareMileExponents}
{$DEFINE CFACTOR:=cSquareMileFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMileQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsCubicMeterName}
{$DEFINE CPLURALNAME:=rsCubicMeterPluralName}
{$DEFINE CPREFIXES:=cCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterQty}{$i adim.inc}

class operator TCubicMeterQty./(const ALeft: TCubicMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterQty./(const ALeft: TCubicMeterQty; const ARight: TKilogramSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterQty./(const ALeft: TNewtonCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TNewtonQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterQty.*(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterQty./(const ALeft: TSexticMeterQty; const ARight: TCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TCubicMeterQty): TSexticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCubicInchSymbol}
{$DEFINE CSINGULARNAME:=rsCubicInchName}
{$DEFINE CPLURALNAME:=rsCubicInchPluralName}
{$DEFINE CPREFIXES:=cCubicInchPrefixes}
{$DEFINE CEXPONENTS:=cCubicInchExponents}
{$DEFINE CFACTOR:=cCubicInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCubicFootSymbol}
{$DEFINE CSINGULARNAME:=rsCubicFootName}
{$DEFINE CPLURALNAME:=rsCubicFootPluralName}
{$DEFINE CPREFIXES:=cCubicFootPrefixes}
{$DEFINE CEXPONENTS:=cCubicFootExponents}
{$DEFINE CFACTOR:=cCubicFootFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicFootQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCubicYardSymbol}
{$DEFINE CSINGULARNAME:=rsCubicYardName}
{$DEFINE CPLURALNAME:=rsCubicYardPluralName}
{$DEFINE CPREFIXES:=cCubicYardPrefixes}
{$DEFINE CEXPONENTS:=cCubicYardExponents}
{$DEFINE CFACTOR:=cCubicYardFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicYardQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsLitreSymbol}
{$DEFINE CSINGULARNAME:=rsLitreName}
{$DEFINE CPLURALNAME:=rsLitrePluralName}
{$DEFINE CPREFIXES:=cLitrePrefixes}
{$DEFINE CEXPONENTS:=cLitreExponents}
{$DEFINE CFACTOR:=cLitreFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLitreQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsGallonSymbol}
{$DEFINE CSINGULARNAME:=rsGallonName}
{$DEFINE CPLURALNAME:=rsGallonPluralName}
{$DEFINE CPREFIXES:=cGallonPrefixes}
{$DEFINE CEXPONENTS:=cGallonExponents}
{$DEFINE CFACTOR:=cGallonFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGallonQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsQuarticMeterSymbol}
{$DEFINE CSINGULARNAME:=rsQuarticMeterName}
{$DEFINE CPLURALNAME:=rsQuarticMeterPluralName}
{$DEFINE CPREFIXES:=cQuarticMeterPrefixes}
{$DEFINE CEXPONENTS:=cQuarticMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsQuinticMeterSymbol}
{$DEFINE CSINGULARNAME:=rsQuinticMeterName}
{$DEFINE CPLURALNAME:=rsQuinticMeterPluralName}
{$DEFINE CPREFIXES:=cQuinticMeterPrefixes}
{$DEFINE CEXPONENTS:=cQuinticMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuinticMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSexticMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSexticMeterName}
{$DEFINE CPLURALNAME:=rsSexticMeterPluralName}
{$DEFINE CPREFIXES:=cSexticMeterPrefixes}
{$DEFINE CEXPONENTS:=cSexticMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSexticMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramName}
{$DEFINE CPLURALNAME:=rsKilogramPluralName}
{$DEFINE CPREFIXES:=cKilogramPrefixes}
{$DEFINE CEXPONENTS:=cKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramQty}{$i adim.inc}

class operator TKilogramQty./(const ALeft: TSquareJouleSquareSecondQty; const ARight: TKilogramQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TNewtonCubicMeterQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TNewtonCubicMeterQty; const ARight: TKilogramQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramSquareSecondQty; const ARight: TKilogramQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareSecondQty; const ARight: TKilogramQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareSecondQty): TKilogramSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareKilogramSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TJouleQty): TSquareKilogramSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TJouleQty; const ARight: TKilogramQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareMeterPerSquareSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TSquareMeterQty; const ARight: TKilogramQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TKilogramMeterQty; const ARight: TKilogramQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TMeterQty; const ARight: TKilogramQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramQty./(const ALeft: TSquareKilogramQty; const ARight: TKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramQty.*(const ALeft: TKilogramQty; const ARight: TKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsTonneSymbol}
{$DEFINE CSINGULARNAME:=rsTonneName}
{$DEFINE CPLURALNAME:=rsTonnePluralName}
{$DEFINE CPREFIXES:=cTonnePrefixes}
{$DEFINE CEXPONENTS:=cTonneExponents}
{$DEFINE CFACTOR:=cTonneFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTonneQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsPoundSymbol}
{$DEFINE CSINGULARNAME:=rsPoundName}
{$DEFINE CPLURALNAME:=rsPoundPluralName}
{$DEFINE CPREFIXES:=cPoundPrefixes}
{$DEFINE CEXPONENTS:=cPoundExponents}
{$DEFINE CFACTOR:=cPoundFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsOunceSymbol}
{$DEFINE CSINGULARNAME:=rsOunceName}
{$DEFINE CPLURALNAME:=rsOuncePluralName}
{$DEFINE CPREFIXES:=cOuncePrefixes}
{$DEFINE CEXPONENTS:=cOunceExponents}
{$DEFINE CFACTOR:=cOunceFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOunceQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsStoneSymbol}
{$DEFINE CSINGULARNAME:=rsStoneName}
{$DEFINE CPLURALNAME:=rsStonePluralName}
{$DEFINE CPREFIXES:=cStonePrefixes}
{$DEFINE CEXPONENTS:=cStoneExponents}
{$DEFINE CFACTOR:=cStoneFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TStoneQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsTonSymbol}
{$DEFINE CSINGULARNAME:=rsTonName}
{$DEFINE CPLURALNAME:=rsTonPluralName}
{$DEFINE CPREFIXES:=cTonPrefixes}
{$DEFINE CEXPONENTS:=cTonExponents}
{$DEFINE CFACTOR:=cTonFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTonQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKilogramName}
{$DEFINE CPLURALNAME:=rsSquareKilogramPluralName}
{$DEFINE CPREFIXES:=cSquareKilogramPrefixes}
{$DEFINE CEXPONENTS:=cSquareKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramQty}{$i adim.inc}

class operator TSquareKilogramQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramQty.*(const ALeft: TSquareKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsAmpereName}
{$DEFINE CPLURALNAME:=rsAmperePluralName}
{$DEFINE CPREFIXES:=cAmperePrefixes}
{$DEFINE CEXPONENTS:=cAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmpereQty}{$i adim.inc}

class operator TAmpereQty.*(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TTeslaMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TNewtonQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TCoulombQty; const ARight: TAmpereQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TSecondQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TSecondQty; const ARight: TAmpereQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmpereQty./(const ALeft: TSquareAmpereQty; const ARight: TAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmpereQty.*(const ALeft: TAmpereQty; const ARight: TAmpereQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsSquareAmpereName}
{$DEFINE CPLURALNAME:=rsSquareAmperePluralName}
{$DEFINE CPREFIXES:=cSquareAmperePrefixes}
{$DEFINE CEXPONENTS:=cSquareAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareAmpereQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsKelvinName}
{$DEFINE CPLURALNAME:=rsKelvinPluralName}
{$DEFINE CPREFIXES:=cKelvinPrefixes}
{$DEFINE CEXPONENTS:=cKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinQty}{$i adim.inc}

class operator TKelvinQty./(const ALeft: TSquareMeterKelvinQty; const ARight: TKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TSquareMeterQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TKelvinQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TMeterKelvinQty; const ARight: TKelvinQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TMeterQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TMeterQty; const ARight: TKelvinQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TJoulePerKelvinQty; const ARight: TKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TJouleQty; const ARight: TKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TKilogramKelvinQty; const ARight: TKelvinQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TKilogramQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKilogramQty; const ARight: TKelvinQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TQuarticKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TCubicKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TCubicKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TSquareKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TCubicKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinQty./(const ALeft: TSquareKelvinQty; const ARight: TKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinQty.*(const ALeft: TKelvinQty; const ARight: TKelvinQty): TSquareKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsDegreeCelsiusSymbol}
{$DEFINE CSINGULARNAME:=rsDegreeCelsiusName}
{$DEFINE CPLURALNAME:=rsDegreeCelsiusPluralName}
{$DEFINE CPREFIXES:=cDegreeCelsiusPrefixes}
{$DEFINE CEXPONENTS:=cDegreeCelsiusExponents}
{$DEFINE CFACTOR:=cDegreeCelsiusFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsDegreeFahrenheitSymbol}
{$DEFINE CSINGULARNAME:=rsDegreeFahrenheitName}
{$DEFINE CPLURALNAME:=rsDegreeFahrenheitPluralName}
{$DEFINE CPREFIXES:=cDegreeFahrenheitPrefixes}
{$DEFINE CEXPONENTS:=cDegreeFahrenheitExponents}
{$DEFINE CFACTOR:=cDegreeFahrenheitFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsQuarticKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsQuarticKelvinName}
{$DEFINE CPLURALNAME:=rsQuarticKelvinPluralName}
{$DEFINE CPREFIXES:=cQuarticKelvinPrefixes}
{$DEFINE CEXPONENTS:=cQuarticKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMoleSymbol}
{$DEFINE CSINGULARNAME:=rsMoleName}
{$DEFINE CPLURALNAME:=rsMolePluralName}
{$DEFINE CPREFIXES:=cMolePrefixes}
{$DEFINE CEXPONENTS:=cMoleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMoleQty}{$i adim.inc}

class operator TMoleQty./(const ALeft: TMoleKelvinQty; const ARight: TMoleQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TKelvinQty; const ARight: TMoleQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TKelvinQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TJoulePerMoleQty; const ARight: TMoleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMoleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TJouleQty; const ARight: TMoleQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TMoleQty; const ARight: TKatalQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMoleQty./(const ALeft: TMoleQty; const ARight: TSecondQty): TKatalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCandelaSymbol}
{$DEFINE CSINGULARNAME:=rsCandelaName}
{$DEFINE CPLURALNAME:=rsCandelaPluralName}
{$DEFINE CPREFIXES:=cCandelaPrefixes}
{$DEFINE CEXPONENTS:=cCandelaExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaQty}{$i adim.inc}

class operator TCandelaQty./(const ALeft: TLumenQty; const ARight: TCandelaQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCandelaQty.*(const ALeft: TSteradianQty; const ARight: TCandelaQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaQty.*(const ALeft: TCandelaQty; const ARight: TSteradianQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsRadianSymbol}
{$DEFINE CSINGULARNAME:=rsRadianName}
{$DEFINE CPLURALNAME:=rsRadianPluralName}
{$DEFINE CPREFIXES:=cRadianPrefixes}
{$DEFINE CEXPONENTS:=cRadianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianQty}{$i adim.inc}

class operator TRadianQty./(const ALeft: TSteradianQty; const ARight: TRadianQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianQty.*(const ALeft: TRadianQty; const ARight: TRadianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsDegreeSymbol}
{$DEFINE CSINGULARNAME:=rsDegreeName}
{$DEFINE CPLURALNAME:=rsDegreePluralName}
{$DEFINE CPREFIXES:=cDegreePrefixes}
{$DEFINE CEXPONENTS:=cDegreeExponents}
{$DEFINE CFACTOR:=cDegreeFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TDegreeQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSteradianSymbol}
{$DEFINE CSINGULARNAME:=rsSteradianName}
{$DEFINE CPLURALNAME:=rsSteradianPluralName}
{$DEFINE CPREFIXES:=cSteradianPrefixes}
{$DEFINE CEXPONENTS:=cSteradianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareDegreeSymbol}
{$DEFINE CSINGULARNAME:=rsSquareDegreeName}
{$DEFINE CPLURALNAME:=rsSquareDegreePluralName}
{$DEFINE CPREFIXES:=cSquareDegreePrefixes}
{$DEFINE CEXPONENTS:=cSquareDegreeExponents}
{$DEFINE CFACTOR:=cSquareDegreeFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareDegreeQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareHertzSymbol}
{$DEFINE CSINGULARNAME:=rsSquareHertzName}
{$DEFINE CPLURALNAME:=rsSquareHertzPluralName}
{$DEFINE CPREFIXES:=cSquareHertzPrefixes}
{$DEFINE CEXPONENTS:=cSquareHertzExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareHertzQty}{$i adim.inc}

class operator TSquareHertzQty./(const ALeft: TJouleQty; const ARight: TSquareHertzQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TKilogramSquareMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareHertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: TSquareHertzQty; const ARight: THertzQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: THertzQty; const ARight: TSquareHertzQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSquareHertzQty; const ARight: TSecondQty): THertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty.*(const ALeft: TSecondQty; const ARight: TSquareHertzQty): THertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareHertzQty./(const ALeft: double; const ARight: TSquareHertzQty): TSquareSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsRadianPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsRadianPerSecondName}
{$DEFINE CPLURALNAME:=rsRadianPerSecondPluralName}
{$DEFINE CPREFIXES:=cRadianPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cRadianPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSecondQty}{$i adim.inc}

class operator TRadianPerSecondQty./(const ALeft: TRadianPerSecondQty; const ARight: THertzQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondQty./(const ALeft: TRadianQty; const ARight: TRadianPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSecondQty.*(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsRadianPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsRadianPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsRadianPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cRadianPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cRadianPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerSquareSecondQty}{$i adim.inc}

class operator TRadianPerSquareSecondQty./(const ALeft: TRadianPerSquareSecondQty; const ARight: TSquareHertzQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSquareSecondQty./(const ALeft: TRadianPerSquareSecondQty; const ARight: TRadianQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSquareSecondQty./(const ALeft: TRadianQty; const ARight: TRadianPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSquareSecondQty.*(const ALeft: TRadianPerSquareSecondQty; const ARight: TSquareSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TRadianPerSquareSecondQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSquareSecondQty./(const ALeft: TRadianPerSecondQty; const ARight: TRadianPerSquareSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerSquareSecondQty.*(const ALeft: TRadianPerSquareSecondQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSquareSecondQty.*(const ALeft: TSecondQty; const ARight: TRadianPerSquareSecondQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSteradianPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSteradianPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsSteradianPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSteradianPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSteradianPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$i adim.inc}

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareHertzQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSteradianQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty./(const ALeft: TSteradianQty; const ARight: TSteradianPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSteradianPerSquareSecondQty; const ARight: TSquareSecondQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSteradianPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TSteradianPerSquareSecondQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondQty}{$i adim.inc}

class operator TMeterPerSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TMeterPerSecondQty; const ARight: THertzQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty./(const ALeft: TMeterQty; const ARight: TMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMeterPerHourSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerHourName}
{$DEFINE CPLURALNAME:=rsMeterPerHourPluralName}
{$DEFINE CPREFIXES:=cMeterPerHourPrefixes}
{$DEFINE CEXPONENTS:=cMeterPerHourExponents}
{$DEFINE CFACTOR:=cMeterPerHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMilePerHourSymbol}
{$DEFINE CSINGULARNAME:=rsMilePerHourName}
{$DEFINE CPLURALNAME:=rsMilePerHourPluralName}
{$DEFINE CPREFIXES:=cMilePerHourPrefixes}
{$DEFINE CEXPONENTS:=cMilePerHourExponents}
{$DEFINE CFACTOR:=cMilePerHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMilePerHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNauticalMilePerHourSymbol}
{$DEFINE CSINGULARNAME:=rsNauticalMilePerHourName}
{$DEFINE CPLURALNAME:=rsNauticalMilePerHourPluralName}
{$DEFINE CPREFIXES:=cNauticalMilePerHourPrefixes}
{$DEFINE CEXPONENTS:=cNauticalMilePerHourExponents}
{$DEFINE CFACTOR:=cNauticalMilePerHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMeterPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsMeterPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cMeterPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cMeterPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$i adim.inc}

class operator TMeterPerSquareSecondQty./(const ALeft: TNewtonQty; const ARight: TMeterPerSquareSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TKilogramQty; const ARight: TMeterPerSquareSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TMeterQty; const ARight: TMeterPerSquareSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TMeterPerSquareSecondQty; const ARight: TMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty./(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareHertzQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty./(const ALeft: TMeterPerSquareSecondQty; const ARight: TMeterQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty./(const ALeft: TMeterQty; const ARight: TMeterPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty./(const ALeft: TMeterPerSecondQty; const ARight: TMeterPerSquareSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TMeterPerSquareSecondQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerSquareSecondQty.*(const ALeft: TSecondQty; const ARight: TMeterPerSquareSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMeterPerSecondPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerSecondPerSecondName}
{$DEFINE CPLURALNAME:=rsMeterPerSecondPerSecondPluralName}
{$DEFINE CPREFIXES:=cMeterPerSecondPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cMeterPerSecondPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerSecondPerSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsMeterPerHourPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerHourPerSecondName}
{$DEFINE CPLURALNAME:=rsMeterPerHourPerSecondPluralName}
{$DEFINE CPREFIXES:=cMeterPerHourPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cMeterPerHourPerSecondExponents}
{$DEFINE CFACTOR:=cMeterPerHourPerSecondFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKilogramMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramMeterName}
{$DEFINE CPLURALNAME:=rsKilogramMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKilogramPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerSecondName}
{$DEFINE CPLURALNAME:=rsKilogramPerSecondPluralName}
{$DEFINE CPREFIXES:=cKilogramPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$i adim.inc}

class operator TKilogramPerSecondQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerSecondQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TPoiseuilleQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TNewtonQty; const ARight: TKilogramPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TKilogramPerSecondQty; const ARight: TSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsKilogramMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cKilogramMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cKilogramMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$i adim.inc}

class operator TKilogramMeterPerSecondQty./(const ALeft: TJouleQty; const ARight: TKilogramMeterPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty.*(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TNewtonQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TKilogramMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty.*(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TSquareKilogramSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramPerSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty./(const ALeft: TKilogramMeterQty; const ARight: TKilogramMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty.*(const ALeft: TKilogramMeterPerSecondQty; const ARight: TSecondQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramMeterPerSecondQty): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareKilogramSquareMeterPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKilogramSquareMeterPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsSquareKilogramSquareMeterPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSquareKilogramSquareMeterPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareKilogramSquareMeterPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramSquareMeterPerSquareSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonSecondSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonSecondName}
{$DEFINE CPLURALNAME:=rsNewtonSecondPluralName}
{$DEFINE CPREFIXES:=cNewtonSecondPrefixes}
{$DEFINE CEXPONENTS:=cNewtonSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsReciprocalMeterSymbol}
{$DEFINE CSINGULARNAME:=rsReciprocalMeterName}
{$DEFINE CPLURALNAME:=rsReciprocalMeterPluralName}
{$DEFINE CPREFIXES:=cReciprocalMeterPrefixes}
{$DEFINE CEXPONENTS:=cReciprocalMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMeterQty}{$i adim.inc}

class operator TReciprocalMeterQty./(const ALeft: double; const ARight: TReciprocalMeterQty): TMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramSquareMeterName}
{$DEFINE CPLURALNAME:=rsKilogramSquareMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKilogramSquareMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramSquareMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsKilogramSquareMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cKilogramSquareMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cKilogramSquareMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$i adim.inc}

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TSquareJouleSquareSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterPerSecondQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterPerSecondQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: THertzQty; const ARight: TKilogramSquareMeterPerSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TJouleQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramMeterPerSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TReciprocalMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TReciprocalMeterQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TReciprocalMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramMeterPerSecondQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: THertzQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TKilogramSquareMeterQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty./(const ALeft: TKilogramSquareMeterQty; const ARight: TKilogramSquareMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TKilogramSquareMeterPerSecondQty; const ARight: TSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramSquareMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramSquareMeterPerSecondQty): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonMeterSecondSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonMeterSecondName}
{$DEFINE CPLURALNAME:=rsNewtonMeterSecondPluralName}
{$DEFINE CPREFIXES:=cNewtonMeterSecondPrefixes}
{$DEFINE CEXPONENTS:=cNewtonMeterSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSecondPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSecondPerMeterName}
{$DEFINE CPLURALNAME:=rsSecondPerMeterPluralName}
{$DEFINE CPREFIXES:=cSecondPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cSecondPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSecondPerMeterQty}{$i adim.inc}

class operator TSecondPerMeterQty./(const ALeft: TSecondQty; const ARight: TSecondPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSecondPerMeterQty.*(const ALeft: TSecondPerMeterQty; const ARight: TMeterQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSecondPerMeterQty.*(const ALeft: TMeterQty; const ARight: TSecondPerMeterQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerMeterName}
{$DEFINE CPLURALNAME:=rsKilogramPerMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$i adim.inc}

class operator TKilogramPerMeterQty./(const ALeft: TNewtonQty; const ARight: TKilogramPerMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerMeterQty./(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerMeterQty./(const ALeft: TKilogramPerMeterQty; const ARight: TSecondPerMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerMeterQty./(const ALeft: TKilogramPerMeterQty; const ARight: TKilogramPerSecondQty): TSecondPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TKilogramPerMeterQty; const ARight: TMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsKilogramPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$i adim.inc}

class operator TKilogramPerSquareMeterQty./(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerSquareMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TMeterPerSecondQty; const ARight: TKilogramPerSquareMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterPerSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TKilogramPerSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramPerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerCubicMeterName}
{$DEFINE CPLURALNAME:=rsKilogramPerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramPerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$i adim.inc}

class operator TKilogramPerCubicMeterQty./(const ALeft: TKilogramPerSquareMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TCubicMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsPoundPerCubicInchSymbol}
{$DEFINE CSINGULARNAME:=rsPoundPerCubicInchName}
{$DEFINE CPLURALNAME:=rsPoundPerCubicInchPluralName}
{$DEFINE CPREFIXES:=cPoundPerCubicInchPrefixes}
{$DEFINE CEXPONENTS:=cPoundPerCubicInchExponents}
{$DEFINE CFACTOR:=cPoundPerCubicInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundPerCubicInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonName}
{$DEFINE CPLURALNAME:=rsNewtonPluralName}
{$DEFINE CPREFIXES:=cNewtonPrefixes}
{$DEFINE CEXPONENTS:=cNewtonExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonQty}{$i adim.inc}

class operator TNewtonQty./(const ALeft: TSquareNewtonQty; const ARight: TNewtonQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonQty.*(const ALeft: TNewtonQty; const ARight: TNewtonQty): TSquareNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsPoundForceSymbol}
{$DEFINE CSINGULARNAME:=rsPoundForceName}
{$DEFINE CPLURALNAME:=rsPoundForcePluralName}
{$DEFINE CPREFIXES:=cPoundForcePrefixes}
{$DEFINE CEXPONENTS:=cPoundForceExponents}
{$DEFINE CFACTOR:=cPoundForceFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundForceQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareNewtonSymbol}
{$DEFINE CSINGULARNAME:=rsSquareNewtonName}
{$DEFINE CPLURALNAME:=rsSquareNewtonPluralName}
{$DEFINE CPREFIXES:=cSquareNewtonPrefixes}
{$DEFINE CEXPONENTS:=cSquareNewtonExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareNewtonQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsPascalSymbol}
{$DEFINE CSINGULARNAME:=rsPascalName}
{$DEFINE CPLURALNAME:=rsPascalPluralName}
{$DEFINE CPREFIXES:=cPascalPrefixes}
{$DEFINE CEXPONENTS:=cPascalExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalQty}{$i adim.inc}

class operator TPascalQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TPascalQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TQuarticMeterQty; const ARight: TPascalQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TQuarticMeterQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TPoiseuilleQty; const ARight: TPascalQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TSecondQty; const ARight: TPascalQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TJouleQty; const ARight: TPascalQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TCubicMeterQty; const ARight: TPascalQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TCubicMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TPascalQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TPascalQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty./(const ALeft: TNewtonQty; const ARight: TPascalQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TPascalQty; const ARight: TSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TPascalQty.*(const ALeft: TSquareMeterQty; const ARight: TPascalQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsBarSymbol}
{$DEFINE CSINGULARNAME:=rsBarName}
{$DEFINE CPLURALNAME:=rsBarPluralName}
{$DEFINE CPREFIXES:=cBarPrefixes}
{$DEFINE CEXPONENTS:=cBarExponents}
{$DEFINE CFACTOR:=cBarFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TBarQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsPoundPerSquareInchSymbol}
{$DEFINE CSINGULARNAME:=rsPoundPerSquareInchName}
{$DEFINE CPLURALNAME:=rsPoundPerSquareInchPluralName}
{$DEFINE CPREFIXES:=cPoundPerSquareInchPrefixes}
{$DEFINE CEXPONENTS:=cPoundPerSquareInchExponents}
{$DEFINE CFACTOR:=cPoundPerSquareInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJoulePerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerCubicMeterName}
{$DEFINE CPLURALNAME:=rsJoulePerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cJoulePerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerCubicMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJouleSymbol}
{$DEFINE CSINGULARNAME:=rsJouleName}
{$DEFINE CPLURALNAME:=rsJoulePluralName}
{$DEFINE CPREFIXES:=cJoulePrefixes}
{$DEFINE CEXPONENTS:=cJouleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleQty}{$i adim.inc}

class operator TJouleQty./(const ALeft: TSquareJouleQty; const ARight: TJouleQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJouleQty.*(const ALeft: TJouleQty; const ARight: TJouleQty): TSquareJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattHourSymbol}
{$DEFINE CSINGULARNAME:=rsWattHourName}
{$DEFINE CPLURALNAME:=rsWattHourPluralName}
{$DEFINE CPREFIXES:=cWattHourPrefixes}
{$DEFINE CEXPONENTS:=cWattHourExponents}
{$DEFINE CFACTOR:=cWattHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsElettronvoltSymbol}
{$DEFINE CSINGULARNAME:=rsElettronvoltName}
{$DEFINE CPLURALNAME:=rsElettronvoltPluralName}
{$DEFINE CPREFIXES:=cElettronvoltPrefixes}
{$DEFINE CEXPONENTS:=cElettronvoltExponents}
{$DEFINE CFACTOR:=cElettronvoltFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TElettronvoltQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonMeterSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonMeterName}
{$DEFINE CPLURALNAME:=rsNewtonMeterPluralName}
{$DEFINE CPREFIXES:=cNewtonMeterPrefixes}
{$DEFINE CEXPONENTS:=cNewtonMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterQty}{$i adim.inc}

class operator TNewtonMeterQty./(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonMeterQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonMeterQty.*(const ALeft: TNewtonMeterQty; const ARight: TSquareMeterQty): TNewtonCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsPoundForceInchSymbol}
{$DEFINE CSINGULARNAME:=rsPoundForceInchName}
{$DEFINE CPLURALNAME:=rsPoundForceInchPluralName}
{$DEFINE CPREFIXES:=cPoundForceInchPrefixes}
{$DEFINE CEXPONENTS:=cPoundForceInchExponents}
{$DEFINE CFACTOR:=cPoundForceInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundForceInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsRydbergSymbol}
{$DEFINE CSINGULARNAME:=rsRydbergName}
{$DEFINE CPLURALNAME:=rsRydbergPluralName}
{$DEFINE CPREFIXES:=cRydbergPrefixes}
{$DEFINE CEXPONENTS:=cRydbergExponents}
{$DEFINE CFACTOR:=cRydbergFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRydbergQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCalorieSymbol}
{$DEFINE CSINGULARNAME:=rsCalorieName}
{$DEFINE CPLURALNAME:=rsCaloriePluralName}
{$DEFINE CPREFIXES:=cCaloriePrefixes}
{$DEFINE CEXPONENTS:=cCalorieExponents}
{$DEFINE CFACTOR:=cCalorieFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCalorieQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJoulePerRadianSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerRadianName}
{$DEFINE CPLURALNAME:=rsJoulePerRadianPluralName}
{$DEFINE CPREFIXES:=cJoulePerRadianPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerRadianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerRadianQty}{$i adim.inc}

class operator TJoulePerRadianQty./(const ALeft: TJouleQty; const ARight: TJoulePerRadianQty): TRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerRadianQty.*(const ALeft: TJoulePerRadianQty; const ARight: TRadianQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerRadianQty.*(const ALeft: TRadianQty; const ARight: TJoulePerRadianQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsJoulePerDegreeSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerDegreeName}
{$DEFINE CPLURALNAME:=rsJoulePerDegreePluralName}
{$DEFINE CPREFIXES:=cJoulePerDegreePrefixes}
{$DEFINE CEXPONENTS:=cJoulePerDegreeExponents}
{$DEFINE CFACTOR:=cJoulePerDegreeFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonMeterPerRadianSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonMeterPerRadianName}
{$DEFINE CPLURALNAME:=rsNewtonMeterPerRadianPluralName}
{$DEFINE CPREFIXES:=cNewtonMeterPerRadianPrefixes}
{$DEFINE CEXPONENTS:=cNewtonMeterPerRadianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerRadianQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonMeterPerDegreeSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonMeterPerDegreeName}
{$DEFINE CPLURALNAME:=rsNewtonMeterPerDegreePluralName}
{$DEFINE CPREFIXES:=cNewtonMeterPerDegreePrefixes}
{$DEFINE CEXPONENTS:=cNewtonMeterPerDegreeExponents}
{$DEFINE CFACTOR:=cNewtonMeterPerDegreeFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsWattSymbol}
{$DEFINE CSINGULARNAME:=rsWattName}
{$DEFINE CPLURALNAME:=rsWattPluralName}
{$DEFINE CPREFIXES:=cWattPrefixes}
{$DEFINE CEXPONENTS:=cWattExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattQty}{$i adim.inc}

class operator TWattQty./(const ALeft: TWattQty; const ARight: TMeterPerSecondQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TNewtonQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TKilogramPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: THertzQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TWattQty; const ARight: TJouleQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty./(const ALeft: TJouleQty; const ARight: TWattQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattQty.*(const ALeft: TWattQty; const ARight: TSecondQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattQty.*(const ALeft: TSecondQty; const ARight: TWattQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombName}
{$DEFINE CPLURALNAME:=rsCoulombPluralName}
{$DEFINE CPREFIXES:=cCoulombPrefixes}
{$DEFINE CEXPONENTS:=cCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombQty}{$i adim.inc}

class operator TCoulombQty./(const ALeft: TSquareCoulombQty; const ARight: TCoulombQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombQty.*(const ALeft: TCoulombQty; const ARight: TCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsAmpereHourSymbol}
{$DEFINE CSINGULARNAME:=rsAmpereHourName}
{$DEFINE CPLURALNAME:=rsAmpereHourPluralName}
{$DEFINE CPREFIXES:=cAmpereHourPrefixes}
{$DEFINE CEXPONENTS:=cAmpereHourExponents}
{$DEFINE CFACTOR:=cAmpereHourFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmpereHourQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsSquareCoulombName}
{$DEFINE CPLURALNAME:=rsSquareCoulombPluralName}
{$DEFINE CPREFIXES:=cSquareCoulombPrefixes}
{$DEFINE CEXPONENTS:=cSquareCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombQty}{$i adim.inc}

class operator TSquareCoulombQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TNewtonSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombQty./(const ALeft: TNewtonSquareMeterQty; const ARight: TSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsVoltSymbol}
{$DEFINE CSINGULARNAME:=rsVoltName}
{$DEFINE CPLURALNAME:=rsVoltPluralName}
{$DEFINE CPREFIXES:=cVoltPrefixes}
{$DEFINE CEXPONENTS:=cVoltExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltQty}{$i adim.inc}

class operator TVoltQty./(const ALeft: TVoltMeterQty; const ARight: TVoltQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TMeterQty; const ARight: TVoltQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TWeberQty; const ARight: TVoltQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TSecondQty; const ARight: TVoltQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TSecondQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TSquareVoltQty; const ARight: TVoltQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TVoltQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TWattQty; const ARight: TVoltQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TAmpereQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TAmpereQty; const ARight: TVoltQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty./(const ALeft: TJouleQty; const ARight: TVoltQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TVoltQty; const ARight: TCoulombQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltQty.*(const ALeft: TCoulombQty; const ARight: TVoltQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareVoltSymbol}
{$DEFINE CSINGULARNAME:=rsSquareVoltName}
{$DEFINE CPLURALNAME:=rsSquareVoltPluralName}
{$DEFINE CPREFIXES:=cSquareVoltPrefixes}
{$DEFINE CEXPONENTS:=cSquareVoltExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareVoltQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsFaradSymbol}
{$DEFINE CSINGULARNAME:=rsFaradName}
{$DEFINE CPLURALNAME:=rsFaradPluralName}
{$DEFINE CPREFIXES:=cFaradPrefixes}
{$DEFINE CEXPONENTS:=cFaradExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradQty}{$i adim.inc}

class operator TFaradQty./(const ALeft: TSquareCoulombQty; const ARight: TFaradQty): TJouleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TJouleQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TJouleQty; const ARight: TFaradQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty./(const ALeft: TCoulombQty; const ARight: TFaradQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TFaradQty; const ARight: TVoltQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradQty.*(const ALeft: TVoltQty; const ARight: TFaradQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsOhmSymbol}
{$DEFINE CSINGULARNAME:=rsOhmName}
{$DEFINE CPLURALNAME:=rsOhmPluralName}
{$DEFINE CPREFIXES:=cOhmPrefixes}
{$DEFINE CEXPONENTS:=cOhmExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmQty}{$i adim.inc}

class operator TOhmQty./(const ALeft: TOhmMeterQty; const ARight: TOhmQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TMeterQty; const ARight: TOhmQty): TOhmMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TMeterQty): TOhmMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TWeberQty; const ARight: TOhmQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TCoulombQty; const ARight: TOhmQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TCoulombQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TVoltQty; const ARight: TOhmQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TAmpereQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TAmpereQty; const ARight: TOhmQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TSquareVoltQty; const ARight: TOhmQty): TWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TWattQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TWattQty; const ARight: TOhmQty): TSquareVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TWattQty; const ARight: TOhmQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TSquareAmpereQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TSquareAmpereQty; const ARight: TOhmQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty./(const ALeft: TSecondQty; const ARight: TOhmQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TOhmQty; const ARight: TFaradQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TOhmQty.*(const ALeft: TFaradQty; const ARight: TOhmQty): TSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSiemensSymbol}
{$DEFINE CSINGULARNAME:=rsSiemensName}
{$DEFINE CPLURALNAME:=rsSiemensPluralName}
{$DEFINE CPREFIXES:=cSiemensPrefixes}
{$DEFINE CEXPONENTS:=cSiemensExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSiemensQty}{$i adim.inc}

class operator TSiemensQty./(const ALeft: TAmpereQty; const ARight: TSiemensQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TSiemensQty; const ARight: TVoltQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TVoltQty; const ARight: TSiemensQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty./(const ALeft: TSiemensQty; const ARight: THertzQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSiemensQty./(const ALeft: TSiemensQty; const ARight: TFaradQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSiemensQty./(const ALeft: TFaradQty; const ARight: TSiemensQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TSiemensQty; const ARight: TSecondQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty.*(const ALeft: TSecondQty; const ARight: TSiemensQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSiemensQty./(const ALeft: double; const ARight: TSiemensQty): TOhmQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsTeslaSymbol}
{$DEFINE CSINGULARNAME:=rsTeslaName}
{$DEFINE CPLURALNAME:=rsTeslaPluralName}
{$DEFINE CPREFIXES:=cTeslaPrefixes}
{$DEFINE CEXPONENTS:=cTeslaExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaQty}{$i adim.inc}

class operator TTeslaQty./(const ALeft: TTeslaMeterQty; const ARight: TTeslaQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TMeterQty; const ARight: TTeslaQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TTeslaQty; const ARight: TMeterQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaQty./(const ALeft: TWeberQty; const ARight: TTeslaQty): TSquareMeterQty;
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

class operator TTeslaQty./(const ALeft: TKilogramPerSecondQty; const ARight: TTeslaQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TTeslaQty; const ARight: TCoulombQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaQty.*(const ALeft: TCoulombQty; const ARight: TTeslaQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWeberSymbol}
{$DEFINE CSINGULARNAME:=rsWeberName}
{$DEFINE CPLURALNAME:=rsWeberPluralName}
{$DEFINE CPREFIXES:=cWeberPrefixes}
{$DEFINE CEXPONENTS:=cWeberExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWeberQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsHenrySymbol}
{$DEFINE CSINGULARNAME:=rsHenryName}
{$DEFINE CPLURALNAME:=rsHenryPluralName}
{$DEFINE CPREFIXES:=cHenryPrefixes}
{$DEFINE CEXPONENTS:=cHenryExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryQty}{$i adim.inc}

class operator THenryQty./(const ALeft: TOhmQty; const ARight: THenryQty): THertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: THertzQty): TOhmQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THertzQty; const ARight: THenryQty): TOhmQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty./(const ALeft: THenryQty; const ARight: TSecondQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty./(const ALeft: THenryQty; const ARight: TOhmQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty./(const ALeft: TWeberQty; const ARight: THenryQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryQty.*(const ALeft: THenryQty; const ARight: TAmpereQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryQty.*(const ALeft: TAmpereQty; const ARight: THenryQty): TWeberQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsLumenSymbol}
{$DEFINE CSINGULARNAME:=rsLumenName}
{$DEFINE CPLURALNAME:=rsLumenPluralName}
{$DEFINE CPREFIXES:=cLumenPrefixes}
{$DEFINE CEXPONENTS:=cLumenExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsLuxSymbol}
{$DEFINE CSINGULARNAME:=rsLuxName}
{$DEFINE CPLURALNAME:=rsLuxPluralName}
{$DEFINE CPREFIXES:=cLuxPrefixes}
{$DEFINE CEXPONENTS:=cLuxExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLuxQty}{$i adim.inc}

class operator TLuxQty./(const ALeft: TLumenQty; const ARight: TLuxQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TLuxQty.*(const ALeft: TLuxQty; const ARight: TSquareMeterQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLuxQty.*(const ALeft: TSquareMeterQty; const ARight: TLuxQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsBequerelSymbol}
{$DEFINE CSINGULARNAME:=rsBequerelName}
{$DEFINE CPLURALNAME:=rsBequerelPluralName}
{$DEFINE CPREFIXES:=cBequerelPrefixes}
{$DEFINE CEXPONENTS:=cBequerelExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TBequerelQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsGraySymbol}
{$DEFINE CSINGULARNAME:=rsGrayName}
{$DEFINE CPLURALNAME:=rsGrayPluralName}
{$DEFINE CPREFIXES:=cGrayPrefixes}
{$DEFINE CEXPONENTS:=cGrayExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSievertSymbol}
{$DEFINE CSINGULARNAME:=rsSievertName}
{$DEFINE CPLURALNAME:=rsSievertPluralName}
{$DEFINE CPREFIXES:=cSievertPrefixes}
{$DEFINE CEXPONENTS:=cSievertExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSievertQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonPerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerCubicMeterName}
{$DEFINE CPLURALNAME:=rsNewtonPerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cNewtonPerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$i adim.inc}

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterPerSquareSecondQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonPerCubicMeterQty; const ARight: TKilogramPerCubicMeterQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TPascalQty; const ARight: TNewtonPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TNewtonPerCubicMeterQty; const ARight: TCubicMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TNewtonPerCubicMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerMeterName}
{$DEFINE CPLURALNAME:=rsNewtonPerMeterPluralName}
{$DEFINE CPREFIXES:=cNewtonPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$i adim.inc}

class operator TNewtonPerMeterQty./(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TNewtonPerMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TKilogramQty; const ARight: TNewtonPerMeterQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TSquareHertzQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TKilogramQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonPerMeterQty; const ARight: TPascalQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TJouleQty; const ARight: TNewtonPerMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TSquareMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TNewtonPerMeterQty; const ARight: TMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerMeterQty.*(const ALeft: TMeterQty; const ARight: TNewtonPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsPoundForcePerInchSymbol}
{$DEFINE CSINGULARNAME:=rsPoundForcePerInchName}
{$DEFINE CPLURALNAME:=rsPoundForcePerInchPluralName}
{$DEFINE CPREFIXES:=cPoundForcePerInchPrefixes}
{$DEFINE CEXPONENTS:=cPoundForcePerInchExponents}
{$DEFINE CFACTOR:=cPoundForcePerInchFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCubicMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsCubicMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsCubicMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cCubicMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cCubicMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$i adim.inc}

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterPerSecondQty; const ARight: TMeterPerSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterPerSecondQty; const ARight: TSquareMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSecondQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TCubicMeterPerSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsPoiseuilleSymbol}
{$DEFINE CSINGULARNAME:=rsPoiseuilleName}
{$DEFINE CPLURALNAME:=rsPoiseuillePluralName}
{$DEFINE CPREFIXES:=cPoiseuillePrefixes}
{$DEFINE CEXPONENTS:=cPoiseuilleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPoiseuilleQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsPascalSecondSymbol}
{$DEFINE CSINGULARNAME:=rsPascalSecondName}
{$DEFINE CPLURALNAME:=rsPascalSecondPluralName}
{$DEFINE CPREFIXES:=cPascalSecondPrefixes}
{$DEFINE CEXPONENTS:=cPascalSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TPascalSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsSquareMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cSquareMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$i adim.inc}

class operator TSquareMeterPerSecondQty./(const ALeft: TPoiseuilleQty; const ARight: TSquareMeterPerSecondQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TKilogramPerCubicMeterQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSecondQty): TPoiseuilleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSecondQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSquareMeterPerSecondQty; const ARight: TSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TSquareMeterPerSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramPerQuarticMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerQuarticMeterName}
{$DEFINE CPLURALNAME:=rsKilogramPerQuarticMeterPluralName}
{$DEFINE CPREFIXES:=cKilogramPerQuarticMeterPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerQuarticMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$i adim.inc}

class operator TKilogramPerQuarticMeterQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TQuarticMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsQuarticMeterSecondSymbol}
{$DEFINE CSINGULARNAME:=rsQuarticMeterSecondName}
{$DEFINE CPLURALNAME:=rsQuarticMeterSecondPluralName}
{$DEFINE CPREFIXES:=cQuarticMeterSecondPrefixes}
{$DEFINE CEXPONENTS:=cQuarticMeterSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKilogramPerQuarticMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramPerQuarticMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsKilogramPerQuarticMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cKilogramPerQuarticMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cKilogramPerQuarticMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$i adim.inc}

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TPascalQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TCubicMeterPerSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TCubicMeterPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TSecondQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty./(const ALeft: TKilogramPerSecondQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TKilogramPerQuarticMeterPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKilogramPerQuarticMeterPerSecondQty.*(const ALeft: TQuarticMeterQty; const ARight: TKilogramPerQuarticMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCubicMeterPerKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsCubicMeterPerKilogramName}
{$DEFINE CPLURALNAME:=rsCubicMeterPerKilogramPluralName}
{$DEFINE CPREFIXES:=cCubicMeterPerKilogramPrefixes}
{$DEFINE CEXPONENTS:=cCubicMeterPerKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$i adim.inc}

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterPerKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterPerKilogramQty; const ARight: TSquareSecondQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty./(const ALeft: double; const ARight: TCubicMeterPerKilogramQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TCubicMeterPerKilogramQty; const ARight: TKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCubicMeterPerKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramSquareSecondName}
{$DEFINE CPLURALNAME:=rsKilogramSquareSecondPluralName}
{$DEFINE CPREFIXES:=cKilogramSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cKilogramSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCubicMeterPerSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsCubicMeterPerSquareSecondName}
{$DEFINE CPLURALNAME:=rsCubicMeterPerSquareSecondPluralName}
{$DEFINE CPREFIXES:=cCubicMeterPerSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cCubicMeterPerSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$i adim.inc}

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TKilogramQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TMeterPerSquareSecondQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty./(const ALeft: TCubicMeterQty; const ARight: TCubicMeterPerSquareSecondQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TCubicMeterPerSquareSecondQty; const ARight: TSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCubicMeterPerSquareSecondQty.*(const ALeft: TSquareSecondQty; const ARight: TCubicMeterPerSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonSquareMeterName}
{$DEFINE CPLURALNAME:=rsNewtonSquareMeterPluralName}
{$DEFINE CPREFIXES:=cNewtonSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cNewtonSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonCubicMeterName}
{$DEFINE CPLURALNAME:=rsNewtonCubicMeterPluralName}
{$DEFINE CPREFIXES:=cNewtonCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cNewtonCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonCubicMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareKilogramPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKilogramPerMeterName}
{$DEFINE CPLURALNAME:=rsSquareKilogramPerMeterPluralName}
{$DEFINE CPREFIXES:=cSquareKilogramPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cSquareKilogramPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$i adim.inc}

class operator TSquareKilogramPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty./(const ALeft: TJouleQty; const ARight: TSquareKilogramPerMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TSquareKilogramPerMeterQty; const ARight: TMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareKilogramPerMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareKilogramPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSquareKilogramPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsSquareKilogramPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cSquareKilogramPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cSquareKilogramPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$i adim.inc}

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty./(const ALeft: TNewtonQty; const ARight: TSquareKilogramPerSquareMeterQty): TNewtonSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty./(const ALeft: TSquareKilogramQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareKilogramPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareKilogramPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramPerSquareMeterQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonSquareMeterPerSquareKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonSquareMeterPerSquareKilogramName}
{$DEFINE CPLURALNAME:=rsNewtonSquareMeterPerSquareKilogramPluralName}
{$DEFINE CPREFIXES:=cNewtonSquareMeterPerSquareKilogramPrefixes}
{$DEFINE CEXPONENTS:=cNewtonSquareMeterPerSquareKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsReciprocalKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsReciprocalKelvinName}
{$DEFINE CPLURALNAME:=rsReciprocalKelvinPluralName}
{$DEFINE CPREFIXES:=cReciprocalKelvinPrefixes}
{$DEFINE CEXPONENTS:=cReciprocalKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$i adim.inc}

class operator TReciprocalKelvinQty./(const ALeft: double; const ARight: TReciprocalKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TReciprocalKelvinQty.*(const ALeft: TReciprocalKelvinQty; const ARight: TKelvinQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

class operator TReciprocalKelvinQty.*(const ALeft: TKelvinQty; const ARight: TReciprocalKelvinQty): double;
begin
  result := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKilogramKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsKilogramKelvinName}
{$DEFINE CPLURALNAME:=rsKilogramKelvinPluralName}
{$DEFINE CPREFIXES:=cKilogramKelvinPrefixes}
{$DEFINE CEXPONENTS:=cKilogramKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKilogramKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJoulePerKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerKilogramName}
{$DEFINE CPLURALNAME:=rsJoulePerKilogramPluralName}
{$DEFINE CPREFIXES:=cJoulePerKilogramPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJoulePerKilogramPerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerKilogramPerKelvinName}
{$DEFINE CPLURALNAME:=rsJoulePerKilogramPerKelvinPluralName}
{$DEFINE CPREFIXES:=cJoulePerKilogramPerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerKilogramPerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$i adim.inc}

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramQty; const ARight: TJoulePerKilogramPerKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TJoulePerKilogramPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKelvinQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerKilogramPerKelvinQty): TKilogramKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TJoulePerKilogramPerKelvinQty; const ARight: TKilogramKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerKilogramPerKelvinQty.*(const ALeft: TKilogramKelvinQty; const ARight: TJoulePerKilogramPerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMeterKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsMeterKelvinName}
{$DEFINE CPLURALNAME:=rsMeterKelvinPluralName}
{$DEFINE CPREFIXES:=cMeterKelvinPrefixes}
{$DEFINE CEXPONENTS:=cMeterKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsKelvinPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKelvinPerMeterName}
{$DEFINE CPLURALNAME:=rsKelvinPerMeterPluralName}
{$DEFINE CPREFIXES:=cKelvinPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cKelvinPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$i adim.inc}

class operator TKelvinPerMeterQty./(const ALeft: TKelvinQty; const ARight: TKelvinPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKelvinPerMeterQty.*(const ALeft: TMeterQty; const ARight: TKelvinPerMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerMeterName}
{$DEFINE CPLURALNAME:=rsWattPerMeterPluralName}
{$DEFINE CPREFIXES:=cWattPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cWattPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterQty}{$i adim.inc}

class operator TWattPerMeterQty./(const ALeft: TWattQty; const ARight: TWattPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterQty.*(const ALeft: TWattPerMeterQty; const ARight: TMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsWattPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cWattPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cWattPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$i adim.inc}

class operator TWattPerSquareMeterQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterQty.*(const ALeft: TWattPerSquareMeterQty; const ARight: TSquareMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerKelvinName}
{$DEFINE CPLURALNAME:=rsWattPerKelvinPluralName}
{$DEFINE CPREFIXES:=cWattPerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cWattPerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerKelvinQty}{$i adim.inc}

class operator TWattPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerKelvinQty.*(const ALeft: TWattPerKelvinQty; const ARight: TKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerMeterPerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerMeterPerKelvinName}
{$DEFINE CPLURALNAME:=rsWattPerMeterPerKelvinPluralName}
{$DEFINE CPREFIXES:=cWattPerMeterPerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cWattPerMeterPerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$i adim.inc}

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinPerMeterQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TKelvinPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TMeterQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattPerMeterQty; const ARight: TWattPerMeterPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerMeterPerKelvinQty): TMeterKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TWattPerMeterPerKelvinQty; const ARight: TMeterKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerMeterPerKelvinQty.*(const ALeft: TMeterKelvinQty; const ARight: TWattPerMeterPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterKelvinName}
{$DEFINE CPLURALNAME:=rsSquareMeterKelvinPluralName}
{$DEFINE CPREFIXES:=cSquareMeterKelvinPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsWattPerSquareMeterPerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerSquareMeterPerKelvinName}
{$DEFINE CPLURALNAME:=rsWattPerSquareMeterPerKelvinPluralName}
{$DEFINE CPREFIXES:=cWattPerSquareMeterPerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cWattPerSquareMeterPerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$i adim.inc}

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerKelvinQty): TSquareMeterKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TWattPerSquareMeterPerKelvinQty; const ARight: TSquareMeterKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerKelvinQty.*(const ALeft: TSquareMeterKelvinQty; const ARight: TWattPerSquareMeterPerKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterQuarticKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterQuarticKelvinName}
{$DEFINE CPLURALNAME:=rsSquareMeterQuarticKelvinPluralName}
{$DEFINE CPREFIXES:=cSquareMeterQuarticKelvinPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterQuarticKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsWattPerQuarticKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerQuarticKelvinName}
{$DEFINE CPLURALNAME:=rsWattPerQuarticKelvinPluralName}
{$DEFINE CPREFIXES:=cWattPerQuarticKelvinPrefixes}
{$DEFINE CEXPONENTS:=cWattPerQuarticKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$i adim.inc}

class operator TWattPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerQuarticKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TWattPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerSquareMeterPerQuarticKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerSquareMeterPerQuarticKelvinName}
{$DEFINE CPLURALNAME:=rsWattPerSquareMeterPerQuarticKelvinPluralName}
{$DEFINE CPREFIXES:=cWattPerSquareMeterPerQuarticKelvinPrefixes}
{$DEFINE CEXPONENTS:=cWattPerSquareMeterPerQuarticKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$i adim.inc}

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TSquareMeterQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TWattPerSquareMeterPerQuarticKelvinQty; const ARight: TSquareMeterQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerQuarticKelvinQty.*(const ALeft: TSquareMeterQuarticKelvinQty; const ARight: TWattPerSquareMeterPerQuarticKelvinQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsJoulePerMolePerKelvinSymbol}
{$DEFINE CSINGULARNAME:=rsJoulePerMolePerKelvinName}
{$DEFINE CPLURALNAME:=rsJoulePerMolePerKelvinPluralName}
{$DEFINE CPREFIXES:=cJoulePerMolePerKelvinPrefixes}
{$DEFINE CEXPONENTS:=cJoulePerMolePerKelvinExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$i adim.inc}

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerMoleQty; const ARight: TJoulePerMolePerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TKelvinQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJoulePerKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleQty; const ARight: TJoulePerMolePerKelvinQty): TJoulePerKelvinQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty./(const ALeft: TJouleQty; const ARight: TJoulePerMolePerKelvinQty): TMoleKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TJoulePerMolePerKelvinQty; const ARight: TMoleKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TJoulePerMolePerKelvinQty.*(const ALeft: TMoleKelvinQty; const ARight: TJoulePerMolePerKelvinQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsOhmMeterSymbol}
{$DEFINE CSINGULARNAME:=rsOhmMeterName}
{$DEFINE CPLURALNAME:=rsOhmMeterPluralName}
{$DEFINE CPREFIXES:=cOhmMeterPrefixes}
{$DEFINE CEXPONENTS:=cOhmMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TOhmMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsVoltPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsVoltPerMeterName}
{$DEFINE CPLURALNAME:=rsVoltPerMeterPluralName}
{$DEFINE CPREFIXES:=cVoltPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cVoltPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltPerMeterQty}{$i adim.inc}

class operator TVoltPerMeterQty./(const ALeft: TVoltMeterQty; const ARight: TVoltPerMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TVoltPerMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TSquareMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TMeterPerSecondQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TTeslaQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TNewtonQty; const ARight: TVoltPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TCoulombQty; const ARight: TVoltPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty./(const ALeft: TVoltQty; const ARight: TVoltPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltPerMeterQty.*(const ALeft: TMeterQty; const ARight: TVoltPerMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonPerCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerCoulombName}
{$DEFINE CPLURALNAME:=rsNewtonPerCoulombPluralName}
{$DEFINE CPREFIXES:=cNewtonPerCoulombPrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerCoulombQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsCoulombPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombPerMeterName}
{$DEFINE CPLURALNAME:=rsCoulombPerMeterPluralName}
{$DEFINE CPREFIXES:=cCoulombPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cCoulombPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$i adim.inc}

class operator TCoulombPerMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerMeterQty.*(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareCoulombPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSquareCoulombPerMeterName}
{$DEFINE CPLURALNAME:=rsSquareCoulombPerMeterPluralName}
{$DEFINE CPREFIXES:=cSquareCoulombPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cSquareCoulombPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$i adim.inc}

class operator TSquareCoulombPerMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TJouleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TJouleQty; const ARight: TSquareCoulombPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombPerMeterQty; const ARight: TCoulombPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty./(const ALeft: TSquareCoulombQty; const ARight: TSquareCoulombPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TSquareCoulombPerMeterQty; const ARight: TMeterQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareCoulombPerMeterQty.*(const ALeft: TMeterQty; const ARight: TSquareCoulombPerMeterQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCoulombPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsCoulombPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cCoulombPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cCoulombPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$i adim.inc}

class operator TCoulombPerSquareMeterQty.*(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TCoulombPerSquareMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TVoltPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TCoulombPerSquareMeterQty; const ARight: TSquareMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerSquareMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterPerSquareCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterPerSquareCoulombName}
{$DEFINE CPLURALNAME:=rsSquareMeterPerSquareCoulombPluralName}
{$DEFINE CPREFIXES:=cSquareMeterPerSquareCoulombPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterPerSquareCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$i adim.inc}

class operator TSquareMeterPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty./(const ALeft: TSquareMeterQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareMeterPerSquareCoulombQty; const ARight: TSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareMeterPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TSquareMeterPerSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonPerSquareCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerSquareCoulombName}
{$DEFINE CPLURALNAME:=rsNewtonPerSquareCoulombPluralName}
{$DEFINE CPREFIXES:=cNewtonPerSquareCoulombPrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerSquareCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$i adim.inc}

class operator TNewtonPerSquareCoulombQty./(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TSquareMeterQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty./(const ALeft: TNewtonQty; const ARight: TNewtonPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TNewtonPerSquareCoulombQty; const ARight: TSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TNewtonPerSquareCoulombQty.*(const ALeft: TSquareCoulombQty; const ARight: TNewtonPerSquareCoulombQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsNewtonSquareMeterPerSquareCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonSquareMeterPerSquareCoulombName}
{$DEFINE CPLURALNAME:=rsNewtonSquareMeterPerSquareCoulombPluralName}
{$DEFINE CPREFIXES:=cNewtonSquareMeterPerSquareCoulombPrefixes}
{$DEFINE CEXPONENTS:=cNewtonSquareMeterPerSquareCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsVoltMeterSymbol}
{$DEFINE CSINGULARNAME:=rsVoltMeterName}
{$DEFINE CPLURALNAME:=rsVoltMeterPluralName}
{$DEFINE CPREFIXES:=cVoltMeterPrefixes}
{$DEFINE CEXPONENTS:=cVoltMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonSquareMeterPerCoulombSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonSquareMeterPerCoulombName}
{$DEFINE CPLURALNAME:=rsNewtonSquareMeterPerCoulombPluralName}
{$DEFINE CPREFIXES:=cNewtonSquareMeterPerCoulombPrefixes}
{$DEFINE CEXPONENTS:=cNewtonSquareMeterPerCoulombExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonSquareMeterPerCoulombQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsVoltMeterPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsVoltMeterPerSecondName}
{$DEFINE CPLURALNAME:=rsVoltMeterPerSecondPluralName}
{$DEFINE CPREFIXES:=cVoltMeterPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cVoltMeterPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$i adim.inc}

class operator TVoltMeterPerSecondQty./(const ALeft: TVoltMeterQty; const ARight: TVoltMeterPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TVoltMeterPerSecondQty; const ARight: TSecondQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TVoltMeterPerSecondQty.*(const ALeft: TSecondQty; const ARight: TVoltMeterPerSecondQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsFaradPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsFaradPerMeterName}
{$DEFINE CPLURALNAME:=rsFaradPerMeterPluralName}
{$DEFINE CPREFIXES:=cFaradPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cFaradPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TFaradPerMeterQty}{$i adim.inc}

class operator TFaradPerMeterQty./(const ALeft: double; const ARight: TFaradPerMeterQty): TNewtonSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TFaradPerMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltPerMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltPerMeterQty; const ARight: TFaradPerMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TCoulombQty; const ARight: TFaradPerMeterQty): TVoltMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TVoltMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TVoltMeterQty; const ARight: TFaradPerMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty./(const ALeft: TFaradQty; const ARight: TFaradPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TFaradPerMeterQty; const ARight: TMeterQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TFaradPerMeterQty.*(const ALeft: TMeterQty; const ARight: TFaradPerMeterQty): TFaradQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsAmperePerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsAmperePerMeterName}
{$DEFINE CPLURALNAME:=rsAmperePerMeterPluralName}
{$DEFINE CPREFIXES:=cAmperePerMeterPrefixes}
{$DEFINE CEXPONENTS:=cAmperePerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerMeterQty}{$i adim.inc}

class operator TAmperePerMeterQty./(const ALeft: TAmpereQty; const ARight: TAmperePerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmperePerMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMeterPerAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsMeterPerAmpereName}
{$DEFINE CPLURALNAME:=rsMeterPerAmperePluralName}
{$DEFINE CPREFIXES:=cMeterPerAmperePrefixes}
{$DEFINE CEXPONENTS:=cMeterPerAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$i adim.inc}

class operator TMeterPerAmpereQty./(const ALeft: TMeterQty; const ARight: TMeterPerAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TMeterPerAmpereQty; const ARight: TAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMeterPerAmpereQty.*(const ALeft: TAmpereQty; const ARight: TMeterPerAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsTeslaMeterSymbol}
{$DEFINE CSINGULARNAME:=rsTeslaMeterName}
{$DEFINE CPLURALNAME:=rsTeslaMeterPluralName}
{$DEFINE CPREFIXES:=cTeslaMeterPrefixes}
{$DEFINE CEXPONENTS:=cTeslaMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonPerAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerAmpereName}
{$DEFINE CPLURALNAME:=rsNewtonPerAmperePluralName}
{$DEFINE CPREFIXES:=cNewtonPerAmperePrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerAmpereQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsTeslaPerAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsTeslaPerAmpereName}
{$DEFINE CPLURALNAME:=rsTeslaPerAmperePluralName}
{$DEFINE CPREFIXES:=cTeslaPerAmperePrefixes}
{$DEFINE CEXPONENTS:=cTeslaPerAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$i adim.inc}

class operator TTeslaPerAmpereQty./(const ALeft: TTeslaQty; const ARight: TTeslaPerAmpereQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TTeslaPerAmpereQty.*(const ALeft: TTeslaPerAmpereQty; const ARight: TAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TTeslaPerAmpereQty.*(const ALeft: TAmpereQty; const ARight: TTeslaPerAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsHenryPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsHenryPerMeterName}
{$DEFINE CPLURALNAME:=rsHenryPerMeterPluralName}
{$DEFINE CPREFIXES:=cHenryPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cHenryPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=THenryPerMeterQty}{$i adim.inc}

class operator THenryPerMeterQty./(const ALeft: TNewtonQty; const ARight: THenryPerMeterQty): TSquareAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TSquareAmpereQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TSquareAmpereQty; const ARight: THenryPerMeterQty): TNewtonQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: TTeslaQty; const ARight: THenryPerMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmperePerMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TAmperePerMeterQty; const ARight: THenryPerMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterPerAmpereQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TTeslaQty): TMeterPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): TTeslaPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: THenryPerMeterQty; const ARight: TTeslaPerAmpereQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: TTeslaMeterQty; const ARight: THenryPerMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TAmpereQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TAmpereQty; const ARight: THenryPerMeterQty): TTeslaMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty./(const ALeft: THenryQty; const ARight: THenryPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: THenryPerMeterQty; const ARight: TMeterQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator THenryPerMeterQty.*(const ALeft: TMeterQty; const ARight: THenryPerMeterQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsTeslaMeterPerAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsTeslaMeterPerAmpereName}
{$DEFINE CPLURALNAME:=rsTeslaMeterPerAmperePluralName}
{$DEFINE CPREFIXES:=cTeslaMeterPerAmperePrefixes}
{$DEFINE CEXPONENTS:=cTeslaMeterPerAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TTeslaMeterPerAmpereQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsNewtonPerSquareAmpereSymbol}
{$DEFINE CSINGULARNAME:=rsNewtonPerSquareAmpereName}
{$DEFINE CPLURALNAME:=rsNewtonPerSquareAmperePluralName}
{$DEFINE CPREFIXES:=cNewtonPerSquareAmperePrefixes}
{$DEFINE CEXPONENTS:=cNewtonPerSquareAmpereExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TNewtonPerSquareAmpereQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsRadianPerMeterSymbol}
{$DEFINE CSINGULARNAME:=rsRadianPerMeterName}
{$DEFINE CPLURALNAME:=rsRadianPerMeterPluralName}
{$DEFINE CPREFIXES:=cRadianPerMeterPrefixes}
{$DEFINE CEXPONENTS:=cRadianPerMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TRadianPerMeterQty}{$i adim.inc}

class operator TRadianPerMeterQty./(const ALeft: TRadianQty; const ARight: TRadianPerMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TRadianPerMeterQty.*(const ALeft: TRadianPerMeterQty; const ARight: TMeterQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerMeterQty.*(const ALeft: TMeterQty; const ARight: TRadianPerMeterQty): TRadianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareSecondPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsSquareSecondPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsSquareSecondPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cSquareSecondPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cSquareSecondPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$i adim.inc}

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: THenryPerMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TFaradPerMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: double; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty./(const ALeft: TSquareSecondQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareSecondPerSquareMeterQty; const ARight: TSquareMeterQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TSquareSecondPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TSquareSecondPerSquareMeterQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareJouleSymbol}
{$DEFINE CSINGULARNAME:=rsSquareJouleName}
{$DEFINE CPLURALNAME:=rsSquareJoulePluralName}
{$DEFINE CPREFIXES:=cSquareJoulePrefixes}
{$DEFINE CEXPONENTS:=cSquareJouleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareJouleQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsJouleSecondSymbol}
{$DEFINE CSINGULARNAME:=rsJouleSecondName}
{$DEFINE CPLURALNAME:=rsJouleSecondPluralName}
{$DEFINE CPREFIXES:=cJouleSecondPrefixes}
{$DEFINE CEXPONENTS:=cJouleSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TJouleSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsElettronvoltSecondSymbol}
{$DEFINE CSINGULARNAME:=rsElettronvoltSecondName}
{$DEFINE CPLURALNAME:=rsElettronvoltSecondPluralName}
{$DEFINE CPREFIXES:=cElettronvoltSecondPrefixes}
{$DEFINE CEXPONENTS:=cElettronvoltSecondExponents}
{$DEFINE CFACTOR:=cElettronvoltSecondFactor}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsSquareJouleSquareSecondSymbol}
{$DEFINE CSINGULARNAME:=rsSquareJouleSquareSecondName}
{$DEFINE CPLURALNAME:=rsSquareJouleSquareSecondPluralName}
{$DEFINE CPREFIXES:=cSquareJouleSquareSecondPrefixes}
{$DEFINE CEXPONENTS:=cSquareJouleSquareSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareJouleSquareSecondQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsLumenPerWattSymbol}
{$DEFINE CSINGULARNAME:=rsLumenPerWattName}
{$DEFINE CPLURALNAME:=rsLumenPerWattPluralName}
{$DEFINE CPREFIXES:=cLumenPerWattPrefixes}
{$DEFINE CEXPONENTS:=cLumenPerWattExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TLumenPerWattQty}{$i adim.inc}

class operator TLumenPerWattQty./(const ALeft: TLumenQty; const ARight: TLumenPerWattQty): TWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TLumenPerWattQty.*(const ALeft: TLumenPerWattQty; const ARight: TWattQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TLumenPerWattQty.*(const ALeft: TWattQty; const ARight: TLumenPerWattQty): TLumenQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsReciprocalMoleSymbol}
{$DEFINE CSINGULARNAME:=rsReciprocalMoleName}
{$DEFINE CPLURALNAME:=rsReciprocalMolePluralName}
{$DEFINE CPREFIXES:=cReciprocalMolePrefixes}
{$DEFINE CEXPONENTS:=cReciprocalMoleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TReciprocalMoleQty}{$i adim.inc}

class operator TReciprocalMoleQty./(const ALeft: double; const ARight: TReciprocalMoleQty): TMoleQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsAmperePerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsAmperePerSquareMeterName}
{$DEFINE CPLURALNAME:=rsAmperePerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cAmperePerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cAmperePerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$i adim.inc}

class operator TAmperePerSquareMeterQty./(const ALeft: TAmperePerMeterQty; const ARight: TAmperePerSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TMeterQty; const ARight: TAmperePerSquareMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty./(const ALeft: TAmpereQty; const ARight: TAmperePerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TAmperePerSquareMeterQty; const ARight: TSquareMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TAmperePerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TAmperePerSquareMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsMolePerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsMolePerCubicMeterName}
{$DEFINE CPLURALNAME:=rsMolePerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cMolePerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cMolePerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$i adim.inc}

class operator TMolePerCubicMeterQty./(const ALeft: TMoleQty; const ARight: TMolePerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TMolePerCubicMeterQty.*(const ALeft: TMolePerCubicMeterQty; const ARight: TCubicMeterQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TMolePerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TMolePerCubicMeterQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCandelaPerSquareMeterSymbol}
{$DEFINE CSINGULARNAME:=rsCandelaPerSquareMeterName}
{$DEFINE CPLURALNAME:=rsCandelaPerSquareMeterPluralName}
{$DEFINE CPREFIXES:=cCandelaPerSquareMeterPrefixes}
{$DEFINE CEXPONENTS:=cCandelaPerSquareMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$i adim.inc}

class operator TCandelaPerSquareMeterQty./(const ALeft: TCandelaQty; const ARight: TCandelaPerSquareMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TCandelaPerSquareMeterQty; const ARight: TSquareMeterQty): TCandelaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCandelaPerSquareMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCandelaPerSquareMeterQty): TCandelaQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCoulombPerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombPerCubicMeterName}
{$DEFINE CPLURALNAME:=rsCoulombPerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cCoulombPerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cCoulombPerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$i adim.inc}

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombPerSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombPerCubicMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TSquareMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TSquareMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCoulombPerCubicMeterQty; const ARight: TCubicMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TCoulombPerCubicMeterQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCoulombPerKilogramSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombPerKilogramName}
{$DEFINE CPLURALNAME:=rsCoulombPerKilogramPluralName}
{$DEFINE CPREFIXES:=cCoulombPerKilogramPrefixes}
{$DEFINE CEXPONENTS:=cCoulombPerKilogramExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$i adim.inc}

class operator TCoulombPerKilogramQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerKilogramQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerKilogramQty.*(const ALeft: TCoulombPerKilogramQty; const ARight: TKilogramQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerKilogramQty.*(const ALeft: TKilogramQty; const ARight: TCoulombPerKilogramQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsGrayPerSecondSymbol}
{$DEFINE CSINGULARNAME:=rsGrayPerSecondName}
{$DEFINE CPLURALNAME:=rsGrayPerSecondPluralName}
{$DEFINE CPREFIXES:=cGrayPerSecondPrefixes}
{$DEFINE CEXPONENTS:=cGrayPerSecondExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TGrayPerSecondQty}{$i adim.inc}

class operator TGrayPerSecondQty./(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TGrayPerSecondQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TGrayPerSecondQty.*(const ALeft: TGrayPerSecondQty; const ARight: TSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TGrayPerSecondQty.*(const ALeft: TSecondQty; const ARight: TGrayPerSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsWattPerSteradianSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerSteradianName}
{$DEFINE CPLURALNAME:=rsWattPerSteradianPluralName}
{$DEFINE CPREFIXES:=cWattPerSteradianPrefixes}
{$DEFINE CEXPONENTS:=cWattPerSteradianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSteradianQty}{$i adim.inc}

class operator TWattPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSteradianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSteradianQty.*(const ALeft: TWattPerSteradianQty; const ARight: TSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsSquareMeterSteradianSymbol}
{$DEFINE CSINGULARNAME:=rsSquareMeterSteradianName}
{$DEFINE CPLURALNAME:=rsSquareMeterSteradianPluralName}
{$DEFINE CPREFIXES:=cSquareMeterSteradianPrefixes}
{$DEFINE CEXPONENTS:=cSquareMeterSteradianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$i adim.inc}

{$DEFINE CSYMBOL:=rsWattPerSquareMeterPerSteradianSymbol}
{$DEFINE CSINGULARNAME:=rsWattPerSquareMeterPerSteradianName}
{$DEFINE CPLURALNAME:=rsWattPerSquareMeterPerSteradianPluralName}
{$DEFINE CPREFIXES:=cWattPerSquareMeterPerSteradianPrefixes}
{$DEFINE CEXPONENTS:=cWattPerSquareMeterPerSteradianExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$i adim.inc}

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSteradianQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattPerSquareMeterQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSteradianQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty./(const ALeft: TWattQty; const ARight: TWattPerSquareMeterPerSteradianQty): TSquareMeterSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TWattPerSquareMeterPerSteradianQty; const ARight: TSquareMeterSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TWattPerSquareMeterPerSteradianQty.*(const ALeft: TSquareMeterSteradianQty; const ARight: TWattPerSquareMeterPerSteradianQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsKatalPerCubicMeterSymbol}
{$DEFINE CSINGULARNAME:=rsKatalPerCubicMeterName}
{$DEFINE CPLURALNAME:=rsKatalPerCubicMeterPluralName}
{$DEFINE CPREFIXES:=cKatalPerCubicMeterPrefixes}
{$DEFINE CEXPONENTS:=cKatalPerCubicMeterExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$i adim.inc}

class operator TKatalPerCubicMeterQty./(const ALeft: TKatalQty; const ARight: TKatalPerCubicMeterQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TKatalPerCubicMeterQty; const ARight: TCubicMeterQty): TKatalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TKatalPerCubicMeterQty.*(const ALeft: TCubicMeterQty; const ARight: TKatalPerCubicMeterQty): TKatalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

{$DEFINE CSYMBOL:=rsCoulombPerMoleSymbol}
{$DEFINE CSINGULARNAME:=rsCoulombPerMoleName}
{$DEFINE CPLURALNAME:=rsCoulombPerMolePluralName}
{$DEFINE CPREFIXES:=cCoulombPerMolePrefixes}
{$DEFINE CEXPONENTS:=cCoulombPerMoleExponents}
{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$i adim.inc}

class operator TCoulombPerMoleQty./(const ALeft: TJoulePerMoleQty; const ARight: TCoulombPerMoleQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerMoleQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TVoltQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMoleQty.*(const ALeft: TVoltQty; const ARight: TCoulombPerMoleQty): TJoulePerMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMoleQty./(const ALeft: TCoulombQty; const ARight: TCoulombPerMoleQty): TMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

class operator TCoulombPerMoleQty.*(const ALeft: TCoulombPerMoleQty; const ARight: TMoleQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TCoulombPerMoleQty.*(const ALeft: TMoleQty; const ARight: TCoulombPerMoleQty): TCoulombQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

class operator TRadianPerSecondQty.:=(const AQuantity: THertzQty): TRadianPerSecondQty;
begin
  result.FValue := AQuantity.FValue;
end;

class operator TRadianPerSecondQty.:=(const AQuantity: TRadianPerSecondQty): THertzQty;
begin
  result.FValue := AQuantity.FValue;
end;

{ External Operators }

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterQty; const ARight: TCubicMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeterQty; const ARight: TQuarticMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuinticMeterQty; const ARight: TCubicMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeterQty; const ARight: TQuinticMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSexticMeterQty; const ARight: TQuarticMeterQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicKelvinQty; const ARight: TSquareKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticKelvinQty; const ARight: TCubicKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: THertzQty): TSecondQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareSecondQty): TSquareHertzQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: THertzQty; const ARight: TSecondQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: THertzQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TSecondQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TRadianQty; const ARight: THertzQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: TRadianQty): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TRadianPerSecondQty; const ARight: TSecondQty): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TSquareSecondQty): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TRadianQty; const ARight: TSquareHertzQty): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertzQty; const ARight: TRadianQty): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSteradianQty; const ARight: TSquareSecondQty): TSteradianPerSquareSecondQty;
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

operator /(const ALeft: TMeterQty; const ARight: TSecondQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: THertzQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: TMeterQty): TMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TMeterPerSecondQty; const ARight: TSecondQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterQty; const ARight: TSquareSecondQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TSquareHertzQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertzQty; const ARight: TMeterQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareSecondQty): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TMeterQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterQty; const ARight: TMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramPerSecondQty; const ARight: TMeterQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TKilogramPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TKilogramQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TMeterQty): TReciprocalMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSquareMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
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

operator *(const ALeft: TKilogramMeterPerSecondQty; const ARight: TMeterQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TKilogramMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterPerSecondQty; const ARight: TReciprocalMeterQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSecondQty; const ARight: TMeterQty): TSecondPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TMeterQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSecondPerMeterQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSecondPerMeterQty; const ARight: TKilogramPerSecondQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TMeterPerSecondQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TSquareMeterQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TCubicMeterQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSquareMeterQty; const ARight: TMeterQty): TKilogramPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TKilogramQty): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSecondQty; const ARight: TNewtonQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonQty; const ARight: TSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TKilogramMeterQty; const ARight: TNewtonQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TSquareMeterPerSquareSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TCubicMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TMeterPerSecondQty): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TSquareMeterPerSquareSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramSquareMeterQty): TSquareHertzQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareMeterQty; const ARight: TJouleQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSecondQty; const ARight: TJouleQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TJouleQty; const ARight: TSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: THertzQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramSquareMeterPerSquareSecondQty; const ARight: TJouleQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TRadianQty): TJoulePerRadianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TSecondQty): TWattQty;
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

operator *(const ALeft: TKilogramPerSecondQty; const ARight: TSquareMeterPerSquareSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKilogramPerSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonQty; const ARight: TMeterPerSecondQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TNewtonQty): TWattQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TSecondQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TCoulombQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TAmpereQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TVoltQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombQty; const ARight: TJouleQty): TFaradQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSecondQty; const ARight: TFaradQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareAmpereQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareVoltQty; const ARight: TWattQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltQty; const ARight: TAmpereQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TOhmQty): TSiemensQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TFaradQty; const ARight: TSecondQty): TSiemensQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TFaradQty; const ARight: THertzQty): TSiemensQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THertzQty; const ARight: TFaradQty): TSiemensQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TAmpereQty; const ARight: TVoltQty): TSiemensQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TCoulombQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TSquareMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TSecondQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TCoulombQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWeberQty; const ARight: TAmpereQty): THenryQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TOhmQty; const ARight: TSecondQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSecondQty; const ARight: TOhmQty): THenryQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TOhmQty; const ARight: THertzQty): THenryQty;
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

operator *(const ALeft: TSecondQty; const ARight: TKatalQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKatalQty; const ARight: TSecondQty): TMoleQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TCubicMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TKilogramPerCubicMeterQty; const ARight: TMeterPerSquareSecondQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TKilogramPerCubicMeterQty): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TSquareMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TPascalQty; const ARight: TMeterQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TPascalQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramQty; const ARight: TSquareHertzQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareHertzQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSecondQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TSecondQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TMeterPerSecondQty): TKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TPoiseuilleQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TPoiseuilleQty; const ARight: TMeterQty): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSecondQty): TSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPoiseuilleQty; const ARight: TKilogramPerCubicMeterQty): TSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TQuarticMeterSecondQty; const ARight: TQuarticMeterQty): TSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramPerSecondQty; const ARight: TQuarticMeterQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramPerQuarticMeterQty; const ARight: TSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKilogramQty; const ARight: TQuarticMeterSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TPascalQty; const ARight: TCubicMeterPerSecondQty): TKilogramPerQuarticMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TKilogramQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TKilogramPerCubicMeterQty): TCubicMeterPerKilogramQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TKilogramSquareSecondQty; const ARight: TSquareSecondQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCubicMeterQty; const ARight: TSquareSecondQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TMeterPerSquareSecondQty; const ARight: TSquareMeterQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TSquareMeterQty; const ARight: TMeterPerSquareSecondQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonQty): TSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TJouleQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TQuarticMeterQty): TPascalQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TMeterPerSecondQty): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TSquareMeterQty): TNewtonMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonCubicMeterQty; const ARight: TNewtonSquareMeterQty): TMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareKilogramQty): TNewtonPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TMeterQty): TSquareKilogramPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareMeterQty): TSquareKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareKilogramQty): TSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TNewtonQty): TSquareMeterPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TSquareMeterQty): TNewtonPerSquareKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TSquareKilogramPerMeterQty;
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

operator *(const ALeft: TKilogramSquareSecondQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramSquareSecondQty): TCubicMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TKilogramQty; const ARight: TNewtonSquareMeterPerSquareKilogramQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TNewtonSquareMeterPerSquareKilogramQty; const ARight: TKilogramQty): TCubicMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TKelvinQty): TReciprocalKelvinQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TKilogramKelvinQty; const ARight: TKilogramQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TJoulePerKelvinQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TKilogramKelvinQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TKelvinQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TKilogramQty): TJoulePerKilogramPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterKelvinQty; const ARight: TMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKelvinQty; const ARight: TMeterQty): TKelvinPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TMeterQty): TWattPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQty): TWattPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TKelvinQty): TWattPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TMeterKelvinQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerMeterQty; const ARight: TKelvinQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TMeterQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinPerMeterQty): TWattPerMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterKelvinQty; const ARight: TSquareMeterQty): TKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSquareMeterKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TKelvinQty): TWattPerSquareMeterPerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerKelvinQty;
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

operator /(const ALeft: TWattQty; const ARight: TSquareMeterQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TQuarticKelvinQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerQuarticKelvinQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerQuarticKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TJoulePerMoleQty): TMoleQty;
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

operator /(const ALeft: TJoulePerKelvinQty; const ARight: TMoleQty): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerMoleQty; const ARight: TKelvinQty): TJoulePerMolePerKelvinQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TOhmMeterQty; const ARight: TMeterQty): TOhmQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltQty; const ARight: TMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TCoulombQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslaQty; const ARight: TMeterPerSecondQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerSecondQty; const ARight: TTeslaQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TMeterQty): TCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareCoulombQty; const ARight: TMeterQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TCoulombPerMeterQty; const ARight: TCoulombQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TCoulombQty; const ARight: TCoulombPerMeterQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TSquareMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TMeterQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterQty; const ARight: TSquareCoulombQty): TSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareCoulombQty): TNewtonPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TNewtonQty): TSquareMeterPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonSquareMeterPerSquareCoulombQty; const ARight: TSquareMeterQty): TNewtonPerSquareCoulombQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltPerMeterQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TCoulombPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJouleQty; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TSquareCoulombPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeterQty; const ARight: TMeterQty): TVoltQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeterQty; const ARight: TSquareMeterQty): TVoltPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TVoltMeterQty; const ARight: TSecondQty): TVoltMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TFaradQty; const ARight: TMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TVoltMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TVoltPerMeterQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TNewtonSquareMeterPerSquareCoulombQty): TFaradPerMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TAmpereQty; const ARight: TMeterQty): TAmperePerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMeterQty; const ARight: TAmpereQty): TMeterPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeterQty; const ARight: TMeterQty): TTeslaQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TTeslaMeterQty): TAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaQty; const ARight: TAmpereQty): TTeslaPerAmpereQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: THenryQty; const ARight: TMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TTeslaMeterQty; const ARight: TAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator *(const ALeft: TTeslaPerAmpereQty; const ARight: TMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterQty; const ARight: TTeslaPerAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TTeslaQty; const ARight: TMeterPerAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: TMeterPerAmpereQty; const ARight: TTeslaQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TTeslaQty; const ARight: TAmperePerMeterQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TNewtonQty; const ARight: TSquareAmpereQty): THenryPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TRadianQty; const ARight: TMeterQty): TRadianPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondQty): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareKilogramPerSquareSecondQty; const ARight: TKilogramQty): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareSecondQty; const ARight: TSquareMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TSquareMeterPerSquareSecondQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator *(const ALeft: TFaradPerMeterQty; const ARight: THenryPerMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator *(const ALeft: THenryPerMeterQty; const ARight: TFaradPerMeterQty): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue * ARight.FValue;
end;

operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TSquareJouleQty): TSquareSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareJouleSquareSecondQty; const ARight: TNewtonCubicMeterQty): TKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TLumenQty; const ARight: TWattQty): TLumenPerWattQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: double; const ARight: TMoleQty): TReciprocalMoleQty;
begin
  result.FValue := ALeft / ARight.FValue;
end;

operator /(const ALeft: TAmpereQty; const ARight: TSquareMeterQty): TAmperePerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TAmperePerMeterQty; const ARight: TMeterQty): TAmperePerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TMoleQty; const ARight: TCubicMeterQty): TMolePerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCandelaQty; const ARight: TSquareMeterQty): TCandelaPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TCubicMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerMeterQty; const ARight: TSquareMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombPerSquareMeterQty; const ARight: TMeterQty): TCoulombPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TKilogramQty): TCoulombPerKilogramQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TSquareMeterPerSquareSecondQty; const ARight: TSecondQty): TGrayPerSecondQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattQty; const ARight: TSteradianQty): TWattPerSteradianQty;
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

operator /(const ALeft: TWattPerSquareMeterQty; const ARight: TSteradianQty): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TWattPerSteradianQty; const ARight: TSquareMeterQty): TWattPerSquareMeterPerSteradianQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TKatalQty; const ARight: TCubicMeterQty): TKatalPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TCoulombQty; const ARight: TMoleQty): TCoulombPerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

operator /(const ALeft: TJoulePerMoleQty; const ARight: TVoltQty): TCoulombPerMoleQty;
begin
  result.FValue := ALeft.FValue / ARight.FValue;
end;

{ TUnit classes }

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerKelvinQty}{$DEFINE TUnit:=TJoulePerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKatalQty}{$DEFINE TUnit:=TKatalUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterQty}{$DEFINE TUnit:=TMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerMoleQty}{$DEFINE TUnit:=TJoulePerMoleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicKelvinQty}{$DEFINE TUnit:=TCubicKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMoleKelvinQty}{$DEFINE TUnit:=TMoleKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonPerSquareKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TSquareMeterPerSquareKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=THertzUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKelvinQty}{$DEFINE TUnit:=TSquareKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareMeterPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSecondQty}{$DEFINE TUnit:=TSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDayQty}{$DEFINE TUnit:=TDayUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THourQty}{$DEFINE TUnit:=THourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMinuteQty}{$DEFINE TUnit:=TMinuteUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareSecondQty}{$DEFINE TUnit:=TSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareDayQty}{$DEFINE TUnit:=TSquareDayUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareHourQty}{$DEFINE TUnit:=TSquareHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMinuteQty}{$DEFINE TUnit:=TSquareMinuteUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAstronomicalQty}{$DEFINE TUnit:=TAstronomicalUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TInchQty}{$DEFINE TUnit:=TInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFootQty}{$DEFINE TUnit:=TFootUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TYardQty}{$DEFINE TUnit:=TYardUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMileQty}{$DEFINE TUnit:=TMileUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNauticalMileQty}{$DEFINE TUnit:=TNauticalMileUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAngstromQty}{$DEFINE TUnit:=TAngstromUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterQty}{$DEFINE TUnit:=TSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareInchQty}{$DEFINE TUnit:=TSquareInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareFootQty}{$DEFINE TUnit:=TSquareFootUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareYardQty}{$DEFINE TUnit:=TSquareYardUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMileQty}{$DEFINE TUnit:=TSquareMileUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterQty}{$DEFINE TUnit:=TCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicInchQty}{$DEFINE TUnit:=TCubicInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicFootQty}{$DEFINE TUnit:=TCubicFootUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicYardQty}{$DEFINE TUnit:=TCubicYardUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLitreQty}{$DEFINE TUnit:=TLitreUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TGallonQty}{$DEFINE TUnit:=TGallonUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticMeterQty}{$DEFINE TUnit:=TQuarticMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuinticMeterQty}{$DEFINE TUnit:=TQuinticMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSexticMeterQty}{$DEFINE TUnit:=TSexticMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramQty}{$DEFINE TUnit:=TKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTonneQty}{$DEFINE TUnit:=TTonneUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundQty}{$DEFINE TUnit:=TPoundUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOunceQty}{$DEFINE TUnit:=TOunceUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TStoneQty}{$DEFINE TUnit:=TStoneUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTonQty}{$DEFINE TUnit:=TTonUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramQty}{$DEFINE TUnit:=TSquareKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmpereQty}{$DEFINE TUnit:=TAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareAmpereQty}{$DEFINE TUnit:=TSquareAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKelvinQty}{$DEFINE TUnit:=TKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeCelsiusQty}{$DEFINE TUnit:=TDegreeCelsiusUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeFahrenheitQty}{$DEFINE TUnit:=TDegreeFahrenheitUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticKelvinQty}{$DEFINE TUnit:=TQuarticKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMoleQty}{$DEFINE TUnit:=TMoleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCandelaQty}{$DEFINE TUnit:=TCandelaUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianQty}{$DEFINE TUnit:=TRadianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TDegreeQty}{$DEFINE TUnit:=TDegreeUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSteradianQty}{$DEFINE TUnit:=TSteradianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareDegreeQty}{$DEFINE TUnit:=TSquareDegreeUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareHertzQty}{$DEFINE TUnit:=TSquareHertzUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerSecondQty}{$DEFINE TUnit:=TRadianPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerSquareSecondQty}{$DEFINE TUnit:=TRadianPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSteradianPerSquareSecondQty}{$DEFINE TUnit:=TSteradianPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSecondQty}{$DEFINE TUnit:=TMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerHourQty}{$DEFINE TUnit:=TMeterPerHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMilePerHourQty}{$DEFINE TUnit:=TMilePerHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNauticalMilePerHourQty}{$DEFINE TUnit:=TNauticalMilePerHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$DEFINE TUnit:=TMeterPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerSquareSecondQty}{$DEFINE TUnit:=TMeterPerSecondPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerHourPerSecondQty}{$DEFINE TUnit:=TMeterPerHourPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramMeterQty}{$DEFINE TUnit:=TKilogramMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerSecondQty}{$DEFINE TUnit:=TKilogramPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TKilogramMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSquareKilogramSquareMeterPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramMeterPerSecondQty}{$DEFINE TUnit:=TNewtonSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalMeterQty}{$DEFINE TUnit:=TReciprocalMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterQty}{$DEFINE TUnit:=TKilogramSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TKilogramSquareMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TNewtonMeterSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSecondPerMeterQty}{$DEFINE TUnit:=TSecondPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerMeterQty}{$DEFINE TUnit:=TKilogramPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerSquareMeterQty}{$DEFINE TUnit:=TKilogramPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerCubicMeterQty}{$DEFINE TUnit:=TKilogramPerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundPerCubicInchQty}{$DEFINE TUnit:=TPoundPerCubicInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonQty}{$DEFINE TUnit:=TNewtonUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForceQty}{$DEFINE TUnit:=TPoundForceUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareNewtonQty}{$DEFINE TUnit:=TSquareNewtonUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TPascalUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TBarQty}{$DEFINE TUnit:=TBarUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundPerSquareInchQty}{$DEFINE TUnit:=TPoundPerSquareInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPascalQty}{$DEFINE TUnit:=TJoulePerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TJouleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattHourQty}{$DEFINE TUnit:=TWattHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TElettronvoltQty}{$DEFINE TUnit:=TElettronvoltUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJouleQty}{$DEFINE TUnit:=TNewtonMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForceInchQty}{$DEFINE TUnit:=TPoundForceInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRydbergQty}{$DEFINE TUnit:=TRydbergUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCalorieQty}{$DEFINE TUnit:=TCalorieUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TJoulePerRadianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerDegreeQty}{$DEFINE TUnit:=TJoulePerDegreeUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerRadianQty}{$DEFINE TUnit:=TNewtonMeterPerRadianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonMeterPerDegreeQty}{$DEFINE TUnit:=TNewtonMeterPerDegreeUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattQty}{$DEFINE TUnit:=TWattUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombQty}{$DEFINE TUnit:=TCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmpereHourQty}{$DEFINE TUnit:=TAmpereHourUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareCoulombQty}{$DEFINE TUnit:=TSquareCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltQty}{$DEFINE TUnit:=TVoltUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareVoltQty}{$DEFINE TUnit:=TSquareVoltUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFaradQty}{$DEFINE TUnit:=TFaradUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOhmQty}{$DEFINE TUnit:=TOhmUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSiemensQty}{$DEFINE TUnit:=TSiemensUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaQty}{$DEFINE TUnit:=TTeslaUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWeberQty}{$DEFINE TUnit:=TWeberUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryQty}{$DEFINE TUnit:=THenryUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLumenQty}{$DEFINE TUnit:=TLumenUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLuxQty}{$DEFINE TUnit:=TLuxUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THertzQty}{$DEFINE TUnit:=TBequerelUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TGrayUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TSievertUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerCubicMeterQty}{$DEFINE TUnit:=TNewtonPerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerMeterQty}{$DEFINE TUnit:=TNewtonPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoundForcePerInchQty}{$DEFINE TUnit:=TPoundForcePerInchUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSecondQty}{$DEFINE TUnit:=TCubicMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPoiseuilleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TPoiseuilleQty}{$DEFINE TUnit:=TPascalSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSecondQty}{$DEFINE TUnit:=TSquareMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TQuarticMeterSecondQty}{$DEFINE TUnit:=TQuarticMeterSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramPerQuarticMeterPerSecondQty}{$DEFINE TUnit:=TKilogramPerQuarticMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerKilogramQty}{$DEFINE TUnit:=TCubicMeterPerKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareSecondQty}{$DEFINE TUnit:=TKilogramSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCubicMeterPerSquareSecondQty}{$DEFINE TUnit:=TCubicMeterPerSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonCubicMeterQty}{$DEFINE TUnit:=TNewtonCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerMeterQty}{$DEFINE TUnit:=TSquareKilogramPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareKilogramPerSquareMeterQty}{$DEFINE TUnit:=TSquareKilogramPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareKilogramQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalKelvinQty}{$DEFINE TUnit:=TReciprocalKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramKelvinQty}{$DEFINE TUnit:=TKilogramKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareSecondQty}{$DEFINE TUnit:=TJoulePerKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerKilogramPerKelvinQty}{$DEFINE TUnit:=TJoulePerKilogramPerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterKelvinQty}{$DEFINE TUnit:=TMeterKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKelvinPerMeterQty}{$DEFINE TUnit:=TKelvinPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerMeterQty}{$DEFINE TUnit:=TWattPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterQty}{$DEFINE TUnit:=TWattPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerKelvinQty}{$DEFINE TUnit:=TWattPerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerMeterPerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterKelvinQty}{$DEFINE TUnit:=TSquareMeterKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterQuarticKelvinQty}{$DEFINE TUnit:=TSquareMeterQuarticKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerQuarticKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerQuarticKelvinQty}{$DEFINE TUnit:=TWattPerSquareMeterPerQuarticKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TJoulePerMolePerKelvinQty}{$DEFINE TUnit:=TJoulePerMolePerKelvinUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TOhmMeterQty}{$DEFINE TUnit:=TOhmMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TVoltPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltPerMeterQty}{$DEFINE TUnit:=TNewtonPerCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerMeterQty}{$DEFINE TUnit:=TCoulombPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareCoulombPerMeterQty}{$DEFINE TUnit:=TSquareCoulombPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerSquareMeterQty}{$DEFINE TUnit:=TCoulombPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TSquareMeterPerSquareCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonPerSquareCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TNewtonSquareMeterPerSquareCoulombQty}{$DEFINE TUnit:=TNewtonSquareMeterPerSquareCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TVoltMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltMeterQty}{$DEFINE TUnit:=TNewtonSquareMeterPerCoulombUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TVoltMeterPerSecondQty}{$DEFINE TUnit:=TVoltMeterPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TFaradPerMeterQty}{$DEFINE TUnit:=TFaradPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmperePerMeterQty}{$DEFINE TUnit:=TAmperePerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMeterPerAmpereQty}{$DEFINE TUnit:=TMeterPerAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TTeslaMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaMeterQty}{$DEFINE TUnit:=TNewtonPerAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TTeslaPerAmpereQty}{$DEFINE TUnit:=TTeslaPerAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=THenryPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=TTeslaMeterPerAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=THenryPerMeterQty}{$DEFINE TUnit:=TNewtonPerSquareAmpereUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TRadianPerMeterQty}{$DEFINE TUnit:=TRadianPerMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareSecondPerSquareMeterQty}{$DEFINE TUnit:=TSquareSecondPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareJouleQty}{$DEFINE TUnit:=TSquareJouleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKilogramSquareMeterPerSecondQty}{$DEFINE TUnit:=TJouleSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TElettronvoltSecondQty}{$DEFINE TUnit:=TElettronvoltSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareJouleSquareSecondQty}{$DEFINE TUnit:=TSquareJouleSquareSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TLumenPerWattQty}{$DEFINE TUnit:=TLumenPerWattUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TReciprocalMoleQty}{$DEFINE TUnit:=TReciprocalMoleUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TAmperePerSquareMeterQty}{$DEFINE TUnit:=TAmperePerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TMolePerCubicMeterQty}{$DEFINE TUnit:=TMolePerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCandelaPerSquareMeterQty}{$DEFINE TUnit:=TCandelaPerSquareMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerCubicMeterQty}{$DEFINE TUnit:=TCoulombPerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerKilogramQty}{$DEFINE TUnit:=TCoulombPerKilogramUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TGrayPerSecondQty}{$DEFINE TUnit:=TGrayPerSecondUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSteradianQty}{$DEFINE TUnit:=TWattPerSteradianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TSquareMeterSteradianQty}{$DEFINE TUnit:=TSquareMeterSteradianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TWattPerSquareMeterPerSteradianQty}{$DEFINE TUnit:=TWattPerSquareMeterPerSteradianUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TKatalPerCubicMeterQty}{$DEFINE TUnit:=TKatalPerCubicMeterUnit}{$i adim.inc}

{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=TCoulombPerMoleQty}{$DEFINE TUnit:=TCoulombPerMoleUnit}{$i adim.inc}

class operator TSecondUnit./(const ALeft: double; const ARight: TSecondUnit): THertzQty;
begin
  result.FValue := ALeft;
end;

class operator TSecondUnit./(const ALeft: TRadianQty; const ARight: TSecondUnit): TRadianPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TRadianPerSecondQty; const ARight: TSecondUnit): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TRadianQty; const ARight: TSquareSecondUnit): TRadianPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

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

class operator TSecondUnit./(const ALeft: TMeterPerSecondQty; const ARight: TSecondUnit): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TMeterQty; const ARight: TSquareSecondUnit): TMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TSquareMeterQty; const ARight: TSquareSecondUnit): TSquareMeterPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TKilogramQty; const ARight: TMeterUnit): TKilogramMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramQty; const ARight: TSecondUnit): TKilogramPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramMeterQty; const ARight: TSecondUnit): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit.*(const ALeft: TKilogramPerSecondQty; const ARight: TMeterUnit): TKilogramMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit.*(const ALeft: TKilogramQty; const ARight: TSquareMeterUnit): TKilogramSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TKilogramSquareMeterQty; const ARight: TSecondUnit): TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TSecondQty; const ARight: TMeterUnit): TSecondPerMeterQty;
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

class operator TSquareMeterUnit./(const ALeft: TNewtonQty; const ARight: TSquareMeterUnit): TPascalQty;
begin
  result.FValue := ALeft.FValue;
end;

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

class operator TRadianUnit./(const ALeft: TJouleQty; const ARight: TRadianUnit): TJoulePerRadianQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TJouleQty; const ARight: TSecondUnit): TWattQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TAmpereUnit.*(const ALeft: TSecondQty; const ARight: TAmpereUnit): TCoulombQty;
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

class operator TSquareMeterUnit.*(const ALeft: TTeslaQty; const ARight: TSquareMeterUnit): TWeberQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit.*(const ALeft: TVoltQty; const ARight: TSecondUnit): TWeberQty;
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

class operator TSecondUnit./(const ALeft: TMoleQty; const ARight: TSecondUnit): TKatalQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TCubicMeterUnit./(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonPerCubicMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TMeterUnit./(const ALeft: TNewtonQty; const ARight: TMeterUnit): TNewtonPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSecondUnit./(const ALeft: TCubicMeterQty; const ARight: TSecondUnit): TCubicMeterPerSecondQty;
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

class operator TCubicMeterUnit.*(const ALeft: TNewtonQty; const ARight: TCubicMeterUnit): TNewtonCubicMeterQty;
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

class operator TMeterUnit./(const ALeft: TRadianQty; const ARight: TMeterUnit): TRadianPerMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit./(const ALeft: TSquareKilogramQty; const ARight: TSquareSecondUnit): TSquareKilogramPerSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareMeterUnit./(const ALeft: TSquareSecondQty; const ARight: TSquareMeterUnit): TSquareSecondPerSquareMeterQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TSquareSecondUnit.*(const ALeft: TSquareJouleQty; const ARight: TSquareSecondUnit): TSquareJouleSquareSecondQty;
begin
  result.FValue := ALeft.FValue;
end;

class operator TWattUnit./(const ALeft: TLumenQty; const ARight: TWattUnit): TLumenPerWattQty;
begin
  result.FValue := ALeft.FValue;
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

{ Helpers }

function TSecondHelper.ToDay: TDayQty;
begin
  result.FValue := FValue / cDayFactor;
end;

function TSecondHelper.ToHour: THourQty;
begin
  result.FValue := FValue / cHourFactor;
end;

function TSecondHelper.ToMinute: TMinuteQty;
begin
  result.FValue := FValue / cMinuteFactor;
end;

function TSquareSecondHelper.ToSquareDay: TSquareDayQty;
begin
  result.FValue := FValue / cSquareDayFactor;
end;

function TSquareSecondHelper.ToSquareHour: TSquareHourQty;
begin
  result.FValue := FValue / cSquareHourFactor;
end;

function TSquareSecondHelper.ToSquareMinute: TSquareMinuteQty;
begin
  result.FValue := FValue / cSquareMinuteFactor;
end;

function TMeterHelper.ToAstronomical: TAstronomicalQty;
begin
  result.FValue := FValue / cAstronomicalFactor;
end;

function TMeterHelper.ToInch: TInchQty;
begin
  result.FValue := FValue / cInchFactor;
end;

function TMeterHelper.ToFoot: TFootQty;
begin
  result.FValue := FValue / cFootFactor;
end;

function TMeterHelper.ToYard: TYardQty;
begin
  result.FValue := FValue / cYardFactor;
end;

function TMeterHelper.ToMile: TMileQty;
begin
  result.FValue := FValue / cMileFactor;
end;

function TMeterHelper.ToNauticalMile: TNauticalMileQty;
begin
  result.FValue := FValue / cNauticalMileFactor;
end;

function TMeterHelper.ToAngstrom: TAngstromQty;
begin
  result.FValue := FValue / cAngstromFactor;
end;

function TSquareMeterHelper.ToSquareInch: TSquareInchQty;
begin
  result.FValue := FValue / cSquareInchFactor;
end;

function TSquareMeterHelper.ToSquareFoot: TSquareFootQty;
begin
  result.FValue := FValue / cSquareFootFactor;
end;

function TSquareMeterHelper.ToSquareYard: TSquareYardQty;
begin
  result.FValue := FValue / cSquareYardFactor;
end;

function TSquareMeterHelper.ToSquareMile: TSquareMileQty;
begin
  result.FValue := FValue / cSquareMileFactor;
end;

function TCubicMeterHelper.ToCubicInch: TCubicInchQty;
begin
  result.FValue := FValue / cCubicInchFactor;
end;

function TCubicMeterHelper.ToCubicFoot: TCubicFootQty;
begin
  result.FValue := FValue / cCubicFootFactor;
end;

function TCubicMeterHelper.ToCubicYard: TCubicYardQty;
begin
  result.FValue := FValue / cCubicYardFactor;
end;

function TCubicMeterHelper.ToLitre: TLitreQty;
begin
  result.FValue := FValue / cLitreFactor;
end;

function TCubicMeterHelper.ToGallon: TGallonQty;
begin
  result.FValue := FValue / cGallonFactor;
end;

function TKilogramHelper.ToTonne: TTonneQty;
begin
  result.FValue := FValue / cTonneFactor;
end;

function TKilogramHelper.ToPound: TPoundQty;
begin
  result.FValue := FValue / cPoundFactor;
end;

function TKilogramHelper.ToOunce: TOunceQty;
begin
  result.FValue := FValue / cOunceFactor;
end;

function TKilogramHelper.ToStone: TStoneQty;
begin
  result.FValue := FValue / cStoneFactor;
end;

function TKilogramHelper.ToTon: TTonQty;
begin
  result.FValue := FValue / cTonFactor;
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
  result.FValue := FValue / cDegreeFactor;
end;

function TSteradianHelper.ToSquareDegree: TSquareDegreeQty;
begin
  result.FValue := FValue / cSquareDegreeFactor;
end;

function TRadianPerSecondHelper.ToHertz: THertzQty;
begin
  result.FValue := FValue;
end;

function THertzHelper.ToRadianPerSecond: TRadianPerSecondQty;
begin
  result.FValue := FValue;
end;

function TSquareHertzHelper.ToRadianPerSquareSecond: TRadianPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TRadianPerSquareSecondHelper.ToSquareHertz: TSquareHertzQty;
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
  result.FValue := FValue / cMeterPerHourFactor;
end;

function TMeterPerSecondHelper.ToMilePerHour: TMilePerHourQty;
begin
  result.FValue := FValue / cMilePerHourFactor;
end;

function TMeterPerSecondHelper.ToNauticalMilePerHour: TNauticalMilePerHourQty;
begin
  result.FValue := FValue / cNauticalMilePerHourFactor;
end;

function TMeterPerSquareSecondHelper.ToMeterPerSecondPerSecond: TMeterPerSecondPerSecondQty;
begin
  result.FValue := FValue;
end;

function TMeterPerSecondPerSecondHelper.ToMeterPerSquareSecond: TMeterPerSquareSecondQty;
begin
  result.FValue := FValue;
end;

function TMeterPerSquareSecondHelper.ToMeterPerHourPerSecond: TMeterPerHourPerSecondQty;
begin
  result.FValue := FValue / cMeterPerHourPerSecondFactor;
end;

function TKilogramMeterPerSecondHelper.ToNewtonSecond: TNewtonSecondQty;
begin
  result.FValue := FValue;
end;

function TNewtonSecondHelper.ToKilogramMeterPerSecond: TKilogramMeterPerSecondQty;
begin
  result.FValue := FValue;
end;

function TKilogramSquareMeterPerSecondHelper.ToNewtonMeterSecond: TNewtonMeterSecondQty;
begin
  result.FValue := FValue;
end;

function TNewtonMeterSecondHelper.ToKilogramSquareMeterPerSecond: TKilogramSquareMeterPerSecondQty;
begin
  result.FValue := FValue;
end;

function TKilogramPerCubicMeterHelper.ToPoundPerCubicInch: TPoundPerCubicInchQty;
begin
  result.FValue := FValue / cPoundPerCubicInchFactor;
end;

function TNewtonHelper.ToPoundForce: TPoundForceQty;
begin
  result.FValue := FValue / cPoundForceFactor;
end;

function TPascalHelper.ToBar: TBarQty;
begin
  result.FValue := FValue / cBarFactor;
end;

function TPascalHelper.ToPoundPerSquareInch: TPoundPerSquareInchQty;
begin
  result.FValue := FValue / cPoundPerSquareInchFactor;
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
  result.FValue := FValue / cWattHourFactor;
end;

function TJouleHelper.ToElettronvolt: TElettronvoltQty;
begin
  result.FValue := FValue / cElettronvoltFactor;
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
  result.FValue := FValue / cPoundForceInchFactor;
end;

function TJouleHelper.ToRydberg: TRydbergQty;
begin
  result.FValue := FValue / cRydbergFactor;
end;

function TJouleHelper.ToCalorie: TCalorieQty;
begin
  result.FValue := FValue / cCalorieFactor;
end;

function TJoulePerRadianHelper.ToJoulePerDegree: TJoulePerDegreeQty;
begin
  result.FValue := FValue / cJoulePerDegreeFactor;
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
  result.FValue := FValue / cNewtonMeterPerDegreeFactor;
end;

function TCoulombHelper.ToAmpereHour: TAmpereHourQty;
begin
  result.FValue := FValue / cAmpereHourFactor;
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

function TNewtonPerMeterHelper.ToPoundForcePerInch: TPoundForcePerInchQty;
begin
  result.FValue := FValue / cPoundForcePerInchFactor;
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
  result.FValue := FValue / cElettronvoltSecondFactor;
end;

{ Power functions }

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

function SquarePower(AQuantity: TKilogramMeterPerSecondQty): TSquareKilogramSquareMeterPerSquareSecondQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareKilogramSquareMeterPerSquareSecondQty): TKilogramMeterPerSecondQty;
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

function SquarePower(AQuantity: TKilogramSquareMeterPerSecondQty): TSquareJouleSquareSecondQty;
begin
  result.FValue := IntPower(AQuantity.FValue, 2);
end;

function SquareRoot(AQuantity: TSquareJouleSquareSecondQty): TKilogramSquareMeterPerSecondQty;
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

{ Math functions }

generic function Min<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
begin
  if AValue1 < AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

generic function Max<TQuantity>(const AValue1, AValue2: TQuantity): TQuantity;
begin
  if AValue1 > AValue2 then
    result := AValue1
  else
    result := AValue2;
end;

end.
