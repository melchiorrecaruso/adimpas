{
  Description: Common unit.

  Copyright (C) 2023-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit ToolKitUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SimulatedAnnealing, StrUtils, SysUtils;

type
  TToolKitItem = record
    FClassName: string;
    FOperator: string;
    FClassParent1: string;
    FClassParent2: string;
    FLongSymbol:  string;
    FShortSymbol: string;
    FIdentifierSymbol: string;
    FBaseClass: string;
    FFactor: string;
    FPrefixes: string;
    FClassType: string;
    FExponents: array [1..8] of longint;
  end;

  TToolKitList = class
  private
    FList: array of TToolKitItem;
    function GetClassName  (const AItem: TToolKitItem): string;
    function GetShortSymbol(const AItem: TToolKitItem): string;
    function GetLongSymbol (const AItem: TToolKitItem): string;
    function GetItem(Index: longint): TToolKitItem;
    function GetCount: longint;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AItem: TToolKitItem): string;
    procedure Delete(Index: longint);
    procedure Clear;

    function Search(const AClassName: string): longint;
  public
    property Items[Index: longint]: TToolKitItem read GetItem; default;
    property Count: longint read GetCount;
  end;

  TToolKitBuilder = class(TSimulatedAnnealing)
  private
    FList: TToolKitList;
    FClassList: TStringList;
    FCommUnits: TStringList;
    FOperatorList: TStringList;

    BaseUnitCount:     longint;
    FactoredUnitCount: longint;
    ExternalOperators: longint;
    ForcedOperators:   longint;
    InternalOperators: longint;
    FTestingCount:     longint;
    FSkipVectorialunits: boolean;

    FDocument:  TStringList;
    FMessages:  TStringList;
    SectionA0:  TStringList;
    SectionA1:  TStringList;
    SectionA2:  TStringList;
    SectionA21: TStringList;
    SectionA22: TStringList;
    SectionA3:  TStringList;
    SectionA31: TStringList;
    SectionA4:  TStringList;
    SectionA5:  TStringList;
    SectionA6:  TStringList;
    SectionA7:  TStringList;
    SectionA8:  TStringList;
    SectionA9:  TStringList;
    SectionA10: TStringList;

    SectionB1:  TStringList;
    SectionB2:  TStringList;
    SectionB21: TStringList;
    SectionB22: TStringList;
    SectionB3:  TStringList;
    SectionB31: TStringList;
    SectionB4:  TStringList;
    SectionB5:  TStringList;
    SectionB6:  TStringList;
    SectionB7:  TStringList;
    SectionB8:  TStringList;
    SectionB9:  TStringList;
    SectionB10: TStringList;

    FOnMessage: TMessageEvent;
    FMessage: string;

    // private routines
    procedure AddItem(const AItem: TToolkitItem; AddOperator: boolean);
    procedure AddBaseItem(const AItem: TToolkitItem);
    procedure AddClonedItem(const AItem: TToolkitItem);
    procedure AddFactoredItem(const AItem: TToolkitItem);

    procedure AddVECBaseItem(const AItem: TToolkitItem);
    procedure AddVECClonedItem(const AItem: TToolkitItem);
    procedure AddVECFactoredItem(const AItem: TToolkitItem);

    procedure AddItemOperators(const AItem: TToolkitItem);
    procedure AddVECItemOperators(const AItem: TToolkitItem);

    procedure AddQuantityOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string);
    procedure AddUnitOperator    (const AOperator, ALeftClass, ARightClass, AResultClass: string; ADual: boolean);

    procedure AddFactoredQuantities(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);

    procedure AddHelperSquaredNorm(const AItem: TToolkitItem);
    procedure AddHelperNorm(const AItem: TToolkitItem);
    procedure AddHelperDual(const AItem: TToolkitItem);
    procedure AddHelperReciprocal(const AItem: TToolKitItem);

    procedure AddHelperDOT      (const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);
    procedure AddHelperWEDGE    (const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);
    procedure AddHelperGEOMETRIC(const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);

    procedure AddResources(const AItem: TToolkitItem);
    procedure AddEquivalence(AFromClass, AToClass: string);

    function  SearchLine(const ALine: string; ASection: TStringList): longint;
    procedure Run(ASolution: TSolution);
  public
    constructor Create(OnMessageEvent: TMessageEvent);
    destructor Destroy; override;

    function  CalculateEnergy(const ASolution: TSolution): double; override;
    procedure CreateSolution(var ANeighbour: TSolution); override;

    procedure Add(const AItem: TToolkitItem);
    procedure Run;
  public
    property SkipVectorialUnits: boolean read FSkipVectorialunits write FSkipVectorialunits;
    property Document: TStringList read FDocument;
    property Messages: TStringList read FMessages;
    property Message: string read FMessage;
  end;


implementation

uses
  Common, DateUtils, LCLType, Math, Process;

const
  adiminc     = 'adim.inc';
  adimVECinc  = 'adimVEC.inc';
  adimBVECinc = 'adimBVEC.inc';
  adimTVECinc = 'adimTVEC.inc';
  adimMVECinc = 'adimMVEC.inc';

// TToolKitBuilder

constructor TToolKitBuilder.Create(OnMessageEvent: TMessageEvent);
begin
  inherited Create(OnMessageEvent);
  FList         := TToolKitList.Create;
  FDocument     := TStringList.Create;
  FMessages     := TStringList.Create;
  FOnMessage    := OnMessageEvent;
  FClassList    := TStringList.Create;
  FClassList.Sorted := TRUE;

  FCommUnits    := TStringList.Create;
  FOperatorList := TStringList.Create;
  FOperatorList.Sorted := TRUE;

  FSkipVectorialunits := False;
  FTestingCount := 0;
end;

destructor TToolKitBuilder.Destroy;
begin
  FOperatorList.Destroy;
  FCommUnits.Destroy;
  FClassList.Destroy;

  FMessages.Destroy;
  FDocument.Destroy;
  FList.Destroy;
  inherited Destroy;
end;

procedure TToolKitBuilder.Add(const AItem: TToolkitItem);
begin
  FMessage := FList.Add(AItem);
  if FMessage <> '' then
  begin
    FMessages.Append(FMessage);
  end;
end;

procedure TToolKitBuilder.AddItem(const AItem: TToolkitItem; AddOperator: boolean);
begin
  if FClassList.IndexOf(GetQuantityType(AItem.FClassName)) = -1 then
  begin
    FClassList.Append(GetQuantityType(AItem.FClassName));

    if AItem.FClassType = '' then
    begin
      if (AItem.FBaseClass = '') then AddBaseItem  (AItem) else
      if (AItem.FFactor    = '') then AddClonedItem(AItem) else AddFactoredItem(AItem);
    end else
    if not FSkipVectorialUnits then
      begin
        if (AItem.FBaseClass = '') then AddVECBaseItem  (AItem) else
        if (AItem.FFactor    = '') then AddVECClonedItem(AItem) else AddVECFactoredItem(AItem);
      end;
  end;

  if AddOperator then
  begin
    if AItem.FClassType = '' then
    begin
      AddItemOperators(AItem)
    end else
    if not FSkipVectorialUnits then
    begin
      AddVECItemOperators(AItem);
    end;
  end;
end;

procedure TToolKitBuilder.AddBaseItem(const AItem: TToolkitItem);
begin
  // BASE UNIT
  if (AItem.FOperator = '*') then
  begin
    SectionA2.Insert(3, '');
    SectionA2.Insert(4, Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
    SectionA2.Insert(5, Format(INTF_END, [adiminc]));
  end else
  begin
    SectionA2.Append('');
    SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
    SectionA2.Append(Format(INTF_END, [adiminc]));
  end;

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));

  SectionB3.Append('');
  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));

  AddResources(AItem);
  Inc(BaseUnitCount);
end;

procedure TToolKitBuilder.AddClonedItem(const AItem: TToolkitItem);
begin
  // CLONED UNIT
  SectionA2.Append('');
  SectionA2.Append(Format(INTF_NOP, []));
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
  SectionA2.Append(Format(INTF_END, [adiminc]));

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FBaseClass), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_NOP, []));
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));

  SectionB3.Append('');
  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FBaseClass), GetUnitType(AItem.FClassName), adiminc]));

  AddResources(AItem);
  AddHelper(AItem.FClassName, AItem.FBaseClass, '');
  AddHelper(AItem.FBaseClass, AItem.FClassName, '');
  Inc(FactoredUnitCount);
end;

procedure TToolKitBuilder.AddFactoredItem(const AItem: TToolkitItem);
begin
  // FACTORED UNIT
  SectionA2.Append('');
  SectionA2.Append(Format(INTF_NOP, []));
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
  SectionA2.Append(Format(INTF_END, [adiminc]));

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_NOP, []));
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CFACTOR,       [GetFactorConst               (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));

  SectionB3.Append('');
  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));

  AddResources(AItem);
  if AItem.FFactor.Contains('%s') = FALSE then
  begin
    AddHelper(AItem.FClassName, AItem.FBaseClass, 'FValue / ' + GetFactorConst(AItem.FClassName));
  end else
  begin
    AddHelper(AItem.FBaseClass, AItem.FClassName, Format(Copy(AItem.FFactor, 1, Pos('|', AItem.FFactor) -1), ['FValue']));
    AddHelper(AItem.FClassName, AItem.FBaseClass, Format(Copy(AItem.FFactor, Pos('|', AItem.FFactor) + 1, Length(AItem.FFactor)), ['FValue']));
  end;
  Inc(FactoredUnitCount);
end;

procedure TToolKitBuilder.AddVECBaseItem(const AItem: TToolkitItem);
var
  BaseInc: string;
begin
  // VEC UNIT
  BaseInc := 'ERROR';
  if UpperCase(AItem.FClassType) = 'TVECTOR'      then BaseInc := adimVECinc;
  if UpperCase(AItem.FClassType) = 'TBIVECTOR'    then BaseInc := adimBVECinc;
  if UpperCase(AItem.FClassType) = 'TTRIVECTOR'   then BaseInc := adimTVECinc;
  if UpperCase(AItem.FClassType) = 'TMULTIVECTOR' then BaseInc := adimMVECinc;

  SectionA2.Insert(3, '');
  SectionA2.Insert(4, Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), BaseInc]));
  SectionA2.Insert(5, Format(INTF_END, [BaseInc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), BaseInc]));

  AddResources(AItem);
  Inc(BaseUnitCount);
end;

procedure TToolKitBuilder.AddVECClonedItem(const AItem: TToolkitItem);
var
  BaseInc: string;
begin
  // VEC CLONED UNIT
  BaseInc := adiminc;
  if UpperCase(AItem.FClassType) = 'TVECTOR'      then BaseInc := adimVECinc;
  if UpperCase(AItem.FClassType) = 'TBIVECTOR'    then BaseInc := adimBVECinc;
  if UpperCase(AItem.FClassType) = 'TTRIVECTOR'   then BaseInc := adimTVECinc;
  if UpperCase(AItem.FClassType) = 'TMULTIVECTOR' then BaseInc := adimMVECinc;

  SectionA2.Append('');
  SectionA2.Append(Format(INTF_NOP, []));
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), BaseInc]));
  SectionA2.Append(Format(INTF_END, [BaseInc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_NOP, []));
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), BaseInc]));

  AddResources(AItem);
  AddHelper(AItem.FClassName, AItem.FBaseClass, '');
  AddHelper(AItem.FBaseClass, AItem.FClassName, '');
  Inc(FactoredUnitCount);
end;

procedure TToolKitBuilder.AddVECFactoredItem(const AItem: TToolkitItem);
var
  BaseInc: string;
begin
  // VEC FACTORED UNIT
  BaseInc := adiminc;
  if UpperCase(AItem.FClassType) = 'TVECTOR'      then BaseInc := adimVECinc;
  if UpperCase(AItem.FClassType) = 'TBIVECTOR'    then BaseInc := adimBVECinc;
  if UpperCase(AItem.FClassType) = 'TTRIVECTOR'   then BaseInc := adimTVECinc;
  if UpperCase(AItem.FClassType) = 'TMULTIVECTOR' then BaseInc := adimMVECinc;

  SectionA2.Append('');
  SectionA2.Append(Format(INTF_NOP, []));
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), BaseInc]));
  SectionA2.Append(Format(INTF_END, [BaseInc]));

  SectionB2.Append('');
  SectionB2.Append(Format(IMPL_NOP, []));
  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CFACTOR,       [GetFactorConst               (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), BaseInc]));

  AddResources(AItem);
  if AItem.FFactor.Contains('%s') = FALSE then
  begin
    AddHelper(AItem.FClassName, AItem.FBaseClass, 'FValue / ' + GetFactorConst(AItem.FClassName));
  end else
  begin
    AddHelper(AItem.FBaseClass, AItem.FClassName, Format(Copy(AItem.FFactor, 1, Pos('|', AItem.FFactor) -1), ['FValue']));
    AddHelper(AItem.FClassName, AItem.FBaseClass, Format(Copy(AItem.FFactor, Pos('|', AItem.FFactor) + 1, Length(AItem.FFactor)), ['FValue']));
  end;
  Inc(FactoredUnitCount);
end;

procedure TToolKitBuilder.AddItemOperators(const AItem: TToolkitItem);
begin
  if (AItem.FBaseClass = '') then
  begin
    if AItem.FOperator = '*' then
    begin
                                                         AddQuantityOperator('*', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 <> AItem.FClassParent2 then AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName));

                                                         AddQuantityOperator('/', GetQuantityType(AItem.FClassName), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
      if AItem.FClassParent1 <> AItem.FClassParent2 then AddQuantityOperator('/', GetQuantityType(AItem.FClassName), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));

      // OP1: A*B=C
      // OP2: B*A=C
      // OP3: B=C/A
      // OP4: A=C/B

      if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), FALSE);
      if Pos('OP2', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent2), GetUnitType(AItem.FClassParent1), GetQuantityType(AItem.FClassName   ), FALSE);
      if Pos('OP3', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), FALSE);
      if Pos('OP4', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), FALSE);

    end else
    if AItem.FOperator = '/' then
    begin
                                                      AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ));
                                                      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent1));
      if AItem.FClassName <> AItem.FClassParent2 then AddQuantityOperator('*', GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
      if AItem.FClassName <> AItem.FClassParent2 then AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2));

      // OP1: C=A/B
      // OP2: B*C=A
      // OP3: C*B=A
      // OP4: B=A/C

      if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), FALSE);
      if Pos('OP2', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent2), GetUnitType(AItem.FClassName),    GetQuantityType(AItem.FClassParent1), FALSE);
      if Pos('OP3', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), FALSE);
      if Pos('OP4', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2), FALSE);

    end else
    if UpperCase(AItem.FOperator) = 'RECIPROCAL' then
    begin
      AddQuantityOperator('/', 'double', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName  ));
      AddQuantityOperator('/', 'double', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
    end else
    if Pos('POWER', UpperCase(AItem.FOperator)) > 0 then
    begin
      AddPower(AItem.FOperator, AItem.FClassParent1, AItem.FClassName);
    end else
    if Pos('ROOT', UpperCase(AItem.FOperator)) > 0 then
    begin
      AddPower(StringReplace(UpperCase(AItem.FOperator), 'ROOT', 'POWER', []), AItem.FClassName, AItem.FClassParent1);
    end;

  end else
    if AItem.FOperator = '=' then
    begin
      if AItem.FBaseClass <> '' then;
        FCommUnits.Add(GetQuantityType(AItem.FBaseClass));

      SectionA7.Append('');
      SectionB7.Append('');
      AddEquivalence(AItem.FBaseClass, AItem.FClassName);
      AddHelper(AItem.FBaseClass, AItem.FClassName, '');
      SectionB7.Append('');
      AddEquivalence(AItem.FClassName, AItem.FBaseClass);
      AddHelper(AItem.FClassName, AItem.FBaseClass, '');
    end else
    if AItem.FOperator = ':=' then
    begin
      if AItem.FBaseClass <> '' then;
        FCommUnits.Add(GetQuantityType(AItem.FBaseClass));

      SectionA7.Append('');
      SectionB7.Append('');
      AddEquivalence(AItem.FBaseClass, AItem.FClassName);
    end else
    if (UpperCase(AItem.FOperator) = 'HELPER') then
    begin
      SectionA7.Append('');
      SectionB7.Append('');
      AddHelper(AItem.FClassName, AItem.FBaseClass, '');
      SectionB7.Append('');
      AddHelper(AItem.FBaseClass, AItem.FClassName, '');
    end;
end;

procedure TToolKitBuilder.AddVECItemOperators(const AItem: TToolkitItem);
begin
  if (AItem.FBaseClass = '') then
  begin
    if AItem.FOperator = '*' then
    begin
      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName));

      if IsAVector(AItem.FClassName) and IsAVector(AItem.FClassParent1) then
        AddHelperDOT(GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName), GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2))
      else
      if IsAVector(AItem.FClassParent1) then
      begin
        AddQuantityOperator('*', GetQuantityType          (AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
        AddQuantityOperator('*', GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType          (AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
      end else
        AddQuantityOperator('/', GetQuantityType(AItem.FClassName), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));

      if IsAVector(AItem.FClassName) and IsAVector(AItem.FClassParent2) then
        AddHelperDOT(GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1))
      else
      if IsAVector(AItem.FClassParent2) then
      begin
        AddQuantityOperator('*', GetQuantityType          (AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
        AddQuantityOperator('*', GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType          (AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
      end else
        AddQuantityOperator('/', GetQuantityType(AItem.FClassName), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));

    end else
    if AItem.FOperator = '/' then
    begin
      if IsAVector(AItem.FClassParent2) then
      begin
        AddQuantityOperator('*', GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
        AddQuantityOperator('*', GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassName));
      end else
        AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));

      if IsAVector(AItem.FClassName) and IsAVector(AItem.FClassParent2) then
      begin
        AddHelperDOT(GetUnitTypeHelper(AItem.FClassName   ), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
        AddHelperDOT(GetUnitTypeHelper(AItem.FClassParent2), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
      end else
      begin
        AddQuantityOperator('*', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
        AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
      end;

      if IsAVector(AItem.FClassName) and IsAVector(AItem.FClassParent1) then
      begin
        AddHelperDOT(GetUnitTypeHelper          (AItem.FClassParent1), GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
        AddHelperDOT(GetReciprocalUnitTypeHelper(AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassName   ), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
      end else
      begin
        AddQuantityOperator('*', GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
        AddQuantityOperator('*', GetReciprocalQuantityType(AItem.FClassName   ), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
      end;

    end else

    if UpperCase(AItem.FOperator) = 'NORM' then
    begin
      AddHelperNorm(AItem);
    end else
    if UpperCase(AItem.FOperator) = 'NORM2' then
    begin
      AddHelperSquaredNorm(AItem);
    end else
    if UpperCase(AItem.FOperator) = 'RECIPROCAL' then
    begin
      AddQuantityOperator('/', 'double', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName  ));
      AddQuantityOperator('/', 'double', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
    end else
    if UpperCase(AItem.FOperator) = 'DUAL' then
    begin
      AddHelperDual(AItem);
    end else

    if UpperCase(AItem.FOperator) = 'DOT' then
    begin
      AddHelperDot(GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      AddHelperDot(GetUnitTypeHelper(AItem.FClassParent2), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName));


    end else
    if UpperCase(AItem.FOperator) = 'DOT/' then
    begin
      AddHelperDot(GetUnitTypeHelper          (AItem.FClassParent1), GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      AddHelperDot(GetReciprocalUnitTypeHelper(AItem.FClassParent2), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassName));


    end else
    if UpperCase(AItem.FOperator) = 'WEDGE' then
    begin
      AddHelperWEDGE(GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1), GetQuantityType          (AItem.FClassParent2), GetQuantityType(AItem.FClassName   ));
      AddHelperWEDGE(GetUnitTypeHelper(AItem.FClassParent2), GetQuantityType(AItem.FClassParent2), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassName   ));
      AddHelperDot  (GetUnitTypeHelper(AItem.FClassName   ), GetQuantityType(AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
      AddHelperDot  (GetUnitTypeHelper(AItem.FClassName   ), GetQuantityType(AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));

      AddHelperDot(GetReciprocalUnitTypeHelper(AItem.FClassParent2), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
      AddHelperDot(GetReciprocalUnitTypeHelper(AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
    end else
    if UpperCase(AItem.FOperator) = 'WEDGE/' then
    begin
      AddHelperWEDGE(GetUnitTypeHelper          (AItem.FClassParent1), GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      AddHelperWEDGE(GetReciprocalUnitTypeHelper(AItem.FClassParent2), GetReciprocalQuantityType(AItem.FClassParent2), GetQuantityType          (AItem.FClassParent1), GetQuantityType(AItem.FClassName));

      AddHelperDot  (GetUnitTypeHelper          (AItem.FClassName   ), GetQuantityType          (AItem.FClassName   ), GetQuantityType          (AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
      AddHelperDot  (GetUnitTypeHelper          (AItem.FClassParent2), GetQuantityType          (AItem.FClassParent2), GetQuantityType          (AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));

      AddHelperDot  (GetUnitTypeHelper          (AItem.FClassParent1), GetQuantityType          (AItem.FClassParent1), GetReciprocalQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
      AddHelperDot  (GetReciprocalUnitTypeHelper(AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassName   ), GetReciprocalQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
    end else

    if UpperCase(AItem.FOperator) = 'GEOMETRIC' then
    begin
      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ));
      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName   ));
      AddQuantityOperator('/', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
      AddQuantityOperator('/', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
    end else
    if UpperCase(AItem.FOperator) = 'GEOMETRIC/' then
    begin
      AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ));
      AddQuantityOperator('*', GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent1));
      AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName   ), GetQuantityType(AItem.FClassParent2));
    end else

    if AItem.FOperator = '*=' then
    begin
      AddUnitOperator('*', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName), FALSE);

      if AItem.FClassParent1 = 'TMultivector' then  AddQuantityOperator('*', 'TMultivector', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TTrivector'   then  AddQuantityOperator('*', 'TTrivector',   GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TBivector'    then  AddQuantityOperator('*', 'TBivector',    GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TVector'      then  AddQuantityOperator('*', 'TVector',      GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));

      if AItem.FClassParent1 = 'TTrivector'   then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TTriversor123', GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TBivector'    then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TBiversor12',   GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TBivector'    then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TBiversor13',   GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TBivector'    then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TBiversor23',   GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TVector'      then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TVersor1',      GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TVector'      then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TVersor2',      GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TVector'      then  AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), 'TVersor3',      GetQuantityType(AItem.FClassName));
    end else
    if AItem.FOperator = '/=' then
    begin
      AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName), FALSE);

      if AItem.FClassParent1 = 'TMultivector' then  AddQuantityOperator('/', 'TMultivector', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TTrivector'   then  AddQuantityOperator('/', 'TTrivector',   GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TBivector'    then  AddQuantityOperator('/', 'TBivector',    GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if AItem.FClassParent1 = 'TVector'      then  AddQuantityOperator('/', 'TVector',      GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
    end;


  end else

    if (AItem.FOperator = ':=') then
    begin

      if AItem.FBaseClass <> '' then;
        FCommUnits.Add(GetQuantityType(AItem.FBaseClass));

      SectionA7.Append('');
      SectionB7.Append('');
      AddEquivalence(AItem.FBaseClass, AItem.FClassName);
    end else
    if (UpperCase(AItem.FOperator) = 'HELPER') then
    begin

    end;

end;

procedure TToolKitBuilder.AddQuantityOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string);
var
  i, iL, iR, iX: longint;
  j: longint;
  ABaseClass, S: string;
begin
  iL := SearchLine(Format(INTF_QUANTITY, [ALeftClass  , '*']), SectionA2);
  iR := SearchLine(Format(INTF_QUANTITY, [ARightClass , '*']), SectionA2);
  iX := SearchLine(Format(INTF_QUANTITY, [AResultClass, '*']), SectionA2);

  ABaseClass := '';
  i := Max(iL, iR);
  if i = iL then ABaseClass := ALeftClass;
  if i = iR then ABaseClass := ARightClass;
  if i < iX then ABaseClass := '';

  for j := 0 to FCommUnits.Count -1 do
  begin
    if ABaseClass <> '' then
    begin
      if (i = iL) and (ALeftClass  = FCommUnits[j]) then ABaseClass := '';
      if (i = iR) and (ARightClass = FCommUnits[j]) then ABaseClass := '';
      if ABaseClass = '' then Inc(ForcedOperators);
    end;
  end;

  if FClassList.IndexOf(ABaseClass + '.' + ALeftClass + AOperator + ARightClass) = -1 then
  begin
    FClassList.Append(ABaseClass + '.' + ALeftClass + AOperator + ARightClass);

    j := -1;
    if ABaseClass = '' then
    begin
      SectionA22.Append(Format(INTF_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB22.Append('');
      SectionB22.Append(Format(IMPL_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(ExternalOperators);
    end else
    begin
      j := SearchLine(Format(IMPL_QUANTITY, [ABaseClass, '*']), SectionB2);
      SectionA2.Insert(i + 1, Format(INTF_OP_CLASS, [            AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB2.Insert(j + 1, '');
      SectionB2.Insert(j + 2, Format(IMPL_OP_CLASS, [ABaseClass, AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(InternalOperators);
    end;

    if AResultClass = 'double' then
      S := '  result :='
    else
      S := '  result.FValue :=';

    if IsAspecialKey(ALeftClass) then
      S := S + ' ALeft'
    else
      S := S + ' ALeft.FValue';

    if IsAVersorKey(ARightClass) then
    begin
       S := S + ' ' + AOperator + ' ARight';
    end else
    begin
      if IsAspecialKey(ARightClass) then
        S := S + ' ' + AOperator + ' ARight'
      else
        S := S + ' ' + AOperator + ' ARight.FValue';
    end;
    S := S + ';';

    if ABaseClass = '' then
    begin
      SectionB22.Append('begin');
      SectionB22.Append(S);
      SectionB22.Append('end;');
    end else
    begin
      SectionB2.Insert(j + 3, 'begin');
      SectionB2.Insert(j + 4, S);
      SectionB2.Insert(j + 5, 'end;');
    end;

  end else
    FMessages.Append('ERROR: operator ' + AOperator + '(' + ALeftClass + '; ' + ARightClass + ') : ' + AResultClass + '; already esists.');
end;

procedure TToolKitBuilder.AddUnitOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string; ADual: boolean);
var
  i: longint;
  BaseQuantity: string;
  FuncLine: string;
begin
  BaseQuantity := GetQuantityType(GetBaseClass(ARightClass));
  if FClassList.IndexOf(ALeftClass + AOperator + ARightClass) = -1 then
  begin
    FClassList.Append(ALeftClass + AOperator + ARightClass);

    i := SearchLine(Format(INTF_UNIT, [BaseQuantity, ARightClass, '*']), SectionA3);

    SectionA3 .Insert(i + 1, Format(INTF_OP_CLASS, [             AOperator, ALeftClass, ARightClass, AResultClass]));
    SectionB31.Append('');
    SectionB31.Append(       Format(IMPL_OP_CLASS, [ARightClass, AOperator, ALeftClass, ARightClass, AResultClass]));
    SectionB31.Append('begin');

    if AResultClass <> 'double' then
    begin

      FuncLine := '  result.FValue := ';

      if IsAspecialKey(ALeftClass) then
        FuncLine := FuncLine + 'ALeft'
      else
        FuncLine := FuncLine + 'ALeft.FValue';

      if ADual then
        FuncLine := FuncLine + '.Dual';

      FuncLine := FuncLine + ';';

    end else
    begin
      if ALeftClass <> 'double' then
        SectionB31.Append('  result := ALeft.FValue;')
      else
        SectionB31.Append('  result := ALeft;');
    end;
    SectionB31.Append(FuncLine);
    SectionB31.Append('end;');
    SectionB31.Append('');
    Inc(InternalOperators);
  end;
end;

procedure TToolKitBuilder.AddPower(AOperator, AQuantity, AResult: string);
var
  S1, S2, S3: string;
begin
  S1 := '';
  S2 := '';
  S3 := '';

  if ('squarepower' = LowerCase(AOperator)) then
  begin
    S1 := 'Square';
    S2 := '2';
    S3 := '1/2';
  end else
  if ('cubicpower' = LowerCase(AOperator)) then
  begin
    S1 := 'Cubic';
    S2 := '3';
    S3 := '1/3';
  end else
  if ('quarticpower' = LowerCase(AOperator)) then
  begin
    S1 := 'Quartic';
    S2 := '4';
    S3 := '1/4';
  end else
  if ('quinticpower' = LowerCase(AOperator)) then
  begin
    S1 := 'Quintic';
    S2 := '5';
    S3 := '1/5';
  end else
  if ('sexticpower' = LowerCase(AOperator)) then
  begin
    S1 := 'Sextic';
    S2 := '6';
    S3 := '1/6';
  end;

  AQuantity := GetQuantityType(AQuantity);
  AResult   := GetQuantityType(AResult);

  SectionA9.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionA9.Append('function ' + S1 + 'Root(AQuantity: ' +  AResult + '): ' + AQuantity + ';');

  SectionB9.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionB9.Append('begin');
  SectionB9.Append('  result.FValue := IntPower(AQuantity.FValue, ' + S2 + ');');
  SectionB9.Append('end;');
  SectionB9.Append('');

  SectionB9.Append('function ' + S1 + 'Root(AQuantity: ' + AResult + '): ' + AQuantity + ';');
  SectionB9.Append('begin');
  SectionB9.Append('  result.FValue := Power(AQuantity.FValue, ' + S3 + ');');
  SectionB9.Append('end;');
  SectionB9.Append('');
end;

procedure TToolKitBuilder.AddHelper(AClassName, ABaseClass, AFactor: string);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(ABaseClass) + ' = record helper for ' + GetQuantityType(ABaseClass));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(ABaseClass), GetQuantityType(ABaseClass)]));
    SectionA8.Append(Format('    function To%s: %s;', [GetUnitID(AClassName), GetQuantityType(AClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function To%s: %s;', [GetUnitID(AClassName), GetQuantityType(AClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.To%s: %s;', [GetUnitTypeHelper(ABaseClass), GetUnitID(AClassName), GetQuantityType(AClassName)]));

  SectionB8.Append('begin');
  if AFactor = '' then
    SectionB8.Append('  result.FValue := FValue;')
  else
    SectionB8.Append('  result.FValue := ' + AFactor + ';');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolKitBuilder.AddHelperReciprocal(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassParent1) + ' = record helper for ' + GetQuantityType(AItem.FClassParent1));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append(Format('    function Reciprocal: %s;', [GetQuantityType(AItem.FClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Reciprocal: %s;', [GetQuantityType(AItem.FClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Reciprocal: %s;', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassName)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Reciprocal;');
//SectionB8.Append('  result.FValue := FValue / FValue.SquaredNorm;');
  SectionB8.Append('end;');
  SectionB8.Append('');

  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassName) + ' = record helper for ' + GetQuantityType(AItem.FClassName));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append(Format('    function Reciprocal: %s;', [GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Reciprocal: %s;', [GetQuantityType(AItem.FClassParent1)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Reciprocal: %s;', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassParent1)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Reciprocal;');
//SectionB8.Append('  result.FValue := FValue / FValue.SquaredNorm;');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolKitBuilder.AddHelperSquaredNorm(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassName) + ' = record helper for ' + GetQuantityType(AItem.FClassName));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append(Format('    function SquaredNorm: %s;', [GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function SquaredNorm: %s;', [GetQuantityType(AItem.FClassParent1)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.SquaredNorm: %s;', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassParent1)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.SquaredNorm;');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolKitBuilder.AddHelperNorm(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassName) + ' = record helper for ' + GetQuantityType(AItem.FClassName));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append(Format('    function Norm: %s;', [GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Norm: %s;', [GetQuantityType(AItem.FClassParent1)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Norm: %s;', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassParent1)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Norm;');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolKitBuilder.AddHelperDual(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassName) + ' = record helper for ' + GetQuantityType(AItem.FClassName));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append(Format('    function Dual: %s;', [GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Dual: %s;', [GetQuantityType(AItem.FClassParent1)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Dual: %s;', [GetUnitTypeHelper(AItem.FClassName), GetQuantityType(AItem.FClassParent1)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Dual;');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolKitBuilder.AddHelperDOT(const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);
var
  Index: longint;
begin
  if FClassList.IndexOf(Format('function dot.%s(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity])) = -1 then
  begin
    FClassList.Add(Format('function dot.%s(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));

    Index := SectionA8.IndexOf('  ' + ABaseUnit + ' = record helper for ' + ABaseQuantity);
    if Index = -1 then
    begin
      SectionA8.Append(Format('  %s = record helper for %s', [ABaseUnit, ABaseQuantity]));
      SectionA8.Append(Format('    function dot(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
      SectionA8.Append('  end;');
      SectionA8.Append('');
    end else
    begin
      SectionA8.Insert(Index + 1, Format('    function dot(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
    end;

    SectionB8.Append('');
    SectionB8.Append(Format('function %s.dot(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));
    SectionB8.Append('begin');
    SectionB8.Append('  result.FValue := FValue.dot(AValue.FValue);');
    SectionB8.Append('end;');
    SectionB8.Append('');
  end;
end;

procedure TToolKitBuilder.AddHelperWEDGE(const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);
var
  Index: longint;
begin
  if FClassList.IndexOf(Format('function %s.wedge(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]))= -1 then
  begin
    FClassList.Add(Format('function %s.wedge(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));

    Index := SectionA8.IndexOf('  ' + ABaseUnit + ' = record helper for ' + ABaseQuantity);
    if Index = -1 then
    begin
      SectionA8.Append(Format('  %s = record helper for %s', [ABaseUnit, ABaseQuantity]));
      SectionA8.Append(Format('    function wedge(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
      SectionA8.Append('  end;');
      SectionA8.Append('');
    end else
    begin
      SectionA8.Insert(Index + 1, Format('    function wedge(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
    end;

    SectionB8.Append('');
    SectionB8.Append(Format('function %s.wedge(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));
    SectionB8.Append('begin');
    SectionB8.Append('  result.FValue := FValue.wedge(AValue.FValue);');
    SectionB8.Append('end;');
    SectionB8.Append('');
  end;
end;

procedure TToolKitBuilder.AddHelperGEOMETRIC(const ABaseUnit, ABaseQuantity, AInputQuantity, AResultQuantity: string);
var
  Index: longint;
begin
  if FClassList.IndexOf(Format('function %s.geometric(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]))= -1 then
  begin
    FClassList.Add(Format('function %s.geometric(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));

    Index := SectionA8.IndexOf('  ' + ABaseUnit + ' = record helper for ' + ABaseQuantity);
    if Index = -1 then
    begin
      SectionA8.Append(Format('  %s = record helper for %s', [ABaseUnit, ABaseQuantity]));
      SectionA8.Append(Format('    function geometric(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
      SectionA8.Append('  end;');
      SectionA8.Append('');
    end else
    begin
      SectionA8.Insert(Index + 1, Format('    function geometric(AValue: %s): %s;', [AInputQuantity, AResultQuantity]));
    end;

    SectionB8.Append('');
    SectionB8.Append(Format('function %s.geometric(AValue: %s): %s;', [ABaseUnit, AInputQuantity, AResultQuantity]));
    SectionB8.Append('begin');
    SectionB8.Append('  result.FValue := FValue * AValue.FValue;');
    SectionB8.Append('end;');
    SectionB8.Append('');
  end;
end;

procedure TToolKitBuilder.AddEquivalence(AFromClass, AToClass: string);
var
  i, iL, iR: longint;
  S: string;
begin
  iL := SearchLine(Format(INTF_QUANTITY, [GetQuantityType(AFromClass), '*']), SectionA2);
  iR := SearchLine(Format(INTF_QUANTITY, [GetQuantityType(AToClass), '*']), SectionA2);

  S := '';
  i := Max(iL, iR);
  if i = iL then S := AFromClass;
  if i = iR then S := AToClass;

  SectionA2 .Insert(i + 1, '  class operator ' +                       ':=(const AQuantity: ' + GetQuantityType(AFromClass) + '): ' + GetQuantityType(AToClass) + ';');
  SectionB21.Append(         'class operator ' + GetQuantityType(S) + '.:=(const AQuantity: ' + GetQuantityType(AFromClass) + '): ' + GetQuantityType(AToClass) + ';');

  SectionB21.Append('begin');
  if GetQuantityType(AFromClass) = 'double' then
    SectionB21.Append('  result.FValue := AQuantity;')
  else
    if GetQuantityType(AToClass) = 'double' then
      SectionB21.Append('  result := AQuantity.FValue;')
    else
      SectionB21.Append('  result.FValue := AQuantity.FValue;');
  SectionB21.Append('end;');
  SectionB21.Append('');
  Inc(InternalOperators);
end;

procedure TToolKitBuilder.AddResources(const AItem: TToolkitItem);
begin
  SectionA4.Append('');
  SectionA4.Append(Format('{ Quantity of %s }', [GetQuantity(AItem.FClassName)]));
  SectionA4.Append('');
  if AItem.FBaseClass = '' then
  begin

    SectionA4.Append('');
    SectionA4.Append('type');
    SectionA4.Append(Format('  %s = %s;', [GetQuantity(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA4.Append('');

    if (AItem.FIdentifierSymbol <> '') then
    begin
      SectionA4.Append('');
      SectionA4.Append('var');
      SectionA4.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FClassName)]));
      SectionA4.Append('');
      AddFactoredQuantities(AItem.FClassName, AItem.FIdentifierSymbol, '', AItem.FPrefixes);
    end;

  end else
    if AItem.FFactor = '' then
    begin

      SectionA4.Append('');
      SectionA4.Append('type');
      SectionA4.Append(Format('  %s = %s;', [GetQuantity(AItem.FClassName), GetQuantityType(AItem.FBaseClass)]));
      SectionA4.Append('');

      if (AItem.FIdentifierSymbol <> '') then
      begin
        SectionA4.Append('');
        SectionA4.Append('var');
        SectionA4.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FBaseClass)]));
        SectionA4.Append('');
        AddFactoredQuantities(AItem.FBaseClass, AItem.FIdentifierSymbol, AItem.FFactor, AItem.FPrefixes);
      end;

    end else
    begin

      SectionA4.Append('');
      SectionA4.Append('type');
      SectionA4.Append(Format('  %s = %s;', [GetQuantity(AItem.FClassName), GetQuantityType(AItem.FBaseClass)]));
      SectionA4.Append('');

      if (AItem.FIdentifierSymbol <> '') then
      begin
        if AItem.FFactor.Contains('%s') = FALSE then
        begin
          SectionA4.Append('');
          SectionA4.Append('const');
          SectionA4.Append(Format('  %s: %s = (FValue: %s);', [AItem.FIdentifierSymbol, GetQuantityType(AItem.FBaseClass), AItem.FFactor]));
          AddFactoredQuantities(AItem.FBaseClass, AItem.FIdentifierSymbol, AItem.FFactor, AItem.FPrefixes);
        end else
        begin
          SectionA4.Append('');
          SectionA4.Append('var');
          SectionA4.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FClassName)]));
          SectionA4.Append('');
        end;
      end;

    end;

  SectionA4.Append('');
  SectionA4.Append('const');
  SectionA4.Append(Format('  rs%sSymbol     = ''%s'';', [GetUnitID(AItem.FClassName), GetSymbol      (AItem.FShortSymbol)]));
  SectionA4.Append(Format('  rs%sName       = ''%s'';', [GetUnitID(AItem.FClassName), GetSingularName(AItem.FLongSymbol )]));
  SectionA4.Append(Format('  rs%sPluralName = ''%s'';', [GetUnitID(AItem.FClassName), GetPluralName  (AItem.FLongSymbol )]));
  SectionA4.Append('');
  SectionA4.Append('const');
  SectionA4.Append(Format('  c%sPrefixes  : TPrefixes  = (%s);', [GetUnitID(AItem.FClassName), GetPrefixes (AItem.FShortSymbol)]));
  SectionA4.Append(Format('  c%sExponents : TExponents = (%s);', [GetUnitID(AItem.FClassName), GetExponents(AItem.FShortSymbol)]));

  if (AItem.FBaseClass <> '') and
     (AItem.FFactor    <> '') then
  begin
    if not AItem.FFactor.Contains('%s') then
      SectionA4.Append(Format('  c%sFactor                 = %s;', [GetUnitID(AItem.FClassName), AItem.FFactor]));
  end;
end;

procedure TToolKitBuilder.AddFactoredQuantities(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
var
  i, j: longint;
  Params: string;
  Power: longint;
  LocList: TStringList;
  Str: string;
begin
  Str := '  %s: %s = (FValue: %s);';
  if AFactor <> '' then
    AFactor := AFactor + ' * ';

  if Length(APrefixes) = 24 then
  begin
    Params := APrefixes;
    SectionA4.Append('');
    SectionA4.Append('const');
  end else
    Params := '------------------------';

  Power  := 1;
  if Pos('2', AIdentifierSymbol) > 0 then Power := 2;
  if Pos('3', AIdentifierSymbol) > 0 then Power := 3;
  if Pos('4', AIdentifierSymbol) > 0 then Power := 4;
  if Pos('5', AIdentifierSymbol) > 0 then Power := 5;
  if Pos('6', AIdentifierSymbol) > 0 then Power := 6;
  if Pos('7', AIdentifierSymbol) > 0 then Power := 7;
  if Pos('8', AIdentifierSymbol) > 0 then Power := 8;
  if Pos('9', AIdentifierSymbol) > 0 then Power := 9;

  LocList := TStringList.Create;
  if (LowerCase(AIdentifierSymbol) <> 'kg' ) and
     (LowerCase(AIdentifierSymbol) <> 'kg2') then
  begin
    if Params[ 1] = 'L' then LocList.Append(Format(Str, ['quetta' + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 1] = 'S' then LocList.Append(Format(Str, ['Q'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 2] = 'L' then LocList.Append(Format(Str, ['ronna'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 2] = 'S' then LocList.Append(Format(Str, ['R'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 3] = 'L' then LocList.Append(Format(Str, ['yotta'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 3] = 'S' then LocList.Append(Format(Str, ['Y'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 4] = 'L' then LocList.Append(Format(Str, ['zetta'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 4] = 'S' then LocList.Append(Format(Str, ['Z'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 5] = 'L' then LocList.Append(Format(Str, ['exa'    + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 5] = 'S' then LocList.Append(Format(Str, ['E'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 6] = 'L' then LocList.Append(Format(Str, ['peta'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));
    if Params[ 6] = 'S' then LocList.Append(Format(Str, ['P'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));

    if Params[ 7] = 'L' then LocList.Append(Format(Str, ['tera'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 7] = 'S' then LocList.Append(Format(Str, ['T'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 8] = 'L' then LocList.Append(Format(Str, ['giga'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 8] = 'S' then LocList.Append(Format(Str, ['G'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 9] = 'L' then LocList.Append(Format(Str, ['mega'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[ 9] = 'S' then LocList.Append(Format(Str, ['M'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[10] = 'L' then LocList.Append(Format(Str, ['kilo'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[10] = 'S' then LocList.Append(Format(Str, ['k'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[11] = 'L' then LocList.Append(Format(Str, ['hecto'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[11] = 'S' then LocList.Append(Format(Str, ['h'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[12] = 'L' then LocList.Append(Format(Str, ['deca'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[12] = 'S' then LocList.Append(Format(Str, ['da'     + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[13] = 'L' then LocList.Append(Format(Str, ['deci'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[13] = 'S' then LocList.Append(Format(Str, ['d'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[14] = 'L' then LocList.Append(Format(Str, ['centi'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[14] = 'S' then LocList.Append(Format(Str, ['c'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[15] = 'L' then LocList.Append(Format(Str, ['milli'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[15] = 'S' then LocList.Append(Format(Str, ['m'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[16] = 'L' then LocList.Append(Format(Str, ['micro'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[16] = 'S' then LocList.Append(Format(Str, ['mi'     + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[17] = 'L' then LocList.Append(Format(Str, ['nano'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[17] = 'S' then LocList.Append(Format(Str, ['n'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[18] = 'L' then LocList.Append(Format(Str, ['pico'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));
    if Params[18] = 'S' then LocList.Append(Format(Str, ['p'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));

    if Params[19] = 'L' then LocList.Append(Format(Str, ['femto'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[19] = 'S' then LocList.Append(Format(Str, ['f'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[20] = 'L' then LocList.Append(Format(Str, ['atto'   + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[20] = 'S' then LocList.Append(Format(Str, ['a'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[21] = 'L' then LocList.Append(Format(Str, ['zepto'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[21] = 'S' then LocList.Append(Format(Str, ['z'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[22] = 'L' then LocList.Append(Format(Str, ['yocto'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[22] = 'S' then LocList.Append(Format(Str, ['y'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[23] = 'L' then LocList.Append(Format(Str, ['ronto'  + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[23] = 'S' then LocList.Append(Format(Str, ['r'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[24] = 'L' then LocList.Append(Format(Str, ['quecto' + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
    if Params[24] = 'S' then LocList.Append(Format(Str, ['q'      + AIdentifierSymbol, GetQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
  end else
    if (LowerCase(AIdentifierSymbol) = 'kg') then
    begin
      AIdentifierSymbol := 'g';
      LocList.Append(Format(Str, ['h'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-01']));
      LocList.Append(Format(Str, ['da' + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-02']));
      LocList.Append(Format(Str, [''   + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-03']));
      LocList.Append(Format(Str, ['d'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-04']));
      LocList.Append(Format(Str, ['c'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-05']));
      LocList.Append(Format(Str, ['m'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-06']));
      LocList.Append(Format(Str, ['mi' + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-09']));
      LocList.Append(Format(Str, ['n'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-12']));
      LocList.Append(Format(Str, ['p'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-15']));
    end else
      if (LowerCase(AIdentifierSymbol) = 'kg2') then
      begin
         AIdentifierSymbol := 'g2';
        LocList.Append(Format(Str, ['h'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-02']));
        LocList.Append(Format(Str, ['da' + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-04']));
        LocList.Append(Format(Str, [''   + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-06']));
        LocList.Append(Format(Str, ['d'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-08']));
        LocList.Append(Format(Str, ['c'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-10']));
        LocList.Append(Format(Str, ['m'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-12']));
        LocList.Append(Format(Str, ['mi' + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-18']));
        LocList.Append(Format(Str, ['n'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-24']));
        LocList.Append(Format(Str, ['p'  + AIdentifierSymbol, GetQuantityType(ABaseClass), '1E-30']));
      end;

  j := 0;
  for i := 0 to LocList.Count -1 do j := Max(j, Length(LocList[i]));
  for i := 0 to LocList.Count -1 do
  begin
    while Length(LocList[i]) < j do
      LocList[i] := ' ' + LocList[i];
    SectionA4.Append(LocList[i]);
  end;
  LocList.Destroy;
end;

procedure TToolKitBuilder.CreateSolution(var ANeighbour: TSolution);
var
  i, j: longint;
  temp: string;
begin
  i := Random(Length(ANeighbour));
  repeat
    j := Random(Length(ANeighbour));
  until i <> j;

  temp          := ANeighbour[i];
  ANeighbour[i] := ANeighbour[j];
  ANeighbour[j] := temp;
end;

function TToolKitBuilder.CalculateEnergy(const ASolution: TSolution): double;
var
  Buffer: array[0..4095] of byte;
  i, j, AIndex, ACount, ReadSize: longint;
  AMemoryStream: TMemoryStream;
  AProcess: TProcess;
  AStringList: TStringList;
  ResultStr: string;
begin
  Result := 0;
  Run(ASolution);
  FDocument.SaveToFile('adim.pas');

  AProcess := TProcess.Create(nil);
  AProcess.Executable:= 'fpc';
  AProcess.Parameters.Add('-MObjFPC');
  AProcess.Parameters.Add('-Scghi');
  AProcess.Parameters.Add('-l');
  AProcess.Parameters.Add('-vewnhibq');
  AProcess.Parameters.Add('-Fibuild\lib');
  AProcess.Parameters.Add('-Fubuild\i386-win32');
  AProcess.Parameters.Add('-FuC:\fpcupdeluxe\lazarus\packager\units\i386-win32');
  AProcess.Parameters.Add('-Fu.');
  AProcess.Parameters.Add('-FUbuild\lib');
  AProcess.Parameters.Add('-FEbuild');
  AProcess.Parameters.Add('-obuild\adimtest.exe');
  AProcess.Parameters.Add('adimtest.pas');
  AProcess.Options  := [poUsePipes, poNoConsole];
  AProcess.Priority := ppHigh;
  AProcess.Execute;

  AStringList := TStringList.Create;
  AMemoryStream := TMemoryStream.Create;
  while AProcess.Running do
  begin
    Sleep(100);
    if AProcess.Output.NumBytesAvailable > 0 then
    begin
      ReadSize := AProcess.Output.NumBytesAvailable;
      if ReadSize > SizeOf(Buffer) then
        ReadSize := SizeOf(Buffer);
      ReadSize := AProcess.Output.Read(Buffer[0], ReadSize);
      AMemoryStream.Write(Buffer[0], ReadSize);
    end;
  end;

  ResultStr := '';
  AMemoryStream.Seek(0, soFromBeginning);
  AStringList.LoadFromStream(AMemoryStream);
  for i := 0 to AStringList.Count -1 do
  begin
    if AStringList[i].Contains(' lines compiled,') then
    begin
      AIndex := Pos(', ', AStringList[i]) + 2;
      ACount := Pos(' sec', AStringList[i]) - AIndex;
      ResultStr := Copy(AStringList[i], AIndex, ACount);

      for j := Low(ResultStr) to High(ResultStr) do
        if ResultStr[j] in ['.', ','] then
          ResultStr[j] := DefaultFormatSettings.DecimalSeparator;
      Result := Result + StrToFloat(ResultStr);
      Break;
    end;
  end;

  if ResultStr = '' then
    if Assigned(FOnMessage) then
      for i := 0 to AStringList.Count -1 do
        FOnMessage(AStringList[i]);

  AMemoryStream.Destroy;
  AStringList.Free;
  AProcess.Free;

  Inc(FTestingCount);
  FOnMessage(' -> ' + FTestingCount.ToString + ' : ' + Result.ToString);
end;

procedure TToolKitBuilder.Run;
var
  i, j: longint;
  S: TStringList;
  BestSolution: TSolution;
begin
  BestSolution := nil;
  if FileExists('solution.bk') then
  begin
    if Assigned(FOnMessage) then
      FOnMessage('Loading backup ...');
    S := TStringList.Create;
    S.LoadFromFile('solution.bk');
    SetLength(BestSolution, S.Count);
    for i := Low(BestSolution) to High(BestSolution) do
      BestSolution[i] := S[i];
    S.Destroy;
    if Assigned(FOnMessage) then
      FOnMessage('Backup loaded.');
    if FExecutionTime > 0 then
      if Length(BestSolution) > 0 then
        Execute(BestSolution);
  end;

  if Length(BestSolution) = 0 then
  begin
    S := TStringList.Create;
    for i := 0 to FList.Count -1 do
    begin
      if S.IndexOf(FList[i].FClassName) = -1 then
      begin
        S.Add(FList[i].FClassName);
        if FList[i].FBaseClass = '' then
        begin
          j := Length(BestSolution);
          SetLength(BestSolution, j + 1);
          BestSolution[j] := FList[i].FClassName;
        end;
      end;
    end;
    S.Destroy;

    if FExecutionTime > 0 then
      if Length(BestSolution) > 0 then
      begin
        for I := 0 to 1000 do
          CreateSolution(BestSolution);
        Execute(BestSolution);
      end;
  end;

  Run(BestSolution);
  if Assigned(FOnMessage) then
    FOnMessage('Storing backup ...');
  S := TStringList.Create;

  for i := Low(BestSolution) to High(BestSolution) do
    S.Add(BestSolution[i]);
  S.SaveToFile('solution.bk');
  S.Destroy;
  if Assigned(FOnMessage) then
    FOnMessage('Backup stored.');

  if Assigned(FOnMessage) then
  begin
    FOnMessage('Forced   Operators: ' + IntToStr(ForcedOperators));
    FOnMessage('External Operators: ' + IntToStr(ExternalOperators));
    FOnMessage('Internal Operators: ' + IntToStr(InternalOperators));
  end;
end;

procedure TToolKitBuilder.Run(ASolution: TSolution);
var
  I, J: longint;
  Stream: TResourceStream;
begin
  SectionA0  := TStringList.Create;
  SectionA1  := TStringList.Create;
  SectionA2  := TStringList.Create;
  SectionA21 := TStringList.Create;
  SectionA22 := TStringList.Create;
  SectionA3  := TStringList.Create;
  SectionA31 := TStringList.Create;
  SectionA4  := TStringList.Create;
  SectionA5  := TStringList.Create;
  SectionA6  := TStringList.Create;
  SectionA7  := TStringList.Create;
  SectionA8  := TStringList.Create;
  SectionA9  := TStringList.Create;
  SectionA10 := TStringList.Create;

  SectionB1  := TStringList.Create;
  SectionB2  := TStringList.Create;
  SectionB21 := TStringList.Create;
  SectionB22 := TStringList.Create;
  SectionB3  := TStringList.Create;
  SectionB31 := TStringList.Create;
  SectionB4  := TStringList.Create;
  SectionB5  := TStringList.Create;
  SectionB6  := TStringList.Create;
  SectionB7  := TStringList.Create;
  SectionB8  := TStringList.Create;
  SectionB9  := TStringList.Create;
  SectionB10 := TStringList.Create;

  Stream := TResourceStream.Create(HInstance, 'SECTION-A0', RT_RCDATA);
  SectionA0.LoadFromStream(Stream);
  SectionA0.Insert(0, '');
  Stream.Destroy;

  Stream := TResourceStream.Create(HInstance, 'SECTION-A1', RT_RCDATA);
  SectionA1.LoadFromStream(Stream);
  SectionA1.Insert(0, '');
  SectionA1.Append('');
  Stream.Destroy;

  Stream := TResourceStream.Create(HInstance, 'SECTION-B1', RT_RCDATA);
  SectionB1.LoadFromStream(Stream);
  SectionB1.Insert(0, '');
  Stream.Destroy;

  SectionA2.Append('');
  SectionA2.Append('{ TQuantity classes }');
  SectionA2.Append('');
  SectionB2.Append('');
  SectionB2.Append('{ TQuantity classes }');
  SectionB2.Append('');

  SectionA21.Append('');
  SectionB21.Append('');

  SectionA22.Append('');
  SectionA22.Append('{ External Operators }');
  SectionA22.Append('');
  SectionB22.Append('');
  SectionB22.Append('{ External Operators }');
  SectionB22.Append('');

  SectionA3.Append('');
  SectionA3.Append('{ TUnit classes }');
  SectionA3.Append('');
  SectionB3.Append('');
  SectionB3.Append('{ TUnit classes }');
  SectionB3.Append('');

  SectionA31.Append('');
  SectionB31.Append('');

  SectionA4.Append('');
  SectionB4.Append('');

  SectionA5.Append('');
  SectionB5.Append('');

  SectionA6.Append('');
  SectionB6.Append('');

  SectionA7.Append('');
  SectionB7.Append('');

  SectionA8.Append('');
  SectionA8.Append('{ Helpers }');
  SectionA8.Append('');
  SectionA8.Append('type ');
  SectionB8.Append('');
  SectionB8.Append('{ Helpers }');
  SectionB8.Append('');

  SectionA9.Append('');
  SectionA9.Append('{ Power functions }');
  SectionA9.Append('');
  SectionB9.Append('');
  SectionB9.Append('{ Power functions }');
  SectionB9.Append('');

  SectionA10.Append('');
  SectionB10.Append('');

  Stream := TResourceStream.Create(HInstance, 'SECTION-A4', RT_RCDATA);
  SectionA10.LoadFromStream(Stream);
  SectionA10.Insert(0, '');
  Stream.Destroy;

  Stream := TResourceStream.Create(HInstance, 'SECTION-B4', RT_RCDATA);
  SectionB10.LoadFromStream(Stream);
  SectionB10.Insert(0, '');
  Stream.Destroy;

  BaseUnitCount     := 0;
  FactoredUnitCount := 0;
  ExternalOperators := 0;
  InternalOperators := 0;
  ForcedOperators   := 0;

  if Length(ASolution) > 0 then
  begin
    for I := Low(ASolution) to High(ASolution) do
      for J := 0 to FList.count -1 do
        if FList[J].FClassName = ASolution[I] then
        begin
          AddItem(FList[J], FALSE);
          Break;
        end;

    for J := 0 to FList.Count -1 do AddItem(FList[J], TRUE);
  end else
  begin
    for J := 0 to FList.Count -1 do AddItem(FList[J], FALSE);
    for J := 0 to FList.Count -1 do AddItem(FList[J], TRUE);
  end;

  SectionA0.Append('');
  SectionA0.Append('{');
  SectionA0.Append(Format('  ADimPas library built on %s.', [DateToStr(Now)]));
  SectionA0.Append('');
  SectionA0.Append(Format('  Number of base units: %d', [BaseUnitCount]));
  SectionA0.Append(Format('  Number of factored units: %d', [FactoredUnitCount]));
  SectionA0.Append(Format('  Number of operators: %d (%d external, %d internal)',
    [ExternalOperators + InternalOperators, ExternalOperators, InternalOperators]));
  SectionA0.Append('}');
  SectionA0.Append('');

  for I := 0 to SectionA0 .Count -1 do FDocument.Append(SectionA0 [I]);
  for I := 0 to SectionA1 .Count -1 do FDocument.Append(SectionA1 [I]);
  for I := 0 to SectionA2 .Count -1 do FDocument.Append(SectionA2 [I]);
  for I := 0 to SectionA21.Count -1 do FDocument.Append(SectionA21[I]);
  for I := 0 to SectionA22.Count -1 do FDocument.Append(SectionA22[I]);

  for I := 0 to SectionA3 .Count -1 do FDocument.Append(SectionA3 [I]);
  for I := 0 to SectionA31.Count -1 do FDocument.Append(SectionA31[I]);

  for I := 0 to SectionA4 .Count -1 do FDocument.Append(SectionA4 [I]);
  for I := 0 to SectionA5 .Count -1 do FDocument.Append(SectionA5 [I]);
  for I := 0 to SectionA6 .Count -1 do FDocument.Append(SectionA6 [I]);
  for I := 0 to SectionA7 .Count -1 do FDocument.Append(SectionA7 [I]);
  for I := 0 to SectionA8 .Count -1 do FDocument.Append(SectionA8 [I]);
  for I := 0 to SectionA9 .Count -1 do FDocument.Append(SectionA9 [I]);
  for I := 0 to SectionA10.Count -1 do FDocument.Append(SectionA10[I]);

  for I := 0 to SectionB1 .Count -1 do FDocument.Append(SectionB1 [I]);
  for I := 0 to SectionB2 .Count -1 do FDocument.Append(SectionB2 [I]);
  for I := 0 to SectionB21.Count -1 do FDocument.Append(SectionB21[I]);
  for I := 0 to SectionB22.Count -1 do FDocument.Append(SectionB22[I]);

  for I := 0 to SectionB3 .Count -1 do FDocument.Append(SectionB3 [I]);
  for I := 0 to SectionB31.Count -1 do FDocument.Append(SectionB31[I]);

  for I := 0 to SectionB4 .Count -1 do FDocument.Append(SectionB4 [I]);
  for I := 0 to SectionB5 .Count -1 do FDocument.Append(SectionB5 [I]);
  for I := 0 to SectionB6 .Count -1 do FDocument.Append(SectionB6 [I]);
  for I := 0 to SectionB7 .Count -1 do FDocument.Append(SectionB7 [I]);
  for I := 0 to SectionB8 .Count -1 do FDocument.Append(SectionB8 [I]);
  for I := 0 to SectionB9 .Count -1 do FDocument.Append(SectionB9 [I]);
  for I := 0 to SectionB10.Count -1 do FDocument.Append(SectionB10[I]);
  CleanDocument(FDocument);

  SectionB10.Destroy;
  SectionB9 .Destroy;
  SectionB8 .Destroy;
  SectionB7 .Destroy;
  SectionB6 .Destroy;
  SectionB5 .Destroy;
  SectionB4 .Destroy;
  SectionB31.Destroy;
  SectionB3 .Destroy;
  SectionB22.Destroy;
  SectionB21.Destroy;
  SectionB2 .Destroy;
  SectionB1 .Destroy;

  SectionA10.Destroy;
  SectionA9 .Destroy;
  SectionA8 .Destroy;
  SectionA7 .Destroy;
  SectionA6 .Destroy;
  SectionA5 .Destroy;
  SectionA4 .Destroy;
  SectionA31.Destroy;
  SectionA3 .Destroy;
  SectionA22.Destroy;
  SectionA21.Destroy;
  SectionA2 .Destroy;
  SectionA1 .Destroy;
  SectionA0 .Destroy;
end;

function TToolKitBuilder.SearchLine(const ALine: string; ASection: TStringList): longint;
var
  i: longint;
begin
  for i := 0 to ASection.Count -1 do
  begin
    if IsWild(ASection[i], ALine, False) then Exit(i);
  end;
  Result := -1;
end;

// TToolKitList

constructor TToolKitList.Create;
begin
  inherited Create;
  FList := nil;
end;

destructor TToolKitList.Destroy;
begin
  FList := nil;
  inherited Destroy;
end;

function TToolKitList.GetItem(Index: longint): TToolKitItem;
begin
  result := FList[Index];
end;

function TToolKitList.GetCount: longint;
begin
  result := Length(FList);
end;

function TToolKitList.Add(AItem: TToolKitItem): string;
var
  i, j, k: longint;
  T: TToolKitItem;
  T1, T2: string;
begin
  result := '';
  if (AItem.FBaseClass = '') then
  begin

    if AItem.FClassName = 'TKilogram?' then AItem.FExponents[1] := 2 else AItem.FExponents[1] := 0;
    if AItem.FClassName = 'TMeter?'    then AItem.FExponents[2] := 2 else AItem.FExponents[2] := 0;
    if AItem.FClassName = 'TSecond?'   then AItem.FExponents[3] := 2 else AItem.FExponents[3] := 0;
    if AItem.FClassName = 'TKelvin?'   then AItem.FExponents[4] := 2 else AItem.FExponents[4] := 0;
    if AItem.FClassName = 'TAmpere?'   then AItem.FExponents[5] := 2 else AItem.FExponents[5] := 0;
    if AItem.FClassName = 'TMole?'     then AItem.FExponents[6] := 2 else AItem.FExponents[6] := 0;
    if AItem.FClassName = 'TCandela?'  then AItem.FExponents[7] := 2 else AItem.FExponents[7] := 0;
    if AItem.FClassName = 'TRadian?'   then AItem.FExponents[8] := 2 else AItem.FExponents[8] := 0;

    i := Search(AItem.FClassParent1);
    j := Search(AItem.FClassParent2);
    if (i <> -1) then
    begin
      AItem.FExponents[1] := FList[i].FExponents[1];
      AItem.FExponents[2] := FList[i].FExponents[2];
      AItem.FExponents[3] := FList[i].FExponents[3];
      AItem.FExponents[4] := FList[i].FExponents[4];
      AItem.FExponents[5] := FList[i].FExponents[5];
      AItem.FExponents[6] := FList[i].FExponents[6];
      AItem.FExponents[7] := FList[i].FExponents[7];
      AItem.FExponents[8] := FList[i].FExponents[8];
    end;

    if (j <> -1) then
    begin
      if (UpperCase(AItem.FOperator) = '/'         ) or
         (UpperCase(AItem.FOperator) = '/='        ) or
         (UpperCase(AItem.FOperator) = 'DOT/'      ) or
         (UpperCase(AItem.FOperator) = 'WEDGE/'    ) or
         (UpperCase(AItem.FOperator) = 'CROSS/'    ) or
         (UpperCase(AItem.FOperator) = 'GEOMETRIC/') then
      begin
        AItem.FExponents[1] := AItem.FExponents[1] - FList[j].FExponents[1];
        AItem.FExponents[2] := AItem.FExponents[2] - FList[j].FExponents[2];
        AItem.FExponents[3] := AItem.FExponents[3] - FList[j].FExponents[3];
        AItem.FExponents[4] := AItem.FExponents[4] - FList[j].FExponents[4];
        AItem.FExponents[5] := AItem.FExponents[5] - FList[j].FExponents[5];
        AItem.FExponents[6] := AItem.FExponents[6] - FList[j].FExponents[6];
        AItem.FExponents[7] := AItem.FExponents[7] - FList[j].FExponents[7];
        AItem.FExponents[8] := AItem.FExponents[8] - FList[j].FExponents[8];
      end else
      if (UpperCase(AItem.FOperator) = '*'        ) or
         (UpperCase(AItem.FOperator) = '*='       ) or
         (UpperCase(AItem.FOperator) = 'DOT'      ) or
         (UpperCase(AItem.FOperator) = 'WEDGE'    ) or
         (UpperCase(AItem.FOperator) = 'CROSS'    ) or
         (UpperCase(AItem.FOperator) = 'GEOMETRIC') then
      begin
        AItem.FExponents[1] := AItem.FExponents[1] + FList[j].FExponents[1];
        AItem.FExponents[2] := AItem.FExponents[2] + FList[j].FExponents[2];
        AItem.FExponents[3] := AItem.FExponents[3] + FList[j].FExponents[3];
        AItem.FExponents[4] := AItem.FExponents[4] + FList[j].FExponents[4];
        AItem.FExponents[5] := AItem.FExponents[5] + FList[j].FExponents[5];
        AItem.FExponents[6] := AItem.FExponents[6] + FList[j].FExponents[6];
        AItem.FExponents[7] := AItem.FExponents[7] + FList[j].FExponents[7];
        AItem.FExponents[8] := AItem.FExponents[8] + FList[j].FExponents[8];
      end;

    end else
    begin
      k := 0;
      if UpperCase(AItem.FOperator) = 'RECIPROCAL'   then k := -1 else
      if UpperCase(AItem.FOperator) = 'SQUAREROOT'   then k := -2 else
      if UpperCase(AItem.FOperator) = 'CUBICROOT'    then k := -3 else
      if UpperCase(AItem.FOperator) = 'SQUAREPOWER'  then k :=  2 else
      if UpperCase(AItem.FOperator) = 'CUBICPOWER'   then k :=  3 else
      if UpperCase(AItem.FOperator) = 'QUARTICPOWER' then k :=  4 else
      if UpperCase(AItem.FOperator) = 'QUINTICPOWER' then k :=  5 else
      if UpperCase(AItem.FOperator) = 'SEXTICPOWER'  then k :=  6 else
      if UpperCase(AItem.FOperator) = 'NORM2'        then k := -2 else
      if UpperCase(AItem.FOperator) = 'NORM'         then k :=  0 else
      if UpperCase(AItem.FOperator) = 'DUAL'         then k :=  0 else
      if UpperCase(AItem.FOperator) = 'EXTRACT'      then k :=  0 else
      if UpperCase(AItem.FOperator) = 'EXPORT'       then k :=  0 else
      if UpperCase(AItem.FOperator) = ''             then k :=  0;

      if (k = -1) or (k > 0) then
      begin
        AItem.FExponents[1] := AItem.FExponents[1] * k;
        AItem.FExponents[2] := AItem.FExponents[2] * k;
        AItem.FExponents[3] := AItem.FExponents[3] * k;
        AItem.FExponents[4] := AItem.FExponents[4] * k;
        AItem.FExponents[5] := AItem.FExponents[5] * k;
        AItem.FExponents[6] := AItem.FExponents[6] * k;
        AItem.FExponents[7] := AItem.FExponents[7] * k;
        AItem.FExponents[8] := AItem.FExponents[8] * k;
      end else
      if (k < 0) then
      begin
        AItem.FExponents[1] := AItem.FExponents[1] div Abs(k);
        AItem.FExponents[2] := AItem.FExponents[2] div Abs(k);
        AItem.FExponents[3] := AItem.FExponents[3] div Abs(k);
        AItem.FExponents[4] := AItem.FExponents[4] div Abs(k);
        AItem.FExponents[5] := AItem.FExponents[5] div Abs(k);
        AItem.FExponents[6] := AItem.FExponents[6] div Abs(k);
        AItem.FExponents[7] := AItem.FExponents[7] div Abs(k);
        AItem.FExponents[8] := AItem.FExponents[8] div Abs(k);
      end;
    end;

    // check
    for i := Low(FList) to High(FList) do
    begin
      T  := FList[i];
      if (T.FBaseClass = '') then
      begin
        T1 := StringReplace(    T.FClassName, 'T' + VECPrefix, 'T', []);
        T2 := StringReplace(AItem.FClassName, 'T' + VECPrefix, 'T', []);

        if (T1              <> T2                 ) and
           (T.FExponents[1] =  AItem.FExponents[1]) and
           (T.FExponents[2] =  AItem.FExponents[2]) and
           (T.FExponents[3] =  AItem.FExponents[3]) and
           (T.FExponents[4] =  AItem.FExponents[4]) and
           (T.FExponents[5] =  AItem.FExponents[5]) and
           (T.FExponents[6] =  AItem.FExponents[6]) and
           (T.FExponents[7] =  AItem.FExponents[7]) and
           (T.FExponents[8] =  AItem.FExponents[8]) then
        begin
          result := Format('WARNING (%s): %s (%s) is equal to %s (%s)', [
            AItem.FOperator, AItem.FClassName, GetShortSymbol(AItem), T.FClassName, GetShortSymbol(T)]);
        end; //else
        //result := Format('CHECKING %s (%s) and %s (%s)', [
        //  AItem.FClassName, GetShortSymbol(AItem), T.FClassName, GetShortSymbol(T)]);
      end;
    end;
  end;

  //if (AItem.FBaseClass = '') then
  //  result := Format('CHECKING %s (%s)', [AItem.FClassName, GetShortSymbol(AItem)]);

  SetLength(FList, Length(FList) + 1);
  FList[High(FList)] := AItem;
end;

function TToolKitList.GetShortSymbol(const AItem: TToolKitItem): string;
var
  i: longint;
  Reciprocal: boolean;
begin
  result := '';
  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] > 0 then
    begin
      case i of
        1: result := result + '.%sg';
        2: result := result + '.%sm';
        3: result := result + '.%ss';
        4: result := result + '.%sK';
        5: result := result + '.%sA';
        6: result := result + '.%smol';
        7: result := result + '.%scd';
        8: result := result + '.%srad';
      end;

      case AItem.FExponents[i] of
         1: result := '√' + result;
         3: result := '√' + result + '3';
         4: result :=       result + '2';
         6: result :=       result + '3';
         8: result :=       result + '4';
        10: result :=       result + '5';
        12: result :=       result + '6';
      end;
    end;
  end;

  Reciprocal := result = '';
  if Reciprocal then result := '1/';

  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] < 0 then
    begin
      if not Reciprocal then
        result := result + '/';

      case Abs(AItem.FExponents[i]) of
        1: result := result + '√';
        3: result := result + '√';
      end;

      case i of
        1: result := result + '%sg';
        2: result := result + '%sm';
        3: result := result + '%ss';
        4: result := result + '%sK';
        5: result := result + '%sA';
        6: result := result + '%smol';
        7: result := result + '%scd';
        8: result := result + '%srad';
      end;

      case Abs(AItem.FExponents[i]) of
        3: result := result + '3';
        4: result := result + '2';
        6: result := result + '3';
        8: result := result + '4';
       10: result := result + '5';
       12: result := result + '6';
      end;
    end;
  end;

  if Pos('.', result) = 1 then
  begin
    System.Delete(result, 1, 1);
  end;
  result := StringReplace(result, '%srad2',  '%ssr',  [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%srad4',  '%ssr2', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%srad6',  '%ssr3', [rfReplaceAll, rfIgnoreCase]);
end;

function TToolKitList.GetLongSymbol(const AItem: TToolKitItem): string;
var
  i: longint;
  Reciprocal: boolean;
begin
  result := '';
  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] > 0 then
    begin
      case AItem.FExponents[i] of
         1: result := result + ' square root ';
         3: result := result + ' square root cubic ';
         4: result := result + ' square ';
         6: result := result + ' cubic ';
         8: result := result + ' quartic ';
        10: result := result + ' quintic ';
        12: result := result + ' sextic ';
      end;

      case i of
        1: result := result + ' %sgram';
        2: result := result + ' %smeter';
        3: result := result + ' %ssecond';
        4: result := result + ' %skelvin';
        5: result := result + ' %sampere';
        6: result := result + ' %smole';
        7: result := result + ' %scandela';
        8: result := result + ' %sradian';
      end;
    end;
  end;

  Reciprocal := result = '';
  if Reciprocal then
    result := 'reciprocal'
  else
    result := result + '?';

  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] < 0 then
    begin
      if not Reciprocal then
        result := result + ' per ';

      case Abs(AItem.FExponents[i]) of
         1: result := result + ' square root ';
         3: result := result + ' square root cubic ';
         4: result := result + ' square ';
         6: result := result + ' cubic ';
         8: result := result + ' quartic ';
        10: result := result + ' quintic ';
        12: result := result + ' sextic ';
      end;

      case i of
        1: result := result + ' %sgram';
        2: result := result + ' %smeter';
        3: result := result + ' %ssecond';
        4: result := result + ' %skelvin';
        5: result := result + ' %sampere';
        6: result := result + ' %smole';
        7: result := result + ' %scandela';
        8: result := result + ' %sradian';
      end;
    end;
  end;

  if Pos(' ', result) = 1 then
  begin
    System.Delete(result, 1, 1);
  end;
  result := StringReplace(result, '  ',             ' ',                 [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, 'square radian',  'steradian',         [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, 'quartic radian', 'square steradian',  [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, 'sextic radian',  'cubic steradian',   [rfReplaceAll, rfIgnoreCase]);
end;

function TToolKitList.GetClassName(const AItem: TToolKitItem): string;
var
  i: longint;
  Reciprocal: boolean;
begin
  result := '';
  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] > 0 then
    begin
      case Abs(AItem.FExponents[i]) of
        2: result := result + 'Square';
        3: result := result + 'Cubic';
        4: result := result + 'Quartic';
        5: result := result + 'Quintic';
        6: result := result + 'Sextic';
      end;

      case i of
        1: result := result + 'Kilogram';
        2: result := result + 'Meter';
        3: result := result + 'Second';
        4: result := result + 'Kelvin';
        5: result := result + 'Ampere';
        6: result := result + 'Mole';
        7: result := result + 'Candela';
        8: result := result + 'Radian';
      end;
    end;
  end;

  Reciprocal := result = '';
  if Reciprocal then
    result := 'Reciprocal'
  else
    result := result + '?';

  for i := Low(AItem.FExponents) to High(AItem.FExponents) do
  begin
    if AItem.FExponents[i] < 0 then
    begin
      if not Reciprocal then
        result := result + 'Per';

      case Abs(AItem.FExponents[i]) of
        2: result := result + 'Square';
        3: result := result + 'Cubic';
        4: result := result + 'Quartic';
        5: result := result + 'Quintic';
        6: result := result + 'Sextic';
      end;

      case i of
        1: result := result + 'Kilogram';
        2: result := result + 'Meter';
        3: result := result + 'Second';
        4: result := result + 'Kelvin';
        5: result := result + 'Ampere';
        6: result := result + 'Mole';
        7: result := result + 'Candela';
        8: result := result + 'Radian';
      end;
    end;
  end;

  if (AItem.FClassType <>        '') and
     (AItem.FClassType <> 'TScalar') then
    result := 'T' + VecPrefix + result
  else
    result := 'T' + result;

  result := StringReplace(result, 'SquareRadian',  'Steradian',       [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, 'QuarticRadian', 'SquareSteradian', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, 'SexticRadian',  'CubicSteradian',  [rfReplaceAll, rfIgnoreCase]);
end;

function TToolKitList.Search(const AClassName: string): longint;
var
  i: longint;
begin
  if AClassName <> '' then
    for i := Low(FList) to High(FList) do
    begin
      if CompareText(FList[i].FClassName, AClassName) = 0 then Exit(i);
    end;
  result := -1;
end;

procedure TToolKitList.Delete(Index: longint);
begin
  System.Delete(FList, Index, 1);
end;

procedure TToolKitList.Clear;
begin
  FList := nil;
end;

end.

