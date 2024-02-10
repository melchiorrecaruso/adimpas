{
  Description: Common unit.

  Copyright (C) 2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  Classes, SimulatedAnnealing, SysUtils;

type
  TToolKitItem = record
    FClassName:        string;
    FOperator:         string;
    FClassParent1:     string;
    FClassParent2:     string;
    FComment:          string;
    FLongSymbol:       string;
    FShortSymbol:      string;
    FIdentifierSymbol: string;
    FBaseClass:        string;
    FFactor:           string;
    FPrefixes:         string;
    FVector:           string;
  end;

  TToolKitExponent = record
    FExponents: array [1..7] of longint;
    FClassName: string;
  end;

  TToolKitList = class(TSimulatedAnnealing)
  private
    CheckList: array of TToolKitExponent;
    ClassList: TStringList;
    CommUnits: TStringList;

    BaseUnitCount:     longint;
    FactoredUnitCount: longint;
    ExternalOperators: longint;
    ForcedOperators:   longint;
    InternalOperators: longint;
    TestingCount:      longint;

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

    FList: array of TToolkitItem;
    FOnMessage: TMessageEvent;

    function GetCount: longint;
    function GetItem(Index: longint): TToolkitItem;

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
    procedure AddUnitOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string);

    procedure AddFactoredQuantities(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);

    procedure AddHelperSquaredNorm(const AItem: TToolkitItem);
    procedure AddHelperNorm(const AItem: TToolkitItem);

    procedure AddHelperDot(const AItem: TToolkitItem);
    procedure AddHelperCross(const AItem: TToolkitItem);
    procedure AddHelperWedge(const AItem: TToolkitItem);



    procedure AddEquivalence(AClassName, ABaseClass: string);



    procedure AddItemResource(const AItem: TToolkitItem);
    procedure AddVECItemResource(const AItem: TToolkitItem);


    procedure CheckClass(AClassName, AOperator, AClassParent1, AClassParent2: string);
    function GetIndex(const AClassName: string): longint;
    function GetSIunit(Index: longint): string;
    function Find(const S: string; List: TStringList): longint;

    procedure Run(ASolution: TSolution);
  public
    constructor Create(OnMessage: TMessageEvent);
    destructor Destroy; override;
    function CalculateEnergy(const ASolution: TSolution): double; override;
    procedure CreateSolution(var ANeighbour: TSolution); override;
    procedure Add(const AItem: TToolkitItem);
    procedure Run;
  public
    property Items[Index: longint]: TToolkitItem read GetItem; Default;
    property Document: TStringList read FDocument;
    property Messages: TStringList read FMessages;
    property Count: longint read GetCount;
  end;


implementation

uses
  Common, DateUtils, LCLType, Math, Process;

const
  adiminc     = 'adim.inc';
  adimVECinc  = 'adimVEC.inc';
  adimBVECinc = 'adimBVEC.inc';
  adimMVECinc = 'adimMVEC.inc';


// TToolkitList

constructor TToolkitList.Create(OnMessage: TMessageEvent);
begin
  inherited Create(OnMessage);
  FOnMessage := OnMessage;
  FList      := nil;
  ClassList  := TStringList.Create;
  CommUnits  := TStringList.Create;
  FDocument  := TStringList.Create;
  FMessages  := TStringList.Create;

  TestingCount := 0;
  ClassList .Sorted := TRUE;
  Randomize;
end;

destructor TToolkitList.Destroy;
begin
  FList := nil;
  ClassList.Destroy;
  CommUnits.Destroy;
  FMessages.Destroy;
  FDocument.Destroy;
  inherited Destroy;
end;

procedure TToolkitList.Add(const AItem: TToolkitItem);
var
  index: longint;
begin
  index := Length(FList);
  SetLength(FList, index + 1);
  FList[index] := AItem;
end;

function TToolkitList.GetItem(Index: longint): TToolkitItem;
begin
  Result := FList[Index];
end;

function TToolkitList.GetCount: longint;
begin
  Result := Length(FList);
end;

procedure TToolkitList.AddItem(const AItem: TToolkitItem; AddOperator: boolean);
begin
  if ClassList.IndexOf(GetQuantityType(AItem.FClassName)) = -1 then
  begin
    ClassList.Append(GetQuantityType(AItem.FClassName));

    if AItem.FVector = '' then
    begin
      if (AItem.FBaseClass = '') then AddBaseItem  (AItem) else
      if (AItem.FFactor    = '') then AddClonedItem(AItem) else AddFactoredItem(AItem);
    end else
    begin
      if (AItem.FBaseClass = '') then AddVECBaseItem  (AItem) else
      if (AItem.FFactor    = '') then AddVECClonedItem(AItem) else AddVECFactoredItem(AItem);
    end;
  end;

  if AddOperator then
  begin
    if AItem.FVector = '' then
      AddItemOperators(AItem)
    else
      AddVECItemOperators(AItem);
  end;
end;

procedure TToolkitList.AddBaseItem(const AItem: TToolkitItem);
begin
  // BASE UNIT
  if (AItem.FOperator = '*') then
  begin
    SectionA2.Insert(3, '');
    SectionA2.Insert(4, Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
    SectionA2.Insert(5, Format(INTF_END, [adiminc]));
    SectionA2.Insert(6, '');
  end else
  begin
    SectionA2.Append('');
    SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
    SectionA2.Append(Format(INTF_END, [adiminc]));
    SectionA2.Append('');
  end;

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));
  SectionA3.Append('');

  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));
  SectionB2.Append('');

  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));

  AddItemResource(AItem);
  Inc(BaseUnitCount);
end;

procedure TToolkitList.AddVECBaseItem(const AItem: TToolkitItem);
var
  xClassName: string;
begin
  // VEC BASE UNIT
  SectionA2.Insert(3, '');
  SectionA2.Insert(4, Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adimVECinc]));
  SectionA2.Insert(5, Format(INTF_END, [adimVECinc]));
  SectionA2.Insert(6, '');

  //SectionA3.Append('');
  //SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));
  //SectionA3.Append(Format(INTF_END, [adiminc]));
  //SectionA3.Append('');

  xClassName := AItem.FClassName;
  while Pos('CL3', xClassName) > 0 do Delete(xClassName, Pos('CL3', xClassName), Length('CL3'));

  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (xClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(xClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (xClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (xClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (xClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adimVECinc]));
  SectionB2.Append('');

  //SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adimVECinc]));

  AddVECItemResource(AItem);
  Inc(BaseUnitCount);
end;


procedure TToolkitList.AddClonedItem(const AItem: TToolkitItem);
begin
  // CLONED UNIT
  SectionA2.Append('');
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
  SectionA2.Append(Format(INTF_END, [adiminc]));
  SectionA2.Append('');

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FBaseClass), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));
  SectionA3.Append('');

  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));
  SectionB2.Append('');

  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FBaseClass), GetUnitType(AItem.FClassName), adiminc]));

  AddItemResource(AItem);
  AddHelper(AItem.FClassName, AItem.FBaseClass, '');
  AddHelper(AItem.FBaseClass, AItem.FClassName, '');

  Inc(FactoredUnitCount);
end;

procedure TToolkitList.AddVECClonedItem(const AItem: TToolkitItem);
begin

end;

procedure TToolkitList.AddFactoredItem(const AItem: TToolkitItem);
begin
  // FACTORED UNIT
  SectionA2.Append('');
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adiminc]));
  SectionA2.Append(Format(INTF_END, [adiminc]));
  SectionA2.Append('');

  SectionA3.Append('');
  SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));
  SectionA3.Append(Format(INTF_END, [adiminc]));

  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_CFACTOR,       [GetFactorConst               (AItem.FClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adiminc]));
  SectionB2.Append('');

  SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adiminc]));

  AddItemResource(AItem);
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

procedure TToolkitList.AddVECFactoredItem(const AItem: TToolkitItem);
var
  xClassName: string;
begin
  // VEC FACTORED UNIT
  SectionA2.Append('');
  SectionA2.Append(Format(INTF_QUANTITY, [GetQuantityType(AItem.FClassName), adimVECinc]));
  SectionA2.Append(Format(INTF_END, [adimVECinc]));
  SectionA2.Append('');

  //SectionA3.Append('');
  //SectionA3.Append(Format(INTF_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adimVECinc]));
  //SectionA3.Append(Format(INTF_END, [adimVECinc]));

  xClassName := AItem.FClassName;
  while Pos('CL3', xClassName) > 0 do Delete(xClassName, Pos('CL3', xClassName), Length('CL3'));

  SectionB2.Append(Format(IMPL_CSYMBOL,       [GetSymbolResourceString      (xClassName)]));
  SectionB2.Append(Format(IMPL_CSINGULARNAME, [GetSingularNameResourceString(xClassName)]));
  SectionB2.Append(Format(IMPL_CPLURALNAME,   [GetPluralNameResourceString  (xClassName)]));
  SectionB2.Append(Format(IMPL_CPREFIXES,     [GetPrefixesConst             (xClassName)]));
  SectionB2.Append(Format(IMPL_CEXPONENTS,    [GetExponentsConst            (xClassName)]));
  SectionB2.Append(Format(IMPL_CFACTOR,       [GetFactorConst               (xClassName)]));
  SectionB2.Append(Format(IMPL_QUANTITY,      [GetQuantityType              (AItem.FClassName), adimVECinc]));
  SectionB2.Append('');

  //SectionB3.Append(Format(IMPL_UNIT, [GetQuantityType(AItem.FClassName), GetUnitType(AItem.FClassName), adimVECinc]));


  (*
  if AItem.FFactor.Contains('%s') = FALSE then
  begin
    AddHelper(AItem.FClassName, AItem.FBaseClass, 'FValue / ' + GetFactorConst(AItem.FClassName));
  end else
  begin
    AddHelper(AItem.FBaseClass, AItem.FClassName, Format(Copy(AItem.FFactor, 1, Pos('|', AItem.FFactor) -1), ['FValue']));
    AddHelper(AItem.FClassName, AItem.FBaseClass, Format(Copy(AItem.FFactor, Pos('|', AItem.FFactor) + 1, Length(AItem.FFactor)), ['FValue']));
  end;
  *)
  Inc(FactoredUnitCount);
end;

procedure TToolkitList.AddItemOperators(const AItem: TToolkitItem);
begin
  if (AItem.FBaseClass = '') then
  begin
    CheckClass(AItem.FClassName, AItem.FOperator, AItem.FClassParent1, AItem.FClassParent2);

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

      if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
      if Pos('OP2', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent2), GetUnitType(AItem.FClassParent1), GetQuantityType(AItem.FClassName));
      if Pos('OP3', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2));
      if Pos('OP4', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));

    end else
      if AItem.FOperator = '/' then
      begin
        AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
        AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent1));
        AddQuantityOperator('*', GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
        AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2));

        // OP1: C=A/B
        // OP2: B*C=A
        // OP3: C*B=A
        // OP4: B=A/C

        if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
        if Pos('OP2', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent2), GetUnitType(AItem.FClassName),    GetQuantityType(AItem.FClassParent1));
        if Pos('OP3', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassName),    GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));
        if Pos('OP4', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2));

      end else
        if Pos('power', LowerCase(AItem.FOperator)) > 0 then
        begin
          AddPower(AItem.FOperator, AItem.FClassParent1, AItem.FClassName);
        end;

  end else
    if (AItem.FOperator = '=') then
    begin

      if AItem.FBaseClass <> '' then;
        CommUnits.Add(GetQuantityType(AItem.FBaseClass));

      SectionA7.Append('');
      SectionB7.Append('');
      AddEquivalence(AItem.FClassName, AItem.FBaseClass);
      AddHelper(AItem.FClassName, AItem.FBaseClass, '');
      SectionB7.Append('');
      AddEquivalence(AItem.FBaseClass, AItem.FClassName);
      AddHelper(AItem.FBaseClass, AItem.FClassName, '');
      SectionB7.Append('');
      SectionA7.Append('');
    end else
      if (LowerCase(AItem.FOperator) = 'helper') then
      begin
        SectionA7.Append('');
        SectionB7.Append('');
        AddHelper(AItem.FClassName, AItem.FBaseClass, '');
        SectionB7.Append('');
        AddHelper(AItem.FBaseClass, AItem.FClassName, '');
        SectionB7.Append('');
        SectionA7.Append('');
      end;
end;

procedure TToolkitList.AddVECItemOperators(const AItem: TToolkitItem);
begin


  if (AItem.FBaseClass = '') then
  begin

    if AItem.FOperator = '*' then
    begin

      if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('*', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));

      AddQuantityOperator('*', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));

      if (GetQuantityType(AItem.FClassParent1) <> 'TVector') and
         (GetQuantityType(AItem.FClassParent2) <> 'TVector') then
      begin
        AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName));
      end;

    end else
      if AItem.FOperator = '/' then
      begin

        if Pos('OP1', AItem.FFactor) > 0 then AddUnitOperator('/', GetQuantityType(AItem.FClassParent1), GetUnitType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));

        AddQuantityOperator('/', GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName));
        AddQuantityOperator('*', GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent1));
        AddQuantityOperator('*', GetQuantityType(AItem.FClassName),    GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1));

      end else

        if AItem.FOperator = 'DOT'   then AddHelperDot        (AItem) else
        if AItem.FOperator = 'NORM'  then AddHelperNorm       (AItem) else
        if AItem.FOperator = 'NORM2' then AddHelperSquaredNorm(AItem);


  end else
    if (AItem.FOperator = '=') then
    begin


    end else
      if (LowerCase(AItem.FOperator) = 'helper') then
      begin

      end;





end;



procedure TToolkitList.AddQuantityOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string);
var
  i, iL, iR, iX: longint;
  j: longint;
  ABaseClass, S: string;
begin
  iL := Find(Format(INTF_QUANTITY, [ALeftClass  , AdimInc]), SectionA2);
  iR := Find(Format(INTF_QUANTITY, [ARightClass , AdimInc]), SectionA2);
  iX := Find(Format(INTF_QUANTITY, [AResultClass, AdimInc]), SectionA2);

  ABaseClass := '';
  i := Max(iL, iR);
  if i = iL then ABaseClass := ALeftClass;
  if i = iR then ABaseClass := ARightClass;
  if i < iX then ABaseClass := '';

  for j := 0 to CommUnits.Count -1 do
  begin
    if ABaseClass <> '' then
    begin
      if (i = iL) and (ALeftClass  = CommUnits[j]) then ABaseClass := '';
      if (i = iR) and (ARightClass = CommUnits[j]) then ABaseClass := '';
      if ABaseClass = '' then Inc(ForcedOperators);
    end;
  end;

  if ClassList.IndexOf(ABaseClass + '.' + ALeftClass + AOperator + ARightClass) = -1 then
  begin
    ClassList.Append(ABaseClass + '.' + ALeftClass + AOperator + ARightClass);

    j := -1;
    if ABaseClass = '' then
    begin
      SectionA22.Append(Format(INTF_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB22.Append(Format(IMPL_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(ExternalOperators);
    end else
    begin
      j := Find(Format(IMPL_QUANTITY, [ABaseClass, adiminc]), SectionB2);
      SectionA2.Insert(i + 1, Format(INTF_OP_CLASS, [            AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB2.Insert(j + 2, Format(IMPL_OP_CLASS, [ABaseClass, AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(InternalOperators);
    end;

    if AResultClass = 'double' then
      S := '  result :='
    else
      S := '  result.FValue :=';

    if (ALeftClass = 'double' ) or
       (ALeftClass = 'TVector') then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.FValue ' + AOperator;

    if ARightClass = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.FValue;';

    if ABaseClass = '' then
    begin
      SectionB22.Append('begin');
      SectionB22.Append(S);
      SectionB22.Append('end;');
      SectionB22.Append('');
    end else
    begin
      SectionB2.Insert(j + 3, 'begin');
      SectionB2.Insert(j + 4, S);
      SectionB2.Insert(j + 5, 'end;');
      SectionB2.Insert(j + 6, '');
    end;

  end else
    FMessages.Append('ERROR: operator ' + AOperator + '(' + ALeftClass + '; ' + ARightClass + ') : ' + AResultClass + '; already esists.');
end;

procedure TToolkitList.AddUnitOperator(const AOperator, ALeftClass, ARightClass, AResultClass: string);
var
  i: longint;
  BaseQuantity: string;
begin
  BaseQuantity := GetQuantityType(GetBaseClass(ARightClass));
  if ClassList.IndexOf(ALeftClass + AOperator + ARightClass) = -1 then
  begin
    ClassList.Append(ALeftClass + AOperator + ARightClass);

    i := Find(Format(INTF_UNIT, [BaseQuantity, ARightClass, AdimInc]), SectionA3);

    SectionA3 .Insert(i + 1, Format(INTF_OP_CLASS, [             AOperator, ALeftClass, ARightClass, AResultClass]));
    SectionB31.Append('');
    SectionB31.Append(       Format(IMPL_OP_CLASS, [ARightClass, AOperator, ALeftClass, ARightClass, AResultClass]));
    SectionB31.Append('begin');
    if AResultClass <> 'double' then
    begin

      if (ALeftClass = 'double'      ) or
         (ALeftClass = 'TVector'     ) or
         (ALeftClass = 'TBivector'   ) or
         (ALeftClass = 'TMultivector') then
        SectionB31.Append('  result.FValue := ALeft;')
      else
        SectionB31.Append('  result.FValue := ALeft.FValue;');

    end else
    begin
      if ALeftClass <> 'double' then
        SectionB31.Append('  result := ALeft.FValue;')
      else
        SectionB31.Append('  result := ALeft;');
    end;
    SectionB31.Append('end;');
    SectionB31.Append('');
    Inc(InternalOperators);
  end;
end;

procedure TToolkitList.AddPower(AOperator, AQuantity, AResult: string);
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

procedure TToolkitList.AddHelper(AClassName, ABaseClass, AFactor: string);
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


procedure TToolkitList.AddHelperSquaredNorm(const AItem: TToolkitItem);
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

procedure TToolkitList.AddHelperNorm(const AItem: TToolkitItem);
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

procedure TToolkitList.AddHelperDot(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassParent1) + ' = record helper for ' + GetQuantityType(AItem.FClassParent1));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append(Format('    function Dot(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Dot(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Dot(AValue: %s): %s;', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Dot(AValue.FValue);');
  SectionB8.Append('end;');
  SectionB8.Append('');

  // Commutate

  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassParent2) + ' = record helper for ' + GetQuantityType(AItem.FClassParent2));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassParent2), GetQuantityType(AItem.FClassParent2)]));
    SectionA8.Append(Format('    function Dot(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Dot(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Dot(AValue: %s): %s;', [GetUnitTypeHelper(AItem.FClassParent2), GetQuantityType(AItem.FClassParent1), GetQuantityType(AItem.FClassName)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Dot(AValue.FValue);');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolkitList.AddHelperCross(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassParent1) + ' = record helper for ' + GetQuantityType(AItem.FClassParent1));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append(Format('    function Cross(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Cross(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Cross(AValue: %s): %s;', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Cross(AValue.FValue);');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolkitList.AddHelperWedge(const AItem: TToolkitItem);
var
  Index: longint;
begin
  Index := SectionA8.IndexOf('  ' + GetUnitTypeHelper(AItem.FClassParent1) + ' = record helper for ' + GetQuantityType(AItem.FClassParent1));
  if Index = -1 then
  begin
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent1)]));
    SectionA8.Append(Format('    function Wedge(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function Wedge(AValue: %s): %s;', [GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.Wedge(AValue: %s): %s;', [GetUnitTypeHelper(AItem.FClassParent1), GetQuantityType(AItem.FClassParent2), GetQuantityType(AItem.FClassName)]));
  SectionB8.Append('begin');
  SectionB8.Append('  result.FValue := FValue.Wedge(AValue.FValue);');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolkitList.AddEquivalence(AClassName, ABaseClass: string);
var
  i, iL, iR: longint;
  S: string;
begin
  iL := Find(Format(INTF_QUANTITY, [GetQuantityType(AClassName), adiminc]), SectionA2);
  iR := Find(Format(INTF_QUANTITY, [GetQuantityType(ABaseClass), adiminc]), SectionA2);

  S := '';
  i := Max(iL, iR);
  if i = iL then S := AClassName;
  if i = iR then S := ABaseClass;

  SectionA2 .Insert(i + 1, '  class operator ' +                       ':=(const AQuantity: ' + GetQuantityType(AClassName) + '): ' + GetQuantityType(ABaseClass) + ';');
  SectionB21.Append(         'class operator ' + GetQuantityType(S) + '.:=(const AQuantity: ' + GetQuantityType(AClassName) + '): ' + GetQuantityType(ABaseClass) + ';');

  SectionB21.Append('begin');
  if GetQuantityType(AClassName) = 'double' then
    SectionB21.Append('  result.FValue := AQuantity;')
  else
    if GetQuantityType(ABaseClass) = 'double' then
      SectionB21.Append('  result := AQuantity.FValue;')
    else
      SectionB21.Append('  result.FValue := AQuantity.FValue;');
  SectionB21.Append('end;');
  SectionB21.Append('');
  Inc(InternalOperators);
end;

procedure TToolkitList.AddItemResource(const AItem: TToolkitItem);
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
  SectionA4.Append(Format('  rs%sSymbol     = ''%s'';', [GetUnitID(AItem.FClassName), GetSymbol   (AItem.FShortSymbol  )]));
  SectionA4.Append(Format('  rs%sName       = ''%s'';', [GetUnitID(AItem.FClassName), GetSingularName(AItem.FLongSymbol)]));
  SectionA4.Append(Format('  rs%sPluralName = ''%s'';', [GetUnitID(AItem.FClassName), GetPluralName  (AItem.FLongSymbol)]));
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

procedure TToolkitList.AddVECItemResource(const AItem: TToolkitItem);
begin

  if AItem.FBaseClass = '' then
  begin

    SectionA4.Append('');
    SectionA4.Append('type');
    SectionA4.Append(Format('  %s = %s;', [GetQuantity(AItem.FClassName), GetQuantityType(AItem.FClassName)]));
    SectionA4.Append('');



  end;




end;

procedure TToolkitList.AddFactoredQuantities(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
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

procedure TToolkitList.CreateSolution(var ANeighbour: TSolution);
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

function TToolkitList.CalculateEnergy(const ASolution: TSolution): double;
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

  Inc(TestingCount);
  FOnMessage(' -> ' + TestingCount.ToString + ' : ' + Result.ToString);
end;

procedure TToolkitList.Run;
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
    for i := Low(FList) to High(FList) do
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

procedure TToolkitList.Run(ASolution: TSolution);
var
  I, J: longint;
  Stream: TResourceStream;
begin
  CheckList := nil;
  ClassList.Clear;
  CommUnits.Clear;
  FDocument.Clear;
  FMessages.Clear;

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
      for J := Low(FList) to High(FList) do
        if FList[J].FClassName = ASolution[I] then
        begin
          AddItem(FList[J], FALSE);
          Break;
        end;

    for J := Low(FList) to High(FList) do AddItem(FList[J], TRUE);
  end else
  begin
    for J := Low(FList) to High(FList) do AddItem(FList[J], FALSE);
    for J := Low(FList) to High(FList) do AddItem(FList[J], TRUE);
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

  CheckList := nil;
end;

procedure TToolkitList.CheckClass(AClassName, AOperator, AClassParent1, AClassParent2: string);
var
  I, Index, Index1, Index2: longint;
  T: TToolKitExponent;
begin
  Index := GetIndex(AClassName);
  if Index = -1 then
  begin

    if (GetUnitType(AClassName) = 'TKilogramUnit') or
       (GetUnitType(AClassName) = 'TMeterUnit'   ) or
       (GetUnitType(AClassName) = 'TSecondUnit'  ) or
       (GetUnitType(AClassName) = 'TKelvinUnit'  ) or
       (GetUnitType(AClassName) = 'TAmpereUnit'  ) or
       (GetUnitType(AClassName) = 'TMoleUnit'    ) or
       (GetUnitType(AClassName) = 'TCandelaUnit' ) then
    begin
      Index := Length(CheckList);
      SetLength(CheckList, Index + 1);

      CheckList[Index].FClassName := GetUnitType(AClassName);
      for I := Low(CheckList[Index].FExponents) to High(CheckList[Index].FExponents) do
        CheckList[Index].FExponents[I] := 0;

      if (GetUnitType(AClassName) = 'TKilogramUnit') then CheckList[Index].FExponents[1] := 1;
      if (GetUnitType(AClassName) = 'TMeterUnit'   ) then CheckList[Index].FExponents[2] := 1;
      if (GetUnitType(AClassName) = 'TSecondUnit'  ) then CheckList[Index].FExponents[3] := 1;
      if (GetUnitType(AClassName) = 'TKelvinUnit'  ) then CheckList[Index].FExponents[4] := 1;
      if (GetUnitType(AClassName) = 'TAmpereUnit'  ) then CheckList[Index].FExponents[5] := 1;
      if (GetUnitType(AClassName) = 'TMoleUnit'    ) then CheckList[Index].FExponents[6] := 1;
      if (GetUnitType(AClassName) = 'TCandelaUnit' ) then CheckList[Index].FExponents[7] := 1;

    end else
    begin

      Index1 := GetIndex(AClassParent1);
      Index2 := GetIndex(AClassParent2);
      if (Index1 = -1) and (Index2 = -1) then
      begin
        if (GetUnitType(AClassName) <> 'TRadianUnit'   ) and
           (GetUnitType(AClassName) <> 'TSteradianUnit') then
          FMessages.Append('ERROR:3 ');
        Exit;
      end;

      T.FClassName := GetUnitType(AClassName);
      for I := Low(T.FExponents) to High(T.FExponents) do
      begin
        T.FExponents[I] := 0;
        if Index1 <> -1 then
          T.FExponents[I] := CheckList[Index1].FExponents[I];

        if Index2 <> -1 then
        begin
          if AOperator = '*' then
            T.FExponents[I] := T.FExponents[I] + CheckList[Index2].FExponents[I]
          else
            if AOperator = '/' then
              T.FExponents[I] := T.FExponents[I] - CheckList[Index2].FExponents[I];
        end;
      end;

      for I := Low(CheckList) to High(CheckList) do
      begin
        if (CheckList[I].FExponents[1] = T.FExponents[1]) and
           (CheckList[I].FExponents[2] = T.FExponents[2]) and
           (CheckList[I].FExponents[3] = T.FExponents[3]) and
           (CheckList[I].FExponents[4] = T.FExponents[4]) and
           (CheckList[I].FExponents[5] = T.FExponents[5]) and
           (CheckList[I].FExponents[6] = T.FExponents[6]) and
           (CheckList[I].FExponents[7] = T.FExponents[7]) then
        begin
          FMessages.Append('WARNING: ' + CheckList[I].FClassName + ' is equal to ' + GetUnitType(AClassName) + ' ' + GetSIUnit(I) + ';');
        end;
      end;

      Index := Length(CheckList);
      SetLength(CheckList, Index + 1);

      CheckList[Index].FClassName := T.FClassName;
      CheckList[Index].FExponents[1] := T.FExponents[1];
      CheckList[Index].FExponents[2] := T.FExponents[2];
      CheckList[Index].FExponents[3] := T.FExponents[3];
      CheckList[Index].FExponents[4] := T.FExponents[4];
      CheckList[Index].FExponents[5] := T.FExponents[5];
      CheckList[Index].FExponents[6] := T.FExponents[6];
      CheckList[Index].FExponents[7] := T.FExponents[7];
    end;

  end else
  begin

    T := CheckList[Index];
    for I := Low(CheckList) to High(CheckList) do
    begin
      if (CheckList[I].FClassName = T.FClassName) then
      begin
        if (CheckList[I].FExponents[1] <> T.FExponents[1]) or
           (CheckList[I].FExponents[2] <> T.FExponents[2]) or
           (CheckList[I].FExponents[3] <> T.FExponents[3]) or
           (CheckList[I].FExponents[4] <> T.FExponents[4]) or
           (CheckList[I].FExponents[5] <> T.FExponents[5]) or
           (CheckList[I].FExponents[6] <> T.FExponents[6]) or
           (CheckList[I].FExponents[7] <> T.FExponents[7]) then
        begin
          FMessages.Append('ERROR:   ' + CheckList[I].FClassName + ' doesn''t match previous declaration ' + GetSIUnit(I) + ';');
        end;
      end;
    end;

  end;
end;

function TToolkitList.GetIndex(const AClassName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := Low(CheckList) to High(CheckList) do
    if GetUnitType(AClassName) = CheckList[I].FClassName then Exit(I);
end;

function TToolkitList.GetSIunit(Index: longint): string;
var
  I: longint;
begin
  Result := '';
  for I := Low(CheckList[Index].FExponents) to High(CheckList[Index].FExponents) do
    if CheckList[Index].FExponents[I] > 0 then
    begin
      case I of
        1: Result := Result + '.kg';
        2: Result := Result + '.m';
        3: Result := Result + '.s';
        4: Result := Result + '.K';
        5: Result := Result + '.A';
        6: Result := Result + '.mol';
        7: Result := Result + '.cd';
      end;
      if CheckList[Index].FExponents[I] > 1 then
        Result := Result + IntToStr(CheckList[Index].FExponents[I]);
    end;

  for I := Low(CheckList[Index].FExponents) to High(CheckList[Index].FExponents) do
    if CheckList[Index].FExponents[I] < 0 then
    begin
      case I of
        1: Result := Result + '/kg';
        2: Result := Result + '/m';
        3: Result := Result + '/s';
        4: Result := Result + '/K';
        5: Result := Result + '/A';
        6: Result := Result + '/mol';
        7: Result := Result + '/cd';
      end;
      if CheckList[Index].FExponents[I] < -1 then
        Result := Result + IntToStr(Abs(CheckList[Index].FExponents[I]));
    end;

  if Pos('.', Result) = 1 then
  begin
    Delete(Result, 1, 1);
  end;

  if Pos('/', Result) = 1 then
  begin
    Result := '1' + Result;
  end;
  Result := '[ ' + Result + ' ]';
end;

function TToolkitList.Find(const S: string; List: TStringList): longint;
var
  i: longint;
begin
  for i := 0 to List.Count -1 do
  begin
    if S = List[i] then Exit(i);
  end;
  Result := -1;
end;

end.

