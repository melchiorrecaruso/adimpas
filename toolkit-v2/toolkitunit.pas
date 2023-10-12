{
  Description: Common unit.

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  end;

  TToolKitExponent = record
    FExponents: array [1..7] of longint;
    FClassName: string;
  end;

  TToolKitList = class(TSimulatedAnnealing)
  private
    CheckList: array of TToolKitExponent;
    ClassList:  TStringList;

    BaseUnitCount:     longint;
    FactoredUnitCount: longint;
    ExternalOperators: longint;
    ForcedOperators:   longint;
    InternalOperators: longint;

    FDocument:  TStringList;
    FMessages:  TStringList;
    SectionA0:  TStringList;
    SectionA1:  TStringList;
    SectionA2:  TStringList;
    SectionA3:  TStringList;
    SectionA4:  TStringList;
    SectionA5:  TStringList;
    SectionA6:  TStringList;
    SectionA7:  TStringList;
    SectionA8:  TStringList;
    SectionA9:  TStringList;
    SectionA10: TStringList;

    SectionB1:  TStringList;
    SectionB2:  TStringList;
    SectionB3:  TStringList;
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

    procedure AddQuantityOperator(AOperator, ALeftClass, ARightClass, AResultClass: string);
    procedure AddUnitIdOperator(AOperator, ALeftClass, ARightClass, AResultClass: string);
    procedure AddClass(const AItem: TToolkitItem; AddOperator: boolean);
    procedure AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);
    procedure AddEquivalence(AClassName, ABaseClass: string);

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

// TToolkitList

constructor TToolkitList.Create(OnMessage: TMessageEvent);
begin
  inherited Create(OnMessage);
  FOnMessage := OnMessage;
  FList      := nil;
  ClassList  := TStringList.Create;
  FDocument  := TStringList.Create;
  FMessages  := TStringList.Create;

  ClassList .Sorted := TRUE;
  Randomize;
end;

destructor TToolkitList.Destroy;
begin
  FList := nil;
  ClassList.Destroy;
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

procedure TToolkitList.AddQuantityOperator(AOperator, ALeftClass, ARightClass, AResultClass: string);
var
  i, iL, iR, iX: longint;
  ABaseClass, S: string;
begin
  iL := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(ALeftClass  )]), SectionA2);
  iR := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(ARightClass )]), SectionA2);
  iX := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(AResultClass)]), SectionA2);

  ABaseClass := '';
  i := Max(iL, iR);
  if i = iL then ABaseClass := ALeftClass;
  if i = iR then ABaseClass := ARightClass;
  if i < iX then ABaseClass := '';

  if ABaseClass <> '' then
  begin
    if (i = iL) and (GetUnitQuantityType(ALeftClass ) = 'THertzQty') then ABaseClass := '';
    if (i = iR) and (GetUnitQuantityType(ARightClass) = 'THertzQty') then ABaseClass := '';
    if ABaseClass = '' then Inc(ForcedOperators);
  end;

  if ABaseClass   <> 'double' then ABaseClass   := GetUnitQuantityType(ABaseClass);
  if ALeftClass   <> 'double' then ALeftClass   := GetUnitQuantityType(ALeftClass);
  if ARightClass  <> 'double' then ARightClass  := GetUnitQuantityType(ARightClass);
  if AResultClass <> 'double' then AResultClass := GetUnitQuantityType(AResultClass);

  if ClassList.IndexOf(ABaseClass + '.' + ALeftClass + AOperator + ARightClass) = -1 then
  begin
    ClassList.Append(ABaseClass + '.' + ALeftClass + AOperator + ARightClass);

    if ABaseClass = '' then
    begin
      SectionA4.Append(Format(INTF_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB2.Append(Format(IMPL_OP, [AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(ExternalOperators);
    end else
    begin
      SectionA2.Insert(i + 1, Format(INTF_OP_CLASS, [            AOperator, ALeftClass, ARightClass, AResultClass]));
      SectionB2.Append(       Format(IMPL_OP_CLASS, [ABaseClass, AOperator, ALeftClass, ARightClass, AResultClass]));
      Inc(InternalOperators);
    end;

    if AResultClass = 'double' then
      S := '  result :='
    else
      S := '  result.FValue :=';

    if ALeftClass = 'double' then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.FValue ' + AOperator;

    if ARightClass = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.FValue;';

    SectionB2.Append('begin');
    SectionB2.Append(S);
    SectionB2.Append('end;');
    SectionB2.Append('');
  end else
    FMessages.Append('ERROR: operator ' + AOperator + '(' + ALeftClass + '; ' + ARightClass + ') : ' + AResultClass + '; already esists.');
end;

procedure TToolkitList.AddUnitIdOperator(AOperator, ALeftClass, ARightClass, AResultClass: string);
var
  i: longint;
  ABaseClass: string;
begin
  i := Find(Format(INTF_UNIT,[GetUnitQuantityType(ARightClass), GetUnitIdentifier(ARightClass)]), SectionA3);

  ABaseClass := ARightClass;
  if ALeftClass   <> 'double' then ALeftClass   := GetUnitQuantityType(ALeftClass);
  if ARightClass  <> 'double' then ARightClass  := GetUnitIdentifier  (ARightClass);
  if AResultClass <> 'double' then AResultClass := GetUnitQuantityType(AResultClass);

  if ClassList.IndexOf(ALeftClass + AOperator + ARightClass) = -1 then
  begin
    ClassList.Append(ALeftClass + AOperator + ARightClass);

    SectionA3.Insert(i + 1, Format(INTF_OP_CLASS, [                               AOperator, ALeftClass, ARightClass, AResultClass]));
    SectionB3.Append('');
    SectionB3.Append(       Format(IMPL_OP_CLASS, [GetUnitIdentifier(ABaseClass), AOperator, ALeftClass, ARightClass, AResultClass]));

    SectionB3.Append('begin');
    if AResultClass <> 'double' then
    begin
      if ALeftClass <> 'double' then
        SectionB3.Append('  result.FValue := ALeft.FValue;')
      else
        SectionB3.Append('  result.FValue := ALeft;');
    end else
    begin
      if ALeftClass <> 'double' then
        SectionB3.Append('  result := ALeft.FValue;')
      else
        SectionB3.Append('  result := ALeft;');
    end;
    SectionB3.Append('end;');
    SectionB3.Append('');
    Inc(InternalOperators);
  end;
end;

procedure TToolkitList.AddClass(const AItem: TToolkitItem; AddOperator: boolean);
begin
  if ClassList.IndexOf(GetUnitQuantityType(AItem.FClassName)) = -1 then
  begin
    ClassList.Append(GetUnitQuantityType(AItem.FClassName));

    if (AItem.FBaseClass = '') then
    begin
      // BASE UNIT
      if (AItem.FOperator = '*') then
      begin
        SectionA2.Insert(0, '');
        SectionA2.Insert(1, '{ Quantity of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA2.Insert(2, Format(INTF_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
        SectionA2.Insert(3, Format(INTF_END, []));
        SectionA2.Insert(4, '');

        SectionA3.Append('');
        SectionA3.Append('{ Unit of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA3.Append(Format(INTF_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));
        SectionA3.Append(Format(INTF_END, []));
      end else
      begin
        SectionA2.Append('');
        SectionA2.Append('{ Quantity of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA2.Append(Format(INTF_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
        SectionA2.Append(Format(INTF_END, []));
        SectionA2.Append('');

        SectionA3.Append('');
        SectionA3.Append('{ Unit of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA3.Append(Format(INTF_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));
        SectionA3.Append(Format(INTF_END, []));
      end;
      SectionA3.Append('');
      SectionA3.Append('type');
      SectionA3.Append(Format('  %s = %s;', [GetUnitQuantity(AItem.FClassName), GetUnitQuantityType(AItem.FClassName)]));

      SectionB2.Append(Format(IMPL_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
      SectionB3.Append(Format(IMPL_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));

      SectionB2.Append(Format('class function %s.Symbol: string; begin result := ''%s'' end;',       [GetUnitQuantityType(AItem.FClassName), GetSymbol      (AItem.FShortSymbol)]));
      SectionB2.Append(Format('class function %s.SingularName: string; begin result := ''%s'' end;', [GetUnitQuantityType(AItem.FClassName), GetSingularName(AItem.FLongSymbol )]));
      SectionB2.Append(Format('class function %s.PluralName: string; begin result := ''%s'' end;',   [GetUnitQuantityType(AItem.FClassName), GetPluralName  (AItem.FLongSymbol )]));
      SectionB2.Append(Format('class function %s.Prefixes: TPrefixes; begin result := [%s]; end;',   [GetUnitQuantityType(AItem.FClassName), GetPrefixes    (AItem.FShortSymbol)]));
      SectionB2.Append(Format('class function %s.Exponents: TExponents; begin result := [%s]; end;', [GetUnitQuantityType(AItem.FClassName), GetExponents   (AItem.FShortSymbol)]));
      SectionB2.Append('');

      if (AItem.FIdentifierSymbol <> '') then
      begin
        SectionA3.Append('');
        SectionA3.Append('var');
        SectionA3.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FClassName)]));
        AddFactoredQuantity(AItem.FClassName, AItem.FIdentifierSymbol, '', AItem.FPrefixes);
      end;

      Inc(BaseUnitCount);
    end else
    begin
      // CLONED UNIT
      if AItem.FFactor = '' then
      begin
        SectionA2.Append('');
        SectionA2.Append('{ Quantity of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA2.Append(Format(INTF_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
        SectionA2.Append(Format(INTF_END, []));
        SectionA2.Append('');

        SectionA3.Append('');
        SectionA3.Append('{ Unit of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA3.Append(Format(INTF_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));
        SectionA3.Append(Format(INTF_END, []));

        SectionA3.Append('');
        SectionA3.Append('type');
        SectionA3.Append(Format('  %s = %s;', [GetUnitQuantity(AItem.FClassName), GetUnitQuantityType(AItem.FBaseClass)]));

        SectionB2.Append(Format(IMPL_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
        SectionB3.Append(Format(IMPL_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));

        SectionB2.Append(Format('class function %s.Symbol: string; begin result := ''%s'' end;',       [GetUnitQuantityType(AItem.FClassName), GetSymbol(AItem.FShortSymbol)]));
        SectionB2.Append(Format('class function %s.SingularName: string; begin result := ''%s'' end;', [GetUnitQuantityType(AItem.FClassName), GetSingularName(AItem.FLongSymbol)]));
        SectionB2.Append(Format('class function %s.PluralName: string; begin result := ''%s'' end;',   [GetUnitQuantityType(AItem.FClassName), GetPluralName(AItem.FLongSymbol)]));
        SectionB2.Append(Format('class function %s.Prefixes: TPrefixes; begin result := [%s]; end;',   [GetUnitQuantityType(AItem.FClassName), GetPrefixes(AItem.FShortSymbol)]));
        SectionB2.Append(Format('class function %s.Exponents: TExponents; begin result := [%s]; end;', [GetUnitQuantityType(AItem.FClassName), GetExponents(AItem.FShortSymbol)]));
        SectionB2.Append('');

        if (AItem.FIdentifierSymbol <> '') then
        begin
          SectionA3.Append('');
          SectionA3.Append('var');
          SectionA3.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FBaseClass)]));
          AddFactoredQuantity(AItem.FBaseClass, AItem.FIdentifierSymbol, AItem.FFactor, AItem.FPrefixes);
        end;
        AddHelper(AItem.FClassName, AItem.FBaseClass, '');
        AddHelper(AItem.FBaseClass, AItem.FClassName, '');

        Inc(FactoredUnitCount);
      end else
      begin
        // FACTORED UNIT
        SectionA2.Append('');
        SectionA2.Append('{ Quantity of ' + GetUnitComment(AItem.FClassName) + ' }');
        if Pos('%s', AItem.FFactor) = 0 then
          SectionA2.Append(Format(INTF_FACTORED, [GetUnitQuantityType(AItem.FClassName)]))
        else
          SectionA2.Append(Format(INTF_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));
        SectionA2.Append(Format(INTF_END, []));
        SectionA2.Append('');

        SectionA3.Append('');
        SectionA3.Append('{ Unit of ' + GetUnitComment(AItem.FClassName) + ' }');
        SectionA3.Append(Format(INTF_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));
        SectionA3.Append(Format(INTF_END, []));

        SectionA3.Append('');
        SectionA3.Append('type');
        SectionA3.Append(Format('  %s = %s;', [GetUnitQuantity(AItem.FClassName), GetUnitQuantityType(AItem.FBaseClass)]));

        if Pos('%s', AItem.FFactor) = 0 then
          SectionB2.Append(Format(IMPL_FACTORED, [GetUnitQuantityType(AItem.FClassName)]))
        else
          SectionB2.Append(Format(IMPL_QUANTITY, [GetUnitQuantityType(AItem.FClassName)]));

        SectionB3.Append(Format(IMPL_UNIT, [GetUnitQuantityType(AItem.FClassName), GetUnitIdentifier(AItem.FClassName)]));

        SectionB2.Append(Format('class function %s.Symbol: string; begin result := ''%s'' end;',       [GetUnitQuantityType(AItem.FClassName), GetSymbol(AItem.FShortSymbol)]));
        SectionB2.Append(Format('class function %s.SingularName: string; begin result := ''%s'' end;', [GetUnitQuantityType(AItem.FClassName), GetSingularName(AItem.FLongSymbol)]));
        SectionB2.Append(Format('class function %s.PluralName: string; begin result := ''%s'' end;',   [GetUnitQuantityType(AItem.FClassName), GetPluralName(AItem.FLongSymbol)]));
        SectionB2.Append(Format('class function %s.Prefixes: TPrefixes; begin result := [%s]; end;',   [GetUnitQuantityType(AItem.FClassName), GetPrefixes(AItem.FShortSymbol)]));
        SectionB2.Append(Format('class function %s.Exponents: TExponents; begin result := [%s]; end;', [GetUnitQuantityType(AItem.FClassName), GetExponents(AItem.FShortSymbol)]));
        if Pos('%s', AItem.FFactor) = 0 then
        begin
          SectionB2.Append(Format('class function %s.ToBaseFactor: double; begin result := %s; end;', [GetUnitQuantityType(AItem.FClassName), AItem.FFactor]));
        end;
        SectionB2.Append('');

        if (AItem.FIdentifierSymbol <> '') then
        begin
          if Pos('%s', AItem.FFactor) = 0 then
          begin
            SectionA3.Append('');
            SectionA3.Append('const');
            SectionA3.Append(Format('  %s: %s = (FValue: %s);', [AItem.FIdentifierSymbol, GetUnitQuantityType(AItem.FBaseClass), AItem.FFactor]));
            AddFactoredQuantity(AItem.FBaseClass, AItem.FIdentifierSymbol, AItem.FFactor, AItem.FPrefixes);
          end else
          begin
            SectionA3.Append('');
            SectionA3.Append('var');
            SectionA3.Append(Format('  %s: %s;', [AItem.FIdentifierSymbol, GetUnitIdentifier(AItem.FClassName)]));
          end;
        end;

        if Pos('%s', AItem.FFactor) = 0 then
        begin
          AddHelper(AItem.FClassName, AItem.FBaseClass, 'FValue / ' + GetUnitQuantityType(AItem.FClassName) + '.ToBaseFactor');
        end else
        begin
          AddHelper(AItem.FBaseClass, AItem.FClassName, Format(Copy(AItem.FFactor, 1, Pos('|', AItem.FFactor) -1), ['FValue']));
          AddHelper(AItem.FClassName, AItem.FBaseClass, Format(Copy(AItem.FFactor, Pos('|', AItem.FFactor) + 1, Length(AItem.FFactor)), ['FValue']));
        end;

        Inc(FactoredUnitCount);
      end;

    end;
  end;
  if not AddOperator then Exit;

  if (AItem.FBaseClass = '') then
  begin
    CheckClass(AItem.FClassName, AItem.FOperator, AItem.FClassParent1, AItem.FClassParent2);

    if AItem.FOperator = '*' then
    begin
      AddQuantityOperator('*', AItem.FClassParent1, AItem.FClassParent2, AItem.FClassName);
      if AItem.FClassParent1 <> AItem.FClassParent2 then
      begin
        AddQuantityOperator('*', AItem.FClassParent2, AItem.FClassParent1, AItem.FClassName);
      end;

      AddQuantityOperator('/', AItem.FClassName, AItem.FClassParent1, AItem.FClassParent2);
      if AItem.FClassParent1 <> AItem.FClassParent2 then
      begin
        AddQuantityOperator('/', AItem.FClassName, AItem.FClassParent2, AItem.FClassParent1);
      end;

      if Pos('OP1', AItem.FFactor) > 0 then AddUnitIdOperator('*', AItem.FClassParent1, AItem.FClassParent2, AItem.FClassName);
      if Pos('OP2', AItem.FFactor) > 0 then AddUnitIdOperator('*', AItem.FClassParent2, AItem.FClassParent1, AItem.FClassName);
      if Pos('OP3', AItem.FFactor) > 0 then AddUnitIdOperator('/', AItem.FClassName, AItem.FClassParent1, AItem.FClassParent2);
      if Pos('OP4', AItem.FFactor) > 0 then AddUnitIdOperator('/', AItem.FClassName, AItem.FClassParent2, AItem.FClassParent1);

    end else
      if AItem.FOperator = '/' then
      begin
        AddQuantityOperator('/', AItem.FClassParent1, AItem.FClassParent2, AItem.FClassName);
        AddQuantityOperator('*', AItem.FClassParent2, AItem.FClassName,    AItem.FClassParent1);
        AddQuantityOperator('*', AItem.FClassName,    AItem.FClassParent2, AItem.FClassParent1);
        AddQuantityOperator('/', AItem.FClassParent1, AItem.FClassName,    AItem.FClassParent2);

        if Pos('OP1', AItem.FFactor) > 0 then AddUnitIdOperator('/', AItem.FClassParent1, AItem.FClassParent2, AItem.FClassName);
        if Pos('OP2', AItem.FFactor) > 0 then AddUnitIdOperator('*', AItem.FClassParent2, AItem.FClassName, AItem.FClassParent1);
        if Pos('OP3', AItem.FFactor) > 0 then AddUnitIdOperator('*', AItem.FClassName, AItem.FClassParent2, AItem.FClassParent1);
        if Pos('OP4', AItem.FFactor) > 0 then AddUnitIdOperator('/', AItem.FClassParent1, AItem.FClassName, AItem.FClassParent2);

      end else
        if Pos('power', LowerCase(AItem.FOperator)) > 0 then
        begin
          AddPower(AItem.FOperator, AItem.FClassParent1, AItem.FClassName);
        end;

  end else
    if (AItem.FOperator = '=') then
    begin
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
      if (AItem.FOperator = ':=') then
      begin
        SectionA7.Append('');
        SectionB7.Append('');
        AddEquivalence(AItem.FClassName, AItem.FBaseClass);
        AddHelper(AItem.FClassName, AItem.FBaseClass, '');
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

procedure TToolkitList.AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
var
  Params: string;
  Power: longint;
  Str: string;
begin
  Str := '  %s: %s = (FValue: %s);';
  if AFactor <> '' then
    AFactor := AFactor + ' * ';

  if Length(APrefixes) = 24 then
  begin
    Params := APrefixes;
    SectionA3.Append('');
    SectionA3.Append('const');
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

  if (LowerCase(AIdentifierSymbol) <> 'kg' ) and
     (LowerCase(AIdentifierSymbol) <> 'kg2') then
  begin
    if Params[ 1] = 'L' then SectionA3.Append(Format(Str, ['quetta' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 1] = 'S' then SectionA3.Append(Format(Str, ['Q'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 2] = 'L' then SectionA3.Append(Format(Str, ['ronna'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 2] = 'S' then SectionA3.Append(Format(Str, ['R'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 3] = 'L' then SectionA3.Append(Format(Str, ['yotta'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 3] = 'S' then SectionA3.Append(Format(Str, ['Y'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 4] = 'L' then SectionA3.Append(Format(Str, ['zetta'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 4] = 'S' then SectionA3.Append(Format(Str, ['Z'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 5] = 'L' then SectionA3.Append(Format(Str, ['exa'    + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 5] = 'S' then SectionA3.Append(Format(Str, ['E'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 6] = 'L' then SectionA3.Append(Format(Str, ['peta'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));
    if Params[ 6] = 'S' then SectionA3.Append(Format(Str, ['P'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));

    if Params[ 7] = 'L' then SectionA3.Append(Format(Str, ['tera'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 7] = 'S' then SectionA3.Append(Format(Str, ['T'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 8] = 'L' then SectionA3.Append(Format(Str, ['giga'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 8] = 'S' then SectionA3.Append(Format(Str, ['G'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 9] = 'L' then SectionA3.Append(Format(Str, ['mega'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[ 9] = 'S' then SectionA3.Append(Format(Str, ['M'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[10] = 'L' then SectionA3.Append(Format(Str, ['kilo'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[10] = 'S' then SectionA3.Append(Format(Str, ['k'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[11] = 'L' then SectionA3.Append(Format(Str, ['hecto'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[11] = 'S' then SectionA3.Append(Format(Str, ['h'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[12] = 'L' then SectionA3.Append(Format(Str, ['deca'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[12] = 'S' then SectionA3.Append(Format(Str, ['da'     + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[13] = 'L' then SectionA3.Append(Format(Str, ['deci'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[13] = 'S' then SectionA3.Append(Format(Str, ['d'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[14] = 'L' then SectionA3.Append(Format(Str, ['centi'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[14] = 'S' then SectionA3.Append(Format(Str, ['c'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[15] = 'L' then SectionA3.Append(Format(Str, ['milli'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[15] = 'S' then SectionA3.Append(Format(Str, ['m'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[16] = 'L' then SectionA3.Append(Format(Str, ['micro'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[16] = 'S' then SectionA3.Append(Format(Str, ['mi'     + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[17] = 'L' then SectionA3.Append(Format(Str, ['nano'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[17] = 'S' then SectionA3.Append(Format(Str, ['n'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[18] = 'L' then SectionA3.Append(Format(Str, ['pico'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));
    if Params[18] = 'S' then SectionA3.Append(Format(Str, ['p'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));

    if Params[19] = 'L' then SectionA3.Append(Format(Str, ['femto'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[19] = 'S' then SectionA3.Append(Format(Str, ['f'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[20] = 'L' then SectionA3.Append(Format(Str, ['atto'   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[20] = 'S' then SectionA3.Append(Format(Str, ['a'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[21] = 'L' then SectionA3.Append(Format(Str, ['zepto'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[21] = 'S' then SectionA3.Append(Format(Str, ['z'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[22] = 'L' then SectionA3.Append(Format(Str, ['yocto'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[22] = 'S' then SectionA3.Append(Format(Str, ['y'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[23] = 'L' then SectionA3.Append(Format(Str, ['ronto'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[23] = 'S' then SectionA3.Append(Format(Str, ['r'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[24] = 'L' then SectionA3.Append(Format(Str, ['quecto' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
    if Params[24] = 'S' then SectionA3.Append(Format(Str, ['q'      + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
  end else
    if (LowerCase(AIdentifierSymbol) = 'kg') then
    begin
      AIdentifierSymbol := 'g';
      SectionA3.Append(Format(Str, ['h'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-01']));
      SectionA3.Append(Format(Str, ['da' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-02']));
      SectionA3.Append(Format(Str, [''   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-03']));
      SectionA3.Append(Format(Str, ['d'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-04']));
      SectionA3.Append(Format(Str, ['c'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-05']));
      SectionA3.Append(Format(Str, ['m'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-06']));
      SectionA3.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-09']));
      SectionA3.Append(Format(Str, ['n'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-12']));
      SectionA3.Append(Format(Str, ['p'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-15']));
    end else
      if (LowerCase(AIdentifierSymbol) = 'kg2') then
      begin
         AIdentifierSymbol := 'g2';
        SectionA3.Append(Format(Str, ['h'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-02']));
        SectionA3.Append(Format(Str, ['da' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-04']));
        SectionA3.Append(Format(Str, [''   + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-06']));
        SectionA3.Append(Format(Str, ['d'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-08']));
        SectionA3.Append(Format(Str, ['c'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-10']));
        SectionA3.Append(Format(Str, ['m'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-12']));
        SectionA3.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-18']));
        SectionA3.Append(Format(Str, ['n'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-24']));
        SectionA3.Append(Format(Str, ['p'  + AIdentifierSymbol, GetUnitQuantityType(ABaseClass), '1E-30']));
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

  AQuantity := GetUnitQuantityType(AQuantity);
  AResult   := GetUnitQuantityType(AResult);

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
  Index := SectionA8.IndexOf('  ' + GetUnitClassNameHelper(ABaseClass) + ' = record helper for ' + GetUnitQuantityType(ABaseClass));
  if Index = -1 then
  begin
    SectionA8.Append('');
    SectionA8.Append('type');
    SectionA8.Append(Format('  %s = record helper for %s', [GetUnitClassNameHelper(ABaseClass), GetUnitQuantityType(ABaseClass)]));
    SectionA8.Append(Format('    function To%s: %s;', [GetUnitDescription(AClassName), GetUnitQuantityType(AClassName)]));
    SectionA8.Append('  end;');
    SectionA8.Append('');
  end else
  begin
    SectionA8.Insert(Index + 1, Format('    function To%s: %s;', [GetUnitDescription(AClassName), GetUnitQuantityType(AClassName)]));
  end;

  SectionB8.Append('');
  SectionB8.Append(Format('function %s.To%s: %s;', [GetUnitClassNameHelper(ABaseClass), GetUnitDescription(AClassName), GetUnitQuantityType(AClassName)]));

  SectionB8.Append('begin');
  if AFactor = '' then
    SectionB8.Append('  result.FValue := FValue;')
  else
    SectionB8.Append('  result.FValue := ' + AFactor + ';');
  SectionB8.Append('end;');
  SectionB8.Append('');
end;

procedure TToolkitList.AddEquivalence(AClassName, ABaseClass: string);
var
  i, iL, iR: longint;
  S: string;
begin
  iL := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(AClassName)]), SectionA2);
  iR := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(ABaseClass)]), SectionA2);

  S := '';
  i := Max(iL, iR);
  if i = iL then S := AClassName;
  if i = iR then S := ABaseClass;

  SectionA2.Insert(i + 1, '  class operator ' +                           ':=(const AQuantity: ' + GetUnitQuantityType(AClassName) + '): ' + GetUnitQuantityType(ABaseClass) + ';');
  SectionB2.Append(         'class operator ' + GetUnitQuantityType(S) + '.:=(const AQuantity: ' + GetUnitQuantityType(AClassName) + '): ' + GetUnitQuantityType(ABaseClass) + ';');

  SectionB2.Append('begin');
  if GetUnitQuantityType(AClassName) = 'double' then
    SectionB2.Append('  result.FValue := AQuantity;')
  else
    if GetUnitQuantityType(ABaseClass) = 'double' then
      SectionB2.Append('  result := AQuantity.FValue;')
    else
      SectionB2.Append('  result.FValue := AQuantity.FValue;');
  SectionB2.Append('end;');
  SectionB2.Append('');
  Inc(InternalOperators);
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
  Run(ASolution);
  Result := MaxDouble;
  FDocument.SaveToFile('adim.pas');
  AProcess := TProcess.Create(nil);
  AProcess.Executable:= 'lazbuild';
  AProcess.Parameters.Add('-B');
  AProcess.Parameters.Add('adimtest.lpi');
  AProcess.Options := AProcess.Options + [poNoConsole, poUsePipes];
  AProcess.Execute;

  AStringList := TStringList.Create;
  AMemoryStream := TMemoryStream.Create;
  while AProcess.Running do
  begin
    if AProcess.Output.NumBytesAvailable > 0 then
    begin
      ReadSize := AProcess.Output.NumBytesAvailable;
      if ReadSize > SizeOf(Buffer) then
        ReadSize := SizeOf(Buffer);
      ReadSize := AProcess.Output.Read(Buffer[0], ReadSize);
      AMemoryStream.Write(Buffer[0], ReadSize);
    end;
  end;
  AMemoryStream.Seek(0, soFromBeginning);
  AStringList.LoadFromStream(AMemoryStream);
  for i := 0 to AStringList.Count -1 do
    if AStringList[i].Contains(' lines compiled,') then
    begin
      AIndex := Pos(', ', AStringList[i]) + 2;
      ACount := Pos(' sec,', AStringList[i]) - AIndex;
      ResultStr := Copy(AStringList[i], AIndex, ACount);

      for j := Low(ResultStr) to High(ResultStr) do
        if ResultStr[j] in ['.', ','] then
          ResultStr[j] := DefaultFormatSettings.DecimalSeparator;
      Result := StrToFloat(ResultStr);
      Break;
    end;
  AMemoryStream.Destroy;
  AStringList.Free;
  AProcess.Free;
//Result := ForcedOperators + ExternalOperators;
end;

procedure TToolkitList.Run;
var
  i, j: longint;
  S: TStringList;
  BestSolution: TSolution;
begin
  BestSolution := nil;
  if FExecutionTime > 0 then
  begin
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
      if Length(BestSolution) > 0 then
        Execute(BestSolution);
    end;
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
//if FExecutionTime > 0 then
  begin
    if Assigned(FOnMessage) then
      FOnMessage('Storing backup ...');
    S := TStringList.Create;
    for i := Low(BestSolution) to High(BestSolution) do
      S.Add(BestSolution[i]);
    S.SaveToFile('solution.bk');
    S.Destroy;
    if Assigned(FOnMessage) then
      FOnMessage('Backup stored.');
  end;
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
  FDocument.Clear;
  FMessages.Clear;

  SectionA0  := TStringList.Create;
  SectionA1  := TStringList.Create;
  SectionA2  := TStringList.Create;
  SectionA3  := TStringList.Create;
  SectionA4  := TStringList.Create;
  SectionA5  := TStringList.Create;
  SectionA6  := TStringList.Create;
  SectionA7  := TStringList.Create;
  SectionA8  := TStringList.Create;
  SectionA9  := TStringList.Create;
  SectionA10 := TStringList.Create;

  SectionB1  := TStringList.Create;
  SectionB2  := TStringList.Create;
  SectionB3  := TStringList.Create;
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
  Stream.Destroy;

  Stream := TResourceStream.Create(HInstance, 'SECTION-B1', RT_RCDATA);
  SectionB1.LoadFromStream(Stream);
  SectionB1.Insert(0, '');
  Stream.Destroy;

  SectionA2.Append('');
  SectionB2.Append('');

  SectionA3.Append('');
  SectionB3.Append('');

  SectionA4.Append('');
  SectionB4.Append('');

  SectionA5.Append('');
  SectionB5.Append('');

  SectionA6.Append('');
  SectionB6.Append('');

  SectionA7.Append('');
  SectionB7.Append('');

  SectionA8.Append('');
  SectionB8.Append('');

  SectionA9.Append('');
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
          AddClass(FList[J], FALSE);
          Break;
        end;

    for J := Low(FList) to High(FList) do AddClass(FList[J], TRUE);
  end else
  begin
    for J := Low(FList) to High(FList) do AddClass(FList[J], FALSE);
    for J := Low(FList) to High(FList) do AddClass(FList[J], TRUE);
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

  for I := 0 to SectionA4 .Count -1 do FDocument.Append(SectionA4 [I]);
  for I := 0 to SectionA3 .Count -1 do FDocument.Append(SectionA3 [I]);

  for I := 0 to SectionA5 .Count -1 do FDocument.Append(SectionA5 [I]);
  for I := 0 to SectionA6 .Count -1 do FDocument.Append(SectionA6 [I]);
  for I := 0 to SectionA7 .Count -1 do FDocument.Append(SectionA7 [I]);
  for I := 0 to SectionA8 .Count -1 do FDocument.Append(SectionA8 [I]);
  for I := 0 to SectionA9 .Count -1 do FDocument.Append(SectionA9 [I]);
  for I := 0 to SectionA10.Count -1 do FDocument.Append(SectionA10[I]);

  for I := 0 to SectionB1 .Count -1 do FDocument.Append(SectionB1 [I]);
  for I := 0 to SectionB2 .Count -1 do FDocument.Append(SectionB2 [I]);
  for I := 0 to SectionB3 .Count -1 do FDocument.Append(SectionB3 [I]);
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
  SectionB3 .Destroy;
  SectionB2 .Destroy;
  SectionB1 .Destroy;

  SectionA10.Destroy;
  SectionA9 .Destroy;
  SectionA8 .Destroy;
  SectionA7 .Destroy;
  SectionA6 .Destroy;
  SectionA5 .Destroy;
  SectionA4 .Destroy;
  SectionA3 .Destroy;
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

    if (GetUnitClassName(AClassName) = 'TKilogramUnit') or
       (GetUnitClassName(AClassName) = 'TMeterUnit'   ) or
       (GetUnitClassName(AClassName) = 'TSecondUnit'  ) or
       (GetUnitClassName(AClassName) = 'TKelvinUnit'  ) or
       (GetUnitClassName(AClassName) = 'TAmpereUnit'  ) or
       (GetUnitClassName(AClassName) = 'TMoleUnit'    ) or
       (GetUnitClassName(AClassName) = 'TCandelaUnit' ) then
    begin
      Index := Length(CheckList);
      SetLength(CheckList, Index + 1);

      CheckList[Index].FClassName := GetUnitClassName(AClassName);
      for I := Low(CheckList[Index].FExponents) to High(CheckList[Index].FExponents) do
        CheckList[Index].FExponents[I] := 0;

      if (GetUnitClassName(AClassName) = 'TKilogramUnit') then CheckList[Index].FExponents[1] := 1;
      if (GetUnitClassName(AClassName) = 'TMeterUnit'   ) then CheckList[Index].FExponents[2] := 1;
      if (GetUnitClassName(AClassName) = 'TSecondUnit'  ) then CheckList[Index].FExponents[3] := 1;
      if (GetUnitClassName(AClassName) = 'TKelvinUnit'  ) then CheckList[Index].FExponents[4] := 1;
      if (GetUnitClassName(AClassName) = 'TAmpereUnit'  ) then CheckList[Index].FExponents[5] := 1;
      if (GetUnitClassName(AClassName) = 'TMoleUnit'    ) then CheckList[Index].FExponents[6] := 1;
      if (GetUnitClassName(AClassName) = 'TCandelaUnit' ) then CheckList[Index].FExponents[7] := 1;

    end else
    begin

      Index1 := GetIndex(AClassParent1);
      Index2 := GetIndex(AClassParent2);
      if (Index1 = -1) and (Index2 = -1) then
      begin
        if (GetUnitClassName(AClassName) <> 'TRadianUnit'   ) and
           (GetUnitClassName(AClassName) <> 'TSteradianUnit') then
          FMessages.Append('ERROR:3 ');
        Exit;
      end;

      T.FClassName := GetUnitClassName(AClassName);
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
          FMessages.Append('WARNING: ' + CheckList[I].FClassName + ' is equal to ' + GetUnitClassName(AClassName) + ' ' + GetSIUnit(I) + ';');
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
    if GetUnitClassName(AClassName) = CheckList[I].FClassName then Exit(I);
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

