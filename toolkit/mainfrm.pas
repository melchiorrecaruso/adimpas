{
  Description: MainForm.

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

unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpspreadsheetgrid, fpspreadsheetctrls, fpsallformats, Forms,
  Controls, Graphics, Dialogs, Grids, Buttons, ComCtrls, StdCtrls, SynHighlighterPas,
  SynEdit;

type
  TDBItem = record
    Name: string;
    Exponents: array [1..7] of longint;
  end;

type
  { TMainForm }

  TMainForm = class(TForm)
    Memo: TMemo;
    TabSheet3: TTabSheet;
    WorkbookSource: TsWorkbookSource;
    WorksheetGrid: TsWorksheetGrid;
    TrigonometricCheckBox: TCheckBox;
    ExportBtn: TBitBtn;
    LoadBtn: TBitBtn;
    RunBtn: TBitBtn;
    PageControl: TPageControl;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
  private
    BaseUnitCount: longint;
    CheckList: array of TDBItem;
    ClassList: TStringList;
    FactoredUnitCount: longint;
    OperatorCount: longint;
    OperatorList: TStringList;

    SectionA0: TStringList;
    SectionA1: TStringList;
    SectionA2: TStringList;
    SectionA3: TStringList;
    SectionA4: TStringList;

    SectionB1: TStringList;
    SectionB2: TStringList;
    SectionB3: TStringList;
    SectionB4: TStringList;

    procedure AddQuantityOperator(AOperator, ALeftParent, ARightParent, AResult: string);
    procedure AddUnitIdOperator(AOperator, ALeftParent, ARightParent, AResult: string);

    procedure AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
      AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor, APrefixes: string);

    procedure AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);
    procedure AddEquivalence(AClassName, ABaseClass: string);

    procedure CheckClass(AClassName, AOperator, AClassParent1, AClassParent2: string);
    function GetIndex(const AClassName: string): longint;
    function GetSIunit(Index: longint): string;
  public

  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, Math, StrUtils;

const
  _class_name        = 00;
  _operator          = 01;
  _class_parent_1    = 02;
  _class_parent_2    = 03;
  _comment           = 04;
  _long_symbol       = 05;
  _short_symbol      = 06;
  _identifier_symbol = 07;
  _base_class        = 08;
  _factor            = 09;
  _prefixes          = 10;


function Split(const AStr: string): TStringArray;
var
  I, Index: longint;
begin
  Index  := 0;
  result := nil;
  SetLength(result, Index + 10);
  for I := Low(AStr) to High(AStr) do
  begin

    if AStr[I] in ['/', '.'] then
    begin
      Inc(Index);
      if Index = Length(result) then
        SetLength(result, Index + 10);
      if AStr[I] <> ' ' then
      begin
        result[Index] := AStr[I];
        Inc(Index);
        if Index = Length(result) then
           SetLength(result, Index + 10);
      end;
      result[Index] := '';
    end else
      result[Index] := result[Index] + AStr[I];
   end;
  SetLength(result, Index + 1);
end;

function GetSymbol(const AShortSymbol: string): string;
begin
  result := AShortSymbol;
  result := StringReplace(result, '.', '·', [rfReplaceAll]);
end;

function GetSingularName(const ALongSymbol: string): string;
begin
  result := ALongSymbol;
  result := StringReplace(StringReplace(result, '!', '', [rfReplaceAll]), '?', '', [rfReplaceAll]);
end;

function GetPluralName(const ALongSymbol: string): string;
begin
  result := ALongSymbol;
  result := StringReplace(StringReplace(result, 'inch!', 'inches', [rfReplaceAll]), 'foot!', 'feet', [rfReplaceAll]);
  result := StringReplace(StringReplace(result, 'y!',    'ies',    [rfReplaceAll]), '?',     's',    [rfReplaceAll]);
end;

function GetUnitComment(S: string): string;
var
  I: longint;
begin
  S := StringReplace(S, '!',  '', [rfReplaceAll]);
  S := StringReplace(S, '?',  '', [rfReplaceAll]);
  S := StringReplace(S, ' ',  '', [rfReplaceAll]);
  if Pos('T', S) = 1 then
    Delete(S, 1, 1);

  result := '';
  for I := low(S) to high(S) do
  begin
    if S[I] = UpCase(S[I]) then
      result := result + ' ';
     result := result + LowerCase(S[I]);
  end;

  if Pos(' ', result) = 1 then
    Delete(result, 1, 1);
end;

function GetUnitDescription(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '!',  '', [rfReplaceAll]);
  Result := StringReplace(Result, '?',  '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ',  '', [rfReplaceAll]);
  if Pos('T', Result) = 1 then
    Delete(Result, 1, 1);
end;

function GetUnitClassName(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '!',  '', [rfReplaceAll]);
  Result := StringReplace(Result, '?',  '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ',  '', [rfReplaceAll]);

  if Result = 'double' then Result := '';
  if Result <> ''      then Result := Result + 'Unit';
end;

function GetUnitClassNameHelper(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '!',  '', [rfReplaceAll]);
  Result := StringReplace(Result, '?',  '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ',  '', [rfReplaceAll]);
  Result := Result + 'Helper';
end;

function GetUnitIdentifier(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '!',  '', [rfReplaceAll]);
  Result := StringReplace(Result, '?',  '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ',  '', [rfReplaceAll]);
  Result := Result + 'UnitId';
end;

function GetUnitQuantity(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, 'Foot!', 'Feet',   [rfReplaceAll]);
  Result := StringReplace(Result, 'Inch!', 'Inches', [rfReplaceAll]);
  Result := StringReplace(Result, 'y!',    'ies',    [rfReplaceAll]);
  Result := StringReplace(Result, '?',     's',      [rfReplaceAll]);
  Result := StringReplace(Result, ' ',     '',       [rfReplaceAll]);
end;

function GetPrefixes(const AShortSymbol: string): string;
var
  I, J: longint;
  S: TStringArray;
begin
  Result := '';
  S := Split(AShortSymbol);
  for I := Low(S) to High(S) do
  begin
    J := Pos('%s', S[I]);
    if J > 0 then
    begin
      if (S[I] = '%sg') or (S[I] = '%sg2') then
        result := result + ' pKilo,'
      else
        result := result + ' pNone,';
    end;
  end;
  S := nil;

  while (Length(Result) > 0) and (Result[Low (Result)] = ' ') do
    Delete(Result, Low (Result), 1);

  while (Length(Result) > 0) and (Result[High(Result)] = ',') do
    Delete(Result, High(Result), 1);
end;

function GetPrefixExponents(const AShortSymbol: string): string;
var
  I, Exponent: longint;
  S: TStringArray;
begin
  Result := '';
  Exponent := 1;
  S := Split(AShortSymbol);
  for I := Low(S) to High(S) do
  begin
    if S[I] = '.' then
      Exponent := 1
    else
      if S[I] = '/' then
        Exponent := -1
      else
      begin
        if Pos('%s', S[I]) > 0 then
        begin
          if S[I][Length(S[I])] in ['2', '3', '4', '5', '6', '7', '8', '9'] then
          begin
            if Exponent < 0 then
              Result := Result + ' -' + S[I][Length(S[I])] + ','
            else
              Result := Result + ' ' + S[I][Length(S[I])] + ',';
          end else
          begin
            if Exponent < 0 then
              Result := Result + ' -1,'
            else
              Result := Result + ' 1,';
          end;
        end;
      end;
  end;
  S := nil;

  while (Length(Result) > 0) and (Result[Low (Result)] = ' ') do
    Delete(Result, Low (Result), 1);

  while (Length(Result) > 0) and (Result[High(Result)] = ',') do
    Delete(Result, High(Result), 1);
end;

function CleanUnitName(const S: string): string;
begin
  Result := S;
  while Pos('  ', Result) > 0 do
  begin
    Delete(Result, Pos('  ', Result), 1);
  end;
end;

function CleanUnitSymbol(const S: string): string;
begin
  Result := S;
  while Pos(' ', Result) > 0 do
  begin
    Delete(Result, Pos(' ', Result), 1);
  end;
end;

procedure CleanDocument(S: TStringList);
var
  i: longint;
begin
  i := 0;
  while i < S.Count do
  begin
    if IsEmptyStr(S[i], [' ']) then
    begin
      S.Delete(i);
    end else
      Break;
  end;

  while (i + 1) < S.Count do
  begin
    if IsEmptyStr(S[i],     [' ']) and
       IsEmptyStr(S[i + 1], [' ']) then
    begin
      S.Delete(i + 1);
    end else
      inc(i);
  end;
end;

procedure TMainForm.AddQuantityOperator(AOperator, ALeftParent, ARightParent, AResult: string);
var
  S: string;
begin
  if ALeftParent  <> 'double' then ALeftParent  := GetUnitQuantity(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetUnitQuantity(ARightParent);
  if AResult      <> 'double' then AResult      := GetUnitQuantity(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + '; inline;');
    SectionB1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + ';');

    if AResult = 'double' then
      S := 'result :='
    else
      S := 'result.FValue :=';

    if ALeftParent = 'double' then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.FValue ' + AOperator;

    if ARightParent = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.FValue;';

    SectionB1.Append('begin ' + S + ' end;');
    SectionB1.Append('');
    Inc(OperatorCount);
  end else
    Memo.Append('ERROR: operator ' + AOperator + '(' + ALeftParent + '; ' + ARightParent + ') : ' + AResult + '; already esists.');
end;

procedure TMainForm.AddUnitIdOperator(AOperator, ALeftParent, ARightParent, AResult: string);
begin
  if ALeftParent  <> 'double' then ALeftParent  := GetUnitQuantity(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetUnitIdentifier(ARightParent);
  if AResult      <> 'double' then AResult      := GetUnitQuantity(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent  + '): ' + AResult + '; inline;');

    SectionB1.Append('');
    SectionB1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent  + '): ' + AResult + ';');

    if AResult <> 'double' then
    begin
      if ALeftParent <> 'double' then
        SectionB1.Append('begin result.FValue := ALeft.FValue; end;')
      else
        SectionB1.Append('begin result.FValue := ALeft; end;');
    end else
    begin
      if ALeftParent <> 'double' then
        SectionB1.Append('begin result := ALeft.FValue; end;')
      else
        SectionB1.Append('begin result := ALeft; end;');
    end;
    SectionB1.Append('');
    Inc(OperatorCount);
  end;
end;

procedure TMainForm.AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
  AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor, APrefixes: string);
begin
  if ClassList.IndexOf(GetUnitDescription(AClassName)) = -1 then
  begin
    ClassList.Append(GetUnitDescription(AClassName));

    if (ABaseClass = '') then
    begin
      SectionA1.Append('');
      SectionA1.Append('type');
      SectionA1.Append('  { Unit of ' + GetUnitComment(AClassName) + ' }');
      SectionA1.Append('  ' + GetUnitClassName(AClassName) + ' = record');
      SectionA1.Append('    const Symbol       = ''' + GetSymbol(AShortSymbol) + ''';');
      SectionA1.Append('    const SingularName = ''' + GetSingularName(ALongSymbol) + ''';');
      SectionA1.Append('    const PluralName   = ''' + GetPluralName(ALongSymbol) + ''';');
      SectionA1.Append('    const Prefixes : TPrefixes = ('  + GetPrefixes(AShortSymbol) + ');');
      SectionA1.Append('    const PrefixExponents : TIntegerDynArray = ('  + GetPrefixExponents(AShortSymbol) + ');');
      SectionA1.Append('  end;');
      SectionA1.Append('  ' + GetUnitQuantity(AClassName) + ' = specialize TQuantity<' + GetUnitClassName(AClassName) + '>;');
      SectionA1.Append('  ' + GetUnitIdentifier(AClassName) + ' = specialize TUnitId<' + GetUnitClassName(AClassName) + '>;');
      SectionA1.Append('');
      if (AIdentifierSymbol <> '') then
      begin
        SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetUnitIdentifier(AClassName)]));
        SectionA1.Append('');
        AddFactoredQuantity(AClassName, AIdentifierSymbol, '', APrefixes);
        SectionA1.Append('');
      end;
      Inc(BaseUnitCount);
    end else
    begin

      if AFactor = '' then
      begin
        SectionA1.Append('');
        SectionA1.Append('type');
        SectionA1.Append('  { Unit of ' + GetUnitComment(AClassName) + ' }');
        SectionA1.Append('  ' + GetUnitClassName(AClassName) + ' = record');
        SectionA1.Append('    const Symbol       = ''' + GetSymbol(AShortSymbol) + ''';');
        SectionA1.Append('    const SingularName = ''' + GetSingularName(ALongSymbol) + ''';');
        SectionA1.Append('    const PluralName   = ''' + GetPluralName(ALongSymbol) + ''';');
        SectionA1.Append('    const Prefixes : TPrefixes = ('  + GetPrefixes(AShortSymbol) + ');');
        SectionA1.Append('    const PrefixExponents : TIntegerDynArray = ('  + GetPrefixExponents(AShortSymbol) + ');');
        SectionA1.Append('  end;');
        SectionA1.Append('  ' + GetUnitQuantity(AClassName) + ' = specialize TQuantity<' + GetUnitClassName(ABaseClass) + '>;');
        SectionA1.Append('  ' + GetUnitIdentifier(AClassName) + ' = specialize TUnitId<' + GetUnitClassName(ABaseClass) + '>;');
        SectionA1.Append('');
        if (AIdentifierSymbol <> '') then
        begin
          SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetUnitIdentifier(AClassName)]));
          SectionA1.Append('');
          AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes);
          SectionA1.Append('');
        end;
        AddHelper(AClassName, ABaseClass, '');
        Inc(FactoredUnitCount);
      end else
      begin
        SectionA1.Append('');
        SectionA1.Append('type');
        SectionA1.Append('  { Unit of ' + GetUnitComment(AClassName) + ' }');
        SectionA1.Append('  ' + GetUnitClassName(AClassName) + ' = record');
        SectionA1.Append('    const Symbol       = ''' + GetSymbol(AShortSymbol) + ''';');
        SectionA1.Append('    const SingularName = ''' + GetSingularName(ALongSymbol) + ''';');
        SectionA1.Append('    const PluralName   = ''' + GetPluralName(ALongSymbol) + ''';');
        SectionA1.Append('    const Prefixes : TPrefixes = ('  + GetPrefixes(AShortSymbol) + ');');
        SectionA1.Append('    const PrefixExponents : TIntegerDynArray = ('  + GetPrefixExponents(AShortSymbol) + ');');
        if Pos('%s', AFactor) = 0 then
        SectionA1.Append('    const ToBaseFactor = ' + AFactor + ';');
        SectionA1.Append('  end;');

        if Pos('%s', AFactor) = 0 then
        SectionA1.Append('  ' + GetUnitQuantity(AClassName) + ' = specialize TQuantity<' + GetUnitClassName(ABaseClass) + '>;')
        else
        SectionA1.Append('  ' + GetUnitQuantity(AClassName) + ' = specialize TQuantity<' + GetUnitClassName(AClassName) + '>;');

        SectionA1.Append('  ' + GetUnitIdentifier(AClassName) + ' = specialize TUnitId<' + GetUnitClassName(AClassName) + '>;');
        SectionA1.Append('');

        if (AIdentifierSymbol <> '') then
        begin
          if Pos('%s', AFactor) = 0 then
          begin
            SectionA1.Append(Format('const %s: specialize TQuantity<%s> = (FValue: %s);',
              [AIdentifierSymbol, GetUnitClassName(ABaseClass), GetUnitClassName(AClassName) + '.ToBaseFactor']));
            SectionA1.Append('');
            AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes);
            SectionA1.Append('');
          end else
          begin
            SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetUnitIdentifier(AClassName)]));
            SectionA1.Append('');
          end;
        end;

        if Pos('%s', AFactor) = 0 then
        begin
          AddHelper(AClassName, ABaseClass, 'FValue / ' + GetUnitClassName(AClassName) + '.ToBaseFactor');
        end else
        begin
          AddHelper(ABaseClass, AClassName, Format(Copy(AFactor, 1, Pos('|', AFactor) -1), ['FValue']));
          AddHelper(AClassName, ABaseClass, Format(Copy(AFactor, Pos('|', AFactor) + 1, Length(AFactor)), ['FValue']));
        end;
        Inc(FactoredUnitCount);
      end;

    end;
  end;

  if (ABaseClass = '') then
  begin
    CheckClass(AClassName, AOperator, AClassParent1, AClassParent2);

    if AOperator = '*' then
    begin
      SectionA1.Append('');
      SectionA1.Append('// ' + AComment);
      SectionB1.Append('');
      SectionB1.Append('// ' + AComment);
      SectionB1.Append('');

      AddQuantityOperator('*', AClassParent1, AClassParent2, AClassName);
      if AClassParent1 <> AClassParent2 then
      begin
        AddQuantityOperator('*', AClassParent2, AClassParent1, AClassName);
      end;

      AddQuantityOperator('/', AClassName, AClassParent1, AClassParent2);
      if AClassParent1 <> AClassParent2 then
      begin
        AddQuantityOperator('/', AClassName, AClassParent2, AClassParent1);
      end;

      if Pos('OP1', AFactor) > 0 then AddUnitIdOperator('*', AClassParent1, AClassParent2, AClassName);
      if Pos('OP2', AFactor) > 0 then AddUnitIdOperator('*', AClassParent2, AClassParent1, AClassName);
      if Pos('OP3', AFactor) > 0 then AddUnitIdOperator('/', AClassName, AClassParent1, AClassParent2);
      if Pos('OP4', AFactor) > 0 then AddUnitIdOperator('/', AClassName, AClassParent2, AClassParent1);

    end else
      if AOperator = '/' then
      begin
        SectionA1.Append('');
        SectionA1.Append('// ' + AComment);
        SectionB1.Append('');
        SectionB1.Append('// ' + AComment);
        SectionB1.Append('');

        AddQuantityOperator('/', AClassParent1, AClassParent2, AClassName);
        AddQuantityOperator('*', AClassParent2, AClassName,    AClassParent1);
        AddQuantityOperator('*', AClassName,    AClassParent2, AClassParent1);
        AddQuantityOperator('/', AClassParent1, AClassName,    AClassParent2);

        if Pos('OP1', AFactor) > 0 then AddUnitIdOperator('/', AClassParent1, AClassParent2, AClassName);
        if Pos('OP2', AFactor) > 0 then AddUnitIdOperator('*', AClassParent2, AClassName, AClassParent1);
        if Pos('OP3', AFactor) > 0 then AddUnitIdOperator('*', AClassName, AClassParent2, AClassParent1);
        if Pos('OP4', AFactor) > 0 then AddUnitIdOperator('/', AClassParent1, AClassName, AClassParent2);

      end else
        if UpperCase(AOperator) = 'RECIPROCAL' then
        begin
          AddQuantityOperator('/', 'double', AClassParent1, AClassName);
          AddQuantityOperator('/', 'double', AClassName, AClassParent1);
        end else
        if Pos('power', LowerCase(AOperator)) > 0 then
        begin
          AddPower(AOperator, AClassParent1, AClassName);
        end;

  end else
    if ('=' = AOperator) then
    begin
      SectionA1.Append('');
      SectionB1.Append('');
      AddEquivalence(AClassName, ABaseClass);
      AddHelper(AClassName, ABaseClass, '');
      SectionB1.Append('');
      AddEquivalence(ABaseClass, AClassName);
      AddHelper(ABaseClass, AClassName, '');
      SectionB1.Append('');
      SectionA1.Append('');
    end else
      if ('helper' = lowercase(AOperator)) then
      begin
        SectionA1.Append('');
        SectionB1.Append('');
        AddHelper(AClassName, ABaseClass, '');
        SectionB1.Append('');
        SectionA1.Append('');
        AddHelper(ABaseClass, AClassName, '');
        SectionB1.Append('');
        SectionA1.Append('');
      end;

end;

procedure TMainForm.AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
var
  Params: string;
  Power: longint;
  Str: string;
begin
  Str := 'const %s: specialize TQuantity<%s> = (FValue: %s);';
  if AFactor <> '' then
    AFactor := AFactor + ' * ';

  if Length(APrefixes) = 24 then
    Params := APrefixes
  else
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
    if Params[ 1] = 'L' then SectionA1.Append(Format(Str, ['quetta' + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 1] = 'S' then SectionA1.Append(Format(Str, ['Q'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +30*Power))]));
    if Params[ 2] = 'L' then SectionA1.Append(Format(Str, ['ronna'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 2] = 'S' then SectionA1.Append(Format(Str, ['R'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +27*Power))]));
    if Params[ 3] = 'L' then SectionA1.Append(Format(Str, ['yotta'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 3] = 'S' then SectionA1.Append(Format(Str, ['Y'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +24*Power))]));
    if Params[ 4] = 'L' then SectionA1.Append(Format(Str, ['zetta'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 4] = 'S' then SectionA1.Append(Format(Str, ['Z'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +21*Power))]));
    if Params[ 5] = 'L' then SectionA1.Append(Format(Str, ['exa'    + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 5] = 'S' then SectionA1.Append(Format(Str, ['E'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +18*Power))]));
    if Params[ 6] = 'L' then SectionA1.Append(Format(Str, ['peta'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));
    if Params[ 6] = 'S' then SectionA1.Append(Format(Str, ['P'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +15*Power))]));

    if Params[ 7] = 'L' then SectionA1.Append(Format(Str, ['tera'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 7] = 'S' then SectionA1.Append(Format(Str, ['T'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 8] = 'L' then SectionA1.Append(Format(Str, ['giga'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 8] = 'S' then SectionA1.Append(Format(Str, ['G'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 9] = 'L' then SectionA1.Append(Format(Str, ['mega'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[ 9] = 'S' then SectionA1.Append(Format(Str, ['M'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[10] = 'L' then SectionA1.Append(Format(Str, ['kilo'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[10] = 'S' then SectionA1.Append(Format(Str, ['k'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[11] = 'L' then SectionA1.Append(Format(Str, ['hecto'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[11] = 'S' then SectionA1.Append(Format(Str, ['h'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[12] = 'L' then SectionA1.Append(Format(Str, ['deca'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[12] = 'S' then SectionA1.Append(Format(Str, ['da'     + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[13] = 'L' then SectionA1.Append(Format(Str, ['deci'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[13] = 'S' then SectionA1.Append(Format(Str, ['d'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[14] = 'L' then SectionA1.Append(Format(Str, ['centi'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[14] = 'S' then SectionA1.Append(Format(Str, ['c'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[15] = 'L' then SectionA1.Append(Format(Str, ['milli'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[15] = 'S' then SectionA1.Append(Format(Str, ['m'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[16] = 'L' then SectionA1.Append(Format(Str, ['micro'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[16] = 'S' then SectionA1.Append(Format(Str, ['mi'     + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[17] = 'L' then SectionA1.Append(Format(Str, ['nano'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[17] = 'S' then SectionA1.Append(Format(Str, ['n'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[18] = 'L' then SectionA1.Append(Format(Str, ['pico'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));
    if Params[18] = 'S' then SectionA1.Append(Format(Str, ['p'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));

    if Params[19] = 'L' then SectionA1.Append(Format(Str, ['femto'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[19] = 'S' then SectionA1.Append(Format(Str, ['f'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -15*Power))]));
    if Params[20] = 'L' then SectionA1.Append(Format(Str, ['atto'   + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[20] = 'S' then SectionA1.Append(Format(Str, ['a'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -18*Power))]));
    if Params[21] = 'L' then SectionA1.Append(Format(Str, ['zepto'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[21] = 'S' then SectionA1.Append(Format(Str, ['z'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -21*Power))]));
    if Params[22] = 'L' then SectionA1.Append(Format(Str, ['yocto'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[22] = 'S' then SectionA1.Append(Format(Str, ['y'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -24*Power))]));
    if Params[23] = 'L' then SectionA1.Append(Format(Str, ['ronto'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[23] = 'S' then SectionA1.Append(Format(Str, ['r'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -27*Power))]));
    if Params[24] = 'L' then SectionA1.Append(Format(Str, ['quecto' + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
    if Params[24] = 'S' then SectionA1.Append(Format(Str, ['q'      + AIdentifierSymbol, GetUnitClassName(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -30*Power))]));
  end else
    if (LowerCase(AIdentifierSymbol) = 'kg') then
    begin
      AIdentifierSymbol := 'g';
      SectionA1.Append(Format(Str, ['h'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-01']));
      SectionA1.Append(Format(Str, ['da' + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-02']));
      SectionA1.Append(Format(Str, [''   + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-03']));
      SectionA1.Append(Format(Str, ['d'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-04']));
      SectionA1.Append(Format(Str, ['c'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-05']));
      SectionA1.Append(Format(Str, ['m'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-06']));
      SectionA1.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-09']));
      SectionA1.Append(Format(Str, ['n'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-12']));
      SectionA1.Append(Format(Str, ['p'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-15']));
    end else
      if (LowerCase(AIdentifierSymbol) = 'kg2') then
      begin
         AIdentifierSymbol := 'g2';
        SectionA1.Append(Format(Str, ['h'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-02']));
        SectionA1.Append(Format(Str, ['da' + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-04']));
        SectionA1.Append(Format(Str, [''   + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-06']));
        SectionA1.Append(Format(Str, ['d'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-08']));
        SectionA1.Append(Format(Str, ['c'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-10']));
        SectionA1.Append(Format(Str, ['m'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-12']));
        SectionA1.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-18']));
        SectionA1.Append(Format(Str, ['n'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-24']));
        SectionA1.Append(Format(Str, ['p'  + AIdentifierSymbol, GetUnitClassName(ABaseClass), '1E-30']));
      end;
end;

procedure TMainForm.AddPower(AOperator, AQuantity, AResult: string);
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

  AQuantity := GetUnitQuantity(AQuantity);
  AResult   := GetUnitQuantity(AResult);

  SectionA3.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionA3.Append('function ' + S1 + 'Root(AQuantity: ' +  AResult + '): ' + AQuantity + ';');

  SectionB3.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionB3.Append('begin result.FValue := IntPower(AQuantity.FValue, ' + S2 + '); end;');
  SectionB3.Append('');

  SectionB3.Append('function ' + S1 + 'Root(AQuantity: ' + AResult + '): ' + AQuantity + ';');
  SectionB3.Append('begin result.FValue := Power(AQuantity.FValue, ' + S3 + '); end;');
  SectionB3.Append('');
end;

procedure TMainForm.AddHelper(AClassName, ABaseClass, AFactor: string);
var
  Index: longint;
begin
  Index := SectionA2.IndexOf('  ' + GetUnitClassNameHelper(ABaseClass) + ' = record helper for ' + GetUnitQuantity(ABaseClass));
  if Index = -1 then
  begin
    SectionA2.Append('');
    SectionA2.Append('type');
    SectionA2.Append('  ' + GetUnitClassNameHelper(ABaseClass) + ' = record helper for ' + GetUnitQuantity(ABaseClass));
    SectionA2.Append('    function To' + GetUnitDescription(AClassName) + ': specialize TQuantity<' + GetUnitClassName(AClassName) + '>;');
    SectionA2.Append('  end;');
    SectionA2.Append('');
  end else
  begin
    SectionA2.Insert(Index + 1, '    function To' + GetUnitDescription(AClassName) + ': specialize TQuantity<' + GetUnitClassName(AClassName) + '>;');
  end;

  SectionB2.Append('');
  SectionB2.Append('function ' + GetUnitClassNameHelper(ABaseClass) + '.To' + GetUnitDescription(AClassName) + ': specialize TQuantity<' + GetUnitClassName(AClassName) + '>;');

  if AFactor = '' then
    SectionB2.Append('begin result.FValue := FValue; end;')
  else
    SectionB2.Append('begin result.FValue := ' + AFactor + '; end;');

  SectionB2.Append('');
end;

procedure TMainForm.AddEquivalence(AClassName, ABaseClass: string);
begin
  SectionA1.Append('operator :=(const AQuantity: ' + GetUnitQuantity(AClassName) + '): ' + GetUnitQuantity(ABaseClass) + '; inline;');
  SectionB1.Append('operator :=(const AQuantity: ' + GetUnitQuantity(AClassName) + '): ' + GetUnitQuantity(ABaseClass) + '; inline;');

  if GetUnitQuantity(AClassName) = 'double' then
    SectionB1.Append('begin result.FValue := AQuantity; end;')
  else
    if GetUnitQuantity(ABaseClass) = 'double' then
      SectionB1.Append('begin result := AQuantity.FValue; end;')
    else
      SectionB1.Append('begin result.FValue := AQuantity.FValue; end;');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex          := 0;
  WindowState                   := wsMaximized;
  WorksheetGrid.AutoFillColumns := True;
end;

procedure TMainForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    WorksheetGrid.BeginUpdate;
    WorksheetGrid.AutoFillColumns   := True;
    WorkbookSource.AutoDetectFormat := True;
    WorksheetGrid.LoadFromSpreadSheetFile(OpenDialog.FileName);
    WorksheetGrid.AutoFillColumns   := False;
    WorksheetGrid.EndUpdate(True);
  end;
end;

procedure TMainForm.ExportBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    SynEdit.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

 procedure TMainForm.RunBtnClick(Sender: TObject);
var
  I: longint;
  Document: TStringList;
  Stream: TResourceStream;
begin
  Document     := TSTringList.Create;
  ClassList    := TStringList.Create;
  CheckList    := nil;
  OperatorList := TStringList.Create;
  SectionA0    := TStringList.Create;
  SectionA1    := TStringList.Create;
  SectionA2    := TStringList.Create;
  SectionA3    := TStringList.Create;
  SectionA4    := TStringList.Create;

  SectionB1    := TStringList.Create;
  SectionB2    := TStringList.Create;
  SectionB3    := TStringList.Create;
  SectionB4    := TStringList.Create;

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
  SectionA2.Append('{ Helpers }');
  SectionA2.Append('');
  SectionB2.Append('');
  SectionB2.Append('{ Helpers }');
  SectionB2.Append('');

  SectionA3.Append('');
  SectionA3.Append('{ Power units }');
  SectionA3.Append('');
  SectionB3.Append('');
  SectionB3.Append('{ Power quantities }');
  SectionB3.Append('');

  if TrigonometricCheckBox.Checked then
  begin
    Stream := TResourceStream.Create(HInstance, 'SECTION-A4', RT_RCDATA);
    SectionA4.LoadFromStream(Stream);
    SectionA4.Insert(0, '');
    Stream.Destroy;

    Stream := TResourceStream.Create(HInstance, 'SECTION-B4', RT_RCDATA);
    SectionB4.LoadFromStream(Stream);
    SectionB4.Insert(0, '');
    Stream.Destroy;
  end;

  Memo.Clear;
  BaseUnitCount     := 0;
  FactoredUnitCount := 0;
  OperatorCount     := 0;
  for I := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    if (WorksheetGrid.Worksheet.ReadAsText(I, _class_name) <> '') then
    begin
      if (Pos('//',  WorksheetGrid.Worksheet.ReadAsText(I, _class_name)) = 0) and
         (Pos('TCL', WorksheetGrid.Worksheet.ReadAsText(I, _class_name)) = 0) then
        AddClass(
                          WorksheetGrid.Worksheet.ReadAsText(I, _class_name       ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _operator         ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_1   ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_2   ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _comment          ),
          CleanUnitName  (WorksheetGrid.Worksheet.ReadAsText(I, _long_symbol      )),
          CleanUnitSymbol(WorksheetGrid.Worksheet.ReadAsText(I, _short_symbol     )),
                          WorksheetGrid.Worksheet.ReadAsText(I, _identifier_symbol),
                          WorksheetGrid.Worksheet.ReadAsText(I, _base_class       ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _factor           ),
                          WorksheetGrid.Worksheet.ReadAsText(I, _prefixes         ));
    end;
  end;

  SectionA0.Append('');
  SectionA0.Append('{');
  SectionA0.Append(Format('  ADimPas library built on %s.', [DateToStr(Now)]));
  SectionA0.Append('');
  SectionA0.Append(Format('  Number of base units: %d', [BaseUnitCount]));
  SectionA0.Append(Format('  Number of factored units: %d', [FactoredUnitCount]));
  SectionA0.Append(Format('  Number of operators: %d', [OperatorCount]));
  SectionA0.Append('}');
  SectionA0.Append('');

  for I := 0 to SectionA0.Count -1 do Document.Append(SectionA0[I]);
  for I := 0 to SectionA1.Count -1 do Document.Append(SectionA1[I]);
  for I := 0 to SectionA2.Count -1 do Document.Append(SectionA2[I]);
  for I := 0 to SectionA3.Count -1 do Document.Append(SectionA3[I]);
  for I := 0 to SectionA4.Count -1 do Document.Append(SectionA4[I]);

  for I := 0 to SectionB1.Count -1 do Document.Append(SectionB1[I]);
  for I := 0 to SectionB2.Count -1 do Document.Append(SectionB2[I]);
  for I := 0 to SectionB3.Count -1 do Document.Append(SectionB3[I]);
  for I := 0 to SectionB4.Count -1 do Document.Append(SectionB4[I]);

  Document.Append('');
  Document.Append('end.');
  CleanDocument(Document);

  SynEdit.BeginUpdate(True);
  SynEdit.Lines.Clear;
  for I := 0 to Document.Count - 1 do
  begin
    SynEdit.Append(Document[I]);
  end;
  SynEdit.EndUpdate;

  SectionB4.Destroy;
  SectionB3.Destroy;
  SectionB2.Destroy;
  SectionB1.Destroy;

  SectionA4.Destroy;
  SectionA3.Destroy;
  SectionA2.Destroy;
  SectionA1.Destroy;
  SectionA0.Destroy;

  OperatorList.Destroy;
  ClassList.Destroy;
  CheckList := nil;
  Document.Destroy;

  PageControl.TabIndex := 1;
end;

procedure TMainForm.CheckClass(AClassName, AOperator, AClassParent1, AClassParent2: string);
var
  I, Index, Index1, Index2: longint;
  S: string;
  T: TDBItem;
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

      CheckList[Index].Name := GetUnitClassName(AClassName);
      for I := Low(CheckList[Index].Exponents) to High(CheckList[Index].Exponents) do
        CheckList[Index].Exponents[I] := 0;

      if (GetUnitClassName(AClassName) = 'TKilogramUnit') then CheckList[Index].Exponents[1] := 1;
      if (GetUnitClassName(AClassName) = 'TMeterUnit'   ) then CheckList[Index].Exponents[2] := 1;
      if (GetUnitClassName(AClassName) = 'TSecondUnit'  ) then CheckList[Index].Exponents[3] := 1;
      if (GetUnitClassName(AClassName) = 'TKelvinUnit'  ) then CheckList[Index].Exponents[4] := 1;
      if (GetUnitClassName(AClassName) = 'TAmpereUnit'  ) then CheckList[Index].Exponents[5] := 1;
      if (GetUnitClassName(AClassName) = 'TMoleUnit'    ) then CheckList[Index].Exponents[6] := 1;
      if (GetUnitClassName(AClassName) = 'TCandelaUnit' ) then CheckList[Index].Exponents[7] := 1;

    end else
    begin

      Index1 := GetIndex(AClassParent1);
      Index2 := GetIndex(AClassParent2);
      if (Index1 = -1) and (Index2 = -1) then
      begin
        if (GetUnitClassName(AClassName) <> 'TRadianUnit'   ) and
           (GetUnitClassName(AClassName) <> 'TSteradianUnit') then
          Memo.Append('ERROR:3 ');
        Exit;
      end;

      T.Name := GetUnitClassName(AClassName);
      for I := Low(T.Exponents) to High(T.Exponents) do
      begin
        T.Exponents[I] := 0;
        if Index1 <> -1 then
          T.Exponents[I] := CheckList[Index1].Exponents[I];

        if Index2 <> -1 then
        begin
          if AOperator = '*' then
            T.Exponents[I] := T.Exponents[I] + CheckList[Index2].Exponents[I]
          else
            if AOperator = '/' then
              T.Exponents[I] := T.Exponents[I] - CheckList[Index2].Exponents[I];
        end;
      end;


      for I := Low(CheckList) to High(CheckList) do
      begin
        if (CheckList[I].Exponents[1] = T.Exponents[1]) and
           (CheckList[I].Exponents[2] = T.Exponents[2]) and
           (CheckList[I].Exponents[3] = T.Exponents[3]) and
           (CheckList[I].Exponents[4] = T.Exponents[4]) and
           (CheckList[I].Exponents[5] = T.Exponents[5]) and
           (CheckList[I].Exponents[6] = T.Exponents[6]) and
           (CheckList[I].Exponents[7] = T.Exponents[7]) then
        begin
          S := 'WARNING: ' + CheckList[I].Name + ' is equal to ' + GetUnitClassName(AClassName) + ' ' + GetSIUnit(I) + ';';
          if Memo.Lines.IndexOf(S) = -1 then
          begin
            Memo.Append(S);
          end;
        end;
      end;

      Index := Length(CheckList);
      SetLength(CheckList, Index + 1);

      CheckList[Index].Name := T.Name;
      CheckList[Index].Exponents[1] := T.Exponents[1];
      CheckList[Index].Exponents[2] := T.Exponents[2];
      CheckList[Index].Exponents[3] := T.Exponents[3];
      CheckList[Index].Exponents[4] := T.Exponents[4];
      CheckList[Index].Exponents[5] := T.Exponents[5];
      CheckList[Index].Exponents[6] := T.Exponents[6];
      CheckList[Index].Exponents[7] := T.Exponents[7];
    end;

  end else
  begin

    T := CheckList[Index];
    for I := Low(CheckList) to High(CheckList) do
    begin
      if (CheckList[I].Name = T.Name) then
      begin
        if (CheckList[I].Exponents[1] <> T.Exponents[1]) or
           (CheckList[I].Exponents[2] <> T.Exponents[2]) or
           (CheckList[I].Exponents[3] <> T.Exponents[3]) or
           (CheckList[I].Exponents[4] <> T.Exponents[4]) or
           (CheckList[I].Exponents[5] <> T.Exponents[5]) or
           (CheckList[I].Exponents[6] <> T.Exponents[6]) or
           (CheckList[I].Exponents[7] <> T.Exponents[7]) then
        begin
          S := 'ERROR:   ' + CheckList[I].Name + ' doesn''t match previous declaration ' + GetSIUnit(I) + ';';
          if Memo.Lines.IndexOf(S) = -1 then
          begin
            Memo.Append(S);
          end;
        end;
      end;
    end;

  end;
end;

function TMainForm.GetIndex(const AClassName: string): longint;
var
  I: longint;
begin
  Result := -1;
  for I := Low(CheckList) to High(CheckList) do
    if GetUnitClassName(AClassName) = CheckList[I].Name then Exit(I);
end;

function TMainForm.GetSIunit(Index: longint): string;
var
  I: longint;
begin
  Result := '';
  for I := Low(CheckList[Index].Exponents) to High(CheckList[Index].Exponents) do
    if CheckList[Index].Exponents[I] > 0 then
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
      if CheckList[Index].Exponents[I] > 1 then
        Result := Result + IntToStr(CheckList[Index].Exponents[I]);
    end;

  for I := Low(CheckList[Index].Exponents) to High(CheckList[Index].Exponents) do
    if CheckList[Index].Exponents[I] < 0 then
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
      if CheckList[Index].Exponents[I] < -1 then
        Result := Result + IntToStr(Abs(CheckList[Index].Exponents[I]));
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

end.

