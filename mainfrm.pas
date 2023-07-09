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
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpspreadsheetgrid, fpspreadsheetctrls, fpspreadsheet, fpsallformats,
  Forms, Controls, Graphics, Dialogs, Grids, Buttons, ComCtrls, StdCtrls, SynHighlighterPas,
  SynEdit;

type
  { TMainForm }

  TMainForm = class(TForm)
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
    ClassList: TStringList;
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
    procedure AddFactoredUnitId(const AIdentifierSymbol, ABaseClass, AFactor: string);

    procedure AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
      AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor: string);

    procedure AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor: string);

    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);
    procedure AddEquivalence(AOperator, AClassName, ABaseClass: string);
  public

  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, Math, StrUtils;

const
  _class_name        = 0;
  _operator          = 1;
  _class_parent_1    = 2;
  _class_parent_2    = 3;
  _comment           = 4;
  _long_symbol       = 5;
  _short_symbol      = 6;
  _identifier_symbol = 7;
  _base_class        = 8;
  _factor            = 9;

function GetNM(const S: string): string;
begin
  Result := S;
  while Pos('?', Result) > 0 do
    Delete(Result, Pos('?', Result), 1);
  if Pos('T', Result) = 1 then
    Delete(Result, 1, 1);
end;

function GetUN(const S: string): string;
begin
  Result := S;
  while Pos('?', Result) > 0 do
    Delete(Result, Pos('?', Result), 1);
  Result := Result + 'Unit';
end;

function GetUH(const S: string): string;
begin
  Result := S;
  while Pos('?', Result) > 0 do
    Delete(Result, Pos('?', Result), 1);
  Result := Result + 'Helper';
end;

function GetID(const S: string): string;
begin
  Result := S;
  while Pos('?', Result) > 0 do
    Delete(Result, Pos('?', Result), 1);
  Result := Result + 'UnitId';
end;

function GetQT(const S: string): string;
begin
  Result := S;
  while Pos('?', Result) > 0 do
    Result[Pos('?', Result)] := 's';
end;

procedure CleanDocument(S: TStringList);
var
  i: longint;
begin
  i := 0;
  while i < S.Count do
  begin
    if (IsEmptyStr(S[i], [' '])) then
    begin
      S.Delete(i);
    end else
      Break;
  end;

  while (i + 1) < S.Count do
  begin
    if (IsEmptyStr(S[i], [' ']) and IsEmptyStr(S[i + 1], [' '])) then
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
  if ALeftParent  <> 'double' then ALeftParent  := GetQT(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetQT(ARightParent);
  if AResult      <> 'double' then AResult      := GetQT(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + '; inline;');
    SectionB1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + ';');
    SectionB1.Append('begin');

    if AResult = 'double' then
      S := '  result :='
    else
      S := '  result.FValue :=';

    if ALeftParent = 'double' then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.FValue ' + AOperator;

    if ARightParent = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.FValue;';

    SectionB1.Append(S);
    SectionB1.Append('end;');
    SectionB1.Append('');
  end else
    MessageDlg('Duplicate Operator: ',  ALeftParent + AOperator + ARightParent + '=' + AResult + ' already esists.', mtError, [mbOk], '');
end;

procedure TMainForm.AddUnitIdOperator(AOperator, ALeftParent, ARightParent, AResult: string);
begin
  if ALeftParent  <> 'double' then ALeftParent  := GetQT(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetID(ARightParent);
  if AResult      <> 'double' then AResult      := GetQT(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const {%H-}ARight: ' + ARightParent  + '): ' + AResult + '; inline;');

    SectionB1.Append('');
    SectionB1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const {%H-}ARight: ' + ARightParent  + '): ' + AResult + ';');
    SectionB1.Append('begin');

    if AResult <> 'double' then
    begin
      if ALeftParent <> 'double' then
        SectionB1.Append('  result.FValue := ALeft.FValue;')
      else
        SectionB1.Append('  result.FValue := ALeft;');
    end else
    begin
      if ALeftParent <> 'double' then
        SectionB1.Append('  result := ALeft.FValue;')
      else
        SectionB1.Append('  result := ALeft;');
    end;
    SectionB1.Append('end;');
    SectionB1.Append('');
  end;
end;

procedure TMainForm.AddFactoredUnitId(const AIdentifierSymbol, ABaseClass, AFactor: string);
begin
  SectionA1.Append('');
  SectionA1.Append(Format('function %s: %s;' , [AIdentifierSymbol, ABaseClass]));
  SectionA1.Append('');

  SectionB1.Append('');
  SectionB1.Append(Format('function %s: %s;' , [AIdentifierSymbol, ABaseClass]));
  SectionB1.Append('begin');
  SectionB1.Append(Format('  result.Value := %s;' , [AFactor]));
  SectionB1.Append('end;');
  SectionB1.Append('');
end;

procedure TMainForm.AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
  AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor: string);
begin
  if ClassList.IndexOf(GetNM(AClassName)) = -1 then
  begin
    ClassList.Append(GetNM(AClassName));

    if (ABaseClass = '') then
    begin
      SectionA1.Append('');
      SectionA1.Append('type');
      SectionA1.Append('  { Unit of ' + GetNM(AClassName) + ' }');
      SectionA1.Append('  ' + GetUN(AClassName) + ' = record');
      SectionA1.Append('    const Symbol = ''' + AShortSymbol + ''';');
      SectionA1.Append('    const Name   = ''' + ALongSymbol + ''';');
      SectionA1.Append('  end;');
      SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(AClassName) + '>;');
      SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TUnitId<' + GetUN(AClassName) + '>;');
      SectionA1.Append('');
      if (AIdentifierSymbol <> '') then
      begin
        SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetID(AClassName)]));
        SectionA1.Append('');
        AddFactoredQuantity(AClassName, AIdentifierSymbol, '');
        SectionA1.Append('');
      end;

    end else
    begin

      if AFactor = '' then
      begin
        SectionA1.Append('');
        SectionA1.Append('type');
        SectionA1.Append('  { Unit of ' + GetNM(AClassName) + ' }');
        SectionA1.Append('  ' + GetUN(AClassName) + ' = record');
        SectionA1.Append('    const Symbol = ''' + AShortSymbol + ''';');
        SectionA1.Append('    const Name   = ''' + ALongSymbol  + ''';');
        SectionA1.Append('  end;');
        SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(ABaseClass) + '>;');
        SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TUnitId<' + GetUN(ABaseClass) + '>;');
        SectionA1.Append('');
        if (AIdentifierSymbol <> '') then
        begin
          SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetID(AClassName)]));
          SectionA1.Append('');
          AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor);
          SectionA1.Append('');
        end;
        AddHelper(AClassName, ABaseClass, '');
      end else
      begin
        SectionA1.Append('');
        SectionA1.Append('type');
        SectionA1.Append('  { Unit of ' + GetNM(AClassName) + ' }');
        SectionA1.Append('  ' + GetUN(AClassName) + ' = record');
        SectionA1.Append('    const Symbol = ''' + AShortSymbol + ''';');
        SectionA1.Append('    const Name   = ''' + ALongSymbol  + ''';');
        if Pos('%s', AFactor) = 0 then
        begin
          SectionA1.Append('    const Factor = '   + AFactor      + ';');
          SectionA1.Append('  end;');
          SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(ABaseClass) + '>;');
        end else
        begin
          SectionA1.Append('  end;');
          SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(AClassName) + '>;');
        end;
        SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TUnitId<' + GetUN(AClassName) + '>;');
        SectionA1.Append('');

        if (AIdentifierSymbol <> '') then
        begin
          if Pos('%s', AFactor) = 0 then
          begin
            SectionA1.Append(Format('const %s: specialize TQuantity<%s> = (FValue: %s);', [AIdentifierSymbol, GetUN(ABaseClass), AFactor]));
            SectionA1.Append('');
            AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor);
            SectionA1.Append('');
          end else
          begin
            SectionA1.Append(Format('var %s: %s;', [AIdentifierSymbol, GetID(AClassName)]));
            SectionA1.Append('');
          end;
        end;

        if Pos('%s', AFactor) = 0 then
        begin
          AddHelper(AClassName, ABaseClass, 'FValue / ' + GetUN(AClassName) + '.Factor');
        end else
        begin
          AddHelper(ABaseClass, AClassName, Format(Copy(AFactor, 1, Pos('|', AFactor) -1), ['FValue']));
          AddHelper(AClassName, ABaseClass, Format(Copy(AFactor, Pos('|', AFactor) + 1, Length(AFactor)), ['FValue']));
        end;

      end;
    end;
  end;

  if (ABaseClass = '') then
  begin
    if AOperator = '*' then
    begin
      SectionA1.Append('');
      SectionA1.Append('// ' + AComment);
      SectionB1.Append('');
      SectionB1.Append('// ' + AComment);

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

        AddQuantityOperator('/', AClassParent1, AClassParent2, AClassName);
        AddQuantityOperator('*', AClassParent2, AClassName,    AClassParent1);
        AddQuantityOperator('*', AClassName,    AClassParent2, AClassParent1);
        AddQuantityOperator('/', AClassParent1, AClassName,    AClassParent2);

        if Pos('OP1', AFactor) > 0 then AddUnitIdOperator('/', AClassParent1, AClassParent2, AClassName);
        if Pos('OP2', AFactor) > 0 then AddUnitIdOperator('*', AClassParent2, AClassName, AClassParent1);
        if Pos('OP3', AFactor) > 0 then AddUnitIdOperator('*', AClassName, AClassParent2, AClassParent1);
        if Pos('OP4', AFactor) > 0 then AddUnitIdOperator('/', AClassParent1, AClassName, AClassParent2);

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
      AddEquivalence(AOperator, AClassName, ABaseClass);
      SectionB1.Append('');
      AddEquivalence(AOperator, ABaseClass, AClassName);
      SectionB1.Append('');
      SectionA1.Append('');
    end else
      if (':=' = AOperator) then
      begin
        SectionA1.Append('');
        SectionB1.Append('');
        AddEquivalence(AOperator, AClassName, ABaseClass);
        SectionB1.Append('');
        SectionA1.Append('');
      end;

end;

procedure TMainForm.AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor: string);
var
  Params: string;
  Power: longint;
  Str: string;
begin
  if AFactor <> '' then
    AFactor := AFactor + ' * ';

  Str    := 'const %s: specialize TQuantity<%s> = (FValue: %s);';
  Params := 'SSLSSSSSSSSS';
  Power  := 1;
  if Pos('2', AIdentifierSymbol) > 0 then Power := 2;
  if Pos('3', AIdentifierSymbol) > 0 then Power := 3;
  if Pos('4', AIdentifierSymbol) > 0 then Power := 4;
  if Pos('5', AIdentifierSymbol) > 0 then Power := 5;
  if Pos('6', AIdentifierSymbol) > 0 then Power := 6;

  if LowerCase(AIdentifierSymbol) = 'day'     then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'day2'    then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'hr'      then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'hr2'     then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'minute'  then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'minute2' then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 's'       then Params := '------SSSSSS';
  if LowerCase(AIdentifierSymbol) = 's2'      then Params := '------------';

  if LowerCase(AIdentifierSymbol) = 'm'       then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'm2'      then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'm3'      then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'm4'      then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'm5'      then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'm6'      then Params := '---SSSSSSSSS';
  if LowerCase(AIdentifierSymbol) = 'au'      then Params := '------------';

  if LowerCase(AIdentifierSymbol) = 'a'       then Params := 'SSLSSSSSSSSL';
  if LowerCase(AIdentifierSymbol) = 'a2'      then Params := 'SSLSSSSSSSSL';

  if LowerCase(AIdentifierSymbol) = 'siemens' then Params := 'LLLLLLLLLLLL';

  if LowerCase(AIdentifierSymbol) = 'ton'     then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'l'       then Params := '---SSSSSS---';
  if LowerCase(AIdentifierSymbol) = 'degc'    then Params := '------------';
  if LowerCase(AIdentifierSymbol) = 'degf'    then Params := '------------';


  if (LowerCase(AIdentifierSymbol) <> 'kg' ) and
     (LowerCase(AIdentifierSymbol) <> 'kg2') then
  begin
    if Params[ 1] = 'L' then SectionA1.Append(Format(Str, [' tera'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 1] = 'S' then SectionA1.Append(Format(Str, ['    T'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, +12*Power))]));
    if Params[ 2] = 'L' then SectionA1.Append(Format(Str, [' giga'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 2] = 'S' then SectionA1.Append(Format(Str, ['    G'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 9*Power))]));
    if Params[ 3] = 'L' then SectionA1.Append(Format(Str, [' mega'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[ 3] = 'S' then SectionA1.Append(Format(Str, ['    M'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 6*Power))]));
    if Params[ 4] = 'L' then SectionA1.Append(Format(Str, [' kilo'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[ 4] = 'S' then SectionA1.Append(Format(Str, ['    k'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 3*Power))]));
    if Params[ 5] = 'L' then SectionA1.Append(Format(Str, ['hecto'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[ 5] = 'S' then SectionA1.Append(Format(Str, ['    h'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 2*Power))]));
    if Params[ 6] = 'L' then SectionA1.Append(Format(Str, [' deca'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[ 6] = 'S' then SectionA1.Append(Format(Str, ['   da'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, + 1*Power))]));
    if Params[ 7] = 'L' then SectionA1.Append(Format(Str, [' deci'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[ 7] = 'S' then SectionA1.Append(Format(Str, ['    d'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 1*Power))]));
    if Params[ 8] = 'L' then SectionA1.Append(Format(Str, ['centi'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[ 8] = 'S' then SectionA1.Append(Format(Str, ['    c'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 2*Power))]));
    if Params[ 9] = 'L' then SectionA1.Append(Format(Str, ['milli'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[ 9] = 'S' then SectionA1.Append(Format(Str, ['    m'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 3*Power))]));
    if Params[10] = 'L' then SectionA1.Append(Format(Str, ['micro'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[10] = 'S' then SectionA1.Append(Format(Str, ['   mi'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 6*Power))]));
    if Params[11] = 'L' then SectionA1.Append(Format(Str, [' nano'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[11] = 'S' then SectionA1.Append(Format(Str, ['    n'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, - 9*Power))]));
    if Params[12] = 'L' then SectionA1.Append(Format(Str, [' pico'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));
    if Params[12] = 'S' then SectionA1.Append(Format(Str, ['    p'  + AIdentifierSymbol, GetUN(ABaseClass), AFactor + FormatFloat('0E+00', IntPower(10, -12*Power))]));
  end else
    if (LowerCase(AIdentifierSymbol) = 'kg') then
    begin
      AIdentifierSymbol := 'g';
      SectionA1.Append(Format(Str, [' h' + AIdentifierSymbol, GetUN(ABaseClass), '1E-01']));
      SectionA1.Append(Format(Str, ['da' + AIdentifierSymbol, GetUN(ABaseClass), '1E-02']));
      SectionA1.Append(Format(Str, ['  ' + AIdentifierSymbol, GetUN(ABaseClass), '1E-03']));
      SectionA1.Append(Format(Str, [' d' + AIdentifierSymbol, GetUN(ABaseClass), '1E-04']));
      SectionA1.Append(Format(Str, [' c' + AIdentifierSymbol, GetUN(ABaseClass), '1E-05']));
      SectionA1.Append(Format(Str, [' m' + AIdentifierSymbol, GetUN(ABaseClass), '1E-06']));
      SectionA1.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUN(ABaseClass), '1E-09']));
      SectionA1.Append(Format(Str, [' n' + AIdentifierSymbol, GetUN(ABaseClass), '1E-12']));
      SectionA1.Append(Format(Str, [' p' + AIdentifierSymbol, GetUN(ABaseClass), '1E-15']));
    end else
      if (LowerCase(AIdentifierSymbol) = 'kg2') then
      begin
         AIdentifierSymbol := 'g2';
        SectionA1.Append(Format(Str, [' h' + AIdentifierSymbol, GetUN(ABaseClass), '1E-02']));
        SectionA1.Append(Format(Str, ['da' + AIdentifierSymbol, GetUN(ABaseClass), '1E-04']));
        SectionA1.Append(Format(Str, ['  ' + AIdentifierSymbol, GetUN(ABaseClass), '1E-06']));
        SectionA1.Append(Format(Str, [' d' + AIdentifierSymbol, GetUN(ABaseClass), '1E-08']));
        SectionA1.Append(Format(Str, [' c' + AIdentifierSymbol, GetUN(ABaseClass), '1E-10']));
        SectionA1.Append(Format(Str, [' m' + AIdentifierSymbol, GetUN(ABaseClass), '1E-12']));
        SectionA1.Append(Format(Str, ['mi' + AIdentifierSymbol, GetUN(ABaseClass), '1E-18']));
        SectionA1.Append(Format(Str, [' n' + AIdentifierSymbol, GetUN(ABaseClass), '1E-24']));
        SectionA1.Append(Format(Str, [' p' + AIdentifierSymbol, GetUN(ABaseClass), '1E-30']));
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

  AQuantity := GetQT(AQuantity);
  AResult   := GetQT(AResult);

  SectionA3.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionA3.Append('function ' + S1 + 'Root(AQuantity: ' +  AResult + '): ' + AQuantity + ';');

  SectionB3.Append('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionB3.Append('begin');
  SectionB3.Append('  result.FValue := Power(AQuantity.FValue, ' + S2 + ');');
  SectionB3.Append('end;');
  SectionB3.Append('');

  SectionB3.Append('function ' + S1 + 'Root(AQuantity: ' + AResult + '): ' + AQuantity + ';');
  SectionB3.Append('begin');
  SectionB3.Append('  result.FValue := Power(AQuantity.FValue, ' + S3 + ');');
  SectionB3.Append('end;');
  SectionB3.Append('');
end;

procedure TMainForm.AddHelper(AClassName, ABaseClass, AFactor: string);
var
  Index: longint;
begin
  Index := SectionA2.IndexOf('  ' + GetUH(ABaseClass) + ' = record helper for ' + GetQT(ABaseClass));
  if Index = -1 then
  begin
    SectionA2.Append('');
    SectionA2.Append('type');
    SectionA2.Append('  ' + GetUH(ABaseClass) + ' = record helper for ' + GetQT(ABaseClass));
    SectionA2.Append('    function As' + GetNM(AClassName) + ': specialize TQuantity<' + GetUN(AClassName) + '>;');
    SectionA2.Append('  end;');
    SectionA2.Append('');
  end else
  begin
    SectionA2.Insert(Index + 1, '    function As' + GetNM(AClassName) + ': specialize TQuantity<' + GetUN(AClassName) + '>;');
  end;

  SectionB2.Append('');
  SectionB2.Append('function ' + GetUH(ABaseClass) + '.As' + GetNM(AClassName) + ': specialize TQuantity<' + GetUN(AClassName) + '>;');
  SectionB2.Append('begin');

  if AFactor = '' then
    SectionB2.Append('  result.FValue := FValue;')
  else
    SectionB2.Append('  result.FValue := ' + AFactor + ';');

  SectionB2.Append('end;');
  SectionB2.Append('');
end;

procedure TMainForm.AddEquivalence(AOperator, AClassName, ABaseClass: string);
begin
  SectionA1.Append('operator :=(const AQuantity: ' + GetQT(AClassName) + '): ' + GetQT(ABaseClass) + '; inline;');

  SectionB1.Append('operator :=(const AQuantity: ' + GetQT(AClassName) + '): ' + GetQT(ABaseClass) + '; inline;');
  SectionB1.Append('begin');

  if GetQT(AClassName) = 'double' then
    SectionB1.Append('  result.FValue := AQuantity;')
  else
    if GetQT(ABaseClass) = 'double' then
      SectionB1.Append('  result := AQuantity.FValue;')
    else
      SectionB1.Append('  result.FValue := AQuantity.FValue;');

  SectionB1.Append('end;');
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
    WorksheetGrid.LoadFromSpreadsheetFile(OpenDialog.FileName);
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

  for I := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    if (WorksheetGrid.Worksheet.ReadAsText(I, _class_name) <> '') then
    begin
      if Pos('//', WorksheetGrid.Worksheet.ReadAsText(I, _class_name)) = 0 then
        AddClass(
          WorksheetGrid.Worksheet.ReadAsText(I, _class_name       ),
          WorksheetGrid.Worksheet.ReadAsText(I, _operator         ),
          WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_1   ),
          WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_2   ),
          WorksheetGrid.Worksheet.ReadAsText(I, _comment          ),
          WorksheetGrid.Worksheet.ReadAsText(I, _long_symbol      ),
          WorksheetGrid.Worksheet.ReadAsText(I, _short_symbol     ),
          WorksheetGrid.Worksheet.ReadAsText(I, _identifier_symbol),
          WorksheetGrid.Worksheet.ReadAsText(I, _base_class       ),
          WorksheetGrid.Worksheet.ReadAsText(I, _factor           ));
    end;
  end;

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
  CleanDocument(Document);
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
  Document.Destroy;

  PageControl.TabIndex := 1;
end;

end.

