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
  SynEdit, Common;

type
  TDBItem = record
    Exponents: array [1..7] of longint;
    Name: string;
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
    ExternalOperators: longint;
    InternalOperators: longint;
    OperatorList: TStringList;

    FList: TToolkitList;

    SectionA0: TStringList;
    SectionA1: TStringList;
    SectionA2: TStringList;
    SectionA3: TStringList;
    SectionA4: TStringList;
    SectionA5: TStringList;
    SectionA6: TStringList;
    SectionA7: TStringList;
    SectionA8: TStringList;
    SectionA9: TStringList;
    SectionA10: TStringList;

    SectionB1: TStringList;
    SectionB2: TStringList;
    SectionB3: TStringList;
    SectionB4: TStringList;
    SectionB5: TStringList;
    SectionB6: TStringList;
    SectionB7: TStringList;
    SectionB8: TStringList;
    SectionB9: TStringList;
    SectionB10: TStringList;

    procedure AddQuantityOperator(AOperator, ALeftParent, ARightParent, AResult: string);
    procedure AddUnitIdOperator(AOperator, ALeftParent, ARightParent, AResult: string);

    procedure AddClass(const AItem: TToolkitItem; AddOperator: boolean);

    procedure AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassName, ABaseClass, AFactor: string);
    procedure AddEquivalence(AClassName, ABaseClass: string);

    procedure CheckClass(AClassName, AOperator, AClassParent1, AClassParent2: string);
    function GetIndex(const AClassName: string): longint;
    function GetSIunit(Index: longint): string;
    function Find(const S: string; List: TStringList): longint;
  public

  end;

const
  INTF_FACTORED = '{$DEFINE INTF_FACTORED}{$DEFINE TQuantity:=%s}{$i adim.inc}';
  INTF_QUANTITY = '{$DEFINE INTF_QUANTITY}{$DEFINE TQuantity:=%s}{$i adim.inc}';
  INTF_UNIT     = '{$DEFINE INTF_UNIT}{$DEFINE TQuantity:=%s}{$DEFINE TUnit:=%s}{$i adim.inc}';
  INTF_END      = '{$DEFINE INTF_END}{$i adim.inc}';

  IMPL_FACTORED = '{$DEFINE IMPL_FACTORED}{$DEFINE TQuantity:=%s}{$i adim.inc}';
  IMPL_QUANTITY = '{$DEFINE IMPL_QUANTITY}{$DEFINE TQuantity:=%s}{$i adim.inc}';
  IMPL_UNIT     = '{$DEFINE IMPL_UNIT}{$DEFINE TQuantity:=%s}{$DEFINE TUnit:=%s}{$i adim.inc}';

  INTF_OP_CLASS = '  class operator %s(const ALeft: %s; const ARight: %s): %s;';
  IMPL_OP_CLASS = 'class operator %s.%s(const ALeft: %s; const ARight: %s): %s;';
  INTF_OP       = 'operator %s(const ALeft: %s; const ARight: %s): %s;';
  IMPL_OP       = 'operator %s(const ALeft: %s; const ARight: %s): %s;';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, Math, SimulatedAnnealing, StrUtils;

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

procedure TMainForm.AddQuantityOperator(AOperator, ALeftParent, ARightParent, AResult: string);
var
  i, iL, iR, iX: longint;
  ABaseClass: string;
  S : string;
begin
  iL := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(ALeftParent )]), SectionA2);
  iR := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(ARightParent)]), SectionA2);
  iX := Find(Format(INTF_QUANTITY, [GetUnitQuantityType(AResult     )]), SectionA2);

  ABaseClass := '';
  if (iL > -1) or
     (iR > -1) then
  begin
    i := Max(iL, iR);
    if i = iL then ABaseClass := ALeftParent;
    if i = iR then ABaseClass := ARightParent;
    if i < iX then ABaseClass := '';

    if ABaseClass <> '' then
    begin
      // if (i = iL) and (GetUnitQuantityType(ALeftParent ) = 'THertzQty') then ABaseClass := '';
      // if (i = iR) and (GetUnitQuantityType(ARightParent) = 'THertzQty') then ABaseClass := '';
    end;
  end;

  if ABaseClass   <> 'double' then ABaseClass   := GetUnitQuantityType(ABaseClass);
  if ALeftParent  <> 'double' then ALeftParent  := GetUnitQuantityType(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetUnitQuantityType(ARightParent);
  if AResult      <> 'double' then AResult      := GetUnitQuantityType(AResult);

  if OperatorList.IndexOf(ABaseClass + '.' + ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ABaseClass + '.' + ALeftParent + AOperator + ARightParent);

    if ABaseClass = '' then
    begin
      SectionA2.Append(Format(INTF_OP, [AOperator, ALeftParent, ARightParent, AResult]));
      SectionB2.Append(Format(IMPL_OP, [AOperator, ALeftParent, ARightParent, AResult]));
      Inc(ExternalOperators);
    end else
    begin
      SectionA2.Insert(i + 1, Format(INTF_OP_CLASS, [            AOperator, ALeftParent, ARightParent, AResult]));
      SectionB2.Append(       Format(IMPL_OP_CLASS, [ABaseClass, AOperator, ALeftParent, ARightParent, AResult]));
      Inc(InternalOperators);
    end;

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

    SectionB2.Append('begin');
    SectionB2.Append(S);
    SectionB2.Append('end;');
    SectionB2.Append('');
  end else
    Memo.Append('ERROR: operator ' + AOperator + '(' + ALeftParent + '; ' + ARightParent + ') : ' + AResult + '; already esists.');
end;

procedure TMainForm.AddUnitIdOperator(AOperator, ALeftParent, ARightParent, AResult: string);
var
  i: longint;
  ABaseClass: string;
begin
  i := Find(Format(INTF_UNIT,[GetUnitQuantityType(ARightParent), GetUnitIdentifier(ARightParent)]), SectionA3);

  ABaseClass := ARightParent;
  if ALeftParent  <> 'double' then ALeftParent  := GetUnitQuantityType(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetUnitIdentifier(ARightParent);
  if AResult      <> 'double' then AResult      := GetUnitQuantityType(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA3.Insert(i + 1, Format(INTF_OP_CLASS, [                               AOperator, ALeftParent, ARightParent, AResult]));
    SectionB3.Append('');
    SectionB3.Append(       Format(IMPL_OP_CLASS, [GetUnitIdentifier(ABaseClass), AOperator, ALeftParent, ARightParent, AResult]));

    SectionB3.Append('begin');
    if AResult <> 'double' then
    begin
      if ALeftParent <> 'double' then
        SectionB3.Append('  result.FValue := ALeft.FValue;')
      else
        SectionB3.Append('  result.FValue := ALeft;');
    end else
    begin
      if ALeftParent <> 'double' then
        SectionB3.Append('  result := ALeft.FValue;')
      else
        SectionB3.Append('  result := ALeft;');
    end;
    SectionB3.Append('end;');
    SectionB3.Append('');
    Inc(InternalOperators);
  end;
end;

procedure TMainForm.AddClass(const AItem: TToolkitItem; AddOperator: boolean);
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
      end;

end;

procedure TMainForm.AddFactoredQuantity(ABaseClass, AIdentifierSymbol, AFactor, APrefixes: string);
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

procedure TMainForm.AddHelper(AClassName, ABaseClass, AFactor: string);
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

procedure TMainForm.AddEquivalence(AClassName, ABaseClass: string);
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
  Inc(InternalOperators);
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
  I, J: longint;
  BestSolution: TSolution;
  Document: TStringList;
  Stream: TResourceStream;
  T: TToolkitItem;
begin
  Document     := TSTringList.Create;
  ClassList    := TStringList.Create;
  CheckList    := nil;
  FList        := TToolkitList.Create;

  OperatorList := TStringList.Create;
  SectionA0    := TStringList.Create;
  SectionA1    := TStringList.Create;
  SectionA2    := TStringList.Create;
  SectionA3    := TStringList.Create;
  SectionA4    := TStringList.Create;
  SectionA5    := TStringList.Create;
  SectionA6    := TStringList.Create;
  SectionA7    := TStringList.Create;
  SectionA8    := TStringList.Create;
  SectionA9    := TStringList.Create;
  SectionA10   := TStringList.Create;

  SectionB1    := TStringList.Create;
  SectionB2    := TStringList.Create;
  SectionB3    := TStringList.Create;
  SectionB4    := TStringList.Create;
  SectionB5    := TStringList.Create;
  SectionB6    := TStringList.Create;
  SectionB7    := TStringList.Create;
  SectionB8    := TStringList.Create;
  SectionB9    := TStringList.Create;
  SectionB10   := TStringList.Create;

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

  if TrigonometricCheckBox.Checked then
  begin
    Stream := TResourceStream.Create(HInstance, 'SECTION-A4', RT_RCDATA);
    SectionA10.LoadFromStream(Stream);
    SectionA10.Insert(0, '');
    Stream.Destroy;

    Stream := TResourceStream.Create(HInstance, 'SECTION-B4', RT_RCDATA);
    SectionB10.LoadFromStream(Stream);
    SectionB10.Insert(0, '');
    Stream.Destroy;
  end;

  Memo.Clear;
  BaseUnitCount     := 0;
  FactoredUnitCount := 0;
  ExternalOperators := 0;
  InternalOperators := 0;
  for I := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    T.FClassName        := WorksheetGrid.Worksheet.ReadAsText(I, _class_name);
    T.FOperator         := WorksheetGrid.Worksheet.ReadAsText(I, _operator);
    T.FClassParent1     := WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_1);
    T.FClassParent2     := WorksheetGrid.Worksheet.ReadAsText(I, _class_parent_2);
    T.FComment          := WorksheetGrid.Worksheet.ReadAsText(I, _comment);
    T.FLongSymbol       := WorksheetGrid.Worksheet.ReadAsText(I, _long_symbol);
    T.FShortSymbol      := WorksheetGrid.Worksheet.ReadAsText(I, _short_symbol);
    T.FIdentifierSymbol := WorksheetGrid.Worksheet.ReadAsText(I, _identifier_symbol);
    T.FBaseClass        := WorksheetGrid.Worksheet.ReadAsText(I, _base_class);
    T.FFactor           := WorksheetGrid.Worksheet.ReadAsText(I, _factor);
    T.FPrefixes         := WorksheetGrid.Worksheet.ReadAsText(I, _prefixes);
    T.FLongSymbol       := CleanUnitName(T.FLongSymbol);
    T.FShortSymbol      := CleanUnitSymbol(T.FShortSymbol);

    if (T.FClassName <> '') and (Pos('//', T.FClassName) = 0) then
    begin
      FList.Add(T);
    end;
  end;

  (*
  FList.Optimize(BestSolution);
  for i := Low(BestSolution) to High(BestSolution) do
  begin
    for j := 0 to FList.Count -1 do
    begin
      if FList[j].FClassName = BestSolution[i] then
      begin
        AddClass(FList[j], False);
        Break;
      end;
    end;
  end;
  BestSolution := nil;
  *)
  for I := 0 to FList.Count -1 do AddClass(FList[I], False);
  for I := 0 to FList.Count -1 do AddClass(FList[I], True);

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

  for I := 0 to SectionA0 .Count -1 do Document.Append(SectionA0 [I]);
  for I := 0 to SectionA1 .Count -1 do Document.Append(SectionA1 [I]);
  for I := 0 to SectionA2 .Count -1 do Document.Append(SectionA2 [I]);
  for I := 0 to SectionA3 .Count -1 do Document.Append(SectionA3 [I]);
  for I := 0 to SectionA4 .Count -1 do Document.Append(SectionA4 [I]);
  for I := 0 to SectionA5 .Count -1 do Document.Append(SectionA5 [I]);
  for I := 0 to SectionA6 .Count -1 do Document.Append(SectionA6 [I]);
  for I := 0 to SectionA7 .Count -1 do Document.Append(SectionA7 [I]);
  for I := 0 to SectionA8 .Count -1 do Document.Append(SectionA8 [I]);
  for I := 0 to SectionA9 .Count -1 do Document.Append(SectionA9 [I]);
  for I := 0 to SectionA10.Count -1 do Document.Append(SectionA10[I]);

  for I := 0 to SectionB1 .Count -1 do Document.Append(SectionB1 [I]);
  for I := 0 to SectionB2 .Count -1 do Document.Append(SectionB2 [I]);
  for I := 0 to SectionB3 .Count -1 do Document.Append(SectionB3 [I]);
  for I := 0 to SectionB4 .Count -1 do Document.Append(SectionB4 [I]);
  for I := 0 to SectionB5 .Count -1 do Document.Append(SectionB5 [I]);
  for I := 0 to SectionB6 .Count -1 do Document.Append(SectionB6 [I]);
  for I := 0 to SectionB7 .Count -1 do Document.Append(SectionB7 [I]);
  for I := 0 to SectionB8 .Count -1 do Document.Append(SectionB8 [I]);
  for I := 0 to SectionB9 .Count -1 do Document.Append(SectionB9 [I]);
  for I := 0 to SectionB10.Count -1 do Document.Append(SectionB10[I]);

  Document.Append('');
  CleanDocument(Document);

  SynEdit.BeginUpdate(True);
  SynEdit.Lines.Clear;
  for I := 0 to Document.Count - 1 do
  begin
    SynEdit.Append(Document[I]);
  end;
  SynEdit.EndUpdate;

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

  FList.Destroy;
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

function TMainForm.Find(const S: string; List: TStringList): longint;
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

