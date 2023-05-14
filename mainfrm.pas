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
    SectionA5: TStringList;
    SectionA6: TStringList;
    SectionA7: TStringList;

    SectionB1: TStringList;
    SectionB2: TStringList;
    SectionB3: TStringList;
    SectionB4: TStringList;
    SectionB5: TStringList;
    SectionB6: TStringList;
    SectionB7: TStringList;

    procedure AddIdentifierOperator(AOperator, ALeftParent, ARightParent, AResult: string);
    procedure AddQuantityOperator(AOperator, ALeftParent, ARightParent, AResult: string);

    procedure AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
      AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor: string);

    procedure AddPower(AOperator, AQuantity, AResult: string);

    procedure AddIdentifierEquivalence(AClassName, AResult: string);
    procedure AddQuantityEquivalence(AClassName, AResult: string);

    procedure AddHelper(AClassParent1, AClassName: string);
  public

  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType;

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
  Result := Result + 'Identifier';
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
    if S[i] = '' then
    begin
      S.Delete(i);
    end else
      Break;
  end;

  while (i + 1) < S.Count do
  begin
    if (S[i] = '') and (S[i + 1] = '') then
    begin
      S.Delete(i + 1);
    end;
    inc(i);
  end;
end;

procedure TMainForm.AddIdentifierOperator(AOperator, ALeftParent, ARightParent, AResult: string);
begin
  if ALeftParent  <> 'double' then ALeftParent  := GetID(ALeftParent);
  if ARightParent <> 'double' then ARightParent := GetID(ARightParent);
  if AResult      <> 'double' then AResult      := GetID(AResult);

  if OperatorList.IndexOf(ALeftParent + AOperator + ARightParent) = -1 then
  begin
    OperatorList.Append(ALeftParent + AOperator + ARightParent);

    SectionA2.Append('operator ' + AOperator + '(const {%H-}ALeft: ' + ALeftParent + '; const {%H-}ARight: ' + ARightParent  + '): ' + AResult + '; inline;');

    SectionB2.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent  + '): ' + AResult + ';');
    SectionB2.Append('begin end;');
    SectionB2.Append('');
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

    SectionA3.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + '; inline;');

    SectionB3.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const ARight: ' + ARightParent + '): ' + AResult  + ';');
    SectionB3.Append('begin');

    if AResult = 'double' then
      S := '  result :='
    else
      S := '  result.Value :=';

    if ALeftParent = 'double' then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.Value ' + AOperator;

    if ARightParent = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.Value;';

    SectionB3.Append(S);
    SectionB3.Append('end;');
    SectionB3.Append('');
  end else
    MessageDlg('Duplicate Operator: ',  ALeftParent + AOperator + ARightParent + '=' + AResult + ' already esists.', mtError, [mbOk], '');
end;

procedure TMainForm.AddClass(const AClassName, AOperator, AClassParent1, AClassParent2,
  AComment, ALongSymbol, AShortSymbol, AIdentifierSymbol, ABaseClass, AFactor: string);
begin
  if ClassList.IndexOf(GetNM(AClassName)) = -1 then
  begin
    ClassList.Append(GetNM(AClassName));

    SectionA1.Add('');
    SectionA1.Add('{ Unit of ' + GetNM(AClassName) + ' }');
    SectionA1.Add('');
    SectionA1.Append('type');

    if (ABaseClass = '') and (AFactor = '') then
    begin
      SectionA1.Append('  ' + GetUN(AClassName) + ' = class(TUnit)');
      SectionA1.Append('    class function Name: string; override;');
      SectionA1.Append('    class function Symbol: string; override;');
      SectionA1.Append('  end;');

      SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TQuantityIdentifier<' + GetUN(AClassName) + '>;');
      SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(AClassName) + '>;');
      SectionA1.Append('');
    end else
    begin
      SectionA1.Append('  ' + GetUN(AClassName) + ' = class(TFactoredUnit)');
      SectionA1.Append('    class function Name: string; override;');
      SectionA1.Append('    class function Symbol: string; override;');
      SectionA1.Append('    class function Factor: double; override;');
      SectionA1.Append('  end;');
      SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TFactoredQuantityIdentifier<' + GetUN(ABaseClass) + ', ' + GetUN(AClassName) + '>;');
    //SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TFactoredQuantity<' + GetUN(ABaseClass) + ', ' + GetUN(AClassName) + '>;');
      SectionA1.Append('');
    end;

    SectionB1.Append('{ Unit of ' + GetUN(AClassName) + ' }');
    SectionB1.Append('');
    SectionB1.Append('class function ' + GetUN(AClassName) + '.Symbol: string;');
    SectionB1.Append('begin');
    SectionB1.Append('  result := ''' + AShortSymbol + ''';');
    SectionB1.Append('end;');
    SectionB1.Append('');
    SectionB1.Append('class function ' + GetUN(AClassName) + '.Name: string;');
    SectionB1.Append('begin');
    SectionB1.Append('  result := ''' + ALongSymbol + ''';');
    SectionB1.Append('end;');
    SectionB1.Append('');

    if (ABaseClass <> '') and (AFactor <> '')  then
    begin
      SectionB1.Append('class function ' + GetUN(AClassName) + '.Factor: double;');
      SectionB1.Append('begin');
      SectionB1.Append('  result := ' + AFactor + ';');
      SectionB1.Append('end;');
      SectionB1.Append('');
    end;

    if AIdentifierSymbol <> '' then
    begin
      SectionA1.Append('var');
      SectionA1.Append('  ' + AIdentifierSymbol + ': ' + GetID(AClassName) + ';');
      SectionA1.Append('');
    end else
    begin
      if (ABaseClass <> '') and (AFactor <> '') then
        if (AClassParent1 <> '') and (AClassParent2 <> '') then
        begin
          SectionA2.Add('');
          SectionA2.Add('// ' + AComment);
          SectionB2.Add('');
          SectionB2.Add('// ' + AComment);
          AddIdentifierOperator('/', AClassParent1, AClassParent2, AClassName);
        end;
    end;
  end;

  if (ABaseClass = '') and (AFactor = '') then
  begin
    if AOperator = '*' then
    begin
      SectionA2.Add('');
      SectionA2.Add('// ' + AComment);
      SectionB2.Add('');
      SectionB2.Add('// ' + AComment);
      AddIdentifierOperator('*', AClassParent1, AClassParent2, AClassName);
      AddIdentifierOperator('/', AClassName,    AClassParent1, AClassParent2);
      if AClassParent1 <> AClassParent2 then
      begin
        AddIdentifierOperator('*', AClassParent2, AClassParent1, AClassName);
        AddIdentifierOperator('/', AClassName,    AClassParent2, AClassParent1);
      end;

      SectionA3.Add('');
      SectionA3.Add('// ' + AComment);
      SectionB3.Add('');
      SectionB3.Add('// ' + AComment);
      AddQuantityOperator('*', AClassParent1, AClassParent2, AClassName);
      AddQuantityOperator('/', AClassName,    AClassParent1, AClassParent2);
      if AClassParent1 <> AClassParent2 then
      begin
        AddQuantityOperator('*', AClassParent2, AClassParent1, AClassName);
        AddQuantityOperator('/', AClassName,     AClassParent2, AClassParent1);
      end;

    end else
      if AOperator = '/' then
      begin
        SectionA2.Add('');
        SectionA2.Add('// ' + AComment);
        SectionB2.Add('');
        SectionB2.Add('// ' + AComment);
        AddIdentifierOperator('/', AClassParent1, AClassParent2, AClassName);
        AddIdentifierOperator('/', AClassParent1, AClassName,    AClassParent2);
        AddIdentifierOperator('*', AClassName,    AClassParent2, AClassParent1);
        AddIdentifierOperator('*', AClassParent2, AClassName,    AClassParent1);

        SectionA3.Add('');
        SectionA3.Add('// ' + AComment);
        SectionB3.Add('');
        SectionB3.Add('// ' + AComment);
        AddQuantityOperator('/', AClassParent1, AClassParent2, AClassName);
        AddQuantityOperator('/', AClassParent1, AClassName,    AClassParent2);
        AddQuantityOperator('*', AClassName,    AClassParent2, AClassParent1);
        AddQuantityOperator('*', AClassParent2, AClassName,    AClassParent1);
      end else
        if Pos('power', LowerCase(AOperator)) > 0 then
        begin
          AddPower(AOperator, AClassParent1, AClassName);
        end else
          if (':=' = LowerCase(AOperator)) then
          begin
            if ('tradian' = LowerCase(AClassName)) then
            begin
              AddIdentifierEquivalence(AClassParent1, AClassName);
            //AddIdentifierEquivalence(AClassName, AClassParent1);
            end;
            AddQuantityEquivalence(AClassParent1, AClassName);
          //AddQuantityEquivalence(AClassName, AClassParent1);
          end else
          if ('helper' = LowerCase(AOperator)) then
          begin
            AddHelper(AClassParent1, AClassName);
          end;
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

  SectionA4.Add('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionA4.Add('function ' + S1 + 'Root(AQuantity: ' +  AResult + '): ' + AQuantity + ';');

  SectionB4.Add('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionB4.Add('begin');
  SectionB4.Add('  result.Value := Power(AQuantity.Value, ' + S2 + ');');
  SectionB4.Add('end;');
  SectionB4.Add('');

  SectionB4.Add('function ' + S1 + 'Root(AQuantity: ' + AResult + '): ' + AQuantity + ';');
  SectionB4.Add('begin');
  SectionB4.Add('  result.Value := Power(AQuantity.Value, ' + S3 + ');');
  SectionB4.Add('end;');
  SectionB4.Add('');
end;

procedure TMainForm.AddIdentifierEquivalence(AClassName, AResult: string);
begin
  AClassName := GetQT(AClassName);
  AResult    := GetQT(AResult);

  if AClassName <> 'double' then
    AClassName := AClassName + 'Identifier';

  if AResult <> 'double' then
    AResult := AResult + 'Identifier';

  if OperatorList.IndexOf('operator := (AQuantity: ' + AClassName + '):' + AResult + ';') = -1 then
  begin
    OperatorList.Add('operator := (AQuantity: ' + AClassName + '):' + AResult + ';');

    SectionA5.Add('operator := ({%H-}AQuantity: ' + AClassName + '): ' + AResult + '; inline;');
    SectionB5.Add('operator := (AQuantity: ' + AClassName + '): ' + AResult + ';');

    if AResult = 'double' then
    begin
      SectionB5.Add('begin');
      SectionB5.Add('  result := 1;');
      SectionB5.Add('end;');
    end else
      SectionB5.Add('begin end;');

    SectionB5.Add('');
  end else
    MessageDlg('Duplicate Operator: ',  'Operator := (AQuantity: ' + AClassName + '):' + AResult + '; already esists.', mtError, [mbOk], '');
end;

procedure TMainForm.AddQuantityEquivalence(AClassName, AResult: string);
var
  S: string;
begin
  AClassName := GetQT(AClassName);
  AResult    := GetQT(AResult);

  if OperatorList.IndexOf('operator := (AQuantity: ' + AClassName + '):' + AResult + ';') = -1 then
  begin
    OperatorList.Add('operator := (AQuantity: ' + AClassName + '):' + AResult + ';');

    SectionA5.Add('operator := (AQuantity: ' + AClassName + '): ' + AResult + '; inline;');
    SectionB5.Add('operator := (AQuantity: ' + AClassName + '): ' + AResult + ';');
    SectionB5.Add('begin');

    S := '';
    if AResult = 'double' then
      S := '  result := '
    else
      S := '  result.Value := ';

    if AClassName = 'double' then
      S := S + 'AQuantity;'
    else
      S := S + 'AQuantity.Value;';

    SectionB5.Add(S);
    SectionB5.Add('end;');
    SectionB5.Add('');
  end else
    MessageDlg('Duplicate Operator: ',  'Operator := (AQuantity: ' + AClassName + '):' + AResult + '; already esists.', mtError, [mbOk], '');
end;

procedure TMainForm.AddHelper(AClassParent1, AClassName: string);
begin
  SectionA7.Add('{ Helper for ' + GetNM(AClassName) + ' }');
  SectionA7.Add('');
  SectionA7.Append('type');
  SectionA7.Append('  ' + GetUH(AClassName) + ' = record helper for ' + GetID(AClassName));
  SectionA7.Append('    function From(const AQuantity: ' + GetQT(AClassParent1) + '): ' + GetQT(AClassName) + ';');
  SectionA7.Append('  end;');
  SectionA7.Add('');

  SectionB7.Append('{ Helper for ' + GetNM(AClassName) + ' }');
  SectionB7.Add('');
  SectionB7.Append('function ' + GetUH(AClassName) + '.From(const AQuantity: ' + GetQT(AClassParent1) + '): ' + GetQT(AClassName) + ';');
  SectionB7.Append('begin');
  SectionB7.Append('  result.Value := AQuantity.Value;');
  SectionB7.Append('end;');
  SectionB7.Append('');
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
  SectionA5    := TStringList.Create;
  SectionA6    := TStringList.Create;
  SectionA7    := TStringList.Create;

  SectionB1    := TStringList.Create;
  SectionB2    := TStringList.Create;
  SectionB3    := TStringList.Create;
  SectionB4    := TStringList.Create;
  SectionB5    := TStringList.Create;
  SectionB6    := TStringList.Create;
  SectionB7    := TStringList.Create;

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

  SectionA2.Add('{ Combining units }');
  SectionA2.Add('');
  SectionB2.Add('{ Combining units }');
  SectionB2.Add('');

  SectionA3.Add('');
  SectionA3.Add('{ Combining quantities }');
  SectionA3.Add('');
  SectionB3.Add('{ Combining quantities }');
  SectionB3.Add('');

  SectionA4.Add('');
  SectionA4.Add('{ Power units }');
  SectionA4.Add('');
  SectionB4.Add('{ Power quantities }');
  SectionB4.Add('');

  SectionA5.Add('');
  SectionA5.Add('{ Equivalences }');
  SectionA5.Add('');
  SectionB5.Add('{ Equivalences }');
  SectionB5.Add('');

  if TrigonometricCheckBox.Checked then
  begin
    Stream := TResourceStream.Create(HInstance, 'SECTION-A6', RT_RCDATA);
    SectionA6.LoadFromStream(Stream);
    SectionA6.Insert(0, '');
    Stream.Destroy;

    Stream := TResourceStream.Create(HInstance, 'SECTION-B6', RT_RCDATA);
    SectionB6.LoadFromStream(Stream);
    SectionB6.Insert(0, '');
    Stream.Destroy;
  end;

  SectionA7.Add('');
  SectionA7.Add('{ Helpers }');
  SectionA7.Add('');
  SectionB7.Add('{ Helpers }');
  SectionB7.Add('');

  for I := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    if (WorksheetGrid.Worksheet.ReadAsText(I, _class_name) <> '') and
       (WorksheetGrid.Worksheet.ReadAsText(I, _base_class)  = '') and
       (WorksheetGrid.Worksheet.ReadAsText(I, _factor    )  = '') then
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

  for I := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    if (WorksheetGrid.Worksheet.ReadAsText(I, _class_name) <> '') and
       (WorksheetGrid.Worksheet.ReadAsText(I, _base_class) <> '') and
       (WorksheetGrid.Worksheet.ReadAsText(I, _factor    ) <> '') then
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
  for I := 0 to SectionA5.Count -1 do Document.Append(SectionA5[I]);
  for I := 0 to SectionA6.Count -1 do Document.Append(SectionA6[I]);
  for I := 0 to SectionA7.Count -1 do Document.Append(SectionA7[I]);

  for I := 0 to SectionB1.Count -1 do Document.Append(SectionB1[I]);
  for I := 0 to SectionB2.Count -1 do Document.Append(SectionB2[I]);
  for I := 0 to SectionB3.Count -1 do Document.Append(SectionB3[I]);
  for I := 0 to SectionB4.Count -1 do Document.Append(SectionB4[I]);
  for I := 0 to SectionB5.Count -1 do Document.Append(SectionB5[I]);
  for I := 0 to SectionB6.Count -1 do Document.Append(SectionB6[I]);
  for I := 0 to SectionB7.Count -1 do Document.Append(SectionB7[I]);
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

  SectionB7.Destroy;
  SectionB6.Destroy;
  SectionB5.Destroy;
  SectionB4.Destroy;
  SectionB3.Destroy;
  SectionB2.Destroy;
  SectionB1.Destroy;

  SectionA7.Destroy;
  SectionA6.Destroy;
  SectionA5.Destroy;
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

