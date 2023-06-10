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

    procedure AddPower(AOperator, AQuantity, AResult: string);
    procedure AddHelper(AClassParent1, AClassName: string);
  public

  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, StrUtils;

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
  Result := Result + 'Id';
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
    if IsEmptyStr(S[i], [' ']) then
    begin
      S.Delete(i);
    end else
      Break;
  end;

  while (i + 1) < S.Count do
  begin
    if IsEmptyStr(S[i], [' ']) and IsEmptyStr(S[i + 1], [' ']) then
    begin
      S.Delete(i + 1);
    end;
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
      S := '  result.Value :=';

    if ALeftParent = 'double' then
      S := S + ' ALeft ' + AOperator
    else
      S := S + ' ALeft.Value ' + AOperator;

    if ARightParent = 'double' then
      S := S + ' ARight;'
    else
      S := S + ' ARight.Value;';

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

    SectionA1.Add('');
    SectionA1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const {%H-}ARight: ' + ARightParent  + '): ' + AResult + '; inline;');
    SectionA1.Add('');

    SectionB1.Append('');
    SectionB1.Append('operator ' + AOperator + '(const ALeft: ' + ALeftParent + '; const {%H-}ARight: ' + ARightParent  + '): ' + AResult + ';');
    SectionB1.Append('begin');
    if ALeftParent <> 'double' then
      SectionB1.Append('  result.Value := ALeft.Value;')
    else
      SectionB1.Append('  result.Value := ALeft;');
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

    SectionA1.Append('');
    SectionA1.Append('type');
    SectionA1.Append('  { Unit of ' + GetNM(AClassName) + ' }');
    if (ABaseClass = '') then
    begin
      SectionA1.Append('  ' + GetUN(AClassName) + ' = record');
      SectionA1.Append('    const Symbol = ''' + AShortSymbol + ''';');
      SectionA1.Append('    const Name   = ''' + ALongSymbol + ''';');
      SectionA1.Append('  end;');
      SectionA1.Append('  ' + GetQT(AClassName) + ' = specialize TQuantity<' + GetUN(AClassName) + '>;');
      SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TUnitId<' + GetUN(AClassName) + '>;');
      SectionA1.Append('');
    end else
    begin
      SectionA1.Append('  ' + GetUN(AClassName) + ' = record');
      SectionA1.Append('    const Symbol = ''' + AShortSymbol + ''';');
      SectionA1.Append('    const Name   = ''' + ALongSymbol  + ''';');
      SectionA1.Append('    const Factor = '   + AFactor      +   ';');
      //SectionA1.Append('    class function ToString(AQuantity: ' + GetQT(ABaseClass) + '): string; static;');
      //SectionA1.Append('    class function ToVerboseString(AQuantity: ' + GetQT(ABaseClass) + '): string; static;');
      SectionA1.Append('  end;');
      SectionA1.Append('  ' + GetID(AClassName) + ' = specialize TFactoredUnitId<' + GetUN(ABaseClass) + ', ' + GetUN(AClassName) + '>;');
      SectionA1.Append('');
      (*
      SectionA1.Append('  { Unit of ' + GetNM(AClassName) + ' }');
      SectionA1.Append('');
      SectionA1.Append('  function ' + GetNM(AClassName) + 'ToString(AQuantity: ' + GetQT(ABaseClass) + '): string;');
      SectionA1.Append('  function ' + GetNM(AClassName) + 'ToVerboseString(AQuantity: ' + GetQT(ABaseClass) + '): string;');


      SectionB1.Append('{ Unit of ' + GetNM(AClassName) + ' }');
      SectionB1.Append('');
      SectionB1.Append('function ' + GetNM(AClassName) + 'ToString(AQuantity: ' + GetQT(ABaseClass) + '): string;');
      SectionB1.Append('begin  result := FloatToStr(AQuantity.Value) + ''' + AShortSymbol + '''; end;');

      SectionB1.Append('');
      SectionB1.Append('function ' + GetNM(AClassName) + 'ToVerboseString(AQuantity: ' + GetQT(ABaseClass) + '): string;');
      SectionB1.Append('begin  result := FloatToStr(AQuantity.Value) + ''' + ALongSymbol + '''; end;');
      SectionB1.Append('');
      *)
    end;
  end;

  if (AIdentifierSymbol <> '') then
  begin
    if (ABaseClass = '') then
    begin
      SectionA1.Append('');
      SectionA1.Append('var');
      SectionA1.Append(Format('  %s: %s;', [AIdentifierSymbol, GetID(AClassName)]));
      SectionA1.Append('');
    end else
    begin
      SectionA1.Append('');
      SectionA1.Append('const');
      SectionA1.Append(Format('  %s: specialize TQuantity<%s> = (Value: %s);', [AIdentifierSymbol, GetUN(ABaseClass), GetUN(AClassName) + '.Factor']));
      (*
      SectionA1.Append(Format('  function %s: %s;', [AIdentifierSymbol, GetQT(ABaseClass)]));
      SectionA1.Append('');

      SectionB1.Append(Format('function %s: %s;', [AIdentifierSymbol, GetQT(ABaseClass)]));
      SectionB1.Append('begin result.Value := ' + AFactor + ' end;');
      SectionB1.Append('');
      *)
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
      AddQuantityOperator('/', AClassName,    AClassParent1, AClassParent2);
      if AClassParent1 <> AClassParent2 then
      begin
        AddQuantityOperator('*', AClassParent2, AClassParent1, AClassName);
        AddQuantityOperator('/', AClassName,     AClassParent2, AClassParent1);
      end;
      if AFactor = '' then
        AddUnitIdOperator('*', AClassParent1, AClassParent2, AClassName);

    end else
      if AOperator = '/' then
      begin
        SectionA1.Append('');
        SectionA1.Append('// ' + AComment);
        SectionB1.Append('');
        SectionB1.Append('// ' + AComment);

        AddQuantityOperator('/', AClassParent1, AClassParent2, AClassName);
        AddQuantityOperator('/', AClassParent1, AClassName,    AClassParent2);
        AddQuantityOperator('*', AClassName,    AClassParent2, AClassParent1);
        AddQuantityOperator('*', AClassParent2, AClassName,    AClassParent1);

        if AFactor = '' then
          AddUnitIdOperator('/', AClassParent1, AClassParent2, AClassName);

      end else
        if Pos('power', LowerCase(AOperator)) > 0 then
        begin
          AddPower(AOperator, AClassParent1, AClassName);
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

  SectionA3.Add('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionA3.Add('function ' + S1 + 'Root(AQuantity: ' +  AResult + '): ' + AQuantity + ';');

  SectionB3.Add('function ' + S1 + 'Power(AQuantity: ' + AQuantity + '): ' + AResult + ';');
  SectionB3.Add('begin');
  SectionB3.Add('  result.Value := Power(AQuantity.Value, ' + S2 + ');');
  SectionB3.Add('end;');
  SectionB3.Add('');

  SectionB3.Add('function ' + S1 + 'Root(AQuantity: ' + AResult + '): ' + AQuantity + ';');
  SectionB3.Add('begin');
  SectionB3.Add('  result.Value := Power(AQuantity.Value, ' + S3 + ');');
  SectionB3.Add('end;');
  SectionB3.Add('');
end;

procedure TMainForm.AddHelper(AClassParent1, AClassName: string);
begin
  SectionA2.Add('');
  SectionA2.Append('type');
  SectionA2.Append('  ' + GetUH(AClassName) + ' = record helper for ' + GetID(AClassName));
  SectionA2.Append('    function From(const AQuantity: ' + GetQT(AClassParent1) + '): ' + GetQT(AClassName) + ';');
  SectionA2.Append('  end;');
  SectionA2.Add('');

  SectionB2.Add('');
  SectionB2.Append('function ' + GetUH(AClassName) + '.From(const AQuantity: ' + GetQT(AClassParent1) + '): ' + GetQT(AClassName) + ';');
  SectionB2.Append('begin');
  SectionB2.Append('  result.Value := AQuantity.Value;');
  SectionB2.Append('end;');
  SectionB2.Append('');
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

  SectionA2.Add('');
  SectionA2.Add('{ Helpers }');
  SectionA2.Add('');
  SectionB2.Add('');
  SectionB2.Add('{ Helpers }');
  SectionB2.Add('');

  SectionA3.Add('');
  SectionA3.Add('{ Power units }');
  SectionA3.Add('');
  SectionB3.Add('');
  SectionB3.Add('{ Power quantities }');
  SectionB3.Add('');

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
     //(WorksheetGrid.Worksheet.ReadAsText(I, _base_class)  = '') and
     //(WorksheetGrid.Worksheet.ReadAsText(I, _factor    )  = '') then
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
  (*
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
  *)

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
  Document.Destroy;

  PageControl.TabIndex := 1;
end;

end.

