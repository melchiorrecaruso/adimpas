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
  Classes, SysUtils, fpspreadsheetgrid, fpspreadsheetctrls, fpsallformats,
  Forms, Controls, Graphics, Dialogs, Grids, Buttons, ComCtrls, StdCtrls, Spin,
  SynHighlighterPas, SynEdit, toolkitunit;

type
  { TMainForm }

  TMainForm = class(TForm)
    Memo: TMemo;
    OptimizationTime: TSpinEdit;
    TabSheet3: TTabSheet;
    WorkbookSource: TsWorkbookSource;
    WorksheetGrid: TsWorksheetGrid;
    OptimizeBox: TCheckBox;
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
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  Common, SimulatedAnnealing;

{$R *.lfm}

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
  i: longint;
  Document: TStringList;
  Messages: TStringList;
  List: TToolkitList;
  T: TToolkitItem;
begin
  Memo.Clear;
  SynEdit.Clear;
  List := TToolKitList.Create;
  for i := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    T.FClassName        := WorksheetGrid.Worksheet.ReadAsText(i, _class_name);
    T.FOperator         := WorksheetGrid.Worksheet.ReadAsText(i, _operator);
    T.FClassParent1     := WorksheetGrid.Worksheet.ReadAsText(i, _class_parent_1);
    T.FClassParent2     := WorksheetGrid.Worksheet.ReadAsText(i, _class_parent_2);
    T.FComment          := WorksheetGrid.Worksheet.ReadAsText(i, _comment);
    T.FLongSymbol       := WorksheetGrid.Worksheet.ReadAsText(i, _long_symbol);
    T.FShortSymbol      := WorksheetGrid.Worksheet.ReadAsText(i, _short_symbol);
    T.FIdentifierSymbol := WorksheetGrid.Worksheet.ReadAsText(i, _identifier_symbol);
    T.FBaseClass        := WorksheetGrid.Worksheet.ReadAsText(i, _base_class);
    T.FFactor           := WorksheetGrid.Worksheet.ReadAsText(i, _factor);
    T.FPrefixes         := WorksheetGrid.Worksheet.ReadAsText(i, _prefixes);
    T.FLongSymbol       := CleanUnitName(T.FLongSymbol);
    T.FShortSymbol      := CleanUnitSymbol(T.FShortSymbol);
    if (T.FClassName <> '') and (Pos('//', T.FClassName) = 0) then
    begin
      List.Add(T);
    end;
  end;

  List.ExecutionTime      := OptimizationTime.Value;
  List.InitialTemperature := 1000000;
  List.CoolingRate        := 0.5;

  Document := TStringList.Create;
  Messages := TStringList.Create;
  case OptimizeBox.Checked of
    True:  List.RunWithOptimizer(Document, Messages);
    False: List.Run(Document, Messages, nil);
  end;
  SynEdit.BeginUpdate(True);
  SynEdit.Lines.Clear;
  for I := 0 to Document.Count - 1 do SynEdit.Append(Document[I]);
  for I := 0 to Messages.Count - 1 do Memo.Lines.Add(Messages[I]);
  SynEdit.EndUpdate;
  Messages.Destroy;
  Document.Destroy;
  List.Destroy;

  PageControl.TabIndex := 1;
end;

end.

