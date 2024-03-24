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
  SynHighlighterPas, SynEdit, ToolKitUnit;

type
  { TMainForm }

  TMainForm = class(TForm)
    SkipVectorialUnits: TCheckBox;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure OnTerminate(Sender: TObject);
    procedure OnMessage(const AMessage: string);
    procedure UpdateButton(Value: boolean);
  private
  public
  end;

  TToolKitManager = class(TThread)
  private
    FList: TToolkitList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;


var
  MainForm: TMainForm;
  ToolKitManager: TToolKitManager = nil;

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
  _vectorclass       = 11;
  _vectorclass1      = 12;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex          := 0;
  WindowState                   := wsMaximized;
  WorksheetGrid.AutoFillColumns := True;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ToolKitManager = nil;
  if not CanClose then
  begin
    if MessageDlg('The optimizer is still running', 'Do you want to terminate it?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ToolKitManager.FList.ExecutionTime := 0;
    end;
  end;
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
  T: TToolkitItem;
begin
  Memo.Clear;
  UpdateButton(False);
  ToolKitManager := TToolKitManager.Create;
  ToolKitManager.OnTerminate := @OnTerminate;
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
    T.FVecClass         := WorksheetGrid.Worksheet.ReadAsText(i, _vectorclass);
    T.FOptions          := WorksheetGrid.Worksheet.ReadAsText(i, _vectorclass1);

    T.FLongSymbol       := CleanUnitName(T.FLongSymbol);
    T.FShortSymbol      := CleanUnitSymbol(T.FShortSymbol);
    if (T.FClassName <> '') and (Pos('//', T.FClassName) = 0) then
    begin
      ToolKitManager.FList.Add(T);
    end;
  end;
  ToolKitManager.Start;
end;

procedure TMainForm.OnTerminate(Sender: TObject);
var
  I: longint;
begin
  UpdateButton(True);
  SynEdit.BeginUpdate(True);
  SynEdit.Lines.Clear;
  with ToolKitManager.FList do
  begin
    for I := 0 to Document.Count - 1 do SynEdit.Append(Document[I]);
    for I := 0 to Messages.Count - 1 do Memo.Lines.Add(Messages[I]);
  end;
  SynEdit.EndUpdate;
  ToolKitManager := nil;
end;

procedure TMainForm.UpdateButton(Value: boolean);
begin
  SynEdit.Clear;
  LoadBtn.Enabled          := Value;
  OptimizeBox.Enabled      := Value;
  OptimizationTime.Enabled := Value;
  ExportBtn.Enabled        := Value;
  RunBtn.Enabled           := Value;
  case Value of
    True:  PageControl.TabIndex := 1;
    False: PageControl.TabIndex := 2;
  end;
end;

procedure TMainForm.OnMessage(const AMessage: string);
begin
  Memo.Append(AMessage);
end;

{ TToolKitThread }

constructor TToolKitManager.Create;
begin
  FList := TToolkitList.Create(@MainForm.OnMessage);
  FList.SkipVectorialUnits := Mainform.SkipVectorialUnits.Checked;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TToolKitManager.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TToolKitManager.Execute;
begin
  FList.ExecutionTime := 0;
  if MainForm.OptimizeBox.Checked then
    FList.ExecutionTime := MainForm.OptimizationTime.Value;

  FList.InitialTemperature := 1000000;
  FList.CoolingRate        := 0.1;
  FList.Run;
end;

end.

