{
  Description: MainForm.

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
    UseFuncInsteadOfOperators: TCheckBox;
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
    procedure UpdateButton(Value: boolean);
    procedure DoMessage;
  private
  public
  end;

  TToolKitThread = class(TThread)
  private
    FList: TToolKitBuilder;
    FMessage: string;
    procedure OnMessage(const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;


var
  MainForm: TMainForm;
  ToolKitThread: TToolKitThread = nil;

implementation

uses
  Common, SimulatedAnnealing;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex          := 0;
  WindowState                   := wsNormal;
  WorksheetGrid.AutoFillColumns := True;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ToolKitThread = nil;
  if not CanClose then
  begin
    if MessageDlg('The optimizer is still running', 'Do you want to terminate it?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ToolKitThread.FList.ExecutionTime := 0;
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
  ToolKitThread := TToolKitThread.Create;
  ToolKitThread.OnTerminate := @OnTerminate;
  ToolKitThread.FList.UseFuncInsteadOfOperators := UseFuncInsteadOfOperators.Checked;
  for i := 0 to WorksheetGrid.Worksheet.GetLastRowIndex do
  begin
    T.FClassName        := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 00));
    T.FOperator         := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 01));
    T.FClassParent1     := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 02));
    T.FClassParent2     := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 03));
    T.FLongSymbol       := CleanDoubleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 04));
    T.FShortSymbol      := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 05));
    T.FIdentifierSymbol := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 06));
    T.FBaseClass        := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 07));
    T.FFactor           := CleanDoubleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 08));
    T.FPrefixes         := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 09));
    T.FClassType        := CleanSingleSpaces(WorksheetGrid.Worksheet.ReadAsText(i, 10));

    if (T.FClassName <> '') and (T.FClassName[1] <> '/') then
    begin
      if SkipVectorialUnits.Checked then
      begin
        if (T.FClassType = '') then
          ToolKitThread.FList.Add(T);
      end else
        ToolKitThread.FList.Add(T);
    end;
  end;
  ToolKitThread.Start;
end;

procedure TMainForm.OnTerminate(Sender: TObject);
var
  I: longint;
begin
  SynEdit.BeginUpdate(True);
  SynEdit.Clear;
  with ToolKitThread.FList do
  begin
    for I := 0 to Document.Count - 1 do SynEdit.Append(Document[I]);
    for I := 0 to Messages.Count - 1 do Memo.Lines.Add(Messages[I]);
  end;
  SynEdit.EndUpdate;
  UpdateButton(True);
  ToolKitThread := nil;
end;

procedure TMainForm.UpdateButton(Value: boolean);
begin
  LoadBtn.Enabled                   := Value;
  OptimizeBox.Enabled               := Value;
  OptimizationTime.Enabled          := Value;
  UseFuncInsteadOfOperators.Enabled := Value;
  SkipVectorialUnits.Enabled        := Value;

  ExportBtn.Enabled        := Value;
  RunBtn.Enabled           := Value;
  case Value of
    True:  PageControl.TabIndex := 1;
    False: PageControl.TabIndex := 2;
  end;
end;

procedure TMainForm.DoMessage;
begin
  Memo.Append(ToolKitThread.FMessage);
end;

{ TToolKitThread }

constructor TToolKitThread.Create;
begin
  FList := TToolKitBuilder.Create(@OnMessage);
  FList.SkipVectorialUnits := Mainform.SkipVectorialUnits.Checked;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TToolKitThread.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TToolKitThread.Execute;
begin
  FList.ExecutionTime := 0;
  if MainForm.OptimizeBox.Checked then
    FList.ExecutionTime := MainForm.OptimizationTime.Value;

  FList.InitialTemperature := 1000000;
  FList.CoolingRate        := 0.1;
  FList.Run;
end;

procedure TToolKitThread.OnMessage(const AMessage: string);
begin
  FMessage := AMessage;
  Synchronize(@MainForm.DoMessage);
end;

end.

