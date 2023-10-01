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

unit Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SimulatedAnnealing, SysUtils;

type
  TToolKitItem = record
    FClassName: string;
    FOperator: string;
    FClassParent1: string;
    FClassParent2: string;
    FComment: string;
    FLongSymbol: string;
    FShortSymbol: string;
    FIdentifierSymbol: string;
    FBaseClass: string;
    FFactor: string;
    FPrefixes: string;
  end;

  TToolKitList = class(TSimulatedAnnealing)
  private
    FList: array of TToolkitItem;
    function GetCount: longint;
    function GetItem(Index: longint): TToolkitItem;
    procedure AddToSolution(var ASolution: TSolution; const AClassName: string);
    function FindInSolution(const ASolution: TSolution; const AClassName: string): longint;
  public
    function CalculateEnergy(const ASolution: TSolution): single; override;
    procedure CreateSolution(var ANeighbour: TSolution); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AItem: TToolkitItem);
    procedure Optimize(var BestSolution: TSolution);
  public
    property Items[Index: longint]: TToolkitItem read GetItem; Default;
    property Count: longint read GetCount;
  end;


function GetSymbol(const AShortSymbol: string): string;
function GetSingularName(const ALongSymbol: string): string;
function GetPluralName(const ALongSymbol: string): string;
function GetUnitQuantityType(const S: string): string;
function GetUnitComment(S: string): string;
function GetUnitDescription(const S: string): string;
function GetUnitClassName(const S: string): string;
function GetUnitClassNameHelper(const S: string): string;
function GetUnitIdentifier(const S: string): string;
function GetUnitQuantity(const S: string): string;
function GetPrefixes(const AShortSymbol: string): string;
function GetExponents(const AShortSymbol: string): string;

function CleanUnitName(const S: string): string;
function CleanUnitSymbol(const S: string): string;
procedure CleanDocument(S: TStringList);


implementation

uses
  Math, StrUtils;

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

function GetUnitQuantityType(const S: string): string;
begin
  Result := S;
  if Result <> '' then
  begin
    Result := StringReplace(Result, 'Foot!', 'Foot', [rfReplaceAll]);
    Result := StringReplace(Result, 'Inch!', 'Inch', [rfReplaceAll]);
    Result := StringReplace(Result, 'y!',    'y',    [rfReplaceAll]);
    Result := StringReplace(Result, '?',     '',     [rfReplaceAll]);
    Result := StringReplace(Result, ' ',     '',     [rfReplaceAll]);
    Result := Result + 'Qty';
  end;
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
  Result := Result + 'Unit';
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

function GetExponents(const AShortSymbol: string): string;
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

// TToolKitOptimizer

procedure TToolkitList.AddToSolution(var ASolution: TSolution; const AClassName: string);
var
  i: longint;
begin
  i := Length(ASolution);
  SetLength(ASolution, i + 1);
  ASolution[i] := AClassName;
end;

function TToolkitList.FindInSolution(const ASolution: TSolution; const AClassName: string): longint;
var
  i: longint;
begin
  for i := Low(ASolution) to High(ASolution) do
  begin
    if AClassName = ASolution[i] then Exit(i);
  end;
  Result := -1;
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

function TToolkitList.CalculateEnergy(const ASolution: TSolution): single;
var
  ABaseClass: string;
  ExternalOperator: longint;
  InternalOperator: longint;
  TotalOperator: longint;
  i, iL, iR, iX: longint;
  j: longint;
  S: TStringList;

  procedure Calculate(const AOperator, ALeftClass, ARightClass, AClassName: string);
  begin
    iL := FindInSolution(ASolution, ALeftClass);
    iR := FindInSolution(ASolution, ARightClass);
    iX := FindInSolution(ASolution, AClassName);

    ABaseClass := '';
    if (iL > -1) or
       (iR > -1) then
    begin
      i := Max(iL, iR);
      if i = iL then ABaseClass := ALeftClass;
      if i = iR then ABaseClass := ARightClass;
      if i < iX then ABaseClass := '';

      if ABaseClass <> '' then
      begin
        if (i = iL) and (ALeftClass  = 'THertz') then ABaseClass := '';
        if (i = iR) and (ARightClass = 'THertz') then ABaseClass := '';
      end;
    end;

    //if S.IndexOf(ABaseClass + '.' + ALeftClass + AOperator + ARightClass) = -1 then
    begin
      S.Add(ABaseClass + '.' + ALeftClass + AOperator + ARightClass);

      if ABaseClass = '' then
        Inc(ExternalOperator)
      else
        Inc(InternalOperator);
      Inc(TotalOperator);
    end;
  end;

begin
  ExternalOperator := 0;
  InternalOperator := 0;
  TotalOperator    := 0;

  S := TStringList.Create;
  for j := Low(FList) to High(FList) do
  begin
    if (FList[j].FOperator = '*') then
    begin
      Calculate('*', FList[j].FClassParent1, FList[j].FClassParent2, FList[j].FClassName);
      if FList[j].FClassParent1 <> FList[j].FClassParent2 then
        Calculate('*', FList[j].FClassParent2, FList[j].FClassParent1, FList[j].FClassName);

      Calculate('/', FList[j].FClassName, FList[j].FClassParent1, FList[j].FClassParent2);
      if FList[j].FClassParent1 <> FList[j].FClassParent2 then
        Calculate('/', FList[j].FClassName, FList[j].FClassParent2, FList[j].FClassParent1);

    end else
      if (FList[j].FOperator = '/') then
      begin
        Calculate('/', FList[j].FClassParent1,  FList[j].FClassParent2, FList[j].FClassName);
        Calculate('*', FList[j].FClassParent2,  FList[j].FClassName,    FList[j].FClassParent1);
        Calculate('*', FList[j].FClassName,     FList[j].FClassParent2, FList[j].FClassParent1);
        Calculate('/', FList[j].FClassParent1,  FList[j].FClassName,    FList[j].FClassParent2);
      end;

  end;
  S.Destroy;

  Result := ExternalOperator;
end;

procedure TToolkitList.Optimize(var BestSolution: TSolution);
var
  i: longint;
  S: TStringList;
begin
  BestSolution := nil;
  S := TStringList.Create;
  for i := Low(FList) to High(FList) do
    if S.IndexOf(FList[i].FClassName) = -1 then
    begin
      S.Add(FList[i].FClassName);
      if FList[i].FBaseClass = '' then
      begin
        AddToSolution(BestSolution, FList[i].FClassName);
      end;
    end;
  S.Destroy;
  Writeln(Length(BestSolution));

  InitialTemperature := 1000000;
  CoolingRate        := 0.1;
  ExecutionTime      := 120;

  writeln('START');
  Execute(BestSolution);
  writeln('END');
end;

// TToolkitList

constructor TToolkitList.Create;
begin
  inherited Create;
  FList := nil;
  Randomize;
end;

destructor TToolkitList.Destroy;
begin
  FList := nil;
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

end.

