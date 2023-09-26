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
  Classes, SysUtils;

type
  TToolkitItem = record
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

  TToolkitList = class
  private
    FList: array of TToolkitItem;
    function GetCount: longint;
    function GetItem(Index: longint): TToolkitItem;
  public
    constructor Create;
    destructor Destroy; override;
                          
    procedure Add(const AItem: TToolkitItem);
    function Find(const AClassName: string): longint;
  public
    property Count: longint read GetCount;
    property Items[Index: longint]: TToolkitItem read GetItem; Default;
  end;


implementation

constructor TToolkitList.Create;
begin
  inherited Create;
  FList := nil;
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

function TToolkitList.Find(const AClassName: string): longint;
var
  i: longint;
begin
  Result := -1;
  for i := Low(FList) to High(FList) do
  begin
    if FList[i].FClassName = AClassName then Exit(i);
  end;
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

