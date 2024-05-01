{
  Description: Simulated annealing optimizer.

  Copyright (C) 2022-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit SimulatedAnnealing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, DateUtils, SysUtils;

type
  // tsolution

  TSolution = array of string;

  TMessageEvent = procedure(const AMessage: string) of object;

  // tsimulatedannealing

  TSimulatedAnnealing = class
  protected
    FCoolingRate: double;
    FExecutionTime: longint;
    FInitialTemperature: double;
    FOnMessageEvent: TMessageEvent;
    procedure CopySolution(const Source: TSolution; var Dest: TSolution);
    function AcceptanceProbability(
      const Energy, NeighbourEnergy, Temperature: double): double;
  public
    constructor Create(OnMessageEvent: TMessageEvent);
    destructor Destroy; override;
    procedure Execute(var BestSolution: TSolution);
    procedure CreateSolution(var Neighbour: TSolution); virtual; abstract;
    function CalculateEnergy(const Solution: TSolution): double; virtual; abstract;
  published
    property CoolingRate: double read FCoolingRate write FCoolingRate;
    property InitialTemperature: double read FInitialTemperature
      write FInitialTemperature;
    property ExecutionTime: longint read FExecutionTime write FExecutionTime;
  end;


implementation

// TSimulatedAnnealing

constructor TSimulatedAnnealing.Create(OnMessageEvent: TMessageEvent);
begin
  inherited Create;
  FCoolingRate := 0.001; // cooling rate
  FExecutionTime := 10;
  FInitialTemperature := 10; // set initial temp
  FOnMessageEvent := OnMessageEvent;
end;

destructor TSimulatedAnnealing.Destroy;
begin
  inherited Destroy;
end;

function TSimulatedAnnealing.AcceptanceProbability(
  const Energy, NeighbourEnergy, Temperature: double): double;
begin
  // if the new solution is better, accept it
  if (NeighbourEnergy < Energy) then
  begin
    Result := 1.0;
  end else
    // if the new solution is worse, calculate an acceptance probability
  begin
    Result := exp((Energy - NeighbourEnergy) / Temperature);
  end;
end;

procedure TSimulatedAnnealing.CopySolution(const Source: TSolution; var Dest: TSolution);
var
  i: longint;
begin
  for i := Low(Source) to High(Source) do
  begin
    Dest[i] := Source[i];
  end;
end;

procedure TSimulatedAnnealing.Execute(var BestSolution: TSolution);
var
  BestEnergy: double;
  CurrentEnergy: double;
  CurrentSolution: TSolution = nil;
  NeighbourEnergy: double;
  NeighbourSolution: TSolution = nil;
  StartTime: tdatetime;
  Temperature: double;
begin
  if Assigned(FOnMessageEvent) then
    FOnMessageEvent('Optimizer starting ...');
  // initialize temperature
  Temperature := FInitialTemperature;
  // initalize best solution
  BestEnergy := CalculateEnergy(BestSolution);
  // initialize current solution
  SetLength(CurrentSolution, system.length(BestSolution));
  CopySolution(BestSolution, CurrentSolution);
  CurrentEnergy := BestEnergy;
  // initialize NeighbourSolution
  setlength(NeighbourSolution, system.length(BestSolution));
  // loop until system has cooled
  StartTime := Now;
  while (Temperature > 0) and (SecondsBetween(Now, StartTime) < FExecutionTime) do
  begin
    // create new neighbour BestSolution
    CopySolution(CurrentSolution, NeighbourSolution);
    CreateSolution(NeighbourSolution);
    NeighbourEnergy := CalculateEnergy(NeighbourSolution);
    // decide if we should accept the neighbour
    if AcceptanceProbability(CurrentEnergy, NeighbourEnergy, Temperature) > random then
    begin
      CopySolution(NeighbourSolution, CurrentSolution);
      CurrentEnergy := NeighbourEnergy;
    end;
    // get BestEnergy of solutions
    if CurrentEnergy < BestEnergy then
    begin
      if Assigned(FOnMessageEvent) then
        FOnMessageEvent('Current energy: ' + FloatToStr(CurrentEnergy));
      CopySolution(CurrentSolution, BestSolution);
      BestEnergy := CurrentEnergy;
    end;
    // cool system
    Temperature := Temperature * (1 - FCoolingRate);
  end;
  SetLength(NeighbourSolution, 0);
  SetLength(CurrentSolution, 0);
  if Assigned(FOnMessageEvent) then
    FOnMessageEvent('Optimizer finished.');
end;

end.
