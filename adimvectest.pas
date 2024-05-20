{
  Description: ADimPas Vectorial Test program.

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
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

program adimvectest;

{$if FPC_FULLVERSION >= 30301}
  {$modeswitch implicitfunctionspecialization}
{$endif}

uses
  ADim, CL3, SysUtils;

var
  side1, side2, side3, side4: TCLMeters;
  area: TCLSquareMeters;
  volume: TCubicMeters;
  hypervolume: TQuarticMeters;

  pressure: TCLPascals;
  stiffness: TNewtonsPerMeter;

  squarecharge: TSquareCoulombs;
  capacitance: TFarads;

  distance: TMeters;
  tolerance: TMeters;
  time: TSeconds;
  speed: TCLMetersPerSecond;
  spin: TKilogramSquareMetersPerSecond;
  acc: TCLMetersPerSquareSecond;
  density: TKilogramsPerCubicMeter;
  specificw: TNewtonsPerCubicMeter;
  force, normal: TCLNewtons;


  torque: TCLNewtonMeters;
  work: TJoules;
  power: TCLWatts;

  charge: TCoulombs;
  potential: TCLVolts;
  current: TCLAmperes;

  flux: TWebers;
  fluxdensity: TTeslas;

  inductance: THenries;
  resistance: TOhms;
  conductance: TSiemens;

  solidangle: TSteradians;
  intensity: TCandelas;
  luminousflux: TLumens;

  dose1: TSieverts;
  dose2: TGrays;

  angularspeed: TCLRadiansPerSecond;

  kA: double;
  kAr: TMeters;
  radius: TCLMeters;

  mass: TKilograms;
  eta: TPascalSeconds;
  Cb: TKilogramsPerSecond;

  mass1: TKilograms;
  mass2: TKilograms;

  cCd: double;
  angle: TCLRadians;

  Uc: TJoules;
  Ug: TJoules;

  Ue: TJoules;
  kx: TNewtonsPerMeter;
  x: TMeters;

  q1: TCoulombs;
  q2: TCoulombs;
  Uel: TJoules;
  U: TJoules;

  p: TKilogramMetersPerSecond;
  p2: TSquareKilogramSquareMetersPerSquareSecond;
  impulse: TKilogramMetersPerSecond;
  Lp: TKilogramSquareMetersPerSecond;

  flowrate: TCubicMetersPerSecond;

  lambda: TReciprocalKelvins;
  deltadist: TMeters;
  deltatemp: TKelvins;

  specificheatcapacity: TJoulesPerKilogramPerKelvin;
  heatcapacity: TJoulesPerKelvin;

  _m1: TKilograms;
  _m2: TKilograms;
  _tf: TKelvins;
  _t1: TKelvins;
  _t2: TKelvins;
  _c1: TJoulesPerKilogramPerKelvin;
  _c2: TJoulesPerKilogramPerKelvin;

  lambda2: TWattsPerMeterPerKelvin;

  E: TVoltsPerMeter;
  sigma: TCoulombsPerSquareMeter;

  B: TTeslas;
  len: TMeters;
  r: TMeters;
  z: TMeters;
  loops: longint;

  i1, i2: TAmperes;

  DeltaE: TVoltsPerMeter;

  Ampl: TMeters;
  Kw: TRadiansPerMeter;
  omega: TCLRadiansPerSecond;
  phi: TRadians;

  wavelen: TMeters;
  wavelenc: TMeters;
  yspeed: TMetersperSecond;
  yacc: TMetersPerSquareSecond;

  E0: TJoules;
  Energy: TJoules;
  freq: THertz;

  I: TKilogramSquareMeters;
  Re: double;

  num: integer;
  alpha: double;
  kc: TReciprocalMeters;
  BoxLen: TMeters;
  EnergyLevels: array[1..4] of TJoules;
  SquarePsi: array[1..4] of TReciprocalMeters;
  Psi0: TReciprocalSquareRootMeters;
  PsiValues: array [1..4] of TReciprocalMeters;
  A0: TReciprocalSquareRootMeters;
  y: double;


  Iteration: longint;
  Iterations: longint;
  Probability: double;
  mu: TJoulesPerTesla;

  radiusvec: TCLMeters;
  displacement: TCLMeters;
  speedvec: TCLMetersPerSecond;
  accvec: TCLMetersPerSquareSecond;

  momentum: TCLKilogramMetersPerSecond;

  anglevec: TCLRadians;
  angularspeedvec: TCLRadiansPerSecond;
  angularacc: TCLRadiansPerSquareSecond;
  angularmomentum: TCLKilogramSquareMetersPerSecond;

  forcevec: TCLNewtons;
  areavec: TCLSquareMeters;

  torquevec: TCLNewtonMeters;

  magneticflux: TCLWebers;
  magneticfield: TCLTeslas;

  pressurevec: TCLPascals;

  torquestifness: TCLNewtonMetersPerRadian;

  electricfield: TCLVoltsPerMeter;

  potentialvec: TCLVolts;
  currentvec: TCLAmperes;
  powervec: TCLWatts;

  impedance: TCLOhms;
  omegavec: TCLRadiansPerSecond;

  side1vec: TCLMeters;
  side2vec: TCLMeters;

begin
  ExitCode := 0;
  DefaultFormatSettings.DecimalSeparator := '.';
  writeln('ADIM-VEC-TEST STARTING ...');

  // TEST-501: Area
  side1 := 5*e1*m;
  side2 := 10*e2*m;
  area  := 50*e12*m2;
  area  := side1.wedge(side2);
  side1 := area.dot(1/side2);
  side2 := (1/side1).dot(area);
  if area .ToVerboseString <> '(+50e12) m2' then halt(1);
  if side1.ToVerboseString <> '(+5e1) m'    then halt(2);
  if side2.ToVerboseString <> '(+10e2) m'   then halt(3);
  writeln('* TEST-501: PASSED');

  // TEST-502: Speed
  displacement := (5*e1 + 5*e2)*m;
  time         := 2*s;
  speed        := displacement/time;
  if speed.ToVerboseString <> '(+2.5e1 +2.5e2) m/s' then halt(1);
  writeln('* TEST-502: PASSED');

  // TEST-503: Vectorial acceleration
  speed    := (5*e1 + 5*e2)*m/s;
  time     := 2*s;
  acc      := speed/time;
  if acc.ToVerboseString <> '(+2.5e1 +2.5e2) m/s2' then halt(1);
  writeln('* TEST-503: PASSED');

  // TEST-504: Momentum
  mass     := 10*kg;
  speed    := (5*e1 + 5*e2)*m/s;
  momentum := mass*speed;
  if momentum.ToVerboseString <> '(+50e1 +50e2) kg·m/s' then halt(1);
  writeln('* TEST-504: PASSED');

  // TEST-505: Angular speed
  angle := (10*e13)*rad;
  time  := 2.5*s;
  angularspeed := angle/time;
  time := angle.dot(1/angularspeed);
  freq := angularspeed.dot(1/angle);

  if time.ToVerboseString         <> '2.5 seconds'   then halt(1);
  if angularspeed.ToVerboseString <> '(+4e13) rad/s' then halt(2);
  writeln('* TEST-505: PASSED');

  // TEST-506: Angular acceleration
  angularspeed := 5*e13*rad/s;
  angularacc   := angularspeed/(2*s);
  if angularacc.ToVerboseString <> '(+2.5e13) rad/s2' then halt(1);
  writeln('* TEST-506: PASSED');

  // TEST-507: Angular momentum
  radius          := 2*e1*m;
  momentum        := 5*e2*kg*m/s;
  angularmomentum := radius.wedge(momentum);
  if angularmomentum.ToVerboseString <> '(+10e12) kg·m2/s' then halt(1);
  writeln('* TEST-507: PASSED');

  // TEST-508: Force
  mass  := 10*kg;
  acc   := (2*e1 + 2*e2)*m/s2;
  force := mass*acc;
  if force.ToVerboseString <> '(+20e1 +20e2) N' then halt(1);

  momentum := 10*e1*kg*m/s;
  time     := 10*s;
  force    := momentum/time;
  if force.ToVerboseString <> '(+1e1) N' then halt(2);
  writeln('* TEST-508: PASSED');

  // TEST-509: Torque
  radius :=  2*e1*m;
  force  := 10*e2*N;
  torque := radius.wedge(force);
  radius := torque.dot(1/force);
  force  := (1/radius).dot(torque);
  if torque.ToVerboseString <> '(+20e12) N·m' then halt(1);
  if radius.ToVerboseString <> '(+2e1) m'     then halt(2);
  if force .ToVerboseString <> '(+10e2) N'    then halt(3);
  writeln('* TEST-509: PASSED');

  // TEST-510: Weber
  magneticfield := (10*e12)*T;
  area          := ( 5*e12)*m2;
  magneticflux  := -magneticfield.Dual.wedge(area);
  magneticfield := magneticflux.dot(1/area).Dual;
  area          := -(1/magneticfield.Dual).dot(magneticflux);
  if magneticflux .ToVerboseString <> '(+50e123) Wb' then halt(1);
  if magneticfield.ToVerboseString <> '(+10e12) T'   then halt(2);
  if area         .ToVerboseString <> '(+5e12) m2'   then halt(3);
  writeln('* TEST-510: PASSED');

  // TEST-511: Henry
  magneticflux := 50*e123*Wb;
  current      :=  5*e2*A;
  inductance   := -magneticflux.Dual/current.Norm;
  if inductance .ToVerboseString <> '10 henries' then halt(1);
  writeln('* TEST-511: PASSED');

  // TEST-512: Pascal
  force    := 10*e1*N;
  area     := 2*e23*m2;
  pressure := -force.wedge(1/area);
  force    := -pressure.dot(area);
  area     := -force.dot(1/pressure);
  if pressure .ToVerboseString <> '(+5e123) Pa' then halt(1);
  if force    .ToVerboseString <> '(+10e1) N'   then halt(2);
  if area     .ToVerboseString <> '(+2e23) m2'  then halt(3);
  writeln('* TEST-512: PASSED');

  // TEST-513: Torque stiffness
  torquestifness := 10*e12*N*m/rad;
  torquevec      := torquestifness * (5*rad);
  if torquevec .ToVerboseString <> '(+50e12) N·m' then halt(1);
  writeln('* TEST-513: PASSED');

  // TEST-514: Loretz force
  electricfield := (10*e1)*N/C;
  charge        := ElementaryCharge;
  force         := charge * electricfield;
  force         := electricfield * charge;
  charge        := force.dot(1/electricfield);
  electricfield := force/charge;
  writeln('* TEST-514: PASSED');

  // TEST-515: Voltages
  omega       := 1*e12*rad/s;
  potential   := 50*e1*V;
  resistance  := 2*Ohm;
  capacitance := 1*F;
  inductance  := 2*H;
  impedance   := resistance - (1/(omega*capacitance) + omega*inductance);
  current     := (1/impedance) * potential;
  power       := current * potential;
  {$IFDEF WINDOWS}
  if Utf8ToAnsi(Format('Z = %s', [impedance    .ToVerboseString])) <> Utf8ToAnsi('Z = (+2 -1e12) Ω')     then halt(1);
  {$ENDIF}
  {$IFDEF UNIX}
  if            Format('Z = %s', [impedance    .ToVerboseString])  <>            'Z = (+2 -1e12) Ω'      then halt(1);
  {$ENDIF}
  if            Format('I = %s', [current      .ToVerboseString])  <>            'I = (+20e1 -10e2) A'   then halt(2);
  if            Format('P = %s', [power        .ToVerboseString])  <>            'P = (+1000 +500e12) W' then halt(3);
  if            Format('Y = %s', [(1/impedance).ToVerboseString])  <>            'Y = (+0.4 +0.2e12) S'  then halt(4);

  if potential.Norm.ToString <> '50 V'               then halt(5);
  if current  .Norm.ToString <> '22.3606797749979 A' then halt(6);
  if power    .Norm.ToString <> '1118.03398874989 W' then halt(7);
  writeln('* TEST-515: PASSED');

  writeln('ADIM-TEST DONE.');
end.
