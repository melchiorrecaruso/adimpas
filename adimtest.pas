{
  Description: ADimPas Test program.

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

program adimtest;

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  ADim;

var
  side1, side2, side3, side4: TMeters;
  area: TSquareMeters;
  volume: TCubicMeters;
  hypervolume: TQuarticMeters;

  pressure: TPascals;
  stiffness: TNewtonsPerMeter;

  squarecharge: TSquareCoulombs;
  capacitance: TFarads;

  distance: TMeters;
  time: TSeconds;
  speed: TMetersPerSecond;
  acc: TMetersPerSquareSecond;
  density: TKilogramsPerCubicMeter;
  specificw: TNewtonsPerCubicMeter;
  force, normal: TNewtons;

  torque: TNewtonMeters;
  work: TJoules;
  power: TWatts;

  charge: TCoulombs;
  potential: TVolts;
  current: TAmperes;

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

  angularspeed: TRadiansPerSecond;

  kA: double;
  kAr: TMeters;
  radius: TMeters;


  mass: TKilograms;
  eta: TPascalSeconds;
  Cb: TKilogramsPerSecond;

  mass1: TKilograms;
  mass2: TKilograms;
  GN: TNewtonSquareMetersPerSquareKilogram;


  cCd: double;
  angle: TRadians;

  Uc: TJoules;
  Ug: TJoules;

  Ue: TJoules;
  kx: TNewtonsPerMeter;

  ke: TNewtonSquareMetersPerSquareCoulomb;
  q1: TCoulombs;
  q2: TCoulombs;
  Uel: TJoules;

  p: TKilogramMetersPerSecond;
  impulse: TKilogramMetersPerSecond;

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
  e0: TFaradsPerMeter;
  er: double;
  sigma: TCoulombsPerSquareMeter;

  m0: THenriesPerMeter;
  B: TTeslas;
  len: TMeters;
  r: TMeters;
  z: TMeters;
  loops: longint;

  i1, i2: TAmperes;

  magneticflux: TWebers;

  DeltaE: TVoltsPerMeter;

  Ampl: TMeters;
  Kw: TRadiansPerMeter;
  Omega: TRadiansPerSecond;
  phi: TRadians;

  wavelen: TMeters;
  yspeed: TMetersperSecond;
  yacc: TMetersPerSquareSecond;

  lightspeed: TMetersPerSecond;
  energy: TJoules;
  plank: TJouleSeconds;
  freq: THertz;

  I: TKilogramSquareMeters;
  Re: double;
begin
  ExitCode := 0;
  DefaultFormatSettings.DecimalSeparator := '.';
  writeln('ADIM-TEST STARTING ...');

  // TEST-00 - AREA
  side1 := 10*m;
  side2 := 5*m;
  area  := side1*side2;
  side1 := area/side2;
  side2 := area/side1;
  if side1.ToVerboseString <> '10 meters'        then halt(1);
  if side2.ToVerboseString <> '5 meters'         then halt(2);
  if area .ToVerboseString <> '50 square meters' then halt(3);
  writeln('* TEST-00: PASSED');

  // TEST-01 - VOLUME
  side1  := 10*m;
  side2  := 5*m;
  side3  := 2*m;
  volume := side1*side2*side3;
  side1  := volume/side2/side3;
  side2  := volume/side1/side3;
  side3  := volume/side1/side2;
  if side1 .ToVerboseString <> '10 meters'        then halt(1);
  if side2 .ToVerboseString <> '5 meters'         then halt(2);
  if side3 .ToVerboseString <> '2 meters'         then halt(3);
  if volume.ToVerboseString <> '100 cubic meters' then halt(4);
  writeln('* TEST-01: PASSED');

  // TEST-02 - HYPER VOLUME
  side1 := 10*m;
  side2 := 5*m;
  side3 := 2*m;
  side4 := 7*m;
  hypervolume := side1*side2*side3*side4;
  side1 := hypervolume/side2/side3/side4;
  side2 := hypervolume/side1/side3/side4;
  side3 := hypervolume/side1/side2/side4;
  side4 := hypervolume/side1/side2/side3;
  if side1      .ToVerboseString <> '10 meters'          then halt(1);
  if side2      .ToVerboseString <> '5 meters'           then halt(2);
  if side3      .ToVerboseString <> '2 meters'           then halt(3);
  if side4      .ToVerboseString <> '7 meters'           then halt(4);
  if hypervolume.ToVerboseString <> '700 quartic meters' then halt(5);
  writeln('* TEST-02: PASSED');

  // TEST-03 - SPEED
  distance := 20*km;
  time     := 2*hr;
  speed    := distance/time;
  time     := distance/speed;
  distance := speed*time;
  if speed   .ToMeterPerHour.ToVerboseString(5, 0, [pKilo]) <> '10 kilometers per hour' then halt(1);
  if time    .ToHour        .ToVerboseString                <> '2 hours'                then halt(2);
  if distance               .ToVerboseString(5, 0, [pKilo]) <> '20 kilometers'          then halt(3);
  writeln('* TEST-03: PASSED');

  // TEST-04 - ACCELERATION
  time  := 5*s;
  speed := 100*km/hr;
  acc   := speed/time;
  time  := speed/acc;
  speed := acc*time;
  if speed.ToMeterPerHour         .ToVerboseString(5, 0, [pKilo       ]) <> '100 kilometers per hour'           then halt(1);
  if time                         .ToVerboseString(5, 0, [            ]) <> '5 seconds'                         then halt(2);
  if acc  .ToMeterPerHourPerSecond.ToVerboseString(5, 0, [pKilo, pNone]) <> '20 kilometers per hour per second' then halt(3);
  if acc                          .ToVerboseString(5, 0, [            ]) <> '5.5556 meters per second squared'  then halt(4);
  writeln('* TEST-04: PASSED');

  // TEST-05 - FORCE
  mass  := 5*kg;
  acc   := 10*m/s2;
  force := mass*acc;
  mass  := force/acc;
  acc   := force/mass;
  if mass .ToVerboseString(5, 0, []) <> '5 kilograms' then halt(1);
  if acc  .ToString       (5, 0, []) <> '10 m/s2'    then halt(2);
  if force.ToVerboseString(5, 0, []) <> '50 newtons'  then halt(3);
  writeln('* TEST-05: PASSED');

  // TEST-06 - ANGULAR SPEED
  angle        := 5*rad;
  time         := 2*s;
  angularspeed := angle/time;
  radius       := 2*m;
  speed        := angularspeed*radius;
  angularspeed := speed/radius;
  if angularspeed .ToVerboseString(5, 1, []) <> '2.5 radians per second' then halt(1);
  if speed        .ToVerboseString(5, 1, []) <> '5 meters per second'    then halt(2);
  writeln('* TEST-06: PASSED');

  // TEST-07 - CENTRIFUGAL FORCE
  mass         := 1*kg;
  angularspeed := 2*rad/s;
  radius       := 10*m;
  speed        := angularspeed*radius;
  acc          := (angularspeed*angularspeed)*radius;
  force        := mass*acc;
  if speed.ToString(5, 0, []) <> '20 m/s'  then halt(1);
  if acc  .ToString(5, 0, []) <> '40 m/s2' then halt(2);
  if force.ToString(5, 0, []) <> '40 N'    then halt(3);
  writeln('* TEST-07: PASSED');

  // TEST-08 - CENTRIPETAL FORCE
  mass         := 10*kg;
  radius       := 1*m;
  angularspeed := 2*rad/s;
  speed        := angularspeed*radius;
  force        := mass*(SquarePower(angularspeed)*radius);
  force        := mass*(SquarePower(speed)/radius);
  if speed.ToString(5, 0, []) <> '2 m/s' then halt(1);
  if force.ToString(5, 0, []) <> '40 N'  then halt(2);
  writeln('* TEST-08: PASSED');

  // TEST-09 - PRESSURE
  force    := 10*N;
  area     := 5*m2;
  pressure := force/area;
  force    := pressure*area;
  area     := force/pressure;
  if pressure.ToString <> '2 Pa' then halt(1);
  if force   .ToString <> '10 N' then halt(2);
  if area    .ToString <> '5 m2' then halt(3);
  writeln('* TEST-09: PASSED');

  // TEST-10 - WORK
  force    := 10*N;
  distance := 5*m;
  work     := force*distance;
  if work.ToString       (5, 0, [pDeca]) <> '5 daJ'        then halt(1);
  if work.ToVerboseString(5, 0, [pDeca]) <> '5 decajoules' then halt(2);
  writeln('* TEST-10: PASSED');

  // TEST-11 - POWER (J/s)
  work  := 50*J;
  time  := 10*s;
  power := work/time;
  if power.ToString(5, 0, []) <> '5 W' then halt(1);
  writeln('* TEST-11: PASSED');

  // TEST-12 - POWER (N.m * rad/s)
  torque       := 10*N*m;
  angularspeed := 2*rad/s;
  power        := torque*angularspeed;
  if power.ToString(5, 0, []) <> '20 W' then halt(1);
  writeln('* TEST-12: PASSED');

  // TEST-13 - VOLT (J/C)
  work      := 50*J;
  charge    := 25*C;
  potential := work/charge;
  if potential.ToString <> '2 V'  then halt(1);
  if charge   .ToString <> '25 C' then halt(2);
  if work     .ToString <> '50 J' then halt(3);
  writeln('* TEST-13: PASSED');

  // TEST-14 - VOLT (W/A)
  power     := 10*W;
  current   := 5*A;
  potential := power/current;
  if potential.ToString <> '2 V'  then halt(1);
  if current  .ToString <> '5 A'  then halt(2);
  if power    .ToString <> '10 W' then halt(3);
  writeln('* TEST-14: PASSED');

  // TEST-15 - VOLT ((W*Ω)^0.5);
  power      := 250*W;
  resistance := 10*Ohm;
  potential  := SquareRoot(power*resistance);
  if potential.ToString <> '50 V' then halt(1);
  writeln('* TEST-15: PASSED');

  // TEST-16 - AMPERE ((W/Ω)^0.5);
  power      := 4000*W;
  resistance := 10*Ohm;
  current    := SquareRoot(power/resistance);
  if current.ToString <> '20 A' then halt(1);
  writeln('* TEST-16: PASSED');

  // TEST-17 - FARAD (C2/J)
  squarecharge := 25*C2;
  work         := 50*J;
  capacitance  := squarecharge/work;
  if capacitance.ToString <> '0.5 F' then halt(1);
  writeln('* TEST-17: PASSED');

  // TEST-18 - FARAD (C/V)
  charge      :=  10*C;
  potential   :=  5*V;
  capacitance := charge/potential;
  if capacitance.ToVerboseString <> '2 farads' then halt(1);
  writeln('* TEST-18: PASSED');

  // TEST-19 - WEBER
  potential := 5*V;
  time      := 10*s;
  flux      := potential*time;
  if flux.ToVerboseString <> '50 webers' then halt(1);
  writeln('* TEST-19: PASSED');

  // TEST-20 - TESLA
  flux        := 25*Wb;
  area        := 10*m2;
  fluxdensity := flux/area;
  if fluxdensity.ToVerboseString <> '2.5 teslas' then halt(1);
  writeln('* TEST-20: PASSED');

  // TEST-21 - HENRY
  flux       := 30*Wb;
  current    := 10*A;
  inductance := flux/current;
  if inductance.ToString        <> '3 H'       then halt(1);
  if inductance.ToVerboseString <> '3 henries' then halt(2);
  writeln('* TEST-21: PASSED');

  // TEST-22 - SIEMENS
  resistance  := 2*Ohm;
  conductance := 1/resistance;
  if conductance.ToVerboseString <> '0.5 siemens' then halt(1);
  writeln('* TEST-22: PASSED');

  // TEST-23 - COULOMB
  current := 5*A;
  time    := 5*s;
  charge  := current*time;
  if charge.ToVerboseString <> '25 coulombs' then halt(1);
  if charge.ToString        <> '25 C'        then halt(2);
  writeln('* TEST-23: PASSED');

  // TEST-24 - LUMEN
  intensity    := 10*cd;
  solidangle   := 90*sr;
  luminousflux := intensity*solidangle;
  if luminousflux.ToString <> '900 lm' then halt(1);
  writeln('* TEST-24: PASSED');

  // TEST-25 - SIEVERT & GRAY
  dose1 := 10*Sv;
  dose2 := 5 *Gy;
  dose1 := 10*m2/s2;
  dose2 := 5 *m2/s2;
  dose1 := 10*j/kg;
  dose2 := 5 *j/kg;
  if dose1          .ToString <> '10 m2/s2' then halt(1);
  if dose2          .ToString <> '5 m2/s2'  then halt(2);
  if dose1.ToSievert.ToString <> '10 Sv'    then halt(3);
  if dose2.ToGray   .ToString <> '5 Gy'     then halt(4);
  writeln('* TEST-25: PASSED');

  // TEST-26 - NEWTON PER METER
  force     := 50*N;
  distance  := 10*mm;
  stiffness := force/distance;
  if stiffness.ToString <> '5000 N/m' then halt(1);
  writeln('* TEST-26: PASSED');

  // TEST-27 - DENSITY
  mass    := 10*kg;
  volume  := 5*m3;
  density := mass/volume;
  mass    := density*volume;
  volume  := mass/density;
  if density.ToString <> '2 kg/m3' then halt(1);
  if mass   .ToString <> '10 kg'   then halt(2);
  if volume .ToString <> '5 m3'    then halt(3);
  writeln('* TEST-27: PASSED');

  // TEST-28 - SPECIFIC WEIGHT
  force     := 100*N;
  volume    := 10*m3;
  specificw := force/volume;
  force     := specificw*volume;
  volume    := force/specificw;
  if specificw.ToString <> '10 N/m3' then halt(1);
  if force    .ToString <> '100 N'   then halt(2);
  if volume   .ToString <> '10 m3'   then halt(3);
  writeln('* TEST-28: PASSED');

  // TEST-29 - SLIDING FRICTION
  normal := 100*N;
  kA     := 0.05;
  force  := kA*normal;
  if force.ToString <> '5 N' then halt(1);
  writeln('* TEST-29: PASSED');

  // TEST-30 - ROLLING FRICTION
  normal := 100*N;
  kAr    := 0.0005*m;
  radius := 50*mm;
  force  := kAr*normal/radius;
  if force.ToString <> '1 N' then halt(1);
  writeln('* TEST-30: PASSED');

  // TEST-31 - VISCOSITY FORCE (LAMINAR FLOW)
  eta    := 10*Pa*s;
  radius := 20*mm;
  side1  := 1*m;
  area   := 2*radius*side1;
  speed  := 0.5*m/s;
  force  := eta/(radius/speed)*area;
  force  := 6*pi*radius*eta*speed;
  if force.ToString(4, 2, []) <> '1.885 N' then halt(1);
  writeln('* TEST-31: PASSED');

  // TEST-32 - DRAG FORCE
  cCd     := 0.47;
  area    := 1000*mm2;
  speed   := 5*m/s;
  density := 1.225*kg/m3;
  force   := 0.5*cCd*(density*SquarePower(Speed))*area;
  if force  .ToString(4, 2, []) <> '0.007197 N'  then halt(1);
  if density.ToString(4, 2, []) <> '1.225 kg/m3' then halt(2);
  writeln('* TEST-32: PASSED');

  // TEST-33 - UNIVERSAL GRAVITATION LAW
  mass1    := 5.972E24*kg;
  mass2    := 7.348E22*kg;
  distance := 384400*km;
  GN       := 6.67E-11*N*m2/kg2;
  GN       := 6.67E-11*N*m2/kg2;
  force    := GN*(mass1*mass2)/(distance*distance);
  if force.ToString(4, 2, []) <> '1.981E20 N' then halt(1);
  writeln('* TEST-33: PASSED');

  // TEST-34 - GRAVITATIONAL POTENTIAL ENERGY
  mass     := 10*kg;
  acc      := 9.81*m/s2;
  distance := 10*m;
  Ug       := mass*acc*distance;
  if Ug.ToString(4, 2, []) <> '981 J' then halt(1);
  writeln('* TEST-34: PASSED');

  // TEST-35 - KINEMATIC POTENTIAL ENERGY
  mass  := 10*kg;
  speed := 5*m/s;
  Uc    := 1/2*mass*(speed*speed);
  if Uc.ToString(4, 2, []) <> '125 J' then halt(1);
  writeln('* TEST-35: PASSED');

  // TEST-36 - ELASTIC POTENTIAL ENERGY
  kx       := 10*N/m;
  distance := 10*m;
  Ue       := 0.5*kx*(distance*distance);
  if Ue.ToString(4, 2, []) <> '500 J' then halt(1);
  writeln('* TEST-36: PASSED');

  // TEST-37 - MOMENTUM
  mass  := 10*kg;
  speed := 5*m/s;
  p     := mass*speed;
  if p.ToString(4, 2, []) <> '50 kg·m/s' then halt(1);
  writeln('* TEST-37: PASSED');

  // TEST-38 - IMPULSE
  force   := 10*N;
  time    := 5*ms;
  impulse := p;
  impulse := force*time;
  if impulse.ToNewtonSecond.ToString(4, 2, [pNone, pMilli]) <> '50 N·ms' then halt(1);
  writeln('* TEST-38: PASSED');

  // TEST-39 - STEVINO'S LAW
  density  := 10*kg/m3;
  acc      := 9.81*m/s2;
  distance := 2*m;
  pressure := density*acc*distance;
  if pressure.ToString(4, 2, []) <> '196.2 Pa' then halt(1);
  writeln('* TEST-39: PASSED');

  // TEST-40 - ARCHIMEDE'S LAW
  density := 0.5*kg/m3;
  acc     := 9.81*m/s2;
  volume  := 0.5*m3;
  force   := density*acc*volume;
  if force.ToString(4, 2, []) <> '2.453 N' then halt(1);
  writeln('* TEST-40: PASSED');

  // TEST-41 - CONTINUITY EQUATION (FLUID)
  volume   := 50*m3;
  time     := 10*s;
  flowrate := volume/time;
  if flowrate.ToString(4, 2, []) <> '5 m3/s' then halt(1);
  writeln('* TEST-41: PASSED');

  // TEST-42 - BERNOULLI'S LAW
  density  := 5*kg/m3;
  speed    := 5*m/s;
  pressure := 1/2*density*(speed*speed);
  if pressure.ToString(4, 2, []) <> '62.5 Pa' then halt(1);

  acc      := 9.81*m/s2;
  distance := 2*m;
  pressure := density*acc*distance;
  if pressure.ToString(4, 2, []) <> '98.1 Pa' then halt(2);
  writeln('* TEST-42: PASSED');

  // TEST-43 - REYNOLDS NUMBER
  flowrate := 5*dm3/minute;
  density  := 1.05*g/cm3;
  eta      := 0.003*Pl;
  radius   := 0.9*cm;
  Re       := 2000;
  speed    := Re*eta/(2*density*radius);
  if speed.ToString(4, 2, []) <> '0.3175 m/s' then halt(1);
  writeln('* TEST-43: PASSED');

  // TEST-44 - LINEAR THERMAL EXPANSION
  distance  := 10*m;
  lambda    := 1.2E-5*(1/K);
  deltatemp := 100*K;
  deltadist := distance*(lambda*deltatemp);
  if deltadist.ToString(4, 2, [pMilli]) <> '12 mm' then halt(1);
  writeln('* TEST-44: PASSED');

  // TEST-45 - HEAT CAPACITY
  mass                 := 10*kg;
  specificheatcapacity := 7.5*J/kg/K;
  heatcapacity         := mass*specificheatcapacity;
  if heatcapacity.ToString(4, 2, []) <> '75 J/K' then halt(1);
  writeln('* TEST-45: PASSED');

  // TEST-46 - CALORIMETER
  _m1 := 10*kg;
  _t1 := 100*K;
  _c1 := 7.5*J/kg/K;
  _m2 := 10*kg;
  _t2 := 50*K;
  _c2 := 7.5*J/kg/K;
  _tf := (_m1*_c1*_t1+_m2*_c2*_t2) / (_m1*_c1+_m2*_c2);
  if _tf.ToString(4, 2, []) <> '75 K' then halt(1);
  writeln('* TEST-46: PASSED');

  // TEST-47 - THERMAL FLUX
  area      := 5*m2;
  side1     := 100*mm;
  lambda2   := 1.1*W/m/K;
  deltatemp := 15*K;
  power     := lambda2*(deltatemp/side1)*area;
  if power.ToString(4, 2, []) <> '825 W' then halt(1);
  writeln('* TEST-47: PASSED');

  // TEST-48 - ELECTRIC POTENTIAL ENERGY
  ke  := 8.988E9*N*m2/C2;
  q1  := 6E-6*C;
  q2  := 9E-6*C;
  Uel := ke*(q1*q2/distance);
  if Uel.ToString(4, 2, [pMilli]) <> '48.54 mJ' then halt(1);
  writeln('* TEST-48: PASSED');

  // TEST-49 - ELECTROSTATIC FORCE ON A POINT CHARGE IN A ELECTRIC FIELD
  E     := 0.0015*N/C;
  q1    := 1.602E-19*C;
  force := E*q1;
  if force.ToString(4, 2, [pYocto]) <> '240.3 yN' then halt(1);
  writeln('* TEST-49: PASSED');

  // TEST-50 - ELECTROSTATIC FORCE BETWEEN TWO POINT CHARGES
  ke       := 8.988E9*N*m2/C2;
  q1       := 6E-6*C;
  q2       := 9E-6*C;
  distance := 2*m;
  force    := ke*(q1*q2)/(distance*distance);
  if force.ToString(4, 2, [pMilli]) <> '121.3 mN' then halt(1);
  writeln('* TEST-50: PASSED');

  // TEST-51 - ELECTRIC FIELD OF A SINGLE POINT CHARGE
  e0 := 8.854187817E-12*F/m;
  er := 1;
  ke := 1/(4*pi*e0*er);
  q1 := 2*C;
  r  := 5*cm;
  E  := ke*(q1/SquarePower(r));
  if E.ToString(4, 2, [pMega, pMilli]) <> '7190 MV/mm' then halt(1);
  writeln('* TEST-51: PASSED');

  // TEST-52 - ELECTRIC FIELD OF UNIFORM CHARGE SPHERE
  e0       := 8.854187817E-12*F/m;
  er       := 1;
  ke       := 1/(4*pi*e0);
  q1       := 2*C;
  r        := 10*cm;
  distance := 5*cm;
  E        := ke*(q1/ (CubicPower(r)/distance));
  if E.ToString(4, 2, [pMega, pMilli]) <> '898.8 MV/mm' then halt(1);
  writeln('* TEST-52: PASSED');

  // TEST-53 - ELECTRIC FIELD OF PARALLEL CONDUCTING PLATES
  e0    := 8.854187817E-12*F/m;
  er    := 1;
  q1    := 2*C;
  Area  := 4*cm2;
  sigma := q1/Area;
  E     := sigma/e0/er;
  if E.ToString(4, 2, [pGiga, pMilli]) <> '564.7 GV/mm' then halt(1);
  writeln('* TEST-53: PASSED');

  // TEST-54 - MAGNETIC FORCE FOR LIFTING A OBJECT
  mass    := 100*g;
  acc     := 9.81*m/s2;
  len     := 20*cm;
  B       := 2.0*T;
  current := (mass*acc)/(len*B*Sin(90*deg));
  if current.ToString(4, 2, [pMilli]) <> '2453 mA' then halt(1);
  writeln('* TEST-54: PASSED');

  // TEST-55 - MAGNETIC FIELD DUE TO STRAIGHT WIRE
  m0       := 4*pi*1E-7*T*m/A;
  current  := 3.0*A;
  R        := 50*cm;
  z        := 0*cm;
  B        := m0/(2*pi) * (current / (SquareRoot(CubicPower(SquarePower(z)+SquarePower(R)))/SquarePower(R) ));
  {$IFDEF WINDOWS}
  if Utf8ToAnsi(B.ToString(4, 2, [pMicro])) <> Utf8ToAnsi('1.2 µT') then halt(1);
  {$ENDIF}
  {$IFDEF UNIX}
  if B.ToVerboseString(4, 2, [pMicro]) <> '1.2 microteslas' then halt(1);
  {$ENDIF}
  writeln('* TEST-55: PASSED');

  // TEST-56 - MAGNETIC FIELD PRODUCED BY A CURRENT-CARRYING SOLENOID
  m0       := 4*pi*1E-7*T*m/A;
  current  := 1600*A;
  loops    := 2000;
  len      := 2.0*m;
  B        := m0*loops*(current/len);
  if B.ToVerboseString(4, 2, []) <> '2.011 teslas' then halt(1);
  writeln('* TEST-56: PASSED');

  // TEST-57 - FORCES BETWEEN PARALLEL CONDUCTORS
  m0    := 4*pi*1E-7*T*m/A;
  i1    := 2.5*A;
  i2    := 1.5*A;
  r     := 4*cm;
  len   := 1.0*m;
  force := (m0/(2*pi)*(len/r)) * (i1*i2);
  if force.ToVerboseString(4, 2, [pMicro]) <> '18.75 micronewtons' then halt(1);
  writeln('* TEST-57: PASSED');

  // TEST-58 - MAGNETIC FLUX
  B            := 0.4*T;
  Area         := 100*cm2;
  angle        := 70*deg;
  magneticflux := B*Area*cos(angle);
  if magneticflux.ToVerboseString(4, 2, [pMicro]) <> '1368 microwebers' then halt(1);
  writeln('* TEST-58: PASSED');

  // TEST-59 - ELECTROMAGNETIC INDUCTION
  magneticflux := 6*1E-5*Wb;
  time         := 0.1*s;
  potential    := magneticflux/time;
  if potential.ToVerboseString(4, 2, [pMicro]) <> '600 microvolts' then halt(1);
  writeln('* TEST-59: PASSED');

  // TEST-60 - DISPLACEMENT CURRENT
  Area     := 100*cm2;
  e0       := 8.854187817E-12*F/m;
  DeltaE   := 6.0E10*N/C;
  time     := 1*s;
  current  := (e0*DeltaE*Area)/time;
  if current.ToVerboseString(4, 2, [pMicro]) <> '5313 microamperes' then halt(1);
  writeln('* TEST-60: PASSED');

  // TEST-61 - HARMONIC WAVE
  Ampl    := 2*m;
  Kw      := 0.2*rad/m;
  omega   := 80*rad/s;
  phi     := 0*rad;
  wavelen := Ampl*Sin(Kw*(1*m) -omega*(0.8*s) + phi);
  yspeed  := -omega*Ampl*Cos(Kw*(1*m) -omega*(0.8*s));
  yacc    := -SquarePower(omega)*Ampl*cos(Kw*(1*m) -omega*(0.8*s));
  power   := (1.0*g/m)*SquarePower(omega*Ampl)*(5*mm/s);
  if wavelen.ToString(4, 2, [pMilli]) <> '-1648 mm'   then halt(1);
  if yspeed .ToString(4, 2, [      ]) <> '-90.69 m/s' then halt(2);
  if yacc   .ToString(4, 2, [      ]) <> '-7255 m/s2' then halt(3);
  if power  .ToString(4, 2, [pMilli]) <> '128 mW'     then halt(4);
  writeln('* TEST-61: PASSED');

  // TEST-62 - RELATIVTY: ENERGY
  mass       := 1*kg;
  lightspeed := 299792458*m/s;
  energy     := mass*SquarePower(lightspeed);
  if energy.ToElettronvolt.ToString(4, 2, [pTera]) <> '5.61E23 TeV' then halt(1);
  if energy               .ToString(4, 2, [pTera]) <> '8.988E4 TJ'  then halt(2);
  writeln('* TEST-62: PASSED');

  // TEST-63 - RELATIVTY: MOMENTUM
  mass       := 9.11*1E-31*kg;
  speed      := 10800000*km/hr;
  p          := mass*speed;
  energy     := SquareRoot(SquarePower(p*lightspeed)+ SquarePower(mass*SquarePower(lightspeed)));
  if p                    .ToString(4, 2, [pPico, pPico, pNone]) <> '2733 pg·pm/s' then halt(1);
  if energy.ToElettronvolt.ToString(4, 2, [pKilo])               <> '511.1 keV'    then halt(2);
  writeln('* TEST-63: PASSED');

  // TEST-64 - MOMENTUM OF PHOTON
  plank  := 6.62607015*1E-34*J*s;
  len    := 1*mim;
  freq   := 1*Hz;
  energy := plank/(len/lightspeed);
  p      := plank*freq/lightspeed;
  p      := plank/len;
  speed  := len*freq;
  if energy.ToElettronvolt.ToString(4, 2, [                   ]) <> '1.24 eV'        then halt(1);
  if p                    .ToString(4, 2, [pPico, pPico, pNone]) <> '0.6626 pg·pm/s' then halt(2);
  if speed                .ToString(9, 2, [pPico, pNone       ]) <> '1000000 pm/s'   then halt(3);
  writeln('* TEST-64: PASSED');

  // TEST-65 - THIRD KEPLER'S LAW
  mass1    := 1.989E30*kg;
  mass2    := 5.972E24*kg;
  distance := 1*au;
  GN       := 6.6743E-11*N*m2/kg2;
  time     := SquareRoot( 4*Sqr(pi)*CubicPower(distance)/(GN*(mass1+mass2)));
  if time.ToDay.toString(5, 0, []) <> '365.2 d' then halt(1);
  writeln('* TEST-65: PASSED');

  // TEST-66 - EARTH'S GRAVITY
  mass     := 5.972E24*kg;
  GN       := 6.6743E-11*N*m2/kg2;
  distance := 6.373E6*m;
  acc      := GN*mass/SquarePower(distance);
  if acc.ToString(3, 2, []) <> '9.81 m/s2' then halt(1);
  writeln('* TEST-66: PASSED');

  // TEST-67 - SIMPLE HARMONIC OSCILLATOR
  mass  := 1*kg;
  kx    := 10*N/m;
  omega := SquareRoot(kx/mass);
  if omega.ToString(3, 2, []) <> '3.16 rad/s' then halt(1);
  writeln('* TEST-67: PASSED');

  // TEST-68 - DAMPED HARMONIC OSCILLATOR
  mass   := 1*kg;
  kx     := 10*N/m;
  Cb     := 10*Pa*s*m;
  omega  := SquareRoot(kx/mass);
  if omega.ToString(3, 2, []) <> '3.16 rad/s' then halt(1);
  writeln('* TEST-68: PASSED');

  // TEST-69 - PHYSICAL PENDULUM
  mass   := 1*kg;
  I      := 10*kg*m2;
  acc    := 9.8*m/s2;
  radius := 20*cm;
  time   := 2*pi*SquareRoot(1/((mass*acc*radius)/I));
  time   := 2*pi*SquareRoot(I/(mass*acc*radius));
  if time.ToString(4, 2, []) <> '14.19 s' then halt(1);
  writeln('* TEST-69: PASSED');

  // TEST-70
  if (1.0*mg  ).ToString       (10, 10, [pKilo]) <> '1E-6 kg'                   then halt(01);
  if (1.0*mg2 ).ToString       (10, 0,  [pKilo]) <> '1E-12 kg2'                 then halt(02);
  if (1.0*mg  ).ToString       (10, 10, [pMega]) <> '1E-9 Mg'                   then halt(03);
  if (1.0*mg2 ).ToString       (10, 0,  [pMega]) <> '1E-18 Mg2'                 then halt(04);
  if (1.0*kg  ).ToString       (10, 0,  [pNone]) <> '1000 g'                    then halt(05);
  if (1.0*kg  ).ToString       (10, 0,  [pKilo]) <> '1 kg'                      then halt(06);
  if (1.0*kg2 ).ToString       (10, 0,  [pNone]) <> '1000000 g2'                then halt(07);
  if (1.0*kg2 ).ToString       (10, 0,  [pKilo]) <> '1 kg2'                     then halt(08);
  if (1.0*km  ).ToString       (10, 0,       []) <> '1000 m'                    then halt(09);
  if (1.0*km2 ).ToString       (10, 0,       []) <> '1000000 m2'                then halt(10);
  if (1.0*kg  ).ToVerboseString(10, 0,  [pNone]) <> '1000 grams'                then halt(11);
  if (1.0*kg2 ).ToVerboseString(10, 0,  [pNone]) <> '1000000 square grams'      then halt(12);
  if (1.0*km  ).ToVerboseString(10, 0,       []) <> '1000 meters'               then halt(13);
  if (1.0*km2 ).ToVerboseString(10, 0,       []) <> '1000000 square meters'     then halt(14);
  if (1.0*km3 ).ToVerboseString(10, 0,       []) <> '1000000000 cubic meters'   then halt(15);
  if (1.0*km4 ).ToVerboseString(10, 0,       []) <> '1E12 quartic meters'       then halt(16);
  if (1.0*day ).ToVerboseString(10, 0,       []) <> '86400 seconds'             then halt(16);
  if (1.0*hr  ).ToVerboseString(10, 0,       []) <> '3600 seconds'              then halt(17);
  if (1.0*day ).ToString       (10, 0,       []) <> '86400 s'                   then halt(18);
  if (1.0*hr  ).ToString       (10, 0,       []) <> '3600 s'                    then halt(19);
  if (1.0*day2).ToVerboseString(10, 0,       []) <> '7464960000 square seconds' then halt(20);
  if (1.0*hr2 ).ToVerboseString(10, 0,       []) <> '12960000 square seconds'   then halt(21);
  if (1.0*day2).ToString       (10, 0,       []) <> '7464960000 s2'             then halt(22);
  if (1.0*hr2 ).ToString       (10, 0,       []) <> '12960000 s2'               then halt(23);
  writeln('* TEST-70: PASSED');

  writeln('ADIM-TEST DONE.');
end.
