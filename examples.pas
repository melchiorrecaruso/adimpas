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

program examples;

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
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

  inductance: THenrys;
  resistance: TOhms;
  conductance: TSiemens;

  solidangle: TSteradians;
  intensity: TCandelas;
  luminousflux: TLumens;

  dose1: TSieverts;
  dose2: TGrays;

  squarevolt: TSquareVolts;
  squareamp: TSquareAmperes;

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

  m0: THenrysPerMeter;
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

  lightspeed: TMetersPerSecond;
  energy: TJoules;
  plank: TJouleSeconds;
  freq: THertz;

  I: TKilogramSquareMeters;
  Re: double;

begin
  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}

  // area
  side1 := 10*m;
  side2 := 5*m;
  area  := side1*side2;
  side1 := area/side2;
  side2 := area/side1;

  writeln('*** Area:');
  writeln('The side-1 is: ', side1.ToString);
  writeln('The side-2 is: ', side2.ToString);
  writeln('The area is: ', area.ToString);
  writeln;

  // volume
  side1  := 10*m;
  side2  := 5*m;
  side3  := 2*m;
  volume := side1*side2*side3;
  side1  := volume/side2/side3;
  side2  := volume/side1/side3;
  side3  := volume/side1/side2;

  writeln('*** Volume:');
  writeln('The side-1 is: ', side1.ToString);
  writeln('The side-2 is: ', side2.ToString);
  writeln('The side-3 is: ', side3.ToString);
  writeln('The volume is: ', volume.ToString);
  writeln;

  // hyper volume
  side1 := 10*m;
  side2 := 5*m;
  side3 := 2*m;
  side4 := 7*m;
  hypervolume := side1*side2*side3*side4;
  side1 := hypervolume/side2/side3/side4;
  side2 := hypervolume/side1/side3/side4;
  side3 := hypervolume/side1/side2/side4;
  side4 := hypervolume/side1/side2/side3;

  writeln('*** Hyper volume:');
  writeln('The side-1 is: ', side1.ToString);
  writeln('The side-2 is: ', side2.ToString);
  writeln('The side-3 is: ', side3.ToString);
  writeln('The side-4 is: ', side4.ToString);
  writeln('The hyper volume is: ', hypervolume.ToString);
  writeln;

  // speed
  distance := 20*km;
  time     := 60*minute;
  speed    := distance/time;
  time     := distance/speed;
  distance := speed*time;

  writeln('*** Speed: v = distance / time');
  writeln('The distance is: ', distance.ToString);
  writeln('The time is: ', time.ToString(4, 0, []));
  writeln('The speed is: ', speed.ToString(4, 0, []));
  writeln;

  // acceleration
  time         := 5*s;
  speed        := 100*km/hr;
  acc          := speed/time;
  time         := speed/acc;
  speed        := acc*time;

  writeln('*** Acceleration: a = speed / time');
  writeln('The speed is: ', speed.ToString(4, 0, []));
  writeln('The time is: ', time.ToString);
  writeln('The acceleration is: ', acc.ToString(4, 0, []));
  writeln;

  // force
  mass         := 5*kg;
  acc          := 10*m/s2;
  force        := mass*acc;
  mass         := force/acc;
  acc          := force/mass;

  writeln('*** Force: F = mass * acceleration');
  writeln('The mass is: ', mass.ToString);
  writeln('The acceleration is: ', acc.ToString);
  writeln('The force is: ', force.ToString);
  writeln;

  // angular speed
  angle        := 2*pi*rad;
  time         := 1*s;
  angularspeed := angle/time;
  radius       := 2*m;
  speed        := angularspeed*radius;
  angularspeed := speed/radius;


  writeln('*** Angular speed:');
  writeln('The angle is: ', angle.ToVerboseString(4, 0, []));
  writeln('The time is: ', time.ToVerboseString(4, 0, []));
  writeln('The angular speed is: ', angularspeed.ToVerboseString(4, 0, []));
  writeln('The radius is: ', radius.ToVerboseString);
  writeln('The tangential speed is: ', speed.ToVerboseString(4, 0, []));
  writeln;

  // centrifugal force
  mass         := 1*kg;
  angularspeed := 2*pi*rad/s;
  radius       := 10*m;
  acc          := (angularspeed*angularspeed)*radius;
  force        := mass*acc;

  writeln('*** Centrifugal force:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The angular speed is: ', angularspeed.ToVerboseString(4, 0, []));
  writeln('The radius is: ', radius.ToVerboseString);
  writeln('The acceleration is: ', acc.ToVerboseString(4, 0, []));
  writeln('The force is: ', force.ToVerboseString(4, 0, []));
  writeln;

  // centripetal force
  mass         := 10*kg;
  radius       := 1*m;
  angularspeed := 2*pi*rad/s;
  speed        := angularspeed*radius;
  force        := mass*(SquarePower(angularspeed)*radius);
  force        := mass*(SquarePower(speed)/radius);

  writeln('*** Centripetal force:');
  writeln('The mass is: ', mass.ToString(4, 0, []));
  writeln('The radius is: ', radius.ToString(4, 0, []));
  writeln('The angularspeed is: ', angularspeed.ToString(4, 0, []));
  writeln('The speed is: ', speed.ToString(4, 0, []));
  writeln('The force is: ', force.ToString(4, 0, []));
  writeln;

  // pressure
  force    := 10*N;
  area     := 5*m2;
  pressure := force/area;

  speed := 10*m/s;     // not speed := 10*(m/s);
  speed := 10*km/hr;   // not speed := 10*(km/hour);
  speed := 10*cm/s;    // not speed := 10*(cm/s);

  writeln('*** Pressure: P = force / area');
  writeln('The force is: ', force.ToString);
  writeln('The area: ', area.ToString);
  writeln('The pressure: ', pressure.ToString);
  writeln;

  // work
  force        := 10*N;
  distance     := 5*m;
  work         := force*distance;

  writeln('*** Work: W = force * distance');
  writeln('The force is: ', force.ToVerboseString);
  writeln('The distance is: ', distance.ToVerboseString);
  writeln('The work is: ', work.ToVerboseString);
  writeln;

  // power
  work  := 50*J;
  time  := 10*s;
  power := work/time;
  writeln('*** Power: P = work / time');
  writeln('The work is: ', work.ToVerboseString);
  writeln('The time is: ', time.ToVerboseString);
  writeln('The power is: ', power.ToVerboseString);
  writeln;

  // power
  torque       := 10*N*m;
  angularspeed := 2*pi*rad/s;
  power        := torque*angularspeed;

  writeln('*** Power: P = torque * angular speed');
  writeln('The torque is: ', torque.ToString(4, 0, []));
  writeln('The angular speed is: ', angularspeed.ToString(4, 0, []));
  writeln('The power is: ', power.ToString(4, 0, []));
  writeln;

  // volt (J/C)
  work      := 50*J;
  charge    := 25*C;
  potential := work/charge;

  writeln('*** Volt (J/C): V = work / charge');
  writeln('The work is: ', work.ToVerboseString);
  writeln('The charge is: ', charge.ToVerboseString);
  writeln('The potential is: ', potential.ToVerboseString);
  writeln;

  // volt (W/A)
  power     := 10*W;
  current   := 5*A;
  potential := power/current;

  writeln('*** Volt (W/A): V = power / current');
  writeln('The power is: ', power.ToVerboseString);
  writeln('The current is: ', current.ToVerboseString);
  writeln('The potential is: ', potential.ToVerboseString);
  writeln;

  // volt ((W*Ω)^0.5);
  power      := 10*W;
  resistance := 50*Ohm;
  squarevolt := power*resistance;

  writeln('*** Volt ((W*Ω)^0.5): V = square root (power / resistance)');
  writeln('The power is: ', power.ToVerboseString);
  writeln('The resistance is: ', resistance.ToVerboseString);
  writeln('The potential is: ', SquareRoot(squarevolt).ToString);
  writeln;

  // ampere ((W/Ω)^0.5);
  power      := 10*w;
  resistance := 50*Ohm;
  squareamp  := power/resistance;

  Writeln('*** Ampere ((W/Ω)^0.5):');
  writeln('The power is: ', power.ToVerboseString(4, 4, []));
  writeln('The resistance is: ', resistance.ToVerboseString(4, 4, []));
  writeln('The current is: ', SquareRoot(squareamp).ToVerboseString(4, 4, []));
  writeln;

  // farad (C2/J)
  squarecharge := 25*C2;
  work         := 50*J;
  capacitance  := squarecharge/work;

  writeln('*** Farad (C2/J):');
  writeln('The charge is: ', SquareRoot(squarecharge).ToVerboseString);
  writeln('The work is: ', work.ToVerboseString);
  writeln('The capacitance is: ', capacitance.ToVerboseString);
  writeln;

  // farad (C/V)
  charge      :=  10*C;
  potential   :=  5*V;
  capacitance := charge/potential;

  writeln('*** Farad (C/V):');
  writeln('The charge is: ', charge.ToString);
  writeln('The potential is: ', potential.ToString);
  writeln('The capacitance is: ', capacitance.ToString);
  writeln;

  // weber
  potential := 5*V;
  time      := 10*s;
  flux      := potential*time;

  writeln('*** Weber:');
  writeln('The potential is: ', potential.ToString);
  writeln('The time is: ', time.ToString);
  writeln('The flux is: ', flux.ToVerboseString);
  writeln;

  // tesla
  flux        := 25*Wb;
  area        := 10*m2;
  fluxdensity := flux/area;

  writeln('*** Tesla:');
  writeln('The flux is: ', flux.ToVerboseString);
  writeln('The area is: ', area.ToVerboseString);
  writeln('The flux density is: ', fluxdensity.ToVerboseString);
  writeln;

  // henry
  flux       := 30*Wb;
  current    := 10*A;
  inductance := flux/current;

  writeln('*** Henry:');
  writeln('The flux is: ', flux.ToVerboseString);
  writeln('The current is: ', current.ToVerboseString);
  writeln('The inductance is: ', inductance.ToVerboseString);
  writeln;

  // siemens
  resistance  := 2*Ohm;
  conductance := 1/resistance;

  writeln('*** Siemens:');
  writeln('The resistance is: ', resistance.ToVerboseString);
  writeln('The conductance is: ', conductance.ToVerboseString);
  writeln;

  // coulomb
  current := 5*A;
  time    := 5*s;
  charge  := current*time;

  writeln('*** Coulomb:');
  writeln('The current is: ', current.ToVerboseString);
  writeln('The time is: ', time.ToVerboseString);
  writeln('The charge is: ', charge.ToVerboseString);
  writeln;

  // lumen
  intensity    := 10*cd;
  solidangle   := 90*sr;
  luminousflux := intensity*solidangle;

  writeln('*** Lumen:');
  writeln('The intensity is: ', intensity.ToVerboseString);
  writeln('The solidangle is: ', solidangle.ToVerboseString);
  writeln('The luminous flux is: ', luminousflux.ToVerboseString);
  writeln;

  // sievert & gray
  dose1 := 10*Sv;
  dose2 := 5*Gy;
  dose1 := 10*m2/s2;
  dose2 := 10*m2/s2;
  dose1 := 10*j/kg;
  dose2 := 10*j/kg;

  writeln('*** Sievert and Gray mixing:');
  writeln('The dose1 is: ', dose1.ToVerboseString);
  writeln('The dose2 is: ', dose2.ToVerboseString);
  writeln;

  // newton per meter
  force     := 50*N;
  distance  := 10*mm;
  stiffness := force/distance;

  writeln('*** Newton per meter:');
  writeln('The force is: ', force.ToString);
  writeln('The distance is: ', distance.ToString);
  writeln('The stiffness is: ', stiffness.ToString);
  writeln;

  // density
  mass          := 10*kg;
  volume        := 5*m3;
  density       := mass/volume;
  mass          := density*volume;
  volume        := mass/density;

  writeln('*** Density:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The volume is: ', volume.ToVerboseString);
  writeln('The density is: ', TKilogramPerCubicMeterUnitId.From(density).ToVerboseString);
  writeln;

  // specific weight
  force     := 100*N;
  volume    := 10*m3;
  specificw := force/volume;
  force     := specificw*volume;
  volume    := force/specificw;

  writeln('*** Specific weight:');
  writeln('The force is: ', force.ToVerboseString);
  writeln('The volume is: ', volume.ToVerboseString);
  writeln('The specific weight is: ', specificw.ToVerboseString);
  writeln;

  // sliding friction
  normal := 100*N;
  kA     := 0.05;
  force  := kA*normal;

  writeln('*** Sliding friction:');
  writeln('The normal force is: ', normal.ToVerboseString);
  writeln('The kA is: ', kA:0:3);
  writeln('The force is: ', force.ToVerboseString);
  writeln;

  // rolling friction
  normal := 100*N;
  kAr    := 0.0005*m;
  radius := 50*mm;
  force  := kAr*normal/radius;

  writeln('*** Rolling friction:');
  writeln('The normal force is: ', normal.ToVerboseString);
  writeln('The kAr is: ', kAr.ToVerboseString);
  writeln('The radius is: ', radius.ToVerboseString(4, 4, [pmilli]));
  writeln('The force is: ', force.ToVerboseString);
  writeln;

  // viscosity force (laminar flow)
  eta    := 10*Pa*s;
  radius := 20*mm;
  side1  := 1*m;
  area   := 2*pi*radius*side1;
  speed  := 0.5*m/s;
  force  := 6*pi*radius*eta*speed;
  force  := eta/(radius/speed)*area;

  writeln('*** Viscosity (laminar flow):');
  writeln('The eta coefficent is: ', eta.ToVerboseString);
  writeln('The radius is: ', radius.ToVerboseString);
  writeln('The side1 is: ', side1.ToVerboseString);
  writeln('The area  is: ', area.ToVerboseString);
  writeln('The speed is: ', speed.ToVerboseString);
  writeln('The force is: ', force.ToVerboseString(4, 0, []));
  writeln;

  // drag force
  cCd     := 0.47;
  area    := 1000*mm2;
  speed   := 5*m/s;
  density := 1.225*kg/m3;
  force   := 0.5*cCd*(density*SquarePower(Speed))*area;

  writeln('*** Drag force:');
  writeln('The Cd coefficent is: ', cCd:0:3);
  writeln('The area is: ', area.ToVerboseString);
  writeln('The speed is: ', speed.ToVerboseString);
  writeln('The density is: ', density.ToVerboseString);
  writeln('The force is: ', force.ToVerboseString(4, 0, []));
  writeln;

  // universal gravitation law
  mass1    := 5.972E24*kg;
  mass2    := 7.348E22*kg;
  distance := 384400*km;
  GN       := 6.67E-11*N*m2/kg2;
  GN       := 6.67E-11*N*m2/kg2;
  force    := GN*(mass1*mass2)/(distance*distance);

  writeln('*** Universal gravitation law:');
  writeln('The mass-1 is: ', mass1.ToVerboseString(4, 0, []));
  writeln('The mass-2 is: ', mass2.ToVerboseString(4, 0, []));
  writeln('The distance is: ', distance.ToVerboseString(4, 4, [pKilo]));
  writeln('The G is: ', GN.ToVerboseString);
  writeln('The force is: ', force.ToVerboseString(4, 0, []));
  writeln;

  // gravitational potential energy
  mass     := 10*kg;
  acc      := 9.81*m/s2;
  distance := 10*m;
  Ug       := mass*acc*distance;

  writeln('*** Gravitational potential energy:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The g is: ', acc.ToVerboseString(4, 0, []));
  writeln('The heigth is: ', distance.ToVerboseString);
  writeln('The gravitational potential energy is: ', Ug.ToVerboseString(4, 0, []));
  writeln;

  // kinematic potential energy
  mass  := 10*kg;
  speed := 5*m/s;
  Uc    := 1/2*mass*(speed*speed);

  writeln('*** Kinematic potential energy:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The speed is: ', speed.ToVerboseString);
  writeln('The potential energy is: ', Uc.ToVerboseString);
  writeln;

  // elastic potential energy
  kx       := 10*N/m;
  distance := 10*m;
  Ue       := 0.5*kx*(distance*distance);

  writeln('*** Elastic potential energy:');
  writeln('The ke is: ', ke.ToVerboseString);
  writeln('The distance is: ', distance.ToVerboseString);
  writeln('The Ue is: ', Ue.ToVerboseString);
  writeln;

  // momentum
  mass  := 10*kg;
  speed := 5*m/s;
  p     := mass*speed;

  writeln('*** Momentum:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The speed is: ', speed.ToVerboseString);
  writeln('The momentum is: ', p.ToVerboseString);
  writeln;

  // impulse
  force   := 10*N;
  time    := 5*ms;
  impulse := p;
  impulse := force*time;

  writeln('*** Impulse:');
  writeln('The force is: ', force.ToVerboseString);
  writeln('The time is: ', time.ToVerboseString(4, 4, [pMilli]));
  writeln('The impulse is: ', impulse.ToVerboseString);
  writeln;

  // Stevino's law
  density      := 10*kg/m3;
  acc          := 9.81*m/s2;
  distance     := 2*m;
  pressure     := density*acc*distance;

  writeln('*** Stevino''s law:');
  writeln('The density is: ', density.ToVerboseString);
  writeln('The acceleration is: ', acc.ToVerboseString);
  writeln('The distance is: ', distance.ToVerboseString);
  writeln('The pressure is: ', pressure.ToVerboseString);
  writeln;

  // Archimede's law
  density      := 0.5*kg/m3;
  acc          := 9.81*m/s2;
  volume       := 0.5*m3;
  force        := density*acc*volume;

  writeln('*** Archimede''s law:');
  writeln('The density is: ', density.ToVerboseString);
  writeln('The acceleration is: ', acc.ToVerboseString);
  writeln('The volume is: ', volume.ToVerboseString);
  writeln('The force is: ', force.ToVerboseString);
  writeln;

  // continuity equation (fluid)
  volume   := 50*m3;
  time     := 10*s;
  flowrate := volume/time;

  writeln('*** Continuity equation (fluid):');
  writeln('The volume is: ', volume.ToVerboseString);
  writeln('The time is: ', time.ToVerboseString);
  writeln('The mass flow rate is: ', flowrate.ToVerboseString);
  writeln;

  // Bernoulli's law
  density  := 5*kg/m3;
  speed    := 5*m/s;
  pressure := 1/2*density*(speed*speed);

  writeln('*** Bernoulli''s law:');
  writeln('The density is: ', density.ToVerboseString);
  writeln('The speed is: ', speed.ToVerboseString);
  writeln('The pressure is: ', pressure.ToVerboseString);
  writeln;

  acc         := 9.81*m/s2;
  distance     := 2*m;
  pressure     := density*acc*distance;

  writeln('The acceleration is: ', acc.ToVerboseString);
  writeln('The distance is: ', distance.ToVerboseString);
  writeln('The pressure is: ', pressure.ToVerboseString);
  writeln;

  // Reynolds number
  flowrate := 5*dm3/minute;
  density  := 1.05*g/cm3;
  eta      := 0.003*Pl;
  radius   := 0.9*cm;
  Re       := 2000;
  speed    := Re*eta/(2*density*radius);

  writeln('The critical speed is: ', speed.ToString(4, 0, []));
  writeln;

  // linear thermal expansion
  distance  := 10*m;
  lambda    := 1.2E-5*(1/K);
  deltatemp := 100*K;
  deltadist := distance*(lambda*deltatemp);

  writeln('*** Linear thermal expansion:');
  writeln('The L0 is: ', distance.ToVerboseString);
  writeln('The lambda is: ', lambda.ToVerboseString);
  writeln('The dT is: ', deltatemp.ToVerboseString);
  writeln('The DL is: ', deltadist.ToVerboseString);
  writeln;

  // heat capacity
  mass                 := 10*kg;
  specificheatcapacity := 7.5*J/kg/K;
  heatcapacity         := mass*specificheatcapacity;

  writeln('*** Heat transfer:');
  writeln('The mass is: ', mass.ToVerboseString);
  writeln('The specific heat capacity is: ', specificheatcapacity.ToVerboseString);
  writeln('The thermal capacity is: ', heatcapacity.ToVerboseString);
  writeln;

  // calorimeter
  _m1 := 10*kg;
  _t1 := 100*K;
  _c1 := 7.5*J/kg/K;
  _m2 := 10*kg;
  _t2 := 50*K;
  _c2 := 7.5*J/kg/K;
  _tf := (_m1*_c1*_t1+_m2*_c2*_t2) / (_m1*_c1+_m2*_c2);

  writeln('*** Calorimeter:');
  writeln('The mass of body n.1 is: ', _m1.ToVerboseString);
  writeln('The temperature of body n.1 is: ', _t1.ToVerboseString);
  writeln('The specific heat capacity of body n.1 is: ', _c1.ToVerboseString);
  writeln;
  writeln('The mass of body n.2 is: ', _m2.ToVerboseString);
  writeln('The tempeerature of body n.2 is: ', _t2.ToVerboseString);
  writeln('The specific heat capacity of body n.2 is: ', _c2.ToVerboseString);
  writeln;
  writeln('The final temperature is: ', _tf.ToVerboseString);
  writeln;

  // thermal flux
  area      := 5*m2;
  side1     := 100*mm;
  lambda2   := 1.1*W/m/K;
  deltatemp := 15*K;
  power     := lambda2*(deltatemp/side1)*area;

  writeln('*** Thermal flux:');
  writeln('The surface is: ', area.ToVerboseString);
  writeln('The width is: ', side1.ToVerboseString);
  writeln('The lambda is: ', lambda2.ToVerboseString);
  writeln('The deltatemp is: ', deltatemp.ToVerboseString);
  writeln('The power is: ', power.ToVerboseString);
  writeln;

  // electric potential energy
  ke       := 8.988E9*N*m2/C2;
  q1       := 6E-6*C;
  q2       := 9E-6*C;
  Uel      := ke*(q1*q2/distance);

  writeln('*** Electric potential energy:');
  writeln('The electric potential energy is: ', Uel.ToVerboseString(4, 0, []));
  writeln;

  // electrostatic force on a point charge in a electric field
  E     := 0.0015*N/C;
  q1    := 1.602E-19*C;
  force := E*q1;

  writeln('*** Electrostatic force on a point charge in a electric field:');
  writeln('The electric field strength is: ', E.ToVerboseString(4, 4, []));
  writeln('The charge on the object experiencing the force is: ', q1.ToVerboseString(4, 4, []));
  writeln('The the force on the charged particle is: ', force.ToVerboseString(4, 4, []));
  writeln;

  // electrostatic force between two point charges
  ke       := 8.988E9*N*m2/C2;
  q1       := 6E-6*C;
  q2       := 9E-6*C;
  distance := 2*m;
  force    := ke*(q1*q2)/(distance*distance);

  writeln('*** Electrostatic force between two point charges:');
  writeln('The Coulomb constant ke is: ', ke.ToString(4, 4, []));
  writeln('The q1 is: ', q1.ToString);
  writeln('The q2 is: ', q2.ToString);
  writeln('The distance is: ', distance.ToString);
  writeln('The force is: ', force.ToString(4, 4, []));
  writeln;

  // electric field of a single point charge
  e0       := 8.854187817E-12*F/m;
  er       := 1;
  ke       := 1/(4*pi*e0*er);
  q1       := 2*C;
  r        := 5*cm;
  E        := ke*(q1/SquarePower(r));

  writeln('*** Electric field of a single point charge:');
  writeln('The vacuum permittivity e0 is: ', e0.ToString(4, 4, []));
  writeln('The relative permittivity er is: ', er:0:2);
  writeln('The Coulomb constant ke is: ', ke.ToString(6, 6, []));
  writeln('The single point charge q1 is: ', q1.ToString);
  writeln('The distance from charge is: ', distance.ToString);
  writeln('The electric field strength is: ', E.ToString(4, 4, []));
  writeln;

  // electric field of uniform charge sphere
  e0       := 8.854187817E-12*F/m;
  er       := 1;
  ke       := 1/(4*pi*e0);
  q1       := 2*C;
  r        := 10*cm;
  distance := 5*cm;
  E        := ke*(q1/ (CubicPower(r)/distance));

  writeln('*** Electric field of uniform charge sphere:');
  writeln('The vacuum permittivity e0 is: ', e0.ToString(4, 4, []));
  writeln('The relative permittivity er is: ', er:0:2);
  writeln('The Coulomb constant ke is: ', ke.ToString(6, 6, []));
  writeln('The sphere charge is: ', q1.ToString);
  writeln('The sphere radius is: ', r.ToString);
  writeln('The distance from center of sphere is: ', distance.ToString);
  writeln('The electric field strength is: ', E.ToString(4, 4, []));
  writeln;

  // electric field of parallel conducting plates
  e0       := 8.854187817E-12*F/m;
  er       := 1;
  q1       := 2*C;
  Area     := 4*cm2;
  sigma    := q1/Area;
  E        := sigma/e0/er;
  writeln('*** Electric field of parallel conducting plates:');
  writeln('The vacuum permittivity e0 is: ', e0.ToString(4, 4, []));
  writeln('The relative permittivity er is: ', er:0:2);
  writeln('The plate charge is: ', q1.ToString(4, 4, []));
  writeln('The plate area is: ', Area.ToString(4, 4, []));
  writeln('The charge density sigma is: ', sigma.ToString(4, 4, []));
  writeln('The electric field strength is: ', E.ToString(4, 4, []));
  writeln;

  // magnetic force for lifting a object
  mass      := 100*g;
  acc       := 9.81*m/s2;
  len       := 20*cm;
  B         := 2.0*T;
  current   := (mass*acc)/(len*B*Sin(90*deg));

  writeln('*** Magnetic force for lifting a object:');
  writeln('The mass is: ', mass.ToString(4, 4, []));
  writeln('The wire length is: ', len.ToString(4, 4, []));
  writeln('The magnetic field B is: ', B.ToString(4, 4, []));
  writeln('The needed magnetic force is: ', (mass*acc).ToString(2, 4, []));
  writeln('The needed electric current is: ', current.ToString(2, 4, []));
  writeln;

  // magnetic field due to straight wire
  m0       := 4*pi*1E-7*T*m/A;
  current  := 3.0*A;
  R        := 50*cm;
  z        := 0*cm;
  B        := m0/(2*pi) * (current / (SquareRoot(CubicPower(SquarePower(z)+SquarePower(R)))/SquarePower(R) ));

  writeln('*** Magnetic field due to straight wire:');
  writeln('The vacuum magnetic permeability is: ', m0.ToString(4, 4, []));
  writeln('The electric current is: ', current.ToString(4, 4, []));
  writeln('The distance of the magnetic field from the wire is: ', distance.ToString(4, 4, []));
  writeln('The magnetic field B is: ', B.ToVerboseString(4, 4, []));
  writeln;

  // magnetic field produced by a current-carrying solenoid
  m0       := 4*pi*1E-7*T*m/A;
  current  := 1600*A;
  loops    := 2000;
  len      := 2.0*m;
  B        := m0*loops*(current/len);

  writeln('*** Magnetic field produced by a current-carrying solenoid:');
  writeln('The vacuum magnetic permeability is: ', m0.ToString(4, 4, []));
  writeln('The electric current is: ', current.ToString(4, 4, []));
  writeln('The solenoid has: ', loops, ' loops');
  writeln('The solenoid length is: ', len.ToString(4, 4, []));
  writeln('The magnetic field B magnitude is: ', B.ToVerboseString(3, 4, []));
  writeln;

  // forces between parallel conductors
  m0       := 4*pi*1E-7*T*m/A;
  i1       := 2.5*A;
  i2       := 1.5*A;
  r        := 4*cm;
  len      := 1.0*m;
  force    := (m0/(2*pi)*(len/r)) * (i1*i2);

  writeln('*** Forces between parallel conductors:');
  writeln('The vacuum magnetic permeability is: ', m0.ToString(4, 4, []));
  writeln('The electric current 1 is: ', i1.ToString(4, 4, []));
  writeln('The electric current 2 is: ', i2.ToString(4, 4, []));
  writeln('The distance separating the conductors is: ', r.ToString(4, 4, []));
  writeln('The length of conductors is: ', len.ToString(4, 4, []));
  writeln('The forces between conductors is: ', force.ToString(4, 4, []));
  writeln;

  // magnetic flux
  B            := 0.4*T;
  Area         := 100*cm2;
  angle        := 70*deg;
  magneticflux := B*Area*cos(angle);

  writeln('*** Magnetic flux:');
  writeln('The magnetic field B magnitude is: ', B.ToVerboseString(4, 4, []));
  writeln('The surface is: ', Area.ToVerboseString(3, 4, []));
  writeln('The magnetic flux is: ', magneticflux.ToVerboseString(4, 4, []));
  writeln;

  // electromagnetic induction
  magneticflux := 6*1E-5*Wb;
  time         := 0.1*s;
  potential    := magneticflux/time;

  writeln('*** Electromagnetic induction:');
  writeln('The magnetic flux change is: ', magneticflux.ToVerboseString(4, 4, []));
  writeln('The time is: ', time.ToVerboseString(4, 4, [pMilli]));
  writeln('The emf  is: ', potential.ToVerboseString (4, 4, []));
  writeln;

  // displacement current
  Area     := 100*cm2;
  e0       := 8.854187817E-12*F/m;
  DeltaE   := 6.0E10*N/C;
  time     := 1*s;
  current  := (e0*DeltaE*Area)/time;

  writeln('*** Displacement current:');
  writeln('The plates Aare is: ', Area.ToString(4, 4, []));
  writeln('The vacuum permittivity e0 is: ', e0.ToString(4, 4, []));
  writeln('The change in electric field : ', DeltaE.ToVerboseString(4, 4, []));
  writeln('The change in time is: ', time.ToVerboseString (4, 4, []));
  writeln('The displacement current is: ', current.ToVerboseString (4, 4, []));
  writeln;

  // harmonic wave
  Ampl  := 2*m;
  Kw    := 0.2*rad/m;
  omega := 80*rad/s;
  phi   := 0*rad;
  writeln('*** Harmonic wave:');
  writeln('The wave amplitude A is: ', Ampl.ToString(4, 4, []));
  writeln('The wave number K is: ', Kw.ToString(4, 4, []));
  writeln('The angular frequency Omega is: ', omega.ToString(4, 4, []));
  writeln('The wave phase is: ', phi.ToString(4, 4, []));
  writeln('The wave value at position (1m, 0.8s) is: ', (Ampl*sin(Kw*(1*m) - omega*(0.8*s) + phi)).ToString(4, 4, []));
  writeln('The transverse speed at position (1m, 0.8s) is: ', (-1*omega*Ampl*cos(Kw*(1*m) - omega*(0.8*s) )).ToString(4, 0, []));
  writeln('The transverse acceleration at position (1m, 0.8s) is: ', (-1*SquarePower(omega)*Ampl*cos(Kw*(1*m) - omega*(0.8*s))).ToString(4, 0, []));
  writeln('The wave power is: ', ((1.0*g/m)*SquarePower(omega*Ampl)*(5*mm/s)).ToString(4, 0, []));
  writeln;

  // relativty: energy
  mass       := 1*kg;
  lightspeed := 299792458*m/s;
  energy     := mass*SquarePower(lightspeed);

  writeln('*** Relativistic energy:');
  writeln('The mass "m" is: ', mass.ToString(4, 4, []));
  writeln('The ligth speed c: ', lightspeed.ToString(4, 4, []));
  writeln('The energy E is: ', energy.ToElettronvolt.ToString(4, 0, [pTera]));
  writeln('The energy E is: ', energy.ToString(4, 0, [pTera]));
  writeln;

  // relativty: momentum
  mass       := 9.11*1E-31*kg;
  speed      := 10800000*km/hr;
  p          := mass*speed;
  energy     := SquareRoot(SquarePower(p*lightspeed)+ SquarePower(mass*SquarePower(lightspeed)));

  writeln('*** Relativistic momentum:');
  writeln('The mass "m" is: ', mass.ToString(4, 4, []));
  writeln('The speed "v": ', speed.ToString(4, 4, []));
  writeln('The momentum "p" is: ', p.ToString(4, 4, []));
  writeln('The energy E is: ', energy.ToElettronVolt.ToString(4, 0, [pTera]));
  writeln;

  // momentum of photon
  plank  := 6.62607015*1E-34*J*s;
  len    := 1*mim;
  freq   := 1*Hz;

  energy := plank/(len/lightspeed);
  p      := plank*freq/lightspeed;
  p      := plank/len;
  speed  := len*freq;

  writeln('*** Momentum of photon:');
  writeln('The Plank costant "h" is: ', plank.ToString(4, 4, []));
  writeln('The wavelength "lambda" is: ', len.ToString(4, 4, []));
  writeln('The frequency "f" is: ', freq.ToString(4, 4, []));
  writeln('The energy "E" is: ', energy.ToString(4, 4, []));
  writeln('The momentum "p" is: ', p.ToString(4, 4, []));
  writeln;

  // third kepler's law
  mass1    := 1.989E30*kg;
  mass2    := 5.972E24*kg;
  distance := 1*au;
  GN       := 6.6743E-11*N*m2/kg2;
  time     := SquareRoot( 4*Sqr(pi)*CubicPower(distance)/(GN*(mass1+mass2)) );

  writeln('*** Third Kepler''law: earth period');
  writeln('The mass of sun is:', mass1.toString(5, 0, []));
  writeln('The mass of earth is:', mass2.toString(5, 0, []));
  writeln('The distance earth-sun is:', distance.ToAstronomical.toString(5, 0, []));
  writeln('The earth period is:', time.ToDay.toString(5, 0, []));
  writeln;

  // Earth's gravity
  mass     := 5.972E24*kg;
  GN       := 6.6743E-11*N*m2/kg2;
  distance := 6.373E6*m;

  writeln('*** Earth''s gravity:');
  writeln('The mass of earth is:', mass.toString(5, 0, []));
  writeln('The radius of earth is:', distance.toString(5, 0, []));
  writeln('The value of "g" constant is:', (GN*mass/SquarePower(distance)).toString(5, 0, []));
  writeln;

  // Simple harmonic oscillator
  mass  := 1*kg;
  kx    := 10*N/m;
  omega := SquareRoot(kx/mass);

  writeln('*** Simple harmonic oscillator:');
  writeln('The mass is:', mass.toString);
  writeln('The spring stiffness is: ', kx.toString(4, 0, []));
  writeln('The angular frequency "ω" is: ', omega.toString(4, 0, []));
  writeln;

  // Damped harmonic oscillator
  mass   := 1*kg;
  kx     := 10*N/m;
  Cb     := 10*Pa*s*m;
  omega  := SquareRoot(kx/mass);

  writeln('*** Damped harmonic oscillator:');
  writeln('The mass is: ', mass.toString(4, 0, []));
  writeln('The spring stiffness is: ', kx.toString(4, 0, []));
  writeln('The undamped angular frequency "ω" is: ', omega.toString(4, 0, []));
  writeln('The damping ratio "ζ" is: ', (Cb/2/SquareRoot(mass*kx)):0:4);
  writeln;

  // Physical pendulum
  mass   := 1*kg;
  I      := 10*kg*m2;
  acc    := 9.8*m/s2;
  radius := 20*cm;
  time   := 2*pi*SquareRoot(1/((mass*acc*radius)/I));
  time   := 2*pi*SquareRoot(I/(mass*acc*radius));

  writeln('*** Physical pendulum:');
  writeln('The mass is: ', mass.toString(4, 0, []));
  writeln('The body inertia is: ', I.toString(4, 0, []));
  writeln('The center of gravity distance is: ', radius.toString(4, 0, []));
  writeln('The pendulum period "T" is: ', time.toString(4, 0, [pCenti]));
  writeln;

  writeln('ToString - TEST');

  writeln;
  writeln('TEST PASSED: <',   (1.0*mg  ).ToString(10, 10, [pKilo]),'>');

  writeln('TEST PASSED: <',   (1.0*m/s2).ToMeterPerHourPerSecond.ToString(10, 10, [pKilo, pNone]),'>');

  writeln('TEST PASSED-00: ', (1.0*mg  ).ToString       (10, 10, [pKilo]) = '1E-6 kg');
  writeln('TEST PASSED-01: ', (1.0*mg2 ).ToString       (10, 0,  [pKilo]) = '1E-12 kg2');
  writeln('TEST PASSED-02: ', (1.0*mg  ).ToString       (10, 10, [pMega]) = '1E-9 Mg');
  writeln('TEST PASSED-03: ', (1.0*mg2 ).ToString       (10, 0,  [pMega]) = '1E-18 Mg2');
  writeln('TEST PASSED-04: ', (1.0*kg  ).ToString       (10, 0,  [pNone]) = '1000 g');
  writeln('TEST PASSED-05: ', (1.0*kg  ).ToString       (10, 0,  [pKilo]) = '1 kg');
  writeln('TEST PASSED-06: ', (1.0*kg2 ).ToString       (10, 0,  [pNone]) = '1000000 g2');
  writeln('TEST PASSED-07: ', (1.0*kg2 ).ToString       (10, 0,  [pKilo]) = '1 kg2');
  writeln('TEST PASSED-08: ', (1.0*km  ).ToString       (10, 0,       []) = '1000 m');
  writeln('TEST PASSED-09: ', (1.0*km2 ).ToString       (10, 0,       []) = '1000000 m2');
  writeln('TEST PASSED-10: ', (1.0*kg  ).ToVerboseString(10, 0,  [pNone]) = '1000 gram');
  writeln('TEST PASSED-11: ', (1.0*kg2 ).ToVerboseString(10, 0,  [pNone]) = '1000000 square gram');
  writeln('TEST PASSED-12: ', (1.0*km  ).ToVerboseString(10, 0,       []) = '1000 meter');
  writeln('TEST PASSED-13: ', (1.0*km2 ).ToVerboseString(10, 0,       []) = '1000000 square meter');
  writeln('TEST PASSED-14: ', (1.0*km3 ).ToVerboseString(10, 0,       []) = '1000000000 cubic meter');
  writeln('TEST PASSED-15: ', (1.0*km4 ).ToVerboseString(10, 0,       []) = '1E12 quartic meter');
  writeln('TEST PASSED-16: ', (1.0*day ).ToVerboseString(10, 0,       []) = '86400 second');
  writeln('TEST PASSED-17: ', (1.0*hr  ).ToVerboseString(10, 0,       []) = '3600 second');
  writeln('TEST PASSED-18: ', (1.0*day ).ToString       (10, 0,       []) = '86400 s');
  writeln('TEST PASSED-19: ', (1.0*hr  ).ToString       (10, 0,       []) = '3600 s');
  writeln('TEST PASSED-20: ', (1.0*day2).ToVerboseString(10, 0,       []) = '7464960000 square second');
  writeln('TEST PASSED-21: ', (1.0*hr2 ).ToVerboseString(10, 0,       []) = '12960000 square second');
  writeln('TEST PASSED-22: ', (1.0*day2).ToString       (10, 0,       []) = '7464960000 s2');
  writeln('TEST PASSED-23: ', (1.0*hr2 ).ToString       (10, 0,       []) = '12960000 s2');

  writeln;
  writeln('Press ENTER to exit.');
  readln;
end.
