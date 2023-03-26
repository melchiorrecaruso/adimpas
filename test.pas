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

program Test;

uses ADim;

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
  acceleration: TMetersPerSquareSecond;
  density: TKilogramsPerCubicMeter;
  specificw: TNewtonsPerCubicMeter;
  force, normal: TNewtons;

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
  squaretime: TSquareSeconds;

  angularspeed: TRadiansPerSecond;

  kA: double;
  kAr: TMeters;
  radius: TMeters;


  mass: TKilograms;
  eta: TPascalsSecond;

  mass1: TKilograms;
  mass2: TKilograms;
  squaremass: TSquareKilograms;
  GN: TNewtonsSquareMeterPerSquareKilogram;


  Cd: double;
  angle: TRadians;

  Uc: TJoules;
  Ug: TJoules;
  g: TMetersPerSquareSecond;

  Ue: TJoules;
  ke: TNewtonsPerMeter;

  kel: TNewtonsSquareMeterPerSquareCoulomb;
  q1: TCoulombs;
  q2: TCoulombs;
  Uel: TJoules;

  p: TKilogramsMeterPerSecond;
  impulse: TNewtonsSecond;

  flowrate: TCubicMetersPerSecond;


  lambda: TReciprocalKelvins;
  deltadist: TMeters;
  deltatemp: TKelvins;

  heatcapacity: TJoulesPerKilogramPerKelvin;
  thermalcapacity: TJoulesPerKelvin;



  _m1: TKilograms;
  _m2: TKilograms;
  _tf: TKelvins;
  _t1: TKelvins;
  _t2: TKelvins;
  _c1: TJoulesPerKilogramPerKelvin;
  _c2: TJoulesPerKilogramPerKelvin;

  lambda2: TWattsPerMeterPerKelvin;


  (*

  kAv2: TNewtonSquareSecondPerSquareMeter;

  *)


begin
                                           writeln('*** area:');
  side1         := 10*m;                   writeln('The side-1 is: ', side1.ToString);
  side2         := 5*m;                    writeln('The side-2 is: ', side2.ToString);
  area          := side1*side2;            writeln('The area is: ', area.ToString);
                                           writeln;


                                           writeln('*** volume:');
  side3         := 2*m;                    writeln('The side-3 is: ', side3.ToString);
  volume        := area*side3;             writeln('The volume is: ', volume.ToString);
                                           writeln;


                                           writeln('*** hyper volume:');
  side4         := 7*m;                    writeln('The side-4 is: ', side4.ToString);
  hypervolume   := volume*side4;           writeln('The hyper volume is: ', hypervolume.ToString);
                                           writeln;


                                           writeln('*** speed:');
  distance      := 20*m;                   writeln('The distance is: ', distance.ToString);
  time          := 5*s;                    writeln('The time is: ', time.ToString);
  speed         := distance/time;          writeln('The speed is: ', speed.ToString);
                                           writeln;

  speed         := 10*(km/hr);

                                           writeln('*** acceleration:');
  acceleration  := speed/time;             writeln('The acceleration is: ', acceleration.ToString);
  acceleration  := distance/(time*time);   writeln('The acceleration is: ', acceleration.ToString);
                                           writeln;


  mass         := 10*kg;                   writeln('*** force:');
  acceleration := 10*(m/s2);
  force        := mass*acceleration;
  force        := acceleration*mass;       writeln('The force is: ', force.ToVerboseString);
  mass         := force/acceleration;      writeln('The mass is: ', mass.ToVerboseString);
  acceleration := force/mass;              writeln('The acceleration is: ', acceleration.ToVerboseString);
                                           writeln;


                                           writeln('*** pressure');
  force         := 10*N;                   writeln('The force is: ', force.ToString);
  area          := 5*m2;                   writeln('The area: ', area.ToString);
  pressure      := force/area;             writeln('The pressure: ', pressure.ToString);
                                           writeln;


                                           writeln('*** work:');
   force        := 10*N;                   writeln('The force is: ', force.ToVerboseString);
   distance     := 5*m;                    writeln('The distance is: ', distance.ToVerboseString);
   work         := force*distance;         writeln('The work is: ', work.ToVerboseString);
                                           writeln;


                                           writeln('*** power:');
   work         := 50*J;                   writeln('The work is: ', work.ToVerboseString);
   time         := 10*s;                   writeln('The time is: ', time.ToVerboseString);
   power        := work/time;              writeln('The power is: ', power.ToVerboseString);
                                           writeln;


                                           writeln('*** volt (J/C):');
   work          := 50*J;                  writeln('The work is: ', work.ToVerboseString);
   charge        := 25*C;                  writeln('The charge is: ', charge.ToVerboseString);
   potential     := work/charge;           writeln('The potential is: ', potential.ToVerboseString);
                                                                                     writeln;

                                           writeln('*** volt (W/A):');
   current       := 5*A;                   writeln('The current is: ', current.ToVerboseString);
   power         := 10*W;                  writeln('The power is: ', power.ToVerboseString);
   potential     := power/current;         writeln('The potential is: ', potential.ToVerboseString);
                                                                                     writeln;

                                           writeln('*** farad (C2/J):');
   work          := 50*J;                  writeln('The work is: ', work.ToVerboseString);
   squarecharge  := 25*C2;                 writeln('The square charge is: ', squarecharge.ToVerboseString);
   capacitance   := squarecharge/work;     writeln('The capacitance is: ', capacitance.ToVerboseString);
                                           writeln;

                                           writeln('*** farad (C/V):');
   potential     :=  5*V;                  writeln('The potential is: ', potential.ToString);
   charge        :=  10*C;                 writeln('The charge is: ', charge.ToString);
   capacitance   := charge/potential;      writeln('The capacitance is: ', capacitance.ToString);
                                           writeln;

                                           writeln('*** weber:');
   potential     := 5*V;                   writeln('The potential is: ', potential.ToString);
   time          := 10*s;                  writeln('The time is: ', time.ToString);
   flux          := potential*time;        writeln('The flux is: ', flux.ToVerboseString);
                                           writeln;


                                           writeln('*** tesla:');
   area          := 10*m2;                 writeln('The area is: ', area.ToVerboseString);
   flux          := 25*Wb;                 writeln('The flux is: ', flux.ToVerboseString);
   fluxdensity   := flux/area;             writeln('The flux density is: ', fluxdensity.ToVerboseString);
                                           writeln;

                                           writeln('*** henry:');
   flux          := 30*Wb;                 writeln('The flux is: ', flux.ToVerboseString);
   current       := 10*A;                  writeln('The current is: ', current.ToVerboseString);
   inductance    := flux/current;          writeln('The inductance is: ', inductance.ToVerboseString);
                                           writeln;

                                           writeln('*** siemens:');
   resistance    := 2*Ohm;                 writeln('The resistance is: ', resistance.ToVerboseString);
   conductance   := 1/resistance;          writeln('The conductance is: ', conductance.ToVerboseString);
                                           writeln;

                                           writeln('*** coulomb:');
   current       := 5*A;                   writeln('The current is: ', current.ToVerboseString);
   time          := 5*s;                   writeln('The time is: ', time.ToVerboseString);
   charge        := current*time;          writeln('The charge is: ', charge.ToVerboseString);
                                           writeln;

                                           writeln('*** lumen:');
   intensity     := 10*ADim.cd;            writeln('The intensity is: ', intensity.ToVerboseString);
   solidangle    := 90*sr;                 writeln('The solidangle is: ', solidangle.ToVerboseString);
   luminousflux  := intensity*solidangle;  writeln('The luminous flux is: ', luminousflux.ToVerboseString);
                                           writeln;


                                           writeln('*** sievert and gray mixing:');
   dose1         := 10*Sv;                 writeln('The dose1 is: ', dose1.ToVerboseString);
   dose2         := 5*Gy;                  writeln('The dose2 is: ', dose2.ToVerboseString);
   //dose1         := 10*(m2/s2);
   //dose2         := 10*(m2/s2);
   dose1         := 10*(j/kg);
   dose2         := 10*(j/kg);
                                           writeln;


                                             writeln('*** alternative volt definition:');
   power         := 10*w;                    writeln('The power is: ', power.ToVerboseString);
   resistance    := 50*Ohm;                  writeln('The resistance is: ', resistance.ToVerboseString);
   squarevolt    := power*resistance;        writeln('The squarevolt is: ', squarevolt.ToString);
   potential     := SquareRoot(squarevolt);  writeln('The potential is: ', potential.ToVerboseString);
                                                                                     writeln;

                                             writeln('*** alterantive ampere definition:');
   power         := 10*w;                    writeln('The power is: ', power.ToVerboseString);
   resistance    := 50*Ohm;                  writeln('The resistance is: ', resistance.ToVerboseString);
   squareamp     := power/resistance;        writeln('The squareamp is: ', squareamp.ToString);
   current       := SquareRoot(squareamp);   writeln('The current is: ', current.ToVerboseString);
                                             writeln;


                                           writeln('*** stiffnes:');
  force         := 50*N;                   writeln('The force is: ', force.ToString);
  distance      := 10*mm;                  writeln('The distance is: ', distance.ToString);
  stiffness     := force/distance;         writeln('The stiffness is: ', stiffness.ToString);
                                           writeln;


  volume        := 5*mm3;                  writeln('*** density:');
  density       := 150*(kg/mm3);
  mass          := density*volume;         writeln('The mass is: ', mass.ToVerboseString);
  volume        := mass/density;           writeln('The volume is: ', mm3.From(volume).ToVerboseString);
  density       := mass/volume;            writeln('The density is: ', (kg/mm3).From(density).ToVerboseString);
                                           writeln;


  force        := 100*N;                   writeln('*** specific weight:');
  volume       := 10*m3;
  specificw    := force/volume;            writeln('The specific weight is: ', specificw.ToVerboseString);
  volume       := force/specificw;         writeln('The volume is: ', volume.ToVerboseString);
  force        := specificw*volume;        writeln('The force is: ', force.ToVerboseString);
                                           writeln;


  kA           := 0.15;                    writeln('*** sliding friction:');
  normal       := 100*N;                   writeln('The normal force is: ', normal.ToVerboseString);
  force        := kA*normal;               writeln('The force is: ', force.ToVerboseString);
  ka           := force/normal;            writeln('The kA is: ', kA:0:3);
                                           writeln;


  kAr          := 0.15*m;                  writeln('*** rolling friction:');
  normal       := 100*N;                   writeln('The normal force is: ', force.ToVerboseString);
  radius       := 5*m;                     writeln('The radius is: ', radius.ToVerboseString);
  force        := kAr*normal/radius;       writeln('The force is: ', force.ToVerboseString);
  force        := normal*kAr/radius;
  kAr          := force/normal*radius;     writeln('The kAr is: ', kAr.ToVerboseString);
                                           writeln;


  eta          := 10*(Pa*s);               writeln('*** viscosity (laminar flow):');
  radius       := 20*mm;                   writeln('The radius is: ', radius.ToVerboseString);
  side1        := 1*m;                     writeln('The side1 is: ', side1.ToVerboseString);
  area         := 2*pi*radius*side1;       writeln('The area  is: ', area.ToVerboseString);
  speed        := 0.5*(m/s);               writeln('The speed is: ', speed.ToVerboseString);
  force        :=
    -1*eta*(speed/radius)*area;            writeln('The force is: ', force.ToVerboseString);
                                           writeln;


  Cd           := 0.47;                    writeln('*** drag force:');
  area         := pi*radius*radius;        writeln('The area is: ', area.ToVerboseString);
  speed        := 5*(m/s);                 writeln('The speed is: ', speed.ToVerboseString);
  density      := 1.225*(kg/m3);           writeln('The density is: ', density.ToVerboseString);

  force        := 0.5*Cd*(density*SquarePower(Speed))*area;

                                           writeln('The force is: ', force.ToVerboseString);
                                           writeln;


                                           writeln('*** angular speed:');
  angle        := 2*pi*rad;                writeln('The angle is: ', angle.ToVerboseString);
  time         := 1*s;                     writeln('The time is: ', time.ToVerboseString);
  angularspeed := angle/time;              writeln('The angular speed is: ', angularspeed.ToVerboseString);
  radius       := 10*m;                    writeln('The radius is: ', radius.ToVerboseString);
  speed        := angularspeed*radius;     writeln('The speed is: ', speed.ToVerboseString);
                                           writeln;

                                                            writeln('*** centrifugal force:');
  mass         := 1*kg;                                     writeln('The mass is: ', mass.ToVerboseString);
  angularspeed := 2*pi*(rad/s);                             writeln('The angular speed is: ', angularspeed.ToVerboseString);
  radius       := 10*m;                                     writeln('The radius is: ', radius.ToVerboseString);
  acceleration := (angularspeed*angularspeed)*radius;
  acceleration := SquarePower(angularspeed*radius)/radius;  writeln('The acceleration is: ', acceleration.ToVerboseString);
  force        := mass*acceleration;                        writeln('The force is: ', force.ToVerboseString);
                                                            writeln;


  mass1        := 5.972E24*kg;             writeln('*** universal gravitation law:');
  mass2        := 7.348E22*kg;
  squaremass   := mass1*mass2;             writeln('The mass product is: ', squaremass.ToVerboseString(4,0));
  distance     := 384400*km;               writeln('The distance is: ', km.From(distance).ToVerboseString);
  GN           := 6.67E-11*(N*(m2/kg2));
  GN           := 6.67E-11*(N*m2/kg2);     writeln('The G is: ', GN.ToVerboseString);
  force        := GN*squaremass/SquarePower(distance);  writeln('The force is: ', force.ToVerboseString(4, 0));
  force        := GN*(mass1*mass2)/(distance*distance); writeln('The force is: ', force.ToVerboseString(4, 0));
                                                        writeln;














                                            writeln('*** kinematic potential energy:');
  mass         := 10*kg;                    writeln('The mass is: ', mass.ToVerboseString);
  speed        := 5*(m/s);                  writeln('The speed is: ', speed.ToVerboseString);
  Uc           := 1/2*mass*(speed*speed);
  Uc           := 1/2*mass*(speed*speed);   writeln('The potential energy is: ', Uc.ToVerboseString);
                                            writeln;

                                            writeln('*** gravitational potential energy:');
  mass         := 10*kg;                    writeln('The mass is: ', mass.ToVerboseString);
  g            := 9.81*(m/s2);              writeln('The g is: ', g.ToVerboseString(4, 0));
  distance     := 10*m;                     writeln('The heigth is: ', distance.ToVerboseString);
  Ug           := mass*g*distance;          writeln('The gravitational potential energy is: ', Ug.ToVerboseString(4, 0));
                                            writeln;

                                                   writeln('*** elastic potential energy:');
  ke           := 10*(N/m);                        writeln('The ke is: ', ke.ToVerboseString);
  distance     := 10*m;                            writeln('The distance is: ', distance.ToVerboseString);
  Ue           := 1/2*ke*distance*distance;
  Ue           := 1/2*ke*(distance*distance);      writeln('The Ue is: ', Ue.ToVerboseString);
                                                   writeln;

                                                   writeln('*** electrostatic force:');
  kel          := 9E9*(N*m2/C2);                   writeln('The K.el is: ', kel.ToVerboseString(4, 0));
  q1           := 6E-6*C;                          writeln('The q1 is: ', q1.ToVerboseString);
  q2           := 9E-6*C;                          writeln('The q2 is: ', q2.ToVerboseString);
  distance     := 2*m;                             writeln('The distance is: ', distance.ToVerboseString);
  force        := kel*(q1*q2)/(distance*distance); writeln('The force is: ', force.ToVerboseString(4, 0));
                                                   writeln;


                                         writeln('*** electric potential energy:');
  Uel          := kel*(q1*q2/distance);  writeln('The electric potential energy is: ', Uel.ToVerboseString(4, 0));
                                         writeln;

                                         writeln('*** momentum:');
  mass         := 10*kg;                 writeln('The mass is: ', mass.ToVerboseString);
  speed        := 5*(m/s);               writeln('The speed is: ', speed.ToVerboseString);
  p            := mass*speed;            writeln('The momentum is: ', p.ToVerboseString);
                                         writeln;

                                         writeln('*** impulse:');
  force        := 10*N;                  writeln('The force is: ', force.ToVerboseString);
  time         := 5*ms;                  writeln('The time is: ', ms.From(time).ToVerboseString);
  impulse      := force*time;            writeln('The impulse is: ', impulse.ToVerboseString);
  impulse      := p;                     writeln;

                                                  writeln('*** Stevino''s law:');
  density      := 10*(kg/m3);                     writeln('The density is: ', density.ToVerboseString);
  acceleration := 9.81*(m/s2);                    writeln('The acceleration is: ', acceleration.ToVerboseString);
  distance     := 2*m;                            writeln('The distance is: ', distance.ToVerboseString);
  pressure     := density*acceleration*distance;  writeln('The pressure is: ', pressure.ToVerboseString);
                                                  writeln;

                                                  writeln('*** Archimede''s law:');
  density      := 0.5*(kg/m3);                    writeln('The density is: ', density.ToVerboseString);
  acceleration := 9.81*(m/s2);                    writeln('The acceleration is: ', acceleration.ToVerboseString);
  volume       := 0.5*m3;                         writeln('The volume is: ', volume.ToVerboseString);
  force        := density*acceleration*volume;    writeln('The force is: ', force.ToVerboseString);
                                                  writeln;

                                                  writeln('*** continuity equation (fluid):');
  volume       := 50*m3;                          writeln('The volume is: ', volume.ToVerboseString);
  time         := 10*s;                           writeln('The time is: ', time.ToVerboseString);
  flowrate     := volume/time;                    writeln('The mass flow rate is: ', flowrate.ToVerboseString);
                                                  writeln;

                                                  writeln('*** Bernoulli''s law:');
  density      := 5*(kg/m3);                      writeln('The density is: ', density.ToVerboseString);
  speed        := 5*(m/s);                        writeln('The speed is: ', speed.ToVerboseString);
  pressure     := 1/2*density*(speed*speed);      writeln('The pressure is: ', pressure.ToVerboseString);
                                                  writeln;
  acceleration := 9.81*(m/s2);                    writeln('The acceleration is: ', acceleration.ToVerboseString);
  distance     := 2*m;                            writeln('The distance is: ', distance.ToVerboseString);
  pressure     := density*acceleration*distance;  writeln('The pressure is: ', pressure.ToVerboseString);
                                                  writeln;

                                                  writeln('Linear thermal expansion:');
  distance     := 10*m;                           writeln('The L0 is: ', distance.ToVerboseString);
  lambda       := 1.2E-5*(1/K);                   writeln('The lambda is: ', lambda.ToVerboseString);
  deltatemp    := 100*K;                          writeln('The DT is: ', deltatemp.ToVerboseString);
  deltadist    := distance*(lambda*deltatemp);    writeln('The DL is: ', deltadist.ToVerboseString);
                                                  writeln;

                                                  writeln('Heat transfer:');
  mass            := 10*kg;                       writeln('The mass is: ', mass.ToVerboseString);
  heatcapacity    := 7.5*(J/kg/K);                writeln('The specific heat capacity is: ', heatcapacity.ToVerboseString);
  thermalcapacity := mass*heatcapacity;           writeln('The thermal capacity is: ', thermalcapacity.ToVerboseString);

                                                  writeln;
                                                  writeln('calorimeter:');
  _m1           := 10*kg;                         writeln('The mass of body n.1 is: ', _m1.ToVerboseString);
  _t1           := 100*K;                         writeln('The temperature of body n.1 is: ', _t1.ToVerboseString);
  _c1           := 7.5*(J/kg/K);                  writeln('The specific heat capacity of body n.1 is: ', _c1.ToVerboseString);
                                                  writeln;
  _m2           := 10*kg;                         writeln('The mass of body n.2 is: ', _m2.ToVerboseString);
  _t2           := 50*K;                          writeln('The tempeerature of body n.2 is: ', _t2.ToVerboseString);
  _c2           := 7.5*(J/kg/K);                  writeln('The specific heat capacity of body n.2 is: ', _c2.ToVerboseString);
                                                  writeln;
  _tf := (_m1*_c1*_t1+_m2*_c2*_t2)/
    (_m1*_c1+_m2*_c2);                            writeln('The final temperature is: ', _tf.ToVerboseString);
                                                  writeln;

                                                  writeln('thermal flux:');
   area         := 5*m2;                          writeln('The surface is: ', area.ToVerboseString);
   side1        := 100*mm;                        writeln('The width is: ', side1.ToVerboseString);
   lambda2      := 1.1*(W/m/K);                   writeln('The lambda is: ', lambda2.ToVerboseString);
   deltatemp    := 15*K;                          writeln('The deltatemp is: ', deltatemp.ToVerboseString);
   power        := lambda2*(deltatemp/side1)*area;writeln('The power is: ', power.ToVerboseString);
   time         := 1*day;                         writeln('The time is: ', hr.From(time).ToVerboseString);
   work         := power*time;                    writeln('The energy is: ', work.ToVerboseString);
                                                  writeln;

  // end test
  readln;
end.
