# ADimPas

Another unit for type checking of dimensioned quantities at compile time in FreePascal. From a [circular's](https://github.com/circular17/DimPas) idea.

## <u>Requirements:</u>

- FreePascal (trunk version)

## <u>Supported Units:</u>

- ampere $[A]$
- becquerel $[Bq]$
- coulomb $[C]$
- cubic meter $[m^3]$
- cubic meter per kelvin $[m^3/K]$
- cubic meter per second $[m^3/s]$
- farad $[F]$
- gray $[Gy]$
- henry $[H]$
- joule $[J]$
- joule per kelvin $[J/K]$
- joule per kilogram $[J/kg]$
- joule per kilogram per kelvin $[J/(kg·K)]$
- joule per mole $[J/mol]$
- joule per mole per kelvin $[J/(mol·K)]$
- joule per radian $[J/rad]$
- katal $[kat]$
- kelvin per meter $[K/m]$
- kilogram $[kg]$
- kilogram kelvin $[kg·K]$
- kilogram meter per second $[kg·m/s]$
- kilogram per cubic meter $[kg/m^3]$
- lumen $[lm]$
- meter $[m]$
- meter kelvin $[m·K]$
- meter per second $[m/s]$
- meter per square second $[m/s^2]$
- mole kelvin $[mol·K]$
- newton $[N]$
- newton meter $[N·m]$
- newton meter per radian $[N·m/rad]$
- newton per cubic meter $[N/m^3]$
- newton per meter $[N/m]$
- newton per square coulomb $[N/C^2]$
- newton per square kilogram $[N/kg^2]$
- newton second $[N·s]$
- newton square meter $[N·m^2]$
- newton square meter per square coulomb $[N·m^2/C^2]$
- newton square meter per square kilogram $[N·m^2/kg^2]$
- ohm $[Ω]$
- ohm meter $[Ω·m]$
- pascal $[Pa]$
- pascal per kelvin $[Pa/K]$
- pascal second $[Pa·s]$
- quartic meter $[m^4]$
- radian $[rad]$
- radian per second $[rad/s]$
- radian per square second $[rad/s^2]$
- reciprocal coulomb $[1/C]$
- reciprocal ohm $[1/Ω]$
- second $[s]$
- siemens $[S]$
- siemens per meter $[S/m]$
- sievert $[Sv]$
- square coulomb per meter $[C^2/m]$
- square coulomb per newton $[C^2/N]$
- square coulomb per newton per square meter $[C^2/(N·m^2)]$
- square coulomb per square meter $[C^2/m^2]$
- square kilogram $[kg^2]$
- square kilogram per square meter $[kg^2/m^2]$
- square meter $[m^2]$
- square meter kelvin $[m^2·K]$
- square meter per second $[m^2/s]$
- square meter per square coulomb $[m^2/C^2]$
- square meter per square kilogram $[m^2/kg^2]$
- square meter per square second $[m^2/s^2]$
- square meter quartic kelvin $[m^2·K^4]$
- square radian per square second $[rad^2/s^2]$
- steradian $[sr]$
- tesla $[T]$
- volt $[V]$
- watt $[W]$
- watt per kelvin $[W/K]$
- watt per meter $[W/m]$
- watt per meter per kelvin $[W/(m·K)]$
- watt per quartic kelvin $[W/K^4]$
- watt per square meter $[W/m^2]$
- watt per square meter per kelvin $[W/(m^2·K)]$
- watt per square meter per quartic kelvin $[W/(m^2·K^4)]$
- weber $[Wb]$

## <u>Supported Identifiers:</u>

- decagram per cubic meter $[dag/m^3]$
- gram per cubic meter $[g/m^3]$
- hectogram per cubic meter $[hg/m^3]$
- kilogram per cubic centimeter $[kg/cm^3]$
- kilogram per cubic decimeter $[kg/dm^3]$
- kilogram per cubic millimeter $[kg/mm^3]$
- newton meter per degree $[N·m/deg]$
- newton millimeter $[N·mm]$
- newton millimeter per degree $[N·mm/deg]$
- newton millimeter per radian $[N·mm/rad]$
- newton per centimeter $[N/cm]$
- newton per decimeter $[N/dm]$
- newton per millimeter $[N/mm]$

## <u>Supported equations:</u>

### Archimede's principle

$F_a = ρ·g·V$ ,

where
- $F_a$ denotes the buoyant force applied onto the submerged object, $[N]$,
- $ρ$ is the the density of the fluid, $[kg/m^3]$,
- $g$ is the acceleration due to gravity, $[m/s^2]$,
- $V$ represents the volume of the displaced fluid, $[m^3]$.

### Bernoulli's law

$p + ρ·g·h + 1/2·ρ·v² = costant$ ,

where 
- $p$ is the pressure at the chosen point, $[Pa]$,
- $ρ$ is the density of the fluid at all points in the fluid, $[kg/m^3]$,
- $g$ is the acceleration due to gravity, $[m/s^2]$,
- $h$ is the mean potential elevation of the section, $[m]$,
- $v$ is the fluid flow speed at a point, $[m/s]$.

### Calorimeter equation

$m_1·c_1·(T_e-T_1) = m_2·c_2·(T_2-T_e)$ ,

where
- $m_1$ is the mass of body 1, $[kg]$,
- $m_2$ is the mass of body 2, $[kg]$,
- $c_1$ is specific heat capacity of body 1, $[J/(kg·K)]$,
- $c_2$ is specific heat capacity of body 2, $[J/(kg·K)]$,
- $T_1$ is the initial temperature of body 1, $[K]$,
- $T_2$ is the initial temperature of body 2, $[K]$,
- $T_e$ is the final temperature of bodies, $[K]$.

### Continuity equation (fluid)

$q = V/t=A·v$ ,

where
- $q$ is the volumetric flow rate, $[m^3/s]$,
- $V$ is then volume of fluid, $[m^3]$,
- $t$ is the unit of time, $[s]$,
- $A$ is the cross-sectin area, $[m^2]$,
- $v$ is the speed of fluid, $[m/s]$.

### Drag force

$F = 1/2·(ρ·v^2)·C_d·A$ ,

where 
- $ρ$ is the the density of the fluid, $[kg/m^3]$,
- $v$ is the speed of the object relative to the fluid, $[m/s]$,
- $A$ is the cross sectional area, $[m^2]$,
- $C_d$ is the drag coefficient.

### Electrical conductivity

$σ = 1/ρ$ ,  

where 
- $ρ$ is the electrical resistivity, $[Ω·m]$.

### Electrical resistivity

$ρ = R·(A/L)$ ,

where 
- $R$ is the electrical resistance, $[Ω]$,
- $L$ is the length of the specimen, $[m]$,
- $A$ is the cross-sectional area of the specimen, $[m^2]$.

### Electrostatic force

$F_e = k_e·(q_1·q_2)/r^2$ ,

where 
- $k_e$ is the Coulomb constant, $k_e ≈ 8.988×10^9$ $[N⋅m^2/C^2]$,
- $q_1$ is the charge 1, $[C]$,
- $q_2$ is the charge 2, $[C]$,
- $r$ is the distance between the charges, $[m]$.

### Electrostatic potential energy

$U_e = k_e·(q_1·q_2)/r$ ,

where
- $k_e$ is the Coulomb constant, $k_e ≈ 8.988×10^9$ $[N⋅m^2/C^2]$,
- $q_1$ is the charge 1, $[C]$,
- $q_2$ is the charge 2, $[C]$,
- $r$ is the distance between the charges.

### Elastic potential energy

$U_e = 1/2·k_e·Δx^2$ ,

where
- $k_e$ is elastic costant, $[N/m]$,
- $x$ is the defomation, $[m]$.

### Gravitational potential energy

$U_g = G·[(m_1·m_2)/r]$ ,

where
- $m_1$ is the mass of the particle 1, $[kg]$,
- $m_2$ is the mass of the particle 2, $[kg]$,
- $G$ is the gravitational constant, $[m^3/(kg⋅s^2)]$,
- $r$ is the distance between particles, $[m]$.

### Heat capacity

$Q = (m·c)·ΔT$ ,

where 
- $m$ is the mass of body, $[kg]$,
- $c$ is the specific heat capacity, $[J/(kg·K)]$,
- $ΔT$ is the change in temperature, $[K]$.

### Heat conduction

$Q̇ = [k·(ΔT/L)]·A$ ,

where 
- $k$ is the material's conductivity, $[W/(m·K)]$,
- $ΔT$ is the temperature difference between the ends, $[K]$,
- $L$  is the distance between the ends, $[m]$,
- $A$ is the cross-sectional surface area, $[m^2]$.

### Heat convection

$Q̇ = [h·(T-T_f)]·A$ , 

where
- $h$ is the heat transfer coefficient, $[W/(m^2·K)]$,
- $T$ T is the object's surface temperature, $[K]$,
- $T_f$ T is the fluid temperature, $[K]$,
- $A$ is the area of the object, $[m^2]$.

### Ideal gas law:

$p·V = (n·R)·T$ , 

where
- $p$ is the pressure, $[Pa]$,
- $V$ is the volume, $[m^3]$,
- $n$ is the amount of substance, $[mol]$,
- $R$ is the ideal gas constant, $R = 8.314$ $[J/(mol·K)]$,
- $T$ is the absolute temperature of the gas, $[K]$.

### Impulse

$J = F·Δt$ ,

where
- $J$ is impulse, $[N·s]$,
- $F$ is the applied force, $[N]$, 
- $Δt$ is time interval, $[s]$.

### Kinetic energy

$E_c = 1/2·m·v^2$ ,

where
- $m$ is the mass of the body, $[kg]$,
- $v$ is the velocity of the body, $[m/s]$.

### Linear thermal expansion

$ΔL = L_0·(λ·ΔT$)$ ,

where
- $L_0$ is the initial length, $[m]$, 
- $λ$ is the lienar coefficient of thermal expansion, $[1/K]$, 
- $ΔT$ is the difference of the temperature between the two recorded strains, $[K]$.

### Momentum

$p = m·v$ ,

where
- $p$ is the momentum, $[kg·m/s]$,
- $m$ is the mass of the body, $[kg]$,
- $v$ is the speed of the body, $[m/s]$.

### Stevino's law

$p = p_0 + ρ·g·h$ ,

where
- $p_0$ is the initial pressure, $[Pa]$,
- $ρ$ is the density of the flow, $[kg/m^3]$,
- $g$ is the acceleration due to gravity, $[m/s^2]$,
- $h$ where h is the height $(z − z_0)$ of the liquid column between the test volume and the zero reference point of the pressure, $[m]$. 

### Thermal radiation

$Q = (ε·σ·T^4)·A$ ,

where 
- $ε$ is the surcafe emissivity factor, a dimensionless number,
- $σ$ is the Stefan–Boltzmann constant, $[W/(m^2·K^4)]$,
- $T$ is the absolute temperature of the body, $[K]$, 
- $A$ is the surface area of the body. $[m^2]$.

### Universal gravitation law

$F_g = G·[(m_1·m_2)/r^2]$ ,

where 
- $G$ is the gravitational constant, $[m^3/(kg⋅s^2)]$,
- $m_1$ is the mass of the particle 1, $[kg]$,
- $m_2$ is the mass of the particle 2, $[kg]$,
- $r$ is the distance between particles, $[m]$.

### Vacuum permittivity

$ε_0 = 1/(4⋅π⋅k_e)$ ,

where
- $k_e$ is the Coulomb constant, $k_e ≈ 8.988×10^9$ $[N⋅m^2/C^2]$.

### Viscous friction (laminar flow)

$F/A = μ·(u/y)$ ,

where 
- $μ$ is the dynamic viscosity of the fluid, $[Pa⋅s]$,
- $u/y$ is the rate of shear deformation or shear velocity, $[1/s]$,
- $A$ is the area of each plate, $[m^2]$.

### Kinematic viscosity

$ν = μ/ρ$ ,

where
- $μ$ is the dynamic viscosity of the fluid, $[Pa⋅s]$,
- $ρ$ is the the density of the fluid, $[kg/m^3]$.

## LICENSE

[GNU Lesser General Public License v3.0](https://github.com/melchiorrecaruso/ADimPas/blob/main/LICENSE)
