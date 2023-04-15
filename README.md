# ADimPas

Another unit for type checking of dimensioned quantities at compile time in FreePascal. From a [circular's](https://github.com/circular17/DimPas) idea.

## <u>Requirements:</u>

- FreePascal (trunk version)

## <u>Supported Units:</u>
- kilogram $[kg]$
- meter $[m]$
- second $[s]$
- ampere $[A]$
- radian $[rad]$
- steradian $[sr]$
- square meter $[m^2]$
- cubic meter $[m^3]$
- quartic meter $[m^4]$
- meter per second $[m/s]$
- meter per square second $[m/s^2]$
- radian per second $[rad/s]$
- radian per square second $[rad/s^2]$
- newton $[N]$
- pascal $[Pa]$
- joule $[J]$
- newton meter $[N·m]$ 
- watt $[W]$
- coulomb $[C]$
- volt $[V]$
- farad $[F]$
- ohm $[Ω]$
- siemens $[S]$
- weber $[Wb]$
- tesla $[T]$
- henry $[H]$
- lumen $[lm]$
- becquerel $[Bq]$
- gray $[Gy]$
- sievert $[Sv]$
- katal $[kat]$
- kilogram per cubic meter $[kg/m^3]$
- newton per cubic meter $[N/m^3]$
- pascal second $[Pa·s]$
- newton square meter per square kilogram $[N·m^2/kg^2]$
- newton per meter $[N/m]$
- newton square meter per square coulomb $[N·m^2/C^2]$
- kilogram meter per second $[kg·m/s]$

## <u>Supported equation</u>

### Momentum:

$p = m·v$

### Impulse:

$dp = F·dt$

### Electrostatic force:

$F_e = k_e·(q_1·q_2)/r^2$    where $k_e$ is the Coulomb constant.

### Universal gravitation law

$F_g = G·(m_1·m_2)/r^2$    where $G$ is universal gravitational constant.

### Kinematic potential energy

$U_c = 1/2·m·v^2$

### Gravitational potential energy

$U_g = m·g·h$

### Elastic potential energy

$U_e = 1/2·k_e·x^2$    where $k_e$ is elestic costant.

### Stevino's law

$P = ρ·g·h$

### Archimede's law

$F_a = ρ·g·V$

### Continuity equation (fluid)

$q = dV/t$

### Bernoulli's law

$p_0 + ρ·g·h + 1/2·ρ·v² = costant$   

### Linear thermal expansion

$ΔL = L_0·λ·dT$    where $λ$ is the lienar coefficient of thermal expansion.

### Heat capacity

$dQ/dT = m·c$    where $c$ is the specific heat capacity.

### Calorimeter equation

$m_1·c_1·(T_e-T_1) = m_2·c_2·(T_2-T_e)$

### Thermal flux

$W/A = k·(dT/s)$    where $k$ is the thermal conductivity.

## LICENSE
[GNU Lesser General Public License v3.0](https://github.com/melchiorrecaruso/ADimPas/blob/main/LICENSE)

