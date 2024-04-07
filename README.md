# <center>ADimPas Library</center>

Another library for type checking of dimensioned quantities at compile time in FreePascal. From a [circular's](https://github.com/circular17/DimPas) idea.

[<center>![Actions Status](https://github.com/melchiorrecaruso/ADimPas/workflows/build-test/badge.svg)</center>](https://github.com/melchiorrecaruso/ADimPas/actions)

## <u>What it's </u>

Ensuring coherence of physical dimensions in physical equations or mathematical relationships involving various variables is crucial. Dimensional analysis provides a fundamental tool to verify this coherence and correct any errors in the expressions.

The ADimpas library allows defining variables and constants in terms of quantity and units of measurement, automating dimensional analysis at compilation time.

## <u>How to use</u>

Code:
``` pas
uses
  ADim;
var 
  distance: TMeters;
  time: TSeconds;
  speed: TMetersPerSecond;  
begin
  distance := 5000*m;
  time     := 2*hr;
  speed    := distance/time;
  
  writeln('Speed is ', speed.ToKilometerPerHour.ToString);
end;
```
Output: 
``` 
Speed is 2.5 km/h
``` 


Refer to the [adimtest](adimtest.pas) source code for additional examples.

## <u>Requirements</u>

- [FreePascal compiler](https://www.freepascal.org)
- [Lazarus IDE](https://www.lazarus-ide.org)

## <u>Supported mathematical formulae:<u>

- [Mechanics](doc/mechanics.md)
- [Fluid Mechanics](doc/fluidmechanics.md)
- [Electricity and Magnetism](doc/electricityandmagnetism.md)
- [Heat and Thermodynamics](doc/heatandthermodynamics.md)
- [Waves and Hearing](doc/waves.md)
- [Light and Vision](doc/lightandvision.md)
- [Relativity](doc/relativity.md)
- [Quantum Mechanics](doc/quantummechanics.md)
- [Units of measurement](doc/unitsofmeasurement.md)

## References

- [HyperPhysics](http://hyperphysics.phy-astr.gsu.edu/hbase/hframe.html)
- [University Physics Volume 1](https://openstax.org/details/books/university-physics-volume-1)
- [University Physics Volume 2](https://openstax.org/details/books/university-physics-volume-2)
- [University Physics Volume 3](https://openstax.org/details/books/university-physics-volume-3)
- [National Institute of Standards and Technology](https://www.nist.gov/pml/owm/metric-si/si-units)
- [Bureau international des poids et mesures](https://www.bipm.org/en/)

## LICENSE

[GNU Lesser General Public License v3.0](https://github.com/melchiorrecaruso/ADimPas/blob/main/LICENSE)
