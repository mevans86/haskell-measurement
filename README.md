# haskell-measurement
A simple Haskell module for working with scientific measurements.

## Introduction
This simple module introduces the "Quant" type, which includes fields for a value and units. To create a measurement, just use record syntax thusly.

	ghci> let lengthM = Quant { value = 5.0, unit = meter }
	ghci> lengthM
	5.0 meters

Simple as that! haskell-measurement takes care of everything behind the scenes. In case you're interested, units are handled as a list of two-tuples (doubles?). The list contains seven members corresponding to the seven fundamental SI units (their order in the list is managed internally). The first member of each tuple is the unit's prefix ("centi," "milli," "micro," etc.), although this feature is not yet fully implemented (see below). The second member is the power to which that unit is raised in the "complex unit" corresponding to the whole measurement. This design allows you to work with derived SI units without ever worrying about their definitions in terms of the fundamentals. It also means that computations with measurements are unit-aware---multiplying two lengths will yield an area in meters squared, for example.

## Units
haskell-measurement defines the seven fundamental SI units as constants. Here are their names:

* kilogram *(yes, the unit is the kilogram!)*
* meter
* second
* kelvin
* mole
* ampere
* candela

Why the kilogram, you ask? Annoyingly, many derived units are defined using the kilogram, rather than the gram. The Newton is a prime example (kg m s<sup>&ndash;2</sup>. haskell-measurement also defines a number of derived units as constants. 

* coulomb
* farad
* hertz
* joule
* newton
* ohm
* pascal
* volt
* watt

Last but certainly not least, the unit of identity is defined as `unity`. Unity is useful for creating inverse units and dimensionless constants. Calculated Quants that end up dimensionless have `unity` as their unit.

	ghci> let frequency = Quant { value = 50000000.42, unit = unity `unitOver` second }
	ghci> frequency
	5.000000042e7  s^-1

Of course, the same effect could be achieved using the `hertz` derived unit (see below).

# Functions
At present, haskell-measurement supports basic arithmetic on measurements. Using them in an infix manner is strongly advised:

	ghci> force1 `plus` force2
	144.0 kg m s^-2
	
Basic arithmetic is implemented as the `plus`, `minus`, `times`, and `dividedBy` functions.

haskell-measurement also includes a couple of functions for working with units; these allow one to define complex units directly during the creation of a measurement. `unitTimes` and `unitOver` are fairly self-explanatory: they multiply and divide two units, respectively. `unitToThePowerOf` raises a unit to a power without need for an arbitrary computation. The idea is to allow the creation of complex units without unnecessary computations. Now you can use this...

	ghci> Quant { value = 5.0, unit = meter `unitToThePowerOf` 2 }
	
Instead of having to do this...

	ghci> Quant { value = 5.0, unit = meter } `times` Quant { value = 1.0, unit = meter }

It works on derived units too!

Quant is a derived instance of Show, but I have also provided a `showPretty` function that outputs a measurement with units in human-readable form. `showPretty` outputs derived units where applicable with appropriate prefixes (derived units raised to powers are not supported, however). The rule for prefixes on derived units is: if prefixed fundamental units were used to create the derived quantity, these fundamental prefixes are "rolled up" to create a net prefix for the derived unit. You can also display a unit in a more human-friendly way by using `shiftPrefix` wisely. For example,

	ghci> let freq = fromStringToQuant "1000000 s^-1"
	ghci> showPretty $ shiftPrefix 6 freq
	"1.0 MHz"

The `fromStringToQuant` function takes a single string parameter and returns a Quant. The string has a few formatting requirements: the value of the measurement must come first, followed by a space-separated list of multiplied units. Powers of units are supported, so the following works just fine.

	ghci> let force = fromStringToQuant "12.23 m kg s^-2"
	ghci> showPretty force
	"12.23 N"

Using scientific notation here is supported; just make sure to write values as, for example, "12.23e-3."

Prefix handling is partially implemented via the `prefix`, `prefixString`, `shiftPrefix`, and `normalizePrefix` functions. I've realized that the issue of prefixes is somewhat complicated, because km<sup>2</sup> really means (km)<sup>2</sup>, while prefixes attached to units without exponents are simply a proxy for "times ten to the *n*th power." The former is hard to implement; the latter is easy. So, the latter is done for both base and derived units (you still can't read prefixes using `fromStringToQuant`, though).

The `pureQuant` function takes a numeric value and returns a minimal Quant with dimensionless units. I'm beginning to think about implementing Quant as an applicative functor or monad, and this function plays the role of `pure` or `return`.

# To Do
*Prefix handling:* prefix handling is partially implemented for existing Quants (see above), but reading a measurement from a string will still only work properly when the fundamental units with no prefixes are used (note that "kg" works fine, since the kilogram is the fundamental unit of mass).

*Showing derived units*: the `showPretty` function partially addresses this issue, but powers of derived units remain unsupported (and probably will for the forseeable future).
