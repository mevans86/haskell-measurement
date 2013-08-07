-- |A Haskell module for working with scientific measurements.
module Measurement
(Quant, QuantUnit,
    coulomb,
    farad,
    hertz,
    joule,
    newton,
    ohm,
    pascal,
    volt,
    watt,
    plus,
    minus,
    times,
    dividedBy,
    fromStringToQuant,
    pureQuant,
    prefix,
    prefixString,
    shiftPrefix,
    showFundamental,
    showPretty,
    unitOver,
    unitTimes,
    unitToThePowerOf) where

-- |Type synonym for [(Integer, Integer)], a list of [(prefix, power)] tuples for the seven fundamental SI units.
type QuantUnit = [(Integer, Integer)]

-- |Data type representing a measurement with a value and unit.
data Quant a = Quant { value :: a, unit :: QuantUnit } | Nothing deriving (Eq, Read, Show)

-- Dimensionless units

-- |The identity.
unity :: QuantUnit
unity = replicate 7 (0, 0)

-- SI units

-- |The SI unit of mass.
kilogram :: QuantUnit
kilogram = [(3,1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
-- |The SI unit of length.
meter :: QuantUnit
meter = [(0, 0), (0,1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
-- |The SI unit of time.
second :: QuantUnit
second = [(0, 0), (0, 0), (0,1), (0, 0), (0, 0), (0, 0), (0, 0)]
-- |The SI unit of temperature.
kelvin :: QuantUnit
kelvin = [(0, 0), (0, 0), (0, 0), (0,1), (0, 0), (0, 0), (0, 0)]
-- |The SI unit of amount or number of things.
mole :: QuantUnit
mole = [(0, 0), (0, 0), (0, 0), (0, 0), (0,1), (0, 0), (0, 0)]
-- |The SI unit of electrical current.
ampere :: QuantUnit
ampere = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0,1), (0, 0)]
-- |The SI unit of luminous intensity.
candela :: QuantUnit
candela = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0,1)]

-- Derived units

-- |A helper unit of mass.
gram :: QuantUnit
gram = [(0,1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
-- |The SI derived unit of electrical charge. 1 C = 1 A s
coulomb :: QuantUnit
coulomb = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) ampere second
-- |The SI derived unit of electrical capacitance. 1 F = 1 C V^-1
farad :: QuantUnit
farad = coulomb `unitOver` volt
-- |The SI derived unit of frequency. 1 Hz = 1 s^-1
hertz :: QuantUnit
hertz = unity `unitOver` second
-- |The SI derived unit of energy. 1 J = 1 N m
joule :: QuantUnit
joule = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) newton meter
-- |The SI derived unit of force. 1 N = 1 kg m s^-2
newton :: QuantUnit
newton = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, -2 * pow1 + pow2)) second . zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) kilogram $ meter
-- |The SI derived unit of electrical resistance. 1 ohm = 1 V A^-1
ohm :: QuantUnit
ohm = volt `unitOver` ampere
-- |The SI derived unit of pressure. 1 Pa = 1 N m^-2
pascal :: QuantUnit
pascal = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 - 2 * pow2)) newton meter
-- |The SI derived unit of electrical potential. 1 V = 1 W A^-1
volt :: QuantUnit
volt = watt `unitOver` ampere
-- |The SI derived unit of electrical power. 1 W = 1 J s^-1
watt :: QuantUnit
watt = joule `unitOver` second

-- Operations on Quant a's

-- |Takes two Quant a's and returns the Quant a corresponding to their sum.
plus :: (Num a) => Quant a -> Quant a -> Quant a
plus q1 q2
    | unit q1 == unit q2 = Quant { value = value q1 + value q2, unit = unit q1 }
    | otherwise = Measurement.Nothing

-- |Takes two Quant a's and returns the Quant a corresponding to their difference.
minus :: (Num a) => Quant a -> Quant a -> Quant a
minus q1 q2
    | unit q1 == unit q2 = Quant { value = value q1 - value q2, unit = unit q1 }
    | otherwise = Measurement.Nothing

-- |Takes two Quant a's and returns the prefix-free Quant a corresponding to their product.
times :: (Num a, Fractional a) => Quant a -> Quant a -> Quant a
times q1 q2 = normalizePrefix Quant { value = value normQ1 * value normQ2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) (unit normQ1) (unit normQ2) }
    where normQ1 = normalizePrefix q1
          normQ2 = normalizePrefix q2

-- |Takes two Quant a's and returns the Quant a corresponding to the first divided by the second.
dividedBy :: (Eq a, Fractional a, Num a) => Quant a -> Quant a -> Quant a
dividedBy q1 q2
    | value q2 == 0 = Measurement.Nothing
    | otherwise = normalizePrefix Quant { value = value q1 / value q2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 - pre2, pow1 - pow2)) (unit q1) (unit q2) }

-- Utility Functions

-- |Takes a String containing value and unit information and returns a Quant a. Unit prefixes are not supported yet (except for "kg").
fromStringToQuant :: (Fractional a, Num a, Read a) => String -> Quant a
fromStringToQuant str
    | length tok >= 2 = Quant { value = val, unit = foldl (zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2))) unity (map unitVectorFromString tok) } 
    | otherwise = Measurement.Nothing
    where tok = words str
          val = read (head tok)

-- |Takes a (prefix, power) tuple and returns the appropriate prefix String.
prefixString :: Integer -> String
prefixString pre
    | pre == 24 = "Y"
    | pre == 21 = "Z"
    | pre == 18 = "E"
    | pre == 15 = "P"
    | pre == 12 = "T"
    | pre == 9 = "G"
    | pre == 6 = "M"
    | pre == 3 = "k"
    | pre == 2 = "h"
    | pre == 1 = "da"
    | pre == -1 = "d"
    | pre == -2 = "c"
    | pre == -3 = "m"
    | pre == -6 = "mc"
    | pre == -9 = "n"
    | pre == -12 = "p"
    | pre == -15 = "f"
    | pre == -18 = "a"
    | pre == -21 = "z"
    | pre == -24 = "y"
    | otherwise = ""

prefix :: Quant a -> Integer
prefix q = fst (head (unit q))

-- |Takes a number and returns a minimal Quant with dimensionless units.
pureQuant :: (Num a) => a -> Quant a
pureQuant val = Quant { value = val, unit = unity }

-- |Takes a Quant and normalizes its prefix values by "rolling" them up into fst (head unit q). Only one prefix value is needed per Quant, really.
normalizePrefix :: (Num a, Fractional a) => Quant a -> Quant a
normalizePrefix q = Quant { value = value q * 10 ^ sum (map fst unitList), unit = updatedUnit }
    where unitList = unit q
          updatedUnit = zip (repeat 0) $ map snd unitList

-- |Takes an integer (n) and a Quant a, and returns a Quant a in which the prefix of the unit has been shifted n decimal places. If a shift to an unknown prefix is attempted, Measurement.Nothing is returned. TO DO: deal with shifts of units to powers. Doesn't really work right now.
shiftPrefix :: (Num a, Fractional a) => Integer -> Quant a -> Quant a
shiftPrefix 0 q = q
shiftPrefix shift q
    | ((finalPrefix `mod` 3) /= 0) && (abs finalPrefix > 3) = Measurement.Nothing
    | otherwise = Quant { value = value q / 10 ^^ shift, unit = (finalPrefix, snd (head (unit q))) : tail (unit q) }
        where finalPrefix = prefix q + shift

-- |Takes a Quant a and returns a String representation of the measurement with fundamental SI units only.
showFundamental :: (Eq a, Show a) => Quant a -> String
showFundamental q = (show . value $ q) ++ " " ++ pf ++ unwords (filter (/= "") [kg, m, mol, amp, cd, k, s])
            where pf = prefixString (prefix q)
                  kg
                    | snd (head (unit q)) == 0 = ""
                    | snd (head (unit q)) == 1 = "g"
                    | otherwise = "g^" ++ show (snd (head (unit q)))
                  m
                    | snd (unit q !! 1) == 0 = ""
                    | snd (unit q !! 1) == 1 = "m"
                    | otherwise = "m^" ++ show (snd (unit q !! 1))
                  s
                    | snd (unit q !! 2) == 0 = ""
                    | snd (unit q !! 2) == 1 = "s"
                    | otherwise = "s^" ++ show (snd (unit q !! 2))
                  k
                    | snd (unit q !! 3) == 0 = ""
                    | snd (unit q !! 3) == 1 = "K"
                    | otherwise = "K^" ++ show (snd (unit q !! 3))
                  mol
                    | snd (unit q !! 4) == 0 = ""
                    | snd (unit q !! 4) == 1 = "mol"
                    | otherwise = "mol^" ++ show (snd (unit q !! 4))
                  amp
                    | snd (unit q !! 5) == 0 = ""
                    | snd (unit q !! 5) == 1 = "A"
                    | otherwise = "A^" ++ show (snd (unit q !! 5))
                  cd
                    | snd (unit q !! 6) == 0 = ""
                    | snd (unit q !! 6) == 1 = "cd"
                    | otherwise = "cd^" ++ show (snd (unit q !! 6))

-- |Takes a Quant a and returns a String representation of the measurement, using derived units where applicable.
showPretty :: (Eq a, Show a, Num a, Fractional a) => Quant a -> String
showPretty q
        | q == Measurement.Nothing = "Nothing"
        | baseUnit == coulomb = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "C"
        | baseUnit == farad = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "F"
        | baseUnit == hertz = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "Hz"
        | baseUnit == joule = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "J"
        | kiloUnit == newton = (show . value $ q) ++ " " ++ prefixString (prefix q - 3) ++ "N"
        | baseUnit == ohm = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "ohm"
        | baseUnit == pascal = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "Pa"
        | baseUnit == volt = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "V"
        | baseUnit == watt = (show . value $ q) ++ " " ++ prefixString (prefix q) ++ "W"
        | otherwise = showFundamental q
        where baseUnit = unit (shiftPrefix (-1 * prefix q) q)
              kiloUnit = unit (shiftPrefix (-1 * prefix q + 3) q)

-- |Takes a (Char -> Bool) function and a String, and returns a list of Strings split when the (Char -> Bool) function
-- |evaluates to True.
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

-- |Takes two QuantUnits and returns the QuantUnit resulting from their division.
unitOver :: QuantUnit -> QuantUnit -> QuantUnit
unitOver = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 - pre2, pow1 - pow2))

-- |Takes two QuantUnits and returns the QuantUnit resulting from their multiplication.
unitTimes :: QuantUnit -> QuantUnit -> QuantUnit
unitTimes = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2))

-- |Takes a QuantUnit and an Integer (n) and returns the QuantUnit resulting from the raising of the QuantUnit to the power of n.
unitToThePowerOf :: QuantUnit -> Integer -> QuantUnit
unitToThePowerOf q1 expt = map (\(pre, pow) -> (pre, pow * expt)) q1

-- |Takes a string expressing a unit raised to a power and returns the corresponding QuantUnit.
-- |Unit prefixes are not supported yet (except for "kg").
unitVectorFromString :: String -> QuantUnit
unitVectorFromString str = case head unitExp of "g" -> fmap (\(pre, pow) -> (pre, pow * expt)) gram
                                                "kg" -> fmap (\(pre, pow) -> (pre, pow * expt)) kilogram
                                                "m" -> fmap (\(pre, pow) -> (pre, pow * expt)) meter
                                                "s" -> fmap (\(pre, pow) -> (pre, pow * expt)) second
                                                "K" -> fmap (\(pre, pow) -> (pre, pow * expt)) kelvin
                                                "mol" -> fmap (\(pre, pow) -> (pre, pow * expt)) mole
                                                "A" -> fmap (\(pre, pow) -> (pre, pow * expt)) ampere
                                                "cd" -> fmap (\(pre, pow) -> (pre, pow * expt)) candela
                                                _ -> unity
    where unitExp = splitWhen (=='^') str
          expt = if length unitExp < 2 then 1 else read (unitExp !! 1) :: Integer
