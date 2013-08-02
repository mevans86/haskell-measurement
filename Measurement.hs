module Measurement
(Quant,
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
    readFromString,
    shiftPrefix,
    showFundamental,
    showPretty,
    unitTimes,
    unitToThePowerOf,
    unitOver) where

type QuantUnit = [(Integer, Integer)]
data Quant a = Quant { value :: a, unit :: QuantUnit } | Nothing deriving (Eq, Read, Show)

-- Dimensionless units
unity :: QuantUnit
unity = replicate 7 (1, 0)

-- SI units
kilogram :: QuantUnit
kilogram = [(3,1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
meter :: QuantUnit
meter = [(0, 0), (0,1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
second :: QuantUnit
second = [(0, 0), (0, 0), (0,1), (0, 0), (0, 0), (0, 0), (0, 0)]
kelvin :: QuantUnit
kelvin = [(0, 0), (0, 0), (0, 0), (0,1), (0, 0), (0, 0), (0, 0)]
mole :: QuantUnit
mole = [(0, 0), (0, 0), (0, 0), (0, 0), (0,1), (0, 0), (0, 0)]
ampere :: QuantUnit
ampere = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0,1), (0, 0)]
candela :: QuantUnit
candela = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0,1)]

-- Derived units
coulomb :: QuantUnit
coulomb = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) ampere second
farad :: QuantUnit
farad = coulomb `unitOver` volt
hertz :: QuantUnit
hertz = unity `unitOver` second
joule :: QuantUnit
joule = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) newton meter
newton :: QuantUnit
newton = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, -2 * pow1 + pow2)) second . zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) kilogram $ meter
ohm :: QuantUnit
ohm = volt `unitOver` ampere
pascal :: QuantUnit
pascal = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 - 2 * pow2)) newton meter
volt :: QuantUnit
volt = watt `unitOver` ampere
watt :: QuantUnit
watt = joule `unitOver` second

plus :: (Num a) => Quant a -> Quant a -> Quant a
plus q1 q2
    | unit q1 == unit q2 = Quant { value = value q1 + value q2, unit = unit q1 }
    | otherwise = Measurement.Nothing

minus :: (Num a) => Quant a -> Quant a -> Quant a
minus q1 q2
    | unit q1 == unit q2 = Quant { value = value q1 - value q2, unit = unit q1 }
    | otherwise = Measurement.Nothing

times :: (Num a) => Quant a -> Quant a -> Quant a
times q1 q2 = Quant { value = value q1 * value q2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2)) (unit q1) (unit q2) }

dividedBy :: (Eq a, Fractional a, Num a) => Quant a -> Quant a -> Quant a
dividedBy q1 q2
    | value q2 == 0 = Measurement.Nothing
    | otherwise = Quant { value = value q1 / value q2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 - pre2, pow1 - pow2)) (unit q1) (unit q2) }

-- Utility Functions
shiftPrefix :: (Num a, Fractional a) => Integer -> Quant a -> Quant a
shiftPrefix shift q
    | length (filter (\(_, pow) -> (pow > 0)) (unit q)) /= 1 = Measurement.Nothing
    | ((finalPrefix `mod` 3) /= 0) && (abs finalPrefix > 3) = Measurement.Nothing
    | otherwise = Quant { value = (value q / 10^^shift)^^(sum . map snd $ unit q), unit = map (\(pre, pow) -> (pre + shift, pow)) (unit q) }
        where finalPrefix = head [ pre | (pre, pow) <- unit q, pow > 0 ] + shift

prefix :: (Num a, Eq a) => (a, b) -> String
prefix (pre, _)
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

readFromString :: (Fractional a, Num a, Read a) => String -> Quant a
readFromString str
    | length tok >= 2 = Quant { value = val, unit = foldl (zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2))) unity (map unitVectorFromString tok) } 
    | otherwise = Measurement.Nothing
    where tok = words str
          val = read (head tok)

showFundamental :: (Eq a, Show a) => Quant a -> String
showFundamental q = (show . value $ q) ++ kg ++ m ++ mol ++ amp ++ cd ++ k ++ s
            where kg
                    | snd (head (unit q)) == 0 = ""
                    | snd (head (unit q)) == 1 = " " ++ prefix (head (unit q)) ++ "g"
                    | otherwise = " " ++ prefix (head (unit q)) ++ "g^" ++ show (snd (head (unit q)))
                  m
                    | snd (unit q !! 1) == 0 = ""
                    | snd (unit q !! 1) == 1 = " " ++ prefix (unit q !! 1) ++ "m"
                    | otherwise = " " ++ prefix (unit q !! 1) ++ "m^" ++ show (snd (unit q !! 1))
                  s
                    | snd (unit q !! 2) == 0 = ""
                    | snd (unit q !! 2) == 1 = " " ++ prefix (unit q !! 2) ++ "s"
                    | otherwise = " " ++ prefix (unit q !! 2) ++ "s^" ++ show (snd (unit q !! 2))
                  k
                    | snd (unit q !! 3) == 0 = ""
                    | snd (unit q !! 3) == 1 = " " ++ prefix (unit q !! 3) ++ "K"
                    | otherwise = " " ++ prefix (unit q !! 3) ++ "K^" ++ show (snd (unit q !! 3))
                  mol
                    | snd (unit q !! 4) == 0 = ""
                    | snd (unit q !! 4) == 1 = " " ++ prefix (unit q !! 4) ++ "mol"
                    | otherwise = " " ++ prefix (unit q !! 4) ++ "mol^" ++ show (snd (unit q !! 4))
                  amp
                    | snd (unit q !! 5) == 0 = ""
                    | snd (unit q !! 5) == 1 = " " ++ prefix (unit q !! 5) ++ "A"
                    | otherwise = " " ++ prefix (unit q !! 5) ++ "A^" ++ show (snd (unit q !! 5))
                  cd
                    | snd (unit q !! 6) == 0 = ""
                    | snd (unit q !! 6) == 1 = " " ++ prefix (unit q !! 6) ++ "cd"
                    | otherwise = " " ++ prefix (unit q !! 6) ++ "cd^" ++ show (snd (unit q !! 6))

showPretty :: (Eq a, Show a) => Quant a -> String
showPretty q
        | q == Measurement.Nothing = "Nothing"
        | unit q == coulomb = (show . value $ q) ++ " C"
        | unit q == farad = (show . value $ q) ++ " F"
        | unit q == hertz = (show . value $ q) ++ " Hz"
        | unit q == joule = (show . value $ q) ++ " J"
        | unit q == newton = (show . value $ q) ++ " N"
        | unit q == ohm = (show . value $ q) ++ " ohm"
        | unit q == pascal = (show . value $ q) ++ " Pa"
        | unit q == volt = (show . value $ q) ++ " V"
        | unit q == watt = (show . value $ q) ++ " W"
        | otherwise = showFundamental q

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

unitOver :: QuantUnit -> QuantUnit -> QuantUnit
unitOver = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 - pre2, pow1 - pow2))

unitTimes :: QuantUnit -> QuantUnit -> QuantUnit
unitTimes = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 + pre2, pow1 + pow2))

unitToThePowerOf :: QuantUnit -> Integer -> QuantUnit
unitToThePowerOf q1 expt = map (\(pre, pow) -> (pre, pow * expt)) q1

unitVectorFromString :: String -> QuantUnit
unitVectorFromString str = case head unitExp of "kg" -> fmap (\(pre, pow) -> (pre, pow * expt)) kilogram
                                                "m" -> fmap (\(pre, pow) -> (pre, pow * expt)) meter
                                                "s" -> fmap (\(pre, pow) -> (pre, pow * expt)) second
                                                "K" -> fmap (\(pre, pow) -> (pre, pow * expt)) kelvin
                                                "mol" -> fmap (\(pre, pow) -> (pre, pow * expt)) mole
                                                "A" -> fmap (\(pre, pow) -> (pre, pow * expt)) ampere
                                                "cd" -> fmap (\(pre, pow) -> (pre, pow * expt)) candela
                                                _ -> unity
    where unitExp = splitWhen (=='^') str
          expt = if length unitExp < 2 then 1 else read (unitExp !! 1) :: Integer
