module Measurement
(Quant,
    plus,
    minus,
    times,
    dividedBy,
    readFromString,
    unitTimes,
    unitOver) where

data Quant a = Quant { value :: a, unit :: [(Float, Integer)] } | Nothing deriving (Eq, Read)

instance (Show a, Eq a) => Show (Quant a) where
    show q
        | q == Measurement.Nothing = "Nothing"
        | otherwise = (show . value $ q) ++ " " ++ kg ++ m ++ mol ++ amp ++ cd ++ k ++ s
            where kg
                    | snd (head (unit q)) == 0 = ""
                    | snd (head (unit q)) == 1 = "kg"
                    | otherwise = "kg^" ++ show (snd (head (unit q)))
                  m
                    | snd (unit q !! 1) == 0 = ""
                    | snd (unit q !! 1) == 1 = " m"
                    | otherwise = " m^" ++ show (snd (unit q !! 1))
                  s
                    | snd (unit q !! 2) == 0 = ""
                    | snd (unit q !! 2) == 1 = " s"
                    | otherwise = " s^" ++ show (snd (unit q !! 2))
                  k
                    | snd (unit q !! 3) == 0 = ""
                    | snd (unit q !! 3) == 1 = " K"
                    | otherwise = " K^" ++ show (snd (unit q !! 3))
                  mol
                    | snd (unit q !! 4) == 0 = ""
                    | snd (unit q !! 4) == 1 = " mol"
                    | otherwise = " mol^" ++ show (snd (unit q !! 4))
                  amp
                    | snd (unit q !! 5) == 0 = ""
                    | snd (unit q !! 5) == 1 = " A"
                    | otherwise = " A^" ++ show (snd (unit q !! 5))
                  cd
                    | snd (unit q !! 6) == 0 = ""
                    | snd (unit q !! 6) == 1 = " cd"
                    | otherwise = " cd^" ++ show (snd (unit q !! 6))

-- Dimensionless units
unity :: [(Float, Integer)]
unity = replicate 7 (1, 0)

-- SI units
kilogram :: [(Float, Integer)]
kilogram = [(1,1), (1,0), (1,0), (1,0), (1,0), (1,0), (1,0)]
meter :: [(Float, Integer)]
meter = [(1,0), (1,1), (1,0), (1,0), (1,0), (1,0), (1,0)]
second :: [(Float, Integer)]
second = [(1,0), (1,0), (1,1), (1,0), (1,0), (1,0), (1,0)]
kelvin :: [(Float, Integer)]
kelvin = [(1,0), (1,0), (1,0), (1,1), (1,0), (1,0), (1,0)]
mole :: [(Float, Integer)]
mole = [(1,0), (1,0), (1,0), (1,0), (1,1), (1,0), (1,0)]
ampere :: [(Float, Integer)]
ampere = [(1,0), (1,0), (1,0), (1,0), (1,0), (1,1), (1,0)]
candela :: [(Float, Integer)]
candela = [(1,0), (1,0), (1,0), (1,0), (1,0), (1,0), (1,1)]

-- Derived units
coulomb :: [(Float, Integer)]
coulomb = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2)) ampere second
farad :: [(Float, Integer)]
farad = coulomb `unitOver` volt
hertz :: [(Float, Integer)]
hertz = unity `unitOver` second
joule :: [(Float, Integer)]
joule = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2)) newton meter
newton :: [(Float, Integer)]
newton = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, -2 * pow1 + pow2)) second . zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2)) kilogram $ meter
ohm :: [(Float, Integer)]
ohm = volt `unitOver` ampere
pascal :: [(Float, Integer)]
pascal = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 - 2 * pow2)) newton meter
volt :: [(Float, Integer)]
volt = watt `unitOver` ampere
watt :: [(Float, Integer)]
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
times q1 q2 = Quant { value = value q1 * value q2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2)) (unit q1) (unit q2) }

dividedBy :: (Eq a, Fractional a, Num a) => Quant a -> Quant a -> Quant a
dividedBy q1 q2
    | value q2 == 0 = Measurement.Nothing
    | otherwise = Quant { value = value q1 / value q2, unit = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 / pre2, pow1 - pow2)) (unit q1) (unit q2) }

-- Utility Functions
readFromString :: (Fractional a, Num a, Read a) => String -> Quant a
readFromString str
    | length tok >= 2 = Quant { value = val, unit = foldl (zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2))) unity (map unitVectorFromString tok) } 
    | otherwise = Measurement.Nothing
    where tok = words str
          val = read (head tok)

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

unitOver :: [(Float, Integer)] -> [(Float, Integer)] -> [(Float, Integer)]
unitOver = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 / pre2, pow1 - pow2))

unitTimes :: [(Float, Integer)] -> [(Float, Integer)] -> [(Float, Integer)]
unitTimes = zipWith (\(pre1, pow1) (pre2, pow2) -> (pre1 * pre2, pow1 + pow2))

unitVectorFromString :: String -> [(Float, Integer)]
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
