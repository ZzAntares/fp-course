{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO Add tests for earily refactor of this module

{-

Write a function (dollars) that accepts a `String` and returns a `String`.  It
will accept a numeric value as input, representing an amount of money, and
convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and
eleven cents"

Invalid characters should be ignored, meaning that every input string has an
output string.  The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and
output. There are also functions and data structures that may assist you in
deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion = let
  preillion :: List (Chars -> Chars)
  preillion = listh [ const ""
                    , const "un"
                    , const "do"
                    , const "tre"
                    , const "quattuor"
                    , const "quin"
                    , const "sex"
                    , const "septen"
                    , const "octo"
                    , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
                    ]
  postillion :: List Chars
  postillion = listh [ "vigintillion"
                     , "trigintillion"
                     , "quadragintillion"
                     , "quinquagintillion"
                     , "sexagintillion"
                     , "septuagintillion"
                     , "octogintillion"
                     , "nonagintillion"
                     , "centillion"
                     , "decicentillion"
                     , "viginticentillion"
                     , "trigintacentillion"
                     , "quadragintacentillion"
                     , "quinquagintacentillion"
                     , "sexagintacentillion"
                     , "septuagintacentillion"
                     , "octogintacentillion"
                     , "nonagintacentillion"
                     , "ducentillion"
                     , "deciducentillion"
                     , "vigintiducentillion"
                     , "trigintaducentillion"
                     , "quadragintaducentillion"
                     , "quinquagintaducentillion"
                     , "sexagintaducentillion"
                     , "septuagintaducentillion"
                     , "octogintaducentillion"
                     , "nonagintaducentillion"
                     , "trecentillion"
                     , "decitrecentillion"
                     , "vigintitrecentillion"
                     , "trigintatrecentillion"
                     , "quadragintatrecentillion"
                     , "quinquagintatrecentillion"
                     , "sexagintatrecentillion"
                     , "septuagintatrecentillion"
                     , "octogintatrecentillion"
                     , "nonagintatrecentillion"
                     , "quadringentillion"
                     , "deciquadringentillion"
                     , "vigintiquadringentillion"
                     , "trigintaquadringentillion"
                     , "quadragintaquadringentillion"
                     , "quinquagintaquadringentillion"
                     , "sexagintaquadringentillion"
                     , "septuagintaquadringentillion"
                     , "octogintaquadringentillion"
                     , "nonagintaquadringentillion"
                     , "quingentillion"
                     , "deciquingentillion"
                     , "vigintiquingentillion"
                     , "trigintaquingentillion"
                     , "quadragintaquingentillion"
                     , "quinquagintaquingentillion"
                     , "sexagintaquingentillion"
                     , "septuagintaquingentillion"
                     , "octogintaquingentillion"
                     , "nonagintaquingentillion"
                     , "sescentillion"
                     , "decisescentillion"
                     , "vigintisescentillion"
                     , "trigintasescentillion"
                     , "quadragintasescentillion"
                     , "quinquagintasescentillion"
                     , "sexagintasescentillion"
                     , "septuagintasescentillion"
                     , "octogintasescentillion"
                     , "nonagintasescentillion"
                     , "septingentillion"
                     , "deciseptingentillion"
                     , "vigintiseptingentillion"
                     , "trigintaseptingentillion"
                     , "quadragintaseptingentillion"
                     , "quinquagintaseptingentillion"
                     , "sexagintaseptingentillion"
                     , "septuagintaseptingentillion"
                     , "octogintaseptingentillion"
                     , "nonagintaseptingentillion"
                     , "octingentillion"
                     , "decioctingentillion"
                     , "vigintioctingentillion"
                     , "trigintaoctingentillion"
                     , "quadragintaoctingentillion"
                     , "quinquagintaoctingentillion"
                     , "sexagintaoctingentillion"
                     , "septuagintaoctingentillion"
                     , "octogintaoctingentillion"
                     , "nonagintaoctingentillion"
                     , "nongentillion"
                     , "decinongentillion"
                     , "vigintinongentillion"
                     , "trigintanongentillion"
                     , "quadragintanongentillion"
                     , "quinquagintanongentillion"
                     , "sexagintanongentillion"
                     , "septuagintanongentillion"
                     , "octogintanongentillion"
                     , "nonagintanongentillion"
                     ]
  in listh [ ""
           , "thousand"
           , "million"
           , "billion"
           , "trillion"
           , "quadrillion"
           , "quintillion"
           , "sextillion"
           , "septillion"
           , "octillion"
           , "nonillion"
           , "decillion"
           , "undecillion"
           , "duodecillion"
           , "tredecillion"
           , "quattuordecillion"
           , "quindecillion"
           , "sexdecillion"
           , "septendecillion"
           , "octodecillion"
           , "novemdecillion"
           ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit :: Digit -> Chars
showDigit Zero  = "zero"
showDigit One   = "one"
showDigit Two   = "two"
showDigit Three = "three"
showDigit Four  = "four"
showDigit Five  = "five"
showDigit Six   = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine  = "nine"

showTens :: Digit -> Digit -> Chars
showTens Zero Zero = ""
showTens Zero d    = showDigit d
showTens One d
  | Zero  <- d = "ten"
  | One   <- d = "eleven"
  | Two   <- d = "twelve"
  | Three <- d = "thirteen"
  | Four  <- d = "fourteen"
  | Five  <- d = "fifteen"
  | Six   <- d = "sixteen"
  | Seven <- d = "seventeen"
  | Eight <- d = "eighteen"
  | Nine  <- d = "nineteen"
showTens d Zero
  | Two   <- d = "twenty"
  | Three <- d = "thirty"
  | Four  <- d = "forty"
  | Five  <- d = "fifty"
  | Six   <- d = "sixty"
  | Seven <- d = "seventy"
  | Eight <- d = "eighty"
  | Nine  <- d = "ninety"
showTens t u = showTens t Zero ++ "-" ++ showDigit u

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _   = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars "" = dollars "0"
dollars s =
  let
    (bucks, cents) = break (== '.') s

    -- Treatment for bucks
    cleanBucks :: List (Optional Digit)
    cleanBucks = filter (/= Empty) $ fromChar <$> (if isEmpty bucks then "0" else bucks)
    digitBucks :: List Digit
    digitBucks =
      case sequence cleanBucks of  -- make a runOptional in Optional.hs
        Full xs -> xs
        Empty   -> Nil

    -- Treatment for cents
    cleanCents :: List (Optional Digit)
    cleanCents = filter (/= Empty) $ fromChar <$> cents
    digitCents :: List Digit
    digitCents =
      case sequence cleanCents of  -- make a runOptional in Optional.hs
        Full xs -> xs
        Empty   -> Nil

    dolares =
      let dls = getDollars digitBucks
      in
        if dls == "one"
        then dls ++ " dollar"
        else dls ++ " dollars"

    centavos =
      let cvs = getCents $ take 2 digitCents
      in
        if cvs == "one"
        then cvs ++ " cent"
        else cvs ++ " cents"

  in dolares ++ " and " ++ centavos

-- | Creates groups of N elements each and returns them in a List.
--
-- >>> groupsOf 3 "2134"
-- "2" :. "134" :. Nil
groupsOf :: Int -> List a -> List (List a)
groupsOf n xs =
  let
    reducer x (acc, tmp)
      | length tmp == n = (tmp :. acc , x :. Nil)
      | otherwise       = (acc        , x :. tmp)
    (groups, rest) = foldRight reducer (Nil, Nil) xs
  in
    rest :. groups

-- | Converts numerical strings up to three digits to words.
--
-- >>> readDigits [One, Two, Three]
-- "one hundred and twenty three"
--
-- >>> readDigits [Five, Four]
-- "fifty four"
--
-- >>> readDigits [Nine]
-- "nine"
-- TODO It screams for refactor and automation
readDigits :: List Digit -> Chars
readDigits Nil                         = ""
readDigits (u    :. Nil)               = showDigit u
readDigits (t    :. u    :. Nil)       = showTens t u
readDigits (Zero :. Zero :. Zero :. _) = ""
readDigits (h    :. Zero :. Zero :. _) = showDigit h ++ " hundred"
readDigits (Zero :. Zero :. u    :. _) = showDigit u
readDigits (Zero :. t    :. u    :. _) = showTens t u
readDigits (h    :. t    :. u    :. _) = showDigit h ++ " hundred and " ++ showTens t u

getDollars :: List Digit -> Chars
-- TODO It screams for refactor and automation
getDollars ds =
  let triplets = groupsOf 3 ds  -- [Two, Three, Four, One]  :=>  [[Two], [Three, Four, One]]
      nwords = readDigits <$> triplets  -- converts digits to words
      -- TODO Use "filter not isEmpty" instead of using the "if" inside lambdas
      res = zipWith (\a b -> if isEmpty b || isEmpty a then a else a ++ " " ++ b) (reverse nwords) illion
  in foldRight (\x acc -> if isEmpty acc then x else acc ++ " " ++ x) "" (filter (not . isEmpty) res)  -- reverses on the fly

-- TODO Change to readCents to have consistency with the digits alter
getCents :: List Digit -> Chars
getCents Nil                   = showDigit Zero
getCents (Zero :. Nil )        = showDigit Zero
getCents (t :. Nil )           = showTens t Zero
getCents (Zero :. Zero :. _  ) = showDigit Zero
getCents (t :. u :. _  )       = showTens t u

-- TODO This should go, there's no need for it to be an external function
fromChars :: Chars -> List (Optional Digit)
fromChars = (fromChar <$>)
