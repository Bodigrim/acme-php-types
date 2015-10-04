{-# LANGUAGE LambdaCase        #-}

module Data.PHP
  ( PHP
  , null
  , false
  , true
  , (===)
  , (!=)
  , (!==)
  ) where

import Prelude hiding (null)
import Data.String

data PHP
  = PBool Bool
  | PInteger Int
  | PFloat Double
  | PString String
  | PNull

default (PHP)

instance IsString PHP where
  fromString = PString

instance Show PHP where
  show = castString

instance Eq PHP where
  -- See http://php.net/manual/en/language.operators.comparison.php#language.operators.comparison.types
  a == b = case (a, b) of
    (x@PNull, y@PString{})     -> castString x == castString y
    (x@PString{}, y@PNull)     -> castString x == castString y

    (x@PBool{}, y)             -> castBool x == castBool y
    (x@PNull, y)               -> castBool x == castBool y
    (x, y@PBool{})             -> castBool x == castBool y
    (x, y@PNull)               -> castBool x == castBool y

    (x@PString{}, y@PString{})
      | isNumeric x && isNumeric y -> castFloat x == castFloat y
      | otherwise                  -> castString x == castString y

    (x, y)                     -> castFloat x == castFloat y

instance Ord PHP where
  -- See http://php.net/manual/en/language.operators.comparison.php#language.operators.comparison.types
  a <= b = case (a, b) of
    (x@PNull, y@PString{})     -> castString x <= castString y
    (x@PString{}, y@PNull)     -> castString x <= castString y
    (x@PString{}, y@PString{}) -> castString x <= castString y

    (x@PBool{}, y)             -> castBool x <= castBool y
    (x@PNull, y)               -> castBool x <= castBool y
    (x, y@PBool{})             -> castBool x <= castBool y
    (x, y@PNull)               -> castBool x <= castBool y

    (x, y)                     -> castFloat x <= castFloat y

instance Num PHP where
  -- See http://php.net/manual/en/language.operators.arithmetic.php
  fromInteger = PInteger . fromInteger

  a + b = if isInt a && isInt b
    then PInteger (castInteger a + castInteger b)
    else PFloat   (castFloat a + castFloat b)

  a - b = if isInt a && isInt b
    then PInteger (castInteger a - castInteger b)
    else PFloat   (castFloat a - castFloat b)

  a * b = if isInt a && isInt b
    then PInteger (castInteger a + castInteger b)
    else PFloat   (castFloat a + castFloat b)

  abs a = if isInt a
    then PInteger (abs (castInteger a))
    else PFloat   (abs (castFloat a))

  signum a = if isInt a
    then PInteger (signum (castInteger a))
    else PFloat   (signum (castFloat a))

instance Fractional PHP where
  -- See http://php.net/manual/en/language.operators.arithmetic.php
  a / b = if isInt a && isInt b && c == fromInteger (truncate c)
    then PInteger (truncate c)
    else PFloat c
    where
      c = castFloat a / castFloat b

  fromRational a = PFloat (fromRational a)

(===) :: PHP -> PHP -> Bool
PNull      === PNull       = True
PString a  === PString b   = a == b
PFloat a   === PFloat b    = a == b
PInteger a === PInteger b  = a == b
PBool a    === PBool b     = a == b
_          === _           = False

(!=) :: PHP -> PHP -> Bool
(!=) = (/=)

(!==) :: PHP -> PHP -> Bool
(!==) = (not .) . (===)

isNumeric :: PHP -> Bool
isNumeric = \case
  PInteger _ -> True
  PFloat _   -> True
  PString xs -> case reads (dropWhile (== ' ') xs) :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False
  _          -> False

isInt :: PHP -> Bool
isInt = \case
  PFloat _   -> False
  PString xs -> case reads (dropWhile (== ' ') xs) :: [(Int, String)] of
    [(_, "")] -> True
    _         -> False
  _          -> True

null :: PHP
null = PNull

false :: PHP
false = PBool False

true :: PHP
true = PBool True

castBool :: PHP -> Bool
castBool = \case
  PBool False -> False
  PInteger 0  -> False
  PFloat 0.0  -> False
  PString ""  -> False
  PString "0" -> False
  PNull       -> False
  _           -> True

castInteger :: PHP -> Int
castInteger = \case
  PBool False -> 0
  PBool True  -> 1
  PInteger x  -> x
  PFloat x    -> truncate x
  PString xs  -> case reads xs of
                  []           -> 0
                  ((x, _) : _) -> castInteger (PFloat x)
  PNull       -> 0

castFloat :: PHP -> Double
castFloat = \case
  PInteger x -> fromIntegral x
  PFloat x   -> x
  PString xs -> case reads xs of
                  []           -> 0
                  ((x, _) : _) -> x
  x          -> fromIntegral (castInteger x)

castString :: PHP -> String
castString = \case
  PBool False -> ""
  PBool True  -> "1"
  PInteger x  -> show x
  PFloat x    -> show x
  PString xs  -> xs
  PNull       -> ""

