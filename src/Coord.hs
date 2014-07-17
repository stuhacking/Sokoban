{-| 2D Coordinate datatype |-}
module Coord where

data Coord = Coord { x :: Int, y :: Int}
           deriving (Show, Eq, Ord)

instance Num Coord where
  (Coord x1 y1) + (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)
  (Coord x1 y1) - (Coord x2 y2) = Coord (x1 - x2) (y1 - y2)
  (Coord x1 y1) * (Coord x2 y2) = Coord (x1 * x2) (y1 * y2)
  negate (Coord x y) = Coord (negate x) (negate y)
  abs (Coord x y) = Coord (abs x) (abs y)
  signum (Coord x y) = Coord (signum x) (signum y)
  fromInteger x = Coord iX iX
    where iX = fromInteger x :: Int

toPair :: Coord -> (Int, Int)
toPair (Coord x y) = (x, y)
