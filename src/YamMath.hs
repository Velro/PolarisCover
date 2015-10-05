module YamMath
  (
    v2ClampMagnitude,
    v2InnerProduct,
    clampValue
  ) where

import qualified Linear as L

v2ClampMagnitude ::(Num a, Ord a, Fractional a, Floating a) => L.V2 a -> a -> L.V2 a
v2ClampMagnitude input mx =
  if vectorLength > mx
  then input * (L.V2 compon compon)
  else input
  where
    vectorLength = v2InnerProduct input input
    compon = (mx / sqrt vectorLength)

clampValue :: (Ord a) => a -> a -> a -> a
clampValue mn mx = max mn . min mx

v2InnerProduct :: (Num a) => L.V2 a -> L.V2 a -> a
v2InnerProduct (L.V2 aX aY) (L.V2 bX bY) = aX * bX + aY * bY
