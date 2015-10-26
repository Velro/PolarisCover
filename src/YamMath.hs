module YamMath
  (
    v2ClampMagnitude,
    v2InnerProduct,
    v2MoveTowards,
    moveTowards,
    clampValue,
    v3TimesScalar
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

v2InnerProduct :: (Num a) => L.V2 a -> L.V2 a -> a
v2InnerProduct (L.V2 aX aY) (L.V2 bX bY) = aX * bX + aY * bY

v2MoveTowards :: (Num a, Ord a) => L.V2 a -> L.V2 a -> a -> L.V2 a
v2MoveTowards (L.V2 lastValueX lastValueY) (L.V2 targetValueX targetValueY) maxDelta =
  L.V2 resultX resultY where
    resultX = moveTowards lastValueX targetValueX maxDelta
    resultY = moveTowards lastValueY targetValueY maxDelta

moveTowards :: (Num a, Ord a) => a -> a -> a -> a
moveTowards lastValue target maxDelta =
  let
    sign = signum target
    result = lastValue + (maxDelta * sign)
  in
    if abs result > abs target
    then target
    else result

clampValue :: (Ord a) => a -> a -> a -> a
clampValue mn mx = max mn . min mx

v3TimesScalar :: (Num a, Ord a) => L.V3 a -> a -> L.V3 a
v3TimesScalar (L.V3 x y z) b = L.V3 (x * b)(y * b)(z * b)
--mapRange :: Num -> Num -> Num -> Num -> Num -> Num
--mapRange rangeA rangeB rangeX rangeY value =
