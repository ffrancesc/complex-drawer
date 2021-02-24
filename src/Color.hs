module Color (Color, encode, hsv, rgb) where

import           Data.Word


data Color = RGB Double Double Double

encode :: Color -> [Word8]
encode (RGB r g b) = map q [r, g, b] ++ [255]
  where q = fromIntegral . floor . (+0.5) . (*255) . min 1 . max 0


rgb :: Double -> Double -> Double -> Color
rgb = RGB

hsv :: Double -> Double -> Double -> Color
hsv h s v = case hi of
    0 -> RGB v t p
    1 -> RGB q v p
    2 -> RGB p v t
    3 -> RGB p q v
    4 -> RGB t p v
    5 -> RGB v p q
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)


mod1 x | pf < 0 = pf+1
       | otherwise = pf
 where
  (_, pf) = properFraction x
