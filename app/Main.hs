module Main where

import           Codec.BMP
import           Complex
import           Control.Monad
import qualified Data.ByteString as B

import           Color
import           Data.Complex
import           Data.Maybe
import           Data.Word
import           Lib
import           System.IO

main :: IO ()
main = forever $ do
  putStr " f(z) = " >> hFlush stdout
  l <- getLine
  putStr "niter = " >> hFlush stdout
  niter <- read <$> getLine :: IO Int
  let fz = maybe (const (1 / 0)) eval (parseExpr l)
  let s@(w, h) = (2560, 1600)
  let proj = Projection s 0 10 10
  let bmp = makeBMP w h (((colorize . compose niter fz) .) . pixelToComplex proj)
  writeBMP "picture.bmp" bmp
-- putStrLn $ prettyfy (fz 0)
-- forM_ [1 .. 10000000] $ \z -> return (fz (z :+ 0)) --putStrLn $ "f(" <> show z <> ") = " <> show (fz (z :+ 0))

compose 0 _ = id
compose n f = f . compose (n-1) f

pixelToComplex :: Projection -> Int -> Int -> Complex Double
pixelToComplex proj dispX dispY = o + (re :+ im)  where
  (dispW, dispH) = displaySize proj
  (w, h) = (width proj, height proj)
  o = center proj
  re = w * (fromIntegral dispX - fromIntegral dispW / 2) / fromIntegral dispW
  im = h * (fromIntegral dispY - fromIntegral dispH / 2) / fromIntegral dispH

data Projection = Projection {
  displaySize :: (Int, Int),
  center      :: Complex Double,
  width       :: Double,
  height      :: Double
}


colorize :: Complex Double -> Color
colorize z@(a :+ b) = hsv h s v
  where
    logabs = log . realPart . abs $ z
    dlat x = 1--abs (x) -- fromIntegral (round x))

    toDeg phi = 180 * (phi / pi) + if phi < 0 then 360 else 0
    h = toDeg (phase z)
    s = 1 - abs (logabs - fromIntegral (round logabs))
    v = sgm 100 0.01 ( min (dlat a) (dlat b))

    sgm c1 c2 x = 1 / (1 + exp (- c1 * (x - c2)))

makeBMP :: Int -> Int -> (Int -> Int -> Color) -> BMP
makeBMP width height f =
  let pixels = [f x y | y <- [0..height-1], x <- [0..width-1]]
      rgba = B.pack . concatMap encode $ pixels
  in packRGBA32ToBMP width height rgba


