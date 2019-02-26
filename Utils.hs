module Utils where

import qualified Data.Vector.Storable as Vec
import Data.Word8
import Linear

loadPng :: String -> IO (Image PixelRGBA8)
loadPng path = do
    loadedPng <- readPng path
    case loadedPng of
        Left error -> return $ empty
        Right myPng -> do
            case myPng of
                ImageRGBA8 image -> return $ image
                otherwise        -> return $ empty
    where empty = generateImage (\ x y -> PixelRGBA8 0 0 0 0) 32 32

pixelToV4 :: PixelRGBA8 -> V4 Word8
pixelToV4 (PixelRGBA8 r g b a) = V4 r g b a

pixelDifference :: PixelRGBA8 -> PixelRGBA8 -> Int
pixelDifference pixelA pixelB = do
    let intPixelA = fmap fromIntegral (pixelToV4 pixelA)
    let intPixelB = fmap fromIntegral (pixelToV4 pixelB)
    qd intPixelA intPixelB

zipImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Int
zipImages image1 image2 = Vec.sum $ Vec.zipWith (-) data1 data2  where
    data1 = imageData image1
    data2 = imageData image2
