module Codec.QR.Image where

import Codec.QR.QR
import Codec.Picture

data Extension = PNG | JPG | BMP | GIF | TIFF | HDR 

-- Incluir colores y repasar codigo

saveImage :: Extension -> FilePath -> String -> Int -> Int -> QR -> IO()
saveImage ext path name size minBorder qr = case ext of
        PNG -> savePngImage (completePath ".png") i 
        JPG -> saveJpgImage 100 (completePath ".jpg") i
        BMP -> saveBmpImage (completePath ".bmp") i
        GIF -> either error id $ saveGifImage (completePath ".gif") i
        TIFF -> saveTiffImage (completePath ".tiff") i
        HDR -> saveRadianceImage (completePath ".hdr") i
 --       SVG -> createSVG (path++name++".svg") size minBorder qr
   where i = createImage size minBorder qr
         completePath s = path++"/"++name++s


createImage :: Int -> Int -> QR -> DynamicImage
createImage size minBorder qr = if size<minSize
                                then error $ "Min size is "++show
                                     minSize++" pixels"
                                else ImageY8 $ generateImage f size size
   where minSize = qrSize+2*minBorder
         qrSize = getSize qr
         (scale,restBorder) = divMod size minSize
         border = minBorder*scale + (div restBorder 2)
         f x y | and [x'>=0,y'>=0,x'<=qrSize,y'<=qrSize] = 
                      if qr ! (y',x') then 0 else 255
               | otherwise = 255
            where x' = div (x-border) scale
                  y' = div (y-border) scale


{-
createSVG :: FilePath -> Int -> Int -> QR -> IO()
createSVG path size minBorder qr = writeFile path $
  "<svg version=\"1.0\" xmlns=\"http://www.w3.org/2000/svg\""++
      " width=\""++show size++"\" height=\""++show size++"\">\n" ++
  "<rect width=\""++show size++"\" height=\""++show size++
         "\" style=\"fill:rgb(255,255,255)\" />\n"++
  foldl go "" (assocs qr)++
  "</svg>"
   where qrSize = getSize qr
         (scale,restBorder) = divMod size (qrSize+2*minBorder)
         border = minBorder*scale + (div restBorder 2)
         go xs ((x,y),b) = xs ++ (if b then "<rect x=\""++ show (scale * (y + border))++ 
                                           "\" y=\""++ show (scale * (x + border))++
                                       "\" width=\""++ show scale ++
                                      "\" height=\""++ show scale ++
                                       "\" style=\"fill:rgb(0,0,0)\" />\n"
                                  else "") 
 -}