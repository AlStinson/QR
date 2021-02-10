module Codec.QR.ErrorCorrection where

data ErrorCorrection = L | M | Q | H

errorCorrectionIndicator :: ErrorCorrection -> Int
errorCorrectionIndicator L = 1
errorCorrectionIndicator M = 0
errorCorrectionIndicator Q = 3
errorCorrectionIndicator H = 2

indicatorErrorCorrection :: Int -> ErrorCorrection
indicatorErrorCorrection 0 = M
indicatorErrorCorrection 1 = L
indicatorErrorCorrection 2 = H
indicatorErrorCorrection 3 = Q
