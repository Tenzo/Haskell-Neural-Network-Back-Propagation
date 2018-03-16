-----------------------------------------------------------------------------
--
-- Module      :  NeuralNetwork
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module NeuralNetwork (
    initNet,
    learnNet,
    getPixel,
    printLayer,
    Layer
) where

import Data.Matrix as Matrix
import Data.Tuple.Select
import Graphics.UI.WXCore
--import Data.Sequence( update)
import RandMatrix (generateRandomMatrix)

data Layer = Layer {
        weights :: Matrix Double,
        diffs :: Matrix Double,
        x :: Matrix Double,
        y :: Matrix Double,
        a :: Matrix Double,
        d :: Matrix Double
        } deriving Show

type NeuralNet = [Layer]

sig :: Int -> Double -> Double
sig col x = 1/(1 + exp(-x))

sigDiff :: Int -> Double -> Double
sigDiff col x = x * (1 - x)


-- choose matrix multiplication method
matMult = multStd2

mapMatrix :: (Int -> a -> a) -> Matrix a -> Matrix a
mapMatrix func matrix = mapRowN func matrix (nrows matrix) where
         mapRowN func matrix 0 = matrix
         mapRowN func matrix n = mapRowN func (Matrix.mapRow func n matrix) (n-1)

initNet :: [Int] -> IO [Layer]
initNet sizeList = do
         if (length sizeList) < 2
                then do
                        return []
                else do
                        -- get two neighbour sizes
                        let size1 = head sizeList
                        let size2 = head(tail sizeList)
                        w <- generateRandomMatrix (size1 + 1) size2
                        let zeroVec = (Matrix.zero 1 size2)
                        let layer = Layer{weights = w,
                                          diffs = zeroVec,
                                          x = zeroVec,
                                          y = zeroVec,
                                          a = zeroVec,
                                          d = zeroVec
                                           }
                        xs <- initNet  ( tail sizeList)
                        return (layer:xs)

feedLayer2 :: Layer -> Layer -> Layer
feedLayer2 inputLayer layer = Layer{
                     weights= weights(layer),
                     diffs = diffs(layer),
                     x = makeX inputLayer,
                     a = matMult (makeX inputLayer) (weights layer),
                     y = mapMatrix sig (matMult (makeX inputLayer) (weights layer)),
                     d = mapMatrix sigDiff (mapMatrix sig (matMult (makeX inputLayer) (weights layer)))} where
                        makeX inputLayer = extendTo (1.0) (nrows (y inputLayer)) (ncols (y inputLayer) + 1)  (y inputLayer)

feedLayer :: Layer -> Layer -> Layer
feedLayer inputLayer layer = feededLayer where
                    makeX = extendTo (-1.0) (nrows (y inputLayer)) (ncols (y inputLayer) + 1)  (y inputLayer)
                    makeA = matMult makeX (weights layer)
                    makeY = mapMatrix sig makeA
                    makeD = mapMatrix sigDiff makeY
                    feededLayer = Layer{
                         weights= weights(layer),
                         diffs = diffs(layer),
                         x = makeX,
                         a = makeA,
                         y = makeY,
                         d = makeD}

lastLayerFeedBack :: Layer -> Matrix Double -> Layer
lastLayerFeedBack layer rgb =  Layer{
                     weights = weights(layer),
                     diffs = fromLists [(zipWith (*) (toList ((y layer) - rgb))  (toList (d layer)))],
                     x = x layer,
                     y = y layer,
                     a = a layer,
                     d = d layer}

feedBack2 :: Layer -> Layer -> Layer
feedBack2 layer frontLayer = Layer{
                     weights= weights(layer),
                     diffs = fromLists[zipWith (*) (toList( matMult (diffs frontLayer)  (transpose(submatrix 1 (nrows (weights frontLayer) - 1) 1 (ncols (weights frontLayer) ) (weights frontLayer)))))  (toList(d layer))],
                     x = x layer,
                     y = y layer,
                     a = a layer,
                     d = d layer}

feedBack :: Layer -> Layer -> Layer
feedBack layer frontLayer = backLayer where
                    cuttedWeights = submatrix 1 (nrows (weights frontLayer) - 1) 1 (ncols (weights frontLayer)) (weights frontLayer)
                    eps = matMult (diffs frontLayer) (transpose cuttedWeights)
                    makeDiffs = elementwise (*) eps (d layer) --fromLists[ zipWith (*) (toList eps) (toList (d layer))]
                    backLayer = Layer{
                     weights= weights(layer),
                     diffs = makeDiffs,
                     x = x layer,
                     y = y layer,
                     a = a layer,
                     d = d layer}



fixWeights layer = fixedLayer where
                    scaledX = (scaleMatrix 0.5 (x layer))
                    fix = elementwise (*) scaledX (diffs layer)--fromLists[ zipWith (*) (toList scaledX) (toList (diffs layer))]
                    fixedLayer = Layer {
                     weights= (weights layer) - (matMult (transpose (scaleMatrix 0.5 (x layer))) (diffs layer)),
                     diffs = diffs(layer),
                     x = x layer,
                     y = y layer,
                     a = a layer,
                     d = d layer}

printLayer :: Layer -> IO()
printLayer layer = do
    let xMatrix = "(" ++ (show (nrows ( x layer ))) ++ "x" ++ (show( ncols ( x layer))) ++")"
    let yMatrix = "(" ++ (show(nrows ( y layer ))) ++ "x" ++ (show( ncols ( y layer))) ++")"
    let aMatrix = "(" ++ (show(nrows ( a layer ))) ++ "x" ++ (show( ncols ( a layer))) ++")"
    let dMatrix = "(" ++ (show(nrows ( d layer ))) ++ "x" ++ (show( ncols ( d layer))) ++")"
    let wMatrix = "(" ++ (show(nrows ( weights layer ))) ++ "x" ++ (show( ncols ( weights layer))) ++")"
    let diffMatrix = "(" ++ (show(nrows ( diffs layer ))) ++ "x" ++ (show( ncols ( diffs layer))) ++ ")"
    putStrLn ("Input X:" ++ xMatrix ++ " A: " ++ aMatrix ++ " Y: " ++ yMatrix ++ " D: " ++ dMatrix ++ " Weights: " ++ wMatrix ++ " Diffs: " ++ diffMatrix)

learnNet net imgSize xy rgb doFix = fixedNet where
        --scale img coords to 0.05-0.95
        imgX = ((fromIntegral (sel1 xy)) / (fromIntegral (fst imgSize)) * 0.9) + 0.05
        imgY = ((fromIntegral (sel2 xy)) / (fromIntegral (snd imgSize)) * 0.9) + 0.05
        sinImgX = sin imgX
        sinImgY = sin imgY
        sin2ImgX = sinh imgX
        sin2ImgY = sinh imgY
        --scale 0-255 values to 0.05-0.95
        r = (((fromIntegral (sel1 rgb)) / 255.0)* 0.9) + 0.05
        g = (((fromIntegral (sel2 rgb)) / 255.0)* 0.9) + 0.05
        b = (((fromIntegral (sel3 rgb)) / 255.0)* 0.9) + 0.05
        rgbMatrix = fromLists [[r, g, b]]
        xyMatrix = fromLists [[imgX, imgY, sinImgX, sinImgY, sin2ImgX, sin2ImgY]]
        zeroMatrix = (Matrix.zero 1 1)
        xyLayer = Layer{weights = zeroMatrix,
                     diffs = zeroMatrix,
                     x = zeroMatrix,
                     y = xyMatrix,
                     a = zeroMatrix,
                     d = zeroMatrix}

        -- feed the net forward
        feededNet = tail (scanl feedLayer xyLayer net)
        -- feed the net backward

        feedBackLast = lastLayerFeedBack (last feededNet) rgbMatrix
        feededNet2 = (take ((length feededNet) -1) feededNet)  ++ [feedBackLast]
        feededBackNet = scanr1 feedBack feededNet2

        fixedNet = case doFix of
            False -> feededBackNet
            True -> map fixWeights feededBackNet
        --let output = toList(y (last fixedNet))
        --putStrLn ("Output: " ++ (show output))
        --mapM_ printLayer fixedNet
        --let out = weights(feedBackLast)
        --return  fixedNet


getPixel net imgSize xy = (point (sel1 xy) (sel2 xy), rgb (rgbList!!0) (rgbList!!1) (rgbList!!2)) where
        --scale img coords to 0.05-0.95
        imgX = ((fromIntegral (sel1 xy)) / (fromIntegral (sel1 imgSize)) * 0.9) + 0.05
        imgY = ((fromIntegral (sel2 xy)) / (fromIntegral (sel2 imgSize)) * 0.9) + 0.05
        sinImgX = sin imgX
        sinImgY = sin imgY
        sin2ImgX = sinh imgX
        sin2ImgY = sinh imgY

        xyMatrix = fromLists [[imgX, imgY, sinImgX, sinImgY, sin2ImgX, sin2ImgY]]
        zeroMatrix = (Matrix.zero 1 1)
        xyLayer = Layer{weights = zeroMatrix,
                     diffs = zeroMatrix,
                     x = zeroMatrix,
                     y = xyMatrix,
                     a = zeroMatrix,
                     d = zeroMatrix}

        lastLayer = last (tail (scanl feedLayer xyLayer net))
        -- convert pixels back to 0-255 values
        rgbList = map (\x -> floor(((x - 0.05)/0.9) * 255)) (toList(y lastLayer))
        --return (point (sel1 xy) (sel2 xy), rgb (rgbList!!0) (rgbList!!1) (rgbList!!2))



