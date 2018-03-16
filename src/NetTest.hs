module Test where

import System.Environment
import Graphics.UI.WX
--import Graphics.UI.WX.Types
import Graphics.UI.WXCore
import Data.Array
import System.Random
import Data.Tuple.Select
import Text.Regex.Posix
import Control.Monad.Random
import GUI

import NeuralNetwork(initNet,
    learnNet,
    learnNetNTimes,
    getPixel,
    printLayer,
    Layer)

testViewer :: IO ()
testViewer = do
	f <- frame [text := "Working!"]
	img <- imageCreateFromFile "../img/image.png"
	
	imgArr <- imageGetPixelArray img
	origSizeX <- imageGetWidth img
        origSizeY <- imageGetHeight img
	initialNet <- initNet [2, 5, 20, 20, 5 , 3]
	print ("Img size: " ++ (show (origSizeX, origSizeY)))
	let pixel = getPixel initialNet (origSizeX, origSizeY) (100, 100)
	print ("Init pixel" ++ (show pixel))
	let size = (origSizeX, origSizeY)
   	rndPixs <- generateRandomPixels size 5000 imgArr
   	let trainedNet =  trainXtimes initialNet rndPixs size
	let pixel2 = map (getPixel trainedNet (origSizeX, origSizeY) ) [(x,x) | x <- [0..10]]
	print ("Pixel after 50k " ++ (show pixel2))
	
	rndPixs2 <- generateRandomPixels size 5000 imgArr
	let trainedNet2 =  trainXtimes trainedNet rndPixs2 size
	let pixel3 = map (getPixel trainedNet2 (origSizeX, origSizeY) ) [(x,x) | x <- [0..10]]
	print ("Pixel after 100k " ++ (show pixel3))

	print (show trainedNet2)

	close f
