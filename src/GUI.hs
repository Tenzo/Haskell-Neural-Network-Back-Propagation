-----------------------------------------------------------------------------
--
-- Module      :  GUI
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

module GUI where

import System.Environment
import Graphics.UI.WX
--import Graphics.UI.WX.Types
import Graphics.UI.WXCore
import Data.Array
import System.Random
import Data.Tuple.Select
import Text.Regex.Posix
import Control.Monad.Random

import NeuralNetwork(initNet,
    learnNet,
    getPixel,
    printLayer,
    Layer)

--main :: IO ()
--main
--  = start imageViewer

-- Specify image files for the file open dialog.
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]


-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       f      <- frame [text := "ImageViewer", picture := "./bitmaps/eye.ico", fullRepaintOnResize := False]
       imagesPanel      <- panel  f []
       -- use a mutable variable to hold the image
       vbitmap <- variable [value := Nothing]
       annbitmap <- variable [value := Nothing]
       ann <- variable [value := Nothing]

       -- add a scrollable window widget in the frame
       {--
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vbitmap
                                  ,bgcolor := white, fullRepaintOnResize := False]
       annSw  <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaintANN annbitmap
                                  ,bgcolor := black, fullRepaintOnResize := False]
       -}
       sw     <- window imagesPanel [ on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False]
       annSw     <- window imagesPanel [ on paint := onPaintANN annbitmap, bgcolor := black, fullRepaintOnResize := False]
       set imagesPanel [ layout := row 2 [margin 5 (fill(widget sw)),
                                  margin 5 (fill(widget annSw)) ]]
       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       startNet   <- menuItem file [text := "&Start Net\tCtrl+S",  help := "Start Artificial Neural Network", enabled := False]

       menuLine file
       quit   <- menuQuit file [help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About ImageViewer"]

       -- create Toolbar
       tbar   <- toolBar f []
       toolMenu tbar open  "Open"  "./bitmaps/fileopen16.png" []
       toolMenu tbar startNet "Start ANN" "./bitmaps/hsicon.ico"    []
       toolMenu tbar about "About" "./bitmaps/wxwin16.png"    []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := column 1 [floatTopLeft $ fill(widget imagesPanel)]
             ,statusBar        := [status]
             ,menuBar          := [file,hlp]
             ,outerSize        := sz 1600 900    -- niceness
             ,on (menu about)  := infoDialog f "About ANN" "Author: Pawel Kmiecik"
             ,on (menu quit)   := close f
             ,on (menu open)   := onOpen f sw vbitmap mclose status
             ,on (menu startNet) := onStartNet annSw vbitmap annbitmap ann status
             ,on (menu mclose) := onClose  sw vbitmap mclose status

             -- nice close down, but no longer necessary as bitmaps are managed automatically.
             ,on closing       :~ \previous -> do{ closeImage vbitmap; previous }
             ]
  where
    --onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
    onOpen f sw vbitmap mclose status
      = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> openImage sw vbitmap mclose status fname

    onClose sw vbitmap mclose status
      = do closeImage vbitmap
           set mclose [enabled := False]
           set sw     [virtualSize := sz 0 0]
           set status [text := ""]
           repaint sw

    closeImage vbitmap
      = do mbBitmap <- swap vbitmap value Nothing
           case mbBitmap of
             Nothing -> return ()
             Just bm -> objectDelete bm

    openImage sw vbitmap mclose status fname
      = do -- load the new bitmap
           -- bm <- bitmapCreateFromFile fname  -- can fail with exception
           let bm = image fname
           closeImage vbitmap
           set vbitmap [value := Just bm]
           set mclose [enabled := True]
           set status [text := fname]
           -- reset the scrollbars
           bmsize <- get bm size
           set sw [virtualSize := bmsize]
           repaint sw
       --`catch` \err -> repaint sw

    onPaint vbitmap dc viewArea
      = do mbBitmap <- get vbitmap value
           case mbBitmap of
             Nothing -> return ()
             Just bm -> drawImage dc bm pointZero []

    --onStartNet :: ScrolledWindow b -> Var (Maybe (Image ())) -> Var (Maybe (Image ())) -> IO ()
    onStartNet annSw vbitmap  annbitmap ann status
      = do -- start Artificial Neural Network
        origImg <- get vbitmap value
        case origImg of
             Nothing -> return ()
             Just img -> do
                  --imgsize <- get img size
                  origSizeX <- imageGetWidth img
                  origSizeY <- imageGetHeight img
                  currentNet <- get ann value
                  imgArr <- imageGetPixelArray img
                  w <- imageGetWidth img
                  h <- imageGetHeight img
                  --let arrCh = changeImgArr imgArr origSizeX origSizeY
                  case currentNet of
                        Nothing -> do
                                   set status [text := "Started 1st training"]
                                   initialNet <- initNet [6, 30, 32, 10, 3]
                                   let size = (origSizeX, origSizeY)
                                   rndPixs <- generateRandomPixels size 5000 imgArr
                                   let trainedNet =  trainXtimes initialNet rndPixs size
                                   --mapM_ printLayer trainedNet
                                   set ann [value := Just trainedNet]
                                   let newImgArr = buildImgArr trainedNet imgArr origSizeX origSizeY
                                   --let newImgArr = changeImgArr imgArr origSizeX origSizeY
                                   set status [text := "Finished 1st training"]
                                   newImg <- imageCreateFromPixelArray newImgArr
                                   set annbitmap [value := Just newImg]
                                   repaint annSw
                                   onStartNet annSw vbitmap  annbitmap ann status
                        Just net -> do
                                   set status [text := "Started training"]
                                   let size = (origSizeX, origSizeY)
                                   rndPixs <- generateRandomPixels size 5000 imgArr
                                   let trainedNet =  trainXtimes net rndPixs size
                                   --mapM_ printLayer trainedNet
                                   set ann [value := Just trainedNet]
                                   let newImgArr = buildImgArr trainedNet imgArr origSizeX origSizeY
                                   --let newImgArr = changeImgArr imgArr origSizeX origSizeY
                                   set status [text := "Finished training"]
                                   newImg <- imageCreateFromPixelArray newImgArr
                                   set annbitmap [value := Just newImg]
                                   repaint annSw
                                   onStartNet annSw vbitmap  annbitmap ann status

    onPaintANN annbitmap dc viewArea
      = do mbBitmap <- get annbitmap value
           case mbBitmap of
             Nothing -> return ()
             Just img -> do
                        drawImage dc img pointZero []
{--
trainXtimes 0 net imgArr size  = net
trainXtimes n net imgArr size  = trainedNet where
                 maxX = fst size
                 maxY = snd size
                 randX = 34 --randomR (0, maxX - 1) g
                 randY = 34 --randomR (0, maxY - 1) g
                 color = imgArr ! (point randX randY)
                 colorTuple = (colorRed color, colorGreen color, colorBlue color)
                 newNet = learnNet net (maxX, maxY) (randX, randY) colorTuple
                 trainedNet = trainXtimes (n-1) newNet imgArr size

                 --return trainedNet
--}

trainXtimes net [] size  = net
trainXtimes net rndPixs size  = trainedNet where
                newNet = learnNet net size (fst (head rndPixs)) (snd (head rndPixs)) True --((mod (length rndPixs) 1) == 0)
                trainedNet = trainXtimes newNet (tail rndPixs) size

buildImgArr net srcArr w h = array b (map (\xy -> getPixel net (w, h) (xy)) [ (x, y)| x <- [0..w-1], y <- [0..h-1] ]) where
                b = bounds srcArr

getPixelsList  net  w h = (map (\xy -> getPixel net (w, h) (xy)) [ (x, y)| x <- [0..w-1], y <- [0..h-1] ])

generateRandomPixels bnds n arr = do
                    randXY <- getRandXY n bnds
                    let clrs = map (\xy -> ( xy , (getPoint (fst xy) (snd xy) arr)  ) ) randXY
                    return clrs

getRandXY :: Int-> (Int, Int) -> IO [(Int, Int)]
getRandXY n size = newStdGen >>= return . rndPoints n size where
    rndPoints :: Int-> (Int, Int) -> StdGen -> [(Int, Int)]
    rndPoints  n size sg = take n (zip x y)
      where
        (sg1, sg2) = split sg
        x = randomRs (0, (fst size) - 1) sg1
        y = randomRs (0,  (snd size) - 1) sg2

getPoint x y arr = (colorRed clr, colorGreen clr, colorBlue clr) where
                clr = arr!(point x y)

calcScale :: Size -> Size -> Double
calcScale (Size bw bh) (Size vw vh) = min scalew scaleh
    where scalew = fromIntegral vw / fromIntegral bw
          scaleh = fromIntegral vh / fromIntegral bh

loadImageArr img
        = do
        arr <- imageGetPixelArray img
        return arr

-- ### THESE ARE ONLY FOR PROBLEMS WITH DECLARATIONS ####

-- Takes a array of pixels and width, height of an image
-- and a creates a new array where pixel color has been affected
-- to blur the image. The code will ignore all the edge pixels.
--changeImgArr :: (Enum a, Eq a, Num a, Ix (Point2 a)) => Array (Point2 a) Color -> a -> a -> Array (Point2 a) Color
changeImgArr arr w h = array b ([( point x y, arr!(point x y) ) | x <- [0..w-1], y <- [0..h-1], x==0 || y==0] ++
        [( point x y, recChgImg arr x y ) | x <- [1..w-2], y <- [1..h-2]] ++
        [( point x y, arr!(point x y) ) | x <- [0..w-1], y <- [0..h-1], x==(w-1) || y==(h-1)])
        where
        b = bounds arr
-- Gets the color of current pixel and its neighbors (up,down,left,right)
-- Algorithm:
-- newX = (Xi,j + Xi-1,j + Xi+1,j + Xi,j-1 + Xi,j+1) / 5

--recChgImg :: (Num a, Ix (Point2 a)) => Array (Point2 a) Color -> a -> a -> Color
recChgImg arr x y = newCol
        where
        col = arr!(point x y)
        colUp = arr!(point (x-1) y)
        colDown = arr!(point (x+1) y)
        colLeft = arr!(point x (y-1))
        colRight = arr!(point x (y+1))
        newColR = (colorRed col + colorRed colUp + colorRed colDown + colorRed colLeft + colorRed colRight) `div` 5
        newColG = (colorGreen col + colorGreen colUp + colorGreen colDown + colorGreen colLeft + colorGreen colRight) `div` 5
        newColB = (colorBlue col + colorBlue colUp + colorBlue colDown + colorBlue colLeft + colorBlue colRight) `div` 5
        newCol = rgb newColR newColG newColB

