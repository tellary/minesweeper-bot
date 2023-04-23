{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleMinesweeper where

import Data.Maybe
import Control.Monad.Extra
import Control.Monad.Writer
import Data.Ord (comparing)
import Codec.Picture
import Codec.Picture.Extra        (crop)
import Control.Concurrent         (threadDelay)
import Control.Monad.IO.Class     (liftIO)
import Data.ByteString.Lazy       (ByteString, toStrict)
import Data.List
import Model
import Test.WebDriver
import Test.WebDriver.Common.Keys (enter)
import Test.WebDriver.Session

-- ./chromedriver --port=9515 --log-level=ALL --url-base=/wd/hub
-- ./chromedriver --port=9515 --url-base=/wd/hub
remoteConfig = useBrowser chrome defaultConfig { wdHost = "localhost"
                                               , wdPort = 9515
                                               }

returnSession config wd = runSession config $ do
  sess <- getSession
  a <- wd
  return (sess, a)

data GameSize = Easy | Medium | Hard

openGame :: GameSize -> WD DynamicImage
openGame size = do
  openPage "https://www.google.com"
  searchTextarea <- findElem (ByTag "textarea")
  sendKeys "minesweeper" searchTextarea
  sendKeys enter searchTextarea
  playButton <- findElem (ByXPath "//div[text() = 'Play']")
  keysSent <- sendKeys enter playButton

  selectSize size

  canvas <- findElem (ByTag "canvas")
  click canvas
  -- wait 3 seconds to allow visual effects to pass
  liftIO (threadDelay (3*1000*1000))
  takeFieldScreenshot

selectSize Medium = return ()
selectSize size = do
  click =<< findElem (ByTag "g-dropdown-menu")
  case size of
    Easy -> click =<< findElem (ByXPath "//div[text() = 'Easy']")
    Hard -> click =<< findElem (ByXPath "//div[text() = 'Hard']")

takeFieldScreenshot :: WD DynamicImage
takeFieldScreenshot = do
  canvas <- findElem (ByTag "canvas")
  (x, y) <- elemPos canvas
  liftIO (putStrLn $ "(x, y): " ++ show (x, y))
  (width, height) <- elemSize canvas
  liftIO (putStrLn $ "(width, height): " ++ show (width, height))
  bodyXY <- elemPos =<< findElem (ByTag "body")
  img <- either undefined id . decodeImage . toStrict <$> screenshot
  return
    $ dynamicPixelMap
      (crop
        (truncate (2 * x))
        (truncate (2 * y))
        (truncate (2 * width))
        (truncate (2 * height))
      )
      img

readFieldSize :: Pixel a => Image a -> FieldSize
readFieldSize img
  = FieldSize
    (imageWidth  img `div` firstCellSize)
    (imageHeight img `div` firstCellSize)
    firstCellSize
  where
    firstCellSize
      = 1 + (length . head . group . map (\i -> pixelAt img i 0) $ [0..])

centerOfCellAt :: FieldSize -> Int -> Int -> (Int, Int)
centerOfCellAt FieldSize {..} x y
  = ( x * cellSize + cellSize `div` 2, y * cellSize + cellSize `div` 2 )

centerOfCellPixelAt :: Image PixelRGB8 -> FieldSize -> Int -> Int -> PixelRGB8
centerOfCellPixelAt img fs x y = uncurry (pixelAt img) (centerOfCellAt fs x y)

topLeftCornerOfCellAt :: FieldSize -> Int -> Int -> (Int, Int)
topLeftCornerOfCellAt FieldSize {..} x y
  = ( x * cellSize + cellSize `div` 10, y * cellSize + cellSize `div` 10)

topLeftCornerOfCellPixelAt img fs x y
  = uncurry (pixelAt img) (topLeftCornerOfCellAt fs x y)

topLeftQuarterOfCellAt :: FieldSize -> Int -> Int -> (Int, Int)
topLeftQuarterOfCellAt FieldSize {..} x y
  = ( x * cellSize + cellSize `div` 4, y * cellSize + cellSize `div` 4)

topLeftQuarterOfCellPixelAt img fs x y
  = uncurry (pixelAt img) (topLeftQuarterOfCellAt fs x y)

pixelDistance :: Floating a => PixelRGB8 -> PixelRGB8 -> a
pixelDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2)
  = sqrt
    (
        (fromIntegral r1 - fromIntegral r2)^2
      + (fromIntegral g1 - fromIntegral g2)^2
      + (fromIntegral b1 - fromIntegral b2)^2
    )

fieldGreenLight = PixelRGB8 179 214 102
fieldGreenDark = PixelRGB8 172 208 95
flag = PixelRGB8 212 68 36
openLight = PixelRGB8 224 195 164
openDark = PixelRGB8 211 185 157
openBlue1 = PixelRGB8 52 119 203
openGreen2 = PixelRGB8 106 151 88
openRed3 = PixelRGB8 195 63 56
openPurple4 = PixelRGB8 187 151 156
openOrange5 = PixelRGB8 234 168 89
openBlue6 = PixelRGB8 113 167 163

fieldPixels =
  [ (fieldGreenLight, Field)
  , (fieldGreenDark, Field)
  ]

openCellPixels =
  [ (openLight, Open)
  , (openDark, Open)
  , (openBlue1, Number 1)
  , (openGreen2, Number 2)
  , (openRed3, Number 3)
  , (openPurple4, Number 4)
  , (openOrange5, Number 5)
  , (openBlue6, Number 6)
  ]

closestCellByPixel pixel pixels
  = minimumBy (comparing fst)
  . map (\(pixel1, cell) -> (pixelDistance pixel pixel1, cell))
  $ pixels

data ReadCellMsg
  = ReadCellMsg
  { msgType :: ReadCellMsgType
  , x :: Int
  , y :: Int
  , pixel :: PixelRGB8
  } deriving Show

data ReadCellMsgType
  = IsFieldOrFlagMsg
  | IsNotFieldOrFlagMsg
  | IsFlagMsg
  | IsNotFlagMsg
  | ReadOpenCellFailed
  deriving Show

isFieldOrFlag img fs x y
  | error < 25
  = writer (True, [ReadCellMsg IsFieldOrFlagMsg x y topLeftCornerPixel])
  | otherwise
  = writer (False, [ReadCellMsg IsNotFieldOrFlagMsg x y topLeftCornerPixel])
  where
    topLeftCornerPixel = topLeftCornerOfCellPixelAt img fs x y
    (error, _) = closestCellByPixel topLeftCornerPixel fieldPixels

isFlag img fs x y
  | error < 25 = writer (True, [ReadCellMsg IsFlagMsg x y pixel])
  | otherwise = writer (False, [ReadCellMsg IsNotFlagMsg x y pixel])
  where
    pixel = topLeftQuarterOfCellPixelAt img fs x y
    error = pixelDistance pixel flag

readOpenCellAt
  :: Image PixelRGB8 -> FieldSize -> Int -> Int
  -> Writer [ReadCellMsg] (Maybe Cell)
readOpenCellAt img fs x y
  | error < 25 = return . Just $ closestCell
  | otherwise = do
      tell [ReadCellMsg ReadOpenCellFailed x y centerPixel]
      return Nothing
  where
    centerPixel = centerOfCellPixelAt img fs x y
    (error, closestCell) = closestCellByPixel centerPixel openCellPixels


readCellAt
  :: Image PixelRGB8 -> FieldSize -> Int -> Int
  -> Writer [ReadCellMsg] (Maybe Cell)
readCellAt img fs x y = do
  ifM (isFieldOrFlag img fs x y)
    ( ifM (isFlag img fs x y)
      ( return . Just $ Flag )
      ( return . Just $ Field )
    )
    ( readOpenCellAt img fs x y)

readFieldWithMsgs
  :: Image PixelRGB8 -> FieldSize
  -> [[(Maybe Cell, [ReadCellMsg])]]
readFieldWithMsgs img fs@FieldSize {..}
  = [
      [runWriter $ readCellAt img fs x y
      | x <- [0 .. fieldWidth - 1]]
    | y <- [0..fieldHeight - 1]
    ]

fromCellsWithMsgs
  = map (map (\(f, msgs) -> if isJust f then fromJust f else error $ show msgs))

readField :: Image PixelRGB8 -> FieldSize -> Field
readField img fs
  =  fromCellsWithMsgs $ readFieldWithMsgs img fs

readFieldFromScreen0 readFieldF = do
  img <- convertRGB8 <$> takeFieldScreenshot
  let fs = readFieldSize img
  return $ readFieldF img fs

readFieldFromScreen :: WD Field
readFieldFromScreen = readFieldFromScreen0 readField

readFieldFromScreenWithMsg = readFieldFromScreen0 readFieldWithMsgs

openField0 readFieldF size = do
  screen <- openGame size
  let img = convertRGB8 screen
  let fs = readFieldSize img
  return $ readFieldF img fs

openField = openField0 readField
openFieldWithMsgs = openField0 readFieldWithMsgs

digCell :: IO ()
digCell = undefined

-- r <- returnSession remoteConfig (openFieldWithMsgs Easy)
-- pPrintNoColor (snd r)
-- pPrintNoColor . fromCellsWithMsgs $ snd r
-- pPrintNoColor =<< runWD (fst r) readFieldFromScreen
-- r <- returnSession remoteConfig (openFieldWithMsgs Hard)
-- img <- runWD (fst r) (convertRGB8 <$> takeFieldScreenshot)
