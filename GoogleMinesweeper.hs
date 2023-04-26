{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleMinesweeper where

import Codec.Picture
import Codec.Picture.Extra        (crop)
import Control.Concurrent         (threadDelay)
import Control.Monad.Extra        (ifM)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Writer       (Writer, runWriter, tell, writer)
import Data.ByteString.Lazy       (ByteString, toStrict)
import Data.List                  (group, minimumBy)
import Data.Ord                   (comparing)
import Data.Ratio                 (Ratio, denominator, numerator, (%))
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

data InCellPosition
  = InCellPosition { ratioX :: Rational, ratioY :: Rational } deriving Show

inCellAt :: FieldSize -> InCellPosition -> Position -> (Int, Int)
inCellAt FieldSize {..} inCellPos pos
  = ( (x pos) * cellSize
      + (cellSize * fromIntegral (numerator (ratioX inCellPos)))
        `div` fromIntegral (denominator (ratioX inCellPos))
    , (y pos) * cellSize
      + (cellSize * fromIntegral (numerator (ratioY inCellPos)))
        `div` fromIntegral (denominator (ratioY inCellPos))
    )

inCellPixelAt
  :: Image PixelRGB8 -> FieldSize -> InCellPosition -> Position
  -> PixelRGB8
inCellPixelAt img fs inCellPos pos
  = uncurry (pixelAt img) (inCellAt fs inCellPos pos)

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
openPurple4 = PixelRGB8 113 45 156
openOrange5 = PixelRGB8 234 168 89
openBlue6 = PixelRGB8 113 167 163

fieldPixels =
  [ (fieldGreenLight, Field)
  , (fieldGreenDark, Field)
  ]

openCellTypeMiddlePixels =
  [ (openLight, Open)
  , (openDark, Open)
  , (openBlue1, Number 1)
  , (openGreen2, Number 2)
  , (openRed3, Number 3)
  , (openOrange5, Number 5)
  , (openBlue6, Number 6)
  ]

closestCellTypeByPixel pixel pixels
  = minimumBy (comparing fst)
  . map (\(pixel1, cell) -> (pixelDistance pixel pixel1, cell))
  $ pixels

data ReadCellMsg
  = ReadCellMsg
  { msgType :: ReadCellMsgType
  , pos :: Position
  , pixel :: PixelRGB8
  } deriving Show

data ReadCellMsgType
  = IsFieldOrFlagMsg
  | IsNotFieldOrFlagMsg
  | IsFlagMsg
  | IsNotFlagMsg
  | ReadOpenCellFailedMsg
  | ReadOpenCellSuccessMsg
  | IsNumber4Msg
  | IsNotNumber4Msg
  deriving Show

isFieldOrFlag
  :: Image PixelRGB8 -> FieldSize -> Position
  -> Writer [ReadCellMsg] Bool
isFieldOrFlag img fs pos
  | error < 25
  = writer (True, [ReadCellMsg IsFieldOrFlagMsg pos topLeftCornerPixel])
  | otherwise
  = writer (False, [ReadCellMsg IsNotFieldOrFlagMsg pos topLeftCornerPixel])
  where
    topLeftCornerPixel
      = inCellPixelAt img fs (InCellPosition (1%10) (1%10)) pos
    (error, _) = closestCellTypeByPixel topLeftCornerPixel fieldPixels

isFlag img fs pos
  | error < 25 = writer (True, [ReadCellMsg IsFlagMsg pos pixel])
  | otherwise = writer (False, [ReadCellMsg IsNotFlagMsg pos pixel])
  where
    pixel = inCellPixelAt img fs (InCellPosition (1%4) (1%4)) pos
    error = pixelDistance pixel flag

isNumber4 img fs pos
  | error < 5 = writer (True, [ReadCellMsg IsNumber4Msg pos pixel])
  | otherwise = writer (False, [ReadCellMsg IsNotNumber4Msg pos pixel])
  where
    pixel = inCellPixelAt img fs (InCellPosition (35%60) (35%60)) pos
    error = pixelDistance pixel openPurple4

readOpenCellMiddleAt
  :: Image PixelRGB8 -> FieldSize -> Position
  -> Writer [ReadCellMsg] (Maybe Cell)
readOpenCellMiddleAt img fs pos
  | error < 25 = do
      tell [ReadCellMsg ReadOpenCellSuccessMsg pos centerPixel]
      return . Just $ Cell closestCellType pos
  | otherwise = do
      tell [ReadCellMsg ReadOpenCellFailedMsg pos centerPixel]
      return Nothing
  where
    centerPixel = inCellPixelAt img fs (InCellPosition (1%2) (1%2)) pos
    (error, closestCellType) = closestCellTypeByPixel centerPixel openCellTypeMiddlePixels


readCellAt
  :: Image PixelRGB8 -> FieldSize -> Position
  -> Writer [ReadCellMsg] (Maybe Cell)
readCellAt img fs pos = do
  ifM (isFieldOrFlag img fs pos)
    ( ifM (isFlag img fs pos)
      ( return . Just $ Cell Flag pos )
      ( return . Just $ Cell Field pos )
    )
    ( ifM (isNumber4 img fs pos)
      ( return . Just $ Cell (Number 4) pos )
      ( readOpenCellMiddleAt img fs pos)
    )

readFieldWithMsgs
  :: Image PixelRGB8 -> FieldSize
  -> [[(Maybe Cell, [ReadCellMsg])]]
readFieldWithMsgs img fs@FieldSize {..}
  = [
      [runWriter $ readCellAt img fs (Position x y)
      | x <- [0 .. fieldWidth - 1]]
    | y <- [0..fieldHeight - 1]
    ]

fromCellsWithMsgs fieldWithMsgs = do
  rowOfCells <- fieldWithMsgs
  return $ do
    (cell, msgs) <- rowOfCells
    return $ maybe (error $ show msgs) id cell

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
-- r <- returnSession remoteConfig (openField Medium)
-- pPrintNoColor (map (map cellType) . snd $ r)
-- pPrintNoColor . fromCellsWithMsgs $ snd r
-- pPrintNoColor =<< runWD (fst r) readFieldFromScreen
-- r <- returnSession remoteConfig (openFieldWithMsgs Hard)
-- img <- runWD (fst r) (convertRGB8 <$> takeFieldScreenshot)
