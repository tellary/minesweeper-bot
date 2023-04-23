{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleMinesweeper where

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

upperLeftThirdOfCellAt :: FieldSize -> Int -> Int -> (Int, Int)
upperLeftThirdOfCellAt FieldSize {..} x y
  = ( x * cellSize + cellSize `div` 3, y * cellSize + cellSize `div` 3)

upperLeftThirdOfCellPixelAt img fs x y
  = uncurry (pixelAt img) (upperLeftThirdOfCellAt fs x y)

fieldGreenLight = PixelRGB8 180 212 102
fieldGreenDark = PixelRGB8 172 206 95
flagLight = PixelRGB8 181 206 98
flagDark = PixelRGB8 174 200 92
openLight = PixelRGB8 224 195 164
openDark = PixelRGB8 211 185 157
openBlue1 = PixelRGB8 52 119 203
openGreenDark2 = PixelRGB8 103 148 87
openGreenLight2 = PixelRGB8 106 150 88
openRed3 = PixelRGB8 195 63 56
openPurpleLight4 = PixelRGB8 217 184 163
openPurpleDark4 = PixelRGB8 204 175 157
openOrange5 = PixelRGB8 234 168 89 -- Light

readCellAt :: Image PixelRGB8 -> FieldSize -> Int -> Int -> Cell
readCellAt img fs x y
  | centerPixel == fieldGreenLight
  = Field
  | centerPixel == fieldGreenDark
  = Field
  | centerPixel == flagLight
  = Flag
  | centerPixel == flagDark
  = Flag
  | centerPixel == openLight
  = Open
  | centerPixel == openDark
  = Open
  | centerPixel == openBlue1
  = Number 1
  | centerPixel == openGreenLight2
  = Number 2
  | centerPixel == openGreenDark2
  = Number 2
  | centerPixel == openRed3
  = Number 3
  | centerPixel == openPurpleLight4
  = Number 4
  | centerPixel == openPurpleDark4
  = Number 4
  | centerPixel == openOrange5
  = Number 5
  | otherwise = error $ "Can't read cell: " ++ show centerPixel
  where
    centerPixel = centerOfCellPixelAt img fs x y

readField :: Image PixelRGB8 -> FieldSize -> Field
readField img fs@FieldSize {..}
  = [
      [readCellAt img fs x y | x <- [0 .. fieldWidth - 1]]
      | y <- [0..fieldHeight - 1]
    ]

openField size = do
  screen <- openGame size
  let img = convertRGB8 screen
  let fs = readFieldSize img
  return $ readField img fs

digCell :: IO ()
digCell = undefined

-- r <- returnSession remoteConfig (openField Easy)


