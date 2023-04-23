{-# LANGUAGE OverloadedStrings     #-}

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

openGame :: WD DynamicImage
openGame = do
  openPage "https://www.google.com"
  searchTextarea <- findElem (ByTag "textarea")
  sendKeys "minesweeper" searchTextarea
  sendKeys enter searchTextarea
  playButton <- findElem (ByXPath "//div[text() = 'Play']")
  keysSent <- sendKeys enter playButton
  canvas <- findElem (ByTag "canvas")
  click canvas
  -- wait 3 seconds to allow visual effects to pass
  liftIO (threadDelay (3*1000*1000))
  takeFieldScreenshot

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

readFieldDimensions :: Pixel a => Image a -> (Int, Int)
readFieldDimensions img
  = (imageWidth img `div` firstCellSize, imageHeight img `div` firstCellSize)
  where
    firstCellSize
      = 1 + (length . head . group . map (\i -> pixelAt img i 0) $ [0..])

readField :: DynamicImage -> Field
readField = undefined

openField = dynamicMap readFieldDimensions <$> openGame

digCell :: IO ()
digCell = undefined

-- r <- returnSession remoteConfig openGame
-- img <- runWD (fst r) takeFieldScreenshot
-- img2 = convertRGB8 img
-- pPrintNoColor . take 10 . map (\g -> (length g, head g)) . group . map (\i -> pixelAt img2 i 0) $ [0..]
-- dynamicMap readFieldDimensions img
-- r <- returnSession remoteConfig openField
