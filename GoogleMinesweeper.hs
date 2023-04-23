{-# LANGUAGE OverloadedStrings     #-}

module GoogleMinesweeper where

import Codec.Picture
import Codec.Picture.Extra        (crop)
import Control.Concurrent         (threadDelay)
import Control.Monad.Catch        (SomeException, catch)
import Control.Monad.IO.Class     (liftIO)
import Data.ByteString.Lazy
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
  catch (wd >>= \a -> return (sess, Right a))
    $ \(e::SomeException) -> return (sess, Left e)

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

readField :: DynamicImage -> Field
readField = undefined

openCell :: IO ()
openCell = undefined

-- writeDynamicBitmap "field.png" =<< runSession remoteConfig openGame
