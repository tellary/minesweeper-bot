{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Bot
import Codec.Picture
import Codec.Picture.Extra        (crop)
import Control.Concurrent         (threadDelay)
import Control.DeepSeq            (rnf)
import Control.Lens               hiding (element)
import Control.Lens.TH
import Control.Monad              (forM_)
import Control.Monad.Catch        (Exception, MonadThrow, handle, throwM)
import Control.Monad.Extra        (ifM)
import Control.Monad.IO.Class     (MonadIO (liftIO))
import Control.Monad.Writer       (Writer, runWriter, tell, writer)
import Data.ByteString.Lazy       (ByteString, toStrict)
import Data.Foldable              (fold)
import Data.Functor.Compose       (Compose (Compose), getCompose)
import Data.List                  (group, minimumBy, nub)
import Data.Ord                   (comparing)
import Data.Ratio                 (Ratio, denominator, numerator, (%))
import Data.Text                  (unpack)
import Data.Time
import Field                      (Cell (..), CellField, CellType (..),
                                   FieldSize (..), ImgFieldSize (..),
                                   Position (..), mkField)
import Game
import Test.WebDriver
import Test.WebDriver.Common.Keys (enter)
import Test.WebDriver.Session
import Text.Printf                (printf)

-- ./chromedriver --port=9515 --log-level=ALL --url-base=/wd/hub
-- ./chromedriver --port=9515 --url-base=/wd/hub
remoteConfig = useBrowser chrome defaultConfig { wdHost = "localhost"
                                               , wdPort = 9515
                                               }

timeItNamed :: MonadIO m => String -> m a -> m a
timeItNamed name a = do
  start <- liftIO $ getCurrentTime
  result <- a
  stop <- liftIO $ getCurrentTime
  let time :: Double
        = realToFrac . nominalDiffTimeToSeconds $ diffUTCTime stop start
  liftIO $ printf "%s: %6.2fs\n" name time
  return result

returnSession config wd = runSession config $ do
  sess <- getSession
  a <- wd
  return (sess, a)

openGame :: GameSize -> WD (DynamicImage, Int)
openGame size = do
  openPage "https://www.google.com"
  openGameFromSearch size

openGameFromSearch :: GameSize -> WD (DynamicImage, Int)
openGameFromSearch size = do
  searchTextarea <- findElem (ByTag "textarea")
  sendKeys "minesweeper" searchTextarea
  sendKeys enter searchTextarea
  playButton <- findElem (ByXPath "//div[text() = 'Play']")
  keysSent <- sendKeys enter playButton

  selectSize size

  canvas <- findElem (ByTag "canvas")
  click canvas
  -- wait 1 second to allow visual effects to pass
  liftIO (threadDelay (1*1000*1000))
  takeFieldScreenshot
  
selectSize Medium = return ()
selectSize size = do
  click =<< findElem (ByTag "g-dropdown-menu")
  case size of
    Easy -> click =<< findElem (ByXPath "//div[text() = 'Easy']")
    Hard -> click =<< findElem (ByXPath "//div[text() = 'Hard']")

takeFieldScreenshot :: WD (DynamicImage, Int)
takeFieldScreenshot = do
  moveToFrom (0, 0) =<< findElem (ByTag "body")
  canvas <- findElem (ByTag "canvas")
  (x, y) <- elemPos canvas
  liftIO (putStrLn $ "(x, y): " ++ show (x, y))
  (width, height) <- elemSize canvas
  liftIO (putStrLn $ "(width, height): " ++ show (width, height))
  bodyXY <- elemPos =<< findElem (ByTag "body")
  scr <- timeItNamed "screenshot" screenshot
  img <- timeItNamed "decode screenshot" $ do
    return . either undefined id . decodeImage . toStrict $ scr
  cropped <- timeItNamed "crop screenshot" . return
    $ dynamicPixelMap
      (crop
        (truncate (2 * x))
        (truncate (2 * y))
        (truncate (2 * width))
        (truncate (2 * height))
      )
      img
  flagsAndTime <- findElem (ByXPath "//img[contains(@src, 'flag_icon.png')]/..")
  flags <- getText =<< findElemFrom flagsAndTime (ByTag "div")
  return (cropped, read . unpack $ flags)

readImgFieldSize :: Pixel a => Image a -> ImgFieldSize
readImgFieldSize img
  = ImgFieldSize
    (FieldSize
      (imageWidth  img `div` firstCellSize)
      (imageHeight img `div` firstCellSize)
    )
    firstCellSize
  where
    firstCellSize
      = 1 + (length . head . group . map (\i -> pixelAt img i 0) $ [0..])

data InCellPosition
  = InCellPosition { ratioX :: Rational, ratioY :: Rational } deriving Show

inCellAt :: ImgFieldSize -> InCellPosition -> Position -> (Int, Int)
inCellAt ImgFieldSize {imgCellSize = cellSize} inCellPos pos
  = ( (x pos) * cellSize
      + (cellSize * fromIntegral (numerator (ratioX inCellPos)))
        `div` fromIntegral (denominator (ratioX inCellPos))
    , (y pos) * cellSize
      + (cellSize * fromIntegral (numerator (ratioY inCellPos)))
        `div` fromIntegral (denominator (ratioY inCellPos))
    )

inCellPixelAt
  :: Image PixelRGB8 -> ImgFieldSize -> InCellPosition -> Position
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
openPurple4 = PixelRGB8 113 39 156
openOrange5 = PixelRGB8 234 168 89
openBlue6 = PixelRGB8 113 167 163
openBlack7 = PixelRGB8 66 66 66

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
  , (openBlack7, Number 7)
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
  , pixelError :: Float
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

errorThreshold = 25

isFieldOrFlag
  :: Image PixelRGB8 -> ImgFieldSize -> Position
  -> Writer [ReadCellMsg] Bool
isFieldOrFlag img fs pos
  | error < errorThreshold
  = writer (True, [ReadCellMsg IsFieldOrFlagMsg pos topLeftCornerPixel error])
  | otherwise
  = writer (False, [ReadCellMsg IsNotFieldOrFlagMsg pos topLeftCornerPixel error])
  where
    topLeftCornerPixel
      = inCellPixelAt img fs (InCellPosition (1%10) (1%10)) pos
    (error, _) = closestCellTypeByPixel topLeftCornerPixel fieldPixels

isFlag img fs pos
  | error < errorThreshold
  = writer (True, [ReadCellMsg IsFlagMsg pos pixel error])
  | otherwise = writer (False, [ReadCellMsg IsNotFlagMsg pos pixel error])
  where
    pixel = inCellPixelAt img fs (InCellPosition (1%4) (1%4)) pos
    error = pixelDistance pixel flag

isNumber4 img fs pos
  | error < 7
  = writer (True, [ReadCellMsg IsNumber4Msg pos pixel error])
  | otherwise = writer (False, [ReadCellMsg IsNotNumber4Msg pos pixel error])
  where
    pixel = inCellPixelAt img fs (InCellPosition (35%60) (35%60)) pos
    error = pixelDistance pixel openPurple4

readOpenCellMiddleAt
  :: Image PixelRGB8 -> ImgFieldSize -> Position
  -> Writer [ReadCellMsg] (Maybe Cell)
readOpenCellMiddleAt img fs pos
  | error < errorThreshold = do
      tell [ReadCellMsg ReadOpenCellSuccessMsg pos centerPixel error]
      return . Just $ Cell closestCellType pos
  | otherwise = do
      tell [ReadCellMsg ReadOpenCellFailedMsg pos centerPixel error]
      return Nothing
  where
    centerPixel = inCellPixelAt img fs (InCellPosition (1%2) (1%2)) pos
    (error, closestCellType) = closestCellTypeByPixel centerPixel openCellTypeMiddlePixels


readCellAt
  :: Image PixelRGB8 -> ImgFieldSize -> Position
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
  :: Image PixelRGB8 -> ImgFieldSize
  -> [[(Maybe Cell, [ReadCellMsg])]]
readFieldWithMsgs img fs@ImgFieldSize {imgFieldSize = FieldSize{..}}
  = [
      [runWriter $ readCellAt img fs (Position x y)
      | x <- [0 .. fieldWidth - 1]]
    | y <- [0..fieldHeight - 1]
    ]

data ReadFieldException = ReadFieldException [ReadCellMsg] deriving Show

instance Exception ReadFieldException

fromCellsWithMsgs
  :: MonadThrow m => [[(Maybe Cell, [ReadCellMsg])]] -> m [[Cell]]
fromCellsWithMsgs cellsWithMsgs
  = either
    (\msgs -> throwM $ ReadFieldException msgs)
    (return . getCompose)
    eitherMsgsOrCells
  where
    xs = Compose cellsWithMsgs
    es = fmap
         (\(cell, msgs) -> maybe (Left msgs) Right cell)
         xs
    eitherMsgsOrCells = sequence es

data Screen
  = Screen
  { _screenImage :: Image PixelRGB8
  , _screenFieldSize :: ImgFieldSize
  , _screenGame :: Game
  }

$(makeLenses ''Screen)

readField :: Image PixelRGB8 -> ImgFieldSize -> WD CellField
readField img fs
  = timeItNamed "readField"
    (mkField <$> (fromCellsWithMsgs $ readFieldWithMsgs img fs))

readScreen :: GameSize -> WD Screen
readScreen size = do
  (dynImg, flags) <- takeFieldScreenshot
  let img = convertRGB8 dynImg
  let fs = readImgFieldSize img
  Screen img fs . Game size Nothing flags <$> readField img fs

updateScreen :: Screen -> WD Screen
updateScreen screen
  = handle (\(e :: ReadFieldException) -> do
               liftIO (putStrLn . show $ e)
               updateScreen screen) $ do
  (dynImg, flags) <- takeFieldScreenshot
  let img = convertRGB8 dynImg
  cellField <- readField img (screen^.screenFieldSize)
  return $ screen
    & screenGame . field .~ cellField
    & screenImage .~ img
    & screenGame . flagsLeft .~ flags

readScreenWithMsg = do
  (dynImg, flags) <- takeFieldScreenshot
  let img = convertRGB8 dynImg
  let fs = readImgFieldSize img
  return $ readFieldWithMsgs img fs

openScreen :: GameSize -> WD Screen
openScreen size = do
  openPage "https://www.google.com"
  openScreenFromSearch size

openScreenFromSearch :: GameSize -> WD Screen
openScreenFromSearch size = do
  (dynImg, flags) <- openGameFromSearch size
  let img8 = convertRGB8 dynImg
  let fs = readImgFieldSize img8
  let buildScreen
        = Screen img8 fs . Game size Nothing flags <$> readField img8 fs
  let buildScreenLoop
        = handle (
        \(e :: ReadFieldException)
        -> liftIO (putStrLn . show $ e) >> buildScreenLoop
        ) $ buildScreen
  buildScreenLoop

openScreenWithMsgs size = do
  (dynImg, flags) <- openGame size
  let img = convertRGB8 dynImg
  let fs = readImgFieldSize img
  return $ readFieldWithMsgs img fs

-- r <- returnSession remoteConfig (openScreenWithMsgs Easy)
-- r <- returnSession remoteConfig (openScreen Medium)
-- pPrintNoColor (map (map cellType) . snd $ r)
-- pPrintNoColor . fromCellsWithMsgs $ snd r
-- pPrintNoColor =<< (runWD (fst r) $ (fmap cellType . gameField) <$> readScreen)
-- r <- returnSession remoteConfig (openScreenWithMsgs Hard)
-- img <- runWD (fst r) (convertRGB8 <$> takeFieldScreenshot)

moveToCell :: Element -> ImgFieldSize -> Position -> WD ()
moveToCell canvas fs pos = do
  (x, y) <- elemPos canvas
  let centerCell = inCellAt fs (InCellPosition (1%2) (1%2)) pos
  let screenPos
        = ( fst centerCell `div` 2
          , snd centerCell `div` 2
          )
  moveToFrom screenPos canvas
  
markCell :: Element -> ImgFieldSize -> Position -> WD ()
markCell canvas fs pos = do
  moveToCell canvas fs pos
  liftIO (threadDelay (10*1000))
  clickWith RightButton

digAroundCell canvas fs pos = do
  moveToCell canvas fs pos
  withMouseDown (clickWith RightButton)

openCell canvas fs pos = do
  moveToCell canvas fs pos
  clickWith LeftButton

performOnCells
  :: Foldable t
  => (Element -> ImgFieldSize -> Position -> WD ())
  -> ImgFieldSize
  -> t Position
  -> WD ()
performOnCells action fs cellPositions = do
  canvas <- findElem (ByTag "canvas")
  forM_ cellPositions (action canvas fs)

markCells :: Foldable t => ImgFieldSize -> t Position -> WD ()
markCells fs ps
  = timeItNamed (printf "markCells (%d)" (length ps))
  . performOnCells markCell fs $ ps

digAroundCells :: Foldable t => ImgFieldSize -> t Position -> WD ()
digAroundCells = performOnCells digAroundCell

openCells :: Foldable t => ImgFieldSize -> t Position -> WD ()
openCells fs ps
  = timeItNamed (printf "openCells (%d)" (length ps))
  . performOnCells openCell fs $ ps

play size = do
  field <- openScreen size
  continuePlay field

restartPlay screen =
  continuePlay
    (screen & screenGame . flagsLeft .~ initialFlags (screen^.screenGame.size))

performActions screen = do
  let game = screen^.screenGame
  let ((toMark, toOpen), game') = runGame solve2 game
  markCells (screen^.screenFieldSize) toMark
  openCells (screen^.screenFieldSize) toOpen
  liftIO $ printf "flagsLeft: %d\n" (game'^.flagsLeft)
  return (not . null $ toMark, screen & screenGame .~ game')

continuePlay :: Screen -> WD ()
continuePlay screen = do
  (marked, screen') <- performActions screen
  screen'' <- updateScreen screen'
  if marked
    then do
      continuePlay screen''
    else do
      exitPlay screen'' 1

exitPlay :: Screen -> Int -> WD ()
exitPlay _ 1000 = do
  liftIO $ putStrLn "Stopped playing"
  return ()
exitPlay screen count = do
  (marked, screen') <- performActions screen
  screen'' <- updateScreen screen'
  if marked
    then
      continuePlay screen''
    else do
      liftIO . putStrLn $ "No flags set, count: " ++ show count
      exitPlay screen'' (count + 1)

-- r <- returnSession remoteConfig (play Hard)
-- r <- returnSession remoteConfig (openScreen Hard)
-- runWD (fst r) $ restartPlay (snd r)
--
-- r <- returnSession remoteConfig (return ())
-- runWD (fst r) $ openPage "https://www.google.com"
-- field <- runWD (fst r) $ openScreenFromSearch Hard
-- runWD (fst r) $ continuePlay field

main = returnSession remoteConfig (play Hard)
