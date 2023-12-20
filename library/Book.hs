module Book where

import Relude
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (runResourceT, allocate, ResourceT, ReleaseKey, release)
import System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO

getDataDir :: IO FilePath
getDataDir = do
  dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
  Dir.createDirectoryIfMissing True dir
  pure dir

writeGreetingFile :: IO ()
writeGreetingFile = do
  dir <- getDataDir
  h <- IO.openFile (dir </> "greeting.txt") WriteMode
  T.hPutStrLn h (T.pack  "hello")
  T.hPutStrLn h (T.pack  "world")
  IO.hClose h

writeGreetingTry :: IO ()
writeGreetingTry = do
  dir <- getDataDir
  T.hPutStrLn IO.stderr (T.pack  "About to open the file :|")
  openResult <- tryAny $ IO.openFile (dir </> "greeting.txt") WriteMode
  case openResult of
    Left _ -> T.hPutStrLn IO.stderr (T.pack  "Cannot open file to write :(")
    Right h -> do
      T.hPutStrLn h (T.pack  "hello")
      T.hPutStrLn h (T.pack  "world")
      IO.hClose h
      T.hPutStrLn IO.stderr (T.pack  "Done :)")

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
  liftIO (T.hPutStrLn h (T.pack  "hello"))
  liftIO (T.hPutStrLn h (T.pack  "world"))

-- Exercise 1
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource fp mode = allocate (IO.openFile fp mode) IO.hClose

-- Exercise 2
handlePrintTest :: IO ()
handlePrintTest = mapM_ (runResourceT . printShowAndhShow) [ ReadMode
                                                           , WriteMode
                                                           , AppendMode
                                                           , ReadWriteMode
                                                           ]

printShowAndhShow :: IOMode -> ResourceT IO ()
printShowAndhShow mode = do
  dir <- liftIO getDataDir
  (rk, h) <- fileResource (dir </> "greeting.txt") mode

  liftIO $ IO.putStrLn $ "Printing show instances for mode: '" <> show mode <> "':"

  liftIO $ IO.putStrLn "  show:"
  liftIO $ IO.putStrLn $ "    " <> show h
  liftIO $ IO.putStrLn ""

  liftIO $ IO.putStrLn "  IO.hShow:"
  hShowHandle <- liftIO $ IO.hShow h
  liftIO $ IO.putStrLn $ "    " <> hShowHandle
  liftIO $ IO.putStrLn ""

  release rk

-- Exercise 3
howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
  hs <- openManyHandles
  putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
  h <- fileResourceMaybe
  case h of
    Just handle -> do
      hs <- openManyHandles
      pure (handle : hs)
    _ -> pure []

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- tryAny do
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    pure h
  case result of
    Right x -> pure $ Just x
    Left e  -> do
      print (displayException e)
      pure Nothing

repeatUntilIO :: Monad m => m t -> (t -> Bool) -> (t -> m a) -> m ()
repeatUntilIO getChunk isEnd f = proceed
  where
    proceed = do
      chunk <- getChunk
      unless (isEnd chunk) (f chunk >> proceed)

printFileContentsUpperCase2 :: IO ()
printFileContentsUpperCase2 = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ repeatUntilIO (T.hGetChunk h) T.null (T.putStr . T.toUpper)

-- Exercise 4
digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit

-- Exercise 5
capitalizeLast :: Text -> Text
capitalizeLast t = case T.unsnoc t of
  Just (lait, daeh) -> T.snoc lait (Char.toUpper daeh) -- snoc things (== cons)
  Nothing           -> T.empty

-- Exercise 6
unParen :: Text -> Maybe Text
unParen t = case T.breakOn (T.pack "(") t of
              (pre, post) -> case T.unpack post of
                "" -> Nothing
                _  -> case T.breakOnEnd (T.pack ")") (T.tail post) of
                  (ppost, ppre) -> case T.unpack ppost of
                    "" -> Nothing
                    _  -> case T.unsnoc ppost of
                      Nothing -> Nothing
                      Just x -> Just $ pre <> fst x <> ppre

-- Exercise 7
characterCount :: FilePath -> IO Int
characterCount fp = runResourceT $ do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO $ loop h
    where
      loop h = do
        chunk <- T.hGetChunk h
        (if T.null chunk then pure 0 else (do
          s <- loop h
          pure $ T.length chunk + s))

-- Exercise 8
repeatUntil :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m ()) -> m ()
repeatUntil getChunk isEnd f = proceed
  where
    proceed = do
      chunk <- getChunk
      unless (isEnd chunk) (f chunk >> proceed)

-- Exercise 9
-- myWhen :: Monad m => Bool -> m () -> m ()
-- myWhen b m = if b then m else pure ()

-- myUnless :: Monad m => Bool -> m () -> m ()
-- myUnless b = myWhen (not b)

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 108, 111] :: [Word8]

copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
  (_, h2) <- binaryFileResource (dir </> "greeting2.txt") WriteMode
  liftIO $ repeatUntil (BS.hGetSome h1 1_024) BS.null (BS.hPutStr h2)

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

-- Exercise 11
asciiUpper :: ByteString -> ByteString
asciiUpper =  BS.map (\c -> case () of
                              _ | c > 96 && c < 123 -> c - 32
                                | otherwise         -> c)
