{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Int
import Data.Bits
import Data.String
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Aeson

import System.IO
import System.Environment
import System.Directory

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Version
import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Concurrent.Async        as A
import Control.Concurrent.Async.Lifted as AL

-- | Parsing nozomi file
parseNozomiBinary :: BS.ByteString -> [Int32]
parseNozomiBinary bs = parse' (BS.unpack bs)
  where
    parse' [] = []
    parse' (x0:x1:x2:x3:xs) = (packInt32 x0 x1 x2 x3) : (parse' xs)
    packInt32 x0 x1 x2 x3 = x0' .|. x1' .|. x2' .|. x3' where
      x0' = shiftL (fromIntegral x0) 24 :: Int32
      x1' = shiftL (fromIntegral x1) 16 :: Int32
      x2' = shiftL (fromIntegral x2) 8  :: Int32
      x3' = shiftL (fromIntegral x3) 0  :: Int32

parseNozomiText :: BS.ByteString -> [Int32]
parseNozomiText bs = ints where
  texts = BSC.splitWith (=='\n') bs
  ints = [fromIntegral int | Just (int,_) <- map BSC.readInt texts]

-- | Sync monad
data SyncEnvironment =
  SyncEnvironment
  { stdoutSem  :: TSem
  , succeedSem :: TSem
  , failedSem  :: TSem
  , hSucceed :: Handle
  , hFailed  :: Handle
  , httpManager :: Manager
  }

type Sync = ReaderT SyncEnvironment IO

putStrLnStdout :: String -> Sync ()
putStrLnStdout str = do
  sem <- reader stdoutSem
  liftIO . atomically . waitTSem $ sem
  liftIO . putStrLn $ str
  liftIO . atomically . signalTSem $ sem

putStrLnSucceed :: String -> Sync ()
putStrLnSucceed str = do
  sem <- reader succeedSem
  handle <- reader hSucceed
  liftIO . atomically . waitTSem $ sem
  liftIO $ hPutStrLn handle str
  liftIO . atomically . signalTSem $ sem

putStrLnFailed :: String -> Sync ()
putStrLnFailed str = do
  sem <- reader failedSem
  handle <- reader hFailed
  liftIO . atomically . waitTSem $ sem
  liftIO $ hPutStrLn handle str
  liftIO . atomically . signalTSem $ sem

-- | Gallery information & Parsing
type GalleryId = String
type ImageName = String

data ImageInfo = ImageInfo
  { name    :: T.Text
  , width   :: Int
  , height  :: Int
  , haswebp :: Int
  }
  deriving (Show)

instance FromJSON ImageInfo where
  parseJSON = withObject "ImageInfo" $ \v -> ImageInfo
    <$> v .: "name"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "haswebp"

parseGalleryJs :: BSL.ByteString -> Maybe [ImageName]
parseGalleryJs = parse >=> return . map (T.unpack . name)
  where
    parse = decode . (BSL.drop 18) :: BSL.ByteString -> Maybe [ImageInfo]

-- | Resource names
domainName :: GalleryId -> String
domainName gid = prefix:"a.hitomi.la" where
  frontendNum = 2
  g = last gid
  prefix =
    if (ord g - ord '0') `mod` frontendNum == 0
      then 'a'
      else if g == '1'
        then 'a'
        else 'b'

galleryHtmlUrl :: GalleryId -> String
galleryHtmlUrl gid = "hitomi.la/galleries/" ++ gid ++ ".html"

galleryJsUrl :: GalleryId -> String
galleryJsUrl gid = "ltn.hitomi.la/galleries/" ++ gid ++ ".js"

galleryBlockUrl :: GalleryId -> String
galleryBlockUrl gid = "ltn.hitomi.la/galleryblock/" ++ gid ++ ".html"

galleryImageDirUrl :: GalleryId -> String
galleryImageDirUrl gid = (domainName gid) ++ "/galleries/" ++ gid

galleryImageUrl :: GalleryId -> ImageName -> String
galleryImageUrl gid name = (galleryImageDirUrl gid) ++ "/" ++ name

localUrl :: String -> String
localUrl = ("hitomi/" ++)

networkUrl :: String -> String
networkUrl = ("https://" ++)

-- | Error tracing
traceResponseError :: String -> Response a -> Sync ()
traceResponseError callee response = putStrLnStdout output
  where
    status = responseStatus response
    code = statusCode status
    msg = statusMessage status
    output = "HTTP Error from : " ++ callee ++ "\n"
          ++ "Error Code : " ++ (show code) ++ "\n"
          ++ BSC.unpack msg

traceHttpException :: HttpException -> Sync ()
traceHttpException = putStrLnStdout . show

-- | Proxy setting
proxySetting :: Maybe Proxy
proxySetting = Just (Proxy "127.0.0.1" 30001)

-- | Ensure resources
parseRequest' :: MonadThrow m => String -> m Request
parseRequest' str = parseRequest (replace str)
  where
    replace [] = []
    replace (x:xs) = case x of
      '[' -> "%5B" ++ replace xs
      ']' -> "%5D" ++ replace xs
      c -> c:replace xs

(=>>) :: Monad m => (a -> m b) -> m c -> a -> m c
k =>> m = \x -> k x >> m

ensureGalleryHtml :: GalleryId -> Sync Bool
ensureGalleryHtml gid = do
  hasGalleryHtml <- liftIO $ doesFileExist (localUrl url)
  if hasGalleryHtml
    then return True
    else do
      putStrLnStdout ("Fetching : galleryHtml " ++ gid)
      _request <- parseRequest' (networkUrl url)
      let request = _request { requestVersion = http11
                             , proxy = proxySetting
                             }
      manager <- reader httpManager
      catch (liftIO (httpLbs request manager) >>= process) (traceHttpException =>> return False)
  where
    url = galleryHtmlUrl gid
    process response =
      if status == status200
        then liftIO $ BSL.writeFile (localUrl url) body >> return True
        else if status == status404
          then return True
          else (traceResponseError callee response) >> return False
      where
        body = responseBody response
        status = responseStatus response
        callee = "ensureGalleryHtml " ++ gid

ensureGalleryJs :: GalleryId -> Sync (Maybe BSL.ByteString)
ensureGalleryJs gid = do
  hasGalleryJs <- liftIO $ doesFileExist (localUrl url)
  if hasGalleryJs
    then liftIO $ BSL.readFile (localUrl url) >>= return . Just
    else do
      putStrLnStdout ("Fetching : galleryJs " ++ gid)
      _request <- parseRequest' (networkUrl url)
      let request = _request { requestVersion = http11
                             , proxy = proxySetting
                             }
      manager <- reader httpManager
      catch (liftIO (httpLbs request manager) >>= process) (traceHttpException =>> return Nothing)
  where
    url = galleryJsUrl gid
    process response =
      if status == status200
        then liftIO $ BSL.writeFile (localUrl url) body >> return (Just body)
        else (traceResponseError callee response) >> return Nothing
      where
        body = responseBody response
        status = responseStatus response
        callee = "ensureGalleryJs " ++ gid

ensureGalleryBlock :: GalleryId -> Sync Bool
ensureGalleryBlock gid = do
  hasGalleryBlock <- liftIO $ doesFileExist (localUrl url)
  if hasGalleryBlock
    then return True
    else do
      putStrLnStdout ("Fetching : galleryBlock " ++ gid)
      _request <- parseRequest' (networkUrl url)
      let request = _request  { requestVersion = http11
                              , proxy = proxySetting
                              }
      manager <- reader httpManager
      catch (liftIO (httpLbs request manager) >>= process) (traceHttpException =>> return False)
  where
    url = galleryBlockUrl gid
    process response = 
      if status == status200
        then liftIO $ BSL.writeFile (localUrl url) body >> return True
        else (traceResponseError callee response) >> return False
      where
        body = responseBody response
        status = responseStatus response
        callee = "ensureGalleryBlock " ++ gid

ensureGalleryImage :: GalleryId -> ImageName -> Sync Bool
ensureGalleryImage gid name = do
  hasGalleryImage <- liftIO $ doesFileExist (localUrl url)
  if hasGalleryImage
    then return True
    else do
      putStrLnStdout ("Fetching : galleryImage " ++ gid ++ " " ++ name)
      _request <- parseRequest' (networkUrl url)
      let request = _request  { requestHeaders = headers
                              , requestVersion = http11
                              , proxy = proxySetting
                              , responseTimeout = responseTimeoutMicro 60000000
                              }
      manager <- reader httpManager
      tryN 5 $ catch (liftIO (httpLbs request manager) >>= process) (traceHttpException =>> return False)
  where
    url = galleryImageUrl gid name
    headers = [("Referer", (fromString . networkUrl . galleryHtmlUrl) gid)]
    process response =
      if status == status200
        then liftIO $ BSL.writeFile (localUrl url) body >> return True
        else (traceResponseError callee response) >> return False
      where
        body = responseBody response
        status = responseStatus response
        callee = "ensureGalleryImage " ++ gid ++ " " ++ name
    tryN 0 io = return False
    tryN n io = do
      r <- io
      case r of
        True  -> return True
        False -> tryN (n-1) io

ensureGallery :: GalleryId -> Sync ()
ensureGallery gid = do
  putStrLnStdout ("Try Syncing : " ++ gid)
  
  asyncEnsureGalleryHtml  <- AL.async (ensureGalleryHtml  gid)
  asyncEnsureGalleryJs    <- AL.async (ensureGalleryJs    gid)
  asyncEnsureGalleryBlock <- AL.async (ensureGalleryBlock gid)

  maybeGalleryJs <- AL.wait asyncEnsureGalleryJs
  case maybeGalleryJs of
    Just galleryJs -> do
      case parseGalleryJs galleryJs of
        Just names -> do
          dirDoesExist <- liftIO $ doesDirectoryExist imgDirUrl
          when (not dirDoesExist) (liftIO $ createDirectory imgDirUrl)

          tasks <- mapM AL.async (map (ensureGalleryImage gid) names)
          p <- AL.wait asyncEnsureGalleryHtml
          q <- AL.wait asyncEnsureGalleryBlock
          rs <- mapM AL.wait tasks
          if p
            then if q
              then if foldr (&&) True rs
                then putStrLnStdout ("Succeed : " ++ gid) >> putStrLnSucceed gid
                else putStrLnStdout ("Failed : ensureGalleryImage " ++ gid) >> putStrLnFailed gid
              else putStrLnStdout ("Failed : ensureGalleryJs " ++ gid) >> putStrLnFailed gid
            else putStrLnStdout ("Failed : ensureGalleryHtml " ++ gid) >> putStrLnFailed gid
        Nothing -> do
          AL.wait asyncEnsureGalleryHtml
          AL.wait asyncEnsureGalleryBlock
          putStrLnStdout $ "Failed : parseGalleryJs " ++ gid
          putStrLnFailed gid
    Nothing -> do
      AL.wait asyncEnsureGalleryHtml
      AL.wait asyncEnsureGalleryBlock
      putStrLnStdout $ "Failed : ensureGalleryJs " ++ gid
      putStrLnFailed gid
  where
    imgDirUrl = localUrl (galleryImageDirUrl gid)

-- | Number of asynchronous tasks
asyncNum :: Int
asyncNum = 10

-- | Main
main :: IO ()
main = do
  args <- getArgs

  if length args == 0
    then putStrLn "Please specify the nozomi file"
    else do
      gids <- init args

      _stdoutSem  <- atomically $ newTSem 1
      _succeedSem <- atomically $ newTSem 1
      _failedSem  <- atomically $ newTSem 1

      _hSucceed <- openFile "hitomi/succeed" AppendMode
      _hFailed  <- openFile "hitomi/failed"  AppendMode

      hSetBuffering _hSucceed LineBuffering
      hSetBuffering _hFailed  LineBuffering

      _httpManager <- newManager tlsManagerSettings

      let environment =
            SyncEnvironment
            { stdoutSem    = _stdoutSem
            , succeedSem   = _succeedSem
            , failedSem    = _failedSem
            , hSucceed     = _hSucceed
            , hFailed      = _hFailed
            , httpManager  = _httpManager
            }

      loop environment [] gids

  where
    lastN n xs = reverse . (take n) . reverse $ xs
    
    parseNozomi nozomiName = do
      nozomi <- BS.readFile nozomiName
      if lastN 7 nozomiName == ".nozomi"
        then return (parseNozomiBinary nozomi)
        else return (parseNozomiText nozomi)
      
    init args = do
      new     <- mapM parseNozomi args >>= return . concat
      succeed <- BS.readFile "hitomi/succeed" >>= return . S.fromList . parseNozomiText
      failed  <- BS.readFile "hitomi/failed"  >>= return . S.fromList . parseNozomiText
      let pass x = not (S.member x succeed || S.member x failed)
      return [show gid | gid <- new, pass gid]

    loop environment tasks [] = do
      mapM_ A.wait tasks
      hClose (hSucceed environment)
      hClose (hFailed  environment)

    loop environment tasks gids =
      if length tasks < asyncNum
        then do
          task <- A.async $ runReaderT (ensureGallery $ head gids) environment
          loop environment (task:tasks) (tail gids)
        else do
          (task,_) <- A.waitAny tasks
          loop environment (filter (/=task) tasks) gids
