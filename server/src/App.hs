{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Map
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

options :: Options
options = Options "client"

app :: IO Application
app = serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets options
  db     <- mkDB
  return (apiServer db :<|> Tagged assets)

apiServer :: DB -> Server Api
apiServer db = listItems db :<|> getItem db :<|> postItem db :<|> deleteItem db
  :<|> putImage db

putImage :: DB -> ItemId -> String -> Handler ()
putImage db@(DB mvar) n url = liftIO $ do
  let updateUrl (Item id name _) = Item id name url
  mItem <- lookupItem db n
  case mItem of
    Nothing -> return ()
    Just item ->
        modifyMVar_ mvar $ \m -> return $ adjust updateUrl n m

listItems :: DB -> Handler [ItemId]
listItems db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwError err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new = liftIO $ insertItem db new

-- fake DB

newtype DB = DB (MVar (Map ItemId Item))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print
mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \m -> do
  let newKey = case keys m of
        [] -> ItemId 0
        ks -> succ (maximum ks)
      newItem = Item newKey new ""
  return (insert newKey newItem m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) = keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m ()
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \m -> return (delete i m)
  return ()

