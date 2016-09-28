{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad
import Data.Aeson
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text)
import Data.Monoid ((<>))

data Payload =
    Payload { title     :: Text
            , content   :: Text
            , author    :: Text
            } | NothingFound deriving (Show, Eq)

instance FromJSON Payload where
    parseJSON (Object v) =
        Payload <$> v .: "title"
                <*> v .: "content"
                <*> v .: "author"
    parseJSON _ = mzero

getJsonAction :: ActionM ()
getJsonAction = do
    j <- jsonData
    text $ case j of
      Payload t c a -> "The Title of your Song is: " <> t
        <> " with the content: " <> c
        <> " and author " <> a
      NothingFound -> "Nothing"

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    post "/" getJsonAction

-- usage: curl -v --data "{\"title\":\"yeah\",\"content\":\"cool song\",\"author\":\"Fabian\"}" http://localhost:3000
