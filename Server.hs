{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum, mzero)
import Data.Data (Data, Typeable)
import Data.Maybe (fromJust)
import Data.Aeson
import Happstack.Server
import Happstack.Server.Types
import Control.Applicative ((<$>), (<*>))
import Parser (parseLine, Person(Person, firstName, gender, lastName, dob, favoriteColor))

main :: IO ()
main = simpleHTTP nullConf myApp

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

myApp :: ServerPart Response
myApp = do decodeBody myPolicy
           msum [ dir "unit"   $ postUnit ]

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return ""

postUnit :: ServerPart Response
postUnit = do
  body <- getBody
  case parseLine $ L.unpack body of
    Just person -> ok $ toResponse $ encode $ person
    Nothing   -> badRequest $ toResponse $ ("Could not parse" :: String)
