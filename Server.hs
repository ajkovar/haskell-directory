{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BlockArguments #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum, mzero)
import Data.Data (Data, Typeable)
import Data.Maybe (fromJust)
import Data.Aeson
import Happstack.Server
import Happstack.Server.Types
import Data.IORef.Lifted (newIORef, readIORef, writeIORef, IORef)
import Control.Applicative ((<$>), (<*>))
import Parser (parseLine, Person(Person, firstName, gender, lastName, dob, favoriteColor))
import Data.Sort (sortBy)
-- import control.monad.trans (liftio)

main :: IO ()
main = do
  ref <- newIORef $ ([] :: [Person])
  readIORef ref >>= print
  simpleHTTP nullConf $ myApp ref

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

myApp :: IORef [Person] -> ServerPart Response
myApp ref = do 
  decodeBody myPolicy
  msum [ 
          dirs "records/gender"   $ getPeopleByGender ref
        , dir "records"   $ postPerson ref
       ]

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return ""

getPeopleByGender :: IORef [Person] -> ServerPart Response
getPeopleByGender ref = do
  people <- readIORef ref
  ok $ toResponse $ encode $ sortBy (\p1 p2 -> compare (gender p1) (gender p2)) people

postPerson :: IORef [Person] -> ServerPart Response
postPerson ref = do
  body <- getBody
  case parseLine $ L.unpack body of
    Just person -> do
      people <- readIORef ref
      writeIORef ref (people ++ [person])
      ok $ toResponse $ encode $ person
    Nothing   -> badRequest $ toResponse $ ("Could not parse!" :: String)