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
import Parser (parseLine, Person(Person, firstName, gender, lastName, dob, favoriteColor), fullName)
import Data.Sort (sortBy)

main :: IO ()
main = do
  ref <- newIORef $ ([] :: [Person])
  simpleHTTP nullConf $ myApp ref

myApp :: IORef [Person] -> ServerPart Response
myApp ref = do 
  msum [ 
     dirs "records/gender" $ getPeopleByProp gender ref
   , dirs "records/birthdate" $ getPeopleByProp dob ref
   , dirs "records/name" $ getPeopleByProp fullName ref
   , dir "records" $ postPerson ref
   ]

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return ""

getPeopleByProp :: (Ord a) => (Person -> a) -> IORef [Person] -> ServerPart Response
getPeopleByProp prop ref = do
  people <- readIORef ref
  ok $ toResponse $ encode $ sortBy (\p1 p2 -> compare (prop p1) (prop p2)) people

postPerson :: IORef [Person] -> ServerPart Response
postPerson ref = do
  body <- getBody
  case parseLine $ L.unpack body of
    Just person -> do
      people <- readIORef ref
      writeIORef ref (people ++ [person])
      ok $ toResponse $ encode $ person
    Nothing   -> badRequest $ toResponse $ ("Could not parse!" :: String)