{- | Module for interacting with the NIST Randomness Beacon (prototype)
     web API. The functions exported from this module return 512-bit
     full-entropy ByteStrings that you can use for whatever you want.
     New values are computed every 60 seconds on the NIST server.

     For more information, see the project homepage:

         https://beacon.nist.gov/home -}


module Net.Beacon
       ( Timestamp
       , Record()
       , interval
       , timeStamp
       , seedValue
       , previousOutputValue
       , signatureValue
       , outputValue
       , statusCode
       , getLastRecord
       , getCurrentRecord
       , getPreviousRecord
       , getNextRecord
       , getStartChainRecord
       ) where


import Control.Monad

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types

import Network.HTTP (getResponseBody, simpleHTTP, getRequest)

import qualified Data.ByteString.Lazy.Char8 as B


data Record = Record { interval            :: Int
                     , timeStamp           :: Int
                     , seedValue           :: B.ByteString
                     , previousOutputValue :: B.ByteString
                     , signatureValue      :: B.ByteString
                     , outputValue         :: B.ByteString
                     , statusCode          :: Int
                     } deriving (Show, Eq)


type Timestamp = Int


getXmlData :: String -> IO B.ByteString
getXmlData url = do
  body <- getResponseBody <=< simpleHTTP $ getRequest url
  return $ B.pack body


getRecord :: B.ByteString -> Maybe Record
getRecord stuff = do
  xml <- parseXMLDoc stuff
  let fc = findChild' xml
  Record
    <$> (read   <$> fc "frequency")
    <*> (read   <$> fc "timeStamp")
    <*> (B.pack <$> fc "seedValue")
    <*> (B.pack <$> fc "previousOutputValue")
    <*> (B.pack <$> fc "signatureValue")
    <*> (B.pack <$> fc "outputValue")
    <*> (read   <$> fc "statusCode")
  where
    findChild' xml name = strContent <$> findChild (QName name Nothing Nothing) xml


getLastRecord :: IO (Maybe Record)
getLastRecord = do
  x <- getXmlData "http://beacon.nist.gov/rest/record/last"
  return $ getRecord x


getCurrentRecord :: Timestamp -> IO (Maybe Record)
getCurrentRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  return $ getRecord x


getPreviousRecord :: Timestamp -> IO (Maybe Record)
getPreviousRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/previous/" ++ (show ts)
  return $ getRecord x


getNextRecord :: Timestamp -> IO (Maybe Record)
getNextRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/next/" ++ (show ts)
  return $ getRecord x


getStartChainRecord :: Timestamp -> IO (Maybe Record)
getStartChainRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/start-chain/" ++ (show ts)
  return $ getRecord x
