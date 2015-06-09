{- | Module for interacting with the NIST Randomness Beacon (prototype)
     web API. The functions exported from this module return 512-bit
     full-entropy ByteStrings that you can use for whatever you want.
     New values are computed every 60 seconds on the NIST server.

     For more information, see the project homepage:

         https://beacon.nist.gov/home
-}

module Net.Beacon
       ( getLastRecord
       , getCurrentRecord
       , getPreviousRecord
       , getNextRecord
       , getStartChainRecord
       ) where

import Control.Monad
import Data.Time.Clock.POSIX
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Network.HTTP (getResponseBody, simpleHTTP, getRequest)

import qualified Data.ByteString.Lazy.Char8 as B

timestamp :: IO Int
timestamp = fmap round getPOSIXTime

getXmlData :: String -> IO B.ByteString
getXmlData url = do
  body <- getResponseBody <=< simpleHTTP $ getRequest url
  return $ B.pack body

getOutputValue :: B.ByteString -> Maybe String
getOutputValue stuff = do
  x <- parseXMLDoc stuff
  c <- findChild (QName "outputValue" Nothing Nothing) x
  return $ strContent c

getLastRecord :: IO (Maybe B.ByteString)
getLastRecord = do
  x <- getXmlData "http://beacon.nist.gov/rest/record/last"
  return $ B.pack <$> getOutputValue x

getCurrentRecord :: IO (Maybe B.ByteString)
getCurrentRecord = do
  ts <- timestamp
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  return $ B.pack <$> getOutputValue x

getPreviousRecord :: IO (Maybe B.ByteString)
getPreviousRecord = do
  ts <- timestamp
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/previous/" ++ (show ts)
  return $ B.pack <$> getOutputValue x

getNextRecord :: IO (Maybe B.ByteString)
getNextRecord = do
  ts <- timestamp
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/next/" ++ (show ts)
  return $ B.pack <$> getOutputValue x

getStartChainRecord :: IO (Maybe B.ByteString)
getStartChainRecord = do
  ts <- timestamp
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/start-chain/" ++ (show ts)
  return $ B.pack <$> getOutputValue x
