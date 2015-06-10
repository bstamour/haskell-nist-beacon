{-|
Module      : Beacon
Description : Module for interacting with the NIST Randomness Beacon.
Copyright   : (c) Bryan St. Amour, 2015
License     : BSD
Maintainer  : bryan@bryanstamour.com
Stability   : experimental
Portability : POSIX

This moduleis for interacting with the NIST Randomness Beacon (prototype)
web API. The Randomness Beacon is designed to be a public source of random
information. It generates and publishes 512-bit blocks of random data collected
from a quantum mechanical process every 60 seconds.

For more information about the project, see

    https://beacon.nist.gov/home
-}


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


-- | A single record: the random data plus some additional information.
data Record =
  Record
  { -- | The time interval, in seconds, between expected records.
    interval :: Int

    -- | The time the seed value was generated as the number of seconds since
    --   January 1, 1970.
  , timeStamp :: Int

    -- | A seed value represented as a 64 byte (512-bit) hex string value.
  , seedValue :: B.ByteString

    -- | The SHA-512 hash value for the previous record - 64 byte hex string.
  , previousOutputValue :: B.ByteString

    -- | A digital signature (RSA) computed over (in order):
    --   version, frequency, timeStamp, seedValue, previousHashValue,
    --   errorCode.
    --
    --   Note: Except for version, the hash is on the byte representations
    --   and not the string representations of the data values.
  , signatureValue :: B.ByteString

    -- | The SHA-512 hash of the signatureValue as a 64 byte hex string.
  , outputValue :: B.ByteString

    -- | The status code value:
    --     0 - Chain intact, values all good
    --     1 - Start of a new chain of values, previous hash value will be all
    --         zeroes
    --     2 - Time between values is greater than the frequency, but the
    --         chain is still intact
  , statusCode :: Int
  } deriving (Show, Eq)


type Timestamp = Int


-- | Last record published.
getLastRecord :: IO (Maybe Record)
getLastRecord = do
  x <- getXmlData "http://beacon.nist.gov/rest/record/last"
  return $ getRecord x


-- | Current record, or closest to the timestamp.
getCurrentRecord :: Timestamp -> IO (Maybe Record)
getCurrentRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  return $ getRecord x


-- | Previous record.
getPreviousRecord :: Timestamp -> IO (Maybe Record)
getPreviousRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/previous/" ++ (show ts)
  return $ getRecord x


-- | Next record.
getNextRecord :: Timestamp -> IO (Maybe Record)
getNextRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/next/" ++ (show ts)
  return $ getRecord x


-- | Start chain record.
getStartChainRecord :: Timestamp -> IO (Maybe Record)
getStartChainRecord ts = do
  x <- getXmlData $ "http://beacon.nist.gov/rest/record/start-chain/" ++ (show ts)
  return $ getRecord x


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
