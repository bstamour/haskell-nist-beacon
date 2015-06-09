module Net.Beacon where

import Control.Monad
import Data.Time.Clock.POSIX
import Network.HTTP (getResponseBody, simpleHTTP, getRequest)

timestamp :: IO Int
timestamp = fmap round getPOSIXTime

getXmlData :: String -> IO String
getXmlData url = do
  body <- getResponseBody <=< simpleHTTP $ getRequest url
  return body

{- For a better description, see
   https://beacon.nist.gov/home

   Current (or next closest) record
   https://beacon.nist.gov/rest/record/<unix time stamp>

   Previous record
   https://beacon.nist.gov/rest/record/previous/<unix time stamp>

   Next record
   https://beacon.nist.gov/rest/record/next/<unix time stamp>

   Last record
   https://beacon.nist.gov/rest/record/last

   Start chain record
   https://beacon.nist.gov/rest/record/start-chain/<unix time stamp> -}

getLastRecord :: IO String
getLastRecord = getXmlData "http://beacon.nist.gov/rest/record/last"

getCurrentRecord :: IO String
getCurrentRecord = do
  ts <- timestamp
  getXmlData $ "http://beacon.nist.gov/rest/record/" ++ (show ts)

getPreviousRecord :: IO String
getPreviousRecord = do
  ts <- timestamp
  getXmlData $ "http://beacon.nist.gov/rest/record/previous/" ++ (show ts)

getNextRecord :: IO String
getNextRecord = do
  ts <- timestamp
  getXmlData $ "http://beacon.nist.gov/rest/record/next/" ++ (show ts)

getStartChainRecord :: IO String
getStartChainRecord = do
  ts <- timestamp
  getXmlData $ "http://beacon.nist.gov/rest/record/start-chain/" ++ (show ts)
