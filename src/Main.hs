-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the entry point for the "parsestar-aeson" executable.
-----------------------------------------------------------------------------

module Main where

import           Data.Aeson (ToJSON(toJSON), Value(Null))
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.HashMap.Strict
import           Data.STAR (STAR(..), STARBlock(..), STAREntry(..))
import qualified Data.STAR
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified System.Exit

nullify :: Data.ByteString.ByteString -> Value
nullify str
  | str `elem` map (Data.Text.Encoding.encodeUtf8 . Data.Text.pack) ["?", "."] = Null
  | otherwise = toJSON (Data.Text.Encoding.decodeUtf8 str)

instance ToJSON STAR where
  toJSON (STAR blocks) =
    toJSON blocks

instance ToJSON STARBlock where
  toJSON (Global entries) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Global"))
          , (Data.Text.pack "entries", toJSON entries)
          ]
  toJSON (Data dataKey entries) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Data"))
          , (Data.Text.pack "dataKey", toJSON (Data.Text.Encoding.decodeUtf8 dataKey))
          , (Data.Text.pack "entries", toJSON entries)
          ]

instance ToJSON STAREntry where
  toJSON (Entry key value) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Entry"))
          , (Data.Text.pack "key", toJSON (Data.Text.Encoding.decodeUtf8 key))
          , (Data.Text.pack "value", nullify value)
          ]
  toJSON (Ref key value) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Ref"))
          , (Data.Text.pack "key", toJSON (Data.Text.Encoding.decodeUtf8 key))
          , (Data.Text.pack "value", nullify value)
          ]
  toJSON (Frame key frameEntries) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Frame"))
          , (Data.Text.pack "key", toJSON (Data.Text.Encoding.decodeUtf8 key))
          , (Data.Text.pack "frameEntries", toJSON frameEntries)
          ]
  toJSON (Loop table) =
    toJSON (Data.HashMap.Strict.fromList xs)
      where
        xs =
          [ (Data.Text.pack "$type", toJSON (Data.Text.pack "Loop"))
          , (Data.Text.pack "table", toJSON table)
          ]

main :: IO ()
main =
  fmap (fmap (Data.Aeson.encode . STAR) . Data.STAR.parseSTAR) Data.ByteString.getContents
    >>=
  either (\_ -> System.Exit.exitFailure) (\str -> Data.ByteString.Lazy.putStr str >> System.Exit.exitSuccess)
