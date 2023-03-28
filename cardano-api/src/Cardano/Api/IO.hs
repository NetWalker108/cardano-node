{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.IO
  ( File(..)
  , toFileIn
  , toFileOut
  , Directory(..)
  , fileMap
  , FileDirection(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Coerce (coerce)
import           Data.String (IsString)

data FileDirection
  = In
  -- ^ Indicate the file is to be used for reading.
  | Out
  -- ^ Indicate the file is to be used for writing.
  | InOut
  -- ^ Indicate the file is to be used for both reading and writing.

-- | A file path with additional type information to indicate whether it is to
-- be used for reading or writing.
newtype File (direction :: FileDirection) = File
  { unFile :: FilePath
  } deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | A directory path.
newtype Directory = Directory
  { unDirectory :: FilePath
  } deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

fileMap :: (FilePath -> FilePath) -> File direction -> File direction
fileMap f = File . f . unFile

toFileIn :: File 'InOut -> File 'In
toFileIn = coerce

toFileOut :: File 'InOut -> File 'Out
toFileOut = coerce
