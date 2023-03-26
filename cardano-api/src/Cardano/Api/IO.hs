{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.IO
  ( File(..)
  , HasFileMode(..)
  , Directory(..)
  , fileMap
  , FileDirection(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Kind (Type)
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
newtype File (direction :: FileDirection) = File { unFile :: FilePath }
  deriving (Eq, Ord)
  deriving newtype (Show, IsString, FromJSON, ToJSON)

-- | A directory path.
newtype Directory = Directory { unDirectory :: FilePath }
  deriving (Eq, Ord)
  deriving newtype (Show, IsString, FromJSON, ToJSON)

fileMap :: (FilePath -> FilePath) -> File direction -> File direction
fileMap f = File . f . unFile

class HasFileMode (f :: FileDirection -> Type) where
  usingIn :: f 'InOut -> f 'In
  usingOut :: f 'InOut -> f 'Out

instance HasFileMode File where
  usingIn :: File 'InOut -> File 'In
  usingIn = File . unFile

  usingOut :: File 'InOut -> File 'Out
  usingOut = File . unFile
