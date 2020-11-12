{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.KESKeys
  ( golden_shelleyKESKeys
  ) where

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import           Text.Regex.TDFA ((=~))

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyKESKeys :: Property
golden_shelleyKESKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "kes-verification-key-file"
  signKeyFile <- noteTempFile tempDir "kes-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "node","key-gen-KES"
    , "--verification-key-file", verKeyFile
    , "--signing-key-file", signKeyFile
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "kes_vk[a-z0-9]{59}"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "kes_sk[a-z0-9]{980}"
