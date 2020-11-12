{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys
  ( golden_shelleyVRFKeys
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
golden_shelleyVRFKeys :: Property
golden_shelleyVRFKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "vrf-verification-key-file"
  signKeyFile <- noteTempFile tempDir "vrf-signing-key-file"

  -- Generate vrf verification key
  void $ execCardanoCLI
    [ "node","key-gen-VRF"
    , "--verification-key-file", verKeyFile
    , "--signing-key-file", signKeyFile
    ]

  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "vrf_vk[a-z0-9]{59}"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "vrf_sk[a-z0-9]{110}"
