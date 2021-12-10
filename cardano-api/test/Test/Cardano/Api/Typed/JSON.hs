{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Data.Aeson (eitherDecode, encode)
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Gen.Cardano.Api.Typed (genMaybePraosNonce, genProtocolParameters)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import           Cardano.Api
import           Data.String (IsString (..))

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

test_roundtrip_protocol_parameters_JSON :: [TestTree]
test_roundtrip_protocol_parameters_JSON =
  [ testPropertyNamed (show era) (fromString (show era)) $
    H.property $ do
      pp <- forAll $ genProtocolParameters era
      tripping pp encode eitherDecode
  | AnyCardanoEra era <- [minBound..]
  ]

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.JSON"
  [ testPropertyNamed "roundtrip praos nonce JSON"         "roundtrip praos nonce JSON"         prop_roundtrip_praos_nonce_JSON
  , testGroup "roundtrip protocol parameters JSON"                                              test_roundtrip_protocol_parameters_JSON
  ]
