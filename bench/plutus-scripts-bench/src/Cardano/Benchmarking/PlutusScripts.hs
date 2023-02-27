{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.PlutusScripts
    ( encodePlutusScript
    , findPlutusScript
    , getAllScripts
    , listPlutusScripts
    ) where

import           Prelude

import           Data.ByteString.Lazy as LBS (ByteString)

import           Cardano.Api

import qualified Cardano.Benchmarking.PlutusScripts.CustomCall as CustomCall
import qualified Cardano.Benchmarking.PlutusScripts.EcdsaSecp256k1Loop as ECDSA
import qualified Cardano.Benchmarking.PlutusScripts.Loop as Loop
import qualified Cardano.Benchmarking.PlutusScripts.SchnorrSecp256k1Loop as Schnorr


getAllScripts ::
     [(String, (String, ScriptInAnyLang))]
getAllScripts =
  [ let s = nmn CustomCall.scriptName in (s, (s, asAnyLang CustomCall.scriptSerialized))
  , let s = nmn ECDSA.scriptName in (s, (s, asAnyLang ECDSA.scriptSerialized))
  , let s = nmn Loop.scriptName in (s, (s, asAnyLang Loop.scriptSerialized))
  , let s = nmn Schnorr.scriptName in (s, (s, asAnyLang Schnorr.scriptSerialized))
  ]
    where nmn = normalizeModuleName

listPlutusScripts ::
     [String]
listPlutusScripts
  = fst <$> getAllScripts

findPlutusScript ::
     String
  -> Maybe ScriptInAnyLang
findPlutusScript
  = fmap snd . (`lookup` getAllScripts)

encodePlutusScript ::
     ScriptInAnyLang
  -> LBS.ByteString
encodePlutusScript
  = \case
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) s -> textEnvelopeToJSON Nothing s
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) s -> textEnvelopeToJSON Nothing s
    _                                                       -> "{}"


asAnyLang :: forall lang. IsPlutusScriptLanguage lang =>
     PlutusScript lang
  -> ScriptInAnyLang
asAnyLang script
  = toScriptInAnyLang $ PlutusScript (plutusScriptVersion @lang) script

-- "A.B.C" --> "C.hs"
normalizeModuleName ::
     String
  -> String
normalizeModuleName
  = (++ ".hs") . reverse . takeWhile (/= '.') . reverse
