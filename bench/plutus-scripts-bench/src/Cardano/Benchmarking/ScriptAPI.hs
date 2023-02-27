{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.ScriptAPI
  ( BenchScript, psName, psScript
  )
  -- ( scriptName
  -- , scriptSerialized
  -- , script
  -- )
  where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude as Haskell (String, (.), (<$>))

import           Cardano.Api (ScriptInAnyLang)
import           Cardano.Api.Shelley (PlutusScript (..))
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.), (<$>))


data BenchScript
  = BenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

mkBenchScriptV1 :: String -> PlutusScript PlutusScriptV1 -> BenchScript
mkBenchScriptV1 name body = BenchScript name 
