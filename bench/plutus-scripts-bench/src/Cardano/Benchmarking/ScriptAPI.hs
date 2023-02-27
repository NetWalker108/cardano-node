{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.ScriptAPI
  ( BenchScript, psName, psScript, mkBenchScript
  )
  where

import           Prelude as Haskell (String, (.), (<$>))
import           Cardano.Api (ScriptInAnyLang, toScriptInAnyLang)
import           Cardano.Api.Shelley (PlutusScript (..))
import qualified Data.ByteString.Short as SBS
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.), (<$>))


data BenchScript
  = BenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

mkBenchScript :: String -> ScriptInAnyLang -> BenchScript
mkBenchScript name body = BenchScript name body
