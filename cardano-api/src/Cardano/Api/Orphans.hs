{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Control.State.Transition (STS (State))
import           Data.Aeson (ToJSON (..), Value, object, pairs, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.BiMap (BiMap (..), Bimap)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as Short
import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Tuple (swap)
import           Data.UMap (Trip (Triple), UMap (UnifiedMap))
import           Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import           Data.Word (Word64)
import           GHC.Word (Word8)
import           Numeric.Natural (Natural)

import qualified Cardano.Api.Address as Api
import qualified Cardano.Api.Certificate as Api
import           Cardano.Api.OperationalCertificate (KESPeriod (..))
import           Cardano.Api.Script
import qualified Cardano.Api.Script as Script
import qualified Cardano.Api.SerialiseRaw as Api
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Api.TxBody as Api
import           Cardano.Api.Utils (textShow)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Crypto.VRF (VerKeyVRF)
import           Cardano.Ledger.Address (Addr, RewardAcnt)
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure,
                   AlonzoUtxowPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import           Cardano.Ledger.Babbage.PParams (BabbagePParams, BabbagePParamsUpdate)
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import           Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import           Cardano.Ledger.BaseTypes (Network, ProtVer, StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Chain (ChainPredicateFailure (..))
import           Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Coin as Shelley
import           Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Conway as Conway
import           Cardano.Ledger.Core (EraTxOut)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import           Cardano.Ledger.Shelley.API (ApplyTxError (..), BlockTransitionError (..),
                   MIRPot (..), ShelleyTxOut (..))
import qualified Cardano.Ledger.Shelley.API as Shelley
import           Cardano.Ledger.Shelley.API.Types (ShelleyUTXOW)
import qualified Cardano.Ledger.Shelley.EpochBoundary as ShelleyEpoch
import qualified Cardano.Ledger.Shelley.LedgerState as ShelleyLedger
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import           Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import qualified Cardano.Ledger.Shelley.RewardUpdate as Shelley
import           Cardano.Ledger.Shelley.Rules.Bbody (ShelleyBbodyPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Delegs (ShelleyDelegsPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDelplPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEpochPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Ledger
import           Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLedgersPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Mir (ShelleyMirPredFailure)
import           Cardano.Ledger.Shelley.Rules.NewEpoch (ShelleyNewEpochPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Newpp (ShelleyNewppPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.PoolReap (ShelleyPoolreapPredFailure)
import           Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPpupPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Rupd (ShelleyRupdPredFailure)
import           Cardano.Ledger.Shelley.Rules.Snap (ShelleySnapPredFailure)
import           Cardano.Ledger.Shelley.Rules.Tick (ShelleyTickPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Upec (ShelleyUpecPredFailure (..))
import           Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUTXO)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import qualified Cardano.Ledger.ShelleyMA.Rules as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import           Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.TxIn as Ledger.TxIn
import           Cardano.Ledger.UnifiedMap (UnifiedMap)
import           Cardano.Ledger.Val (Val)
import           Cardano.Protocol.TPraos.API (ChainTransitionError (..))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock (..))
import qualified Cardano.Protocol.TPraos.BHeader as Protocol
import           Cardano.Protocol.TPraos.Rules.OCert (OcertPredicateFailure (..))
import           Cardano.Protocol.TPraos.Rules.Overlay (OverlayPredicateFailure (..))
import           Cardano.Protocol.TPraos.Rules.Prtcl (PrtclPredicateFailure (..),
                   PrtlSeqFailure (..))
import           Cardano.Protocol.TPraos.Rules.Tickn (TicknPredicateFailure)
import           Cardano.Protocol.TPraos.Rules.Updn (UpdnPredicateFailure)
import           Cardano.Slotting.Slot (WithOrigin, withOriginToMaybe)

import           Ouroboros.Consensus.Block (EpochNo, Header)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyCompatible,
                   ShelleyLedgerError (..))
import qualified Ouroboros.Consensus.Shelley.Ledger as Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect (ProtocolUpdate (..),
                   ShelleyLedgerUpdate (..), UpdateProposal (..), UpdateState (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ShelleyHash)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Network.Block

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

deriving newtype instance Crypto.Crypto crypto => ToJSON (Ledger.AuxiliaryDataHash crypto)


instance ToJSON (MaryValue era) where
  toJSON = object . toMaryValuePairs
  toEncoding = Aeson.pairs . mconcat . toMaryValuePairs

toMaryValuePairs :: Aeson.KeyValue a => MaryValue crypto -> [a]
toMaryValuePairs (MaryValue !l !ps) =
  [ "lovelace" .= l
  , "policies" .= ps
  ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON = object . toAccountStatePairs
  toEncoding = Aeson.pairs . mconcat . toAccountStatePairs

toAccountStatePairs :: Aeson.KeyValue a => ShelleyLedger.AccountState -> [a]
toAccountStatePairs (Shelley.AccountState !tr !rs) =
  [ "treasury" .= tr
  , "reserves" .= rs
  ]

instance forall era.
         ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = Aeson.pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsUpdate era)
  , ToJSON (Core.PParams era)
  , Aeson.KeyValue a
  )
  => ShelleyLedger.EpochState era
  -> [a]
toEpochStatePairs eState =
  let !esAccountState = Shelley.esAccountState eState
      !esSnapshots = Shelley.esSnapshots eState
      !esLState = Shelley.esLState eState
      !esPrevPp = Shelley.esPrevPp eState
      !esPp = Shelley.esPp eState
      !esNonMyopic = Shelley.esNonMyopic eState
  in  [ "esAccountState" .= esAccountState
      , "esSnapshots" .= esSnapshots
      , "esLState" .= esLState
      , "esPrevPp" .= esPrevPp
      , "esPp" .= esPp
      , "esNonMyopic" .= esNonMyopic
      ]


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = Aeson.pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsUpdate era)
  , Aeson.KeyValue a
  ) => ShelleyLedger.LedgerState era -> [a]
toLedgerStatePairs lState =
  let !lsUTxOState = Shelley.lsUTxOState lState
      !lsDPState = Shelley.lsDPState lState
  in  [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsDPState
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.IncrementalStake crypto) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = Aeson.pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.IncrementalStake crypto -> [a]
toIncrementalStakePairs iStake =
  let !credentials = Map.toList (ShelleyLedger.credMap iStake)
      !pointers = Map.toList (ShelleyLedger.ptrMap iStake)
  in  [ "credentials" .= credentials
      , "pointers" .= pointers
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsUpdate era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON = object . toUtxoStatePairs
  toEncoding = Aeson.pairs . mconcat . toUtxoStatePairs

toUtxoStatePairs ::
  ( Aeson.KeyValue a
  , Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Control.State.Transition.State (Core.EraRule "PPUP" era))
  ) => ShelleyLedger.UTxOState era -> [a]
toUtxoStatePairs utxoState =
  let !utxo = Shelley._utxo utxoState
      !deposited = Shelley._deposited utxoState
      !fees = Shelley._fees utxoState
      !ppups = Shelley._ppups utxoState
      !stakeDistro = Shelley._stakeDistro utxoState
  in  [ "utxo" .= utxo
      , "deposited" .= deposited
      , "fees" .= fees
      , "ppups" .= ppups
      , "stake" .= stakeDistro
      ]

instance ( ToJSON (Core.PParamsUpdate era)
         , Core.Era era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON = object . toPpupStatePairs
  toEncoding = Aeson.pairs . mconcat . toPpupStatePairs

toPpupStatePairs ::
  ( Aeson.KeyValue a
  , ToJSON (Core.PParamsUpdate era)
  , Core.Era era
  ) => ShelleyLedger.PPUPState era -> [a]
toPpupStatePairs ppUpState =
  let !proposals = Shelley.proposals ppUpState
      !futureProposals = Shelley.futureProposals ppUpState
  in  [ "proposals" .= proposals
      , "futureProposals" .= futureProposals
      ]

instance ( ToJSON (Core.PParamsUpdate era)
         , Core.Era era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates
  toEncoding (Shelley.ProposedPPUpdates ppUpdates) = toEncoding $ Map.toList ppUpdates

instance ToJSON (ShelleyPParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Shelley._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Shelley._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Shelley._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Shelley._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Shelley._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Shelley._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Shelley._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Shelley._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Shelley._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Shelley._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Shelley._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Shelley._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Shelley._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Shelley._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Shelley._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Shelley._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Shelley._minPoolCost pp) ]

instance ToJSON (BabbagePParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Babbage._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Babbage._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Babbage._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Babbage._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Babbage._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Babbage._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Babbage._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Babbage._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Babbage._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Babbage._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Babbage._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Babbage._tau pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Babbage._protocolVersion pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Babbage._minPoolCost pp) ]
     ++ [ "coinsPerUTxOByte"      .= x | x <- mbfield (Babbage._coinsPerUTxOByte pp) ]
     ++ [ "costmdls"              .= x | x <- mbfield (Babbage._costmdls pp) ]
     ++ [ "prices"                .= x | x <- mbfield (Babbage._prices pp) ]
     ++ [ "maxTxExUnits"          .= x | x <- mbfield (Babbage._maxTxExUnits pp) ]
     ++ [ "maxBlockExUnits"       .= x | x <- mbfield (Babbage._maxBlockExUnits pp) ]
     ++ [ "maxValSize"            .= x | x <- mbfield (Babbage._maxValSize pp) ]
     ++ [ "collateralPercentage"  .= x | x <- mbfield (Babbage._collateralPercentage pp) ]
     ++ [ "maxCollateralInputs"   .= x | x <- mbfield (Babbage._maxCollateralInputs pp) ]

instance ToJSON (BabbagePParams (era Consensus.StandardCrypto)) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= Babbage._minfeeA pp
      , "minFeeB" .= Babbage._minfeeB pp
      , "maxBlockBodySize" .= Babbage._maxBBSize pp
      , "maxTxSize" .= Babbage._maxTxSize pp
      , "maxBlockHeaderSize" .= Babbage._maxBHSize pp
      , "keyDeposit" .= Babbage._keyDeposit pp
      , "poolDeposit" .= Babbage._poolDeposit pp
      , "eMax" .= Babbage._eMax pp
      , "nOpt" .= Babbage._nOpt pp
      , "a0" .= Babbage._a0 pp
      , "rho" .= Babbage._rho pp
      , "tau" .= Babbage._tau pp
      , "protocolVersion" .= Babbage._protocolVersion pp
      , "minPoolCost" .= Babbage._minPoolCost pp
      , "coinsPerUTxOByte" .= Babbage._coinsPerUTxOByte pp
      , "costmdls" .= Babbage._costmdls pp
      , "prices" .= Babbage._prices pp
      , "maxTxExUnits" .= Babbage._maxTxExUnits pp
      , "maxBlockExUnits" .= Babbage._maxBlockExUnits pp
      , "maxValSize" .= Babbage._maxValSize pp
      , "collateralPercentage" .= Babbage._collateralPercentage pp
      , "maxCollateralInputs" .= Babbage._maxCollateralInputs pp
      ]

mbfield :: StrictMaybe a -> [a]
mbfield SNothing  = []
mbfield (SJust x) = [x]

instance ( Ledger.Era era
         , ToJSON (Core.Value era)
         , ToJSON (Babbage.Datum era)
         , ToJSON (Core.Script era)
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         , Val (Core.Value era)
         ) => ToJSON (BabbageTxOut era) where
  toJSON = object . toBabbageTxOutPairs
  toEncoding = Aeson.pairs . mconcat . toBabbageTxOutPairs

toBabbageTxOutPairs ::
  ( Aeson.KeyValue a
  , Ledger.Era era
  , ToJSON (Core.Value era)
  , ToJSON (Core.Script era)
  , Ledger.Crypto era ~ Consensus.StandardCrypto
  , Val (Core.Value era)
  ) => BabbageTxOut era -> [a]
toBabbageTxOutPairs (BabbageTxOut !addr !val !dat !mRefScript) =
  [ "address" .= addr
  , "value" .= val
  , "datum" .= dat
  , "referenceScript" .= mRefScript
  ]

instance ( Ledger.Era era
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         ) => ToJSON (Babbage.Datum era) where
  toJSON d =
    case Alonzo.datumDataHash d of
      SNothing -> Aeson.Null
      SJust dH -> toJSON $ ScriptDataHash dH
  toEncoding d =
    case Alonzo.datumDataHash d of
      SNothing -> toEncoding Aeson.Null
      SJust dH -> toEncoding $ ScriptDataHash dH

instance ToJSON (Alonzo.AlonzoScript (Babbage.BabbageEra Consensus.StandardCrypto)) where
  toJSON = Aeson.String . Text.decodeUtf8 . B16.encode . CBOR.serialize'

instance ToJSON (Alonzo.AlonzoScript (Conway.ConwayEra Consensus.StandardCrypto)) where
  toJSON = Aeson.String . Text.decodeUtf8 . B16.encode . CBOR.serialize'

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON = object . toDpStatePairs
  toEncoding = Aeson.pairs . mconcat . toDpStatePairs

toDpStatePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DPState crypto -> [a]
toDpStatePairs dpState =
  let !dstate = Shelley.dpsDState dpState
      !pstate = Shelley.dpsPState dpState
  in  [ "dstate" .= dstate
      , "pstate" .= pstate
      ]

instance (ToJSON coin, ToJSON ptr, ToJSON pool) => ToJSON (Trip coin ptr pool) where
  toJSON = object . toTripPair
  toEncoding = Aeson.pairs . mconcat . toTripPair

toTripPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  ) => Trip coin ptr pool -> [a]
toTripPair (Triple !coin !ptr !pool) =
  [ "coin" .= coin
  , "ptr" .= ptr
  , "pool" .= pool
  ]

instance Crypto.Crypto crypto => ToJSON (UnifiedMap crypto) where
  toJSON = object . toUnifiedMapPair
  toEncoding = Aeson.pairs . mconcat . toUnifiedMapPair

toUnifiedMapPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  , ToJSON cred
  , ToJSONKey cred
  , ToJSONKey ptr
  ) => UMap coin cred pool ptr -> [a]
toUnifiedMapPair (UnifiedMap !m1 !m2) =
  [ "credentials" .= m1
  , "pointers" .= m2
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON = object . toDStatePair
  toEncoding = Aeson.pairs . mconcat . toDStatePair

toDStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DState crypto -> [a]
toDStatePair dState =
  let !unifiedRewards = Shelley._unified dState
      !fGenDelegs = Map.toList (Shelley._fGenDelegs dState)
      !genDelegs = Shelley._genDelegs dState
      !irwd = Shelley._irwd dState
  in  [ "unifiedRewards" .= unifiedRewards
      , "fGenDelegs" .= fGenDelegs
      , "genDelegs" .= genDelegs
      , "irwd" .= irwd
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs
  toEncoding (Shelley.GenDelegs delegs) = toEncoding delegs

instance Crypto.Crypto crypto => ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = Aeson.pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.InstantaneousRewards crypto -> [a]
toInstantaneousRewardsPair iRwds =
  let !iRReserves = Shelley.iRReserves iRwds
      !iRTreasury = Shelley.iRTreasury iRwds
  in  [ "iRReserves" .= iRReserves
      , "iRTreasury" .= iRTreasury
      ]

instance
  Crypto.Crypto crypto =>
  ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto))
  where
  toJSON = object . toPtrCredentialStakingPair
  toEncoding = Aeson.pairs . mconcat . toPtrCredentialStakingPair

toPtrCredentialStakingPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto) -> [a]
toPtrCredentialStakingPair (MkBiMap ptsStakeM stakePtrSetM) =
  let !stakedCreds = Map.toList ptsStakeM
      !credPtrR = stakePtrSetM
  in  [ "stakedCreds" .= stakedCreds
      , "credPtrR" .= credPtrR
      ]

deriving newtype instance ToJSON Shelley.CertIx
deriving newtype instance ToJSON Shelley.TxIx

instance ToJSON Shelley.Ptr where
  toJSON = object . toPtrPair
  toEncoding = Aeson.pairs . mconcat . toPtrPair

instance ToJSONKey Shelley.Ptr

toPtrPair :: Aeson.KeyValue a => Shelley.Ptr -> [a]
toPtrPair (Shelley.Ptr !slotNo !txIndex !certIndex) =
  [ "slot" .= unSlotNo slotNo
  , "txIndex" .= txIndex
  , "certIndex" .= certIndex
  ]


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON = object . toPStatePair
  toEncoding = Aeson.pairs . mconcat . toPStatePair

toPStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.PState crypto -> [a]
toPStatePair pState =
  let !pParams = Shelley._pParams pState
      !fPParams = Shelley._fPParams pState
      !retiring = Shelley._retiring pState
  in  [ "pParams pState" .= pParams
      , "fPParams pState" .= fPParams
      , "retiring pState" .= retiring
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo
  toEncoding (Shelley.UTxO utxo) = toEncoding utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (ShelleyTxOut era) where
  toJSON = object . toTxOutPair
  toEncoding = Aeson.pairs . mconcat . toTxOutPair

toTxOutPair ::
  ( Aeson.KeyValue a
  , ToJSON (Core.Value era)
  , EraTxOut era)
  => ShelleyTxOut era -> [a]
toTxOutPair (ShelleyTxOut !addr !amount) =
  [ "address" .= addr
  , "amount" .= amount
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText
  toEncoding = toEncoding . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Shelley.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Shelley.TxIn crypto -> Text
txInToText (Shelley.TxIn (Shelley.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Shelley.NonMyopic crypto) where
  toJSON = object . toNonMyopicPair
  toEncoding = Aeson.pairs . mconcat . toNonMyopicPair

toNonMyopicPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.NonMyopic crypto -> [a]
toNonMyopicPair nonMy =
  let !likelihoodsNM = Shelley.likelihoodsNM nonMy
      !rewardPotNM = Shelley.rewardPotNM nonMy
  in  [ "likelihoodsNM" .= likelihoodsNM
      , "rewardPotNM" .= rewardPotNM
      ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd
  toEncoding (Shelley.Likelihood llhd) =
    toEncoding $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON = object . toSnapShotsPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotsPair

toSnapShotsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShots crypto -> [a]
toSnapShotsPair ss =
  let !pstakeMark = Shelley._pstakeMark ss
      !pstakeSet = Shelley._pstakeSet ss
      !pstakeGo = Shelley._pstakeGo ss
      !feeSS = Shelley._feeSS ss
  in  [ "pstakeMark" .= pstakeMark
      , "pstakeSet" .= pstakeSet
      , "pstakeGo" .= pstakeGo
      , "feeSS" .= feeSS
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON = object . toSnapShotPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotPair

toSnapShotPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShot crypto -> [a]
toSnapShotPair ss =
  let !stake = Shelley._stake ss
      !delegations = ShelleyEpoch._delegations ss
      !poolParams = Shelley._poolParams ss
  in  [ "stake" .= stake
      , "delegations" .= delegations
      , "poolParams" .= poolParams
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s
  toEncoding (Shelley.Stake s) = toEncoding s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON = object . toRewardUpdatePair
  toEncoding = Aeson.pairs . mconcat . toRewardUpdatePair

toRewardUpdatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.RewardUpdate crypto -> [a]
toRewardUpdatePair rUpdate =
  let !deltaT = Shelley.deltaT rUpdate
      !deltaR = Shelley.deltaR rUpdate
      !rs = Shelley.rs rUpdate
      !deltaF = Shelley.deltaF rUpdate
      !nonMyopic = Shelley.nonMyopic rUpdate
  in  [ "deltaT" .= deltaT
      , "deltaR" .= deltaR
      , "rs" .= rs
      , "deltaF" .= deltaF
      , "nonMyopic" .= nonMyopic
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON  = \case
    Shelley.Pulsing _ _ -> Aeson.Null
    Shelley.Complete ru -> toJSON ru
  toEncoding  = \case
    Shelley.Pulsing _ _ -> toEncoding Aeson.Null
    Shelley.Complete ru -> toEncoding ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i
  toEncoding (Shelley.DeltaCoin i) = toEncoding i

instance Crypto.Crypto crypto => ToJSON (Ledger.PoolDistr crypto) where
  toJSON (Ledger.PoolDistr m) = toJSON m
  toEncoding (Ledger.PoolDistr m) = toEncoding m

instance Crypto.Crypto crypto => ToJSON (Ledger.IndividualPoolStake crypto) where
  toJSON = object . toIndividualPoolStakePair
  toEncoding = Aeson.pairs . mconcat . toIndividualPoolStakePair

toIndividualPoolStakePair ::
  ( Aeson.KeyValue a
  , Crypto.HashAlgorithm (Crypto.HASH crypto)
  ) => Ledger.IndividualPoolStake crypto -> [a]
toIndividualPoolStakePair indivPoolStake =
  let !individualPoolStake = Ledger.individualPoolStake indivPoolStake
      !individualPoolStakeVrf = Ledger.individualPoolStakeVrf indivPoolStake
  in  [ "individualPoolStake" .= individualPoolStake
      , "individualPoolStakeVrf" .= individualPoolStakeVrf
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Reward crypto) where
  toJSON = object . toRewardPair
  toEncoding = Aeson.pairs . mconcat . toRewardPair

toRewardPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.Reward crypto -> [a]
toRewardPair reward =
  let !rewardType = Shelley.rewardType reward
      !rewardPool = Shelley.rewardPool reward
      !rewardAmount = Shelley.rewardAmount reward
  in  [ "rewardType" .= rewardType
      , "rewardPool" .= rewardPool
      , "rewardAmount" .= rewardAmount
      ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance Crypto.Crypto c => ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash
  toEncoding = toEncoding . SafeHash.extractHash

-----

-- deriving newtype instance ToJSON SystemStart
-- deriving newtype instance FromJSON SystemStart


instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.Credential 'Shelley.Staking crypto) (Shelley.KeyHash 'Shelley.StakePool crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.KeyHash    'Shelley.StakePool crypto) (Shelley.PoolParams crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VP (Shelley.Credential 'Shelley.Staking   crypto) (Shelley.CompactForm Shelley.Coin)) where
  toJSON = toJSON . fmap fromCompact . VMap.toMap
  toEncoding = toEncoding . fmap fromCompact . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (Consensus.StakeSnapshots crypto) where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair :: (Aeson.KeyValue a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
stakeSnapshotsToPair Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total" .= object
      [ "stakeMark" .= ssMarkTotal
      , "stakeSet" .= ssSetTotal
      , "stakeGo" .= ssGoTotal
      ]
    ]

instance ToJSON (Consensus.StakeSnapshot crypto) where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue a => Consensus.StakeSnapshot crypto -> [a]
stakeSnapshotToPair Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]

instance ToJSON (PredicateFailure (Core.EraRule "LEDGER" era)) => ToJSON (ApplyTxError era) where
  toJSON (ApplyTxError es) = toJSON es

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.Tx era)
  , ToJSON (Ledger.TxIn.TxId (Ledger.Crypto era))
  ) => ToJSON (Ledger.GenTx (Ledger.ShelleyBlock protocol era)) where
  toJSON _tx = object [ "txid" .= toJSON @String "TODO" ]

-- instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock protocol era))) where
--   toJSON = String . Text.take 8 . renderTxId

instance
  ( ShelleyCompatible protocol era
  , Consensus.ShelleyBasedEra era
  , ToJSON (ShelleyHash (Ledger.Crypto era))
  , ToJSON (Protocol.BHeader (Ledger.Crypto era))
  ) => ToJSON (Header (ShelleyBlock protocol era)) where
  toJSON b = object
    [ "kind"      .= toJSON @String "ShelleyBlock"
    , "hash"      .= do condense (blockHash b)  :: String
    , "slotNo"    .= do condense (blockSlot b)  :: String
    , "blockNo"   .= do condense (blockNo b)    :: String
    -- , "delegate"  .= condense (headerSignerVk h)
    ]

instance (Crypto.Crypto crypto, Crypto.VRF crypto ~ crypto) => ToJSON (TPraosCannotForge crypto) where
  toJSON (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) = object
    [ "kind"      .= toJSON @String "TPraosCannotForgeKeyNotUsableYet"
    , "keyStart"  .= do keyStartPeriod  :: KESPeriod
    , "wallClock" .= do wallClockPeriod :: KESPeriod
    ]
  toJSON (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) = object
    [ "kind"      .= toJSON @String "TPraosCannotLeadWrongVRF"
    , "expected"  .= do genDlgVRFHash   :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    , "actual"    .= do coreNodeVRFHash :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    ]

deriving newtype instance ToJSON KESPeriod

instance ToJSON HotKey.KESInfo where
  toJSON HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } = object
    [ "kind"        .= toJSON @String "KESInfo"
    , "startPeriod" .= do kesStartPeriod  :: KESPeriod
    , "endPeriod"   .= do kesEndPeriod    :: KESPeriod
    , "evolution"   .= do kesEvolution    :: Word
    ]

instance ToJSON HotKey.KESEvolutionError where
  toJSON (HotKey.KESCouldNotEvolve kesInfo targetPeriod) = object
    [ "kind"          .= toJSON @String "KESCouldNotEvolve"
    , "kesInfo"       .= do kesInfo       :: HotKey.KESInfo
    , "targetPeriod"  .= do targetPeriod  :: KESPeriod
    ]
  toJSON (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) = object
    [ "kind"          .= toJSON @String "KESKeyAlreadyPoisoned"
    , "kesInfo"       .= do kesInfo       :: HotKey.KESInfo
    , "targetPeriod"  .= do targetPeriod  :: KESPeriod
    ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (PredicateFailure (Shelley.UTxO era))
  -- , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "BBODY" era))
  , ToJSON (BlockTransitionError era)
  ) => ToJSON (ShelleyLedgerError era) where
  toJSON (BBodyError (BlockTransitionError fs)) = object
    [ "kind"      .= toJSON @String "BBodyError"
    , "failures"  .= do map toJSON fs  :: [Value]
    ]

instance
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Ledger.PParamsUpdate era)
  , ToJSON (Ouroboros.Consensus.Shelley.Ledger.Inspect.ProtocolUpdate era)
  ) => ToJSON (ShelleyLedgerUpdate era) where
  toJSON (ShelleyUpdatedProtocolUpdates updates) = object
    [ "kind"    .= toJSON @String "ShelleyUpdatedProtocolUpdates"
    , "updates" .= do map toJSON updates :: [Value]
    ]

instance
  ( Ledger.Era era
  , ToJSON (Ledger.PParamsUpdate era)
  , ToJSON (UpdateProposal era)
  , ToJSON (UpdateState (Ledger.Crypto era))
  ) => ToJSON (ProtocolUpdate era) where
  toJSON ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} = object
    [ "proposal" .= do protocolUpdateProposal :: UpdateProposal era
    , "state"    .= do protocolUpdateState    :: UpdateState (Consensus.EraCrypto era)
    ]

instance ToJSON (Ledger.PParamsUpdate era)
         => ToJSON (UpdateProposal era) where
  toJSON UpdateProposal{proposalParams, proposalVersion, proposalEpoch} = object
    [ "params"  .= do proposalParams  :: Ledger.PParamsUpdate era
    , "version" .= do proposalVersion :: Maybe ProtVer
    , "epoch"   .= do proposalEpoch   :: EpochNo
    ]

instance
  ( Crypto.Crypto crypto
  -- , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  ) => ToJSON (UpdateState crypto) where
  toJSON UpdateState{proposalVotes, proposalReachedQuorum} = object
    [ "proposal"      .= do proposalVotes         :: [KeyHash 'Genesis crypto]
    , "reachedQuorum" .= do proposalReachedQuorum :: Bool
    ]

instance
  ( Crypto.Crypto crypto
  -- , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  -- , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (PrtclPredicateFailure crypto)
  , ToJSON L.ActiveSlotCoeff
  ) => ToJSON (ChainTransitionError crypto) where
  toJSON (ChainTransitionError fs) = object
    [ "kind"      .= toJSON @String "ChainTransitionError"
    , "failures"  .= do map toJSON fs :: [Value]
    ]

instance ToJSON ChainPredicateFailure where
  toJSON (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) = object
    [ "kind"          .= toJSON @String "HeaderSizeTooLarge"
    , "headerSize"    .= do hdrSz     :: Natural
    , "maxHeaderSize" .= do maxHdrSz  :: Natural
    ]
  toJSON (BlockSizeTooLargeCHAIN blkSz maxBlkSz) = object
    [ "kind"          .= toJSON @String "BlockSizeTooLarge"
    , "blockSize"     .= do blkSz     :: Natural
    , "maxBlockSize"  .= do maxBlkSz  :: Natural
    ]
  toJSON (ObsoleteNodeCHAIN currentPtcl supportedPtcl) = object
    [ "kind"              .= toJSON @String "ObsoleteNode"
    , "explanation"       .= toJSON @String explanation
    , "currentProtocol"   .= do currentPtcl   :: Natural
    , "supportedProtocol" .= do supportedPtcl :: Natural
    ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."

instance
  ( ToJSON (Protocol.PrevHash crypto)
  , ToJSON (WithOrigin (LastAppliedBlock crypto))
  , ToJSON BlockNo
  ) => ToJSON (PrtlSeqFailure crypto) where
  toJSON (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) = object
    [ "kind"        .= toJSON @String "WrongSlotInterval"
    , "lastSlot"    .= do lastSlot :: Word64
    , "currentSlot" .= do currSlot :: Word64
    ]
  toJSON (WrongBlockNoPrtclSeq lab currentBlockNo) = object
    [ "kind"                .= toJSON @String "WrongBlockNo"
    , "lastAppliedBlockNo"  .= do showLastAppBlockNo lab :: Text
    , "currentBlockNo"      .= (toJSON @Text . textShow $ unBlockNo currentBlockNo)
    ]
  toJSON (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) = object
    [ "kind"                  .= toJSON @String "WrongBlockSequence"
    , "lastAppliedBlockHash"  .= toJSON @Text (textShow lastAppliedHash)
    , "currentBlockHash"      .= toJSON @Text (textShow currentHash)
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (Ledger.EraRule "LEDGERS" era))
         ) => ToJSON (ShelleyBbodyPredFailure era) where
  toJSON (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) = object
    [ "kind"                  .= toJSON @String "WrongBlockBodySizeBBODY"
    , "actualBlockBodySize"   .= do actualBodySz  :: Int
    , "claimedBlockBodySize"  .= do claimedBodySz :: Int
    ]
  toJSON (InvalidBodyHashBBODY actualHash claimedHash) = object
    [ "kind"            .= toJSON @String "InvalidBodyHashBBODY"
    , "actualBodyHash"  .= do textShow actualHash   :: Text
    , "claimedBodyHash" .= do textShow claimedHash  :: Text
    ]
  toJSON (LedgersFailure f) = toJSON f


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (ShelleyUTXOW era))
         , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToJSON (ShelleyLedgersPredFailure era) where
  toJSON (LedgerFailure f) = object
    [ "kind"  .= toJSON @String "LedgerFailure"
    , "value" .= f
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (ShelleyUTXOW era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEGS" era))
         , ToJSON (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => ToJSON (ShelleyLedgerPredFailure era) where
  toJSON (UtxowFailure f) = object
    [ "kind"  .= toJSON @String "UtxowFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "UTXOW" era)
    ]
  toJSON (DelegsFailure f) = object
    [ "kind"  .= toJSON @String "DelegsFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "DELEGS" era)
    ]



instance ToJSON (Alonzo.ScriptPurpose Consensus.StandardCrypto) where
  toJSON = \case
    Alonzo.Minting pid -> object
      [ "minting" .= toJSON pid
      ]
    Alonzo.Spending txin -> object
      [ "spending" .= Api.fromShelleyTxIn txin
      ]
    Alonzo.Rewarding rwdAcct -> object
      [ "rewarding" .= toJSON @Text (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)
      ]
    Alonzo.Certifying cert -> object
      [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)
      ]

instance ToJSONKey (Shelley.ScriptHash Consensus.StandardCrypto) where
  toJSONKey = contramap (Api.serialiseToRawBytesHexText . Script.ScriptHash) toJSONKey

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToJSON (PredicateFailure (Ledger.EraRule "PPUP" era))
         , ToJSON (PredicateFailure (Ledger.EraRule "UTXO" era))
         , Ledger.Crypto era ~ StandardCrypto
         ) => ToJSON (AlonzoUtxowPredFailure era) where
  toJSON (Alonzo.ShelleyInAlonzoUtxowPredFailure _) = toJSON @String "TODO"
  toJSON (Alonzo.MissingRedeemers scripts) = object
    [ "kind"    .= toJSON @String "MissingRedeemers"
    , "scripts" .= do Map.fromList $ fmap swap scripts -- :: Map (Shelley.ScriptHash StandardCrypto) (Alonzo.ScriptPurpose StandardCrypto)
    ]
  toJSON (Alonzo.MissingRequiredDatums required received) = object
    [ "kind"      .= toJSON @String "MissingRequiredDatums"
    , "required"  .= do map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList required) :: [Text]
    , "received"  .= do map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList received) :: [Text]
    ]
  toJSON (Alonzo.PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) = object
    [ "kind"        .= toJSON @String "PPViewHashesDontMatch"
    , "fromTxBody"  .= do strictMaybeToMaybe ppHashInTxBody    :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
    , "fromPParams" .= do strictMaybeToMaybe ppHashFromPParams :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
    ]
  toJSON (Alonzo.MissingRequiredSigners missingKeyWitnesses) = object
    [ "kind"      .= toJSON @String "MissingRequiredSigners"
    , "witnesses" .= do Set.toList missingKeyWitnesses :: [KeyHash 'Witness StandardCrypto]
    ]
  toJSON (Alonzo.UnspendableUTxONoDatumHash txins) = object
    [ "kind"  .= toJSON @String "MissingRequiredSigners"
    , "txins" .= do Set.toList txins :: [TxIn StandardCrypto]
    ]
  toJSON (Alonzo.NonOutputSupplimentaryDatums disallowed acceptable) = object
    [ "kind"        .= toJSON @String "NonOutputSupplimentaryDatums"
    , "disallowed"  .= do Set.toList disallowed :: [Ledger.DataHash StandardCrypto]
    , "acceptable"  .= do Set.toList acceptable :: [Ledger.DataHash StandardCrypto]
    ]
  toJSON (Alonzo.ExtraRedeemers rdmrs) = object
    [ "kind"  .= toJSON @String "ExtraRedeemers"
    , "rdmrs" .= do map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs :: [String]
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (ShelleyUTXO era))
         , ToJSON (PredicateFailure (Core.EraRule "UTXO" era))
         ) => ToJSON (Shelley.ShelleyUtxowPredFailure era) where
  toJSON (Shelley.ExtraneousScriptWitnessesUTXOW extraneousScripts) = object
    [ "kind"              .= toJSON @String "InvalidWitnessesUTXOW"
    , "extraneousScripts" .= do extraneousScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (Shelley.InvalidWitnessesUTXOW wits') = object
    [ "kind"              .= toJSON @String "InvalidWitnessesUTXOW"
    , "invalidWitnesses"  .= do map textShow wits' :: [Text]
    ]
  toJSON (Shelley.MissingVKeyWitnessesUTXOW wits') = object
    [ "kind"              .= toJSON @String "MissingVKeyWitnessesUTXOW"
    , "missingWitnesses"  .= do wits' :: Set (KeyHash 'Witness (Ledger.Crypto era))
    ]
  toJSON (Shelley.MissingScriptWitnessesUTXOW missingScripts) = object
    [ "kind"            .= toJSON @String "MissingScriptWitnessesUTXOW"
    , "missingScripts"  .= do missingScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (Shelley.ScriptWitnessNotValidatingUTXOW failedScripts) = object
    [ "kind"          .= toJSON @String "ScriptWitnessNotValidatingUTXOW"
    , "failedScripts" .= do failedScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (Shelley.UtxoFailure f) = toJSON f
  toJSON (Shelley.MIRInsufficientGenesisSigsUTXOW genesisSigs) = object
    [ "kind"        .= toJSON @String "MIRInsufficientGenesisSigsUTXOW"
    , "genesisSigs" .= do genesisSigs :: Set (KeyHash 'Witness (Ledger.Crypto era))
    ]
  toJSON (Shelley.MissingTxBodyMetadataHash metadataHash) = object
    [ "kind"          .= toJSON @String "MissingTxBodyMetadataHash"
    , "metadataHash"  .= do metadataHash :: Alonzo.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON (Shelley.MissingTxMetadata txBodyMetadataHash) = object
    [ "kind"                .= toJSON @String "MissingTxMetadata"
    , "txBodyMetadataHash"  .= do txBodyMetadataHash :: Alonzo.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON (Shelley.ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) = object
    [ "kind"                .= toJSON @String "ConflictingMetadataHash"
    , "txBodyMetadataHash"  .= do txBodyMetadataHash  :: Alonzo.AuxiliaryDataHash (Ledger.Crypto era)
    , "fullMetadataHash"    .= do fullMetadataHash    :: Alonzo.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON Shelley.InvalidMetadata = object
    [ "kind"  .= toJSON @String "InvalidMetadata"
    ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => ToJSON (Shelley.ShelleyUtxoPredFailure era) where
  toJSON (Shelley.BadInputsUTxO badInputs) = object
    [ "kind"      .= toJSON @String "BadInputsUTxO"
    , "badInputs" .= do badInputs :: Set (TxIn (Ledger.Crypto era))
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (Shelley.ExpiredUTxO ttl slot) = object
    [ "kind" .= toJSON @String "ExpiredUTxO"
    , "ttl"  .= do ttl  :: SlotNo
    , "slot" .= do slot :: SlotNo
    ]
  toJSON (Shelley.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= toJSON @String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (Shelley.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooSmallUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= toJSON @String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (Shelley.OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= toJSON @String "OutputBootAddrAttrsTooBig"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= toJSON @String "The Byron address attributes are too big"
    ]
  toJSON Shelley.InputSetEmptyUTxO = object
    [ "kind" .= toJSON @String "InputSetEmptyUTxO"
    ]
  toJSON (Shelley.FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= toJSON @String "FeeTooSmallUTxO"
    , "minimum" .= do minfee  :: Coin
    , "fee"     .= do txfee   :: Coin
    ]
  toJSON (Shelley.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= toJSON @String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed  :: Ledger.Value era
    , "produced"  .= do produced  :: Ledger.Value era
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (Shelley.UpdateFailure f) = object
    [ "kind"  .= toJSON @String "UpdateFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "PPUP" era)
    ]
  toJSON (Shelley.WrongNetwork network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto era))
    ]
  toJSON (Shelley.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetworkWithdrawal"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (RewardAcnt (Ledger.Crypto era))
    ]

instance ToJSON MA.ValidityInterval where
  toJSON vi = object $
        [ "invalidBefore"    .= x | x <- mbfield' (MA.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield' (MA.invalidHereafter vi) ]
    where mbfield' SNothing  = []
          mbfield' (SJust x) = [x]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToJSON (MA.ShelleyMAUtxoPredFailure era) where
  toJSON (MA.BadInputsUTxO badInputs) = object
    [ "kind"      .= toJSON @String "BadInputsUTxO"
    , "badInputs" .= do badInputs                        :: Set (TxIn (Ledger.Crypto era))
    , "error"     .= do renderBadInputsUTxOErr badInputs :: Value
    ]
  toJSON (MA.OutsideValidityIntervalUTxO validityInterval slot) = object
    [ "kind"              .= toJSON @String "ExpiredUTxO"
    , "validityInterval"  .= do validityInterval  :: MA.ValidityInterval
    , "slot"              .= do slot              :: SlotNo
    ]
  toJSON (MA.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= toJSON @String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  toJSON MA.InputSetEmptyUTxO = object
    [ "kind"  .= toJSON @String "InputSetEmptyUTxO"
    ]
  toJSON (MA.FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= toJSON @String "FeeTooSmallUTxO"
    , "minimum" .= do minfee  :: Coin
    , "fee"     .= do txfee   :: Coin
    ]
  toJSON (MA.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= toJSON @String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed                                     :: Ledger.Value era
    , "produced"  .= do produced                                     :: Ledger.Value era
    , "error"     .= do renderValueNotConservedErr consumed produced :: Value
    ]
  toJSON (MA.WrongNetwork network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto era))
    ]
  toJSON (MA.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetworkWithdrawal"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (RewardAcnt (Ledger.Crypto era))
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (MA.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooSmallUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= toJSON @String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (MA.UpdateFailure f) = toJSON f
  toJSON (MA.OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= toJSON @String "OutputBootAddrAttrsTooBig"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= toJSON @String "The Byron address attributes are too big"
    ]
  toJSON MA.TriesToForgeADA = object
    [ "kind"  .= toJSON @String "TriesToForgeADA"
    ]
  toJSON (MA.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooBigUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= toJSON @String "Too many asset ids in the tx output"
    ]


instance Ledger.Era era => ToJSON (ShelleyPpupPredFailure era) where
  toJSON (NonGenesisUpdatePPUP proposalKeys genesisKeys) = object
    [ "kind"  .= toJSON @String "NonGenesisUpdatePPUP"
    , "keys"  .= do proposalKeys Set.\\ genesisKeys :: Set (KeyHash 'Genesis (Ledger.Crypto era))
    ]
  toJSON (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) = object
    [ "kind"          .= toJSON @String "PPUpdateWrongEpoch"
    , "currentEpoch"  .= do currEpoch     :: EpochNo
    , "intendedEpoch" .= do intendedEpoch :: EpochNo
    , "votingPeriod"  .= toJSON @String (show votingPeriod)
    ]
  toJSON (PVCannotFollowPPUP badPv) = object
    [ "kind"                .= toJSON @String "PVCannotFollowPPUP"
    , "badProtocolVersion"  .= do badPv :: ProtVer
    ]


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToJSON (ShelleyDelegsPredFailure era) where
  toJSON (DelegateeNotRegisteredDELEG targetPool) = object
    [ "kind"        .= toJSON @String "DelegateeNotRegisteredDELEG"
    , "targetPool"  .= do targetPool :: KeyHash 'StakePool (Ledger.Crypto era)
    ]
  toJSON (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) = object
    [ "kind"                  .= toJSON @String "WithdrawalsNotInRewardsDELEGS"
    , "incorrectWithdrawals"  .= do incorrectWithdrawals -- :: Map (RewardAcnt (Ledger.Crypto era)) Coin
    ]
  toJSON (DelplFailure f) = toJSON do f :: PredicateFailure (Ledger.EraRule "DELPL" era)

instance ( ToJSON (PredicateFailure (Core.EraRule "POOL" era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToJSON (ShelleyDelplPredFailure era) where
  toJSON (PoolFailure   f) = toJSON do f :: PredicateFailure (Ledger.EraRule "POOL"  era)
  toJSON (DelegFailure  f) = toJSON do f :: PredicateFailure (Ledger.EraRule "DELEG" era)

instance Ledger.Era era => ToJSON (ShelleyDelegPredFailure era) where
  toJSON (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyAlreadyRegisteredDELEG"
    , "credential"  .= toJSON @Text (textShow alreadyRegistered)
    , "error"       .= toJSON @String "Staking credential already registered"
    ]
  toJSON (StakeKeyInRewardsDELEG alreadyRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyInRewardsDELEG"
    , "credential"  .= toJSON @Text (textShow alreadyRegistered)
    , "error"       .= toJSON @String "Staking credential registered in rewards map"
    ]
  toJSON (StakeKeyNotRegisteredDELEG notRegistered) = object
    [ "kind"        .= toJSON @String "StakeKeyNotRegisteredDELEG"
    , "credential"  .= toJSON @Text (textShow notRegistered)
    , "error"       .= toJSON @String "Staking credential not registered"
    ]
  toJSON (StakeKeyNonZeroAccountBalanceDELEG remBalance) = object
    [ "kind"              .= toJSON @String "StakeKeyNonZeroAccountBalanceDELEG"
    , "remainingBalance"  .= remBalance
    ]
  toJSON (StakeDelegationImpossibleDELEG unregistered) = object
    [ "kind"        .= toJSON @String "StakeDelegationImpossibleDELEG"
    , "credential"  .= toJSON @Text (textShow unregistered)
    , "error"       .= toJSON @String "Cannot delegate this stake credential because it is not registered"
    ]
  toJSON WrongCertificateTypeDELEG = object
    [ "kind" .= toJSON @String "WrongCertificateTypeDELEG"
    ]
  toJSON (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"            .= toJSON @String "GenesisKeyNotInMappingDELEG"
    , "unknownKeyHash"  .= toJSON @Text (textShow genesisKeyHash)
    , "error"           .= toJSON @String "This genesis key is not in the delegation mapping"
    ]
  toJSON (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"              .= toJSON @String "DuplicateGenesisDelegateDELEG"
    , "duplicateKeyHash"  .= toJSON @Text (textShow genesisKeyHash)
    , "error"             .= toJSON @String "This genesis key has already been delegated to"
    ]
  toJSON (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) = object
    [ "kind"          .= toJSON @String "InsufficientForInstantaneousRewardsDELEG"
    , "pot"           .= toJSON @String potText
    , "neededAmount"  .= do neededMirAmount :: Coin
    , "reserves"      .= do reserves        :: Coin
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) = object
    [ "kind"                        .= toJSON @String "MIRCertificateTooLateinEpochDELEG"
    , "currentSlotNo"               .= do currSlot    :: SlotNo
    , "mustBeSubmittedBeforeSlotNo" .= do boundSlotNo :: SlotNo
    ]
  toJSON (DuplicateGenesisVRFDELEG vrfKeyHash) = object
    [ "kind"    .= toJSON @String "DuplicateGenesisVRFDELEG"
    , "keyHash" .= do vrfKeyHash -- :: Crypto.Hash (Crypto.HASH (Ledger.Crypto era)) (VerKeyVRF (Ledger.Crypto era))
    ]
  toJSON MIRTransferNotCurrentlyAllowed = object
    [ "kind" .= toJSON @String "MIRTransferNotCurrentlyAllowed"
    ]
  toJSON MIRNegativesNotCurrentlyAllowed = object
    [ "kind" .= toJSON @String "MIRNegativesNotCurrentlyAllowed"
    ]
  toJSON (InsufficientForTransferDELEG mirpot attempted available) = object
    [ "kind"      .= toJSON @String "DuplicateGenesisVRFDELEG"
    , "pot"       .= toJSON @String potText
    , "attempted" .= do attempted :: Coin
    , "available" .= do available :: Coin
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON MIRProducesNegativeUpdate = object
    [ "kind" .= toJSON @String "MIRProducesNegativeUpdate"
    ]
  toJSON (MIRNegativeTransfer pot coin) = object
    [ "kind"    .= toJSON @String "MIRNegativeTransfer"
    , "error"   .= toJSON @String "Attempt to transfer a negative amount from a pot."
    , "pot"     .= toJSON @String potText
    , "amount"  .= do coin :: Coin
    ]
    where potText = case pot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"

instance ToJSON (ShelleyPoolPredFailure era) where
  toJSON (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) = object
    [ "kind"                .= toJSON @String "StakePoolNotRegisteredOnKeyPOOL"
    , "unregisteredKeyHash" .= toJSON @Text (textShow unregStakePool)
    , "error"               .= toJSON @String "This stake pool key hash is unregistered"
    ]
  toJSON (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) = object
    [ "kind"                    .= toJSON @String "StakePoolRetirementWrongEpochPOOL"
    , "currentEpoch"            .= toJSON @Text (textShow currentEpoch)
    , "intendedRetirementEpoch" .= toJSON @Text (textShow intendedRetireEpoch)
    , "maxEpochForRetirement"   .= toJSON @Text (textShow maxRetireEpoch)
    ]
  toJSON (StakePoolCostTooLowPOOL certCost protCost) = object
    [ "kind"              .= toJSON @String "StakePoolCostTooLowPOOL"
    , "certificateCost"   .= toJSON @Text (textShow certCost)
    , "protocolParCost"   .= toJSON @Text (textShow protCost)
    , "error"             .= toJSON @String "The stake pool cost is too low"
    ]
  toJSON (PoolMedataHashTooBig poolID hashSize) = object
    [ "kind"      .= toJSON @String "PoolMedataHashTooBig"
    , "poolID"    .= toJSON @Text (textShow poolID)
    , "hashSize"  .= toJSON @Text (textShow hashSize)
    , "error"     .= toJSON @String "The stake pool metadata hash is too large"
    ]

-- Apparently this should never happen according to the Shelley exec spec
  toJSON (WrongCertificateTypePOOL index) =
    case index of
      0 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: Delegation certificate"
        ]
      1 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: MIR certificate"
        ]
      2 -> object
        [ "kind"  .= toJSON @String "WrongCertificateTypePOOL"
        , "error" .= toJSON @String "Wrong certificate type: Genesis certificate"
        ]
      k -> object
        [ "kind"            .= toJSON @String "WrongCertificateTypePOOL"
        , "certificateType" .= do k :: Word8
        , "error"           .= toJSON @String "Wrong certificate type: Unknown certificate type"
        ]

  toJSON (WrongNetworkPOOL networkId listedNetworkId poolId) = object
    [ "kind"            .= toJSON @String "WrongNetworkPOOL"
    , "networkId"       .= toJSON @Text (textShow networkId)
    , "listedNetworkId" .= toJSON @Text (textShow listedNetworkId)
    , "poolId"          .= toJSON @Text (textShow poolId)
    , "error"           .= toJSON @String "Wrong network ID in pool registration certificate"
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToJSON (ShelleyTickPredFailure era) where
  toJSON (NewEpochFailure f) = object
    [ "kind"  .= toJSON @String "NewEpochFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "NEWEPOCH" era)
    ]
  toJSON (RupdFailure f) = object
    [ "kind"  .= toJSON @String "RupdFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "RUPD" era)
    ]


instance ( ToJSON (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "MIR" era))
         ) => ToJSON (ShelleyNewEpochPredFailure era) where
  toJSON (EpochFailure f) = object
    [ "kind"    .= toJSON @String "EpochFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "EPOCH" era)
    ]
  toJSON (MirFailure f) = object
    [ "kind"    .= toJSON @String "MirFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "MIR" era)
    ]
  toJSON (CorruptRewardUpdate update) = object
    [ "kind"    .= toJSON @String "CorruptRewardUpdate"
    , "update"  .= toJSON @String (show update)
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "SNAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToJSON (ShelleyEpochPredFailure era) where
  toJSON (PoolReapFailure f) = object
    [ "kind"    .= toJSON @String "PoolReapFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "POOLREAP" era)
    ]
  toJSON (SnapFailure f) = object
    [ "kind"    .= toJSON @String "SnapFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "SNAP" era)
    ]
  toJSON (UpecFailure f) = object
    [ "kind"    .= toJSON @String "UpecFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "UPEC" era)
    ]

-- TODO: Need to elaborate more on this error
instance ToJSON (ShelleyNewppPredFailure era) where
  toJSON (UnexpectedDepositPot outstandingDeposits depositPot) = object
    [ "kind"                .= toJSON @String "UnexpectedDepositPot"
    , "outstandingDeposits" .= toJSON @Text (textShow outstandingDeposits)
    , "depositPot"          .= toJSON @Text (textShow depositPot)
    ]

instance
  ( Crypto.Crypto crypto
  , ToJSON (OverlayPredicateFailure crypto)
  ) => ToJSON (PrtclPredicateFailure crypto) where
  toJSON (OverlayFailure f) = object
    [ "kind"    .= toJSON @String "OverlayFailure"
    , "update"  .= do f :: OverlayPredicateFailure crypto
    ]
  toJSON (UpdnFailure f) = object
    [ "kind"    .= toJSON @String "UpdnFailure"
    , "update"  .= do f :: UpdnPredicateFailure crypto
    ]

instance
  ( Crypto.Crypto crypto
  , ToJSON (OcertPredicateFailure crypto)
  , ToJSON L.ActiveSlotCoeff
  ) => ToJSON (OverlayPredicateFailure crypto) where
  toJSON (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) = object
    [ "kind"            .= toJSON @String "UnknownGenesisKeyOVERLAY"
    , "unknownKeyHash"  .= toJSON @Text (textShow genKeyHash)
    ]
  toJSON (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) = object
    [ "kind"                .= toJSON @String "VRFKeyBadLeaderValueOVERLAY"
    , "seedNonce"           .= toJSON @Text (textShow seedNonce)
    , "currentSlot"         .= toJSON @Text (textShow currSlotNo)
    , "previousHashAsNonce" .= toJSON @Text (textShow prevHashNonce)
    , "leaderElectionValue" .= toJSON @Text (textShow leaderElecVal)
    ]
  toJSON (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) = object
    [ "kind"                .= toJSON @String "VRFKeyBadNonceOVERLAY"
    , "seedNonce"           .= toJSON @Text (textShow seedNonce)
    , "currentSlot"         .= toJSON @Text (textShow currSlotNo)
    , "previousHashAsNonce" .= toJSON @Text (textShow prevHashNonce)
    , "blockNonce"          .= toJSON @Text (textShow blockNonce)
    ]
  toJSON (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) = object
    [ "kind"                    .= toJSON @String "VRFKeyWrongVRFKeyOVERLAY"
    , "poolHash"                .= textShow issuerHash
    , "registeredVRFKeHash"     .= textShow regVRFKeyHash
    , "unregisteredVRFKeyHash"  .= textShow unregVRFKeyHash
    ]
  toJSON (VRFKeyUnknown (KeyHash kHash)) = object
    [ "kind"    .= toJSON @String "VRFKeyUnknownOVERLAY"
    , "keyHash" .= toJSON @Text (textShow kHash)
    ]
  toJSON (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) = object
    [ "kind"                  .= toJSON @String "VRFLeaderValueTooBigOVERLAY"
    , "leaderElectionValue"   .= toJSON @Text (textShow leadElecVal)
    , "delegationPoolWeight"  .= toJSON @Text (textShow weightOfDelegPool)
    , "activeSlotCoefficient" .= toJSON @Text (textShow actSlotCoefff)
    ]
  toJSON (NotActiveSlotOVERLAY notActiveSlotNo) = object
    [ "kind" .= toJSON @String "NotActiveSlotOVERLAY"
    , "slot" .= toJSON @Text (textShow notActiveSlotNo)
    ]
  toJSON (WrongGenesisColdKeyOVERLAY actual expected) = object
    [ "kind"      .= toJSON @String "WrongGenesisColdKeyOVERLAY"
    , "actual"    .= do actual    :: KeyHash 'BlockIssuer crypto
    , "expected"  .= do expected  :: KeyHash 'GenesisDelegate crypto
    ]
  toJSON (WrongGenesisVRFKeyOVERLAY issuer actual expected) = object
    [ "kind"      .= toJSON @String "WrongGenesisVRFKeyOVERLAY"
    , "issuer"    .= do issuer    :: KeyHash 'BlockIssuer crypto
    , "actual"    .= do actual
    , "expected"  .= do expected
    ]
  toJSON (OcertFailure f) = toJSON f

instance
  ( Crypto.Crypto crypto
  ) => ToJSON (OcertPredicateFailure crypto) where
  toJSON (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) = object
    [ "kind"                  .= toJSON @String "KESBeforeStartOCERT"
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertstart)
    , "currentKESPeriod"      .= toJSON @Text (textShow current)
    , "error"                 .= toJSON @String "Your operational certificate's KES start period is before the KES current period."
    ]
  toJSON (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) = object
    [ "kind"                  .= toJSON @String "KESAfterEndOCERT"
    , "currentKESPeriod"      .= toJSON @Text (textShow current)
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertstart)
    , "maxKESEvolutions"      .= toJSON @Text (textShow maxKESEvolutions)
    , "error"                 .= toJSON @String "The operational certificate's KES start period is greater than the max number of KES + the KES current period"
    ]
  toJSON (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) = object
    [ "kind"              .= toJSON @String "CounterTooSmallOCert"
    , "currentKESCounter" .= toJSON @Text (textShow currentKESCounter)
    , "lastKESCounter"    .= toJSON @Text (textShow lastKEScounterUsed)
    , "error"             .= toJSON @String "The operational certificate's last KES counter is greater than the current KES counter."
    ]
  toJSON (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) = object
    [ "kind"                  .= toJSON @String "InvalidSignatureOCERT"
    , "opCertKESStartPeriod"  .= toJSON @Text (textShow oCertKESStartPeriod)
    , "opCertCounter"         .= toJSON @Text (textShow oCertCounter)
    ]
  toJSON (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) = object
    [ "kind"                        .= toJSON @String "InvalidKesSignatureOCERT"
    , "opCertKESStartPeriod"        .= toJSON @Text (textShow startKESPeriod)
    , "opCertKESCurrentPeriod"      .= toJSON @Text (textShow currKESPeriod)
    , "opCertExpectedKESEvolutions" .= toJSON @Text (textShow expectedKESEvolutions)
    , "error"                       .= do err :: String
    ]
  toJSON (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) = object
    [ "kind" .= toJSON @String "NoCounterForKeyHashOCERT"
    , "stakePoolKeyHash" .= toJSON @Text (textShow stakePoolKeyHash)
    , "error" .= toJSON @String "A counter was not found for this stake pool key hash"
    ]


instance ToJSON (ShelleyUpecPredFailure era) where
  toJSON (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) = object
    [ "kind"              .= toJSON @String "UnexpectedDepositPot"
    , "totalOutstanding"  .= toJSON @Text (textShow totalOutstanding)
    , "depositPot"        .= toJSON @Text (textShow depositPot)
    ]


--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------


instance ( Ledger.Era era
         , ToJSON (Ledger.Value era)
         , ToJSON (Ledger.TxOut era)
         , ToJSON (PredicateFailure (Ledger.EraRule "UTXOS" era))
         , Consensus.ShelleyBasedEra era
         ) => ToJSON (AlonzoUtxoPredFailure era) where
  toJSON (Alonzo.BadInputsUTxO badInputs) = object
    [ "kind"      .= toJSON @String "BadInputsUTxO"
    , "badInputs" .= do badInputs                               -- :: Set (TxIn (Ledger.Crypto (Consensus.AlonzoEra StandardCrypto)))
    , "error"     .= do renderBadInputsUTxOErr badInputs :: Value
    ]
  toJSON (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) = object
    [ "kind"              .= toJSON @String "ExpiredUTxO"
    , "validityInterval"  .= validtyInterval
    , "slot"              .= do slot            :: SlotNo
    ]
  toJSON (Alonzo.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= toJSON @String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  toJSON Alonzo.InputSetEmptyUTxO = object
    [ "kind" .= toJSON @String "InputSetEmptyUTxO"
    ]
  toJSON (Alonzo.FeeTooSmallUTxO minfee currentFee) = object
    [ "kind"    .= toJSON @String "FeeTooSmallUTxO"
    , "minimum" .= do minfee      :: Coin
    , "fee"     .= do currentFee  :: Coin
    ]
  toJSON (Alonzo.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= toJSON @String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed                                     :: Ledger.Value era
    , "produced"  .= do produced                                     :: Ledger.Value era
    , "error"     .= do renderValueNotConservedErr consumed produced :: Value
    ]
  toJSON (Alonzo.WrongNetwork network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto (Consensus.AlonzoEra (Ledger.Crypto era))))
    ]
  toJSON (Alonzo.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= toJSON @String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= toJSON @String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (Alonzo.UtxosFailure predFailure) = object
    [ "kind"    .= toJSON @String "UtxosFailure"
    , "error"   .= toJSON predFailure
    ]
  toJSON (Alonzo.OutputBootAddrAttrsTooBig txouts) = object
    [ "kind"    .= toJSON @String "OutputBootAddrAttrsTooBig"
    , "outputs" .= txouts
    , "error"   .= toJSON @String "The Byron address attributes are too big"
    ]
  toJSON Alonzo.TriesToForgeADA = object
    [ "kind"  .= toJSON @String "TriesToForgeADA"
    ]
  toJSON (Alonzo.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= toJSON @String "OutputTooBigUTxO"
    , "outputs" .= badOutputs
    , "error"   .= toJSON @String "Too many asset ids in the tx output"
    ]
  toJSON (Alonzo.InsufficientCollateral computedBalance suppliedFee) = object
    [ "kind"    .= toJSON @String "InsufficientCollateral"
    , "balance" .= computedBalance
    , "txfee"   .= suppliedFee
    ]
  toJSON (Alonzo.ScriptsNotPaidUTxO utxos) = object
    [ "kind"  .= toJSON @String "ScriptsNotPaidUTxO"
    , "utxos" .= utxos
    ]
  toJSON (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) = object
    [ "kind"        .= toJSON @String "ExUnitsTooBigUTxO"
    , "maxexunits"  .= pParamsMaxExUnits
    , "exunits"     .= suppliedExUnits
    ]
  toJSON (Alonzo.CollateralContainsNonADA inputs) = object
    [ "kind"    .= toJSON @String "CollateralContainsNonADA"
    , "inputs"  .= inputs
    ]
  toJSON (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) = object
    [ "kind"            .= toJSON @String "WrongNetworkInTxBody"
    , "networkid"       .= actualNetworkId
    , "txbodyNetworkId" .= netIdInTxBody
    ]
  toJSON (Alonzo.OutsideForecast slotNum) = object
    [ "kind" .= toJSON @String "OutsideForecast"
    , "slot" .= slotNum
    ]
  toJSON (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) = object
    [ "kind"    .= toJSON @String "TooManyCollateralInputs"
    , "max"     .= maxCollateralInputs
    , "inputs"  .= numberCollateralInputs
    ]
  toJSON Alonzo.NoCollateralInputs = object
    [ "kind"  .= toJSON @String "NoCollateralInputs"
    ]

instance ( -- ToJSON (Alonzo.CollectError (Ledger.Crypto era))
         -- ,
        --  ToJSON (PredicateFailure (Ledger.EraRule "PPUP" era))
         ) =>ToJSON (AlonzoUtxosPredFailure era) where
  toJSON (Alonzo.ValidationTagMismatch isValidating reason) = object
    [ "kind"          .= toJSON @String "ValidationTagMismatch"
    , "isvalidating"  .= isValidating
    , "reason"        .= do reason :: Alonzo.TagMismatchDescription
    ]
  toJSON (Alonzo.CollectErrors _errors) = object
    [ "kind"    .= toJSON @String "CollectErrors"
    , "errors"  .= toJSON @String "TODO" -- errors
    ]
  toJSON (Alonzo.UpdateFailure _pFailure) = toJSON @String "TODO" -- toJSON pFailure

deriving newtype instance ToJSON Alonzo.IsValid

-- instance ToJSON (Alonzo.CollectError (Ledger.Crypto era)) where
--   toJSON = \case
--     Alonzo.NoRedeemer sPurpose -> object
--       [ "kind"          .= String "CollectError"
--       , "error"         .= String "NoRedeemer"
--       , "scriptpurpose" .= do sPurpose :: Alonzo.ScriptPurpose (Ledger.Crypto era)
--       ]
--     Alonzo.NoWitness sHash -> object
--       [ "kind" .= String "CollectError"
--       , "error" .= String "NoWitness"
--       , "scripthash" .= toJSON sHash
--       ]
--     Alonzo.NoCostModel lang -> object
--       [ "kind" .= String "CollectError"
--       , "error" .= String "NoCostModel"
--       , "language" .= toJSON lang
--       ]
--     Alonzo.BadTranslation err -> object
--       [ "kind" .= String "PlutusTranslationError"
--       , "error" .= case err of
--           Alonzo.ByronTxOutInContext txOutSource ->
--             object
--               [ "kind" .= String "ByronTxOutInContext"
--               , "txOutSource" .= txOutSource
--               ]
--           Alonzo.TranslationLogicMissingInput txin ->
--             object
--               [ "kind" .= String "TranslationLogicMissingInput"
--               , "txin" .= txin
--               ]
--           Alonzo.RdmrPtrPointsToNothing ptr ->
--             object
--               [ "kind" .= String "RedeemerPointerPointsToNothing"
--               , "ptr" .= Api.renderScriptWitnessIndex (Api.fromAlonzoRdmrPtr ptr)
--               ]
--           Alonzo.LanguageNotSupported lang ->
--             object
--               [ "kind" .= String "LanguageNotSupported"
--               , "lang" .= toJSON lang
--               ]
--           Alonzo.InlineDatumsNotSupported txOutSource ->
--             object
--               [ "kind" .= String "InlineDatumsNotSupported"
--               , "txOutSource" .= toJSON txOutSource
--               ]
--           Alonzo.ReferenceScriptsNotSupported txOutSource ->
--             object
--               [ "kind" .= String "ReferenceScriptsNotSupported"
--               , "txOutSource" .= toJSON txOutSource
--               ]
--           Alonzo.ReferenceInputsNotSupported txins ->
--             object
--               [ "kind" .= String "ReferenceInputsNotSupported"
--               , "txins" .= toJSON txins
--               ]
--           Alonzo.TimeTranslationPastHorizon msg ->
--             object
--               [ "kind" .= String "TimeTranslationPastHorizon"
--               , "msg" .= toJSON msg
--               ]
--       ]

instance ToJSON (Alonzo.TxOutSource StandardCrypto) where
  toJSON = \case
    Alonzo.TxOutFromInput txin ->
      object
        [ "kind" .= toJSON @String "TxOutFromInput"
        , "msg" .= toJSON txin
        ]
    Alonzo.TxOutFromOutput txix ->
      object
        [ "kind" .= toJSON @String "TxOutFromOutput"
        , "msg" .= toJSON txix
        ]

instance ToJSON Alonzo.FailureDescription where
  toJSON (Alonzo.PlutusFailure _t _bs) = object
    [ "kind"                  .= toJSON @String "FailureDescription"
    , "error"                 .= toJSON @String "PlutusFailure"
    , "reconstructionDetail"  .= toJSON @String "TODO" -- do Alonzo.debugPlutus (BSU.toString bs) :: Alonzo.PlutusDebugInfo
    -- , "description"           .= t
    ]

-- instance
--   ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
--   ) => ToJSON (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
--   toJSON err = object
--     [ "kind"  .= String "AlonzoBbodyPredFail"
--     , "error" .= String (show err)
--     ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- instance ToJSON Alonzo.PlutusDebugInfo where
--   toJSON = \case
--     Alonzo.DebugSuccess budget -> object
--       [ "kind"    .= String "DebugSuccess"
--       , "budget"  .= do budget :: Cek.ExBudget
--       ]
--     Alonzo.DebugCannotDecode msg -> object
--       [ "kind"    .= String "DebugCannotDecode"
--       , "message" .= do msg :: String
--       ]
--     Alonzo.DebugInfo texts e d -> object
--       [ "kind"  .= String "DebugInfo"
--       , "texts" .= do texts :: [Text]
--       , "error" .= do e     :: Alonzo.PlutusError
--       , "debug" .= do d     :: Alonzo.PlutusDebug
--       ]
--     Alonzo.DebugBadHex msg -> object
--       [ "kind"    .= String "DebugBadHex"
--       , "message" .= do msg :: String
--       ]

-- instance ToJSON Alonzo.PlutusError where
--   toJSON = \case
--     Alonzo.PlutusErrorV1 evaluationError -> toJSON do evaluationError :: PV1.EvaluationError
--     Alonzo.PlutusErrorV2 evaluationError -> toJSON do evaluationError :: PV1.EvaluationError

-- instance ToJSON Alonzo.PlutusDebug where
--   toJSON = \case
--     Alonzo.PlutusDebugV1 _costModel exUnits sbs ds protVer -> object
--       [ "exUnits"     .= do exUnits                                               :: Ledger.ExUnits
--       , "sbs"         .= do Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text
--       , "scriptHash"  .= do scriptHashOf Alonzo.PlutusV1 sbs                      :: Text
--       , "dsSummary"   .= do plutusDataToDsSummary ds                              :: Aeson.Value
--       , "protVer"     .= do protVer                                               :: ProtVer
--       -- , "ds"          .= toJSON ds
--       -- , "costModel"   .= costModel
--       ]
--     Alonzo.PlutusDebugV2 _costModel exUnits sbs ds protVer -> object
--       [ "exUnits"     .= do exUnits                                               :: Ledger.ExUnits
--       , "sbs"         .= do Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text
--       , "scriptHash"  .= do scriptHashOf Alonzo.PlutusV2 sbs                      :: Text
--       , "dsSummary"   .= do plutusDataToDsSummary ds                              :: Aeson.Value
--       , "protVer"     .= do protVer                                               :: ProtVer
--       -- , "ds"          .= toJSON ds
--       -- , "costModel"   .= costModel
--       ]

-- plutusDataToDsSummary :: [Plutus.Data] -> Aeson.Value
-- plutusDataToDsSummary [dat, redeemer, info] = Aeson.object
--   [ "data"      .= do dat                             :: Plutus.Data
--   , "redeemer"  .= do redeemer                        :: Plutus.Data
--   , "info"      .= do plutusInfoDataToDsSummary info  :: Value
--   ]
-- plutusDataToDsSummary [dat, info] = Aeson.object
--   [ "data"      .= do dat                             :: Plutus.Data
--   , "info"      .= do plutusInfoDataToDsSummary info  :: Value
--   ]
-- plutusDataToDsSummary _ = Aeson.Null

-- plutusInfoDataToDsSummary :: Plutus.Data -> Aeson.Value
-- plutusInfoDataToDsSummary info = case PV1.fromData info of
--   Nothing -> String "no-info"
--   Just PV1.ScriptContext { PV1.scriptContextTxInfo, PV1.scriptContextPurpose} -> object
--     [ "txInfo"  .= do txInfoToJson scriptContextTxInfo          :: Value
--     , "purpose" .= do scriptPurposeToJson scriptContextPurpose  :: Value
--     ]

-- txInfoToJson :: PV1.TxInfo -> Value
-- txInfoToJson txInfo = Aeson.object
--   [ "inputs"      .= do PV1.txInfoInputs      txInfo :: [PV1.TxInInfo]
--   , "outputs"     .= do PV1.txInfoOutputs     txInfo :: [PV1.TxOut]
--   , "fee"         .= do PV1.txInfoFee         txInfo :: PV1.Value
--   , "mint"        .= do PV1.txInfoMint        txInfo :: PV1.Value
--   , "dCert"       .= do PV1.txInfoDCert       txInfo :: [PV1.DCert]
--   , "wdrl"        .= do PV1.txInfoWdrl        txInfo :: [(PV1.StakingCredential, Integer)]
--   , "validRange"  .= do PV1.txInfoValidRange  txInfo :: (PV1.Interval PV1.POSIXTime)
--   , "signatories" .= do PV1.txInfoSignatories txInfo :: [PV1.PubKeyHash]
--   , "data"        .= do PV1.txInfoData        txInfo :: [(PV1.DatumHash, PV1.Datum)]
--   , "id"          .= do PV1.txInfoId          txInfo :: PV1.TxId
--   ]

-- instance ToJSON PV1.Datum where
--   toJSON v = toJSON do PV1.builtinDataToData (PV1.getDatum v) :: Plutus.Data

-- instance ToJSON PV1.DatumHash where
--   toJSON v = toJSON do show v :: Text

-- instance ToJSON PV1.DCert where
--   toJSON = \case
--     PV1.DCertDelegRegKey stakingCredential -> object
--       [ "DCertDelegRegKey" .= do stakingCredential :: PV1.StakingCredential
--       ]
--     PV1.DCertDelegDeRegKey stakingCredential -> object
--       [ "DCertDelegDeRegKey" .= do stakingCredential :: PV1.StakingCredential
--       ]
--     PV1.DCertDelegDelegate delegator delagatee -> object
--       [ "DCertDelegDelegate" .= object
--         [ "delegator" .= do delegator :: PV1.StakingCredential
--         , "delegatee" .= do delagatee :: PV1.PubKeyHash
--         ]
--       ]
--     PV1.DCertPoolRegister poolId poolVfr -> object
--       [ "DCertPoolRegister" .= object
--         [ "poolId"  .= do poolId  :: PV1.PubKeyHash
--         , "poolVfr" .= do poolVfr :: PV1.PubKeyHash
--         ]
--       ]
--     PV1.DCertPoolRetire pkh n -> object
--       [ "DCertPoolRetire" .= object
--         [ "stakePoolId"   .= do pkh :: PV1.PubKeyHash
--         , "epochRetiring" .= do n   :: Integer
--         ]
--       ]
--     PV1.DCertGenesis -> String "DCertGenesis"
--     PV1.DCertMir -> String "DCertMir"

-- instance ToJSON (PV1.Interval PV1.POSIXTime) where
--   toJSON (PV1.Interval lo hi) = toJSON $
--     lowerBoundToJsonArray lo <>
--     upperBoundToJsonArray hi
--     where
--       lowerBoundToJsonArray :: PV1.LowerBound PV1.POSIXTime -> [Value]
--       lowerBoundToJsonArray = \case
--         PV1.LowerBound PV1.PosInf     _     -> ["(", "+∞"                 ]
--         PV1.LowerBound PV1.NegInf     _     -> ["(", "-∞"                 ]
--         PV1.LowerBound (PV1.Finite a) True  -> ["[", toJSON (PV1.toData a)]
--         PV1.LowerBound (PV1.Finite a) False -> ["(", toJSON (PV1.toData a)]

--       upperBoundToJsonArray :: PV1.UpperBound PV1.POSIXTime -> [Value]
--       upperBoundToJsonArray = \case
--         PV1.UpperBound PV1.PosInf     _     -> ["+∞"                 , ")"]
--         PV1.UpperBound PV1.NegInf     _     -> ["-∞"                 , ")"]
--         PV1.UpperBound (PV1.Finite a) True  -> [toJSON (PV1.toData a), "]"]
--         PV1.UpperBound (PV1.Finite a) False -> [toJSON (PV1.toData a), ")"]

-- instance ToJSON PV1.PubKeyHash where
--   toJSON v = toJSON do show v :: Text

-- instance ToJSON PV1.StakingCredential where
--   toJSON = \case
--     PV1.StakingHash credential -> object
--       [ "StakingHash" .= do credential :: PV1.Credential
--       ]
--     PV1.StakingPtr a b c -> toJSON do [a, b, c] :: [Integer]

-- instance ToJSON PV1.Credential where
--   toJSON = \case
--     PV1.PubKeyCredential pubKeyHash -> object
--       [ "PubKeyCredential" .= do pubKeyHash :: PV1.PubKeyHash
--       ]
--     PV1.ScriptCredential validatorHash -> object
--       [ "ScriptCredential" .= do validatorHash :: PV1.ValidatorHash
--       ]

-- instance ToJSON PV1.ValidatorHash where
--   toJSON h = toJSON do show h :: Text

-- instance ToJSON PV1.TxId where
--   toJSON v = toJSON do show v :: Text

-- instance ToJSON PV1.TxInInfo where
--   toJSON v = object
--     [ "outRef"    .= toJSON do PV1.txInInfoOutRef   v :: PV1.TxOutRef
--     , "resolved"  .= toJSON do PV1.txInInfoResolved v :: PV1.TxOut
--     ]

-- instance ToJSON PV1.TxOut where
--   toJSON v = object
--     [ "address"   .= do PV1.txOutAddress   v :: PV1.Address
--     , "value"     .= do PV1.txOutValue     v :: PV1.Value
--     , "datumHash" .= do PV1.txOutDatumHash v :: Maybe PV1.DatumHash
--     ]

-- instance ToJSON PV1.Address where
--   toJSON v = object
--     [ "credential"        .= do PV1.addressCredential        v :: PV1.Credential
--     , "stakingCredential" .= do PV1.addressStakingCredential v :: Maybe PV1.StakingCredential
--     ]

-- instance ToJSON PV1.Value where
--   toJSON (PV1.Value m) = toJSON do m :: AssocMap.Map PV1.CurrencySymbol (AssocMap.Map PV1.TokenName Integer)

-- instance ToJSON PV1.TokenName where
--   toJSON v = toJSON do show v :: Text

-- instance ToJSONKey PV1.TokenName where
--   toJSONKey = contramap (builtinByteStringToBase16Text . PV1.unTokenName) toJSONKey -- toJSONKeyText $ show @PV1.TokenName @Text

-- builtinByteStringToBase16Text :: PV1.BuiltinByteString -> Text
-- builtinByteStringToBase16Text bs = Text.filter (/= '"') (Text.pack (show bs)) -- TODO is there a better way to encode as Text

-- instance ToJSON PV1.CurrencySymbol where
--   toJSON (PV1.CurrencySymbol bs) = toJSON do show bs :: Text

-- instance ToJSONKey PV1.CurrencySymbol where
--   toJSONKey = toJSONKeyText $ show @PV1.CurrencySymbol @Text

-- instance (ToJSONKey k, Ord k, ToJSON a) => ToJSON (AssocMap.Map k a) where
--   toJSON = toJSON . Map.fromList . AssocMap.toList

-- instance ToJSON PV1.TxOutRef where
--   toJSON (PV1.TxOutRef txid idx) = toJSON
--     [ toJSON do txid  :: PV1.TxId
--     , toJSON do idx   :: Integer
--     ]

-- scriptPurposeToJson :: PV1.ScriptPurpose -> Value
-- scriptPurposeToJson = \case
--   PV1.Minting currencySymbol -> Aeson.object
--     [ "kind"  .= String "Minting"
--     , "value" .= do currencySymbol :: PV1.CurrencySymbol
--     ]
--   PV1.Spending outRef -> Aeson.object
--     [ "kind"  .= String "Spending"
--     , "value" .= toJSON outRef
--     ]
--   PV1.Rewarding stakingCredential -> Aeson.object
--     [ "kind"  .= String "Rewarding"
--     , "value" .= do stakingCredential :: PV1.StakingCredential
--     ]
--   PV1.Certifying dCert -> Aeson.object
--     [ "kind"  .= String "Certifying"
--     , "value" .= do dCert :: PV1.DCert
--     ]

-- scriptHashOf :: Alonzo.Language -> Short.ShortByteString -> Text
-- scriptHashOf lang sbs = Text.pack $ Hash.hashToStringAsHex h
--   where Ledger.ScriptHash h = case lang of
--           Alonzo.PlutusV1 -> Ledger.hashScript @Consensus.StandardAlonzo (Ledger.PlutusScript lang sbs)
--           Alonzo.PlutusV2 -> error "not implemented"

-- instance ToJSON Plutus.EvaluationError where
--   toJSON = \case
--     Plutus.CekError e -> object
--       [ "kind"    .= String "CekError"
--       , "error"   .= do show e  :: Text
--       , "value"   .= do e       :: (Cek.ErrorWithCause
--                                       (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError PlutusCore.DefaultFun))
--                                       (UntypedPlutusCore.Core.Type.Term
--                                         PlutusCore.DeBruijn.NamedDeBruijn
--                                         PlutusCore.DefaultUni
--                                         PlutusCore.DefaultFun
--                                         ()))
--       ]
--     Plutus.DeBruijnError e -> object
--       [ "kind"    .= String "DeBruijnError"
--       , "error"   .= do show e  :: Text
--       ]
--     Plutus.CodecError e -> object
--       [ "kind"    .= String "CodecError"
--       , "error"   .= do show e  :: Text
--       ]
--     Plutus.IncompatibleVersionError actual -> object
--       [ "kind"    .= String "IncompatibleVersionError"
--       , "actual"  .= do actual  :: UntypedPlutusCore.Core.Type.Version ()
--       ]
--     Plutus.CostModelParameterMismatch -> object
--       [ "kind"    .= String "CostModelParameterMismatch"
--       ]

-- instance ToJSON (Plutus.Version ann) where
--   toJSON (Plutus.Version _ i j k) = object
--     [ "i" .= do i :: Natural
--     , "j" .= do j :: Natural
--     , "k" .= do k :: Natural
--     ]

-- instance ToJSON Plutus.Data where
--   toJSON = \case
--     Plutus.Constr t as -> object
--       [ "Constr" .= do toJSON (t :: Integer):fmap toJSON (as :: [Plutus.Data]) :: [Value]
--       ]
--     Plutus.Map es -> object
--       [ "Map" .= do fmap dataEntryToJson es :: [Value]
--       ]
--     Plutus.List es  -> toJSON do es :: [Plutus.Data]
--     Plutus.I n      -> toJSON do n :: Integer
--     Plutus.B bs     -> toJSON do Text.decodeLatin1 (B16.encode bs) :: Text

-- dataEntryToJson :: (Plutus.Data, Plutus.Data) -> Value
-- dataEntryToJson (k, v) = toJSON [toJSON k, toJSON v]

-- instance ToJSON Cek.CekUserError where
--   toJSON = \case
--     Cek.CekOutOfExError (Cek.ExRestrictingBudget res) -> object
--       [ "kind"    .= String "CekOutOfExError"
--       , "budget"  .= do res :: Cek.ExBudget
--       ]
--     Cek.CekEvaluationFailure -> object
--       [ "kind"  .= String "CekEvaluationFailure"
--       ]

-- instance (ToJSON name, ToJSON fun) => ToJSON (Cek.CekEvaluationException name uni fun) where

-- instance (ToJSON name, ToJSON fun) => ToJSON (UntypedPlutusCore.Core.Type.Term name uni fun ann) where
--   toJSON = \case
--     UntypedPlutusCore.Core.Type.Var {} -> Aeson.object
--       [ "kind" .= String "Var"
--       ]
--     UntypedPlutusCore.Core.Type.LamAbs {} -> Aeson.object
--       [ "kind" .= String "LamAbs"
--       ]
--     UntypedPlutusCore.Core.Type.Apply {} -> Aeson.object
--       [ "kind" .= String "Apply"
--       ]
--     UntypedPlutusCore.Core.Type.Force {} -> Aeson.object
--       [ "kind" .= String "Force"
--       ]
--     UntypedPlutusCore.Core.Type.Delay {} -> Aeson.object
--       [ "kind" .= String "Delay"
--       ]
--     UntypedPlutusCore.Core.Type.Constant {} -> Aeson.object
--       [ "kind" .= String "Constant"
--       ]
--     UntypedPlutusCore.Core.Type.Builtin {} -> Aeson.object
--       [ "kind" .= String "Builtin"
--       ]
--     UntypedPlutusCore.Core.Type.Error {} -> Aeson.object
--       [ "kind" .= String "Error"
--       ]

-- -- Used by ToJSON (Cek.CekEvaluationException name uni fun)
-- instance ToJSON fun => ToJSON (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError fun)) where
--   toJSON = \case
--     PlutusCore.InternalEvaluationError internal -> object
--       [ "InternalEvaluationError" .= do internal  :: PlutusCore.MachineError fun
--       ]
--     PlutusCore.UserEvaluationError user -> object
--       [ "UserEvaluationError"     .= do user      :: Cek.CekUserError
--       ]

-- instance ToJSON PlutusCore.NamedDeBruijn where

-- instance ToJSON PlutusCore.DeBruijn.Index where

-- instance ToJSON PlutusCore.DefaultFun where

-- instance (forall a. ToJSON (f a)) => ToJSON (PlutusCore.Some f) where
--   toJSON (PlutusCore.Some a) = object
--     [ "kind"  .= String "Some"
--     , "value" .= do a
--     ]

-- instance (ToJSON (uni (PlutusCore.Esc a)), ToJSON a) => ToJSON (PlutusCore.ValueOf uni a) where
--   toJSON (PlutusCore.ValueOf u a) = object
--     [ "kind"  .= String "ValueOf"
--     , "uni"   .= do u :: uni (PlutusCore.Esc a)
--     , "a"     .= do a :: a
--     ]

-- instance ToJSON fun => ToJSON (PlutusCore.MachineError fun) where
--   toJSON = \case
--     PlutusCore.NonPolymorphicInstantiationMachineError -> "NonPolymorphicInstantiationMachineError"
--     PlutusCore.NonWrapUnwrappedMachineError -> "NonWrapUnwrappedMachineError"
--     PlutusCore.NonFunctionalApplicationMachineError -> "NonFunctionalApplicationMachineError"
--     PlutusCore.OpenTermEvaluatedMachineError -> "OpenTermEvaluatedMachineError"
--     PlutusCore.UnliftingMachineError (PlutusCore.UnliftingErrorE t) -> object
--       [ "UnliftingMachineError" .= object
--         [ "UnliftingError" .= do t :: Text
--         ]
--       ]
--     PlutusCore.BuiltinTermArgumentExpectedMachineError -> "BuiltinTermArgumentExpectedMachineError"
--     PlutusCore.UnexpectedBuiltinTermArgumentMachineError -> "UnexpectedBuiltinTermArgumentMachineError"
--     PlutusCore.EmptyBuiltinArityMachineError -> "EmptyBuiltinArityMachineError"
--     PlutusCore.UnknownBuiltin fun -> object
--       [ "UnknownBuiltin" .= do fun :: fun
--       ]

_textShow :: Show a => a -> Text
_textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =
  case withOriginToMaybe wOblk of
    Nothing -> "Genesis Block"
    Just blk -> textShow . unBlockNo $ labBlockNo blk

-- The following instances aren't used above

instance ToJSON Alonzo.TagMismatchDescription where
  toJSON = \case
    Alonzo.PassedUnexpectedly -> object
      [ "kind"  .= toJSON @String "TagMismatchDescription"
      , "error" .= toJSON @String "PassedUnexpectedly"
      ]
    Alonzo.FailedUnexpectedly forReasons -> object
      [ "kind"            .= toJSON @String "TagMismatchDescription"
      , "error"           .= toJSON @String "FailedUnexpectedly"
      , "reconstruction"  .= do NEL.toList forReasons :: [Alonzo.FailureDescription]
      ]

renderBadInputsUTxOErr ::  Set (Ledger.TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = toJSON @String "The transaction contains no inputs."
  | otherwise = toJSON @String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = toJSON @String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance ToJSON TicknPredicateFailure where
  toJSON x = case x of {} -- no constructors

instance ToJSON (ShelleyPoolreapPredFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (ShelleySnapPredFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (ShelleyMirPredFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (ShelleyRupdPredFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (UpdnPredicateFailure crypto) where
  toJSON x = case x of {} -- no constructors
