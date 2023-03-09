{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}

module Test.Cli.Babbage.LeadershipSchedule
  ( hprop_leadershipSchedule
  ) where

import           Cardano.CLI.Shelley.Output (QueryTipLocalStateOutput (..))
import           Control.Monad (void)
import           Data.List ((\\))
import           Data.Monoid (Last (..))
import           GHC.Stack (callStack)
import           Hedgehog (Property)
import           Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Info as SYS
import qualified Testnet.Util.Base as H

import           Cardano.Testnet
import           Testnet.Util.Assert
import           Testnet.Util.Process
import           Testnet.Util.Runtime

hprop_leadershipSchedule :: Property
hprop_leadershipSchedule = H.integrationRetryWorkspace 2 "babbage-leadership-schedule" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@Conf { tempAbsPath } <- H.noteShowM $
    mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  work <- H.note $ tempAbsPath </> "work"
  H.createDirectoryIfMissing work

  let
    tempBaseAbsPath = getTmpBaseAbsPath (TmpPath tempAbsPath)
    testnetOptions = BabbageOnlyTestnetOptions $ babbageDefaultTestnetOptions
      { babbageNodeLoggingFormat = NodeLoggingFormatAsJson
      }
  tr@TestnetRuntime
    { testnetMagic
    , poolNodes
    -- , wallets
    -- , delegators
    } <- testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes

  env <- H.evalIO getEnvironment

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  tipDeadline <- H.noteShowM $ DTC.addUTCTime 210 <$> H.noteShowIO DTC.getCurrentTime

  H.byDeadlineM 10 tipDeadline "Wait for two epochs" $ do
    void $ execCli' execConfig
      [ "query", "tip"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "current-tip.json"
      ]

    tipJson <- H.leftFailM . H.readJsonFile $ work </> "current-tip.json"
    tip <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @QueryTipLocalStateOutput tipJson

    currEpoch <- case mEpoch tip of
      Nothing -> H.failMessage callStack "cardano-cli query tip returned Nothing for EpochNo"
      Just currEpoch -> return currEpoch

    H.note_ $ "Current Epoch: " <> show currEpoch
    H.assert $ currEpoch > 2

  stakePoolId <- filter ( /= '\n') <$> execCli
    [ "stake-pool", "id"
    , "--cold-verification-key-file", poolNodeKeysColdVkey $ poolKeys poolNode1
    ]

  let poolVrfSkey = poolNodeKeysVrfSkey $ poolKeys poolNode1

  id do
    scheduleFile <- H.noteTempFile tempAbsPath "schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

    H.byDeadlineM 5 leadershipScheduleDeadline "Failed to query for leadership schedule" $ do
      void $ execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolId
        , "--vrf-signing-key-file", poolVrfSkey
        , "--out-file", scheduleFile
        , "--current"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile scheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson

    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    leaderSlots <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
      someLeaderSlots <- getRelevantLeaderSlots (poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)
      if L.null someLeaderSlots
        then H.failure
        else do
          maxActualSlot <- H.noteShow $ maximum someLeaderSlots
          H.assert $ maxActualSlot >= maxSlotExpected
          pure someLeaderSlots

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)

  id do
    scheduleFile <- H.noteTempFile tempAbsPath "schedule.log"

    leadershipScheduleDeadline <- H.noteShowM $ DTC.addUTCTime 180 <$> H.noteShowIO DTC.getCurrentTime

    H.byDeadlineM 5 leadershipScheduleDeadline "Failed to query for leadership schedule" $ do
      void $ execCli' execConfig
        [ "query", "leadership-schedule"
        , "--testnet-magic", show @Int testnetMagic
        , "--genesis", shelleyGenesisFile tr
        , "--stake-pool-id", stakePoolId
        , "--vrf-signing-key-file", poolVrfSkey
        , "--out-file", scheduleFile
        , "--next"
        ]

    scheduleJson <- H.leftFailM $ H.readJsonFile scheduleFile

    expectedLeadershipSlotNumbers <- H.noteShowM $ fmap (fmap slotNumber) $ H.leftFail $ J.parseEither (J.parseJSON @[LeadershipSlot]) scheduleJson
    maxSlotExpected <- H.noteShow $ maximum expectedLeadershipSlotNumbers

    H.assert $ not (L.null expectedLeadershipSlotNumbers)

    leadershipDeadline <- H.noteShowM $ DTC.addUTCTime 90 <$> H.noteShowIO DTC.getCurrentTime

    -- We need enough time to pass such that the expected leadership slots generated by the
    -- leadership-schedule command have actually occurred.
    leaderSlots <- H.byDeadlineM 10 leadershipDeadline "Wait for chain to surpass all expected leadership slots" $ do
      someLeaderSlots <- getRelevantLeaderSlots (poolNodeStdout poolNode1) (minimum expectedLeadershipSlotNumbers)
      if L.null someLeaderSlots
        then H.failure
        else do
          maxActualSlot <- H.noteShow $ maximum someLeaderSlots
          H.assert $ maxActualSlot >= maxSlotExpected
      pure someLeaderSlots

    H.noteShow_ expectedLeadershipSlotNumbers
    H.noteShow_ leaderSlots

    -- As there are no BFT nodes, the next leadership schedule should match slots assigned exactly
    H.assert $ L.null (expectedLeadershipSlotNumbers \\ leaderSlots)
