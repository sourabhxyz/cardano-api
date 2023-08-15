{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Domain.ProtocolParametersUpdate
  ( emptyProtocolParametersUpdate
  , protocolUpdateProtocolVersion
  , protocolUpdateDecentralization
  , protocolUpdateExtraPraosEntropy
  , protocolUpdateMaxBlockHeaderSize
  , protocolUpdateMaxBlockBodySize
  , protocolUpdateMaxTxSize
  , protocolUpdateTxFeeFixed
  , protocolUpdateTxFeePerByte
  , protocolUpdateMinUTxOValue
  , protocolUpdateStakeAddressDeposit
  , protocolUpdateStakePoolDeposit
  , protocolUpdateMinPoolCost
  , protocolUpdatePoolRetireMaxEpoch
  , protocolUpdateStakePoolTargetNum
  , protocolUpdatePoolPledgeInfluence
  , protocolUpdateMonetaryExpansion
  , protocolUpdateTreasuryCut
  , protocolUpdateUTxOCostPerWord
  , protocolUpdateCostModels
  , protocolUpdatePrices
  , protocolUpdateMaxTxExUnits
  , protocolUpdateMaxBlockExUnits
  , protocolUpdateMaxValueSize
  , protocolUpdateCollateralPercent
  , protocolUpdateMaxCollateralInputs
  , protocolUpdateUTxOCostPerByte
  ) where

import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
import           Cardano.Api.Feature.AlonzoEraOnly
import           Cardano.Api.Feature.AlonzoEraOnwards
import           Cardano.Api.Feature.BabbageEraOnwards
import           Cardano.Api.Feature.ShelleyToAllegraEra
import           Cardano.Api.Feature.ShelleyToAlonzoEra

import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger

import           GHC.Natural (Natural)
import           Lens.Micro (Lens')

emptyProtocolParametersUpdate :: ShelleyBasedEra era -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
emptyProtocolParametersUpdate w = shelleyBasedEraConstraints w Ledger.emptyPParamsUpdate

-- | Protocol version, major and minor. Updating the major version is used to
-- trigger hard forks.
protocolUpdateProtocolVersion :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.ProtVer)
protocolUpdateProtocolVersion w = shelleyBasedEraConstraints w Ledger.ppuProtocolVersionL

-- | The decentralization parameter. This is fraction of slots that belong to
-- the BFT overlay schedule, rather than the Praos schedule. So 1 means fully
-- centralised, while 0 means fully decentralised.
--
-- This is the \"d\" parameter from the design document.
protocolUpdateDecentralization :: ShelleyToAlonzoEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.UnitInterval)
protocolUpdateDecentralization w = shelleyToAlonzoEraConstraints w Ledger.ppuDL

-- | Extra entropy for the Praos per-epoch nonce.
--
-- This can be used to add extra entropy during the decentralisation process.
-- If the extra entropy can be demonstrated to be generated randomly then this
-- method can be used to show that the initial federated operators did not
-- subtly bias the initial schedule so that they retain undue influence after
-- decentralisation.
protocolUpdateExtraPraosEntropy :: ShelleyToAlonzoEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Nonce)
protocolUpdateExtraPraosEntropy w = shelleyToAlonzoEraConstraints w Ledger.ppuExtraEntropyL

-- | The maximum permitted size of a block header.
--
-- This must be at least as big as the largest legitimate block headers but
-- should not be too much larger, to help prevent DoS attacks.
--
-- Caution: setting this to be smaller than legitimate block headers is a sure
-- way to brick the system!
protocolUpdateMaxBlockHeaderSize :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateMaxBlockHeaderSize w = shelleyBasedEraConstraints w Ledger.ppuMaxBHSizeL

-- | The maximum permitted size of the block body (that is, the block payload,
-- without the block header).
--
-- This should be picked with the Praos network delta security parameter in
-- mind. Making this too large can severely weaken the Praos consensus
-- properties.
--
-- Caution: setting this to be smaller than a transaction that can change the
-- protocol parameters is a sure way to brick the system!
protocolUpdateMaxBlockBodySize :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateMaxBlockBodySize w = shelleyBasedEraConstraints w Ledger.ppuMaxBBSizeL

-- | The maximum permitted size of a transaction.
--
-- Typically this should not be too high a fraction of the block size,
-- otherwise wastage from block fragmentation becomes a problem, and the
-- current implementation does not use any sophisticated box packing
-- algorithm.
protocolUpdateMaxTxSize :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateMaxTxSize w = shelleyBasedEraConstraints w Ledger.ppuMaxTxSizeL

-- | The constant factor for the minimum fee calculation.
protocolUpdateTxFeeFixed :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateTxFeeFixed w = shelleyBasedEraConstraints w Ledger.ppuMinFeeBL

-- | The linear factor for the minimum fee calculation.
protocolUpdateTxFeePerByte :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateTxFeePerByte w = shelleyBasedEraConstraints w Ledger.ppuMinFeeAL

-- | The minimum permitted value for new UTxO entries, ie for transaction
-- outputs.
protocolUpdateMinUTxOValue :: ShelleyToAllegraEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateMinUTxOValue w = shelleyToAllegraEraConstraints w Ledger.ppuMinUTxOValueL

-- | The deposit required to register a stake address.
protocolUpdateStakeAddressDeposit :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateStakeAddressDeposit w = shelleyBasedEraConstraints w Ledger.ppuKeyDepositL

-- | The deposit required to register a stake pool.
protocolUpdateStakePoolDeposit :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateStakePoolDeposit w = shelleyBasedEraConstraints w Ledger.ppuPoolDepositL

-- | The minimum value that stake pools are permitted to declare for their
-- cost parameter.
protocolUpdateMinPoolCost :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Coin)
protocolUpdateMinPoolCost w = shelleyBasedEraConstraints w Ledger.ppuMinPoolCostL

-- | The maximum number of epochs into the future that stake pools are
-- permitted to schedule a retirement.
protocolUpdatePoolRetireMaxEpoch :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.EpochNo)
protocolUpdatePoolRetireMaxEpoch w = shelleyBasedEraConstraints w Ledger.ppuEMaxL

-- | The equilibrium target number of stake pools.
--
-- This is the \"k\" incentives parameter from the design document.
protocolUpdateStakePoolTargetNum :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateStakePoolTargetNum w = shelleyBasedEraConstraints w Ledger.ppuNOptL

-- | The influence of the pledge in stake pool rewards.
--
-- This is the \"a_0\" incentives parameter from the design document.
protocolUpdatePoolPledgeInfluence :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.NonNegativeInterval)
protocolUpdatePoolPledgeInfluence w = shelleyBasedEraConstraints w Ledger.ppuA0L

-- | The monetary expansion rate. This determines the fraction of the reserves
-- that are added to the fee pot each epoch.
--
-- This is the \"rho\" incentives parameter from the design document.
protocolUpdateMonetaryExpansion :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.UnitInterval)
protocolUpdateMonetaryExpansion w = shelleyBasedEraConstraints w Ledger.ppuRhoL

-- | The fraction of the fee pot each epoch that goes to the treasury.
--
-- This is the \"tau\" incentives parameter from the design document.
protocolUpdateTreasuryCut :: ShelleyBasedEra era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.UnitInterval)
protocolUpdateTreasuryCut w = shelleyBasedEraConstraints w Ledger.ppuTauL

-- | Cost in ada per word of UTxO storage.
--
-- /Obsoleted by 'protocolUpdateUTxOCostPerByte'/
protocolUpdateUTxOCostPerWord :: AlonzoEraOnly era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.CoinPerWord)
protocolUpdateUTxOCostPerWord w = alonzoEraOnlyConstraints w Ledger.ppuCoinsPerUTxOWordL

-- | Cost models for script languages that use them.
protocolUpdateCostModels :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.CostModels)
protocolUpdateCostModels w = alonzoEraOnwardsConstraints w Ledger.ppuCostModelsL

-- | Price of execution units for script languages that use them.
protocolUpdatePrices :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.Prices)
protocolUpdatePrices w = alonzoEraOnwardsConstraints w Ledger.ppuPricesL

-- | Max total script execution resources units allowed per tx
protocolUpdateMaxTxExUnits :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.ExUnits)
protocolUpdateMaxTxExUnits w = alonzoEraOnwardsConstraints w Ledger.ppuMaxTxExUnitsL

-- | Max total script execution resources units allowed per block
protocolUpdateMaxBlockExUnits :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.ExUnits)
protocolUpdateMaxBlockExUnits w = alonzoEraOnwardsConstraints w Ledger.ppuMaxBlockExUnitsL

-- | Max size of a 'Value' in a tx output.
protocolUpdateMaxValueSize :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateMaxValueSize w = alonzoEraOnwardsConstraints w Ledger.ppuMaxValSizeL

-- | The percentage of the script contribution to the txfee that must be
-- provided as collateral inputs when including Plutus scripts.
protocolUpdateCollateralPercent :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateCollateralPercent w = alonzoEraOnwardsConstraints w Ledger.ppuCollateralPercentageL

-- | The maximum number of collateral inputs allowed in a transaction.
protocolUpdateMaxCollateralInputs :: AlonzoEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Natural)
protocolUpdateMaxCollateralInputs w = alonzoEraOnwardsConstraints w Ledger.ppuMaxCollateralInputsL

-- | Cost in ada per byte of UTxO storage.
--
-- /Supercedes 'protocolUpdateUTxOCostPerWord'/
protocolUpdateUTxOCostPerByte :: BabbageEraOnwards era -> Lens' (Ledger.PParamsUpdate (ShelleyLedgerEra era)) (Ledger.StrictMaybe Ledger.CoinPerByte)
protocolUpdateUTxOCostPerByte w = babbageEraOnwardsConstraints w Ledger.ppuCoinsPerUTxOByteL
