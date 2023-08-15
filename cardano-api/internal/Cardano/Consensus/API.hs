{-# LANGUAGE PatternSynonyms #-}

module Cardano.Consensus.API (
  -- * Eras
    CardanoEras
  , TriggerHardFork (..)
  , eraParams
  -- ** Shelley
  , ShelleyBasedEra
  , ShelleyCompatible
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  -- *** Standard
  , StandardShelley
  , StandardAllegra
  , StandardMary
  , StandardAlonzo
  , StandardBabbage
  , StandardConway

  -- * Blocks
  , Point
  , blockPrevHash
  -- ** Byron
  , ByronBlock (..)
  , ByronBlockHFC
  , pattern BlockByron
  -- ** Shelley
  , ShelleyBlock (..)
  , ShelleyBlockHFC
  , pattern BlockShelley
  , pattern BlockAllegra
  , pattern BlockMary
  , pattern BlockAlonzo
  , pattern BlockBabbage
  , pattern BlockConway
  -- *** Standard
  , StandardShelleyBlock
  , StandardAllegraBlock
  , StandardMaryBlock
  , StandardAlonzoBlock
  , StandardBabbageBlock
  , StandardConwayBlock
  -- ** HardFork
  , HardForkBlock
  , pattern DegenBlock
  -- ** Cardano
  , CardanoBlock

  -- * Protocols
  , LedgerSupportsProtocol
  , ProtocolInfo (..)
  , ProtocolClientInfo (..)
  , ShelleyProtocolHeader
  , ChainDepState
  -- ** Hard Fork
  , HardForkProtocol
  -- ** PBft
  , PBftSignatureThreshold (..)
  -- ** TPraos
  , TPraos
  , TPraosState (..)
  -- ** Praos
  , Praos
  , PraosState (..)
  , PraosNonces (..)
  , getPraosNonces
  , Nonce
  , mkInputVRF
  , vrfLeaderValue
  -- ** ProtocolParams
  , ProtocolParamsByron (..)
  , ProtocolParamsShelleyBased (..)
  , ProtocolParamsShelley (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsMary (..)
  , ProtocolParamsAlonzo (..)
  , ProtocolParamsBabbage (..)
  , ProtocolParamsConway (..)
  -- ** Protocol Info
  , protocolInfoByron
  , protocolInfoCardano
  , protocolInfoShelley
  , protocolClientInfoByron
  , protocolClientInfoCardano
  , protocolClientInfoShelley
  -- ** Protocol transitions
  , ProtocolTransitionParamsShelleyBased (..)
  -- ** Protocol Supports
  , PraosProtocolSupportsNode
  , PraosProtocolSupportsNodeCrypto
  -- * Block Forging
  , BlockForging
  , blockForgingByron

  -- * Transactions
  , GenTx (..)
  , OneEraGenTx (..)
  , mkShelleyTx
  -- ** TxId
  , byronIdTx
  , TxId (..)
  , OneEraGenTxId (..)
  , WrapGenTxId (..)
  -- ** Apply Transaction Error
  , ApplyTxErr
  , pattern DegenApplyTxErr
  , pattern ApplyTxErrByron
  , pattern ApplyTxErrShelley
  , pattern ApplyTxErrAllegra
  , pattern ApplyTxErrMary
  , pattern ApplyTxErrAlonzo
  , pattern ApplyTxErrBabbage
  , pattern ApplyTxErrConway
  , pattern ApplyTxErrWrongEra

  -- * Hashes
  , OneEraHash (..)
  , ByronHash (..)
  , ShelleyHash (..)
  , ConvertRawHash (..)

  -- * Queries
  , Query (..)
  , ShowQuery
  , BlockQuery (..)
  , QueryHardFork (..)
  , HardForkQueryResult
  , pattern DegenQueryResult
  -- ** Interpret query
  , Interpreter
  , interpretQuery
  -- *** Time conversions
  , slotToWallclock
  , wallclockToSlot
  , interpreterToEpochInfo
  , slotToEpoch
  , PastHorizonException (..)
  , SafeZone (..)
  , eraSafeZone
  -- ** Snapshots
  , StakeSnapshots(..)
  , StakeSnapshot (..)
  -- ** Particular queries
  , getCompactGenesis

  -- * Serialization
  -- ** Codecs
  , Codecs
  , Codecs' (..)
  , clientCodecs
  , CodecConfig
  -- ** Utilities
  , encodeTelescope
  , encodeShelleyLedgerState
  , encodeByronLedgerState
  , decodeTelescope
  , decodeShelleyLedgerState
  , decodeByronLedgerState

  -- * Configurations
  -- ** Ledger
  , WrapPartialLedgerConfig (..)
  , ByronPartialLedgerConfig (..)
  , PerEraLedgerConfig (..)
  , HardForkLedgerConfig (..)
  , topLevelConfigLedger
  -- ** Protocol
  , topLevelConfigProtocol
  -- ** Consensus
  , SecurityParam (..)
  , ConsensusConfig (..)

  -- * Ledger State
  , LedgerState (..)
  , ledgerState
  , ledgerTipHash
  , ledgerTipSlot

  -- * Mempool
  , mkOverrides
  , noOverridesMeasure

  -- * Block application
  -- ** Ledger result
  , LedgerResult (..)
  -- ** Ledger event
  , WrapLedgerEvent (..)
  , AuxLedgerEvent
  , ShelleyLedgerEvent (..)
  , OneEraLedgerEvent (..)
  -- ** API
  , tickThenReapplyLedgerResult
  , tickThenApplyLedgerResult
  -- ** Ledger error
  , HardForkLedgerError

  -- * Versions
  , supportedNodeToClientVersions
  , BlockNodeToClientVersion

  -- * Era indices and mismatches
  , EraIndex (..)
  , eraIndexZero
  , eraIndexSucc
  , MismatchEraInfo
  , EraMismatch (..)
  , mkEraMismatch
  , inject

  -- * Run
  , RunNode

  -- * Util
  , IOLike
  , CardanoHardForkConstraints
  ) where

import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Byron.Ledger.Ledger
import Ouroboros.Consensus.Byron.Ledger.Mempool
import Ouroboros.Consensus.Byron.Node
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.ByronHFC
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Degenerate
import Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import Ouroboros.Consensus.HardFork.Combinator.Mempool
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import Ouroboros.Consensus.HardFork.History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Network.NodeToClient
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PBFT
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Protocol.Praos.VRF
import Ouroboros.Consensus.Protocol.TPraos
import Ouroboros.Consensus.Shelley.HFEras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
import Ouroboros.Consensus.Shelley.Node.Praos
import Ouroboros.Consensus.Shelley.Protocol.Abstract
import Ouroboros.Consensus.Shelley.ShelleyHFC
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IOLike
