{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Internal.Eon.BabbageEraOnwards
  ( BabbageEraOnwards (..)
  , babbageEraOnwardsConstraints
  , babbageEraOnwardsToShelleyBasedEra
  , BabbageEraOnwardsConstraints
  , IsBabbageBasedEra (..)
  )
where

import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Query.Types

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.UTxO qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.SafeHash qualified as L
import Cardano.Ledger.UTxO qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Typeable (Typeable)

data BabbageEraOnwards era where
  BabbageEraOnwardsBabbage :: BabbageEraOnwards BabbageEra
  BabbageEraOnwardsConway :: BabbageEraOnwards ConwayEra

deriving instance Show (BabbageEraOnwards era)

deriving instance Eq (BabbageEraOnwards era)

instance Eon BabbageEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> no
    MaryEra -> no
    AlonzoEra -> no
    BabbageEra -> yes BabbageEraOnwardsBabbage
    ConwayEra -> yes BabbageEraOnwardsConway

instance ToCardanoEra BabbageEraOnwards where
  toCardanoEra = \case
    BabbageEraOnwardsBabbage -> BabbageEra
    BabbageEraOnwardsConway -> ConwayEra

instance Convert BabbageEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert BabbageEraOnwards ShelleyBasedEra where
  convert = \case
    BabbageEraOnwardsBabbage -> ShelleyBasedEraBabbage
    BabbageEraOnwardsConway -> ShelleyBasedEraConway

instance Convert BabbageEraOnwards MaryEraOnwards where
  convert = \case
    BabbageEraOnwardsBabbage -> MaryEraOnwardsBabbage
    BabbageEraOnwardsConway -> MaryEraOnwardsConway

instance Convert BabbageEraOnwards AlonzoEraOnwards where
  convert = \case
    BabbageEraOnwardsBabbage -> AlonzoEraOnwardsBabbage
    BabbageEraOnwardsConway -> AlonzoEraOnwardsConway

type BabbageEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraTxOut (ShelleyLedgerEra era)
  , L.BabbageEraPParams (ShelleyLedgerEra era)
  , L.BabbageEraTxBody (ShelleyLedgerEra era)
  , L.BabbageEraTxOut (ShelleyLedgerEra era)
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxOut (ShelleyLedgerEra era) ~ L.BabbageTxOut (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue L.StandardCrypto
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

babbageEraOnwardsConstraints
  :: ()
  => BabbageEraOnwards era
  -> (BabbageEraOnwardsConstraints era => a)
  -> a
babbageEraOnwardsConstraints = \case
  BabbageEraOnwardsBabbage -> id
  BabbageEraOnwardsConway -> id

{-# DEPRECATED babbageEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
babbageEraOnwardsToShelleyBasedEra :: BabbageEraOnwards era -> ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = convert

class IsAlonzoBasedEra era => IsBabbageBasedEra era where
  babbageBasedEra :: BabbageEraOnwards era

instance IsBabbageBasedEra BabbageEra where
  babbageBasedEra = BabbageEraOnwardsBabbage

instance IsBabbageBasedEra ConwayEra where
  babbageBasedEra = BabbageEraOnwardsConway
