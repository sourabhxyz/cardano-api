{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Internal.LedgerEvents.Rule.BBODY.LEDGER
  ( LatestBBodyEventConstraints
  , handleShelleyLEDGEREvents
  )
where

import Cardano.Api.Internal.LedgerEvents.LedgerEvent
import Cardano.Api.Internal.LedgerEvents.Rule.BBODY.DELEGS
import Cardano.Api.Internal.LedgerEvents.Rule.BBODY.UTXOW

import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoEvent (..)
  , AlonzoUtxosEvent (..)
  , AlonzoUtxowEvent (..)
  )
import Cardano.Ledger.Alonzo.Rules qualified as Alonzo
import Cardano.Ledger.Core qualified as Ledger.Core
import Cardano.Ledger.Crypto qualified as Crypto
import Cardano.Ledger.Shelley.Rules qualified as Shelley

import Control.State.Transition.Extended

type LatestBBodyEventConstraints ledgerera =
  ( Event (Ledger.Core.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
  , Event (Ledger.Core.EraRule "LEDGERS" ledgerera) ~ Shelley.ShelleyLedgersEvent ledgerera
  , Event (Ledger.Core.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  , Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  , Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  , Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  )

handleShelleyLEDGEREvents
  :: Event (Ledger.Core.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  => Event (Ledger.Core.EraRule "UTXOS" ledgerera) ~ AlonzoUtxosEvent ledgerera
  => Event (Ledger.Core.EraRule "DELEGS" ledgerera) ~ Shelley.ShelleyDelegsEvent ledgerera
  => Ledger.Core.EraCrypto ledgerera ~ Crypto.StandardCrypto
  => Shelley.ShelleyLedgerEvent ledgerera -> Maybe LedgerEvent
handleShelleyLEDGEREvents ledgerEvent =
  case ledgerEvent of
    Shelley.UtxowEvent e -> handleAlonzoUTxOWEvent e
    Shelley.DelegsEvent e -> handleShelleyDELEGSEvent e
