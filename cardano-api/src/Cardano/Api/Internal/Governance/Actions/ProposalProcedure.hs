{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Governance.Actions.ProposalProcedure where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.ProtocolParameters
import Cardano.Api.Internal.ReexposeLedger qualified as Ledger
import Cardano.Api.Internal.SerialiseCBOR
import Cardano.Api.Internal.SerialiseTextEnvelope
import Cardano.Api.Internal.TxIn

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Governance qualified as Gov
import Cardano.Ledger.Conway.Governance qualified as Ledger
import Cardano.Ledger.Core (EraCrypto)
import Cardano.Ledger.Core qualified as Shelley
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole))

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Word
import GHC.Exts (IsList (..))

data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovAction era)

-- TODO: Conway - Transitiion to Ledger.GovAction
data GovernanceAction era
  = MotionOfNoConfidence
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (ShelleyLedgerEra era)))
  | ProposeNewConstitution
      (StrictMaybe (Ledger.GovPurposeId Ledger.ConstitutionPurpose (ShelleyLedgerEra era)))
      (Ledger.Anchor StandardCrypto)
      (StrictMaybe (Shelley.ScriptHash StandardCrypto))
  | ProposeNewCommittee
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (ShelleyLedgerEra era)))
      [L.Credential ColdCommitteeRole StandardCrypto]
      -- ^ Old constitutional committee
      (Map (L.Credential ColdCommitteeRole StandardCrypto) EpochNo)
      -- ^ New committee members with epoch number when each of them expires
      Rational
      -- ^ Quorum of the committee that is necessary for a successful vote
  | InfoAct
  | -- | Governance policy
    TreasuryWithdrawal
      [(Network, StakeCredential, L.Coin)]
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto))
  | InitiateHardfork
      (StrictMaybe (Ledger.GovPurposeId Ledger.HardForkPurpose (ShelleyLedgerEra era)))
      ProtVer
  | -- | Governance policy
    UpdatePParams
      (StrictMaybe (Ledger.GovPurposeId Ledger.PParamUpdatePurpose (ShelleyLedgerEra era)))
      (Ledger.PParamsUpdate (ShelleyLedgerEra era))
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto))

toGovernanceAction
  :: ()
  => ShelleyBasedEra era
  -> GovernanceAction era
  -> Gov.GovAction (ShelleyLedgerEra era)
toGovernanceAction sbe =
  shelleyBasedEraConstraints sbe $ \case
    MotionOfNoConfidence prevGovId ->
      Gov.NoConfidence prevGovId
    ProposeNewConstitution prevGovAction anchor mConstitutionScriptHash ->
      Gov.NewConstitution
        prevGovAction
        Gov.Constitution
          { Gov.constitutionAnchor = anchor
          , Gov.constitutionScript = mConstitutionScriptHash
          }
    ProposeNewCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
      Gov.UpdateCommittee
        prevGovId -- previous governance action id
        (fromList oldCommitteeMembers) -- members to remove
        newCommitteeMembers -- members to add
        ( fromMaybe
            ( error $
                mconcat
                  [ "toGovernanceAction: the given quorum "
                  , show quor
                  , " was outside of the unit interval!"
                  ]
            )
            $ boundRational @UnitInterval quor
        )
    InfoAct ->
      Gov.InfoAction
    TreasuryWithdrawal withdrawals govPol ->
      let m = fromList [(L.RewardAccount nw (toShelleyStakeCredential sc), l) | (nw, sc, l) <- withdrawals]
       in Gov.TreasuryWithdrawals m govPol
    InitiateHardfork prevGovId pVer ->
      Gov.HardForkInitiation prevGovId pVer
    UpdatePParams preGovId ppup govPol ->
      Gov.ParameterChange preGovId ppup govPol

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Gov.GovAction (ShelleyLedgerEra era)
  -> GovernanceAction era
fromGovernanceAction = \case
  Gov.NoConfidence prevGovId ->
    MotionOfNoConfidence prevGovId
  Gov.NewConstitution prevGovId constitution ->
    ProposeNewConstitution
      prevGovId
      (Gov.constitutionAnchor constitution)
      (Gov.constitutionScript constitution)
  Gov.ParameterChange prevGovId pparams govPolicy ->
    UpdatePParams prevGovId pparams govPolicy
  Gov.HardForkInitiation prevGovId pVer ->
    InitiateHardfork prevGovId pVer
  Gov.TreasuryWithdrawals withdrawlMap govPolicy ->
    let res =
          [ (L.raNetwork rwdAcnt, fromShelleyStakeCredential (L.raCredential rwdAcnt), coin)
          | (rwdAcnt, coin) <- toList withdrawlMap
          ]
     in TreasuryWithdrawal res govPolicy
  Gov.UpdateCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
    ProposeNewCommittee
      prevGovId
      (toList oldCommitteeMembers)
      newCommitteeMembers
      (unboundRational quor)
  Gov.InfoAction ->
    InfoAct

newtype Proposal era = Proposal {unProposal :: Gov.ProposalProcedure (ShelleyLedgerEra era)}

instance IsShelleyBasedEra era => Show (Proposal era) where
  show (Proposal pp) = do
    let ppStr = shelleyBasedEraConstraints (shelleyBasedEra @era) $ show pp
    "Proposal {unProposal = " <> ppStr <> "}"

instance IsShelleyBasedEra era => Eq (Proposal era) where
  (Proposal pp1) == (Proposal pp2) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ pp1 == pp2

instance IsShelleyBasedEra era => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ Shelley.toEraCBOR @Conway.Conway vp

instance IsShelleyBasedEra era => FromCBOR (Proposal era) where
  fromCBOR =
    Proposal <$> shelleyBasedEraConstraints (shelleyBasedEra @era) (Shelley.fromEraCBOR @Conway.Conway)

instance IsShelleyBasedEra era => SerialiseAsCBOR (Proposal era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (Proposal era) where
  textEnvelopeType _ = "Governance proposal"

instance HasTypeProxy era => HasTypeProxy (Proposal era) where
  data AsType (Proposal era) = AsProposal
  proxyToAsType _ = AsProposal

createProposalProcedure
  :: ShelleyBasedEra era
  -> Network
  -> L.Coin
  -- ^ Deposit
  -> StakeCredential
  -- ^ Credential to return the deposit to.
  -> GovernanceAction era
  -> Ledger.Anchor StandardCrypto
  -> Proposal era
createProposalProcedure sbe nw dep cred govAct anchor =
  shelleyBasedEraConstraints sbe $
    Proposal
      Gov.ProposalProcedure
        { Gov.pProcDeposit = dep
        , Gov.pProcReturnAddr = L.RewardAccount nw $ toShelleyStakeCredential cred
        , Gov.pProcGovAction = toGovernanceAction sbe govAct
        , Gov.pProcAnchor = anchor
        }

fromProposalProcedure
  :: ShelleyBasedEra era
  -> Proposal era
  -> (L.Coin, StakeCredential, GovernanceAction era)
fromProposalProcedure sbe (Proposal pp) =
  shelleyBasedEraConstraints
    sbe
    ( Gov.pProcDeposit pp
    , fromShelleyStakeCredential (L.raCredential (Gov.pProcReturnAddr pp))
    , fromGovernanceAction (Gov.pProcGovAction pp)
    )

createPreviousGovernanceActionId
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => TxId
  -> Word16
  -- ^ Governance action transation index
  -> Ledger.GovPurposeId (r :: Ledger.GovActionPurpose) (ShelleyLedgerEra era)
createPreviousGovernanceActionId txid index =
  Ledger.GovPurposeId $ createGovernanceActionId txid index

createGovernanceActionId :: TxId -> Word16 -> Gov.GovActionId StandardCrypto
createGovernanceActionId txid index =
  Ledger.GovActionId
    { Ledger.gaidTxId = toShelleyTxId txid
    , Ledger.gaidGovActionIx = Ledger.GovActionIx index
    }

createAnchor :: Url -> ByteString -> Anchor StandardCrypto
createAnchor url anchorData =
  Ledger.Anchor
    { anchorUrl = url
    , anchorDataHash = hashAnchorData $ Ledger.AnchorData anchorData
    }

-- | Get anchor data url and hash from a governance action. A return value of `Nothing`
-- means that the governance action does not contain anchor data.
getAnchorDataFromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Gov.GovAction (ShelleyLedgerEra era)
  -> Maybe (Ledger.Anchor StandardCrypto)
getAnchorDataFromGovernanceAction govAction =
  case govAction of
    Gov.ParameterChange{} -> Nothing
    Gov.HardForkInitiation _ _ -> Nothing
    Gov.TreasuryWithdrawals _ _ -> Nothing
    Gov.NoConfidence _ -> Nothing
    Gov.UpdateCommittee{} -> Nothing
    Gov.NewConstitution _ constitution -> Just $ Ledger.constitutionAnchor constitution
    Gov.InfoAction -> Nothing
