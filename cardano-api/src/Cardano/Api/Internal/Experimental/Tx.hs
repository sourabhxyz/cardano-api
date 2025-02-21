{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Experimental.Tx
  ( -- * Creating transactions using the new API

    -- |
    -- Both the old and new APIs can be used to create transactions, and
    -- it is possible to transform a transaction from one format to the other
    -- as they share the same representation. However, the focus will shift
    -- towards using the new API, while the old API will be deprecated to ensure
    -- simplicity, closer alignment with the ledger, and easier maintenance.
    --
    -- In both the new and old APIs, constructing a transaction requires creating
    -- a 'TxBodyContent', along with at least one witness (for example, a
    -- 'ShelleyWitnessSigningKey') to sign the transaction.
    -- This process remains unchanged.
    --
    -- To learn how to create a transaction using the old API, see the
    -- "Cardano.Api.Internal.Tx.Body" documentation.
    --
    -- In the examples below, the following qualified modules are used:
    --
    -- @
    -- import qualified Cardano.Api as Api                -- the general `cardano-api` exports (including the old API)
    -- import qualified Cardano.Api.Script as Script      -- types related to scripts (Plutus and native)
    -- import qualified Cardano.Api.Ledger as Ledger      -- cardano-ledger re-exports
    -- import qualified Cardano.Api.Experimental as Exp   -- the experimental API
    -- @
    --
    -- For instructions on how to do this, refer to the @Test.Cardano.Api.Experimental@ documentation.

    -- ** Creating a 'TxBodyContent'

    -- |
    -- Regardless of whether the experimental or the traditional API is used, creating a 'TxBodyContent'
    -- is necessary.
    --
    -- You can see how to do this in the documentation of the "Cardano.Api.Internal.Tx.Body" module.

    -- ** Balancing a transaction

    -- |
    -- If a UTXO has exactly 12 ada, the transaction could be constructed as described in
    -- "Cardano.Api.Internal.Tx.Body", and it would be valid. However:
    --
    --   * Ada may be wasted
    --   * The UTXO that we intend to spend may not contain exactly 12 ada
    --   * The transaction may not be this simple.
    --
    -- For these reasons, it is recommended to balance the transaction before proceeding with
    -- signing and submitting.
    --
    -- For instructions on how to balance a transaction, refer to the "Cardano.Api.Internal.Fees" documentation.

    -- ** Creating a 'ShelleyWitnessSigningKey'

    -- |
    -- Signing a transaction requires a witness, such as a 'ShelleyWitnessSigningKey'.
    --
    -- For instructions on creating a 'ShelleyWitnessSigningKey' refer to the "Cardano.Api.Internal.Tx.Sign" documentation.

    -- ** Creating a transaction using the new API

    -- |
    -- This section outlines how to create a transaction using the new API. First,
    -- create an 'UnsignedTx' using the 'makeUnsignedTx' function and the 'Era' and
    -- 'TxBodyContent' that we defined earlier:
    --
    -- @
    -- let (Right unsignedTx) = Exp.makeUnsignedTx era txBodyContent
    -- @
    --
    -- Next, use the key witness to sign the unsigned transaction with the 'makeKeyWitness' function:
    --
    -- @
    -- let transactionWitness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)
    -- @
    --
    -- Finally, sign the transaction using the 'signTx' function:
    --
    -- @
    -- let newApiSignedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra) = Exp.signTx era [] [transactionWitness] unsignedTx
    -- @
    --
    -- The empty list represents the bootstrap witnesses, which are not needed in this case.
    --
    -- The transaction is now signed.

    -- ** Converting a transaction from the new API to the old API

    -- |
    -- A transaction created with the new API can be easily converted to the old API by
    -- wrapping it with the 'ShelleyTx' constructor:
    --
    -- @
    -- let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe newApiSignedTx
    -- @

    -- ** Inspecting transactions

    -- |
    -- When using a 'Tx' created with the experimental API, the 'TxBody' and
    -- 'TxWits' can be extracted using the 'txBody' and 'txWits' lenses from
    -- "Cardano.Api.Ledger" respectively.

    -- * Contents
    UnsignedTx (..)
  , UnsignedTxError (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , hashTxBody
  )
where

import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Core (ToCardanoEra (toCardanoEra), forEraInEon)
import Cardano.Api.Internal.Experimental.Eras
import Cardano.Api.Internal.Feature
import Cardano.Api.Internal.Pretty (docToString, pretty)
import Cardano.Api.Internal.ReexposeLedger (StrictMaybe (..), maybeToStrictMaybe)
import Cardano.Api.Internal.ReexposeLedger qualified as L
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign

import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage qualified as Ledger
import Cardano.Ledger.Conway qualified as Ledger
import Cardano.Ledger.Conway.TxBody qualified as L
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.SafeHash qualified as L

import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro

-- | A transaction that can contain everything
-- except key witnesses.
newtype UnsignedTx era
  = UnsignedTx (Ledger.Tx (LedgerEra era))

instance IsEra era => Show (UnsignedTx era) where
  showsPrec p (UnsignedTx tx) = case useEra @era of
    BabbageEra -> showsPrec p (tx :: Ledger.Tx Ledger.Babbage)
    ConwayEra -> showsPrec p (tx :: Ledger.Tx Ledger.Conway)

newtype UnsignedTxError
  = UnsignedTxError TxBodyError

makeUnsignedTx
  :: Era era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (UnsignedTx era)
makeUnsignedTx era bc = obtainCommonConstraints era $ do
  let sbe = convert era

  -- cardano-api types
  let apiTxOuts = txOuts bc
      apiScriptWitnesses = collectTxBodyScriptWitnesses sbe bc
      apiScriptValidity = txScriptValidity bc
      apiMintValue = txMintValue bc
      apiProtocolParameters = txProtocolParams bc
      apiCollateralTxIns = txInsCollateral bc
      apiReferenceInputs = txInsReference bc
      apiExtraKeyWitnesses = txExtraKeyWits bc
      apiReturnCollateral = txReturnCollateral bc
      apiTotalCollateral = txTotalCollateral bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns apiCollateralTxIns
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = convTxOuts sbe apiTxOuts
      fee = convTransactionFee sbe $ txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      returnCollateral = convReturnCollateral sbe apiReturnCollateral
      totalCollateral = convTotalCollateral apiTotalCollateral
      certs = convCertificates sbe $ txCertificates bc
      txAuxData = toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc)
      scripts = convScripts apiScriptWitnesses
      languages = convLanguages apiScriptWitnesses
      sData = convScriptData sbe apiTxOuts apiScriptWitnesses
      (datums, redeemers) = case sData of
        TxBodyScriptData _ ds rs -> (ds, rs)
        TxBodyNoScriptData -> (mempty, L.Redeemers mempty)

  let setMint = convMintValue apiMintValue
      setReqSignerHashes = convExtraKeyWitnesses apiExtraKeyWitnesses
      ledgerTxBody =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ txins
          & L.collateralInputsTxBodyL .~ collTxIns
          & L.referenceInputsTxBodyL .~ refTxIns
          & L.outputsTxBodyL .~ outs
          & L.totalCollateralTxBodyL .~ totalCollateral
          & L.collateralReturnTxBodyL .~ returnCollateral
          & L.feeTxBodyL .~ fee
          & L.vldtTxBodyL . L.invalidBeforeL .~ convValidityLowerBound (txValidityLowerBound bc)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ convValidityUpperBound sbe (txValidityUpperBound bc)
          & L.reqSignerHashesTxBodyL .~ setReqSignerHashes
          & L.scriptIntegrityHashTxBodyL .~ getScriptIntegrityHash apiProtocolParameters languages sData
          & L.withdrawalsTxBodyL .~ withdrawals
          & L.certsTxBodyL .~ certs
          & L.mintTxBodyL .~ setMint
          & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData

      scriptWitnesses =
        L.mkBasicTxWits
          & L.scriptTxWitsL
            .~ fromList
              [ (L.hashScript sw, sw)
              | sw <- scripts
              ]
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers

  eraSpecificTxBody <- eraSpecificLedgerTxBody era ledgerTxBody bc

  return . UnsignedTx $
    L.mkBasicTx eraSpecificTxBody
      & L.witsTxL .~ scriptWitnesses
      & L.auxDataTxL .~ maybeToStrictMaybe (toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc))
      & L.isValidTxL .~ txScriptValidityToIsValid apiScriptValidity

eraSpecificLedgerTxBody
  :: Era era
  -> Ledger.TxBody (LedgerEra era)
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (Ledger.TxBody (LedgerEra era))
eraSpecificLedgerTxBody BabbageEra ledgerbody bc = do
  let sbe = convert BabbageEra

  setUpdateProposal <- convTxUpdateProposal sbe (txUpdateProposal bc)

  return $ ledgerbody & L.updateTxBodyL .~ setUpdateProposal
eraSpecificLedgerTxBody ConwayEra ledgerbody bc =
  let propProcedures = txProposalProcedures bc
      voteProcedures = txVotingProcedures bc
      treasuryDonation = txTreasuryDonation bc
      currentTresuryValue = txCurrentTreasuryValue bc
   in return $
        ledgerbody
          & L.proposalProceduresTxBodyL
            .~ convProposalProcedures (maybe TxProposalProceduresNone unFeatured propProcedures)
          & L.votingProceduresTxBodyL
            .~ convVotingProcedures (maybe TxVotingProceduresNone unFeatured voteProcedures)
          & L.treasuryDonationTxBodyL .~ maybe (L.Coin 0) unFeatured treasuryDonation
          & L.currentTreasuryValueTxBodyL
            .~ L.maybeToStrictMaybe (unFeatured =<< currentTresuryValue)

hashTxBody
  :: L.HashAnnotated (Ledger.TxBody era) EraIndependentTxBody L.StandardCrypto
  => L.TxBody era -> L.Hash L.StandardCrypto EraIndependentTxBody
hashTxBody = L.extractHash @L.StandardCrypto . L.hashAnnotated

makeKeyWitness
  :: Era era
  -> UnsignedTx era
  -> ShelleyWitnessSigningKey
  -> L.WitVKey L.Witness L.StandardCrypto
makeKeyWitness era (UnsignedTx unsignedTx) wsk =
  obtainCommonConstraints era $
    let txbody = unsignedTx ^. L.bodyTxL
        txhash :: L.Hash L.StandardCrypto EraIndependentTxBody
        txhash = obtainCommonConstraints era $ hashTxBody txbody
        sk = toShelleySigningKey wsk
        vk = getShelleyKeyWitnessVerificationKey sk
        signature = makeShelleySignature txhash sk
     in L.WitVKey vk signature

signTx
  :: Era era
  -> [L.BootstrapWitness L.StandardCrypto]
  -> [L.WitVKey L.Witness L.StandardCrypto]
  -> UnsignedTx era
  -> Ledger.Tx (LedgerEra era)
signTx era bootstrapWits shelleyKeyWits (UnsignedTx unsigned) =
  obtainCommonConstraints era $
    let currentScriptWitnesses = unsigned ^. L.witsTxL
        keyWits =
          obtainCommonConstraints era $
            L.mkBasicTxWits
              & L.addrTxWitsL
                .~ Set.fromList shelleyKeyWits
              & L.bootAddrTxWitsL
                .~ Set.fromList bootstrapWits
        signedTx = unsigned & L.witsTxL .~ (keyWits <> currentScriptWitnesses)
     in signedTx

-- Compatibility related. Will be removed once the old api has been deprecated and deleted.

convertTxBodyToUnsignedTx
  :: HasCallStack => ShelleyBasedEra era -> TxBody era -> UnsignedTx era
convertTxBodyToUnsignedTx sbe txbody =
  forEraInEon
    (toCardanoEra sbe)
    (error $ "convertTxBodyToUnsignedTx: Error - unsupported era " <> docToString (pretty sbe))
    ( \w -> do
        let ShelleyTx _ unsignedLedgerTx = makeSignedTransaction [] txbody
        UnsignedTx $ obtainCommonConstraints w unsignedLedgerTx
    )
