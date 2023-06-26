{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressPoolDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    makeCommitteeDelegationCertificate,
    makeCommitteeHotKeyUnregistrationCertificate,

    -- * Registering DReps
    DRepMetadataReference(..),

    -- * Special certificates
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    MIRTarget (..),

    -- * Internal conversion functions
    toShelleyCertificate,
    fromShelleyCertificate,
    toShelleyPoolParams,
    fromShelleyPoolParams,

    -- * Data family instances
    AsType(..)
  ) where

import           Cardano.Api.Address
import           Cardano.Api.DRepMetadata
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Value

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Allegra.Core (AtMostEra)
import qualified Cardano.Ledger.Babbage as L
import           Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Shelley
import qualified Cardano.Ledger.Coin as Shelley (toDeltaCoin)
import qualified Cardano.Ledger.Core as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Shelley as Shelley
import           Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import           Cardano.Slotting.Slot (EpochNo (..))

import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.IP (IPv4, IPv6)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

data Certificate =

     -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressPoolDelegationCertificate StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo

     -- Special certificates
   | GenesisKeyDelegationCertificate
      (Hash GenesisKey)
      (Hash GenesisDelegateKey)
      (Hash VrfKey)

   | CommitteeDelegationCertificate
      (Hash CommitteeColdKey)
      (Hash CommitteeHotKey)

   | CommitteeHotKeyDeregistrationCertificate
      (Hash CommitteeColdKey)

   | MIRCertificate MIRPot MIRTarget

  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance ToCBOR Certificate where
    toCBOR = Shelley.toEraCBOR @Shelley.Shelley . (toShelleyCertificate @Shelley.Shelley) -- FIXME!

instance FromCBOR Certificate where
    fromCBOR = (fromShelleyCertificate @Shelley.Shelley) <$> Shelley.fromEraCBOR @Shelley.Shelley

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr cert = case cert of
      StakeAddressRegistrationCertificate{}   -> "Stake address registration"
      StakeAddressDeregistrationCertificate{} -> "Stake address deregistration"
      StakeAddressPoolDelegationCertificate{} -> "Stake address stake pool delegation"
      StakePoolRegistrationCertificate{}      -> "Pool registration"
      StakePoolRetirementCertificate{}        -> "Pool retirement"
      GenesisKeyDelegationCertificate{}       -> "Genesis key delegation"
      CommitteeDelegationCertificate{} -> "Constitution committee member key delegation"
      CommitteeHotKeyDeregistrationCertificate{} -> "Constitution committee member hot key deregistration"
      MIRCertificate{}                        -> "MIR"

-- | The 'MIRTarget' determines the target of a 'MIRCertificate'.
-- A 'MIRCertificate' moves lovelace from either the reserves or the treasury
-- to either a collection of stake credentials or to the other pot.
data MIRTarget =

     -- | Use 'StakeAddressesMIR' to make the target of a 'MIRCertificate'
     -- a mapping of stake credentials to lovelace.
     StakeAddressesMIR [(StakeCredential, Lovelace)]

     -- | Use 'SendToReservesMIR' to make the target of a 'MIRCertificate'
     -- the reserves pot.
   | SendToReservesMIR Lovelace

     -- | Use 'SendToTreasuryMIR' to make the target of a 'MIRCertificate'
     -- the treasury pot.
   | SendToTreasuryMIR Lovelace
  deriving stock (Eq, Show)

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Lovelace,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Lovelace,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: Text,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- DRep parameters
--

data DRepMetadataReference =
  DRepMetadataReference
  { drepMetadataURL  :: Text
  , drepMetadataHash :: Hash DRepMetadata
  }
  deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Constructor functions
--

makeStakeAddressRegistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressRegistrationCertificate = StakeAddressRegistrationCertificate

makeStakeAddressDeregistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressDeregistrationCertificate = StakeAddressDeregistrationCertificate

makeStakeAddressPoolDelegationCertificate :: StakeCredential -> PoolId -> Certificate
makeStakeAddressPoolDelegationCertificate = StakeAddressPoolDelegationCertificate

makeStakePoolRegistrationCertificate :: StakePoolParameters -> Certificate
makeStakePoolRegistrationCertificate = StakePoolRegistrationCertificate

makeStakePoolRetirementCertificate :: PoolId -> EpochNo -> Certificate
makeStakePoolRetirementCertificate = StakePoolRetirementCertificate

makeGenesisKeyDelegationCertificate :: Hash GenesisKey
                                    -> Hash GenesisDelegateKey
                                    -> Hash VrfKey
                                    -> Certificate
makeGenesisKeyDelegationCertificate = GenesisKeyDelegationCertificate

makeCommitteeDelegationCertificate :: ()
  => Hash CommitteeColdKey
  -> Hash CommitteeHotKey
  -> Certificate
makeCommitteeDelegationCertificate = CommitteeDelegationCertificate

makeCommitteeHotKeyUnregistrationCertificate :: ()
  => Hash CommitteeColdKey
  -> Certificate
makeCommitteeHotKeyUnregistrationCertificate = CommitteeHotKeyDeregistrationCertificate

makeMIRCertificate :: MIRPot -> MIRTarget -> Certificate
makeMIRCertificate = MIRCertificate


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyCertificate :: (Shelley.EraCrypto era ~ StandardCrypto,
  Shelley.ShelleyEraTxCert era,
  Shelley.TxCert era ~ Shelley.ShelleyTxCert era,
                        AtMostEra L.BabbageEra era) => Certificate -> Shelley.ShelleyTxCert era
toShelleyCertificate (StakeAddressRegistrationCertificate stakecred) =
    Shelley.RegTxCert (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressDeregistrationCertificate stakecred) =
    Shelley.UnRegTxCert (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressPoolDelegationCertificate
                        stakecred (StakePoolKeyHash poolid)) =
    Shelley.DelegStakeTxCert (toShelleyStakeCredential stakecred) poolid

toShelleyCertificate (StakePoolRegistrationCertificate poolparams) =
    Shelley.RegPoolTxCert (toShelleyPoolParams poolparams)

toShelleyCertificate (StakePoolRetirementCertificate
                       (StakePoolKeyHash poolid) epochno) =
    Shelley.RetirePoolTxCert poolid epochno

toShelleyCertificate (GenesisKeyDelegationCertificate
                       (GenesisKeyHash         genesiskh)
                       (GenesisDelegateKeyHash delegatekh)
                       (VrfKeyHash             vrfkh)) =
    Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh

toShelleyCertificate
  ( CommitteeDelegationCertificate
    (CommitteeColdKeyHash _ckh)
    (CommitteeHotKeyHash  _hkh)
  ) = error "TODO CIP-1694 Need ledger types for CommitteeDelegationCertificate"

toShelleyCertificate
  ( CommitteeHotKeyDeregistrationCertificate
    (CommitteeColdKeyHash _ckh)
  ) = error "TODO CIP-1694 Need ledger types for CommitteeHotKeyDeregistrationCertificate"

toShelleyCertificate (MIRCertificate mirpot (StakeAddressesMIR amounts)) =
    Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])

toShelleyCertificate (MIRCertificate mirPot (SendToReservesMIR amount)) =
    case mirPot of
      TreasuryMIR ->
        Shelley.MirTxCert $
          Shelley.MIRCert
            TreasuryMIR
            (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
      ReservesMIR ->
        error "toShelleyCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"

toShelleyCertificate (MIRCertificate mirPot (SendToTreasuryMIR amount)) =
    case mirPot of
      ReservesMIR ->
        Shelley.MirTxCert $
          Shelley.MIRCert
            ReservesMIR
            (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
      TreasuryMIR ->
        error "toShelleyCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"


fromShelleyCertificate ::
  (Shelley.EraCrypto era ~ StandardCrypto,
    Shelley.ShelleyEraTxCert era,
    Shelley.TxCert era ~ Shelley.ShelleyTxCert era,
                          AtMostEra L.BabbageEra era)
  => Shelley.ShelleyTxCert era -> Certificate
fromShelleyCertificate (Shelley.RegTxCert stakecred) =
    StakeAddressRegistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.UnRegTxCert stakecred) =
    StakeAddressDeregistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.DelegStakeTxCert stakecred poolid) =
    StakeAddressPoolDelegationCertificate
      (fromShelleyStakeCredential stakecred)
      (StakePoolKeyHash poolid)

fromShelleyCertificate (Shelley.RegPoolTxCert poolparams) =
    StakePoolRegistrationCertificate
      (fromShelleyPoolParams poolparams)

fromShelleyCertificate (Shelley.RetirePoolTxCert poolid epochno) =
    StakePoolRetirementCertificate
      (StakePoolKeyHash poolid)
      epochno

fromShelleyCertificate (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh) =
    GenesisKeyDelegationCertificate
      (GenesisKeyHash         genesiskh)
      (GenesisDelegateKeyHash delegatekh)
      (VrfKeyHash             vrfkh)

fromShelleyCertificate (Shelley.MirTxCert
                         (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) =
    MIRCertificate
      mirpot
      (StakeAddressesMIR
        [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
        | (sc, v) <- Map.toList amounts ]
      )
fromShelleyCertificate (Shelley.MirTxCert
                         (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) =
    MIRCertificate ReservesMIR
      (SendToTreasuryMIR $ fromShelleyLovelace amount)

fromShelleyCertificate (Shelley.MirTxCert
                         (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) =
    MIRCertificate TreasuryMIR
      (SendToReservesMIR $ fromShelleyLovelace amount)

toShelleyPoolParams :: StakePoolParameters -> Shelley.PoolParams StandardCrypto
toShelleyPoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters such as the PoolMargin below, but also
    -- do simple client-side sanity checks, e.g. on the pool metadata url
    Shelley.PoolParams {
      Shelley.ppId      = poolkh
    , Shelley.ppVrf     = vrfkh
    , Shelley.ppPledge  = toShelleyLovelace stakePoolPledge
    , Shelley.ppCost    = toShelleyLovelace stakePoolCost
    , Shelley.ppMargin  = fromMaybe
                               (error "toShelleyPoolParams: invalid PoolMargin")
                               (Shelley.boundRational stakePoolMargin)
    , Shelley.ppRewardAcnt   = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley.ppOwners  = Set.fromList
                               [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley.ppRelays  = Seq.fromList
                               (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley.ppMetadata = toShelleyPoolMetadata <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toShelleyStakePoolRelay :: StakePoolRelay -> Shelley.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Shelley.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Shelley.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)

    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Shelley.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Shelley.PoolMetadata
    toShelleyPoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetadata {
        Shelley.pmUrl  = toShelleyUrl stakePoolMetadataURL
      , Shelley.pmHash = Crypto.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Shelley.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Shelley.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: Text -> Shelley.Url
    toShelleyUrl = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                 . Shelley.textToUrl


fromShelleyPoolParams :: Shelley.PoolParams StandardCrypto
                      -> StakePoolParameters
fromShelleyPoolParams
    Shelley.PoolParams {
      Shelley.ppId
    , Shelley.ppVrf
    , Shelley.ppPledge
    , Shelley.ppCost
    , Shelley.ppMargin
    , Shelley.ppRewardAcnt
    , Shelley.ppOwners
    , Shelley.ppRelays
    , Shelley.ppMetadata
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash ppId
    , stakePoolVRF           = VrfKeyHash ppVrf
    , stakePoolCost          = fromShelleyLovelace ppCost
    , stakePoolMargin        = Shelley.unboundRational ppMargin
    , stakePoolRewardAccount = fromShelleyStakeAddr ppRewardAcnt
    , stakePoolPledge        = fromShelleyLovelace ppPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList ppOwners)
    , stakePoolRelays        = map fromShelleyStakePoolRelay
                                   (Foldable.toList ppRelays)
    , stakePoolMetadata      = fromShelleyPoolMetadata <$>
                                 strictMaybeToMaybe ppMetadata
    }
  where
    fromShelleyStakePoolRelay :: Shelley.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Shelley.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (strictMaybeToMaybe mipv4)
        (strictMaybeToMaybe mipv6)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Shelley.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata Shelley.PoolMetadata {
                              Shelley.pmUrl
                            , Shelley.pmHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Shelley.urlToText pmUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                              . Crypto.hashFromBytes
                              $ pmHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Shelley.DnsName -> ByteString
    fromShelleyDnsName = Text.encodeUtf8
                       . Shelley.dnsToText
