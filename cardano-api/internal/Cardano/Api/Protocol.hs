{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Protocol
  ( BlockType(..)
  , SomeBlockType (..)
  , reflBlockType
  , Protocol(..)
  , ProtocolInfoArgs(..)
  , ProtocolClient(..)
  , ProtocolClientInfoArgs(..)
  ) where

import           Cardano.Api.Modes

import Cardano.Ledger.Crypto
import Cardano.Consensus.API

import           Data.Bifunctor (bimap)

import           Type.Reflection ((:~:) (..))

class (RunNode blk, IOLike m) => Protocol m blk where
  data ProtocolInfoArgs blk
  protocolInfo :: ProtocolInfoArgs blk -> (ProtocolInfo blk, m [BlockForging m blk])

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class RunNode blk => ProtocolClient blk where
  data ProtocolClientInfoArgs blk
  protocolClientInfo :: ProtocolClientInfoArgs blk -> ProtocolClientInfo blk


-- | Run PBFT against the Byron ledger
instance IOLike m => Protocol m ByronBlockHFC where
  data ProtocolInfoArgs ByronBlockHFC = ProtocolInfoArgsByron ProtocolParamsByron
  protocolInfo (ProtocolInfoArgsByron params) = ( inject $ protocolInfoByron params
                                                , pure . map inject $ blockForgingByron params
                                                )

instance (CardanoHardForkConstraints StandardCrypto, IOLike m) => Protocol m (CardanoBlock StandardCrypto) where
  data ProtocolInfoArgs (CardanoBlock StandardCrypto) =
         ProtocolInfoArgsCardano
           ProtocolParamsByron
          (ProtocolParamsShelleyBased StandardShelley)
          (ProtocolParamsShelley StandardCrypto)
          (ProtocolParamsAllegra StandardCrypto)
          (ProtocolParamsMary StandardCrypto)
          (ProtocolParamsAlonzo StandardCrypto)
          (ProtocolParamsBabbage StandardCrypto)
          (ProtocolParamsConway StandardCrypto)
          (ProtocolTransitionParamsShelleyBased StandardShelley)
          (ProtocolTransitionParamsShelleyBased StandardAllegra)
          (ProtocolTransitionParamsShelleyBased StandardMary)
          (ProtocolTransitionParamsShelleyBased StandardAlonzo)
          (ProtocolTransitionParamsShelleyBased StandardBabbage)
          (ProtocolTransitionParamsShelleyBased StandardConway)

  protocolInfo (ProtocolInfoArgsCardano
               paramsByron
               paramsShelleyBased
               paramsShelley
               paramsAllegra
               paramsMary
               paramsAlonzo
               paramsBabbage
               paramsConway
               paramsByronShelley
               paramsShelleyAllegra
               paramsAllegraMary
               paramsMaryAlonzo
               paramsAlonzoBabbage
               paramsBabbageConway) =
    protocolInfoCardano
      paramsByron
      paramsShelleyBased
      paramsShelley
      paramsAllegra
      paramsMary
      paramsAlonzo
      paramsBabbage
      paramsConway
      paramsByronShelley
      paramsShelleyAllegra
      paramsAllegraMary
      paramsMaryAlonzo
      paramsAlonzoBabbage
      paramsBabbageConway

instance ProtocolClient ByronBlockHFC where
  data ProtocolClientInfoArgs ByronBlockHFC =
    ProtocolClientInfoArgsByron EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

instance CardanoHardForkConstraints StandardCrypto => ProtocolClient (CardanoBlock StandardCrypto) where
  data ProtocolClientInfoArgs (CardanoBlock StandardCrypto) =
    ProtocolClientInfoArgsCardano EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsCardano epochSlots) =
    protocolClientInfoCardano epochSlots

instance ( IOLike m
         , LedgerSupportsProtocol
             (ShelleyBlock
                (TPraos StandardCrypto) (ShelleyEra StandardCrypto))
         )
  => Protocol m (ShelleyBlockHFC (TPraos StandardCrypto) StandardShelley) where
  data ProtocolInfoArgs (ShelleyBlockHFC (TPraos StandardCrypto) StandardShelley) = ProtocolInfoArgsShelley
    (ProtocolParamsShelleyBased StandardShelley)
    (ProtocolParamsShelley StandardCrypto)
  protocolInfo (ProtocolInfoArgsShelley paramsShelleyBased paramsShelley) =
    bimap inject (fmap $ map inject) $ protocolInfoShelley paramsShelleyBased paramsShelley

instance LedgerSupportsProtocol
          (ShelleyBlock
            (TPraos StandardCrypto) (ShelleyEra StandardCrypto))
  => ProtocolClient (ShelleyBlockHFC (TPraos StandardCrypto) StandardShelley) where
  data ProtocolClientInfoArgs (ShelleyBlockHFC (TPraos StandardCrypto) StandardShelley) =
    ProtocolClientInfoArgsShelley
  protocolClientInfo ProtocolClientInfoArgsShelley =
    inject protocolClientInfoShelley

data BlockType blk where
  ByronBlockType :: BlockType ByronBlockHFC
  ShelleyBlockType :: BlockType (ShelleyBlockHFC (TPraos StandardCrypto) StandardShelley)
  CardanoBlockType :: BlockType (CardanoBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

reflBlockType :: BlockType blk -> BlockType blk' -> Maybe (blk :~: blk')
reflBlockType ByronBlockType   ByronBlockType   = Just Refl
reflBlockType ShelleyBlockType ShelleyBlockType = Just Refl
reflBlockType CardanoBlockType CardanoBlockType = Just Refl
reflBlockType _                _                = Nothing


data SomeBlockType where
  SomeBlockType :: BlockType blk -> SomeBlockType

deriving instance Show SomeBlockType
