{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Tx.UTxO where

import Cardano.Api.Internal.Eon.ShelleyBasedEra (IsShelleyBasedEra)
import Cardano.Api.Internal.Eras.Core (IsCardanoEra)
import Cardano.Api.Internal.Tx.Body (CtxUTxO, TxOut (..))
import Cardano.Api.Internal.TxIn (TxIn (..))

import Cardano.Ledger.Babbage ()

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts (IsList (..))

newtype UTxO era = UTxO {unUTxO :: Map TxIn (TxOut CtxUTxO era)}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, IsList)

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

instance
  IsShelleyBasedEra era
  => FromJSON (UTxO era)
  where
  parseJSON = Aeson.withObject "UTxO" $ \hm -> do
    let l = toList $ KeyMap.toHashMapText hm
    res <- mapM toTxIn l
    pure . UTxO $ Map.fromList res
   where
    toTxIn :: (Text, Aeson.Value) -> Parser (TxIn, TxOut CtxUTxO era)
    toTxIn (txinText, txOutVal) = do
      (,)
        <$> parseJSON (Aeson.String txinText)
        <*> parseJSON txOutVal

-- | Infix version of `difference`.
(\\) :: UTxO era -> UTxO era -> UTxO era
a \\ b = difference a b

-- | Create an empty `UTxO`.
empty :: UTxO era
empty = UTxO Map.empty

-- | Create a `UTxO` from a single unspent transaction output.
singleton :: TxIn -> TxOut CtxUTxO era -> UTxO era
singleton i o = UTxO $ Map.singleton i o

-- | Find a 'TxOut' for a given 'TxIn'.
lookup :: TxIn -> UTxO era -> Maybe (TxOut CtxUTxO era)
lookup k = Map.lookup k . unUTxO

-- | Filter all `TxOut` that satisfy the predicate.
filter :: (TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filter fn = UTxO . Map.filter fn . unUTxO

-- | Filter all UTxO to only include 'out's satisfying given predicate.
filterWithKey :: (TxIn -> TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filterWithKey fn = UTxO . Map.filterWithKey fn . unUTxO

-- | Get the 'UTxO domain input's set
inputSet :: UTxO (TxOut CtxUTxO era) -> Set TxIn
inputSet = Map.keysSet . unUTxO

-- | Remove the right hand side from the left hand side.
difference :: UTxO era -> UTxO era -> UTxO era
difference a b = UTxO $ Map.difference (unUTxO a) (unUTxO b)
