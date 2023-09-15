{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Api.Eon
  ( Featured (..)
  , unFeatured
  , asFeaturedInEra
  , asFeaturedInShelleyBasedEra
  ) where

import           Cardano.Api.Eras.Core

-- | A value only if the feature is supported in this era
data Featured feature era a where
  Featured
    :: feature era
    -- ^ The witness that the feature is supported in this era
    -> a
    -- ^ The value to use
    -> Featured feature era a

deriving instance (Eq a, Eq (feature era)) => Eq (Featured feature era a)
deriving instance (Show a, Show (feature era)) => Show (Featured feature era a)

instance Functor (Featured feature era) where
  fmap f (Featured feature a) = Featured feature (f a)

unFeatured :: Featured feature era a -> a
unFeatured (Featured _ a) = a

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeaturedInEra :: ()
  => Eon eon
  => a
  -> CardanoEra era
  -> Maybe (Featured eon era a)
asFeaturedInEra value = featureInEra Nothing (Just . flip Featured value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeaturedInShelleyBasedEra :: ()
  => Eon eon
  => a
  -> ShelleyBasedEra era
  -> Maybe (Featured eon era a)
asFeaturedInShelleyBasedEra value = asFeaturedInEra value . shelleyBasedToCardanoEra
