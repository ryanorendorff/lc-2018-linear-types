{-# LANGUAGE GADTs #-}

module LinearValue where

import qualified Prelude.Linear as PL
import Prelude.Linear (Unrestricted)

newtype Linear a = Linear {unLinear :: (a ->. Unrestricted r) ->. r}

