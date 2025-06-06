{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module OpenGames.Engine.Copy where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict (StateT, get, modify, put)
import Data.Kind
import Data.Vector.Unboxed.Mutable
import EVM.Types
import Optics.Core
import Optics.State

class Copy (a :: Type -> Type) where
  copy :: StateT (a s) (ST s) (a s)

class Restore (a :: Type -> Type) where
  restore :: a s -> StateT (a s) (ST s) ()

instance Copy (VM Concrete) where
  copy = do
    state <- get
    let st = state ^. #state
    let fr = state ^. #frames
    st' <- copyFrameState st
    fr' <- traverse copyFrame fr
    let newState =
          state
            & #state
            .~ st'
            & #frames
            .~ fr'
            & #result
            .~ Nothing
    pure newState
    where
      copyFrame :: Frame Concrete s -> StateT (VM Concrete s) (ST s) (Frame Concrete s)
      copyFrame (Frame ctx state) = Frame ctx <$> copyFrameState state
      copyFrameState :: FrameState Concrete s -> StateT (VM Concrete s) (ST s) (FrameState Concrete s)
      copyFrameState oldFrame = do
        let mem = oldFrame ^. #memory
        mem' <- case mem of
          ConcreteMemory mem -> ConcreteMemory <$> clone mem
          SymbolicMemory x -> pure (SymbolicMemory x)

        pure (oldFrame & #memory .~ mem')

instance Restore (VM Concrete) where
  restore = put
