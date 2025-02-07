{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Lido where

import EVM.TH
import Types

$(loadAll [mkContractFileInfo "DualGovernance.sol" [mkContractInfo "DualGovernance" "dualgov"]
          ,mkContractFileInfo "Escrow.sol" [mkContractInfo "Escrow" "escrow"]])

-- stakeOrUnstakeReal :: Agent -> StETH -> AccountState -- GlobalLidoState -> SignallingEscrowState
--                -> IO (AccountState, SignallingEscrowState)
-- stakeOrUnstakeReal name amount accounts
--   = do escrow_lockStETH name amount
--        st <- dualgov_getEffectiveState name
--        pure (subtract accounts name amount, st)
--
--
