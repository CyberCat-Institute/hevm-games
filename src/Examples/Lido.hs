{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Lido where

import EVM.Fetch (zero)
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.Types
import EVM.TH
import Types
import SupportFunctions

import OpenGames hiding (dependentDecision, fromFunctions, fromLens, forwardFunction)
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor

import Control.Monad.Trans.State.Strict (StateT, evalStateT, execStateT, modify)
import EVM.Types (VM, W256, VMType(..))

-- maybe we don't need dual gov
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
-- Decision of representative stETH holder (aka staker) to lock or unlock funds from signalling escrow
-- Includes payoff adjustment from locked or unlocked funds
--   :: HEVMGame
--        [() -> EthTransaction, () -> EthTransaction]
--        [HEVMState (DiagnosticInfoBayesian () EthTransaction)
--        ,HEVMState (DiagnosticInfoBayesian () EthTransaction)]
--        ()
--        ()
--        ()
--        ()
stakingGame :: (Eq a, Show a)
            => GovernanceParams
            -> Agent
            -> [Double]
            -> HEVMGame '[(CurrentProposal a, OpportunityCosts, RiskFactor) -> Double]
                       '[HEVMState (DiagnosticInfoBayesian (CurrentProposal a, OpportunityCosts, RiskFactor) Double)]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal a, OpportunityCosts, RiskFactor)
                        ()
                        (SignallingEscrowState, GovernanceState, GovernanceValues)
                        W256
stakingGame governanceParams stakingAgent actionSpace  = [opengame|
  inputs: globalLidoState, currentTime, currentSignallingEscrowState, currentDGState, currentDGValues, proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;

  :---:

  // Decision of amount to stake or unstake from signalling escrow
  inputs: proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: hevmDecision stakingAgent (actionSpace);
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: fromFunctions computeRiskCosts id;
  outputs: costsOfStaking;
  returns: ;

  // Transfer funds between staker's wallet and signalling escrow
  inputs: amountStaked, globalLidoState, currentSignallingEscrowState;
  feedback: ;
  operation: fromFunctions (\(amountStaked, globalLidoState, currentSignallingEscrowState)
                           -> stakeOrUnstake stakingAgent amountStaked globalLidoState currentSignallingEscrowState) id;
  outputs: (newGlobalLidoState, newSignallingEscrowState);
  returns: ;

  // Compute actual payoffs as own assets at risk
  inputs: newGlobalLidoState, agentRiskFactor ;
  feedback: agentPayoff ;
  operation: fromLens id (computeAssetsAtRisk stakingAgent) ;
  outputs: discard ;
  returns: payoff;

  :---:

  outputs: newSignallingEscrowState, currentDGState, currentDGValues;
  returns: payoff;
|]
