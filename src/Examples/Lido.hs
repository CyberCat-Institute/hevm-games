{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}

module Examples.Lido where

import qualified Data.Map.Strict as M
import Control.Monad.ST (RealWorld)

import EVM.Fetch (zero)
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.Types
import EVM.TH

import GHC.Word (Word64)
import Types
import SupportFunctions

import OpenGames hiding (dependentDecision, fromFunctions, fromLens, forwardFunction)
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor hiding (Lit)

import Control.Monad.Trans.State.Strict (StateT, evalStateT, execStateT, modify)
import EVM.Types (VM, W256, VMType(..))

-- maybe we need one more for extracting the state?
$(loadAll [mkContractFileInfo "DualGovernance.sol" [mkContractInfo "DualGovernance" "dualgov"]
          ,mkContractFileInfo "Escrow.sol" [mkContractInfo "Escrow" "escrow"]])

lots :: Word64
lots = 1_000_000

stakeOrUnstakeReal :: EthAgent -> W256 -> AccountState -- GlobalLidoState -> SignallingEscrowState
               -> EVM Concrete RealWorld (AccountState, SignallingEscrowState)
stakeOrUnstakeReal agent amount accounts
  = do let lockTransaction = escrow_lockStETH (addr agent) 0 lots (fromIntegral amount)
       let stateTransaction = dualgov_getEffectiveState (addr agent) 0 lots -- do we even need this?
       (result, newState) <- sendAndRunAll [lockTransaction, stateTransaction]
       pure (subtract accounts (name agent) amount, undefined)
    where
    subtract :: AccountState -> Agent -> W256 -> AccountState
    subtract st agent amt =
      AccountState $ M.alter (fmap (\x -> x - amt)) agent (getAccountsStETH st)

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
            -> EthAgent
            -> [W256]
            -> HEVMGame '[(CurrentProposal a, OpportunityCostsEVM, RiskFactorEVM) -> W256]
                       '[HEVMState (DiagnosticInfoBayesian (CurrentProposal a, OpportunityCostsEVM, RiskFactorEVM) W256)]
                        (AccountState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal a, OpportunityCostsEVM, RiskFactorEVM)
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
  operation: hevmDecision (name stakingAgent) actionSpace;
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: fromFunctions computeRiskCosts' id;
  outputs: costsOfStaking;
  returns: ;

  // Transfer funds between staker's wallet and signalling escrow
  inputs: amountStaked, globalLidoState, currentSignallingEscrowState;
  feedback: ;
  operation: fromLensM (\(amountStaked, globalLidoState, currentSignallingEscrowState)
                           -> stakeOrUnstakeReal stakingAgent amountStaked globalLidoState) (const pure);
  outputs: (newGlobalLidoState, newSignallingEscrowState);
  returns: ;

  // Compute actual payoffs as own assets at risk
  inputs: newGlobalLidoState, agentRiskFactor ;
  feedback: agentPayoff ;
  operation: fromLens id (computeAssetsAtRisk' (name stakingAgent)) ;
  outputs: discard ;
  returns: payoff;

  :---:

  outputs: newSignallingEscrowState, currentDGState, currentDGValues;
  returns: payoff;
|]

player1 = LitAddr 0x1234
player2 = LitAddr 0x1235

transactions :: [W256]
transactions = undefined

lidoOutcome = do
  let addresses =
        [ (player1, Lit 1_000_000_000),
          (player2, Lit 1_000_000_000),
          (dualgov_contract, Lit 10_000),
          (escrow_contract, Lit 10_001)
        ]
  i <- setupAddresses addresses <$> stToIO initial
  let game = stakingGame undefined undefined playerTransactions
  let what = evaluate game  undefined undefined
  _
  -- evaluated1 <- stToIO (evalStateT aaa i)
  -- evaluated2 <- stToIO (evalStateT bbb i)
  -- let out1 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)
