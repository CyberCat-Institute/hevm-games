{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Examples.Lido where

import qualified Optics.Core as Op
import qualified Optics.Operators.Unsafe as Op

import qualified Data.Map.Strict as M
import Control.Monad.ST (RealWorld)

import EVM.Fetch (zero)
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.Types
import EVM.TH

import GHC.Word (Word64)
import Types hiding (GameParameters(..))
import SupportFunctions
import Strategies

import Parameterization
import OpenGames hiding (dependentDecision, fromFunctions, fromLens, forwardFunction)
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor hiding (Lit)
import OpenGames.Engine.OpticClass

import Control.Monad.Trans.State.Strict (StateT, evalStateT, execStateT, modify)
import EVM.Types (VM, W256, VMType(..))

import Debug.Trace

-- maybe we need one more for extracting the state?
$(loadAll [mkContractFileInfo "DualGovernance.sol" [mkContractInfo "DualGovernance" "dualgov"]
          ,mkContractFileInfo "Escrow.sol" [mkContractInfo "Escrow" "escrow"]])

lots :: Word64
lots = 1_000_000

stakeOrUnstakeReal :: EthAgent -> W256 -> AccountState -- GlobalLidoState -> SignallingEscrowState
               -> EVM Concrete RealWorld (AccountState, SignallingEscrowState)
stakeOrUnstakeReal agent amount accounts
  = do let lockTransaction = escrow_lockStETH (addr agent) 0 lots (fromIntegral amount)
       -- let stateTransaction = dualgov_getEffectiveState (addr agent) 0 lots -- do we even need this?
       (result, newState) <- sendAndRunAll [lockTransaction]
       let contracts = Op.preview (#env Op.% #contracts) newState
       -- somehow access dualgov state
       pure (subtract accounts (name agent) amount,
          SignallingEscrowState (trace (show contracts) mempty) mempty mempty mempty)
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
stakingGame :: GovernanceParams
            -> EthAgent
            -> [W256]
            -> HEVMGame '[(CurrentProposal (ProposalModel' W256), OpportunityCostsEVM, RiskFactorEVM) -> W256]
                       '[HEVMState (DiagnosticInfoBayesian (CurrentProposal (ProposalModel' W256), OpportunityCostsEVM, RiskFactorEVM) W256)]
                        (AccountState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal (ProposalModel' W256), OpportunityCostsEVM, RiskFactorEVM)
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

playerAgent = EthAgent "player1" player1

maximumAmount = 100

transactions :: [W256]
transactions = [0,maximumAmount `div` 2, maximumAmount] -- 0, half, everything

initialAccountState = AccountState (M.fromList [("player1", maximumAmount)])


lidoOutcome GameParametersEVM{..} = do
  let addresses =
        [ (player1, Lit 1_000_000_000),
          (dualgov_contract, Lit 10_000),
          (escrow_contract, Lit 10_000)
        ]
  i <- setupAddresses addresses <$> stToIO initial
  let game = stakingGame defaultGovernanceParams playerAgent transactions
  let strat _ = minimum transactions
  let ctxt = MonadContextM
               (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)))
               (\_ _ -> let p = evaluateProposal proposal in pure (snd p))
  let evaluated :- Nil = evaluate game (strat :- Nil) ctxt

  evaluated1 <- stToIO (evalStateT evaluated i)
  -- evaluated2 <- stToIO (evalStateT bbb i)
  -- let out1 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)
  generateOutput (evaluated1 :- Nil)

runAll = lidoOutcome simpleStakingGameParametersEVM
