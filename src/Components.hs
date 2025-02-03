{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Components
  where

import SupportFunctions
import Types
import ActionSpaces

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-----------------------------------------------
Describes all the major components of our models
------------------------------------------------}


------------------
-- 1 Staking model
------------------

-- Decision of representative stETH holder (aka staker) to lock or unlock funds from signalling escrow
-- Includes payoff adjustment from locked or unlocked funds
stakingGame :: (Eq a, Show a)
            => GovernanceParams
            -> Agent
            -> [Double]
            -> OpenGame StochasticStatefulOptic
                        StochasticStatefulContext
                       '[Kleisli Stochastic (CurrentProposal a, OpportunityCosts, RiskFactor) Double]
                       '[[DiagnosticInfoBayesian (CurrentProposal a, OpportunityCosts, RiskFactor) Double]]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal a, OpportunityCosts, RiskFactor)
                        ()
                        (SignallingEscrowState, GovernanceState, GovernanceValues)
                        Payoff
stakingGame governanceParams stakingAgent actionSpace  = [opengame|
  inputs: globalLidoState, currentTime, currentDGState, currentDGValues, proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;

  :---:

  // Decision of amount to stake or unstake from signalling escrow
  inputs: proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: dependentDecision stakingAgent (const actionSpace);
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: forwardFunction computeRiskCosts;
  outputs: costsOfStaking;
  returns: ;

  // Transfer funds between staker's wallet and signalling escrow
  inputs: amountStaked, globalLidoState;
  feedback: ;
  operation: fromFunctions (\(amountStaked, globalLidoState)
                           -> stakeOrUnstake stakingAgent amountStaked globalLidoState) id;
  outputs: (newGlobalLidoState, newSignallingEscrowState);
  returns: ;

-- this above needs to communicate with the contract
-- The signaling state is internalised in the contract
-- we use `fromFunction` but from HEVMGAmes to run it
-- stakeOrUnstakeHEVM sends teh transactions
-- the payoff is computed from GlobalLidoState & SignallingEscrowState
-- to do :
--  - fix conflicting import methods from dependencies
--  - setup the contarct to run in the first place
--  - run it and pray it works

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

-- Same as stakingGame but instead of observing the actual proposal, agent observes a signal
stakingSignalGame :: GovernanceParams
            -> Agent
            -> [Double]
            -> OpenGame StochasticStatefulOptic
                        StochasticStatefulContext
                       '[Kleisli Stochastic (SignalProposal, OpportunityCosts) Double]
                       '[[DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double]]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, SignalProposal, OpportunityCosts)
                        ()
                        (SignallingEscrowState, GovernanceState, GovernanceValues)
                        Payoff
stakingSignalGame governanceParams stakingAgent actionSpace  = [opengame|
  inputs: globalLidoState, currentTime, currentSignallingEscrowState, currentDGState, currentDGValues, signal, opportunityCostFactor;
  feedback: ;
  :---:

  // Decision of amount to stake or unstake from signalling escrow
  inputs: signal, opportunityCostFactor;
  feedback: ;
  operation: dependentDecision stakingAgent (const actionSpace);
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: forwardFunction computeRiskCosts;
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
  inputs: newGlobalLidoState ;
  feedback: agentPayoff ;
  operation: fromLens id (computeAssetsAtRiskPublicOnly stakingAgent) ;
  outputs: discard ;
  returns: payoff;



  :---:

  outputs: newSignallingEscrowState, currentDGState, currentDGValues;
  returns: payoff;
|]


-- Same as stakingSignalGame but exposing the amount staked to be consumed by follower game (see below)
stakingSignalLeaderGame :: GovernanceParams
            -> Agent
            -> [Double]
            -> OpenGame StochasticStatefulOptic
                        StochasticStatefulContext
                       '[Kleisli Stochastic (SignalProposal, OpportunityCosts) Double]
                       '[[DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double]]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, SignalProposal, OpportunityCosts)
                        ()
                        (SignallingEscrowState, GovernanceState, GovernanceValues, Double)
                        Payoff
stakingSignalLeaderGame governanceParams stakingAgent actionSpace  = [opengame|
  inputs: globalLidoState, currentTime, currentSignallingEscrowState, currentDGState, currentDGValues, signal, opportunityCostFactor;
  feedback: ;
  :---:

  // Decision of amount to stake or unstake from signalling escrow
  inputs: signal, opportunityCostFactor;
  feedback: ;
  operation: dependentDecision stakingAgent (const actionSpace);
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: forwardFunction computeRiskCosts;
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
  inputs: newGlobalLidoState ;
  feedback: agentPayoff ;
  operation: fromLens id (computeAssetsAtRiskPublicOnly stakingAgent) ;
  outputs: discard ;
  returns: payoff;



  :---:

  outputs: newSignallingEscrowState, currentDGState, currentDGValues, amountStaked;
  returns: payoff;
|]



-- Same as stakingSignalGame but the agent observes the action of a staker before him
stakingSignalFollowerGame :: GovernanceParams
            -> Agent
            -> [Double]
            -> OpenGame StochasticStatefulOptic
                        StochasticStatefulContext
                       '[Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double]
                       '[[DiagnosticInfoBayesian (SignalProposal, OpportunityCosts, Double) Double]]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, SignalProposal, OpportunityCosts, Double)
                        ()
                        (SignallingEscrowState, GovernanceState, GovernanceValues)
                        Payoff
stakingSignalFollowerGame governanceParams stakingAgent actionSpace  = [opengame|
  inputs: globalLidoState, currentTime, currentSignallingEscrowState, currentDGState, currentDGValues, signal, opportunityCostFactor, stakedByAgentBefore;
  feedback: ;
  :---:

  // Decision of amount to stake or unstake from signalling escrow
  inputs: signal, opportunityCostFactor, stakedByAgentBefore;
  feedback: ;
  operation: dependentDecision stakingAgent (const actionSpace);
  outputs: amountStaked;
  returns: agentPayoff - costsOfStaking;

  // Compute costs of transferring into the escrow
  inputs:  opportunityCostFactor, amountStaked;
  feedback: ;
  operation: forwardFunction computeRiskCosts;
  outputs: costsOfStaking;
  returns: ;

  // Transfer funds between staker's wallet and signalling escrow
  inputs: amountStaked, globalLidoState, ;
  feedback: ;
  operation: fromFunctions (\(amountStaked, globalLidoState )
                           -> stakeOrUnstake stakingAgent amountStaked globalLidoState ) id;
  outputs: (newGlobalLidoState, newSignallingEscrowState);
  returns: ;

  // Compute actual payoffs as own assets at risk
  inputs: newGlobalLidoState ;
  feedback: agentPayoff ;
  operation: fromLens id (computeAssetsAtRiskPublicOnly stakingAgent) ;
  outputs: discard ;
  returns: payoff;



  :---:

  outputs: newSignallingEscrowState, currentDGState, currentDGValues;
  returns: payoff;
|]


---------------------
-- 2 DAO voting model
---------------------

-- Decision of DAO to modify the state of a proposal
daoVoting :: (Show a, Eq a) => GovernanceParams -> Agent
          -> OpenGame StochasticStatefulOptic
                      StochasticStatefulContext
                      '[Kleisli Stochastic (CurrentProposal a) AllDAOActions]
                      '[[DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions]]
                      (TimeAbsolute, GovernanceState, CurrentProposal a)
                      ()
                      (TimeAbsolute, CurrentProposal a)
                      Payoff
daoVoting governanceParams daoAgent = [opengame|
    inputs: currentTime, governanceState, proposal ;
    feedback: ;

    :---:

    // DAO votes to submit, cancel or execute the current proposal
    inputs: proposal;
    feedback: ;
    operation: dependentDecision daoAgent (const allDAOActions);
    outputs: decision;
    returns: payoff;

    // DAO implements the chosen action if able
    inputs: governanceParams, currentTime, governanceState, proposal, decision;
    feedback: ;
    operation: forwardFunction (\(governanceParams, currentTime, governanceState, proposal, decision)
                               -> daoDoMove governanceParams currentTime governanceState proposal decision);
    outputs: newTime, updatedProposal;
    returns: ;

    :---:

    outputs: newTime, updatedProposal;
    returns: payoff;
  |]


-------------------------------
-- 3 Combined DAO-staker models
-------------------------------

-- Model where the DAO moves, then 1 representative staker moves, then time passes
-- Intended to be used for both DAO proposes + staker stakes, and DAO cancels + staker unstakes
daoStakerGame :: (Eq a, Show a)
              => GovernanceParams
              -> Agent
              -> Agent
              -> [Double]
              -> OpenGame StochasticStatefulOptic StochasticStatefulContext
                          '[ Kleisli Stochastic (CurrentProposal a) AllDAOActions
                          , Kleisli Stochastic (CurrentProposal a, OpportunityCosts, RiskFactor) Double ]
                          '[ [DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions]
                          , [DiagnosticInfoBayesian (CurrentProposal a, OpportunityCosts, RiskFactor) Double]]
                          (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal a, OpportunityCosts, RiskFactor)
                          ()
                          (TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal a)
                          (Payoff, Payoff)
daoStakerGame governanceParams daoAgent stakingAgent actionSpaceStaker = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;

  :---:

  // DAO chooses & implements move
  inputs: time, governanceState, proposal;
  feedback: ;
  operation: daoVoting governanceParams daoAgent;
  outputs: time', proposal';
  returns: daoPayoff;

  // Staker chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal', opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: stakingGame governanceParams stakingAgent actionSpaceStaker;
  outputs: signallingEscrowState', governanceState', governanceValues';
  returns: stakerPayoff;

  // Both agents wait for time to pass
  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues';
  feedback: ;
  operation: forwardFunction (\(globalLidoState, time', signallingEscrowState', governanceState', governanceValues') -> transitionWithTime globalLidoState governanceParams time' signallingEscrowState' governanceState' governanceValues');
  outputs: time'', governanceState'', governanceValues'';
  returns: ;

  :---:

  outputs: time'', signallingEscrowState', governanceState'', governanceValues'', proposal';
  returns: daoPayoff, stakerPayoff;
|]

-- Variant of DAO-staker game with 2 heterogenous staking agents
-- (aka representatives of 2 classes of stakers)
dao2StakersGame :: (Eq a, Show a)
                => GovernanceParams
                -> Agent
                -> (Agent, Agent)
                -> [Double]
                -> OpenGame StochasticStatefulOptic
                            StochasticStatefulContext
                            '[Kleisli Stochastic (CurrentProposal a) AllDAOActions,
                              Kleisli Stochastic (CurrentProposal a, OpportunityCosts, RiskFactor) Double,
                              Kleisli Stochastic (CurrentProposal a, OpportunityCosts, RiskFactor) Double]
                            '[[DiagnosticInfoBayesian
                                 (CurrentProposal a) AllDAOActions],
                              [DiagnosticInfoBayesian (CurrentProposal a, OpportunityCosts, RiskFactor) Double],
                              [DiagnosticInfoBayesian (CurrentProposal a, OpportunityCosts, RiskFactor) Double]]
                            (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
                             GovernanceState, GovernanceValues, CurrentProposal a,
                             (OpportunityCosts, OpportunityCosts), (RiskFactor, RiskFactor))
                            ()
                            (TimeAbsolute, SignallingEscrowState, GovernanceState,
                             GovernanceValues, CurrentProposal a)
                            (Payoff, Payoff, Payoff)
dao2StakersGame governanceParams daoAgent (stakingAgent1, stakingAgent2) actionSpaceStaker
  = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, agentRiskFactors;
  feedback: ;

  :---:

  // DAO chooses & implements move
  inputs: time, governanceState, proposal;
  feedback: ;
  operation: daoVoting governanceParams daoAgent;
  outputs: time', proposal';
  returns: daoPayoff;

  // Staker 1 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal', fst opportunityCostFactors, fst agentRiskFactors;
  feedback: ;
  operation: stakingGame governanceParams stakingAgent1 actionSpaceStaker;
  outputs: signallingEscrowState', governanceState', governanceValues';
  returns: staker1Payoff;

  // Staker 2 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState', governanceState', governanceValues', proposal', snd opportunityCostFactors, snd agentRiskFactors;
  feedback: ;
  operation: stakingGame governanceParams stakingAgent2 actionSpaceStaker;
  outputs: signallingEscrowState'', governanceState'', governanceValues'';
  returns: staker2Payoff;

  // Both agents wait for time to pass
  inputs: globalLidoState, time', signallingEscrowState'', governanceState'', governanceValues'';
  feedback: ;
  operation: forwardFunction (\(globalLidoState, time', signallingEscrowState', governanceState', governanceValues') -> transitionWithTime globalLidoState governanceParams time' signallingEscrowState' governanceState' governanceValues');
  outputs: time'', governanceState''', governanceValues''';
  returns: ;

  :---:

  outputs: time'', signallingEscrowState'', governanceState''', governanceValues''', proposal';
  returns: daoPayoff, staker1Payoff, staker2Payoff;
|]

-- Like DaoStaker game but with signal about actual value instead of known value
dao2StakersSignalGame :: (Eq a, Show a)
                => GovernanceParams
                -> Agent
                -> (Agent, Agent)
                -> [Double]
                -> OpenGame StochasticStatefulOptic
                            StochasticStatefulContext
                            '[Kleisli Stochastic (CurrentProposal a) AllDAOActions,
                              Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
                              Kleisli Stochastic (SignalProposal, OpportunityCosts) Double]
                            '[[DiagnosticInfoBayesian
                                 (CurrentProposal a) AllDAOActions],
                              [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
                              [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double]]
                            (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
                             GovernanceState, GovernanceValues, CurrentProposal a,
                             (OpportunityCosts, OpportunityCosts), (SignalProposal,SignalProposal))
                            ()
                            (TimeAbsolute, SignallingEscrowState, GovernanceState,
                             GovernanceValues, CurrentProposal a)
                            (Payoff, Payoff, Payoff)
dao2StakersSignalGame governanceParams daoAgent (stakingAgent1, stakingAgent2) actionSpaceStaker
  = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;

  :-----:

  // DAO chooses & implements move
  inputs: time, governanceState, proposal;
  feedback: ;
  operation: daoVoting governanceParams daoAgent;
  outputs: time', proposal';
  returns: daoPayoff;

  // Staker 1 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, fst signals, fst opportunityCostFactors;
  feedback: ;
  operation: stakingSignalGame governanceParams stakingAgent1 actionSpaceStaker;
  outputs: signallingEscrowState', governanceState', governanceValues';
  returns: staker1Payoff;

  // Staker 2 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState', governanceState', governanceValues', snd signals, snd opportunityCostFactors;
  feedback: ;
  operation: stakingSignalGame governanceParams stakingAgent2 actionSpaceStaker;
  outputs: signallingEscrowState'', governanceState'', governanceValues'';
  returns: staker2Payoff;

  // Both agents wait for time to pass
  inputs: globalLidoState, time', signallingEscrowState'', governanceState'', governanceValues'';
  feedback: ;
  operation: forwardFunction (\(globalLidoState, time', signallingEscrowState', governanceState', governanceValues') -> transitionWithTime globalLidoState governanceParams time' signallingEscrowState' governanceState' governanceValues');
  outputs: time'', governanceState''', governanceValues''';
  returns: ;


  :-----:

  outputs: time'', signallingEscrowState'', governanceState''', governanceValues''', proposal';
  returns: daoPayoff, staker1Payoff, staker2Payoff;
|]

-- Like dao2StakersSignalGame but second player observes action by first player
dao2StakersSignalFollowerGame
  :: (Show a, Eq a) =>
     GovernanceParams
     -> Agent
     -> (Agent, Agent)
     -> [Double]
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[Kleisli Stochastic (CurrentProposal a) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli
              Stochastic (SignalProposal, OpportunityCosts, Double) Double]
          '[[DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian
               (SignalProposal, OpportunityCosts, Double) Double]]
          (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
           GovernanceState, GovernanceValues, CurrentProposal a,
           (OpportunityCosts, OpportunityCosts),
           (SignalProposal, SignalProposal))
          ()
          (TimeAbsolute, SignallingEscrowState, GovernanceState,
           GovernanceValues, CurrentProposal a)
          (Double, Double, Double)
dao2StakersSignalFollowerGame governanceParams daoAgent (stakingAgent1, stakingAgent2) actionSpaceStaker
  = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;

  :-----:

  // DAO chooses & implements move
  inputs: time, governanceState, proposal;
  feedback: ;
  operation: daoVoting governanceParams daoAgent;
  outputs: time', proposal';
  returns: daoPayoff;

  // Staker 1 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, fst signals, fst opportunityCostFactors;
  feedback: ;
  operation: stakingSignalLeaderGame governanceParams stakingAgent1 actionSpaceStaker;
  outputs: signallingEscrowState', governanceState', governanceValues', stakedByPlayer1;
  returns: staker1Payoff;

  // Staker 2 chooses & implements move
  inputs: globalLidoState, time, signallingEscrowState', governanceState', governanceValues', snd signals, snd opportunityCostFactors, stakedByPlayer1;
  feedback: ;
  operation: stakingSignalFollowerGame governanceParams stakingAgent2 actionSpaceStaker;
  outputs: signallingEscrowState'', governanceState'', governanceValues'';
  returns: staker2Payoff;

  // Both agents wait for time to pass
  inputs: globalLidoState, time', signallingEscrowState'', governanceState'', governanceValues'';
  feedback: ;
  operation: forwardFunction (\(globalLidoState, time', signallingEscrowState', governanceState', governanceValues') -> transitionWithTime globalLidoState governanceParams time' signallingEscrowState' governanceState' governanceValues');
  outputs: time'', governanceState''', governanceValues''';
  returns: ;


  :-----:

  outputs: time'', signallingEscrowState'', governanceState''', governanceValues''', proposal';
  returns: daoPayoff, staker1Payoff, staker2Payoff;
|]

-- Add one version where there is a clear follower/leader
