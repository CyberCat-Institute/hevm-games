{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Model
  where

import ActionSpaces
import Components
import SupportFunctions
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-------------------
Contains full models
--------------------}


--------------
-- First model
--------------

-- 1. Inputs a fixed proposal
-- 2. Representative LDO holder decides whether or not to submit
-- 3. Staking component runs
-- 4. Proposal executes if able
-- Note: Does not include a second staking phase, so does not allow unstaking later
modelBasic :: GovernanceParams
           -> Agent
           -> [ProposalVote]
           -> Agent
           -> [Double]
           -> (CurrentProposal ProposalModel -> (Double, Double)) 
           -> OpenGame StochasticStatefulOptic StochasticStatefulContext
                       '[ Kleisli Stochastic (Proposal ProposalModel) ProposalVote
                        , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
                       '[ [DiagnosticInfoBayesian (Proposal ProposalModel) ProposalVote]
                        , [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]]
                       (GlobalLidoState, TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, Proposal ProposalModel, OpportunityCosts, RiskFactor)
                       ()
                       (SignallingEscrowState, TimeAbsolute, GovernanceState, GovernanceValues, CurrentProposal ProposalModel)
                       ()
modelBasic governanceParams daoAgent actionSpaceDAO stakingAgent actionSpaceStaker payoffFunction = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactor, agentRiskFactor ;
  feedback: ;

  :---:

  // DAO decision to submit proposal
  inputs: proposal;
  feedback: ;
  operation: dependentDecision daoAgent (const actionSpaceDAO);
  outputs: daoVote;
  returns: benefitToLDOHolders;

  // DAO submits proposal, and immediately cancels if voted against submission
  inputs: governanceState, time, proposal, daoVote;
  feedback: ;
  operation: forwardFunction (\(governanceState, time, proposal, daoVote) -> daoSubmitProposal' governanceState time proposal daoVote);
  outputs: submittedProposal;
  returns: ;

  // Staking agent moves & DG subsystem runs
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, submittedProposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: stakingGame governanceParams stakingAgent actionSpaceStaker;
  outputs: signallingEscrowState', governanceState', governanceValues';
  returns: benefitToStETHHolders;

  // Both agents wait for time to pass
  inputs: globalLidoState, time, signallingEscrowState', governanceState', governanceValues';
  feedback: ;
  operation: forwardFunction (\(globalLidoState, time, signallingEscrowState', governanceState', governanceValues') -> transitionWithTime globalLidoState governanceParams time signallingEscrowState' governanceState' governanceValues');
  outputs: time', governanceState'', governanceValues'';
  returns: ;

  // DAO executes proposal if able, waiting for time to pass if necessary
  inputs: time', governanceState'', submittedProposal;
  feedback: ;
  operation: fromFunctions (\(time', governanceState'', submittedProposal) -> daoExecuteProposal governanceParams time' governanceState'' submittedProposal) id;
  outputs: time'', updatedProposal;
  returns: ;

  // Get payoffs resulting from an executed proposal
  inputs: updatedProposal;
  feedback: ;
  operation: forwardFunction payoffFunction ;
  outputs: benefitToLDOHolders, benefitToStETHHolders;
  returns: ;

  :---:

  outputs: signallingEscrowState', time'', governanceState'', governanceValues'', updatedProposal;
  returns: ;
|]

-- modelBasic2 is a refinement of modelBasic that is designed in a more modular way
modelBasic2 :: GovernanceParams
            -> Agent
            -> Agent
            -> [Double]
            -> OpenGame StochasticStatefulOptic StochasticStatefulContext
                        '[ Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                         , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                         , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                         , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
                        '[ [DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions]
                         , [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
                         , [DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions]
                         , [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]]
                        (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
                         GovernanceState, GovernanceValues, CurrentProposal ProposalModel, OpportunityCosts, RiskFactor)
                        ()
                        (TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal ProposalModel)
                        (CurrentProposal ProposalModel)
modelBasic2 governanceParams daoAgent stakingAgent actionSpaceStaker = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;

  :---:

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: daoStakerGame governanceParams daoAgent stakingAgent actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0; // Payoffs in this component are handled by implicit state

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactor, agentRiskFactor;
  feedback: ;
  operation: daoStakerGame governanceParams daoAgent stakingAgent actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: benefitToLDOHolders, benefitToStETHHolders;

  inputs: ;
  feedback: benefitToLDOHolders, benefitToStETHHolders;
  operation: fromFunctions id evaluateProposal;
  outputs: ;
  returns: finalProposal;

  :---:

  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]

-- Variant of modelBasic2 with 2 staking agents, ie. representatives of 2 classes of stakers
modelHeterogenous :: GovernanceParams
                  -> Agent
                  -> (Agent, Agent)
                  -> [Double]
                  -> OpenGame
                       StochasticStatefulOptic
                       StochasticStatefulContext
                       '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
                         Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                         Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                         Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
                         Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                         Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
                       '[[DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
                         [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                         [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                         [DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
                         [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                         [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]]
                       (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
                        GovernanceState, GovernanceValues, CurrentProposal ProposalModel,
                        (OpportunityCosts, OpportunityCosts), (RiskFactor, RiskFactor))
                       ()
                       (TimeAbsolute, SignallingEscrowState, GovernanceState,
                        GovernanceValues, CurrentProposal ProposalModel)
                       (CurrentProposal ProposalModel)
modelHeterogenous governanceParams daoAgent stakingAgents actionSpaceStaker = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, agentRiskFactors;
  feedback: ;

  :---:

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, agentRiskFactors;
  feedback: ;
  operation: dao2StakersGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0, 0; // Payoffs in this component are handled by implicit state

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactors, agentRiskFactors;
  feedback: ;
  operation: dao2StakersGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: evaluateProposalLDOs finalProposal, evaluateProposalStakers finalProposal, evaluateProposalStakers finalProposal ;

  :---:

  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]


-- Expanded variant of modelHeterogenous
-- This is a proper Bayesian game with private and potentially asymmetric risk factors
modelBayesian :: GovernanceParams
              -> Agent
              -> (Agent, Agent)
              -> [Double]
              -> Double
              -> Double
              -> OpenGame
                   StochasticStatefulOptic
                   StochasticStatefulContext
                   '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
                     Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                     Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                     Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
                     Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double,
                     Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
                   '[[DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
                     [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                     [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                     [DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
                     [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double],
                     [DiagnosticInfoBayesian (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]]
                   (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
                    GovernanceState, GovernanceValues, CurrentProposal ProposalModel,
                    (OpportunityCosts, OpportunityCosts))
                   ()
                   (TimeAbsolute, SignallingEscrowState, GovernanceState,
                    GovernanceValues, CurrentProposal ProposalModel)
                   (CurrentProposal ProposalModel)
modelBayesian governanceParams daoAgent stakingAgents actionSpaceStaker p1 p2 = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors ;
  feedback: ;

  :---:

  // nature determines agentRiskFactor staker1
  inputs: ;
  feedback: ;
  operation: nature (distributionAgentRisks p1) ;
  outputs: agentRiskFactor1;
  returns: ;

  // nature determines agentRiskFactor staker2
  inputs: ;
  feedback: ;
  operation: nature (distributionAgentRisks p2) ;
  outputs: agentRiskFactor2;
  returns: ;

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, (agentRiskFactor1, agentRiskFactor2) ;
  feedback: ;
  operation: modelHeterogenous governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'' ;
  returns: finalProposal ;

  :---:

  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]

-- Variant of modelBasic2 with 2 heterogenous stakers and proper objective uncertainty about the proposals
modelHeterogenousUncertainty
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
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli Stochastic (CurrentProposal a) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double]
          '[[DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double]]
          (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
           GovernanceState, GovernanceValues, CurrentProposal a,
           (OpportunityCosts, OpportunityCosts),
           (SignalProposal, SignalProposal))
          ()
          (TimeAbsolute, SignallingEscrowState, GovernanceState,
           GovernanceValues, CurrentProposal a)
          (CurrentProposal ProposalModel)
modelHeterogenousUncertainty governanceParams daoAgent stakingAgents actionSpaceStaker = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;
  :---:

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;
  operation: dao2StakersSignalGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0, 0; // State hack

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactors, signals;
  feedback: ;
  operation: dao2StakersSignalGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: evaluateProposalLDOs finalProposal, evaluateProposalStakers finalProposal, evaluateProposalStakers finalProposal ;

  :---:
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]


-- Variant of modelBayesian with endogenous signal creation 
modelBayesianEndogenousSignal :: GovernanceParams
     -> Agent
     -> (Agent, Agent)
     -> [Double]
     -> Double
     -> Double
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double]
          '[[DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (CurrentProposal ProposalModel) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double]]
          (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
           GovernanceState, GovernanceValues, (OpportunityCosts, OpportunityCosts))
          ()
          (TimeAbsolute, SignallingEscrowState, GovernanceState,
           GovernanceValues, CurrentProposal ProposalModel)
          (CurrentProposal ProposalModel)
modelBayesianEndogenousSignal governanceParams daoAgent stakingAgents actionSpaceStaker p1 p2 = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, opportunityCostFactors;
  feedback: ;
  :---:

  // nature determines proposal benefits for stakers
  inputs:   ;
  feedback: ;
  operation: nature randomProposal ;
  outputs: proposal;
  returns: ;

  // nature determines signal for staker1
  inputs:  proposal ;
  feedback: ;
  operation: liftStochasticForward (distributionSignals p1) ;
  outputs: signal1;
  returns: ;

  // nature determines signal for staker2
  inputs: proposal ;
  feedback: ;
  operation: liftStochasticForward (distributionSignals p2) ;
  outputs: signal2;
  returns: ;

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, (signal1, signal2);
  feedback: ;
  operation: dao2StakersSignalGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0, 0; // State hack

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactors, (signal1, signal2);
  feedback: ;
  operation: dao2StakersSignalGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: evaluateProposalLDOs finalProposal, evaluateProposalStakers finalProposal, evaluateProposalStakers finalProposal ;

  :---:
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]

-- Variant of modelBayesian with 2 heterogenous stakers, one leader and the other follower 
-- and proper objective uncertainty about the proposals
modelLeaderFollower
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
              Stochastic (SignalProposal, OpportunityCosts, Double) Double,
            Kleisli Stochastic (CurrentProposal a) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli
              Stochastic (SignalProposal, OpportunityCosts, Double) Double]
          '[[DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian
               (SignalProposal, OpportunityCosts, Double) Double],
            [DiagnosticInfoBayesian (CurrentProposal a) AllDAOActions],
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
          (CurrentProposal ProposalModel)
modelLeaderFollower governanceParams daoAgent stakingAgents actionSpaceStaker = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;
  :---:

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, signals;
  feedback: ;
  operation: dao2StakersSignalFollowerGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0, 0; // State hack

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactors, signals;
  feedback: ;
  operation: dao2StakersSignalFollowerGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: evaluateProposalLDOs finalProposal, evaluateProposalStakers finalProposal, evaluateProposalStakers finalProposal ;

  :---:
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]

-- Variant of modelLeaderFollower with endogenous signal creation 
modelLeaderFollowerEndogenousSignal
  :: GovernanceParams
     -> Agent
     -> (Agent, Agent)
     -> [Double]
     -> Double
     -> Double
     -> OpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli
              Stochastic (SignalProposal, OpportunityCosts, Double) Double,
            Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions,
            Kleisli Stochastic (SignalProposal, OpportunityCosts) Double,
            Kleisli
              Stochastic (SignalProposal, OpportunityCosts, Double) Double]
          '[[DiagnosticInfoBayesian
               (CurrentProposal ProposalModel) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian
               (SignalProposal, OpportunityCosts, Double) Double],
            [DiagnosticInfoBayesian
               (CurrentProposal ProposalModel) AllDAOActions],
            [DiagnosticInfoBayesian (SignalProposal, OpportunityCosts) Double],
            [DiagnosticInfoBayesian
               (SignalProposal, OpportunityCosts, Double) Double]]
          (GlobalLidoState, TimeAbsolute, SignallingEscrowState,
           GovernanceState, GovernanceValues,
           (OpportunityCosts, OpportunityCosts))
          ()
          (TimeAbsolute, SignallingEscrowState, GovernanceState,
           GovernanceValues, CurrentProposal ProposalModel)
          (CurrentProposal ProposalModel)
modelLeaderFollowerEndogenousSignal governanceParams daoAgent stakingAgents actionSpaceStaker p1 p2 = [opengame|
  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, opportunityCostFactors;
  feedback: ;
  :---:

  // nature determines proposal benefits for stakers
  inputs:   ;
  feedback: ;
  operation: nature randomProposal ;
  outputs: proposal;
  returns: ;

  // nature determines signal for staker1
  inputs:  proposal ;
  feedback: ;
  operation: liftStochasticForward (distributionSignals p1) ;
  outputs: signal1;
  returns: ;

  // nature determines signal for staker2
  inputs: proposal ;
  feedback: ;
  operation: liftStochasticForward (distributionSignals p2) ;
  outputs: signal2;
  returns: ;

  inputs: globalLidoState, time, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCostFactors, (signal1, signal2);
  feedback: ;
  operation: dao2StakersSignalFollowerGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time', signallingEscrowState', governanceState', governanceValues', proposal';
  returns: 0, 0, 0; // State hack

  inputs: globalLidoState, time', signallingEscrowState', governanceState', governanceValues', proposal', opportunityCostFactors, (signal1, signal2);
  feedback: ;
  operation: dao2StakersSignalFollowerGame governanceParams daoAgent stakingAgents actionSpaceStaker;
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: evaluateProposalLDOs finalProposal, evaluateProposalStakers finalProposal, evaluateProposalStakers finalProposal ;

  :---:
  outputs: time'', signallingEscrowState'', governanceState'', governanceValues'', proposal'';
  returns: finalProposal;
|]

