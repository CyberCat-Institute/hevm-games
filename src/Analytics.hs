{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Analytics
   where

import ActionSpaces
import Components
import Model
import Payoffs
import SupportFunctions
import Types
import Components
import Parameterization
import Strategies

import OpenGames.Engine.Engine hiding (Payoff)
import OpenGames.Preprocessor
import OpenGames.Engine.Diagnostics

{------------------------------------------------
Contains the analytics to be executed
There are 5 types of analyses:
1. (Bayesian) Nash eq. checks with printout
2. (Bayesian) Nash eq. checks (Boolean condition)
3. Simulations of play
4. Simulation of next state
5. Parameter bounds searches
-------------------------------------------------}

------------------------------------------------------
-- 0. Helper functionality for parameter bounds search
------------------------------------------------------

searchBounds :: (Ord a) => [a] -> (a -> Bool) -> (a, a, Maybe (a, a))
searchBounds grid predicate = case filter predicate grid of {
  [] -> (minPossible, maxPossible, Nothing);
  xs -> (minPossible, maxPossible, Just (minimum xs, maximum xs))
}
  where minPossible = minimum grid
        maxPossible = maximum grid

printSearchBounds :: (Ord a, Show a) => String -> String -> String -> [a] -> (a -> Bool) -> IO ()
printSearchBounds modelName strategyName paramName grid predicate = do {
  putStrLn $ unwords ["Model:", modelName, "Strategy:", strategyName, "Parameter:", paramName];
  case searchBounds grid predicate of {
    (minPossible, maxPossible, Nothing) -> putStrLn $ unwords ["Minimum searched:", show minPossible, "Maximum searched:", show maxPossible, "No passing values"];
    (minPossible, maxPossible, Just (minPassing, maxPassing)) -> putStrLn $ unwords ["Minimum searched:", show minPossible, "Maximum searched:", show maxPossible, "Minimum passing:", show minPassing, "Maximum passing:", show maxPassing];
  }
}


--------------------------
-- 1. Equilibrium checking
--------------------------

-- 1.1 Staking game
equilibriumStakingGame GameParameters{..} strategy = evaluate game strategy ctxt
   where
     game = stakingGame governanceParameters agentStaker stakerMoves
     ctxt = StochasticStatefulContext (pure ((),(globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor))) (\_ (_,govState,_) ->
                                                                                                                                                       let payoff = snd (evaluateProposal proposal)
                                                                                                                                                           in pure (payoff))

-- Fix the context so that a positive transfer means different payoff
equilibriumStakingGame2 GameParameters{..} strategy = evaluate game strategy ctxt
   where
     game = stakingGame governanceParameters agentStaker stakerMoves
     ctxt = StochasticStatefulContext (pure ((),(globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor))) (\_ (newEscrow,_,_) ->
                                                        let Proposal (ProposalModel { benefitToLDOHolders = ldo, benefitToStETHHolders = payoff }) = currentProposal proposal
                                                            lockedStETH' = sumAllAccounts $ lockedStETH newEscrow
                                                            support' =  lockedStETH' / (totalStETHSupply globalLidoState)
                                                            in if support' > secondSealRageQuitSupport governanceParameters
                                                                  then pure 0
                                                                  else pure payoff)

-- Print the equilibrium / deviating strategy information
printEquilibriumStakingGame gameParameters strategy = generateIsEq $ equilibriumStakingGame gameParameters strategy
printEquilibriumStakingGame2 gameParameters strategy = generateIsEq $ equilibriumStakingGame2 gameParameters strategy

-- Extract Boolean equilibrium condition
eqEquilibriumStakingGame gameParameters strategy = generateEquilibrium $ equilibriumStakingGame gameParameters strategy
eqEquilibriumStakingGame2 gameParameters strategy = generateEquilibrium $ equilibriumStakingGame2 gameParameters strategy

-- Print the full output
printOutputStakingGame gameParameters strategy = generateOutput $ equilibriumStakingGame gameParameters strategy
printOutputStakingGame2 gameParameters strategy = generateOutput $ equilibriumStakingGame2 gameParameters strategy


-- 1.2 DAO-staker game

printEquilibriumDaoStakerGame gameParameters strategy
  = let GameParameters{..} = gameParameters
     in generateIsEq $ evaluate (daoStakerGame governanceParameters agentDao agentStaker stakerMoves)
                                strategy
                                (StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCosts, agentRiskFactor)))
                                                           (\() (_, _, _, _, currentProposal) -> pure (evaluateProposal currentProposal)))

-- 1.3 modelBasic

-- Definition of evaluation
equilibriummodelBasicGame GameParameters{..} strategy = evaluate game strategy ctxt
   where
     game = modelBasic governanceParameters agentDao voteDAO agentStaker stakerMoves proposalPayoffs
     ctxt = StochasticStatefulContext (pure ((),(globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor))) (\_ _ -> pure ())

-- Print the equilibrium / deviating strategy information
printEquilibriummodelBasicGame gameParameters strategy = generateIsEq $ equilibriummodelBasicGame gameParameters strategy

-- Extract Boolean equilibrium condition
eqEquilibriumModelBasicGame gameParameters strategy = generateEquilibrium $ equilibriummodelBasicGame gameParameters strategy

-- Parameter bounds search
parameterSearchModelBasic :: IO ()
parameterSearchModelBasic = do {
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyVetoSignalling" "firstSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x, secondSealRageQuitSupport = x + 0.09}}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyVetoSignalling stakerMoves params;
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyRageQuit" "firstSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x}}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyRageQuit stakerMoves params;
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyVetoSignalling" "secondSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {secondSealRageQuitSupport = x}}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyVetoSignalling stakerMoves params;
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyRageQuit" "secondSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x - 0.09, secondSealRageQuitSupport = x}}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyRageQuit stakerMoves params;
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyVetoSignalling" "opportunityCosts" [0, 0.0005 .. 0.01] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {opportunityCosts = x}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyVetoSignalling stakerMoves params;
  printSearchBounds "modelBasic" "modelBasicOptimalStrategyRageQuit" "opportunityCosts" [0, 0.0005 .. 0.01] $ \x ->
    let params = (modelBasicGameParametersTestProposal 1 (-1)) {opportunityCosts = x}
     in eqEquilibriumModelBasicGame params $ modelBasicOptimalStrategyRageQuit stakerMoves params;
}

-- 1.4 modelBasic2

-- Definition of evaluation
-- NOTE feeds the final proposal state back into the system
equilibriumModelBasic2Game GameParameters{..} strategy = evaluate game strategy ctxt
  where game = modelBasic2 governanceParameters agentDao agentStaker stakerMoves
        ctxt = (StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCosts, agentRiskFactor))) (\() (_, _, _, _, x) -> pure x))

-- Definition of evaluation with a tailored continuation _proposalContinuation_
-- NOTE In this way we can short-cut the future evolution of a proposal
equilibriumModelBasic2GameContinuation GameParameters{..} strategy proposalContinuation = evaluate game strategy ctxt
  where game = modelBasic2 governanceParameters agentDao agentStaker stakerMoves
        ctxt = (StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, proposal, opportunityCosts, agentRiskFactor))) (\() x -> pure (proposalContinuation x)))

-- Print the equilibrium / deviating strategy information
printEquilibriumModelBasic2Game gameParameters strategy = generateIsEq $ equilibriumModelBasic2Game gameParameters strategy
printEquilibriumModelBasic2GameContinuation gameParameters strategy proposalContinuation = generateIsEq $ equilibriumModelBasic2GameContinuation gameParameters strategy proposalContinuation

-- Extract Boolean equilibrium condition
eqEquilibriumModelBasic2Game gameParameters strategy = generateEquilibrium $ equilibriumModelBasic2Game gameParameters strategy
eqEquilibriumModelBasic2GameContinuation gameParameters strategy proposalContinuation = generateEquilibrium $ equilibriumModelBasic2GameContinuation gameParameters strategy proposalContinuation

-- Print full output
printOutputModelBasic2Game gameParameters strategy = generateOutput $ equilibriumModelBasic2Game gameParameters strategy
printOutputModelBasic2GameContinuation gameParameters strategy proposalContinuation = generateOutput $ equilibriumModelBasic2GameContinuation gameParameters strategy proposalContinuation

-- 1.5 modelHeterogenous

-- Definition of evaluation
equilibriumModelHeterogenousGame GameParameters{..}  strategy = evaluate game strategy ctxt
  where game = modelHeterogenous governanceParameters agentDao ("StakingAgent1", "StakingAgent2") stakerMoves
        ctxt = StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, proposal, (opportunityCosts, opportunityCosts2), (agentRiskFactor, agentRiskFactor2))))
                                         (\() (_, _, _, _, finalProposal) -> pure finalProposal)

-- Print the equilibrium / deviating strategy information
printEquilibriumModelHeterogenousGame gameParameters  strategy
  = generateIsEq $ equilibriumModelHeterogenousGame gameParameters strategy

-- Extract Boolean equilibrium condition
eqEquilibriumModelHeterogenousGame gameParameters  strategy = generateEquilibrium $ equilibriumModelHeterogenousGame gameParameters  strategy

-- Parameter bounds search
parameterSearchModelBasic2 :: IO ()
parameterSearchModelBasic2 = do {
  printSearchBounds "modelBasic2" "modelBasic2ProposeExecuteOptimalMinStakingStrategy" "firstSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasic2GameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x, secondSealRageQuitSupport = x + 0.09}}
     in eqEquilibriumModelBasic2Game params $ modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params;
  printSearchBounds "modelBasic2" "modelBasic2ProposeExecuteOptimalMinStakingStrategy" "secondSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBasic2GameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x - 0.09, secondSealRageQuitSupport = x}}
     in eqEquilibriumModelBasic2Game params $ modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params;
  printSearchBounds "modelBasic2" "modelBasic2ProposeExecuteOptimalMinStakingStrategy" "opportunityCosts" [0, 0.0005 .. 0.01] $ \x ->
    let params = (modelBasic2GameParametersTestProposal 1 (-1)) {opportunityCosts = x}
     in eqEquilibriumModelBasic2Game params $ modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params;
}

-- 1.6 modelBayesian

-- Definition of evaluation
equilibriumModelBayesianGame GameParameters{..}  strategy prob0EventPlayer1 prob0EventPlayer2 = evaluate game strategy ctxt
  where game = modelBayesian governanceParameters agentDao ("StakingAgent1", "StakingAgent2") stakerMoves prob0EventPlayer1 prob0EventPlayer2
        ctxt = StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, proposal, (opportunityCosts, opportunityCosts2))))
                                         (\() (_, _, _, _, finalProposal) -> pure finalProposal)

-- Print the equilibrium / deviating strategy information
printEquilibriumModelBayesianGame gameParameters strategy p1 p2
  = generateIsEq $ equilibriumModelBayesianGame gameParameters strategy p1 p2

-- Extract Boolean equilibrium condition
eqEquilibriumModelBayesianGame gameParameters strategy p1 p2 = generateEquilibrium $ equilibriumModelBayesianGame gameParameters strategy p1 p2

-- Parameter bounds search
parameterSearchModelBayesian :: IO ()
parameterSearchModelBayesian = do {
  printSearchBounds "modelBayesian" "modelBayesianProposeExecuteOptimalMinStakingStrategy2" "firstSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBayesianGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x, secondSealRageQuitSupport = x + 0.09}}
     in eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.5 0.5;
  printSearchBounds "modelBayesian" "modelBayesianProposeExecuteOptimalMinStakingStrategy2" "secondSealRageQuitSupport" [0, 0.05 .. 10.0] $ \x ->
    let params = (modelBayesianGameParametersTestProposal 1 (-1)) {governanceParameters = defaultGovernanceParams {firstSealRageQuitSupport = x - 0.09, secondSealRageQuitSupport = x}}
     in eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.5 0.5;
  printSearchBounds "modelBayesian" "modelBayesianProposeExecuteOptimalMinStakingStrategy2" "opportunityCosts" [0, 0.0005 .. 0.01] $ \x ->
    let params = (modelBayesianGameParametersTestProposal 1 (-1)) {opportunityCosts = x}
     in eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.5 0.5;
}

-- 1.5 ModelBayesianEndogenousSignal

-- Definition of evaluation
equilibriumModelBayesianEndogenousSignalGame GameParameters{..}  strategy signalStrengthPlayer1 signalStrengthPlayer2 = evaluate game strategy ctxt
  where game = modelBayesianEndogenousSignal governanceParameters agentDao ("StakingAgent1", "StakingAgent2") stakerMoves signalStrengthPlayer1 signalStrengthPlayer2
        ctxt = StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, (opportunityCosts, opportunityCosts2))))
                                         (\() (_, _, _, _, finalProposal) -> pure finalProposal)

-- Print the equilibrium / deviating strategy information
printEquilibriumModelBayesianEndogenousSignalGame gameParameters strategy p1 p2
  = generateIsEq $ equilibriumModelBayesianEndogenousSignalGame gameParameters strategy p1 p2

-- Extract Boolean equilibrium condition
eqEquilibriumModelBayesianEndogenousSignalGame gameParameters strategy p1 p2 = generateEquilibrium $ equilibriumModelBayesianEndogenousSignalGame gameParameters strategy p1 p2

-- 1.5 ModelLeaderFollowerEndogenousSignal

-- Definition of evaluation
equilibriumModelLeaderFollowerEndogenousSignalGame GameParameters{..}  strategy signalStrengthPlayer1 signalStrengthPlayer2 = evaluate game strategy ctxt
  where game = modelLeaderFollowerEndogenousSignal governanceParameters agentDao ("StakingAgent1", "StakingAgent2") stakerMoves signalStrengthPlayer1 signalStrengthPlayer2
        ctxt = StochasticStatefulContext (pure ((), (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues, (opportunityCosts, opportunityCosts2))))
                                         (\() (_, _, _, _, finalProposal) -> pure finalProposal)

-- Print the equilibrium / deviating strategy information
printEquilibriumModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2
  = generateIsEq $ equilibriumModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2

-- Extract Boolean equilibrium condition
eqEquilibriumModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2 = generateEquilibrium $ equilibriumModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2

-- Print full output
printOutputModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2
  = generateOutput $ equilibriumModelLeaderFollowerEndogenousSignalGame gameParameters strategy p1 p2


-----------------
-- 2. Simulations
-----------------

-- 2.1 Staking game

-- Prepare games for testing
simulationStakingGame GameParameters{..} strategy = play game strategy
   where
     game = stakingGame governanceParameters agentStaker stakerMoves

-- Extract the next state from the game
nextStateSimulationStakingGame gameParameters strategy = nextState $ simulationStakingGame gameParameters strategy
nextStateSimulationDaoStakerGame GameParameters{..} strategy = nextState $ play (daoStakerGame governanceParameters agentDao agentStaker stakerMoves) strategy

-- 2.2 modelBasic

-- Prepare games for testing
simulationmodelBasicGame GameParameters{..} strategy = play game strategy
   where
     game = modelBasic governanceParameters agentDao voteDAO agentStaker stakerMoves proposalPayoffs

-- Extract the next state from the game
nextStateSimulationModelBasicGame gameParameters strategy = nextState $ simulationmodelBasicGame gameParameters strategy

-- ModelBasic2
-- Prepare games for testing
simulationModelBasic2Game GameParameters{..} strategy = play game strategy
  where game = modelBasic2 governanceParameters agentDao agentStaker stakerMoves

-- Extract the next state from the game
nextStateSimulationModelBasic2Game gameParameters strategy = nextState $ simulationModelBasic2Game gameParameters strategy

