{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Strategies
  where

import Payoffs
import SupportFunctions
import Types hiding (GameParametersEVM(..))

import OpenGames.Engine.Engine

import Data.List (minimum,maximum)

{--------------------------------------------------------
Defines the player strategies used for analysis of models
---------------------------------------------------------}

--------------------
-- 1. DAO strategies
--------------------

-- Vote yes irrespective of proposal
daoVotingYes :: Kleisli Stochastic (Proposal ProposalModel) ProposalVote
daoVotingYes = pureAction VoteYes

-- Vote yes to any proposal with positive benefit to LDO holders
daoVotingOptimally :: Kleisli Stochastic (Proposal ProposalModel) ProposalVote
daoVotingOptimally = Kleisli react
  where
    react proposal =
      let Proposal p = proposal
        in if p.benefitToLDOHolders < 0
              then playDeterministically VoteNo
              else playDeterministically VoteYes

-- Vote to submit proposal
daoVotingSubmitProposal :: Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
daoVotingSubmitProposal = pureAction SubmitProposal

-- Vote to cancel proposal
daoVotingCancelProposal :: Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
daoVotingCancelProposal = pureAction CancelProposal

-- Vote to execute proposal
daoVotingExecuteProposal :: Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
daoVotingExecuteProposal = pureAction ExecuteProposal


-----------------------
-- 2. Staker strategies
-----------------------

-- 2.1. Symmetric information

-- Stake the maximum possible amount
-- NOTE We add an unused variable so that we can unify the type of strategy
maxStakingStrategy :: [Double]
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
maxStakingStrategy actionSpace = pureAction (maximum actionSpace)

-- Stake the minimum possible amount, aka unstake the maximum possible amount
-- NOTE We add an unused variable so that we can unify the type of strategy
minStakingStrategy :: [Double] -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
minStakingStrategy actionSpace = pureAction (minimum actionSpace)

-- Stake the minimum amount that will cause a transition to the rage quit state
-- NOTE that this is contingent on the current structure of the game
optimalStakingStrategyRageQuit :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategyRageQuit actionSpace par = Kleisli react
  where
    react (proposal, _, _) =
      let Proposal p = currentProposal proposal
        in if p.benefitToStETHHolders >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically (pivotalForRageQuit par actionSpace)

-- Stake the minimum amount that will cause a transition to the veto signalling state
-- NOTE that this is contingent on the current structure of the game
optimalStakingStrategyVetoSignalling :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategyVetoSignalling actionSpace par = Kleisli react
  where
    react (proposal, _, _) =
      let Proposal p = currentProposal proposal
        in if p.benefitToStETHHolders >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically (pivotalForVetoSignalling par actionSpace)

-- Stake the minimum amount that will cause a transition to the rage quit state
-- when combined with another staking agent
-- NOTE that this is contingent on the current structure of the game
optimalStakingStrategy2StakersRageQuit :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategy2StakersRageQuit actionSpace par = Kleisli react
  where
    react (proposal, _, _) =
      let Proposal p = currentProposal proposal
        in if p.benefitToStETHHolders >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically ((pivotalForRageQuit par actionSpace)/2)

-- Stake the minimum amount that will cause a transition to the veto signalling state
-- when combined with another staking agent
-- NOTE that this is contingent on the current structure of the game
optimalStakingStrategy2StakersVetoSignalling :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategy2StakersVetoSignalling actionSpace par = Kleisli react
  where
    react (proposal,_,_) =
      let Proposal p = currentProposal proposal
        in if p.benefitToStETHHolders >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)


-- 2.2. Asymmetric information

-- Stake the minimum amount that will cause a transition to the veto signalling state
-- when combined with another staking agent with private information
-- In this case, we tie the strategy to explicit values
optimalStakingStrategy2StakersAsymmetricVetoSignalling :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategy2StakersAsymmetricVetoSignalling actionSpace par = Kleisli react
  where
    react (proposal,_,risk) =
      let Proposal p = currentProposal proposal
          in if p.benefitToStETHHolders >= 0
                then playDeterministically (minimum actionSpace)
                else if risk == 0
                        then playDeterministically (minimum actionSpace)
                        else playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)

-- Variant where we stake enough to reach the veto signalling state single-handedly
optimalStakingStrategy2StakersAsymmetricVetoSignalling2 :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
optimalStakingStrategy2StakersAsymmetricVetoSignalling2 actionSpace par = Kleisli react
  where
    react (proposal,_,risk) =
      let Proposal p = currentProposal proposal
          in if p.benefitToStETHHolders >= 0
                then playDeterministically (minimum actionSpace)
                else if risk == 0
                        then playDeterministically (minimum actionSpace)
                        else playDeterministically (pivotalForVetoSignalling par actionSpace)

-- 2.3. Asymmetric information with objective uncertainty about values

minStakingSignalStrategy :: [Double] -> Kleisli Stochastic (SignalProposal, OpportunityCosts) Double
minStakingSignalStrategy actionSpace = pureAction (minimum actionSpace)

-- In this case, we tie the strategy to explicit values
optimalStakingStrategy2StakersSignalVetoSignalling :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts) Double
optimalStakingStrategy2StakersSignalVetoSignalling actionSpace par = Kleisli react
  where
    react (signal,_) =
          if signal >= 0
                then playDeterministically (minimum actionSpace)
                else playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)

-- In this case, we tie the strategy to explicit values
-- We provide enough for the escrow to stop single-handedly
optimalStakingStrategy2StakersSignalVetoSignalling2 :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts) Double
optimalStakingStrategy2StakersSignalVetoSignalling2 actionSpace par = Kleisli react
  where
    react (signal,_) =
          if signal >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically (pivotalForVetoSignalling par actionSpace)

-- Choose minimum or maximum
optimalStakingStrategy2StakersMaxMin :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts) Double
optimalStakingStrategy2StakersMaxMin actionSpace par = Kleisli react
  where
    react (signal,_) =
          if signal >= 0
              then playDeterministically (minimum actionSpace)
              else playDeterministically (maximum actionSpace)

-- Ignore signal and play half
optimalStakingStrategy2StakersIgnoreSignal :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts) Double
optimalStakingStrategy2StakersIgnoreSignal actionSpace par = Kleisli react
  where
    react _ = playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)


-- 2.4. Follower / leader game

-- Minimize amounts to be staked
minStakingSignalFollowerStrategy :: [Double] -> Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double
minStakingSignalFollowerStrategy actionSpace = pureAction (minimum actionSpace)

-- Follow own signal
optimalStakingStrategy2StakersMaxMinFollower :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double
optimalStakingStrategy2StakersMaxMinFollower actionSpace par = Kleisli react
  where
    react (signal,_, staked) =
          if signal >= 0
                then playDeterministically (minimum actionSpace)
                else playDeterministically (maximum actionSpace)

-- Follow staked amount by previous player
optimalStakingStrategy2StakersSignalFollower :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double
optimalStakingStrategy2StakersSignalFollower actionSpace par = Kleisli react
  where
    react (signal,_, staked) =
          if staked >= 0
                then playDeterministically (minimum actionSpace)
                else playDeterministically (maximum actionSpace)

-- Ignore signal
optimalStakingStrategy2StakersIgnoreFollower :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double
optimalStakingStrategy2StakersIgnoreFollower actionSpace par = Kleisli react
  where react _ =
           playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)

-- Follow action by other player; and stake half
optimalStakingStrategy2StakersOptimalFollower :: [Double]
                       -> GameParameters a
                       -> Kleisli Stochastic (SignalProposal, OpportunityCosts, Double) Double
optimalStakingStrategy2StakersOptimalFollower actionSpace par = Kleisli react
  where
    react (_signal,_,action) =
          if action <= 0
                then playDeterministically (minimum actionSpace)
                else playDeterministically ((pivotalForVetoSignalling par actionSpace)/2)

-- 2.5. Support functions

-- Helper for choosing an action just above a threshold
smallestLargerThan :: Ord a => a -> [a] -> a
smallestLargerThan x ls =
      let largerElements = [y | y <- ls, y >= x]
      in if null largerElements then minimum ls else minimum largerElements

-- Choose an element that just brings the threshold of rage quit
pivotalForRageQuit :: GameParameters a -> [Double] -> Double
pivotalForRageQuit par actionSpace =
  let globalState      = globalLidoState par
      signallingState  = signallingEscrowState par
      govParams        = governanceParameters par
      currentThreshold = rageQuitSupport globalState signallingState
      targetThreshold  = secondSealRageQuitSupport govParams
      allStETH         = totalStETHSupply globalState
      stETHInEscrow    = sumAllAccounts $ lockedStETH signallingState
      target           = targetThreshold * allStETH - stETHInEscrow
      -- Find the smallest value in actionSpace that would help cross the threshold
      in if currentThreshold >= targetThreshold
            then minimum actionSpace
            else smallestLargerThan target actionSpace

-- Choose an element that just brings the threshold to veto signalling
pivotalForVetoSignalling :: GameParameters a -> [Double] -> Double
pivotalForVetoSignalling par actionSpace =
  let globalState      = globalLidoState par
      signallingState  = signallingEscrowState par
      govParams        = governanceParameters par
      currentThreshold = rageQuitSupport globalState signallingState
      targetThreshold  = firstSealRageQuitSupport govParams
      allStETH         = totalStETHSupply globalState
      stETHInEscrow    = sumAllAccounts $ lockedStETH signallingState
      target           = targetThreshold * allStETH - stETHInEscrow
      -- Find the smallest value in actionSpace that would help cross the threshold
      in if currentThreshold >= targetThreshold
            then minimum actionSpace
            else smallestLargerThan target actionSpace


---------------------------------------------------
-- 3. Aggregating strategies into strategy profiles
---------------------------------------------------

-- 3.1 Staking game

-- Max staking strategy in isolation
maxStakingStrategyTuple :: [Double] -> List '[Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
maxStakingStrategyTuple actionSpace = (maxStakingStrategy actionSpace) :- Nil

-- Min staking strategy in isolation
minStakingStrategyTuple :: [Double] -> List '[Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
minStakingStrategyTuple actionSpace = (minStakingStrategy actionSpace) :- Nil

-- Rage quit strategy in isolation
optimalStakingStrategyRageQuitTuple :: [Double] -> GameParameters a -> List '[Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
optimalStakingStrategyRageQuitTuple actionSpace par = (optimalStakingStrategyRageQuit actionSpace par) :- Nil

-- Veto signalling strategy in isolation
optimalStakingStrategyVetoSignallingTuple :: [Double] -> GameParameters a -> List '[Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
optimalStakingStrategyVetoSignallingTuple actionSpace par = (optimalStakingStrategyVetoSignalling actionSpace par) :- Nil

-- Constant strategy in isolation
parameterizedStakingStrategyTuple :: Double -> List '[Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
parameterizedStakingStrategyTuple x = (pureAction x) :- Nil


-- 3.2 modelBasic

-- DAO votes yes, staker stakes max
modelBasicMaxStakingStrategy :: [Double]
               -> List
                     '[Kleisli Stochastic (Proposal ProposalModel) ProposalVote,
                       Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasicMaxStakingStrategy actionSpace = daoVotingYes :- (maxStakingStrategy actionSpace) :- Nil

-- DAO votes optimally, staker moves optimally for rage quit
modelBasicOptimalStrategyRageQuit :: [Double]
               -> GameParameters a
               -> List
                     '[Kleisli Stochastic (Proposal ProposalModel) ProposalVote,
                       Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasicOptimalStrategyRageQuit actionSpace par = daoVotingOptimally :- (optimalStakingStrategyRageQuit actionSpace par) :- Nil

-- DAO votes optimally, staker moves optimally for veto signalling
modelBasicOptimalStrategyVetoSignalling :: [Double]
               -> GameParameters a
               -> List
                     '[Kleisli Stochastic (Proposal ProposalModel) ProposalVote,
                       Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasicOptimalStrategyVetoSignalling actionSpace par = daoVotingOptimally :- (optimalStakingStrategyVetoSignalling actionSpace par) :- Nil


-- 3.3 modelBasic2

-- DAO submits and then cancels, staker moves optimally for veto signalling
modelBasic2ProposeCancelOptimalVetoStakingStrategy :: [Double]
                                          -> GameParameters a
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeCancelOptimalVetoStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- daoVotingCancelProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- Nil

-- DAO submits and then cancels, staker stakes max
modelBasic2ProposeCancelMaxStakingStrategy :: [Double]
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeCancelMaxStakingStrategy actionSpace =
  daoVotingSubmitProposal
  :- (maxStakingStrategy actionSpace)
  :- daoVotingCancelProposal
  :- (maxStakingStrategy actionSpace)
  :- Nil

-- DAO submits, staker stakes max, DAO cancels, staker unstakes max
modelBasic2ProposeCancelMinStakingStrategy :: [Double]
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeCancelMinStakingStrategy actionSpace =
  daoVotingSubmitProposal
  :- (minStakingStrategy actionSpace)
  :- daoVotingCancelProposal
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits then executes, staker only unstakes
modelBasic2ProposeExecuteMinStakingStrategy :: [Double]
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeExecuteMinStakingStrategy actionSpace =
  daoVotingSubmitProposal
  :- (minStakingStrategy actionSpace)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits then attempts to execute, staker moves optimally for veto signalling
modelBasic2ProposeExecuteOptimalStakingStrategy :: [Double]
                                           -> GameParameters a
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeExecuteOptimalStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- daoVotingExecuteProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- Nil

-- DAO submits, staker moves optimally for veto signalling, DAO attempts to execute, staker unstakes
modelBasic2ProposeExecuteOptimalMinStakingStrategy :: [Double]
                                           -> GameParameters a
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeExecuteOptimalMinStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits, stakers stake max, DAO cancels, stakers unstake
modelBasic2ProposeCancelMaxMinStakingStrategy :: [Double]
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeCancelMaxMinStakingStrategy actionSpace =
  daoVotingSubmitProposal
  :- (maxStakingStrategy actionSpace)
  :- daoVotingCancelProposal
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits, stakers stake a parametrised amount, DAO cancels, staker unstakes
modelBasic2ProposeCancelParametrisedStakingStrategy :: [Double] -> Double
                                          -> List
                                                '[Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double
                                                , Kleisli Stochastic (CurrentProposal ProposalModel) AllDAOActions
                                                , Kleisli Stochastic (CurrentProposal ProposalModel, OpportunityCosts, RiskFactor) Double]
modelBasic2ProposeCancelParametrisedStakingStrategy actionSpace stake =
  daoVotingSubmitProposal
  :- (Kleisli (const $ pure stake))
  :- daoVotingCancelProposal
  :- (minStakingStrategy actionSpace)
  :- Nil

-- A strategy for testing the behavior given a specific stake
modelBasic2ProposeCancelParametrisedStakingStrategy2 daoAction1 daoAction2 stake1 stake2 =
  daoAction1
  :- (Kleisli (const $ pure stake1))
  :- daoAction2
  :- (Kleisli (const $ pure stake2))
  :- Nil


-- 3.3. modelHeterogenous

-- DAO submits, both stakers move optimally for veto signalling, DAO attempts to execute, stakers unstake
-- NOTE both stakers share the burden
modelHeterogenousProposeExecuteOptimalMinStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersVetoSignalling actionSpace par)
  :- (optimalStakingStrategy2StakersVetoSignalling actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits, one staker moves optimally for veto signalling, DAO attempts to execute, stakers unstake
-- NOTE one staker stakes the other does nothing
modelHeterogenousProposeExecuteOptimalMinStakingStrategy2 actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategyVetoSignalling actionSpace par)
  :- (minStakingStrategy actionSpace)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits then attempts to execute, stakers only unstake
-- NOTE both stakerrs stake nothing
modelHeterogenousProposeExecuteMinStakingStrategy actionSpace =
  daoVotingSubmitProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- 3.4. modelBayesian with 2 stakers and potentially asymmetric information

-- DAO submits, both stakers move optimally for veto signalling, DAO attempts to execute, stakers unstake
-- NOTE both stakers share the burden
modelBayesianProposeExecuteOptimalMinStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersAsymmetricVetoSignalling actionSpace par)
  :- (optimalStakingStrategy2StakersAsymmetricVetoSignalling actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits, both stakers move for veto signalling, DAO attempts to execute, stakers unstake
-- NOTE both stakers stake enough to single handedly stop the mechanism
modelBayesianProposeExecuteOptimalMinStakingStrategy2 actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersAsymmetricVetoSignalling2 actionSpace par)
  :- (optimalStakingStrategy2StakersAsymmetricVetoSignalling2 actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- DAO submits, one staker moves optimally for veto signalling, DAO attempts to execute, stakers unstake
-- NOTE one staker stakes the other does nothing
modelBayesianProposeExecuteOptimalMinStakingStrategy3 actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersAsymmetricVetoSignalling2 actionSpace par)
  :- (minStakingStrategy actionSpace)
  :- daoVotingExecuteProposal
  :- (minStakingStrategy actionSpace)
  :- (minStakingStrategy actionSpace)
  :- Nil

-- 3.5. modelBayesianEndogenousSignal with 2 stakers

-- DAO proposing, executing, optimal and then min staking strategy (current min = unstake)
-- NOTE both stakers share the burden
modelBayesianEndogenousSignalProposeExecuteOptimalMinStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersSignalVetoSignalling actionSpace par)
  :- (optimalStakingStrategy2StakersSignalVetoSignalling actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalStrategy actionSpace)
  :- Nil

-- DAO proposing, executing, optimal and then min staking strategy (current min = unstake)
-- NOTE both stakers stake enough to single handedly stop the mechanism
modelBayesianEndogenousSignalProposeExecuteOptimalMinStakingStrategy2 actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersSignalVetoSignalling2 actionSpace par)
  :- (optimalStakingStrategy2StakersSignalVetoSignalling2 actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalStrategy actionSpace)
  :- Nil

-- DAO proposing, executing, optimal and then min staking strategy (current min = unstake)
-- NOTE only first staker stakes enough
modelBayesianEndogenousSignalProposeExecuteOptimalMinStakingStrategy3 actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersSignalVetoSignalling2 actionSpace par)
  :- (minStakingSignalStrategy actionSpace)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalStrategy actionSpace)
  :- Nil

-- 3.5. modelLeaderFollowerEndogenousSignal with 2 stakers

-- DAO proposing, executing, optimal and then min staking strategy (current min = unstake)
-- NOTE both stakers share the burden
modelLeaderFollowerEndogenousSignalProposeExecuteOptimalMinStakingStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersSignalVetoSignalling actionSpace par)
  :- (optimalStakingStrategy2StakersOptimalFollower actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalFollowerStrategy actionSpace)
  :- Nil

-- Both players stake half -- ignoring signal
modelLeaderFollowerEndogenousSignalProposeExecuteIgnoreSignalStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersIgnoreSignal actionSpace par)
  :- (optimalStakingStrategy2StakersIgnoreFollower actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalFollowerStrategy actionSpace)
  :- Nil

-- Both players stake half -- second player follows action from first player
modelLeaderFollowerEndogenousSignalProposeExecuteFollowFirstActionStrategy actionSpace par =
  daoVotingSubmitProposal
  :- (optimalStakingStrategy2StakersSignalVetoSignalling actionSpace par)
  :- (optimalStakingStrategy2StakersOptimalFollower actionSpace par)
  :- daoVotingExecuteProposal
  :- (minStakingSignalStrategy actionSpace)
  :- (minStakingSignalFollowerStrategy actionSpace)
  :- Nil

