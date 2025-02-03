{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parameterization
  where

import Model
import SupportFunctions
import TimeHelperFunctions
import Types

import OpenGames.Engine.Engine

import Data.Time (UTCTime(..), Day, fromGregorian, addUTCTime, secondsToDiffTime, addUTCTime)

{------------------------------------------------------
Defines the main parameterizations used in the analysis
-------------------------------------------------------}

----------
-- 1. Time
----------

-- Construct start days
firstSeptember = fromGregorian 2024 9 1 

-- Start day
startDay =  UTCTime firstSeptember 0

-- Date for proposal
lastAugust = UTCTime (fromGregorian 2024 8 31) 0


------------------------------------------
-- 2. Dual Governance Mechanism Parameters
------------------------------------------

-- Governance parameters as defined in the draft draft,
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md
defaultGovernanceParams :: GovernanceParams
defaultGovernanceParams = GovernanceParams {
    proposalExecutionMinTimelock = 3 * days
  , firstSealRageQuitSupport = 0.01
  , secondSealRageQuitSupport = 0.1
  , dynamicTimelockMinDuration = 5 * days
  , dynamicTimelockMaxDuration = 45 * days
  , vetoSignallingMinActiveDuration = 5 * hours
  , vetoSignallingDeactivationMaxDuration = 3 * days
  , vetoCooldownDuration = 5 * hours
  , rageQuitExtensionDelay = 7 * days
  , rageQuitEthWithdrawalsMinTimelock = 60 * days
  , rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber = 2
    -- marked as TODO in the spec, https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#rage-quit-state
    -- Currently using values taken from here: https://github.com/lidofinance/dg-python-model/blob/main/DG-modeling.ipynb
  , rageQuitEthWithdrawalsTimelockGrowthCoeffs = (0, 1, 86400*2)
}

-- Create an instance of GovernanceValues
defaultGovernanceValues :: GovernanceValues
defaultGovernanceValues =
  let timeOfActivation = utcTimeFrom 0        -- 1st September 2024
      timeOfDeactivation = utcTimeFrom 10     -- 10 days later
      timeOfReactivation = utcTimeFrom 20     -- 20 days later
      timeOfRageQuitExtensionStart = Nothing  -- No rage quit extension start
      rageQuitSeqNumber = 1                   -- Positive rage quit sequence number
  in GovernanceValues
      { timeOfActivation = timeOfActivation
      , timeOfReactivation = timeOfReactivation
      , timeOfDeactivation = timeOfDeactivation
      , timeOfRageQuitExtensionStart = timeOfRageQuitExtensionStart
      , rageQuitSeqNumber = rageQuitSeqNumber
      }


---------------
-- 3. Proposals
---------------

-- NOTE under this assumption consequences for stETH holders is how much they will lose given their current assets
-- This has to always be negative or maximally zero
proposal1, proposal2 :: Proposal ProposalModel
proposal1 = Proposal $ ProposalModel {benefitToLDOHolders = 2, benefitToStETHHolders = -1}
proposal2 = Proposal $ ProposalModel {benefitToLDOHolders = 1, benefitToStETHHolders = 0}

currentProposal1 :: CurrentProposal ProposalModel
currentProposal1 = CurrentProposal {
    currentProposal  = proposal1
  , timeOfSubmission = lastAugust
  , proposalState    = Pending
  }

currentProposal2 :: CurrentProposal ProposalModel
currentProposal2 = CurrentProposal {
    currentProposal  = proposal2
  , timeOfSubmission = lastAugust
  , proposalState    = Pending
}


---------------------------
-- 4. Parameters for models
---------------------------

simpleStakingGameParameters  = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agnt
 , opportunityCosts      = 0.01
 , agentRiskFactor       = 1 
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal1
 }
 where
   agnt = "StakingAgent"
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agnt,10)] [(agnt,0)] 1 [(agnt,0)] [(agnt,0)] [(agnt,0)] [(agnt,0)] 

modelBasicGameParameters  = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agnt
 , opportunityCosts      = 0.01
 , agentRiskFactor       = 1
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = proposal1
 }
 where
   agnt = "StakingAgent"
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agnt,10)] [(agnt,0)] 1 [(agnt,0)] [(agnt,0)] [(agnt,0)] [(agnt,0)] 

modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agnt
 , opportunityCosts      = 0.01
 , agentRiskFactor       = 1
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = proposal'
 }
 where
   agnt = "StakingAgent"
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agnt,10)] [(agnt,0)] 1 [(agnt,0)] [(agnt,0)] [(agnt,0)] [(agnt,0)]
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}

-- Parameters for testing game
modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agnt
 , agentStaker2          = error "agent 2 not defined"
 , opportunityCosts      = 0.01
 , opportunityCosts2     = error "opporunity cost for second agent not defined"
 , agentRiskFactor       = 1
 , agentRiskFactor2      = error "subjective risks for agent 2 not defined"
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agnt = "StakingAgent"
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agnt,10)] [(agnt,0)] 1 [(agnt,0)] [(agnt,0)] [(agnt,0)] [(agnt,0)]
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}
   currentProposal' = CurrentProposal
     { currentProposal   = proposal'
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }


-- Model with two heterogenous stakers
modelHeterogenousParameters = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agent1
 , agentStaker2          = agent2
 , opportunityCosts      = 0.01 
 , opportunityCosts2     = 0.01 
 , agentRiskFactor       = 1
 , agentRiskFactor2      = 1
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agent1 = "StakingAgent1"
   agent2 = "StakingAgent2"
   emptyAcct = [(agent1, 0), (agent2, 0)]
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agent1, 10), (agent2, 10)] emptyAcct 1 emptyAcct emptyAcct emptyAcct emptyAcct
     --constructInitialStates [(agnt,10)] [(agnt,0)] 1 [(agnt,0)] [(agnt,0)] [(agnt,0)] [(agnt,0)]
   currentProposal' = CurrentProposal
     { currentProposal   = Proposal ProposalToken
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }

modelHeterogenousGameParametersTestProposal payoffLDO payoffStETHHolders = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agent1
 , agentStaker2          = agent2
 , opportunityCosts      = 0.01 
 , opportunityCosts2     = 0.01 
 , agentRiskFactor       = 1
 , agentRiskFactor2      = 1
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agent1 = "StakingAgent1"
   agent2 = "StakingAgent2"
   emptyAcct = [(agent1, 0), (agent2, 0)]
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agent1, 10), (agent2, 10)] emptyAcct 1 emptyAcct emptyAcct emptyAcct emptyAcct
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}
   currentProposal' = CurrentProposal
     { currentProposal   = proposal'
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }

modelHeterogenousGameParametersTestProposalAsymmetric payoffLDO payoffStETHHolders agntRisk1 agntRisk2 = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agent1
 , agentStaker2          = agent2
 , opportunityCosts      = 0.01 
 , opportunityCosts2     = 0.01 
 , agentRiskFactor       = agntRisk1
 , agentRiskFactor2      = agntRisk2
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agent1 = "StakingAgent1"
   agent2 = "StakingAgent2"
   emptyAcct = [(agent1, 0), (agent2, 0)]
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agent1, 10), (agent2, 10)] emptyAcct 1 emptyAcct emptyAcct emptyAcct emptyAcct
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}
   currentProposal' = CurrentProposal
     { currentProposal   = proposal'
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }

modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agent1
 , agentStaker2          = agent2
 , opportunityCosts      = 0.01 
 , opportunityCosts2     = 0.01 
 , agentRiskFactor       = error "agentRiskFactor1 determined internally"
 , agentRiskFactor2      = error "agentRiskFactor2 determined internally"
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agent1 = "StakingAgent1"
   agent2 = "StakingAgent2"
   emptyAcct = [(agent1, 0), (agent2, 0)]
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agent1, 10), (agent2, 10)] emptyAcct 1 emptyAcct emptyAcct emptyAcct emptyAcct
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}
   currentProposal' = CurrentProposal
     { currentProposal   = proposal'
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }

-- NOTE High imbalance in assets
modelBayesianGameParametersTestProposal2 payoffLDO payoffStETHHolders = GameParameters
 { governanceParameters  = defaultGovernanceParams
 , agentDao              = "agentDao"
 , agentStaker           = agent1
 , agentStaker2          = agent2
 , opportunityCosts      = 0.01 
 , opportunityCosts2     = 0.01 
 , agentRiskFactor       = error "agentRiskFactor1 determined internally"
 , agentRiskFactor2      = error "agentRiskFactor2 determined internally"
 , globalLidoState       = globalLidoState'
 , currentTime           = startDay
 , signallingEscrowState = signallingEscrowState'
 , governanceState       = Normal
 , governanceValues      = defaultGovernanceValues
 , proposal              = currentProposal'
 }
 where
   agent1 = "StakingAgent1"
   agent2 = "StakingAgent2"
   emptyAcct = [(agent1, 0), (agent2, 0)]
   (globalLidoState',signallingEscrowState') =
     constructInitialStates [(agent1, 30), (agent2, 3)] emptyAcct 1 emptyAcct emptyAcct emptyAcct emptyAcct
   proposal' = Proposal $ ProposalModel {benefitToLDOHolders = payoffLDO, benefitToStETHHolders = payoffStETHHolders}
   currentProposal' = CurrentProposal
     { currentProposal   = proposal'
     , timeOfSubmission = lastAugust
     , proposalState    = Pending
     }
