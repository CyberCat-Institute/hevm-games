module ModelSupportFunctions where

import Types

import OpenGames.Engine.Engine

{-----------------------------------------------------------------
Contains auxiliary functionality for analytics and model scenarios
------------------------------------------------------------------}


-- Define continuation where when a negative proposal gets vetoed a new beneficial proposal gets executed
simulateContinuationPosProposal :: Proposal ProposalModel 
  -> (TimeAbsolute, SignallingEscrowState, GovernanceState, GovernanceValues, CurrentProposal ProposalModel)  
  -> (CurrentProposal ProposalModel)
simulateContinuationPosProposal proposalAlternative (_, _, govState, _, proposal) 
  | (govState == VetoSignalling && benefitToStETHHolders proposal' < 0) 
    -- ^ Replace a negative proposal in the future with one non negative that will be accepted
    = proposal {currentProposal = proposalAlternative } {proposalState = Executed} 
  | (otherwise )
    -- ^ Just keep the proposal
    = proposal
  where
    Proposal proposal' = currentProposal proposal
