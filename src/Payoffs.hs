module Payoffs where

import SupportFunctions
import Types

{----------------------------------------------
Describes the payoffs for the different players
-----------------------------------------------}

-- Return the payoffs to (LDO holders, StETH holders) resulting from an executed proposal
proposalPayoffs :: CurrentProposal ProposalModel -> (Double, Double)
proposalPayoffs (CurrentProposal (Proposal (ProposalModel benefitToLDOHolders benefitToStETHHolders)) _ Executed) 
  -- If a proposal is executed then it has the given costs/benefits
  = (benefitToLDOHolders, benefitToStETHHolders)
proposalPayoffs _
  -- If a proposal is not executed then there are no payoffs
  = (0, 0)
