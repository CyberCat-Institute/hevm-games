module ActionSpaces where

import Types

{----------------------------------------------------------
Describes the action spaces that the players have access to
-----------------------------------------------------------}

-----------------
-- 1. LDO holders
-----------------

-- DAO votes yes or no on a proposal
voteDAO :: [ProposalVote]
voteDAO = [VoteYes, VoteNo]

-- DAO votes to submit, cancel or execute a proposal
allDAOActions :: [AllDAOActions]
allDAOActions = [SubmitProposal, CancelProposal, ExecuteProposal]

------------------------------
-- 2. GateSeal committee moves
------------------------------

pauseTempProposal :: [ProposalPauseTemp]
pauseTempProposal = [ProposalPauseTemp, ProposalNoPauseTemp]

----------------------------
-- 3. ReSeal committee moves
----------------------------

pausePermanentProposal :: [ProposalPausePermanent]
pausePermanentProposal = [ProposalPausePermanent, ProposalNoPausePermanent]

--------------------------------
-- 4. Tiebreaker committee moves
--------------------------------

tieBreakProposal :: [TieBreak]
tieBreakProposal = [Nothing, Just ExecutePendingProposal, Just UnpauseContracts]

--------------------------------------------------
-- 5. Staking/unstaking moves for (w)stETH holders
--------------------------------------------------

-- Define the epsilon value
epsilon :: Double
epsilon = 1e-6

-- Function to shift positive values upwards by epsilon, to prevent rounding errors
adjustPositiveValues :: [Double] -> [Double]
adjustPositiveValues = map (\x -> if x > 0 then x + epsilon else x)

-- Define staking/unstaking moves for (w)stETH holders
-- Positive values: add stake to escrow
-- Negative values: remove stake from escrow
stakerMoves :: [Double]
stakerMoves = adjustPositiveValues [-10.0, -9.95 .. 10.0]

-- Define moves for players
stakerMovesSmall :: [Double]
stakerMovesSmall = adjustPositiveValues [-3.0, -2.95 .. 3.0]

