{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types
  where

import OpenGames.Engine.Engine
import EVM.Types (W256)

import qualified Data.Map.Strict as M
import Data.Time

{-------------------------------------------------------------------
Describes the types of the model and interfaces to the outside world
--------------------------------------------------------------------}

---------------------------------
-- 0. Tuple instances boilerplate
---------------------------------


deriving instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m,Show n,Show o,Show p)
  => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

deriving instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m,Show n,Show o,Show p, Show q)
  => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

deriving instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m,Show n,Show o,Show p, Show q, Show r)
  => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)


--------------------
-- 1 Basic ETH types
--------------------

type ETH = Double
type StETH = ETH
type StETH' = W256
type WstETH = ETH
type WithdrawlNFT = StETH
type LDO = Double

-- WstETH -> stETH conversion rate
type ConversionRate = Double


--------------------------------
-- 2 Types for representing time
--------------------------------

type TimeAbsolute = UTCTime
type TimeRelative = NominalDiffTime


-----------------------------------
-- 3 Agents and private information
-----------------------------------

type LDOHolder = Agent
type StETHHolder = Agent
type OracleCommittee = Agent
type GateSealCommittee = Agent
type ResealCommittee = Agent
type TieBreakerCommittee = Agent

type RiskFactor = Double
type RiskFactorEVM = W256
type OpportunityCosts = Double


-------------
-- 4 Accounts
-------------

-- We can create various accounts for different tokens we can consider
type Account a = M.Map Agent a


----------
-- 5 State
----------

-- General representation of a proposal wrapping arbitrary state
-- Example usage: Proposal (Double, Double)
-- where fst = benefit/cost to LDO holders, snd = benefit/cost to stETH holders
data Proposal a = Proposal a
  deriving (Show, Eq, Ord)

-- Proposal type tracking benefit/cost to each agent type
data ProposalModel = ProposalModel {
    benefitToLDOHolders :: Double
  , benefitToStETHHolders :: Double
} deriving (Show, Eq, Ord)

-- Trivial proposal model containing no information
-- Needed to separate out private beliefs
data ProposalToken = ProposalToken deriving (Show, Eq, Ord)

-- Proposal life cycle
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#proposal-lifecycle
data ProposalState = Pending | Submitted | Cancelled | Executed
  deriving (Show, Eq, Ord)

-- Metadata of a proposal currently under consideration
data CurrentProposal a = CurrentProposal {
    currentProposal :: Proposal a
  , timeOfSubmission :: TimeAbsolute
  , proposalState :: ProposalState
} deriving (Show, Eq, Ord)

-- Signal type for how an agent is affected

type SignalProposal = Double

-- Global state of Lido
data GlobalLidoState = GlobalLidoState {
    totalStETHSupply :: StETH          -- $S_{st}$ in the docs
  , conversionRate   :: ConversionRate -- $R^{st}_{wst}$ in the docs
  , accountsStETH    :: Account StETH  -- Include accounts (this only includes the stETH contained outside the escrow)
  , accountsWstETH   :: Account WstETH -- Include accounts (this only includes the stETH contained outside the escrow)
} deriving (Show, Eq, Ord)

data AccountState = AccountState {
  getAccountsStETH    :: Account StETH'
}-- Include accounts (this only includes the stETH contained outside the escrow)


-- State of signalling escrow
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#signalling-escrow
data SignallingEscrowState = SignallingEscrowState {
    lockedStETH :: Account StETH         -- $st$ in the docs
  , lockedWstETH :: Account WstETH       -- $wst$ in the docs
  , unfinalizedNFTTotal :: Account StETH -- $shares_u$ in the docs
  , finalizedNFTTotal :: Account ETH     -- $eth_f$ in the docs
} deriving (Show, Eq,Ord)

-- All parameters of the DG mechanism
data GovernanceParams = GovernanceParams {
    proposalExecutionMinTimelock :: TimeRelative
  , firstSealRageQuitSupport :: Double
  , secondSealRageQuitSupport :: Double
  , dynamicTimelockMinDuration :: TimeRelative
  , dynamicTimelockMaxDuration :: TimeRelative
  , vetoSignallingMinActiveDuration :: TimeRelative
  , vetoSignallingDeactivationMaxDuration :: TimeRelative
  , vetoCooldownDuration :: TimeRelative
  , rageQuitExtensionDelay :: TimeRelative
  , rageQuitEthWithdrawalsMinTimelock :: TimeRelative
  , rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber :: Int
  , rageQuitEthWithdrawalsTimelockGrowthCoeffs :: (TimeRelative, TimeRelative, TimeRelative)
} deriving (Show,Eq)

-- Describe the governance state of the dual governance submodule
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#global-governance-state
-- NOTE we choose to model the Deactivation substate as a separate state
data GovernanceState
  = Normal
  | VetoSignalling
  | VetoSignallingDeactivation
  | VetoCooldown
  | RageQuit
  deriving (Show,Eq,Ord)

-- State variables of the DG state machine
data GovernanceValues = GovernanceValues {
    timeOfActivation :: TimeAbsolute
  , timeOfReactivation :: TimeAbsolute
  , timeOfDeactivation :: TimeAbsolute
    -- Value of Nothing indicates withdrawal NFTs have not yet been claimed
  , timeOfRageQuitExtensionStart :: Maybe TimeAbsolute
  , rageQuitSeqNumber :: Int
} deriving (Show,Eq,Ord)


-------------------------
-- 6 Action related types
-------------------------

-- | LDO holders
data ProposalVote = VoteYes | VoteNo
  deriving (Show,Eq,Ord)

data AllDAOActions = SubmitProposal | CancelProposal | ExecuteProposal
  deriving (Show,Eq,Ord)

-- | stETH holders
-- Action to lock or unlock stETH from signalling escrow
data LockStETH = LockStETH Double
  deriving (Show,Eq)

-- | Gate seal committee decision
data ProposalPauseTemp = ProposalPauseTemp | ProposalNoPauseTemp
  deriving (Show,Eq)

-- | Reseal committee decision
data ProposalPausePermanent = ProposalPausePermanent | ProposalNoPausePermanent
  deriving (Show,Eq)

-- | Tiebreaker committee actions
data TieBreakActions = ExecutePendingProposal | UnpauseContracts
  deriving (Show,Eq)

-- | Tiebreaker committee decisions
type TieBreak = Maybe TieBreakActions


-------------------------------
-- 7 Parameters for game models
-------------------------------

-- Parameters bundled for games
data GameParameters a = GameParameters
  { governanceParameters  :: GovernanceParams       -- parameters of the DG system
  , agentDao              :: Agent                  -- name of representative LDO holder
  , agentStaker           :: Agent                  -- name of representative stETH holder
  , agentStaker2          :: Agent                  -- name of another representative stETH holder (may be undefined)
  , opportunityCosts      :: OpportunityCosts       -- opportunity costs of first stETH holder
  , opportunityCosts2     :: OpportunityCosts       -- opportunity costs of second stETH holder (may be undefined)
  , agentRiskFactor       :: RiskFactor             -- risk factor of first stETH holder
  , agentRiskFactor2      :: RiskFactor             -- risk factor of second stETH holder (may be undefined)
  , globalLidoState       :: GlobalLidoState        -- state of entire Lido system
  , currentTime           :: TimeAbsolute           -- current time
  , signallingEscrowState :: SignallingEscrowState  -- state of signalling escrow
  , governanceState       :: GovernanceState        -- state of the DG system
  , governanceValues      :: GovernanceValues       -- state variables of the DG system
  , proposal              :: a                      -- proposal currently in scope
  } deriving (Show,Eq)
