module SupportFunctions where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Time

import TimeHelperFunctions
import Types

import OpenGames.Engine.Engine

{-------------------------------------------------------
Contains basic auxiliary functionality needed for model,
including implementation of the DG state machine
--------------------------------------------------------}

------------------------------------------------
-- 1. Helper functions for staking and unstaking
------------------------------------------------

-- Transfer an (assumed positive) amount from sender account to recipient account
-- Returns Nothing if sender has insufficient funds or sender account does not exist
-- Creates account for recipient if it does not exist
transferFunds :: Agent -> Agent -> Double
              -> Account Double -> Account Double
              -> Maybe (Account Double, Account Double)
transferFunds senderName recipientName amount senderAccount recipientAccount
  = do {
        senderFunds <- M.lookup senderName senderAccount -- fail if sender account has no record
      ; guard (senderFunds >= amount) -- fail if sender account has insufficnet funds
      ; let recipientFunds = M.findWithDefault 0 recipientName recipientAccount
      ; return ( M.insert senderName (senderFunds - amount) senderAccount
               , M.insert recipientName (recipientFunds + amount) recipientAccount)
  }

-- Transfer an (assumed positive) amount from global account to escrow account
-- owned by the same agent. Returns Nothing if transfer from global account does
stakeStETH :: Agent -> StETH -> GlobalLidoState -> SignallingEscrowState
           -> Maybe (GlobalLidoState, SignallingEscrowState)
stakeStETH name amount globalLidoState signallingEscrowState
  = do {
        (newGlobalAccount, newEscrowAccount) <- transferFunds name name amount (accountsStETH globalLidoState) (lockedStETH signallingEscrowState)
      ; return ( globalLidoState {accountsStETH = newGlobalAccount}
               , signallingEscrowState {lockedStETH = newEscrowAccount})
  }

-- Transfer an (assumed positive) amount from escrow account to global account
-- owned by the same agent. Returns Nothing is transfer from escrow account does
unstakeStETH :: Agent -> StETH -> GlobalLidoState -> SignallingEscrowState
             -> Maybe (GlobalLidoState, SignallingEscrowState)
unstakeStETH name amount globalLidoState signallingEscrowState
  = do {
        (newEscrowAccount, newGlobalAccount) <- transferFunds name name amount (lockedStETH signallingEscrowState) (accountsStETH globalLidoState)
      ; return ( globalLidoState {accountsStETH = newGlobalAccount}
               , signallingEscrowState {lockedStETH = newEscrowAccount})
  }

-- Transfer a positive or negative ammount from global account to escrow account
-- owned by the same agent. Fails silently and returns initial state if the transfer fails
stakeOrUnstake :: Agent -> StETH -> GlobalLidoState -> SignallingEscrowState
               -> IO (GlobalLidoState, SignallingEscrowState)
stakeOrUnstake name amount globalLidoState signallingEscrowState
  = let newState | (amount >= 0) = stakeStETH name amount globalLidoState signallingEscrowState
                 | (otherwise)   = unstakeStETH name (-amount) globalLidoState signallingEscrowState
     in case newState of {
          Just (newGlobalLidoState, newSignallingEscrowState) -> (newGlobalLidoState, newSignallingEscrowState)
        ; Nothing -> (globalLidoState, signallingEscrowState)
    }

stakeOrUnstakeReal :: Agent -> StETH -> AccountState -- GlobalLidoState -> SignallingEscrowState
               -> IO (AccountState, SignallingEscrowState)
stakeOrUnstakeReal name amount accounts
  = do escrow_lockStETH name amount
       st <- dualgov_getEffectiveState name
       pure (subtract accounts name amount, st)


-- Helper to sum over all accounts
sumAllAccounts :: Account Double -> Double
sumAllAccounts acc = M.foldr (+) 0 acc

-- Add costs in case of staking into escrow
computeRiskCosts :: (RiskFactor,Double) -> Payoff
computeRiskCosts (f,amount)
  | amount > 0 = amount*f
  | otherwise  = 0

-- Derive individual risk of assets
-- NOTE we are including the publicly known risk for assets with an only privately known assessment
-- (in case the latter is 1 it is the same as the public one)
computeAssetsAtRisk :: Agent -> (GlobalLidoState, RiskFactor) -> RiskFactor -> Payoff
computeAssetsAtRisk agent (state,riskFactorPrivate) riskFactorPublic =
  let wallet      = accountsStETH state
      assetsAgent = M.lookup agent wallet
      in case assetsAgent of
           Nothing -> 0
           Just value -> value * riskFactorPublic *riskFactorPrivate

-- Like _computeAssetsAtRisk_ but only considering the public component
computeAssetsAtRiskPublicOnly :: Agent -> GlobalLidoState -> RiskFactor -> Payoff
computeAssetsAtRiskPublicOnly agent state riskFactorPublic =
  let wallet      = accountsStETH state
      assetsAgent = M.lookup agent wallet
      in case assetsAgent of
           Nothing -> 0
           Just value -> value * riskFactorPublic



----------------------------------------------
-- 2. Helper functions for protocol parameters
----------------------------------------------

-- Compute rage quit support value $R = R(t)$ given current state of the signalling escrow
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#signalling-escrow
-- TODO Find out what rounding mode for division this uses in the Solidity source
rageQuitSupport :: GlobalLidoState -> SignallingEscrowState -> Double
rageQuitSupport (GlobalLidoState totalStETHSupply conversionRate _ _)
                (SignallingEscrowState lockedStETH lockedWstETH unfinalizedNFTTotal finalizedNFTTotal)
  | (denominator > 0) = (sumAllAccounts lockedStETH + sumAllAccounts finalizedNFTTotal + conversionRate*(sumAllAccounts lockedWstETH) + sumAllAccounts unfinalizedNFTTotal)
                      / denominator
    -- Nonpositive denominator is not a recoverable situation, intentionally crash
  | (otherwise) = error "rageQuitSupport: denominator is not positive"
  where denominator = totalStETHSupply + sumAllAccounts finalizedNFTTotal

-- Compute the dynamic timelock duration $T_{lock} (R)$ given the rage quit support $R$
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-signalling-state
dynamicTimelockDuration :: GovernanceParams -> Double -> TimeRelative
dynamicTimelockDuration params currentRageQuitSupport
  | (currentRageQuitSupport <= firstSealRageQuitSupport params)
      = 0
  | (firstSealRageQuitSupport params < currentRageQuitSupport
    && currentRageQuitSupport < secondSealRageQuitSupport params)
      = dynamicTimelockMinDuration params
      + (scaleTimeRelative ((currentRageQuitSupport - firstSealRageQuitSupport params)
                            / (secondSealRageQuitSupport params - firstSealRageQuitSupport params))
                            (dynamicTimelockMaxDuration params - dynamicTimelockMinDuration params))
  | (currentRageQuitSupport >= secondSealRageQuitSupport params)
      = dynamicTimelockMaxDuration params

-- Compute the ETH withdrawal timelock duration $W (i)$ given the current sequence number $i$
-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#rage-quit-state
ethWithdrawalTimelockDuration :: GovernanceParams -> Int -> TimeRelative
ethWithdrawalTimelockDuration params seqNumber
  | (seqNumber < rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber params)
    = rageQuitEthWithdrawalsMinTimelock params
  | (otherwise)
    = rageQuitEthWithdrawalsMinTimelock params + g (fromIntegral (seqNumber - rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber params))
  where (coeff1, coeff2, coeff3) = rageQuitEthWithdrawalsTimelockGrowthCoeffs params
        g :: Double -> TimeRelative
        g x = coeff1 + scaleTimeRelative x coeff2 + scaleTimeRelative (x*x) coeff3


---------------------------------------------
-- 3. Helper functions parameter construction
---------------------------------------------

-- Construct consistent globalLidoState and SignallingEscrowState
constructInitialStates :: [(Agent, StETH)]-- ^ within Lido
                       -> [(Agent, WstETH)] -- ^ within Lido
                       -> ConversionRate
                       -> [(Agent, StETH)]  -- ^ StETH within Escrow
                       -> [(Agent, WstETH)] -- ^ WstETH within Escrow
                       -> [(Agent, StETH)]  -- ^ NFT within Escrow
                       -> [(Agent, ETH)]    -- ^ finalize NFT in Escrow
                       -> (GlobalLidoState,SignallingEscrowState)
constructInitialStates lsAccStETH lsAccWstETH rate escrowLsAccStETH escrowLsAccWstETH escrowLsAccNFT escrowLsAccETH =
  (GlobalLidoState { totalStETHSupply = total
                   , conversionRate   = rate
                   , accountsStETH    = M.fromList lsAccStETH
                   , accountsWstETH   = M.fromList lsAccWstETH
                   },
   SignallingEscrowState { lockedStETH         = M.fromList escrowLsAccStETH
                         , lockedWstETH        = M.fromList escrowLsAccWstETH
                         , unfinalizedNFTTotal = M.fromList escrowLsAccNFT
                         , finalizedNFTTotal   = M.fromList escrowLsAccETH
                         }
  )
  where
    total = globalStETH + escrowStETH
    globalStETH = sum $ fmap snd lsAccStETH
    escrowStETH = sum $ fmap snd escrowLsAccStETH

-----------------------------------------------
-- 4. Checking conditions for state transitions
-----------------------------------------------

-- See https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#global-governance-state

daoCanSubmitProposals :: GovernanceState -> Bool
daoCanSubmitProposals Normal = True
daoCanSubmitProposals VetoSignalling = True
daoCanSubmitProposals VetoSignallingDeactivation = False
daoCanSubmitProposals VetoCooldown = False
daoCanSubmitProposals RageQuit = True

daoCanExecuteProposals :: GovernanceState -> Bool
daoCanExecuteProposals Normal = True
daoCanExecuteProposals VetoSignalling = False
daoCanExecuteProposals VetoSignallingDeactivation = False
daoCanExecuteProposals VetoCooldown = True
daoCanExecuteProposals RageQuit = False

-- DAO attempts to submit a proposal if they voted to submit
-- Produces a proposal in the Pending state if able
daoSubmitProposal :: GovernanceState -> TimeAbsolute -> CurrentProposal a -> CurrentProposal a
daoSubmitProposal governanceState currentTime proposal
  | (daoCanSubmitProposals governanceState && proposalState proposal == Pending)
    = proposal {proposalState = Submitted}
  | (otherwise)
    = proposal

-- Alterantive implementation of daoSubmitProposal used by model1
daoSubmitProposal' :: GovernanceState -> TimeAbsolute -> Proposal a -> ProposalVote
                    -> CurrentProposal a
daoSubmitProposal' governanceState currentTime proposal proposalVote
  = CurrentProposal {
      currentProposal = proposal
    , timeOfSubmission = currentTime
    , proposalState = if daoCanSubmitProposals governanceState && proposalVote == VoteYes
                      then Submitted
                      else Cancelled
  }

-- DAO changes the current proposal's state to cancelled
daoCancelProposal :: CurrentProposal a -> CurrentProposal a
daoCancelProposal proposal
  | (proposalState proposal == Submitted)
    = proposal {proposalState = Cancelled}
  | (otherwise)
    = proposal

-- DAO attempts to execute the current proposal,
-- waiting for the proposal timelock to expire if necessary
-- Returns the end time and the proposal with state modified to Executed if allowed
-- NOTE Calling this function constitutes an assumption that all agents wait
-- from https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-cooldown-state
daoExecuteProposal :: GovernanceParams -> TimeAbsolute -> GovernanceState  -> CurrentProposal a
                   -> (TimeAbsolute, CurrentProposal a)
daoExecuteProposal governanceParams currentTime governanceState currentProposal
  | (daoCanExecuteProposals governanceState
    && timeDifferenceAbsolute currentTime (timeOfSubmission currentProposal)
       > proposalExecutionMinTimelock governanceParams
    && proposalState currentProposal == Submitted)
      -- Proposal successfully executes immediately
    = (currentTime, currentProposal {proposalState = Executed})
  | (daoCanExecuteProposals governanceState && proposalState currentProposal == Submitted)
      -- Proposal successfully executes after waiting for proposal timelock to expire
    = (addTimeDuration (timeOfSubmission currentProposal) (proposalExecutionMinTimelock governanceParams),
       currentProposal {proposalState = Executed})
  | (otherwise)
      -- Execution fails immediately
    = (currentTime, currentProposal)

-- Implement a DAO choice to submit, cancel or execute a proposal
daoDoMove :: GovernanceParams -> TimeAbsolute -> GovernanceState -> CurrentProposal a -> AllDAOActions
  -> (TimeAbsolute, CurrentProposal a)
daoDoMove governanceParams currentTime governanceState proposal daoAction
  = case daoAction of {
        SubmitProposal -> (currentTime, daoSubmitProposal governanceState currentTime proposal)
      ; CancelProposal -> (currentTime, daoCancelProposal proposal)
      ; ExecuteProposal -> daoExecuteProposal governanceParams currentTime governanceState proposal
  }

------------------------------------------------
-- 5. State transitions from each dual gov state
------------------------------------------------

-- The following functions attempt to perform a single transition from some state
-- NOTE They return Nothing if no transitions are enabled

-- Outgoing transitions of the Normal state
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#normal-state
transitionNormal :: GovernanceParams -> TimeAbsolute -> Double -> GovernanceValues
                 -> Maybe (GovernanceState, GovernanceValues)
transitionNormal params currentTime currentRageQuitSupport values
  | (currentRageQuitSupport > firstSealRageQuitSupport params) -- ^ normal -> veto signalling
      = Just (VetoSignalling, values {timeOfActivation = currentTime})
  | (otherwise)
      = Nothing

-- Outgoing transitions of the VetoSignalling parent state
-- NOTE: Deactivation substate is modelled as a separate state
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-signalling-state
transitionVetoSignalling :: GovernanceParams -> TimeAbsolute -> Double
                        -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
transitionVetoSignalling params currentTime currentRageQuitSupport values
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ VetoSignalling -> RageQuit
     >= dynamicTimelockMaxDuration params
  && currentRageQuitSupport > secondSealRageQuitSupport params)
      = Just (RageQuit, values {
            timeOfActivation = currentTime
          , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
          , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
        })
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ VetoSignalling -> Deactivation
     >= dynamicTimelockDuration params currentRageQuitSupport
  && timeDifferenceAbsolute currentTime (max (timeOfActivation values) (timeOfReactivation values))
     >= vetoSignallingMinActiveDuration params)
      = Just (VetoSignallingDeactivation, values {timeOfDeactivation = currentTime}) -- do not change timeOfActivation
  | (otherwise)
      = Nothing

-- Outgoing transitions of the VetoSignalling deactivation substate
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#deactivation-sub-state
transitionVetoSignallingDeactivation :: GovernanceParams -> TimeAbsolute -> Double
                                     -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ Deactivation -> VetoSignalling
     < dynamicTimelockDuration params currentRageQuitSupport)
      = Just (VetoSignalling, values {timeOfReactivation = currentTime}) -- do not change timeOfActivation
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ Deactivation -> RageQuit
     >= dynamicTimelockMaxDuration params
  && currentRageQuitSupport > secondSealRageQuitSupport params)
      = Just (RageQuit, values {
            timeOfActivation = currentTime
          , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
          , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
        })
  | (timeDifferenceAbsolute currentTime (timeOfDeactivation values) -- ^ Deactivation -> VetoCoolDown
      >= vetoSignallingDeactivationMaxDuration params)
    = Just (VetoCooldown, values {timeOfActivation = currentTime})
  | (otherwise)
    = Nothing

-- Outgoing transitions of the VetoCooldown state
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-cooldown-state
transitionVetoCooldown :: GovernanceParams -> TimeAbsolute -> Double
                       -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
transitionVetoCooldown params currentTime currentRageQuitSupport values
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) >= vetoCooldownDuration params -- ^ VetoCoolDown -> VetoSignalling
  && currentRageQuitSupport > firstSealRageQuitSupport params)
    = Just (VetoSignalling, values {timeOfActivation = currentTime})
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) >= vetoCooldownDuration params -- ^ VetoCoolDown -> Normal
  && currentRageQuitSupport <= firstSealRageQuitSupport params)
    = Just (Normal, values {timeOfActivation = currentTime})
  | (otherwise)
    = Nothing

-- Outgoing transitions of the RageQuit state
-- TODO: this state is always stuck until we implement claiming of withdrawal NFTs
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#rage-quit-state
transitionRageQuit :: GovernanceParams -> TimeAbsolute -> Double
                   -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
transitionRageQuit params currentTime currentRageQuitSupport values
  = case timeOfRageQuitExtensionStart values of {
        Nothing -- withdrawal NFTs have not yet been claimed
          -> Nothing
      ; Just timeOfRageQuitExtensionStart'
          | (timeDifferenceAbsolute currentTime timeOfRageQuitExtensionStart' -- ^ RageQuit -> VetoSignalling
              >= rageQuitExtensionDelay params + ethWithdrawalTimelockDuration params (rageQuitSeqNumber values)
            && currentRageQuitSupport > firstSealRageQuitSupport params)
            -> Just (VetoSignalling, values {timeOfActivation = currentTime})
          | (timeDifferenceAbsolute currentTime timeOfRageQuitExtensionStart' -- ^ RageQuit -> VetoCooldown
              >= rageQuitExtensionDelay params + ethWithdrawalTimelockDuration params (rageQuitSeqNumber values)
            && currentRageQuitSupport <= firstSealRageQuitSupport params)
            -> Just (VetoCooldown, values {timeOfActivation = currentTime})
          | (otherwise)
            -> Nothing
  }


-------------------------------------------------------------
-- 6. Full state machine transitions at a fixed point in time
-------------------------------------------------------------

-- Perform a single transitions from the current state if possible
-- NOTE: Returns Nothing is no transitions are enabled
transition1 :: GlobalLidoState -> GovernanceParams -> TimeAbsolute -> SignallingEscrowState
            -> GovernanceState -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
transition1 globalLidoState params currentTime signallingEscrowState governanceState values
  = let currentRageQuitSupport = rageQuitSupport globalLidoState signallingEscrowState
     in case governanceState of
          Normal
            -> transitionNormal params currentTime currentRageQuitSupport values
          VetoSignalling
            -> transitionVetoSignalling params currentTime currentRageQuitSupport values
          VetoSignallingDeactivation
            -> transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
          VetoCooldown
            -> transitionVetoCooldown params currentTime currentRageQuitSupport values
          RageQuit
            -> transitionRageQuit params currentTime currentRageQuitSupport values

-- Apply transitions until no more are available, without advancing the current time
transitionMany :: GlobalLidoState -> GovernanceParams -> TimeAbsolute -> SignallingEscrowState
            -> GovernanceState -> GovernanceValues -> (GovernanceState, GovernanceValues)
transitionMany globalLidoState params currentTime signallingEscrowState state values
  = case transition1 globalLidoState params currentTime signallingEscrowState state values of {
        Nothing
          -> (state, values)
      ; Just (newState, newValues)
          -> transitionMany globalLidoState params currentTime signallingEscrowState newState newValues
  }


------------------------------------------
-- 7 Temporal aspects of the state machine
------------------------------------------

-- Attempt to claim NFT withdrawal tokens
-- NOTE Returns Nothing if not in RageQuit state, or if rage quit extension delay has already begun
-- (i.e. tokens were already claimed)
claimNFTWithdrawalTokens :: TimeAbsolute
                         -> GovernanceState -> GovernanceValues -> Maybe (GovernanceState, GovernanceValues)
claimNFTWithdrawalTokens currentTime state values
  = case (state, timeOfRageQuitExtensionStart values) of
      (RageQuit, Nothing) -- Sucessfully claim withdrawal tokens, begin rage quit extension delay
        -> Just (RageQuit, values {timeOfRageQuitExtensionStart = Just currentTime})
      _ -- Either not in rage quit state or tokens already claimed, fail
        -> Nothing

-- The following functions return the time duration until a state may spontaneously transition
-- without changes to any values
-- NOTE These functions should only be called on states that are stable at the current time,
-- i.e. states produced by transitionMany, otherwise it can produce nonsense values,
-- such as negative durations

-- Spontaneous transitions VetoSignalling -> RageQuit OR VetoSignallingDeactivation
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-signalling-state
stableDurationVetoSignalling :: GovernanceParams -> TimeAbsolute
                             -> Double -> GovernanceValues -> TimeRelative
stableDurationVetoSignalling governanceParams currentTime currentRageQuitSupport governanceValues
  = minimum [
              -- ^ VetoSignalling -> RageQuit
              dynamicTimelockMaxDuration governanceParams
                - timeDifferenceAbsolute currentTime (timeOfActivation governanceValues)
              -- ^ VetoSignalling -> VetoSignallingDeactivation
            , maximum [
                dynamicTimelockDuration governanceParams currentRageQuitSupport
                  - timeDifferenceAbsolute currentTime (timeOfActivation governanceValues)
              , vetoSignallingMinActiveDuration governanceParams
                  - timeDifferenceAbsolute currentTime (max (timeOfActivation governanceValues)
                                                            (timeOfReactivation governanceValues))
              ]
    ]

-- Spontaneous transitions VetoSignallingDeactivation -> VetoCooldown
-- Note that VetoSignallingDeactivation -> VetoSignalling cannot happen spontaneously
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#deactivation-sub-state
stableDurationVetoSignallingDeactivation :: GovernanceParams -> TimeAbsolute
                                         -> GovernanceValues -> TimeRelative
stableDurationVetoSignallingDeactivation governanceParams currentTime governanceValues
  = vetoSignallingDeactivationMaxDuration governanceParams
      - timeDifferenceAbsolute currentTime (timeOfDeactivation governanceValues)

-- Spontaneous transitions VetoCooldown -> VetoSignalling OR Normal
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#veto-cooldown-state
stableDurationVetoCooldown :: GovernanceParams -> TimeAbsolute
                           -> GovernanceValues -> TimeRelative
stableDurationVetoCooldown governanceParams currentTime governanceValues
  = vetoCooldownDuration governanceParams
      - timeDifferenceAbsolute currentTime (timeOfActivation governanceValues)

-- Spontaneous transitions RageQuit -> VetoSignalling OR VetoCooldown
-- NOTE Returns Nothing if NFT withdrawal tokens are unclaimed
-- https://github.com/lidofinance/dual-governance/blob/develop/docs/mechanism.md#rage-quit-state
stableDurationRageQuit :: GovernanceParams -> TimeAbsolute
                       -> GovernanceValues -> Maybe TimeRelative
stableDurationRageQuit governanceParams currentTime governanceValues
  = case timeOfRageQuitExtensionStart governanceValues of {
      Nothing -- RageQuit cannot spontaneously transition if NFT withdrawal tokens are unclaimed
        -> Nothing
    ; Just timeOfRageQuitExtensionStart'
           -- ^ RageQuit -> VetoSignalling OR VetoCooldown
        -> Just $ rageQuitExtensionDelay governanceParams
                    + ethWithdrawalTimelockDuration governanceParams (rageQuitSeqNumber governanceValues)
                    - timeDifferenceAbsolute currentTime timeOfRageQuitExtensionStart'
    }

-- Return time until the current state may spontaneously transition without any change to values
-- NOTE Returns Nothing if the current state cannot spontaneously transition
stableDuration :: GlobalLidoState -> GovernanceParams -> TimeAbsolute -> SignallingEscrowState
               -> GovernanceState -> GovernanceValues -> Maybe TimeRelative
stableDuration globalLidoState governanceParams currentTime signallingEscrowState
               governanceState governanceValues
  = case governanceState of {
        Normal -- Normal state cannot spontaneously transition
          -> Nothing
      ; VetoSignalling
          -> Just $ stableDurationVetoSignalling governanceParams currentTime
                                                 (rageQuitSupport globalLidoState signallingEscrowState)
                                                 governanceValues
      ; VetoSignallingDeactivation
          -> Just $ stableDurationVetoSignallingDeactivation governanceParams currentTime governanceValues
      ; VetoCooldown
          -> Just $ stableDurationVetoCooldown governanceParams currentTime governanceValues
      ; RageQuit
          -> stableDurationRageQuit governanceParams currentTime governanceValues
  }

-- Apply transitions until no more are available, including advancing the current time
-- NOTE Calling this function from a game constitutes a strategic assumption that all agents wait
-- Artificially halts if we spend time in the VetoSignalling state
-- to give players a chance to move and prevent infinite loop
transitionWithTime :: GlobalLidoState -> GovernanceParams -> TimeAbsolute -> SignallingEscrowState
                   -> GovernanceState -> GovernanceValues
                   -> (TimeAbsolute, GovernanceState, GovernanceValues)
transitionWithTime globalLidoState governanceParams currentTime signallingEscrowState governanceState governanceValues
    -- Advance the state as far as possible at the current time
  = let (newGovernanceState, newGovernanceValues) = transitionMany globalLidoState governanceParams  currentTime signallingEscrowState governanceState governanceValues
       -- If we just moved into the VetoSignalling state and would now wait for time to pass
       -- then instead artificially halt to give agents the chance to move
    in if newGovernanceState == VetoSignalling && governanceState /= VetoSignalling
          then (currentTime, VetoSignalling, newGovernanceValues)
          else case stableDuration globalLidoState governanceParams currentTime signallingEscrowState newGovernanceState newGovernanceValues of {
                   Nothing -> case claimNFTWithdrawalTokens currentTime newGovernanceState newGovernanceValues of {
                      -- If we can process NFT withdrawals in the RageQuit state, loop
                      Just (newGovernanceState', newGovernanceValues') -> transitionWithTime  globalLidoState governanceParams currentTime signallingEscrowState newGovernanceState' newGovernanceValues'
                      -- We have reached a state with infinite stable duration, halt
                    ; Nothing -> (currentTime, newGovernanceState, newGovernanceValues)
                   }
                   -- Test whether transitions become re-enabled after a finite stable duration
                 ; Just stableDuration
                     -> let newTime = addTimeDuration currentTime stableDuration
                         in case transition1 globalLidoState governanceParams newTime signallingEscrowState newGovernanceState newGovernanceValues of {
                                -- False positive, no transition became enabled, roll back to previous current time and halt
                                Nothing -> (currentTime, newGovernanceState, newGovernanceValues)
                                -- Some transition became enabled, loop
                              ; Just _ -> transitionWithTime globalLidoState governanceParams newTime signallingEscrowState newGovernanceState newGovernanceValues
                            }
               }


------------------------------------
-- 8. Helper functions for analytics
------------------------------------

-- Turn state into payoff for StETH stakers for consumption in the staking game
evaluateProposal :: CurrentProposal ProposalModel -> (Payoff, Payoff)
evaluateProposal (CurrentProposal (Proposal (ProposalModel benefitToLDOHolders benefitToStETHHolders)) _ Executed)
  = (benefitToLDOHolders, benefitToStETHHolders)
evaluateProposal _
  = (0, 0)

-- Evaluate a proposal for stakers
evaluateProposalStakers:: CurrentProposal ProposalModel -> Payoff
evaluateProposalStakers (CurrentProposal (Proposal (ProposalModel _ benefitToStETHHolders)) _ Executed)
  = benefitToStETHHolders
evaluateProposalStakers _
  = 0

-- Evaluate a proposal for LDO holders
evaluateProposalLDOs :: CurrentProposal ProposalModel -> Payoff
evaluateProposalLDOs (CurrentProposal (Proposal (ProposalModel benefitToLDOHolders _)) _ Executed)
  = benefitToLDOHolders
evaluateProposalLDOs _
  = 0


-- Helper for dealing with prob values
extract2nd :: (a,b,c) -> b
extract2nd (_,x,_) = x

extract1st :: (a,b,c) -> a
extract1st (x,_,_) = x


--------------------------
-- 9. InformationStructure
--------------------------

-- Distribution between risk 0 or risk 1 with a given probability
distributionAgentRisks p | (p == 0.0) = distFromList [(1,1)]
                         | (p == 1.0) = distFromList [(0,1)]
                         | (otherwise) = distFromList [(0,p),(1,(1-p))]

-- Provide a parameterized prob distribution regarding subjective risk for a single agent
-- With given prob true signal else noise
distributionSignals :: Double -> CurrentProposal ProposalModel -> Stochastic Double
distributionSignals p proposal = do
  q <- uniformDist [0.01,0.99]
  if q <= p
     then playDeterministically value
     else uniformDist [-1,0] -- ^ the proposal is negative with a specific probability
  where
     CurrentProposal (Proposal proposal') _ _ = proposal
     value = benefitToStETHHolders  proposal'

-- Random proposal
randomProposal = do
  valueStETHHolders <- uniformDist [(-1),0]
  let proposal' = Proposal $ ProposalModel {benefitToLDOHolders = 1, benefitToStETHHolders = valueStETHHolders}
      currentProposal' = CurrentProposal
          { currentProposal  = proposal'
          , timeOfSubmission = UTCTime (fromGregorian 2024 8 31) 0
          , proposalState    = Pending
          }
  return currentProposal'
