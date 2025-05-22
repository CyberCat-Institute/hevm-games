{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveLift #-}

module EVM.TH (sendAndRun, sendAndRunAll, sendAndRun', sendAndRunDiscard, makeTxCall, balance, loadAll
              , loadLocal
              , ContractFileInfo, mkContractFileInfo, ContractInfo, mkContractInfo, AbiValue (..), Expr (..)
              , stToIO, setupAddresses, getAllContracts) where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict (State, put)
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text as T (Text, breakOn, drop, dropWhile, intercalate, pack, unpack, isPrefixOf)
import Data.Text.IO (readFile)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Char as C (toLower)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (Vector, fromList, toList)

import Control.Monad
import EVM (blankState, emptyContract, exec1, initialContract, loadContract, resetState)
import EVM.Exec (exec, run)
import EVM.Expr
import EVM.Effects (runApp)
import EVM.FeeSchedule
import EVM.Fetch
import EVM.Prelude
import EVM.Solidity
import EVM.Stepper
import EVM.Transaction (initTx)
import EVM.Types
import EVM.Format
import GHC.IO.Unsafe
import GHC.ST
import Language.Haskell.TH.Syntax as TH
import Optics.Core
import Optics.State
import Optics.State.Operators
import Prelude hiding (FilePath, readFile)
import Data.List (nub)

import Debug.Trace

lowerFirst :: String -> String
lowerFirst (x : xs) = C.toLower x : xs

lowerFirst' :: Text -> Text
lowerFirst' = pack . lowerFirst . unpack

-- put this in sttate.callData
-- run it to execute the transaction
-- put more for subsequent calls
-- run more for more results
makeCallData :: EthTransaction -> Expr Buf
makeCallData (EthTransaction _ caller method args _ _) =
  ConcreteBuf $ abiMethod method (AbiTuple (Vector.fromList args))

makeTxCall :: EthTransaction -> EVM Concrete s ()
makeTxCall tx@(EthTransaction addr caller meth args amt gas) = do
  resetState
  assign (#tx % #isCreate) False
  modify (execState (loadContract addr))
  assign (#state % #callvalue) (Lit amt)
  assign (#state % #calldata) (makeCallData tx)
  assign (#state % #caller) (caller)
  assign (#state % #gas) gas
  modify initTx

loadIntoVM :: [(Expr EAddr, ByteString)] -> ST s (VM Concrete s)
loadIntoVM contracts = do
  blankSt <- blankState
  pure $
    VM
      { result = Nothing,
        state = blankSt,
        frames = [],
        env = envForContracts contracts,
        block = emptyBlock,
        tx = emptyTransaction,
        logs = [],
        traces = Zipper.fromForest mempty,
        cache = Cache mempty mempty,
        burned = 0,
        iterations = mempty,
        constraints = [],
        config = undefined
      }
  where
    -- question: Is that a reasonable empty first block?
    emptyBlock :: Block
    emptyBlock =
      Block
        { coinbase = LitAddr 0,
          timestamp = Lit 0,
          number = 0,
          prevRandao = 0,
          maxCodeSize = 0,
          gaslimit = 0,
          baseFee = 0,
          schedule = feeSchedule
        }
    emptyTransaction :: TxState
    emptyTransaction =
      TxState
        { gasprice = 0,
          gaslimit = 0,
          priorityFee = 0,
          origin = LitAddr 0,
          toAddr = LitAddr 0,
          value = Lit 0,
          subState = emptySubState,
          isCreate = True,
          txReversion = mempty
        }
    emptySubState :: SubState
    emptySubState =
      SubState
        { selfdestructs = [],
          touchedAccounts = [],
          accessedAddresses = mempty,
          accessedStorageKeys = mempty,
          refunds = []
        }

    envForContracts :: [(Expr EAddr, ByteString)] -> Env
    envForContracts contracts =
      Env
        { contracts = Map.fromList (fmap (fmap bytecodeToContract) contracts),
          chainId = 0,
          freshAddresses = 0,
          freshGasVals = 0
          -- storage = EmptyStore,
          -- origStorage = mempty
        }

    bytecodeToContract :: ByteString -> Contract
    bytecodeToContract = initialContract . RuntimeCode . ConcreteRuntimeCode

int :: Int -> Exp
int = LitE . IntegerL . toInteger

instance Lift a => Lift (Vector a) where
  lift vec = do ll <- traverse lift (Vector.toList vec)
                let gg = ListE ll
                [| Vector.fromList $( pure gg ) |]

instance Lift AbiType where
  lift (AbiUIntType n)          = [| AbiUIntType n |]
  lift (AbiIntType n)           = [| AbiIntType n |]
  lift AbiAddressType           = [| AbiAddressType |]
  lift AbiBoolType              = [| AbiBoolType |]
  lift (AbiBytesType n)         = [| AbiBytesType n |]
  lift AbiBytesDynamicType      = [| AbiBytesDynamicType |]
  lift AbiStringType            = [| AbiStringType |]
  lift (AbiArrayDynamicType ty) = [| AbiArrayDynamicType ty |]
  lift (AbiArrayType ty arr)    = [| AbiArrayType ty arr |]
  lift (AbiTupleType tys)       = [| AbiTupleType tys |]
  lift AbiFunctionType          = [| AbiFunctionType |]

constructorExprForType :: Quote m => AbiType -> Name -> m Exp
constructorExprForType (AbiUIntType w)  = pure . ((ConE (mkName "AbiUInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiIntType w)   = pure . ((ConE (mkName "AbiInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiAddressType) = pure . (ConE (mkName "AbiAddress") `AppE`) . VarE
constructorExprForType (AbiBoolType)    = pure . (ConE (mkName "AbiBool") `AppE`) . VarE
constructorExprForType (AbiBytesType w) = pure . ((ConE (mkName "AbiBytes") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiBytesDynamicType) = pure . (ConE (mkName "AbiBytesDynamic") `AppE`) . VarE
constructorExprForType (AbiStringType) = pure . (ConE (mkName "AbiString") `AppE`) . VarE
constructorExprForType (AbiArrayDynamicType ty) = \nm ->  [|AbiArrayDynamic ty $(pure (VarE nm))|]
constructorExprForType (AbiArrayType size ty) = error "arrays unsuppported"
constructorExprForType (AbiTupleType types) = error "tuples unsupported" -- ConE (mkName "AbiTuple") [] [VarP (mkName name)]
constructorExprForType (AbiFunctionType) = error "functions unsupported"

data ContractInfo' a = ContractInfo' {
  name :: Text,
  boundName :: Maybe Text, -- there is only a bound name if we generate signatures for its API
  payload :: a} -- the contract type, usually it's a SolcContract but it could also be bytes
  deriving Functor

type ContractInfo = ContractInfo' ()

mkContractInfo :: Text -> Text -> ContractInfo
mkContractInfo name boundName = ContractInfo' name (Just boundName) ()

data ContractFileInfo' a = ContractFileInfo'
  { file :: Text,
    modules :: [ContractInfo' a]
  }

type ContractFileInfo = ContractFileInfo' ()

mkContractFileInfo :: Text -> [ContractInfo] -> ContractFileInfo
mkContractFileInfo = ContractFileInfo'

pat = VarP . mkName

generateTxFactory :: Text -> Method -> Integer -> Text -> Q Dec
generateTxFactory moduleName (Method _ args name sig _) addr contractName = do
  let signatureString :: Q Exp = pure $ LitE $ StringL $ unpack sig
  let argExp :: Q Exp = ListE <$> traverse (\(nm, ty) -> constructorExprForType ty (mkName $ unpack nm)) args
  let patterns :: [Pat] = fmap (VarP . mkName . unpack . fst) args
  let contractAddress :: Q Exp = pure $ AppE (ConE (mkName "LitAddr")) (LitE (IntegerL addr))
  body <-
    [e|
      EthTransaction
        $(contractAddress)
        src
        $(signatureString)
        $(argExp)
        amt
        gas
      |]
  let methodName = unpack (lowerFirst' contractName <> "_" <> name)
  runIO $ putStrLn $ "generating method " ++ methodName
  pure $
    FunD
      (mkName methodName)
      [ Clause
          ( [pat "src", pat "amt", pat "gas"]
              ++ patterns
          )
          (NormalB body)
          []
      ]

instance Lift (Addr) where
  lift (Addr word) = let v = toInteger word in [e|fromInteger v|]

instance Lift (Expr 'EAddr) where
  lift (LitAddr a) = [e|LitAddr (fromInteger a)|]

instance Num (Expr 'EAddr) where
  fromInteger i = LitAddr (fromInteger i)

dropUntilColon :: Text -> Text
dropUntilColon s = T.drop 1 (T.dropWhile (/= ':') s)

loadLocal :: Text -> Q [Dec]
loadLocal contractName = do
  contracts <- runIO (runApp (readBuildOutput "." Foundry))
  case contracts of
    Left err -> runIO (error $ "could not read build ouput, error: " ++ err)
    Right (BuildOutput cts _) ->
      let contractInfo = fromContracts [contractName] cts
      in loadAllContracts contractInfo

fromContracts :: [Text] -> Contracts -> [ContractInfo' SolcContract]
-- fromContracts (Contracts cts) = Prelude.map (\(n, c) -> ContractInfo' n (toLower $ dropUntilColon n) c) (Map.toList cts)
fromContracts names (Contracts cts) = Prelude.map genContract (Map.toList cts)
  where
    genContract :: (Text, SolcContract) -> ContractInfo' SolcContract
    genContract (filename, contract) = let
        boundName = dropUntilColon filename
        usingName = if boundName `elem` names
                       then -- trace (unpack boundName ++ " found in " ++ show names)
                            (Just (lowerFirst' boundName))
                       else -- trace (unpack boundName ++ " not found in " ++ show names)
                            Nothing
      in ContractInfo' filename usingName contract

readContracts :: [ContractFileInfo] -> IO [ContractInfo' SolcContract]
readContracts = fmap concat . traverse loadSolcInfo

loadAll :: [ContractFileInfo] -> Q [Dec]
loadAll = runIO . readContracts >=> loadAllContracts

loadAllContracts :: [ContractInfo' SolcContract] -> Q [Dec]
loadAllContracts allContracts = do
  let allContractsHash = zip [ 0x1000.. ] allContracts
  -- runIO $ putStrLn ("contracts: " <> unlines (fmap (unpack . name) allContracts))
  methods <- concat <$> traverse generateDefsForMethods allContractsHash
  let contractMap = generateContractMap allContractsHash
  -- we remove the contracts that don't have a bound name
  let addedContracts = catMaybes (fmap (\(addr, ContractInfo' nm bn con) -> fmap (\n -> contractName n addr) bn) allContractsHash)
  contractNames <- sequence addedContracts
  init <-
    [d|
      initial = loadIntoVM contractMap
      |]
  pure (init ++ methods ++ contractNames)
  where
    generateContractMap :: [(Integer, ContractInfo' SolcContract)] -> [(Integer, ByteString)]
    generateContractMap = fmap (\(i, contract) -> (i, contract.payload.runtimeCode))
    generateDefsForMethods :: (Integer, ContractInfo' SolcContract) -> Q [Dec]
    generateDefsForMethods (hash, ContractInfo' name (Just boundName) contract) = do
      let methods = Map.elems contract.abiMap
      let noDups = handleDuplicates (fmap handleReservedArgs methods) Map.empty
      -- runIO $ putStrLn ("generating methods for contract" ++ unpack name)
      traverse (\x -> generateTxFactory (dropUntilColon contract.contractName) x hash boundName)
          noDups
    -- if there is no bound name, no need to generate any definition
    generateDefsForMethods (hash, ContractInfo' name Nothing contract)
      = -- runIO (putStrLn ("dropping contract " ++ unpack name)) >>
        pure []

    isReserved :: Text -> Bool
    isReserved "data" = True
    isReserved "import" = True
    isReserved "module" = True
    isReserved "qualified" = True
    isReserved "do" = True
    isReserved _ = False

    reserved' :: (Text, a) -> (Text, a)
    reserved' (arg, a) =
        if isReserved arg
            then (arg <> "'", a)
            else (arg, a)

    handleReservedArgs :: Method -> Method
    handleReservedArgs m =
        over #inputs (fmap reserved') m

    handleDuplicates :: [Method] -> Map Text Int -> [Method]
    handleDuplicates [] _ = []
    handleDuplicates (x : xs) counts = let
      currentName = view #name x
      count = Map.findWithDefault 0 currentName counts
      newCounts = Map.insert currentName (count + 1) counts
      newName = if count == 0
                   then currentName
                   else currentName <> pack (show (count + 1))
      newMethod = set #name newName x
      in newMethod : handleDuplicates xs newCounts

    contractName :: Text -> Integer -> Q Dec
    contractName binder value = do
      let nm = mkName (unpack (binder <> "_contract"))
      let value' = LitAddr (fromInteger value)
      addr <- [e|value'|]
      pure (ValD (VarP nm) (NormalB addr) [])

loadSolcInfo :: ContractFileInfo -> IO [ContractInfo' SolcContract]
loadSolcInfo (ContractFileInfo' contractFilename modules) = do
  file <- readFile (unpack contractFilename)
  json <- solc Solidity file
  let (Contracts sol, _, _) = fromMaybe (error ("canot read json:" ++ unpack json)) (readStdJSON json)
  let retrievedMap = fmap (\mod -> fmap (\() -> Map.lookup ("hevm.sol:" <> mod.name) sol) mod) modules
  emitMissing retrievedMap
  where
    emitMissing :: Show a => [ContractInfo' (Maybe a)] -> IO [ContractInfo' a]
    emitMissing [] = pure []
    emitMissing (ContractInfo' t s Nothing : xs) = putStrLn ("contract " ++ show t ++ " is missing") >> emitMissing xs
    emitMissing (ContractInfo' t s (Just x) : xs) = (ContractInfo' t s x :) <$> emitMissing xs

run' :: EVM Concrete s (Maybe (Expr Buf), VM Concrete s)
run' = do
  vm <- get
  trace (unpack $ showTraceTree undefined vm) $ case vm.result of
    Nothing -> exec1 >> run'
    Just (HandleEffect (Query (PleaseAskSMT (Lit c) _ cont))) ->
      error "SMT effects not handled"
    Just (VMFailure y) -> pure (trace (show y) Nothing, vm)
    Just (VMSuccess y) -> pure (trace "finish ok" (Just y), vm)

-- send and run a transaction on the EVM Concrete state
sendAndRun' :: EthTransaction -> EVM Concrete RealWorld (Maybe (Expr Buf), VM Concrete RealWorld)
sendAndRun' tx = do
  EVM.TH.makeTxCall (traceShow tx tx)
  (output, world) <- run'
  pure (traceShow output (output, world))

-- send and run a single transaction, discard the result
sendAndRunDiscard:: EthTransaction -> EVM Concrete RealWorld (VM Concrete RealWorld)
sendAndRunDiscard tx = snd <$> sendAndRun' tx

sendAndRunAll :: [EthTransaction] -> EVM Concrete RealWorld (Maybe (Expr Buf), VM Concrete RealWorld)
sendAndRunAll [transaction] = sendAndRun' transaction
sendAndRunAll (tx : ts) = do
  EVM.TH.makeTxCall tx
  _ <- run'
  sendAndRunAll ts

-- exectute the EVM Concrete state in IO
sendAndRun ::
  EthTransaction ->
  VM Concrete RealWorld ->
  EVM Concrete RealWorld (VM Concrete RealWorld)
sendAndRun tx st = do
  put st
  sendAndRun' tx
  get

adjustOrAdd :: (Ord k) => (v -> v) -> v -> k -> Map.Map k v -> Map.Map k v
adjustOrAdd f def = alter (Just . maybe def f)

-- Create addresses with corresponding amounts.
-- If the address does not exist, create it, if it exists, add the amount given
-- This is useful to initialise players and contracts at the beginning
setupAddresses :: [(Expr EAddr, Expr EWord)] -> VM Concrete s -> VM Concrete s
setupAddresses amounts =
  over (#env % #contracts) (updateContractMap amounts)
  where
    updateContractMap ::
      [(Expr EAddr, Expr EWord)] ->
      Map.Map (Expr EAddr) Contract ->
      Map.Map (Expr EAddr) Contract
    updateContractMap [] x = x
    updateContractMap ((addr, amount) : cs) map =
      let map' = adjustOrAdd (set #balance amount) (set #balance amount emptyContract) addr map
       in updateContractMap cs map'

    createNew (addr, amount) = (addr, set #balance amount emptyContract)

    updateContractState :: (Expr EAddr, Contract) -> VM Concrete s -> VM Concrete s
    updateContractState (addr, contract) = set (#env % #contracts % at addr) (Just contract)

getAllContracts :: VM Concrete s -> [(Expr EAddr, Expr EWord)]
getAllContracts vm =
  let contracts = Map.toList $ view (#env % #contracts) vm
      contractsAmounts = fmap (fmap (view #balance)) contracts
   in contractsAmounts

balance :: VM Concrete s -> Expr EAddr -> W256
balance st addr =
  let contract = Map.lookup addr st.env.contracts
      Just (Lit balance) = fmap (view #balance) contract
   in balance

-- TODO: use foundry
-- thatOneMethod =
--   let st = loadContracts [ContractFileInfo "solidity/Simple.sol" "Neg" "test"]
--       ourTransaction =
--         EthTransaction
--           (LitAddr 0xabcd)
--           (LitAddr 0x1234)
--           "negate(int256)"
--           [AbiInt 256 3]
--           100000000
--           100000000
--       steps = do
--         evm (makeTxCall ourTransaction)
--         runFully
--    in interpret (zero 0 (Just 0)) undefined steps
