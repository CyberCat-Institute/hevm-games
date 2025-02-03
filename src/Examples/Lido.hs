{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Lido where

import EVM.TH

-- $(loadAll [mkContractFileInfo "DualGovernance.sol" [mkContractInfo "DualGovernance" "dualgov"]])
$(loadAll [mkContractFileInfo "Escrow.sol" [mkContractInfo "Escrow" "escrow2"]])


