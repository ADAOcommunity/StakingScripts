{--
   Copyright 2022 ₳DAO

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stake where

import           Prelude                (String, show, Show)
import           Control.Monad          hiding (fmap)
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.Maybe
import qualified Data.Map               as Map
-- import           Data.Ord
-- import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Credential
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import           Ledger.Index           as Index
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Contexts                   as Validation
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, NonEmpty(..) )
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions, ensureKnownCurrencies)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           GHC.Generics         (Generic)
import           Data.String          (IsString (..))
import           Data.Aeson           (ToJSON, FromJSON)

data PersonalStake = PersonalStake {
  owner :: !PubKeyHash
} deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''PersonalStake [ ('PersonalStake,  0) ]
PlutusTx.makeLift ''PersonalStake

data ODatumHash = ODHash DatumHash | OUnit ()
  deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ODatumHash [ ('ODHash, 0)
                                        , ('OUnit,  1)
                                        ]
PlutusTx.makeLift ''ODatumHash

data ContractStake = ContractStake {
  cOwner :: !Address,
  dHash :: !ODatumHash,
  end   :: !POSIXTime
} deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractStake [ ('ContractStake,  0) ]
PlutusTx.makeLift ''ContractStake

data StakeDatum = PStake PersonalStake | CStake ContractStake

PlutusTx.makeIsDataIndexed ''StakeDatum [ ('PStake, 0)
                                        , ('CStake, 1)
                                        ]
PlutusTx.makeLift ''StakeDatum

data StakeRedeemer = Use | Remove

PlutusTx.makeIsDataIndexed ''StakeRedeemer [ ('Use, 0)
                                           , ('Remove, 1)
                                           ]
PlutusTx.makeLift ''StakeRedeemer

{-# INLINABLE getDatum' #-}
getDatum' :: TxInfo -> TxOut -> Maybe StakeDatum
getDatum' txInfo o = do
  dh      <- txOutDatum o
  Datum d <- findDatum dh txInfo
  PlutusTx.fromBuiltinData d

{-# INLINABLE matchesUser #-}
matchesUser :: TxInfo -> PubKeyHash -> TxOut -> Bool
matchesUser info x v =
  let d = getDatum' info v
  in case d of
    Just (PStake ps) -> x == (owner ps)
    _                -> False

{-# INLINABLE spentOwned #-}
spentOwned :: TxInfo -> PubKeyHash -> Value
spentOwned info x =
  let outs = txInfoOutputs info
      ownedOuts = [txOutValue v | v <- outs, matchesUser info x v]
  in
    foldr (<>) mempty ownedOuts

{-# INLINABLE outOwned #-}
outOwned :: TxInfo -> PubKeyHash -> Value
outOwned info x =
  let ins = [txInInfoResolved i | i <- txInfoInputs info]
      ownedIns = [txOutValue v | v <- ins, matchesUser info x v]
  in
    foldr (<>) mempty ownedIns

data Stakeing
instance Scripts.ValidatorTypes Stakeing where
    type instance RedeemerType Stakeing = StakeRedeemer
    type instance DatumType Stakeing = StakeDatum

{-# INLINABLE stakeScript #-}
stakeScript :: [PubKeyHash] -> StakeDatum -> StakeRedeemer -> ScriptContext -> Bool
stakeScript batchers datum action ctx =
  let info = scriptContextTxInfo ctx
  in case datum of
    PStake x     ->
      (txSignedBy info (owner x))
      ||
      ((gt (spentOwned info (owner x)) (outOwned info (owner x)))
      &&
      (length [s | s <- batchers, txSignedBy info s] > 0))
    CStake cs    -> False
    _            -> False

stakeValidatorInstance :: [PubKeyHash] -> Scripts.TypedValidator Stakeing
stakeValidatorInstance cur = Scripts.mkTypedValidator @Stakeing
    ($$(PlutusTx.compile [|| stakeScript ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode cur)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @StakeDatum @StakeRedeemer

stakeValidatorHash :: [PubKeyHash] -> ValidatorHash
stakeValidatorHash cur = Scripts.validatorHash (stakeValidatorInstance cur)

stakeValidatorScript :: [PubKeyHash] -> Validator
stakeValidatorScript cur = Scripts.validatorScript (stakeValidatorInstance cur)

stakeValidatorAddress :: [PubKeyHash] -> Address
stakeValidatorAddress cur = Ledger.scriptAddress (stakeValidatorScript cur)
