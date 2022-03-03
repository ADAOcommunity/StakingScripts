{--
   Copyright 2021 ₳DAO

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
--}

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
import           PlutusTx.Maybe
import qualified Data.Map               as Map
import           Data.Text              (Text)
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
import           Data.List              (union, intersect)

-- If the proposal is an update then it can have a new validatorhash to spend to.
data StakingScript = StakingScript
    { parameter1 :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''StakingScript [ ('StakingScript, 0) ]
PlutusTx.makeLift ''StakingScript

data Ownership = Ownership
    { owner     :: !PubKeyHash
    , lastMoved :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''Ownership [ ('Ownership, 0) ]
PlutusTx.makeLift ''Ownership

data Treasure = Treasure
    { totalStaked :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''Treasure [ ('Treasure, 0) ]
PlutusTx.makeLift ''Treasure

data StakeDatum = Owned Ownership | Treasury
    deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''StakeDatum [ ('Owned,    0)
                                        , ('Treasury, 2)
                                        ]
PlutusTx.makeLift ''StakeDatum

data StakeAction = Validate | Collect | Withdraw
    deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''StakeAction [ ('Validate, 0)
                                         , ('Collect,  1)
                                         , ('Withdraw, 2)
                                         ]
PlutusTx.makeLift ''StakeAction

data Stakeing
instance Scripts.ValidatorTypes Stakeing where
    type instance RedeemerType Stakeing = StakeAction
    type instance DatumType Stakeing = StakeDatum

-- Datum Related Functions:
{-# INLINABLE findStakeDatum #-}
findStakeDatum :: TxInfo -> TxOut -> Maybe StakeDatum
findStakeDatum txInfo o = do
    dh      <- txOutDatum o
    Datum d <- findDatum dh txInfo
    PlutusTx.fromBuiltinData d

-- Asset Related Functions
{-# INLINABLE collectionMinted #-}
collectionMinted :: ScriptContext -> AssetClass -> Integer
collectionMinted ctx collectionAsset =
  let
    mintVal = txInfoMint $ scriptContextTxInfo ctx
  in
    assetClassValueOf mintVal collectionAsset

{-# INLINABLE assetContinues #-}
assetContinues :: ScriptContext -> [TxOut] -> AssetClass -> Bool
assetContinues ctx continuingOutputs asset =
    sum [assetClassValueOf (txOutValue x) asset | x <- continuingOutputs] > 0

-- High-Level Functions -- ehh lmao
{-# INLINABLE containsClass #-}
containsClass :: TxOut -> AssetClass -> Bool
containsClass o a = (assetClassValueOf (txOutValue o) a) > 0

{--

{-# INLINABLE getOutput #-}
getOutput :: [TxOut] -> AssetClass -> TxOut
getOutput txOuts asset = case [o | o <- txOuts, containsClass o asset] of
    [x] -> x
    _   -> traceError "Fail here."

{-# INLINABLE containsPot #-}
containsPot :: TxInfo -> TxOut -> Bool
containsPot info o =
  let d = potDatum info o
  in case d of
    Just PotDatum -> True
    _             -> False

{-# INLINABLE getOutputPDatum #-}
getOutputPDatum :: TxInfo -> [TxOut] -> TxOut
getOutputPDatum info txOuts = case [o | o <- txOuts, containsPot info o] of
    [x] -> x
    _   -> traceError "Fail here."

--}

{-- {-# INLINABLE startCollectionDatum #-}
startCollectionDatum :: Maybe StakeDatum -> Bool
startCollectionDatum md = case md of
  Just (CollectionDatum c) ->
    length (votes c) == 0
  _                        -> False

{-# INLINABLE validMakerDatum #-}
validMakerDatum :: Maybe StakeDatum -> Bool
validMakerDatum md = case md of
  Just CollectionMaker ->
    True
  _                        -> False

{-# INLINABLE validPotDatum #-}
validPotDatum :: Maybe StakeDatum -> Bool
validPotDatum md = case md of
  Just PotDatum ->
    True
  _                        -> False --}

-- We only can have one CollectionDatum/Token - We need to implement these - definitely.
-- We only can have one 

{-# INLINABLE stakeScript #-}
stakeScript :: StakingScript -> StakeDatum -> StakeAction -> ScriptContext -> Bool
stakeScript bounty datum action ctx = case datum of
    Owned o  -> case action of
      Validate -> True
      Collect  -> True
      Withdraw -> True
      _        -> False
    Treasury -> case action of
      Collect -> True
      _       -> False

stakeValidatorInstance :: StakingScript -> Scripts.TypedValidator Stakeing
stakeValidatorInstance bounty = Scripts.mkTypedValidator @Stakeing
    ($$(PlutusTx.compile [|| stakeScript ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode bounty)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @StakeDatum @StakeAction

stakeValidatorHash :: StakingScript -> ValidatorHash
stakeValidatorHash bounty = Scripts.validatorHash (stakeValidatorInstance bounty)

stakeValidatorScript :: StakingScript -> Validator
stakeValidatorScript bounty = Scripts.validatorScript (stakeValidatorInstance bounty)

stakeValidatorAddress :: StakingScript -> Address
stakeValidatorAddress bounty = Ledger.scriptAddress (stakeValidatorScript bounty)
