
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           Ledger                     (datumHash, scriptHashAddress)


import           Stake

import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
import           Data.String                         (IsString (..))
import           Data.Aeson
import           GHC.Num (encodeDoubleInteger)

{-- treasury = treasuryValidatorHash identityClass' -- ValidatorHash
collectionId = curSymbol collectionMakerClass'

collectionToken' = AssetClass (collectionId, collectionTN)

collectionMakerDatum :: BountyDatum
collectionMakerDatum = CollectionMaker

potDatum' :: BountyDatum
potDatum' = PotDatum

bounty :: Bounty -- TODO fix this so that it fit's the testnet version for us.
bounty = Bounty
    { voters = ["0253cc2bc1ed8176c675f454dd730fae5bfaa147b73924bde70d786a", "97ea01089c844d2a5569ea20744098a7f2dc4c3f450150ceb6f6ad4e"]
    , requiredVotes = 1
    , collectionMakerClass = collectionMakerClass'
    , collectionToken = collectionToken'
    , spendFrom = treasury
    , identityNft = identityClass'
    }
    --}

stake :: StakingScript
stake = StakingScript
    { parameter1 = 1 }

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 0 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump"
  else 
    do 
      let
        scriptnum = 42
        -- treasuryname = "treasury.plutus"
        validatorname = "validator.plutus"
        -- mintCollection = "mintCollection.plutus"
        appliedValidatorScript = stakeValidatorScript stake
        -- appliedTreasuryScript = treasuryValidatorScript identityClass'
        -- appliedMintCollectionToken = policy collectionMakerClass'

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs

        {--
        treasuryAsCbor = serialise appliedTreasuryScript
        treasuryShortBs = SBS.toShort . LB.toStrict $ treasuryAsCbor
        treasuryScript = PlutusScriptSerialised treasuryShortBs
        --}

        {-- mintCollectionAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintCollectionToken

        mintCollAsCbor = serialise mintCollectionAsValidator
        mintCollShortBs = SBS.toShort . LB.toStrict $ mintCollAsCbor
        mintCollScript = PlutusScriptSerialised mintCollShortBs
        --}

        {-- dHash = datumHash $ Datum $ toBuiltinData collectionMakerDatum
        oHash = datumHash $ Datum $ toBuiltinData potDatum'
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData collectionMakerDatum
        ownerToEncode = Plutus.builtinDataToData $ toBuiltinData potDatum'
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode)
        ownerEncoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData ownerToEncode)
        --}

      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript' scriptnum validatorname validatorScript validatorShortBs

      {-- putStrLn $ "Writing output to: " ++ treasuryname
      writePlutusScript' scriptnum treasuryname treasuryScript treasuryShortBs

      putStrLn $ "Writing output to: " ++ mintCollection
      writePlutusScript' scriptnum mintCollection mintCollScript mintCollShortBs --}

      -- putStrLn $ "Writing output to: " ++ mintIdentit-y
      -- writePlutusScript' scriptnum mintIdentity mintIdScript mintIdShortBs

      {-- putStrLn $ "Writing output to: " ++ validVoteName
      writePlutusScript scriptnum validVoteName mintingScript mintingScriptShortBs

      putStrLn $ "Writing output to: " ++ validProposalName
      writePlutusScript scriptnum validProposalName mintingScript mintingScriptShortBs --}

      -- writeFile "collSymbol.txt" (show $ curSymbol collectionMakerClass')

      -- writeFile "idSymbol.txt" (show $ curSymbol identityMakerClass)

      writeFile "validator-hash.txt" (show $ stakeValidatorHash stake)

      -- writeFile "currency-hash.txt" (show $ currencyMPSHash (curSymbol collectionMakerClass'))

      {--
      LB.writeFile "datum.json" encoded
      LB.writeFile "owner.json" ownerEncoded
      writeFile "datum-hash.txt" $ show dHash
      writeFile "owner-hash.txt" $ show oHash

      writeFile "treasury-hash.txt" $ show treasury
      --}

{-- writePlutusScript :: TokenTrace -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript tokenTrace filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataMap [(ScriptDataBytes (fromString "tMinted"), ScriptDataNumber (tMinted tokenTrace)),
                                                               (ScriptDataBytes (fromString "treasuryValue"), ScriptDataNumber (treasuryValue tokenTrace))])
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return () --}

writePlutusScript' :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript' scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()