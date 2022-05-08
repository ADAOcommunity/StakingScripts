
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

batchers :: [PubKeyHash]
-- batchers = ["fe0921cfa53b2deef20f185258f8bc6e127ab6fa1084e62f0830ddef"] -- This just means that noone can use the staked UTxO(s) other than the 'owner'.
batchers = ["45df59537ff070fe9be374ad77630cb4d2dca70ce09c44881e8460da"]

l :: Maybe Integer
l = Nothing

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
        validatorname = "validator.plutus"
        appliedValidatorScript = stakeValidatorScript batchers

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs

        datumAsCbor = serialise l
        datumAsJson = Data.Aeson.toJSON l


      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript' scriptnum validatorname validatorScript validatorShortBs

      writeFile "validator-hash.txt" (show $ stakeValidatorHash batchers)
      writeFile "datum-cbor.txt" (show $ datumAsCbor)
      writeFile "datum-json.txt" (show $ datumAsJson)

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
