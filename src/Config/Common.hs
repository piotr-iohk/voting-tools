{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config.Common where

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..), SlotNo (..))
import           Data.Foldable (asum)
import           Options.Applicative (Parser, auto, flag', help, long, metavar, option)
import qualified Data.ByteString.Char8 as BSC
import           Database.Persist.Postgresql (ConnectionString)

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbHost       :: String
                   }
  deriving (Eq, Show)

pgConnectionString :: DatabaseConfig -> ConnectionString
pgConnectionString (DatabaseConfig dbName dbUser dbHost) = BSC.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser

pSlotNo :: Parser SlotNo
pSlotNo = SlotNo
    <$> option auto
          ( long "slot-no"
          <> metavar "WORD64"
          <> help "Slot number to query"
          )

pCardanoEra :: Parser AnyCardanoEra
pCardanoEra = asum
  [ flag' (AnyCardanoEra ByronEra)
      (  long "byron-era"
      <> help "Specify the Byron era"
      )
  , flag' (AnyCardanoEra ShelleyEra)
      (  long "shelley-era"
      <> help "Specify the Shelley era"
      )
  , flag' (AnyCardanoEra AllegraEra)
      (  long "allegra-era"
      <> help "Specify the Allegra era"
      )
  , flag' (AnyCardanoEra MaryEra)
      (  long "mary-era"
      <> help "Specify the Mary era (default)"
      )
  , flag' (AnyCardanoEra AlonzoEra)
      (  long "alonzo-era"
      <> help "Specify the Alonzo era"
      )

    -- Default for now:
  , pure (AnyCardanoEra MaryEra)
  ]
