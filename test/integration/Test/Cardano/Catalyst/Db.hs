{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Catalyst.Db where

import           Data.Functor (void)
import           Test.Tasty (TestTree, testGroup)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (Pool)
import Test.Tasty.Hedgehog (fromGroup, testProperty)
import Hedgehog (Property, forAll, forAllWith, property, (===))
import Hedgehog.Internal.Property (forAllWithT)
import           Database.Persist.Postgresql (IsolationLevel (Serializable), SqlBackend,
                   SqlPersistT, rawExecute, rawExecute, runSqlPool, runSqlConnWithIsolation, runSqlPoolNoTransaction, Single)
import           Control.Exception.Lifted (bracket_)
import Control.Monad.Base (liftBase)

import Cardano.Catalyst.Db
import Cardano.Catalyst.Db.Gen

import qualified Test.Tasty as Tasty
import qualified Hedgehog.Gen as Gen
import qualified Data.Pool as Pool
import qualified Cardano.Catalyst.Db.Gen as Gen
import qualified Database.Persist.Class as Sql
import qualified Test.Generators as Gen
import qualified Cardano.Api as Cardano
import qualified Cardano.Db as Db

import Cardano.CLI.Query (queryVotingFunds)
import Cardano.CLI.Fetching (RegistrationInfo(..))

withCleanDb :: IO SqlBackend -> TestTree -> TestTree
withCleanDb getBackend testTree =
  Tasty.withResource
    (getBackend >>= cleanDb)
    (const $ getBackend >>= cleanDb)
    (const testTree)
  where
    cleanDb backend = do
      runQuery backend $ do
        r <- ask
        void $ (flip runReaderT) r $ do
          rawExecute "TRUNCATE TABLE tx_in;" []
          rawExecute "TRUNCATE TABLE tx_out;" []
          rawExecute "TRUNCATE TABLE tx_metadata;" []
          rawExecute "TRUNCATE TABLE tx;" []
          rawExecute "TRUNCATE TABLE block;" []
          rawExecute "TRUNCATE TABLE slot_leader;" []
          rawExecute "TRUNCATE TABLE stake_address;" []

runQuery :: SqlBackend -> SqlPersistT IO a -> IO a
runQuery backend query =
  runSqlConnWithIsolation query backend Serializable

tests :: IO (Pool SqlBackend) -> TestTree
tests getConnPool =
  testGroup "Test.Cardano.Catalyst.Db" [
    testProperty "prop_insert" (prop_insert getConnPool)
  ]

-- Isolate a set of queries to a single transaction. The transaction is rolled
-- back if the transaction finishes successfully or throws an exception.
isolated :: MonadBaseControl IO m => Pool SqlBackend -> m a -> m a
isolated pool =
  bracket_
    (liftBase (runQueryNoTransaction pool $ rawExecute "BEGIN" []))
    (liftBase (runQueryNoTransaction pool $ rawExecute "ROLLBACK" []))

-- It's very important that we run each query without the wrapping transaction.
-- Postgres does not support nested transactions, and we are making use of
-- transactions to rollback the database after each test.
runQueryNoTransaction backend query =
  runSqlPoolNoTransaction
    query
    backend
    (Just Serializable)
    -- See https://www.postgresql.org/docs/9.5/transaction-iso.html for more
    -- information. Serializable is probably more strict a isolation level than
    -- we need. The logic in this test suite should prevent transactions running
    -- concurrently (see isolated and NumThreads) but a strict isolation level
    -- is only harmful if we need to retry transactions due to serialization
    -- failures. So if we start seeing that, consider changing this to something
    -- looser.

-- If we register a key, then make a set of contributions against that
-- registration, the voting power reported should match the sum of the
-- contributions.
prop_insert :: IO (Pool SqlBackend) -> Property
prop_insert getConnPool =
  property $ do
    pool <- liftIO getConnPool

    tx <- forAllWith (const "tx") genTransaction
    vote <- forAllWithT (const "vote") Gen.vote

    isolated pool $ do
      (txId, _) <- liftIO $ runQueryNoTransaction pool $ writeTransaction tx
      _ <- liftIO $ runQueryNoTransaction pool $ register' txId vote
      funds <- liftIO $ runQueryNoTransaction pool $ queryVotingFunds Cardano.Mainnet Nothing
      funds === [RegistrationInfo vote (fromIntegral $ Db.unDbLovelace $ Db.txOutSum $ transactionTx tx)]

-- Unsigned registrations are never considered valid
