{-# LANGUAGE TypeApplications #-}

import           Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, testGroup, includingOptions, localOption, askOption)
import           Test.Tasty.Options (IsOption(..), OptionDescription(..))
import Test.Tasty.Runners (NumThreads(..))
import Config.Common (DatabaseConfig(..), pgConnectionString)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, SqlBackend)
import Control.Monad.Logger
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Pool (Pool, LocalPool)

import qualified Data.Text as T
import qualified Data.Pool as Pool
import qualified Test.Tasty as Tasty

import qualified Test.Cardano.Catalyst.Db

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions dbOptions : defaultIngredients)
    tests

withPostgresPool :: ConnectionString -> (IO (Pool SqlBackend) -> TestTree) -> TestTree
withPostgresPool connStr =
  Tasty.withResource
   (runStdoutLoggingT $ createPostgresqlPool connStr numConnections)
   Pool.destroyAllResources
  where
    numConnections = 1

withPostgresConn :: IO (Pool SqlBackend) -> (IO (SqlBackend, LocalPool SqlBackend) -> TestTree) -> TestTree
withPostgresConn getPool =
  Tasty.withResource
    (getPool >>= Pool.takeResource)
    (\(backend, localPool) ->
       getPool >>= (\pool -> Pool.destroyResource pool localPool backend)
    )

tests :: TestTree
tests =
  -- Force sequential execution, as these database tests need to execute one
  -- after the other.
  localOption (NumThreads 1) $
  -- Get database config
  askOption $ \(DbName dbName) ->
  askOption $ \(DbUser dbUser) ->
  askOption $ \(DbHost dbHost) ->
  let
    connStr = pgConnectionString
      $ DatabaseConfig (T.unpack dbName) (T.unpack dbUser) (T.unpack dbHost)
  in do
    -- Establish and share postgres connection between tests
    withPostgresPool connStr $ \getConnPool -> do
      testGroup "Integration tests"
        [ Test.Cardano.Catalyst.Db.tests getConnPool
        ]

newtype DbName = DbName Text
newtype DbUser = DbUser Text
newtype DbHost = DbHost Text

instance IsOption DbName where
  defaultValue = DbName ""
  parseValue str = Just $ DbName $ T.pack str
  optionName = return "db-name"
  optionHelp = return "Name of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

instance IsOption DbUser where
  defaultValue = DbUser ""
  parseValue str = Just $ DbUser $ T.pack str
  optionName = return "db-user"
  optionHelp = return "User of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

instance IsOption DbHost where
  defaultValue = DbHost ""
  parseValue str = Just $ DbHost $ T.pack str
  optionName = return "db-host"
  optionHelp = return "Host of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

dbOptions :: [OptionDescription]
dbOptions =
  [ Option (Proxy @DbName)
  , Option (Proxy @DbUser)
  , Option (Proxy @DbHost)
  ]
